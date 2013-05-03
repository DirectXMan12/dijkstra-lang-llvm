import Control.Monad
import System.Environment
import System.Console.GetOpt
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Data.Maybe

import qualified Data.IntMap as IntMap
import qualified Data.HashMap as HashMap
import Data.List (union)
import Text.ParserCombinators.Parsec (runParser, GenParser)
import Text.ParserCombinators.Parsec.Pos (SourceName)

import qualified BaseDijkstra.ETTypeAnalysis as ETTA
import qualified BaseDijkstra.ETCastPass as ETCP
import qualified BaseDijkstra.TAFPass as TAF
import qualified BaseDijkstra.CodeGenPass as CGP 
import qualified BaseDijkstra.TAFPasses.ExtraneousAssignRemover as TAFEAP
import qualified BaseDijkstra.TAFPasses.BuildBasicBlocks as TAFBBB
import qualified BaseDijkstra.TAFPasses.ConvertToSSA as TAFCTS
import BaseDijkstra.TPMTest
import BaseDijkstra.TreePatternMatcher
import BaseDijkstra.Parser

data Flag 
--  = Classpath String
  = Help
  | OutputIR
  | Package String
  | OutputSymbolTable
  | OutputInitialTree
  | OutputThreeAddressForm
  | Verbose
  | OutputName String
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option "h?" ["help"]                  (NoArg Help)                    "Show this help",
    Option "lj" ["emit-llvm", "emit-ir"]  (NoArg OutputIR)                "Output the compiled LLVM IR to inputfile.ll (DEAULT OPTION)",
    Option "p" ["package"]     (ReqArg Package "PACKAGENAME")  "Currently does nothing, since packages don't mean anything to LLVM",
    Option "s"  ["emit-symbol-table"]     (NoArg OutputSymbolTable)       "Output the symbol table to inputfile.st",
    Option "r"  ["emit-tree", "emit-ast"]     (NoArg OutputInitialTree)       "Output the intial AST to inputfile.tr",
    Option "t"  ["emit-taf"]    (NoArg OutputThreeAddressForm) "Output the final Three Address Form SSA code to inputfile.taf",
    Option "v"  ["verbose"]    (NoArg Verbose)   "Print extra compilation details",
    Option "o"  ["output"]    (ReqArg OutputName "FILENAME")  "Set the output file name to OUTPUTNAME instead of inputfile"
  ]

data CompilerOptions = CompilerOptions { verbose :: Bool, emitIR :: Bool, emitST :: Bool, emitTree :: Bool, emitTAF :: Bool, outputName :: String }

parseOptions :: [String] -> Either String ([String], CompilerOptions)
parseOptions argv =
  let
    (actions, nonOptions, errs) = getOpt RequireOrder options argv
    header = "Usage dcc [OPTION...] inputfile"
    hasHelp acts =
      any (\x ->
            case x of
              Help -> True
              other -> False) acts
  in
    case errs of
      [] ->
        if hasHelp actions || (length actions < 1 && length nonOptions < 1)
        then Left (usageInfo header options)
        else Right (nonOptions, parseReturnedFlags (nonOptions !! 0) actions)
      es -> Left (concat es ++ usageInfo header options)

parseReturnedFlags :: String -> [Flag] -> CompilerOptions
parseReturnedFlags fn acts =
  let
    defaultOptions = CompilerOptions { verbose = False, emitIR = True, emitST = False, emitTree = False, emitTAF = False, outputName = fst $ break ('.' ==) fn } 
  in
    foldl (\opts flag ->
            case flag of
              OutputIR -> opts { emitIR = True }
              Package pkg -> opts
              OutputSymbolTable -> opts { emitST = True }
              OutputInitialTree -> opts { emitTree = True }
              OutputThreeAddressForm -> opts { emitTAF = True }
              Verbose -> opts { verbose = True }
              OutputName outname -> opts { outputName = outname }) defaultOptions acts
  
runParserForStrErr :: GenParser tok st a -> st -> SourceName -> [tok] -> Either String a
runParserForStrErr parser st sn toks =
  case runParser parser st sn toks of
    Right res -> Right res
    Left err -> Left (show err)

parseDijkstraForStrErr :: String -> SourceName -> Either String DijkstraTree
parseDijkstraForStrErr code fn = 
  case parseDijkstraWithSrc code fn of
    Right res -> Right res
    Left err -> Left (show err)

main :: IO ()
main = do
  res <- runEitherT mainCompile
  case res of
    Right _ -> putStrLn $ "Compilation and Output Complete!"
    Left err -> putStrLn err

--main :: IO ()
mainCompile = do
  args <- lift getArgs
  (fileNames, compilerOpts) <- hoistEither $ parseOptions args
  when (length fileNames < 1) $ hoistEither $ Left "You must specify an input file name!"
  let (fileName:_) = fileNames
  let outputBase = outputName compilerOpts
  file <- lift . readFile $ fileName

  let whenVerbose str = when (verbose compilerOpts) $ lift . putStrLn $ str

  whenVerbose "Lexing and parsing to AST..."
  initialAST <- hoistEither $ parseDijkstraForStrErr file fileName
  when (emitTree compilerOpts) $ lift . (writeFile (outputBase ++ ".tr")) $ (show initialAST)

  whenVerbose "Building Symbol Table..."
  (STM symTbl _) <- hoistEither $ parseForEither fullProg initialAST ()
  when (emitST compilerOpts) $ lift . (writeFile (outputBase ++ ".st")) $ (show symTbl)
  
  whenVerbose "Performing type inference..."
  noCastTypedAST <- hoistEither $ parseForEither ETTA.prog initialAST ((initialAST, symTbl), [])

  whenVerbose "Casting as necessary..."
  castTypedAST <- hoistEither $ parseForEither ETCP.prog noCastTypedAST ()

  whenVerbose "Building initial Three Address Form..."
  initTAF <- hoistEither $ parseForEither TAF.prog castTypedAST (0,0)
  let initTAFStream = CGP.makeProgTokStream initTAF

  whenVerbose "Removing extra assign statements..."
  noExtraAssignsTAF <- hoistEither $ runParserForStrErr TAFEAP.prog () fileName initTAFStream

  whenVerbose "Building initial basic blocks..."
  let neaTAFStream = CGP.makeProgTokStream noExtraAssignsTAF
  initBasicBlocks <- hoistEither $ runParserForStrErr TAFBBB.prog False fileName neaTAFStream

  whenVerbose "Performing single-block SSA pass 1..."
  let initABBs = map (\x -> TAFCTS.intraBlockSSA x HashMap.empty) initBasicBlocks
  let bblm = TAFCTS.buildABBMap initABBs
  let efm = TAFCTS.buildEnterFromMap initABBs
  let blockOrder = map (TAFBBB.ind . fst) initABBs

  whenVerbose "Performing multi-block SSA pass 1..."
  let pass1ABBs = TAFCTS.interBlockSSA' bblm efm blockOrder

  whenVerbose "Resolving variable forwards..."
  let resolvedForwardsBBs = TAFCTS.resolveAllForwards bblm efm pass1ABBs

  whenVerbose "Adding Phi lines..."
  let bbPairsWithPhi = map (TAFCTS.insertPhiLines' efm) resolvedForwardsBBs
  let plainBBLM = foldl (\lm (bb,_) -> IntMap.insert (TAFBBB.ind bb) bb lm) IntMap.empty bbPairsWithPhi
  let updatesMap = foldl (\um (_, mp) -> IntMap.unionWith union mp um) IntMap.empty bbPairsWithPhi
  let bbWithPhiLinesMap = IntMap.union (IntMap.mapWithKey (\ind lns -> TAFCTS.insertLinesAtBlkHead (plainBBLM IntMap.! ind) lns) updatesMap) plainBBLM
  let bbWithPhiLines = map (\ind -> bbWithPhiLinesMap IntMap.! ind) blockOrder

  whenVerbose "Performing single-block SSA pass 2..."
  let bbFinalAssignsRemoved  = map (TAFCTS.removeFinalAssigns HashMap.empty) bbWithPhiLines
  let newBBLM = TAFCTS.buildABBMap bbFinalAssignsRemoved

  whenVerbose "Performing multi-block SSA pass 2..."
  let finalBBs = TAFCTS.interBlockSSA2' newBBLM efm blockOrder

  whenVerbose "Composing final Three Address Form..."
  let fullSSATAF = concat $ reverse $ map (reverse . TAFBBB.lines) finalBBs
  when (emitTAF compilerOpts) $ lift . (writeFile (outputBase ++ ".taf")) $ (show fullSSATAF)
  let fullSSATAFStream = CGP.makeProgTokStream fullSSATAF

  whenVerbose "Compiling Three Address Form to LLVM IR..."
  finalCode <- hoistEither $ runParserForStrErr CGP.prog (0, HashMap.empty) fileName fullSSATAFStream
  when (emitIR compilerOpts) $ lift . (writeFile (outputBase ++ ".ll")) $ finalCode

  return ()
