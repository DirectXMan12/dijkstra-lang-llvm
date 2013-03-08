import BaseDijkstra.Parser
import qualified BaseDijkstra.ETTypeAnalysis as ETTA
import BaseDijkstra.TPMTest
import BaseDijkstra.TreePatternMatcher
import qualified BaseDijkstra.ETCastPass as ETCP
import qualified BaseDijkstra.TAFPass as TAF
import qualified BaseDijkstra.TAFScanner as TAFS
import qualified BaseDijkstra.CodeGenPass as CGP 
import qualified BaseDijkstra.TAFPasses.ExtraneousAssignRemover as TAFEAP
import qualified BaseDijkstra.TAFPasses.BuildBasicBlocks as TAFBBB
import qualified BaseDijkstra.TAFPasses.ConvertToSSA as TAFCTS
import qualified BaseDijkstra.SymbolTable as ST
import qualified BaseDijkstra.ThreeAddressForm as TAR
import Text.ParserCombinators.Parsec (runParser)
import qualified Data.HashMap as HashMap
import qualified Data.IntMap as IntMap


-- n = BranchNode VARDECL [LeafNode FLOAT "3.14159", LeafNode INT "3", LeafNode FLOAT "123.45"] Nothing
--n = BranchNode VARDECL [LeafNode TYPE "int", LeafNode ID "va", LeafNode ID "vb"] Nothing
n = BranchNode MULT [LeafNode ID "va", LeafNode ID "vb"] Nothing
p1 = "program cheese; float va, vb; if va > vb :: { int vc; vc <- va } fi"
p2 = "program crackers; float va, vb; input va; vb <- va - 1; if va > vb :: { int vc; vc <- va * vb + 2; print vc } fi"
p3 = "program crumpets; float va, vb; va <- vb * 2;"
p4 = "program cheese; float a; a <- 1.0; int b; b <- a + 1"
p5 = "program cheese; int x; x <- 1; x <- x+1; int y; input y; y <- y*2"
gcdecode = unlines $ [
  "program euclid",
  " int a, b",
  " input a,b",
  " do",
  "   (b ~= 0) :: {",
  "     int t",
  "     t <- b",
  "     b <- a mod b",
  "     a <- t",
  "   }",
  " od",
  " print a"]


t1 = parseDijkstra p1
(Right pt1) = t1 

myprog = do { branchOfType PROGRAM; _V_; visitNextSibling; prog }

Just (STM stres _) = parseForResult myprog pt1 ()

pptaf taf = putStrLn $ foldl (\acc inst -> acc ++ (show inst) ++ "\n") "\n" taf

ppbb bs = putStrLn $ foldl (\acc elem -> acc ++ "BasicBlock " ++ (show $ TAFBBB.ind elem) ++ " exits to " ++ (show $ TAFBBB.exitsTo elem) ++ "\n---------------------------\n" ++ (foldl (\acc inst -> acc ++ (show inst) ++ "\n") "\n" $ reverse $ TAFBBB.lines elem) ++ "\n\n") "" (reverse bs)

parseForASTP prog =
  case res of
    Right ast -> ast
    otherwise -> error ("issue with compiling: " ++ (show res))
    where res = parseForAST prog

parseForAST prog =
  case res of
    (Right (Just (Right (Just (Just ast))))) -> Right ast
    otherwise -> Left res
    where res = parseForAST' prog

parseForAST' prog =
  --fmap (\z -> fmap (\y -> fmap (\x -> parseForResult ETCP.prog x ((x, y), [])) woCasting) z) symTbl
  quadFmap (\ast -> parseForResult ETCP.prog ast ()) woCasting
  where baseAST = parseDijkstra prog
        doubleFmap f t = fmap (\x -> fmap f x) t
        quadFmap f t = doubleFmap (doubleFmap f) t
        stm = fmap (\x -> parseForResult myprog x ()) baseAST
        symTbl = fmap (\x -> fmap (\(STM st _) -> st) x) stm
        woCasting = fmap (\z -> fmap (\y -> fmap (\x -> parseForResult ETTA.prog x ((x, y), [])) baseAST) z) symTbl

buildBBs prog =
  let
    ast = parseForASTP prog
    (Just taf) = parseForResult TAF.prog ast (0,0)
    tafstream1 = CGP.makeProgTokStream taf
    (Right taf2) = runParser TAFEAP.prog () "" tafstream1
    tafstream2 = CGP.makeProgTokStream taf2
  in
    runParser TAFBBB.prog False "" tafstream2

buildABBs prog =
  let (Right bbs) = buildBBs prog
  in map (\x -> TAFCTS.intraBlockSSA x HashMap.empty) bbs

compileCodeNoSSA prog = 
  let
    ast = parseForASTP prog
    (Just taf) = parseForResult TAF.prog ast (0,0)
    tafstream1 = CGP.makeProgTokStream taf
    (Right taf2) = runParser TAFEAP.prog () "" tafstream1
    tafstream2 = CGP.makeProgTokStream taf2
    (Right finalCode) = runParser CGP.prog (0,HashMap.empty) "" tafstream2
  in finalCode

buildAlmostFin prog =
  let
    abbs = buildABBs prog
    bblm = TAFCTS.buildABBMap abbs
    efm = TAFCTS.buildEnterFromMap abbs
    blockOrder = map (TAFBBB.ind . fst) abbs
    res1 = TAFCTS.interBlockSSA' bblm efm blockOrder
  in
    TAFCTS.resolveAllForwards bblm efm res1
 
buildFinalBB prog =
  let
    bbWithSSA = buildAlmostFin prog
    blockOrder = map TAFBBB.ind bbWithSSA
    bbWithPhiLines = map TAFCTS.insertPhiLines bbWithSSA
    bbFinal1 = map (TAFCTS.removeFinalAssigns HashMap.empty) bbWithPhiLines
    bblm = TAFCTS.buildABBMap bbFinal1
    efm = TAFCTS.buildEnterFromMap bbFinal1
    bbFinal = TAFCTS.interBlockSSA2' bblm efm blockOrder
  in bbFinal

compileCode prog =
  let
    bbFinal = buildFinalBB prog
    fullSSATAF = concat $ reverse $ map (\bb -> reverse $ TAFBBB.lines bb) bbFinal
    tafstream = CGP.makeProgTokStream fullSSATAF
    finalCode = runParser CGP.prog (0, HashMap.empty) "" tafstream
  in
    case finalCode of
      Left err -> Left (err, fullSSATAF)
      Right code -> Right code
