module BaseDijkstra.TAFPasses.ExtraneousAssignRemover (prog) where

import BaseDijkstra.TAFScanner
import BaseDijkstra.ThreeAddressForm
import BaseDijkstra.SymbolTable as ST (VariableType(..))
import Text.ParserCombinators.Parsec hiding (label)

type TAFParser = GenTAFParser [ThreeAddressRecord] ()

prog :: TAFParser
prog = do
  pnNode <- anyTAFEntry
  lines <- many1 progLine
  return $ pnNode:(concat lines)

progLine :: TAFParser
progLine = dupAssignLine <|> do { n <- anyTAFEntry; return [n] }

dupAssignLine :: TAFParser
dupAssignLine = try $ do
  finalNode <- anyTAFWithTempTarget
  (targ, arg) <- gen2AOp ASSIGN
  if (arg /= target finalNode)
    then fail "Not an extraneous assignment!" 
    else
      case finalNode of
        (ThreeAddressForm _ bop l r) -> return [ThreeAddressForm targ bop l r]
        (TwoAddressForm _ uop a) -> return [TwoAddressForm targ uop a]
