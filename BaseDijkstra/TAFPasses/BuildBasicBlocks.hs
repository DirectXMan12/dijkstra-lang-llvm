module BaseDijkstra.TAFPasses.BuildBasicBlocks (prog, prog', BasicBlock(..)) where

import BaseDijkstra.TAFScanner
import BaseDijkstra.ThreeAddressForm
import BaseDijkstra.SymbolTable as ST (VariableType(..))
import Text.ParserCombinators.Parsec hiding (label)
import Data.HashMap as HashMap

data BasicBlockBuilder  = AddEntryToCurrent { entry :: ThreeAddressRecord } 
                        | StartNew { labelInd :: Int, entry :: ThreeAddressRecord} 
                        | AddFinalEntryToCurrent { entry :: ThreeAddressRecord, exitTo :: [Int] } 
                        | EndWithoutFinalEntry { exitTo :: [Int] } deriving (Eq, Show)

-- NOTE: the list of lines in a basic block is backwards to facilitate easy addition
data BasicBlock = BasicBlock { ind :: Int, exitsTo :: [Int], lines :: [ThreeAddressRecord] } deriving (Eq, Show)

addTAFToBlock :: ThreeAddressRecord -> BasicBlock -> BasicBlock
addTAFToBlock entry (BasicBlock i et ls) =
  BasicBlock i et (entry:ls)

setBlockExitTo :: [Int] -> BasicBlock -> BasicBlock
setBlockExitTo et (BasicBlock i _ ls) =
  BasicBlock i et ls

-- NOTE: basic block list is backwards to facilitate easy addition and lookup
type TAFParser = GenTAFParser [BasicBlockBuilder] Bool

prog :: GenTAFParser [BasicBlock] Bool
prog = do
  pnNode <- anyTAFEntry
  lines <- many1 progLine
  return $ foldl foldFunc [] $ (StartNew (-1) pnNode):(concat lines)
  where
    foldFunc (b:bs) instruction =
      case instruction of
        (AddEntryToCurrent e) -> (addTAFToBlock e b):bs
        (StartNew lblInd fe) -> (BasicBlock lblInd [] [fe]):b:bs
        (AddFinalEntryToCurrent e et) -> (setBlockExitTo et $ addTAFToBlock e b):bs
        (EndWithoutFinalEntry et) -> (setBlockExitTo et b):bs
    foldFunc [] (StartNew lblInd fe) = [(BasicBlock lblInd [] [fe])]

prog' :: GenTAFParser [BasicBlockBuilder] Bool
prog' = do
  pnNode <- anyTAFEntry
  lines <- many1 progLine
  return $ (StartNew (-1) pnNode):(concat lines)

progLine :: TAFParser
progLine = labelLine <|> branchLine <|> otherLine

labelLine :: TAFParser
labelLine = do
  lblInd <- label
  lastLineWasBranch <- getState
  let resPrefix = if lastLineWasBranch /= True
                    then [AddFinalEntryToCurrent (UncondBranchForm (LabelIdentifier lblInd)) [lblInd]]
                    else []
  setState False
  return $ resPrefix ++ [StartNew lblInd (LabelForm lblInd)]

branchLine :: TAFParser
branchLine = do
  (condPart, lbl1@(LabelIdentifier lbl1Ind)) <- branch
  setState True
  case condPart of
    Just (condVal, lbl2@(LabelIdentifier lbl2Ind)) -> return $ [AddFinalEntryToCurrent (CondBranchForm condVal lbl1 lbl2) [lbl1Ind, lbl2Ind]]
    Nothing -> return $ [AddFinalEntryToCurrent (UncondBranchForm lbl1) [lbl1Ind]]

otherLine :: TAFParser
otherLine = do
  entry <- anyTAFEntry
  setState False
  return $ [AddEntryToCurrent entry]
