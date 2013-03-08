module BaseDijkstra.TAFPasses.ConvertToSSA (intraBlockSSA, interBlockSSA', buildABBMap, buildEnterFromMap, resolveAllForwards, insertPhiLines, removeFinalAssigns, interBlockSSA2') where

import BaseDijkstra.ThreeAddressForm
import BaseDijkstra.SymbolTable as ST (VariableType(..))
import BaseDijkstra.TAFPasses.BuildBasicBlocks as BBB (BasicBlock(..))
import qualified Data.HashMap as HashMap hiding ((!))
import Data.HashMap ((!))
import qualified Data.IntMap as IntMap
import Data.List (union, isInfixOf, (\\))
--import Debug.Trace
--import System.IO.Unsafe

-- For each block

data OperandState = ConstantVal { val :: String, instNum :: Int }
                  | NumberedSelf { instNum :: Int, blockNum :: Int } 
                  | NumberedForeign { foreignVar :: Operand, foreignNum :: Int, instNum :: Int, blockNum :: Int }
                  | Phi [OperandState] deriving (Eq, Show)

type StateVector = HashMap.Map Operand OperandState

getOrInitCount :: Operand -> StateVector -> Int -> (OperandState, StateVector)
getOrInitCount (Constant vt val) vec _ = (ConstantVal val (-1), vec)
getOrInitCount var vec blkNum =
  case (HashMap.lookup var vec) of
    Just opState -> (opState, vec)
    Nothing -> (NumberedSelf 0 blkNum, HashMap.insert var (NumberedSelf 0 blkNum) vec)

incCount :: Int -> Maybe OperandState -> Maybe OperandState
incCount blkNum cv = 
  case cv of
    Just (ConstantVal _ oldNum) -> Just (NumberedSelf (oldNum+1) blkNum) 
    Just (NumberedSelf oldNum bNum) -> Just (NumberedSelf (oldNum+1) bNum)
    Just (NumberedForeign _ _ oldNum bNum) -> Just (NumberedSelf (oldNum+1) bNum)
    Nothing -> Just (NumberedSelf 1 blkNum)

showIdent :: Operand -> String
showIdent (Identifier name _) = name
showIdent (TemporaryIdentifier i _) = "temp" ++ (show i)

createOperand :: Operand -> OperandState -> Operand
createOperand oldOp opState =
  case oldOp of
    Identifier oldName vt ->
      case opState of
        NumberedSelf num bNum ->
          if num == 0 then Identifier (oldName ++ "." ++ show num) vt
          else Identifier (oldName ++ "." ++ show num ++ "." ++ (show bNum)) vt
        NumberedForeign id num _ bNum -> Identifier ((showIdent id) ++ "." ++ show num ++ "." ++ (show bNum)) (varType id)
        ConstantVal val _ -> Constant vt val
    tmpId@(TemporaryIdentifier oldInd vt) ->
      case opState of
        NumberedSelf num bNum -> 
          if num == 0
          then Identifier ((showIdent tmpId) ++ "." ++ show num) vt
          else Identifier ((showIdent tmpId) ++ "." ++ (show num) ++ "." ++ (show bNum)) vt
        NumberedForeign id num _ bNum -> Identifier ((showIdent id) ++ "." ++ show num ++ "." ++ (show bNum)) (varType id)
        ConstantVal val _ -> Constant vt val
    Constant vt val -> Constant vt val

mkOpState :: OperandState -> (Operand, OperandState) -> OperandState
mkOpState oldState (valOp, valState) =
  case valState of
    ConstantVal cv _ -> ConstantVal cv (instNum oldState)
    NumberedSelf vsNum bNum -> NumberedForeign valOp vsNum (instNum oldState) bNum
    NumberedForeign vop vn _ bNum -> NumberedForeign vop vn (instNum oldState) bNum

mkOpStateNoRedir :: OperandState -> (Operand, OperandState) -> OperandState
mkOpStateNoRedir oldState (valOp, valState) =
  case valState of
    ConstantVal cv _ -> ConstantVal cv (instNum oldState)
    NumberedSelf vsNum bNum -> NumberedSelf ((instNum oldState)+1) (blockNum oldState)
    NumberedForeign vop vn _ bNum -> NumberedForeign vop vn (instNum oldState) bNum
    

-- (BasicBlock, Exit State)
type AnnotatedBasicBlock = (BasicBlock, StateVector)

intraBlockSSA :: BasicBlock -> StateVector -> AnnotatedBasicBlock
intraBlockSSA oldBB oldSV =
  let
    currBlkNum = BBB.ind oldBB
    incCount' = incCount currBlkNum
    foldFunc (newLines, newSV) currLine =
      let (nl, nv)  = case currLine of
                        ThreeAddressForm targ bop ls rs
                          | bop /= CAST ->
                            let
                              (lsState, tempVec) = getOrInitCount ls newSV currBlkNum
                              (rsState, tempVec2) = getOrInitCount rs tempVec currBlkNum
                              newVec = HashMap.alter incCount' targ tempVec2
                              targState = newVec ! targ
                            in
                              (ThreeAddressForm (createOperand targ targState) bop (createOperand ls lsState) (createOperand rs rsState), newVec)
                          | otherwise -> 
                            let
                              (rsState, tempVec) = getOrInitCount rs newSV currBlkNum
                              newVec = HashMap.alter incCount' targ tempVec
                              targState = newVec ! targ
                            in
                              (ThreeAddressForm (createOperand targ targState) bop ls (createOperand rs rsState), newVec)
                        TwoAddressForm targ uop ar
                          | uop == ASSIGN ->
                            let
                              (arState, tempVec) = getOrInitCount ar newSV currBlkNum
                              (oldTargState, tempVec2) = getOrInitCount targ tempVec currBlkNum
                              newVec = 
                                case arState of
                                  (NumberedSelf i _) | i == 0 -> HashMap.alter incCount' targ tempVec
                                  other -> HashMap.insert targ (mkOpStateNoRedir oldTargState (ar, arState)) tempVec2
                              --newVec = HashMap.alter incCount targ tempVec
                              targState = newVec ! targ
                            in
                              (TwoAddressForm (createOperand targ targState) uop (createOperand ar arState), newVec)
                              --(TwoAddressForm (createOperand targ targState) uop (createOperand ar arState), newVec)
                          | otherwise -> 
                            let
                              (arState, tempVec) = getOrInitCount ar newSV currBlkNum
                              newVec = HashMap.alter incCount' targ tempVec
                              targState = newVec ! targ
                            in
                              (TwoAddressForm (createOperand targ targState) uop (createOperand ar arState), newVec)
                        SingleOperandForm uop ar | uop == OUTPUT -> 
                          let
                            (arState, newVec) = getOrInitCount ar newSV currBlkNum
                          in (SingleOperandForm uop (createOperand ar arState), newVec)
                        TargetOnlyForm targ niop | niop == INPUT ->
                          let
                            newVec = HashMap.alter incCount' targ newSV
                            targState = newVec ! targ
                          in
                            (TargetOnlyForm (createOperand targ targState) niop, newVec)
                        CondBranchForm cond lbl1 lbl2 ->
                          let
                            (condState, newVec) = getOrInitCount cond newSV currBlkNum
                          in (CondBranchForm (createOperand cond condState) lbl1 lbl2, newVec)
                        other -> (other, newSV)
      in (nl:newLines, nv)
    (newLines, newSV) = foldl foldFunc ([], oldSV) $ (reverse . BBB.lines) oldBB
    
  in
    ((BasicBlock (BBB.ind oldBB) (exitsTo oldBB) newLines), newSV)

type BBLookupMap = IntMap.IntMap AnnotatedBasicBlock

buildABBMap :: [AnnotatedBasicBlock] -> BBLookupMap
buildABBMap bbs =
  foldl (\lm abb -> IntMap.insert (BBB.ind . fst $ abb) abb lm) IntMap.empty bbs

buildEnterFromMap :: [AnnotatedBasicBlock] -> IntMap.IntMap [Int]
buildEnterFromMap bbs =
  foldl (\efm ((BasicBlock efI et _), _) ->
          foldl (\efm keyI -> IntMap.insertWith (\x y -> x ++ y) keyI [efI] efm) efm et) IntMap.empty bbs

interBlockSSA' :: BBLookupMap -> IntMap.IntMap [Int] -> [Int] -> [BasicBlock]
interBlockSSA' bblm enterFromMap finalOrder = 
  let
    filterVec sv = HashMap.filter (\st -> case st of
                                            (NumberedSelf num _) | num == 0 -> False
                                            other -> True) sv
    blockFunc currInd =
      let
        predecessors = IntMap.findWithDefault [] currInd enterFromMap
        predecessorExitVecs = map (\predInd -> snd $ bblm IntMap.! predInd) predecessors
        stateVecToMerge = 
          case length predecessors of
            0 -> HashMap.empty
            1 -> (predecessorExitVecs !! 0)
            other -> makePhis predecessorExitVecs
      in forwardEntryState bblm currInd $ filterVec stateVecToMerge 
    finalBBs = map blockFunc $ (reverse . IntMap.keys) $ bblm
    finalBBLM = foldl (\lm bb -> IntMap.insert (BBB.ind bb) bb lm) IntMap.empty finalBBs
  in map (\ind -> finalBBLM IntMap.! ind) finalOrder

makePhis :: [StateVector] -> StateVector
makePhis inputVectors =
  let
    unionFunc op (Phi optns1) (Phi optns2) = Phi (optns1 `union` optns2)
    wrapWithPhi sv = HashMap.map (\opState -> Phi [opState]) sv
    allPhis = foldl (\aggSV currSV -> HashMap.unionWithKey unionFunc aggSV currSV) HashMap.empty $ map wrapWithPhi inputVectors :: StateVector
    optimizationFunc (Phi [st]) = st
    optimizationFunc p = p
  in
    HashMap.map optimizationFunc allPhis

showOpState :: String -> OperandState -> String
showOpState currVarName (NumberedSelf num bNum) = currVarName ++ "." ++ (show num) ++ "." ++ (show bNum)
showOpState currVarName (NumberedForeign fOp num _ bNum) = (showIdent fOp) ++ "." ++ (show num) ++ "." ++ (show bNum)
showOpState currVarName (ConstantVal v _) = v

createForwardIdentifier :: Int -> Operand -> StateVector -> Operand
createForwardIdentifier currBlkNum oldId@(Identifier oldName vt) fwdStateVec =
  let 
    (name, _:rawInstBlkInd) = break (\x -> x == '.') oldName
    (currInst, currBlk) = 
      if elem '.' rawInstBlkInd
      then
        let (rawInstNum, _:rawBlkNum) = break (\x -> x == '.') rawInstBlkInd
        in (read rawInstNum :: Int, read rawBlkNum :: Int)
      else (read rawInstBlkInd :: Int, (-1))
    fwdVal = HashMap.findWithDefault ({-trace ("\n\ncouldn't find " ++ (show name) ++ " in " ++ (show fwdStateVec) ++ "\n\n") $-} NumberedSelf 0 currBlkNum) (Identifier name vt) fwdStateVec
    --makeOptsStr [opt] = "(" ++ (show $ fst opt) ++ "," ++ (showOpState name $ snd opt) ++ ")"
    --makeOptsStr (opt:optns) = "(" ++ (show $ fst opt) ++ "," ++ (showOpState name $ snd opt) ++ "), " ++ (makeOptsStr optns)
    makeOptsStr [opt] = (showOpState name opt)
    makeOptsStr (opt:optns) = (makeOptsStr [opt]) ++ "," ++ (makeOptsStr optns)
  in
    if currInst == 0
    then
      case fwdVal of
        NumberedSelf fwdInst bNum -> 
          if fwdInst == 0
          then Identifier (name ++ ".forward") vt
          else Identifier (name ++ "." ++ (show fwdInst) ++ "." ++ (show bNum)) vt
        Phi optns -> Identifier (name ++ ".phi[" ++ (makeOptsStr optns) ++ "]") vt
        NumberedForeign fOp fInst _ bNum -> Identifier ((showIdent fOp) ++ "." ++ (show fInst) ++ "." ++ (show bNum)) vt
        ConstantVal v _ -> Constant vt v
    else
      oldId
createForwardIdentifier _ cv@(Constant _ _) fwdStateVec = cv

forwardEntryState :: BBLookupMap -> Int -> StateVector -> BasicBlock
forwardEntryState bblm currInd svToMerge =
  let
    oldBlock = fst $ bblm IntMap.! currInd
    oldLines = BBB.lines oldBlock
    lineForwardFunc ln = 
      case ln of
        ThreeAddressForm targ bop ls rs
          | bop /= CAST -> ThreeAddressForm (createForwardIdentifier currInd targ svToMerge) bop (createForwardIdentifier currInd ls svToMerge) (createForwardIdentifier currInd rs svToMerge) 
          | otherwise -> ThreeAddressForm (createForwardIdentifier currInd targ svToMerge) bop ls (createForwardIdentifier currInd rs svToMerge)
        TwoAddressForm targ uop ar -> TwoAddressForm (createForwardIdentifier currInd targ svToMerge) uop (createForwardIdentifier currInd ar svToMerge)
          {-| uop /= ASSIGN -> TwoAddressForm (createForwardIdentifier currInd targ svToMerge) uop (createForwardIdentifier currInd ar svToMerge) -}
        SingleOperandForm uop ar | uop == OUTPUT -> SingleOperandForm uop (createForwardIdentifier currInd ar svToMerge)
        TargetOnlyForm targ niop | niop == INPUT -> TargetOnlyForm (createForwardIdentifier currInd targ svToMerge) niop
        CondBranchForm cond lbl1 lbl2 -> CondBranchForm (createForwardIdentifier currInd cond svToMerge) lbl1 lbl2
        other -> ln
    newLines = map lineForwardFunc oldLines
  in BasicBlock (BBB.ind oldBlock) (exitsTo oldBlock) newLines

buildAPBBMap :: [BasicBlock] -> BBLookupMap -> BBLookupMap
buildAPBBMap bbs bblm =
  foldl (\lm bb -> IntMap.insert (BBB.ind bb) (bb, snd $ bblm IntMap.! (BBB.ind bb)) lm) IntMap.empty bbs

resolveForward :: BBLookupMap -> IntMap.IntMap [Int] -> Int -> Operand -> Operand
resolveForward bblm efm currInd opToLookup =
  let
    entersFrom = efm IntMap.! currInd
    foldFunc (checkedList, poss) indToCheck
      | indToCheck `elem` checkedList = (checkedList, poss)
      | otherwise =
        let
          currExitVec = HashMap.filter  (\st -> case st of
                                          (NumberedSelf num _) | num == 0 -> False
                                          other -> True) $ snd $ bblm IntMap.! indToCheck
          res = HashMap.lookup opToLookup currExitVec
          thisEntersFrom = efm IntMap.! indToCheck
        in
          case res of
            Just p -> ({-currInd-}indToCheck:checkedList, ([p] `union` poss))
            Nothing -> foldl foldFunc ({-currInd-}indToCheck:checkedList, poss) thisEntersFrom
    rawOptions = snd $ foldl foldFunc (foldFunc ([],[]) currInd) entersFrom 
    mergeToPhi (Phi optsSoFar) (Phi moreOpts) = Phi (optsSoFar `union` moreOpts)
    mergeToPhi (Phi optsSoFar) st = Phi (st:optsSoFar)
    mergedOptions = foldl mergeToPhi (Phi []) rawOptions
    makeOptsStr [opt] = (showOpState (name opToLookup) opt)
    makeOptsStr (opt:optns) = (makeOptsStr [opt]) ++ "," ++ (makeOptsStr optns)
  in
    case mergedOptions of
      (Phi [st]) ->
        case st of
          NumberedSelf n bn -> Identifier ((showIdent opToLookup) ++ "." ++ (show n) ++ "." ++ (show bn)) (varType opToLookup)
          NumberedForeign fOp n _ bn -> Identifier ((showIdent fOp) ++ "." ++ (show n) ++ (show bn)) (varType opToLookup)
          ConstantVal v _ -> Constant (varType opToLookup) v
      (Phi optns) -> Identifier ((showIdent opToLookup) ++ ".phi[" ++ (makeOptsStr optns) ++ "]") (varType opToLookup)

applyToLineArguments :: (Operand -> Operand) -> ThreeAddressRecord -> ThreeAddressRecord
applyToLineArguments f ln =
  case ln of
    ThreeAddressForm targ bop ls rs
      | bop /= CAST -> ThreeAddressForm (f targ) bop (f ls) (f rs) 
      | otherwise -> ThreeAddressForm (f targ) bop ls (f rs)
    TwoAddressForm targ uop ar -> TwoAddressForm (f targ) uop (f ar)
    SingleOperandForm uop ar | uop == OUTPUT -> SingleOperandForm uop (f ar)
    TargetOnlyForm targ niop | niop == INPUT -> TargetOnlyForm (f targ) niop
    CondBranchForm cond lbl1 lbl2 -> CondBranchForm (f cond) lbl1 lbl2
    other -> ln
  

resolveAllForwards :: BBLookupMap -> IntMap.IntMap [Int] -> [BasicBlock] -> [BasicBlock]
resolveAllForwards bblm efm bbs =
  let
    lineFunc blkInd = applyToLineArguments $
      (\arg ->
        case arg of
          (Identifier n vt) | (snd $ break ('.' ==) n) == ".forward" ->
            resolveForward bblm efm blkInd (Identifier (fst $ break ('.' ==) n) vt)
          other -> arg)
    resolveBlock blkInd bb =
      let 
        oldLines = BBB.lines bb
        newLines = map (lineFunc blkInd) oldLines
      in
        BasicBlock (BBB.ind bb) (exitsTo bb) newLines
  in
    map (uncurry resolveBlock) $ zip (map (\bb -> BBB.ind bb) bbs) bbs

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delim orig =
  if delim `elem` orig
  then 
    let (p1, _:p2) = break (delim ==) orig
    in p1:(splitOn delim p2)
  else [orig]

-- Shamelessly (ok, maybe with a bit of shame) taken from a bluebones.net post
replaceAll :: Eq a => [a] -> [a] -> [a] -> [a]
replaceAll [] _ _ = []
replaceAll s find repl =
    if take (length find) s == find
        then repl ++ (replaceAll (drop (length find) s) find repl)
        else [head s] ++ (replaceAll (tail s) find repl)

insertPhiLines :: BasicBlock -> BasicBlock
insertPhiLines bb =
  let
    oldLines = BBB.lines bb
    isPhi (Identifier n _) | ".phi" `isInfixOf` n = True
    isPhi _ = False  
    filterPhis ops = filter isPhi ops
    getPhi ln =
      let
        arr = case ln of
                ThreeAddressForm targ bop ls rs
                  | bop /= CAST -> [targ,ls,rs]
                  | otherwise -> [targ,rs]
                TwoAddressForm targ _ ar -> [targ,ar]
                SingleOperandForm uop ar | uop == OUTPUT -> [ar]
                TargetOnlyForm targ niop | niop == INPUT -> [targ]
                CondBranchForm cond _ _ -> [cond]
                other -> []
      in filterPhis arr
    allPhis = foldl1 union $ map getPhi oldLines
    phiLines = map  (\(Identifier str vt) ->
                      let
                        (name, phiPart) = break ('.' ==) str
                        tmpPart = drop 5 phiPart
                        phiOptsStr = take (length tmpPart-1) tmpPart
                        phiStrList = splitOn ',' phiOptsStr
                        --phiStrList' = map (\x -> replaceAll x "-1" "entry") phiStrList
                        phiPairList = map (\str -> (read (last $ splitOn '.' str)  :: Int, Identifier str vt)) phiStrList
                      in
                        PhiForm (Identifier (name ++ ".phi") vt) phiPairList) allPhis
    replacePhiStr (Identifier n vt) | ("phi" `isInfixOf` n) = Identifier ((fst $ break ('.' ==) n) ++ ".phi") vt
    replacePhiStr p = p
    (fstLine:filteredLines) = reverse $ map (applyToLineArguments replacePhiStr) oldLines
  in
    BasicBlock (BBB.ind bb) (exitsTo bb) $ reverse ((fstLine:phiLines) ++ filteredLines)


---------------- BEGIN PASS 2 Code -----------------------

createOperand2 :: Operand -> OperandState -> Operand
createOperand2 oldOp@(Constant vt val) _ = oldOp
createOperand2 oldOp@(Identifier oldName vt) opState =
  case opState of
    NumberedSelf num _ -> oldOp
    NumberedForeign id _ _ _ -> Identifier (showIdent id) (varType id)
    ConstantVal val _ -> Constant vt val

-- NOTE: need to think about whether or not this presents a danger for messing up any forwarding
removeFinalAssigns :: StateVector -> BasicBlock -> AnnotatedBasicBlock
removeFinalAssigns oldSV oldBB =
  let
    currBlkNum = BBB.ind oldBB
    incCount' = incCount currBlkNum
    foldFunc (newLines, newSV) currLine =
      let (nl, nv)  = case currLine of
                        ThreeAddressForm targ bop ls rs
                          | bop /= CAST ->
                            let
                              (lsState, tempVec) = getOrInitCount ls newSV currBlkNum
                              (rsState, tempVec2) = getOrInitCount rs tempVec currBlkNum
                              newVec = HashMap.alter incCount' targ tempVec2
                              targState = newVec ! targ
                            in
                              (ThreeAddressForm (createOperand2 targ targState) bop (createOperand2 ls lsState) (createOperand2 rs rsState), newVec)
                          | otherwise -> 
                            let
                              (rsState, tempVec) = getOrInitCount rs newSV currBlkNum
                              newVec = HashMap.alter incCount' targ tempVec
                              targState = newVec ! targ
                            in
                              (ThreeAddressForm (createOperand2 targ targState) bop ls (createOperand2 rs rsState), newVec)
                        TwoAddressForm targ uop ar
                          | uop == ASSIGN ->
                            let
                              (arState, tempVec) = getOrInitCount ar newSV currBlkNum
                              (oldTargState, tempVec2) = getOrInitCount targ tempVec currBlkNum
                              newVec = HashMap.insert targ (mkOpState oldTargState (ar, arState)) tempVec2
                              targState = newVec ! targ
                              newTarg = createOperand2 targ targState
                              newAr = createOperand2 ar arState
                            in 
                              if newTarg == newAr
                              then (TargetOnlyForm (Constant ST.NOTAPP "") NOP , newVec)
                              else (TwoAddressForm newTarg uop newAr, newVec)
                              
                          | otherwise -> 
                            let
                              (arState, tempVec) = getOrInitCount ar newSV currBlkNum
                              newVec = HashMap.alter incCount' targ tempVec
                              targState = newVec ! targ
                            in
                              (TwoAddressForm (createOperand2 targ targState) uop (createOperand2 ar arState), newVec)
                        SingleOperandForm uop ar | uop == OUTPUT -> 
                          let
                            (arState, newVec) = getOrInitCount ar newSV currBlkNum
                          in (SingleOperandForm uop (createOperand2 ar arState), newVec)
                        TargetOnlyForm targ niop | niop == INPUT ->
                          let
                            newVec = HashMap.alter incCount' targ newSV
                            targState = newVec ! targ
                          in
                            (TargetOnlyForm (createOperand2 targ targState) niop, newVec)
                        CondBranchForm cond lbl1 lbl2 ->
                          let
                            (condState, newVec) = getOrInitCount cond newSV currBlkNum
                          in (CondBranchForm (createOperand2 cond condState) lbl1 lbl2, newVec)
                        PhiForm targ optns ->
                          let
                            optsConverter (source, optn) =
                              case optn of
                                cv@(Constant _ _) -> (source, cv)
                                ident@(Identifier n vn) -> (source, createOperand2 ident (fst $ getOrInitCount ident newSV currBlkNum))
                            convertedOpts = map optsConverter optns
                          in 
                           (PhiForm targ convertedOpts, newSV) 
                        other -> (other, newSV)
      in (nl:newLines, nv)
    (newLines, newSV) = foldl foldFunc ([], oldSV) $ (reverse . BBB.lines) oldBB
    removeNOPs ln =
      case ln of
        TargetOnlyForm _ niop | niop == NOP -> False
        other -> True
  in (BasicBlock (BBB.ind oldBB) (exitsTo oldBB) (filter removeNOPs newLines), newSV)

interBlockSSA2' :: BBLookupMap -> IntMap.IntMap [Int] -> [Int] -> [BasicBlock]
interBlockSSA2' bblm enterFromMap finalOrder = 
  let
    blockFunc currInd = forwardEntryState2 bblm enterFromMap currInd
    finalBBs = map blockFunc $ (reverse . IntMap.keys) $ bblm
    finalBBLM = foldl (\lm bb -> IntMap.insert (BBB.ind bb) bb lm) IntMap.empty finalBBs
  in map (\ind -> finalBBLM IntMap.! ind) finalOrder

-- This is really simiple now -- all version numbering is encoded in the name during this pass
showOpState2 :: String -> OperandState -> String
showOpState2 currVarName (NumberedSelf num bNum) = currVarName
showOpState2 currVarName (NumberedForeign fOp num _ bNum) = (showIdent fOp)
showOpState2 currVarName (ConstantVal v _) = v

{-
# NOINLINE traceAndPause #
traceAndPause :: String -> a -> a
traceAndPause string expr = unsafePerformIO $ do
  traceIO string
  getLine
  return expr 
-}

createForwardIdentifier2 :: Int -> Operand -> BBLookupMap -> IntMap.IntMap [Int] -> Operand
createForwardIdentifier2 currBlkNum oldId@(Identifier oldName vt) bblm efm =
  let 
    entersFrom = IntMap.findWithDefault [] currBlkNum efm
    foldFunc (checkedList, poss) indToCheck
      | indToCheck `elem` checkedList = (checkedList, poss)
      | otherwise = 
        let
          currExitVec = snd $ bblm IntMap.! indToCheck
          res = HashMap.lookup oldId currExitVec
          thisEntersFrom = IntMap.findWithDefault [] indToCheck efm
        in
          case res of
            Just p -> (indToCheck:checkedList, ([p] `union` poss))
            Nothing -> foldl foldFunc (indToCheck:checkedList, poss) thisEntersFrom
    resultsForThisBlk = foldFunc ([],[]) currBlkNum
    rawOptions' = snd $ foldl foldFunc resultsForThisBlk entersFrom
    rawOptions = if rawOptions' == [] then [NumberedSelf 0 currBlkNum] else rawOptions'
    fwdVal = rawOptions !! 0
    --makeOptsStr [opt] = (showOpState2 name opt)
    --makeOptsStr (opt:optns) = (makeOptsStr [opt]) ++ "," ++ (makeOptsStr optns)
  in
    case fwdVal of
      NumberedSelf _ _ -> oldId
      --Phi optns -> Identifier (name ++ ".phi[" ++ (makeOptsStr optns) ++ "]") vt
      NumberedForeign fOp fInst _ bNum -> Identifier (showIdent fOp) vt
      ConstantVal v _ -> Constant vt v
createForwardIdentifier2 _ cv@(Constant _ _) _ _ = cv

forwardEntryState2 :: BBLookupMap -> IntMap.IntMap [Int] -> Int -> BasicBlock
forwardEntryState2 bblm efm currInd =
  let
    oldBlock = fst $ bblm IntMap.! currInd
    oldLines = BBB.lines oldBlock
    lineForwardFunc ln = 
      case ln of
        ThreeAddressForm targ bop ls rs
          | bop /= CAST -> ThreeAddressForm (createForwardIdentifier2 currInd targ bblm efm) bop (createForwardIdentifier2 currInd ls bblm efm) (createForwardIdentifier2 currInd rs bblm efm) 
          | otherwise -> ThreeAddressForm (createForwardIdentifier2 currInd targ bblm efm) bop ls (createForwardIdentifier2 currInd rs bblm efm)
        TwoAddressForm targ uop ar -> TwoAddressForm (createForwardIdentifier2 currInd targ bblm efm) uop (createForwardIdentifier2 currInd ar bblm efm)
        SingleOperandForm uop ar | uop == OUTPUT -> SingleOperandForm uop (createForwardIdentifier2 currInd ar bblm efm)
        TargetOnlyForm targ niop | niop == INPUT -> TargetOnlyForm (createForwardIdentifier2 currInd targ bblm efm) niop
        CondBranchForm cond lbl1 lbl2 -> CondBranchForm (createForwardIdentifier2 currInd cond bblm efm) lbl1 lbl2
        PhiForm targ optns -> PhiForm targ $ map (\(i, x) -> (i,createForwardIdentifier2 currInd x bblm efm)) optns
        other -> ln
    newLines = map lineForwardFunc oldLines
  in BasicBlock (BBB.ind oldBlock) (exitsTo oldBlock) newLines
