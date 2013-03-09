module BaseDijkstra.CodeGenPass (prog, makeProgTokStream) where

import Text.ParserCombinators.Parsec hiding (label)
import Text.ParserCombinators.Parsec.Pos
import BaseDijkstra.TAFScanner
import BaseDijkstra.ThreeAddressForm
import BaseDijkstra.LLVMIRCode
import qualified BaseDijkstra.SymbolTable as ST (VariableType(..))
import qualified Data.HashMap as HashMap

type TAFParser = GenTAFParser [Char] (Int, HashMap.Map String Int)

makeProgTokStream :: [ThreeAddressRecord] -> [(Token)]
makeProgTokStream tafs = map (\(ind, taf) -> (newPos "TAF Stream" 1 ind, taf)) $ zip [0..] tafs

outputName :: Operand -> String
outputName (Identifier vn _) = "%"++(replaceAll vn "-1" "entry")
outputName (TemporaryIdentifier ind _) = "%temp"++(show ind)
outputName (Constant tp v)
  | tp == ST.BOOL && v == "true" = "1"
  | tp == ST.BOOL && v == "false" = "0"
  | otherwise = v
outputName (LabelIdentifier ind) = "%label"++(show ind)

-- From bluebones.net/2007/01/replace-in-haskell
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
  if take (length find) s == find
    then repl ++ (replace (drop (length find) s) find repl)
    else [head s] ++ (replace (tail s) find repl)

createStrs :: HashMap.Map String Int -> [String]
createStrs strmap =
  let
    replaceNL = (\str -> replace str "\n" "\\0A")
    foldFunc = (\str ind acc -> ("@.str" ++ (show ind) ++ " = private unnamed_addr constant [" ++ (show $ length str+1) ++ " x i8] c\"" ++ (replaceNL str) ++ "\\00\", align 1"):acc)
  in HashMap.foldWithKey foldFunc [] strmap 

prog :: TAFParser
prog = do
  (Identifier progName _) <- gen1AAOp PROGRAMDEF
  lines <- many1 progLine
  (_, strmap) <- getState
  return $ unlines $ (createStrs strmap) ++ [inputFuncStrs, inputFuncs] ++ ["define i32 @main() {", indent1 "entry: "] ++ map indent1 lines ++ [indent2 "ret i32 0", "}"]

indent :: Int -> String -> String
indent 1 str = "  " ++ str
indent n str = (replicate (2*n) ' ') ++ str

indent1 :: String -> String
indent1 = indent 1

indent2 :: String -> String
indent2 = indent 2

progLine :: TAFParser
progLine = arithExpr <|> cmpExpr <|> assignExpr <|> boolExpr <|> branchExpr <|> castExpr <|> labelExpr <|> ioExpr <|> phiLine

-- Shamelessly (ok, maybe with a bit of shame) taken from a bluebones.net post
replaceAll :: Eq a => [a] -> [a] -> [a] -> [a]
replaceAll [] _ _ = []
replaceAll s find repl =
    if take (length find) s == find
        then repl ++ (replaceAll (drop (length find) s) find repl)
        else [head s] ++ (replaceAll (tail s) find repl)

-- Adapted from the source of unwords
joinList :: [a] -> [[a]] -> [a]
joinList sep [] = []
joinList sep [w] = w
joinList sep (w:ws) = w ++ sep ++ (joinList sep ws)

phiLine :: TAFParser
phiLine = do
  (target, optns) <- phi 
  let fmtedOpts = map (\(lblNum, var) ->
                        let
                          strLbl = if lblNum == -1 then "entry" else "label" ++ (show lblNum)
                          replacedVar = outputName var
                        in
                          "[ " ++ replacedVar ++ ", %" ++ strLbl ++ " ]") optns
  let optsList = joinList ", " fmtedOpts 
  let vt =  case varType target of
              ST.INT -> "i64"
              ST.FLOAT -> "double"
              ST.BOOL -> "i1"
  return $ indent1 $ (outputName target) ++ " = phi " ++ vt ++ " " ++ optsList


binArith :: BinaryOperator -> String -> String -> TAFParser
binArith op iop fop = do
  (target, lhs, rhs) <- binaryOperation op
  let (opName, exType) = case (varType target) of ST.INT -> (iop, "i64")
                                                  ST.FLOAT -> (fop, "double")
  return $ indent1 $ (outputName target) ++ " = " ++ opName ++ " " ++ exType ++ " " ++ (outputName lhs) ++ ", " ++ (outputName rhs)

-- NOTE: the modulo operator here is assumed to be the remainder operator, since this is not clear from the specification (and LLVM has a built-in remainder operator).  See the LLVM documentation for more information
arithExpr :: TAFParser
arithExpr = (binArith PLUS "add" "fadd") <|> (binArith MINUS "sub" "fsub") <|> (binArith MULT "mul" "fmul") <|> (binArith IDIV "sdiv" "") <|> (binArith FDIV "" "fdiv") <|> (binArith MOD "srem" "") <|> negExpr

negExpr :: TAFParser
negExpr = do
  (target, arg) <- unaryOperation NEG
  let (opName, exType, otherParam) = case (varType target) of
                                       ST.INT -> ("sub", "i64", "0")
                                       ST.FLOAT -> ("fsub", "double", "-0.0")
  return $ indent1 $ (outputName target) ++ " = " ++ opName ++ " " ++ exType ++ " " ++ otherParam ++ ", " ++ (outputName arg)

cExpr :: BinaryOperator -> String -> String -> TAFParser
cExpr op iop fop = do
  (target, lhs, rhs) <- binaryOperation op
  let (opName, cmpType, exType) = case (varType lhs) of
                                    ST.INT -> ("icmp", iop, "i64")
                                    ST.FLOAT -> ("fcmp", fop, "double")
                                    ST.BOOL -> ("icmp", iop, "i1")
  return $ indent1 $ (outputName target) ++ " = " ++ opName ++ " " ++ cmpType ++ " " ++ exType ++ " " ++ (outputName lhs) ++ ", " ++ (outputName rhs)

cmpExpr :: TAFParser
cmpExpr = (cExpr CMP_EQ "eq" "oeq") <|> (cExpr CMP_NEQ "ne" "one") <|> (cExpr CMP_GT "sgt" "ogt") <|> (cExpr CMP_GE "sge" "oge") <|> (cExpr CMP_LT "slt" "olt") <|> (cExpr CMP_LE "sle" "ole") 

assignExpr :: TAFParser
assignExpr = do
  (target, val) <- unaryOperation ASSIGN
  fail $ "Left over duplicate assign statement: " ++ (outputName target) ++ " = " ++ (outputName val)
  {-let typeStr = case (varType target) of
                  ST.INT -> "i64"
                  ST.FLOAT -> "double"
                  ST.BOOL -> "i1"
  return $ indent1 $ "store " ++ typeStr ++ " " ++ (outputName val) ++ ", " ++ typeStr ++ "* " ++ (outputName target)   store i32 %4, i32* %b, align 4
  --return $ (outputName target) ++ " = " ++ (outputName val)-}

bExpr :: BinaryOperator -> String -> TAFParser
bExpr op sop = do
  (target, lhs, rhs) <- binaryOperation op
  return $ indent1 $ (outputName target) ++ " = " ++ "or il " ++ (outputName lhs) ++ ", " ++ (outputName rhs)

lnotExpr :: TAFParser
lnotExpr = do
  (target, arg) <- unaryOperation LNOT
  return $ indent1 $ (outputName target) ++ " = " ++ " xor il " ++ "1, " ++ (outputName arg)

boolExpr :: TAFParser
boolExpr = (bExpr AND "and") <|> (bExpr OR "or") <|> lnotExpr

branchExpr :: TAFParser
branchExpr = do
  (condPart, lbl1) <- branch
  case condPart of
    Nothing -> return $ indent1 $ "br label " ++ (outputName lbl1)
    Just (condVal, lbl2) -> return $ indent1 $ "br i1 " ++ (outputName condVal) ++ ", label " ++ (outputName lbl1) ++ ", label " ++ (outputName lbl2)

castExpr :: TAFParser
castExpr = do
  (target, castTo, orig) <- cast
  let (op, type1, type2) = case castTo of
                             ST.INT -> ("fptosi", "double", "i64")
                             ST.FLOAT -> ("sitofp", "i64", "double")
  return $ indent1 $ (outputName target) ++ " = " ++ op ++ " " ++ type1 ++ " " ++ (outputName orig) ++ " to " ++ type2

labelExpr :: TAFParser
labelExpr = do
  lid <- label
  return $ "label"++(show lid)++": "

ioExpr :: TAFParser
ioExpr = inputExpr <|> outputExpr <|> errExpr

getOrInitStrId :: String -> HashMap.Map String Int -> (Int, HashMap.Map String Int)
getOrInitStrId str strmap = 
  let
    entry = HashMap.lookup str strmap
  in case entry of
    (Just val) -> (val, strmap)
    Nothing -> let newVal = 1 + (HashMap.fold (\elem acc -> if elem > acc then elem else acc) 0 strmap) in (newVal, HashMap.insert str newVal strmap)

makePrint :: String -> Int -> String
makePrint str strid =
  "call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([" ++ (show $ length str + 1) ++ " x i8]* @.str" ++ (show strid) ++ ", i32 0, i32 0))"

makeInput :: Operand -> HashMap.Map String Int -> (String, HashMap.Map String Int)
makeInput var@(Identifier vn vt) strmap =
  let
    inputStr = "Enter " ++ vn ++ " ["++(show vt)++"]: "
    (inputFunc, paramType) = case vt of
      ST.INT -> ("inputInt", "i64")
      ST.FLOAT -> ("inputFloat", "double")
      ST.BOOL -> ("inputBool", "i1")
    (strId, newStrMap) = getOrInitStrId inputStr strmap
  in ((outputName var) ++ " = call " ++ paramType ++ " @" ++ inputFunc ++ "(i8* getelementptr inbounds ([" ++ (show $ length inputStr + 1) ++ " x i8]* @.str" ++ (show strId) ++ ", i32 0, i32 0))", newStrMap)


inputExpr :: TAFParser
inputExpr = do
  var@(Identifier varName varType) <- gen1ATOp INPUT
  (s2, strmap) <- getState
  let (res, newStrMap) = makeInput var strmap
  setState (s2, newStrMap)
  return $ indent1 res

errExpr :: TAFParser
errExpr = do
  (Identifier str _) <- gen1AAOp ERR
  (s2, strmap) <- getState
  let errStr = "Error: " ++ str ++ "\n"
  let (strId, newStrMap) = getOrInitStrId errStr strmap
  setState (s2, newStrMap)
  return $ indent1 $ (makePrint errStr strId) ++ "\nret i32 " ++ (show $ -1*(newStrMap HashMap.! errStr))

outputTypeStr :: ST.VariableType -> String
outputTypeStr ST.INT = "i64"
outputTypeStr ST.FLOAT = "double"
outputTypeStr ST.BOOL = "i1"

makeArgPrint :: String -> Int -> Operand -> String
makeArgPrint str strid arg =
  --let argsStr = foldl (\acc identifier -> acc ++ ", " ++ (outputTypeStr $ varType identifier) ++ " " ++ (outputName identifier)) "" args
  let argsStr = ", " ++ (outputTypeStr $ varType arg) ++ " " ++ (outputName arg)
  in "call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([" ++ (show $ length str + 1) ++ " x i8]* @.str" ++ (show strid) ++ ", i32 0, i32 0)"++ argsStr ++ ")"

outputExpr :: TAFParser
outputExpr = do
  var@(Identifier str vt) <- gen1AAOp OUTPUT 
  (s2, strmap) <- getState
  let fmtStr = (str ++ ": " ++ (formatStr vt) ++ "\n")
  let (strId, newStrMap) = getOrInitStrId fmtStr strmap
  setState (s2, newStrMap)
  return $ indent1 $ makeArgPrint fmtStr strId var
  where formatStr = (\vt ->
                      case vt of
                        ST.INT -> "%d"
                        ST.FLOAT -> "%f"
                        ST.BOOL -> "%d")
