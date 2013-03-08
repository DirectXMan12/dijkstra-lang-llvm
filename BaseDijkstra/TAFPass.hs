module BaseDijkstra.TAFPass (prog) where

import BaseDijkstra.ThreeAddressForm hiding (UnaryOperator(..), BinaryOperator(..), NilaryOperator(..))
import qualified BaseDijkstra.ThreeAddressForm as TAF (UnaryOperator(..), BinaryOperator(..), NilaryOperator(..))
import BaseDijkstra.TPMTest hiding (DjkTreeParser, prog)
import BaseDijkstra.Parser (NodeType(..))
import BaseDijkstra.NaryTreeZipper.Instances
import BaseDijkstra.TreePatternMatcher hiding ((<|->))
import BaseDijkstra.TypedDijkstraTree
import qualified BaseDijkstra.SymbolTable as ST (VariableType(..))

type GenDjkTreeParser a = TreeParser TypedDijkstraTree NodeType TAFState a
type DjkTreeParser = GenDjkTreeParser [ThreeAddressRecord]

restoreType :: ST.VariableType -> GenDjkTreeParser ()
restoreType tp = TreeParser (\(BranchNode tt cs pos _, crumbs) st -> DidNothing (BranchNode tt cs pos tp, crumbs) st)

(<|->) :: GenDjkTreeParser a -> GenDjkTreeParser b -> GenDjkTreeParser b
ptp <|-> ctp = do
  ptp
  cn <- getCurrNode
  let cnEt = expressionType cn
  _V_
  res <- ctp
  _A_
  restoreType cnEt
  return res

-- WARNING: _V_ _A_ destroys expression type! Use carefully! The custom (<|->) above preserves expression type

prog :: DjkTreeParser 
prog = do
  branchOfType PROGRAM
  _V_
  progName <- leafOfType ID
  visitNextSibling
  chldrn <- many1 progLine
  _A_
  return ((SingleOperandForm TAF.PROGRAMDEF (Identifier progName ST.NOTAPP)):(filter filterChldrn $ concat chldrn))
  where
    filterChldrn x = 
      case x of
        TargetOnlyForm _ op -> if op == TAF.NOP then False else True
        otherwise -> True

progLine :: DjkTreeParser
progLine = decl <|> stmt

decl :: DjkTreeParser
decl = do
  (tp:names) <- branchOfType VARDECL <|-> (leafOfType TYPE <:> many1 (leafOfType ID) :: GenDjkTreeParser [[Char]])
  let nameNodes = map (\x -> LeafNode ID x ST.NOTAPP) names
  return [] {- No type declaration instructions since llvm doesn't actually care about declaring the type of local variables -}

stmt :: DjkTreeParser
stmt = blockStmt <|> assignStmt <|> caseStmt <|> loopStmt <|> inputStmt <|> outputStmt

assignStmt :: DjkTreeParser
assignStmt = do
  branchOfType ASSIGN
  _V_
  ids <- branchOfType VARS <|-> many1 (do { leafOfType ID; getCurrNode })
  visitNextSibling
  exprs <- branchOfType EXPRS <|-> many1 expr
  _A_
  return $ concat $ map (\(LeafNode _ varName varType, exs) -> let lastEx = last exs in (take (length exs - 1) exs) ++ [lastEx, TwoAddressForm (Identifier varName varType) TAF.ASSIGN (target lastEx)]) $ zip ids exprs

caseStmt :: DjkTreeParser
caseStmt = do
  branchOfType CASE {- Verify that we're ok, so that we can continue executing state ops -}
  st <- getUserState
  let (endLbl, newSt) = nextLabelId st
  setUserState newSt
  guards <- branchOfType CASE <|-> many1 (guardedStmt endLbl)
  return $ (concat guards) ++ [SingleOperandForm TAF.ERR (Identifier "abortNoAlternative" ST.NOTAPP), LabelForm endLbl]

guardedStmt :: Int -> DjkTreeParser
guardedStmt endLbl = do
  [cond, res] <- branchOfType WHERE <|-> (expr <+> stmt)
  st <- getUserState
  let (nextLbl, tmpSt) = nextLabelId st
  let (contLbl, newSt) = nextLabelId tmpSt
  setUserState newSt
  return $ cond ++ [CondBranchForm (target $ last cond) (LabelIdentifier contLbl) (LabelIdentifier nextLbl), LabelForm contLbl] ++ res ++ [UncondBranchForm (LabelIdentifier endLbl), LabelForm nextLbl]

loopStmt :: DjkTreeParser
loopStmt = do
  branchOfType LOOP {- Verify that we're ok, so that we can continue executing state ops -}
  st <- getUserState
  let (topLbl, tmpSt) = nextLabelId st
  let (endLbl, newSt) = nextLabelId tmpSt
  setUserState newSt
  guards <- branchOfType LOOP <|-> many1 (guardedStmt topLbl)
  return $ (LabelForm topLbl):(concat guards) ++ [LabelForm endLbl]

outputStmt :: DjkTreeParser
outputStmt = do
  ex <- branchOfType PRINT <|-> expr
  return $ ex ++ [SingleOperandForm TAF.OUTPUT (target $ last ex)]

inputStmt :: DjkTreeParser
inputStmt = do
  ids <- branchOfType INPUT <|-> many1 (do { leafOfType ID; getCurrNode })
  return $ map (\(LeafNode _ val et) -> TargetOnlyForm (Identifier val et) TAF.INPUT) ids

expr :: DjkTreeParser
expr = castExpr <|> orExpr <|> andExpr <|> eqExpr <|> relExpr <|> addExpr <|> multExpr <|> unaryExpr <|> primaryExpr

castExpr :: DjkTreeParser
castExpr = do
  [castToNode, ex] <- branchOfType CAST <|-> (do { nv <- leafOfType TYPE; return [TargetOnlyForm (Identifier nv ST.NOTAPP) TAF.NOP]; } <+> expr)
  st <- getUserState
  let (resId, nextSt) = nextTempId st
  setUserState nextSt
  return $ ex ++ [ThreeAddressForm (TemporaryIdentifier resId (read $ name $ target $ head castToNode :: ST.VariableType)) TAF.CAST (target $ head castToNode) (target $ last ex)]

echoExpr :: NodeType -> TAF.BinaryOperator -> DjkTreeParser
echoExpr nt exprOp = do
  [exs1,exs2] <- branchOfType nt <|-> (expr <+> expr)
  st <- getUserState
  let (resId, nextSt) = nextTempId st
  setUserState nextSt
  return $ exs1 ++ exs2 ++ [(ThreeAddressForm (TemporaryIdentifier resId (varType $ target $ last exs1)) exprOp (target $ last exs1) (target $ last exs2))]

echoExprWT :: NodeType -> TAF.BinaryOperator -> ST.VariableType -> DjkTreeParser
echoExprWT nt exprOp vt = do
  [exs1,exs2] <- branchOfType nt <|-> (expr <+> expr)
  st <- getUserState
  let (resId, nextSt) = nextTempId st
  setUserState nextSt
  return $ exs1 ++ exs2 ++ [(ThreeAddressForm (TemporaryIdentifier resId vt) exprOp (target $ last exs1) (target $ last exs2))]


orExpr :: DjkTreeParser
orExpr = echoExpr OR TAF.OR

andExpr :: DjkTreeParser
andExpr = echoExpr AND TAF.AND

eqExpr :: DjkTreeParser
eqExpr = (echoExprWT CMP_EQ TAF.CMP_EQ ST.BOOL) <|> (echoExprWT CMP_NEQ TAF.CMP_NEQ ST.BOOL)

relExpr :: DjkTreeParser
relExpr = (enb CMP_GT TAF.CMP_GT) <|> (enb CMP_LT TAF.CMP_LT) <|> (enb CMP_GE TAF.CMP_GE) <|> (enb CMP_LE TAF.CMP_LE)
  where enb t1 t2 = echoExprWT t1 t2 ST.BOOL

addExpr :: DjkTreeParser
addExpr = (echoExpr PLUS TAF.PLUS) <|> (echoExpr MINUS TAF.MINUS)

multExpr :: DjkTreeParser
multExpr = (echoExpr MULT TAF.MULT) <|> (echoExpr FDIV TAF.FDIV) <|> (echoExpr IDIV TAF.IDIV) <|> (echoExpr MOD TAF.MOD)

unaryExpr :: DjkTreeParser
unaryExpr = negExpr <|> lnotExpr

lnotExpr :: DjkTreeParser
lnotExpr = do
  ex <- branchOfType LNOT <|-> expr; 
  st <- getUserState
  let (resId, nextSt) = nextTempId st
  setUserState nextSt
  return $ ex ++ [TwoAddressForm (TemporaryIdentifier resId ST.BOOL) TAF.LNOT (target $ last ex)]

negExpr :: DjkTreeParser
negExpr = do
  ex <- branchOfType NEG <|-> expr
  st <- getUserState
  let (resId, nextSt) = nextTempId st
  setUserState nextSt
  return $ ex ++ [TwoAddressForm (TemporaryIdentifier resId (varType $ target $ last ex)) TAF.NEG (target $ last ex)]

primaryExpr :: DjkTreeParser
primaryExpr = constant <|> varLookup

varLookup :: DjkTreeParser
varLookup = do
  (LeafNode _ vn vt) <- do { leafOfType ID; getCurrNode }
  return [TargetOnlyForm (Identifier vn vt) TAF.NOP]

constant :: DjkTreeParser
constant = echo1Expr FLOAT ST.FLOAT <|> echo1Expr INT ST.INT <|> echo1Expr TRUE ST.BOOL <|> echo1Expr FALSE ST.BOOL
  where echo1Expr nt vt = leafOfType nt >>= (\val -> return [TargetOnlyForm (Constant vt val) TAF.NOP])

blockStmt :: DjkTreeParser
blockStmt = do
  chldrn <- branchOfType BLOCK <|-> many1 progLine
  return $ concat chldrn

