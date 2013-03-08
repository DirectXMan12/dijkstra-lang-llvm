module BaseDijkstra.TAFScanner (GenTAFParser, gen1ATOp, gen1AAOp, gen2AOp, binaryOperation, unaryOperation, label, branch, cast, Token, anyTAFWithTempTarget, anyTAFEntry, taftoken, phi) where

import Text.ParserCombinators.Parsec hiding (label)
import BaseDijkstra.ThreeAddressForm
import qualified BaseDijkstra.SymbolTable as ST (VariableType(..))

type Token = (SourcePos, ThreeAddressRecord)

type GenTAFParser rt st = GenParser Token st rt

taftoken :: (ThreeAddressRecord -> Maybe a) -> GenTAFParser a st
taftoken tokTest = do
  token showToken posToken testToken
  where
    showToken (_, tok) = show tok
    posToken (pos, _) = pos
    testToken (_, tok) = tokTest tok

gen1AAOp :: UnaryOperator -> GenTAFParser Operand st
gen1AAOp op =
  taftoken (\tok -> case tok of
    SingleOperandForm uop l | uop == op -> Just l
    other -> Nothing)

gen1ATOp :: NilaryOperator -> GenTAFParser Operand st
gen1ATOp op = 
  taftoken (\tok -> case tok of
    TargetOnlyForm targ niop | niop == op -> Just targ
    other -> Nothing)

gen2AOp :: UnaryOperator -> GenTAFParser (Operand, Operand) st
gen2AOp op =
  taftoken (\tok -> case tok of
    TwoAddressForm targ uop l | uop == op -> Just (targ, l)
    other -> Nothing)

anyTAFEntry :: GenTAFParser ThreeAddressRecord st
anyTAFEntry = taftoken (\tok -> Just tok)

anyTAFWithTempTarget :: GenTAFParser ThreeAddressRecord st
anyTAFWithTempTarget =
  taftoken (\tok -> case tok of
    ThreeAddressForm (TemporaryIdentifier _ _) _ _ _ -> Just tok
    TwoAddressForm (TemporaryIdentifier _ _) _ _ -> Just tok
    other -> Nothing)

binaryOperation :: BinaryOperator -> GenTAFParser (Operand, Operand, Operand) st
binaryOperation op =
  taftoken (\tok -> case tok of
    ThreeAddressForm targ bop l r | bop == op -> Just (targ, l, r)
    other -> Nothing)

unaryOperation :: UnaryOperator -> GenTAFParser (Operand, Operand) st
unaryOperation op = 
  taftoken (\tok -> case tok of
    TwoAddressForm targ uop l | uop == op -> Just (targ, l)
    other -> Nothing)

label :: GenTAFParser Int st
label = 
  taftoken (\tok -> case tok of
    LabelForm lid -> Just lid
    other -> Nothing)

branch :: GenTAFParser (Maybe (Operand, Operand), Operand) st
branch =
  taftoken (\tok -> case tok of
    CondBranchForm cnd lbl1 lbl2 -> Just (Just (cnd, lbl2), lbl1)
    UncondBranchForm lbl -> Just (Nothing, lbl)
    other -> Nothing)

phi :: GenTAFParser (Operand, [(Int, Operand)]) st
phi = 
  taftoken (\tok -> case tok of
    PhiForm targ optns -> Just (targ, optns)
    other -> Nothing)

cast :: GenTAFParser (Operand, ST.VariableType, Operand) st
cast = 
  taftoken (\tok -> case tok of
    ThreeAddressForm targ bop (Identifier strType _) r | bop == CAST -> Just (targ, (read strType :: ST.VariableType), r)
    other -> Nothing)
