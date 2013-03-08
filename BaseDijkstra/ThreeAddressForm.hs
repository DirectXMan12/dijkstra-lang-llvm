{-# LANGUAGE DeriveGeneric #-}
module BaseDijkstra.ThreeAddressForm (ThreeAddressRecord(..), Operand(..), BinaryOperator(..), UnaryOperator(..), NilaryOperator(..), TAFState, nextTempId, nextLabelId) where

import qualified BaseDijkstra.SymbolTable as ST (VariableType(..))
import Data.Hashable
import GHC.Generics (Generic)

data ThreeAddressRecord = ThreeAddressForm { target :: Operand, binop :: BinaryOperator, lhs :: Operand, rhs :: Operand } 
                        | TwoAddressForm { target :: Operand, unop :: UnaryOperator, arg :: Operand } 
                        | TargetOnlyForm { target :: Operand, nilop :: NilaryOperator }
                        | SingleOperandForm { unop :: UnaryOperator, arg :: Operand }
                        | LabelForm { lblId :: Int }
                        | CondBranchForm { cond :: Operand, label1 :: Operand, label2 :: Operand }
                        | UncondBranchForm { label1 :: Operand }
                        | PhiForm { target :: Operand, opts :: [(Int, Operand)] } deriving (Eq)

instance Show ThreeAddressRecord where
  show (ThreeAddressForm t bop l r) = (show t) ++ "<-" ++ (show l) ++ " " ++ (show bop) ++ " " ++ (show r)
  show (TwoAddressForm t uop l) = (show t) ++ "<-" ++ (if uop /= ASSIGN then show uop else "") ++ " " ++ (show l)
  show (TargetOnlyForm t niop) = (show t) ++ "<-" ++ (show niop)
  show (SingleOperandForm uop l) = (show uop) ++ " " ++ (show l)
  show (CondBranchForm c l1 l2) = "br " ++ (show c) ++ " ? " ++ (show l1) ++ " : " ++ (show l2)
  show (UncondBranchForm l) = "br " ++ (show l)
  show (LabelForm l) = "label"++(show l)++": "
  show (PhiForm targ po) = (show targ) ++ "<-phi" ++ (show po)

data Operand  = Identifier { name :: String, varType :: ST.VariableType }
              | TemporaryIdentifier { ind :: Int, varType :: ST.VariableType} 
              | Constant { varType :: ST.VariableType, val :: String } 
              | LabelIdentifier Int deriving (Eq, Generic, Ord)

instance Hashable Operand

instance Show Operand where
  show (Identifier vn vt) = "%"++vn++ (if vt /= ST.NOTAPP then "@"++(show vt) else "")
  show (TemporaryIdentifier ind tp) = "%temp"++(show ind)++(if tp /= ST.NOTAPP then "@"++(show tp) else "")
  show (Constant tp v) = "const:" ++ v ++ (if tp /= ST.NOTAPP then "@" ++ (show tp) else "")
  show (LabelIdentifier ind) = "label" ++ (show ind)

data BinaryOperator = PLUS | MINUS | MULT | FDIV | IDIV | MOD | CMP_EQ | CMP_NEQ | CMP_GT | CMP_LT | CMP_GE | CMP_LE | OR | AND | CAST deriving (Eq, Ord, Enum, Bounded, Show, Read)
data UnaryOperator = OUTPUT | ERR | ASSIGN | PROGRAMDEF | LNOT | NEG deriving (Eq, Ord, Enum, Bounded, Show, Read)
data NilaryOperator = INPUT | NOP deriving (Eq, Ord, Enum, Bounded, Show, Read)

type TAFState = (Int, Int)

nextLabelId :: TAFState -> (Int, TAFState) 
nextLabelId (lblInd, tempIdInd) = (lblInd, (lblInd+1, tempIdInd))

nextTempId :: TAFState -> (Int, TAFState)
nextTempId (lblInd, tempIdInd) = (tempIdInd, (lblInd, tempIdInd+1))
