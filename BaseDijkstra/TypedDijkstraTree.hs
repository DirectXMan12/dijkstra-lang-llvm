{-# LANGUAGE DeriveGeneric #-}
module BaseDijkstra.TypedDijkstraTree (TypedDijkstraTree(..)) where

import BaseDijkstra.Parser (NodeType(..))
import BaseDijkstra.SymbolTable (VariableType(..))
import GHC.Generics (Generic)
import Text.Parsec.Pos (SourcePos(..))
import Data.Hashable

data TypedDijkstraTree = BranchNode {nodeType :: NodeType, children :: [TypedDijkstraTree], posInfo :: Maybe SourcePos, expressionType :: VariableType } | LeafNode {nodeType :: NodeType, value :: String, expressionType :: VariableType } deriving (Eq, Generic)

instance Hashable TypedDijkstraTree

instance Show TypedDijkstraTree where
  show (LeafNode nt val NOTAPP) = "(" ++ (show nt) ++ " " ++ val ++ ")"
  show (LeafNode nt val exprType) = "(" ++ (show nt) ++ "->" ++ (show exprType) ++ " " ++ val ++ ")"
  show (BranchNode nt cs _ NOTAPP) = "(" ++ (show nt) ++ " " ++ (unwords $ map show cs) ++ ")"
  show (BranchNode nt cs _ exprType) = "(" ++ (show nt) ++ "->" ++ (show exprType) ++ " " ++ (unwords $ map show cs) ++ ")"
