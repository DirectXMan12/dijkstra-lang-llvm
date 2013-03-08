{-# LANGUAGE FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
module BaseDijkstra.NaryTreeZipper.Instances () where

import qualified BaseDijkstra.Parser as P (DijkstraTree(..))
import BaseDijkstra.Parser (NodeType(..))
import BaseDijkstra.NaryTreeZipper
import qualified BaseDijkstra.TypedDijkstraTree as TDT
import BaseDijkstra.SymbolTable (VariableType(..))

instance NaryTree P.DijkstraTree NodeType String where
  children (P.BranchNode tt cs _) = cs
  children (P.LeafNode _ _) = error "Tried to call 'children' on a leaf node!"
  nodeValue (P.BranchNode tt _ _) = tt
  nodeValue (P.LeafNode tt _) = tt
  createBranch tt cs = P.BranchNode tt cs Nothing
  nodeContents (P.LeafNode _ lv) = lv
  nodeContents (P.BranchNode _ _ _) = error "Tried to call 'nodeContents' on a branch node!"
 
instance NaryTree TDT.TypedDijkstraTree NodeType String where
  children (TDT.BranchNode tt cs _ _) = cs
  children (TDT.LeafNode _ _ _) = error "Tried to call 'children' on a leaf node!"
  nodeValue (TDT.BranchNode tt _ _ _) = tt
  nodeValue (TDT.LeafNode tt _ _) = tt
  createBranch tt cs = TDT.BranchNode tt cs Nothing NOTAPP
  nodeContents (TDT.LeafNode _ lv _ ) = lv
  nodeContents (TDT.BranchNode _ _ _ _) = error "Tried to call 'nodeContents' on a branch node!"
