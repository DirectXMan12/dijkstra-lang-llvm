{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module BaseDijkstra.TPMTest (prog, STM(..),vardecl,fullProg) where

import BaseDijkstra.NaryTreeZipper
import BaseDijkstra.Parser (DijkstraTree(..), NodeType(..))
import BaseDijkstra.NaryTreeZipper.Instances
import BaseDijkstra.TreePatternMatcher
import BaseDijkstra.SymbolTable
import qualified Data.IntMap as IntMap
import qualified Data.HashMap.Lazy as HashMap
import Data.Monoid

data STM = STM SymbolTable (Maybe DijkstraTree) deriving (Show)

instance Monoid STM where
  mempty = STM (SymbolTable IntMap.empty HashMap.empty) Nothing

  STM (SymbolTable entries1 subtbls1) Nothing `mappend` STM (SymbolTable entries2 subtbls2) Nothing =
    STM (SymbolTable (entries1 `mappend` entries2) (subtbls1 `mappend` subtbls2)) Nothing

  STM (SymbolTable entries1 subtbls1) Nothing `mappend` STM subst (Just node) =
    STM (SymbolTable entries1 (HashMap.insert node subst subtbls1)) Nothing

  STM subst (Just node) `mappend` STM (SymbolTable entries1 subtbls1) Nothing =
    STM (SymbolTable entries1 (HashMap.insert node subst subtbls1)) Nothing

  STM subst1 (Just node1) `mappend` STM subst2 (Just node2) = 
    (mempty `mappend` STM subst1 (Just node1)) `mappend` STM subst2 (Just node2)

type DjkTreeParser a = TreeParser DijkstraTree NodeType () a

fullProg :: DjkTreeParser STM
fullProg = do
  branchOfType PROGRAM; _V_; 
  visitNextSibling; 
  prog

prog :: DjkTreeParser STM
prog = progLines

progLines :: DjkTreeParser STM
progLines = do
  tbls <- many1 (stmt <|> decl <|> anystmt)
  return (foldr mappend mempty tbls)

stmt :: DjkTreeParser STM
stmt = blockstmt

decl :: DjkTreeParser STM
decl = vardecl

anystmt :: DjkTreeParser STM
anystmt = anybn <|> anyln

anybn :: DjkTreeParser STM
anybn = anyBranch <|-> progLines {- do
  anyBranch
  _V_
  res <- progLines
  _A_
  return res -}

anyln :: DjkTreeParser STM
anyln = do
  anyLeaf
  return mempty

anyLeaf :: DjkTreeParser (NodeType, String)
anyLeaf =
  TreeParser (\(tree,crumbs) st ->
    case tree of
      LeafNode lt lv -> Ok (tree, crumbs) st (lt,lv)
      BranchNode _ _ _ -> Error (tree, crumbs) st ("Could not match any leaf"))

anyBranch :: DjkTreeParser NodeType
anyBranch =
  TreeParser (\(tree,crumbs) st ->
    case tree of
      BranchNode bt _ _ -> Ok (tree, crumbs) st bt
      LeafNode _ _ -> Error (tree, crumbs) st ("Could not match any branch")) 

vardecl :: DjkTreeParser STM
vardecl = do
  (tp:chldrn) <- branchOfType VARDECL <|-> (leafOfType TYPE <:> many1 (leafOfType ID))
  let symtbl = insertVars IntMap.empty (caseInsensitiveRead tp) chldrn
  return (STM (SymbolTable symtbl HashMap.empty) Nothing)

blockstmt :: DjkTreeParser STM
blockstmt = do
  node <- blockNode
  _V_
  (STM subtbl _) <- progLines {- _ will just be 'Nothing', since the result of all mappend ops is always a `Nothing` -}
  _A_
  return (STM subtbl (Just node))

blockNode :: DjkTreeParser DijkstraTree
blockNode = do
  branchOfType BLOCK
  node <- getCurrNode
  return node
