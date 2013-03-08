{-# LANGUAGE FunctionalDependencies #-}
module BaseDijkstra.NaryTreeZipper (NaryTree, NaryTreeCrumb(..), NaryTreeZipper, ntToUp, ntToNthChild, ntToNextSibling, ntToPrevSibling, children, nodeValue, createBranch, nodeContents) where

class NaryTree tr cst nv | tr -> cst nv where
  children :: tr -> [tr]
  nodeValue :: tr -> cst
  createBranch :: cst -> [tr] -> tr
  nodeContents :: tr -> nv

data NaryTreeCrumb tr cst = NaryTreeCrumb cst [tr] [tr] deriving (Show, Eq)

type NaryTreeZipper tr cst = (tr, [NaryTreeCrumb tr cst])

ntToUp :: (NaryTree tr cst nv) => NaryTreeZipper tr cst -> NaryTreeZipper tr cst
ntToUp (item, (NaryTreeCrumb cst ls rs):bs) = (createBranch cst (ls ++ [item] ++ rs), bs)

ntToNthChild :: (NaryTree tr cst nv) => Int -> NaryTreeZipper tr cst -> NaryTreeZipper tr cst
ntToNthChild i (oldItem, bs) = 
  (item, (NaryTreeCrumb cst ls rs):bs)
  where
    cst = nodeValue oldItem
    chldrn = children oldItem
    (ls, item:rs) = splitAt i chldrn

ntToNextSibling :: (NaryTree tr cst nv) => NaryTreeZipper tr cst -> NaryTreeZipper tr cst
ntToNextSibling (oldItem, (NaryTreeCrumb cst ls (fi:rs)):bs) =
  (fi, (NaryTreeCrumb cst (ls ++ [oldItem]) rs):bs)

ntToPrevSibling :: (NaryTree tr cst nv) => NaryTreeZipper tr cst -> NaryTreeZipper tr cst
ntToPrevSibling (oldItem, (NaryTreeCrumb cst ls rs):bs) =
  (li, (NaryTreeCrumb cst ls' (oldItem:rs)):bs)
  where
    ls' = take ((length ls)-1) ls
    li = last ls
