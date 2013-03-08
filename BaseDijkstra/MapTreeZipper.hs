{-# LANGUAGE FunctionalDependencies #-}
module BaseDijkstra.MapTreeZipper (MapTree, MapTreeCrumb(..), MapTreeZipper, mtToUp, mtToChild, children, nodeValue, createBranch) where
import Data.HashMap.Lazy as HashMap
import Data.Hashable

class MapTree tr cst k | tr -> cst k where
  children :: (Hashable k) => tr -> HashMap.HashMap k tr
  nodeValue :: tr -> cst
  createBranch :: (Hashable k) => cst -> (HashMap.HashMap k tr) -> tr

data MapTreeCrumb tr cst k = MapTreeCrumb cst k (HashMap.HashMap k tr) deriving (Show)
type MapTreeZipper tr cst k = ((k,tr), [MapTreeCrumb tr cst k])

mtToUp :: (Eq k, Hashable k, MapTree tr cst k) => MapTreeZipper tr cst k -> MapTreeZipper tr cst k
mtToUp ((k,v), (MapTreeCrumb cst nk cm):bs) =
  ((nk,createBranch cst newm), bs)
  where newm = HashMap.insert k v cm

mtToChild :: (Hashable k, Eq k, MapTree tr cst k) => k -> MapTreeZipper tr cst k -> MapTreeZipper tr cst k
mtToChild ki ((oldk, oldi), bs) =
  ((ki,item), (MapTreeCrumb cst oldk nm):bs)
  where
    cst = nodeValue oldi
    chld = children oldi
    nm = HashMap.delete oldk chld
    (Just item) = HashMap.lookup ki chld
