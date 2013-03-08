{-# LANGUAGE DeriveGeneric, FunctionalDependencies, FlexibleInstances #-}
module BaseDijkstra.SymbolTable (VariableType(..), VariableSymbol(..), SymbolTable(..), caseInsensitiveRead, insertVars) where

import BaseDijkstra.Parser (DijkstraTree(..))
import qualified Data.IntMap as IntMap
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.Monoid
import GHC.Generics (Generic)
import Data.Char (toUpper)
import BaseDijkstra.MapTreeZipper

data VariableType = FLOAT | INT | BOOL | NUMERIC | UNDECIDED | NOTAPP deriving (Eq,Ord,Enum,Bounded,Show, Read, Generic)
instance Hashable VariableType

data VariableSymbol = VariableSymbol { name :: String, varType :: VariableType, hsh :: Int }
data SymbolTable = SymbolTable { variables :: IntMap.IntMap VariableSymbol, subScopes :: HashMap.HashMap DijkstraTree SymbolTable } deriving (Show)

caseInsensitiveRead :: (Read a) => String -> a
caseInsensitiveRead str = read (map toUpper str)

instance MapTree SymbolTable (IntMap.IntMap VariableSymbol) DijkstraTree where
  children (SymbolTable _ chldrn) = chldrn
  nodeValue (SymbolTable vars _) = vars
  createBranch vars mp = SymbolTable vars mp

-- Main Symbol Table

mkSymbol :: String -> VariableType -> VariableSymbol
mkSymbol name tp = VariableSymbol name tp (hash name)

instance Eq VariableSymbol where
  (VariableSymbol _ tp1 hsh1) == (VariableSymbol _ tp2 hsh2) = hsh1 == hsh2 && tp1 == tp2

instance Show VariableSymbol where
  show (VariableSymbol n tp _) = n ++ ":" ++ (show tp)

instance Ord VariableSymbol where
  (VariableSymbol n1 tp1 hsh1) `compare` (VariableSymbol n2 tp2 hsh2) = compare hsh1 hsh2 `mappend` compare tp1 tp2 `mappend` compare n1 n2


-- main code

insertVars :: IntMap.IntMap VariableSymbol -> VariableType -> [String] -> IntMap.IntMap VariableSymbol
insertVars st vt [] = st
insertVars st vt (vn:vs) = 
  let newSym = mkSymbol vn vt
  in insertVars (IntMap.insert (hsh newSym) newSym st) vt vs
