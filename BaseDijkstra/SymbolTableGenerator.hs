{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
module BaseDijkstra.SymbolTableGenerator (DjkTreeZipper(..), SymTblZipper(..), buildSymbolTable, SymbolTable(..)) where
import BaseDijkstra.Parser as P
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.Monoid
import qualified Data.IntMap as IntMap
import BaseDijkstra.NaryTreeZipper
import BaseDijkstra.MapTreeZipper
import Data.Char

-- DijkstraTree Zipper
instance NaryTree P.DijkstraTree P.NodeType String where
  children (BranchNode tt cs _) = cs
  children (LeafNode _ _) = error "Tried to call 'children' on a leaf node!"
  nodeValue (BranchNode tt _ _) = tt
  nodeValue (LeafNode tt _) = tt
  createBranch tt cs = BranchNode tt cs Nothing

type DjkTreeZipper = NaryTreeZipper P.DijkstraTree P.NodeType
--
-- SymbolTable Zipper
data VariableType = FLOAT | INT | BOOL | NUMERIC | UNDECIDED deriving (Eq,Ord,Enum,Bounded,Show, Read)
data VariableSymbol = VariableSymbol { name :: String, varType :: VariableType, hsh :: Int }
data SymbolTable = SymbolTable { variables :: IntMap.IntMap VariableSymbol, subScopes :: HashMap.HashMap DijkstraTree SymbolTable } deriving (Show)

caseInsensitiveRead :: (Read a) => String -> a
caseInsensitiveRead str = read (map toUpper str)

type VarSymMap = IntMap.IntMap VariableSymbol

instance MapTree SymbolTable (IntMap.IntMap VariableSymbol) DijkstraTree where
  children (SymbolTable vars ss) = ss  
  nodeValue (SymbolTable vars ss) = vars
  createBranch vars ss = SymbolTable vars ss

type SymTblZipper = MapTreeZipper SymbolTable (IntMap.IntMap VariableSymbol) DijkstraTree

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

makeSymTable :: DijkstraTree -> SymTblZipper -> SymTblZipper
makeSymTable node ((currK, currTbl), stCrumbs) =
  mtToChild node newZip
  where
    newZip = ((currK, newTbl), stCrumbs)
    newTbl = SymbolTable oldVars newChildren
    newChildren = HashMap.insert node (SymbolTable IntMap.empty HashMap.empty) oldChildren
    (SymbolTable oldVars oldChildren) = currTbl

runOnEachChild :: Int -> (DjkTreeZipper, SymTblZipper) -> ((DjkTreeZipper, SymTblZipper) -> (DjkTreeZipper, SymTblZipper)) -> (DjkTreeZipper, SymTblZipper)    
runOnEachChild chldI ((currTree, djkCrumbs), stz) fun =
  if chldI < (length . BaseDijkstra.NaryTreeZipper.children) currTree
    then 
      let
        (tZip, stZip) = (fun (ntToNthChild chldI (currTree, djkCrumbs), stz))
        newZip = (ntToUp tZip, stZip)
      in
        runOnEachChild (chldI+1) newZip fun
  else
    ((currTree, djkCrumbs), stz)

buildSymbolTable' :: (DjkTreeZipper, SymTblZipper) -> (DjkTreeZipper, SymTblZipper)
buildSymbolTable' ((currTree, djkCrumbs), ((currK, SymbolTable currVars currChldTbls), stCrumbs)) = 
  case currTree of
    (BranchNode P.VARDECL ((LeafNode P.TYPE varType):varNames) _) -> 
      ((currTree, djkCrumbs), ((currK, SymbolTable newVars currChldTbls), stCrumbs))
      where
        newVars = insertVars currVars (caseInsensitiveRead varType :: VariableType) (Prelude.map (\(LeafNode ID vn) -> vn) varNames) {- register the variable -}

    (BranchNode P.BLOCK lines _) ->
      runOnEachChild 0 ((currTree, djkCrumbs), newSTZipper) buildSymbolTable' {- make new symtable, build -} 
      where
        zipperWithTbl = makeSymTable currTree ((currK, SymbolTable currVars currChldTbls), stCrumbs)  
        newSTZipper = mtToChild currTree zipperWithTbl

    (BranchNode _ _ _) -> runOnEachChild 0 ((currTree, djkCrumbs), ((currK, SymbolTable currVars currChldTbls), stCrumbs)) buildSymbolTable'

    otherwise -> ((currTree, djkCrumbs), ((currK, SymbolTable currVars currChldTbls), stCrumbs))

buildSymbolTable :: DijkstraTree -> SymbolTable
buildSymbolTable tree = 
  (snd . fst . snd $ (buildSymbolTable' (baseTreeZip, baseTblZip)))
  where
    baseTreeZip = (tree, []) :: DjkTreeZipper
    baseTblZip = ((tree, SymbolTable IntMap.empty HashMap.empty), []) :: SymTblZipper
