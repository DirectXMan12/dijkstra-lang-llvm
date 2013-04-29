module BaseDijkstra.TreePatternMatcher (Reply(..), TreeParser(..), leafOfType, branchOfType, anyNode, _V_, _A_, (<|>), (<+>), (<|->), (<?>), (<:>), many, many1, visitNextSibling, getCurrNode, getUserState, setUserState, parse, parseForResult, parseForEither, TreeParseError) where
import BaseDijkstra.NaryTreeZipper
import Control.Monad
import Data.Monoid

-- * Basic Type Definitions --

type TreeParseError = String

-- |Represents a parser for a tree structure of type `tr` with node value `tr` and a result type of `res` (Note: generally requires `tr` to be an instance of `NaryTree` -- NaryTree tr cst a)
data TreeParser tr cst st res = TreeParser { parseTree :: NaryTreeZipper tr cst -> st -> Reply tr cst st res }

-- |Represents a reply from a monadic parser function
data Reply tr cst st res = Ok (NaryTreeZipper tr cst) st res | Error (NaryTreeZipper tr cst) st TreeParseError | DidNothing (NaryTreeZipper tr cst) st deriving (Show, Eq)


parse :: TreeParser tr cst st res -> tr -> st -> Reply tr cst st res
parse tp tr st = parseTree tp (tr, []) st

parseForResult :: (Show cst, Show tr) => TreeParser tr cst st res -> tr -> st -> Maybe res
parseForResult tp tr st = case parse tp tr st of
  Ok _ _ r -> Just r
  DidNothing _ _ -> Nothing
  Error zip _ msg -> error ("[at " ++ show zip ++ "] " ++ msg)

parseForEither :: (Show cst, Show tr) => TreeParser tr cst st res -> tr -> st -> Either TreeParseError res
parseForEither tp tr st = case parse tp tr st of
  Ok _ _ r -> Right r
  DidNothing _ _ -> Left "No result returned"
  Error zip _ msg -> Left ("[at " ++ show zip ++ "] " ++ msg)
  
-- Instance Declarations --

instance Functor (Reply tr cst st) where
  fmap f (Ok zipper st res) = Ok zipper st (f res)
  fmap f (Error zipper st err) = Error zipper st err
  fmap f (DidNothing zipper st) = DidNothing zipper st

instance Functor (TreeParser tr cst st) where
  fmap f (TreeParser tp) = TreeParser (\zipper st -> fmap f (tp zipper st))

instance Monad (TreeParser tr cst st) where
  return x = TreeParser (\zipper st -> Ok zipper st x)

  (TreeParser tp) >>= f =
    TreeParser (\zipper st ->
      let res1 = tp zipper st in
        case res1 of
          Ok (tr, crumbs) st res -> parseTree (f res) (tr, crumbs) st
          Error (tr, crumbs) st err -> Error (tr, crumbs) st err
          DidNothing (tr, crumbs) st -> parseTree (f undefined) (tr, crumbs) st)


instance MonadPlus (TreeParser tr cst st) where
  mzero = TreeParser (\zip st -> DidNothing zip st)

  (TreeParser tp1) `mplus` (TreeParser tp2) = TreeParser (\zip st -> 
    let r = tp1 zip st in case r of
      DidNothing _ st -> tp2 zip st
      Error _ st _ -> tp2 zip st
      other -> other)

-- * NaryTree Specific Functions --

-- ** Useful Basic Functions --

-- |MOVE ACTION: nth Child -- moves the current zipper focus to the given child (0-indexed) of the current node
visitNthChild :: (NaryTree tr cst nv) => Int -> TreeParser tr cst st a
visitNthChild nt = 
  TreeParser (\(tree, crumbs) st ->
    DidNothing (ntToNthChild nt (tree, crumbs)) st)

-- |MOVE ACTION: Next Sibling -- move the current zipper focus to the next sibling of the current node
visitNextSibling :: (NaryTree tr cst nv) => TreeParser tr cst st a
visitNextSibling =
  TreeParser (\(tree, crumbs) st ->
    let (NaryTreeCrumb _ _ rs):bs = crumbs
    in case rs of
      [] -> Error (tree, crumbs) st "Couldn't match enough siblings!" 
      other -> DidNothing (ntToNextSibling (tree, crumbs)) st)

-- |A basic tree parser that matches a node of the given "type" (nodeValue), but does not return anything
branchOfType :: (Show cst, Eq cst, NaryTree tr cst nv) => cst -> TreeParser tr cst st ()
branchOfType nt =
  TreeParser (\(tree, crumbs) st ->
    if (nodeValue tree == nt)
      then Ok (tree, crumbs) st ()
      else Error (tree, crumbs) st ("Couldn't match branch of type " ++ show nt))

-- |A basic tree parser that matches a node of the given "type" (nodeValue), returning the "contents" (nodeContents) of the node
leafOfType :: (Show cst, Eq cst, NaryTree tr cst nv) => cst -> TreeParser tr cst st nv
leafOfType nt =
  TreeParser (\(tree, crumbs) st ->
    if (nodeValue tree == nt)
      then
        let 
          lv = nodeContents tree
        in Ok (tree, crumbs) st lv
      else Error (tree, crumbs) st ("Could not match leaf of type " ++ show nt))

-- |A basic tree parser which always succeeds
anyNode :: (NaryTree tr cst nv) => TreeParser tr cst st ()
anyNode = 
  TreeParser (\(tree, crumbs) st -> Ok (tree, crumbs) st ())  

-- |A utitlity tree parser that extracts the current node so that we can use it (mainly for things where a node has to be associated with a certain result, like a symbol table)
getCurrNode :: (NaryTree tr cst nv) => TreeParser tr cst st tr
getCurrNode = 
  TreeParser (\(tree, crumbs) st -> Ok (tree,crumbs) st tree)

-- |A utitlity tree parser that extracts the current user state
getUserState :: (NaryTree tr cst nv) => TreeParser tr cst st st
getUserState = 
  TreeParser (\(tree, crumbs) st -> Ok (tree,crumbs) st st)

-- |A utitlity tree parser that sets the current user state
setUserState :: (NaryTree tr cst nv) => st -> TreeParser tr cst st st
setUserState newst = 
  TreeParser (\(tree, crumbs) st -> Ok (tree,crumbs) newst newst)


-- ** Combinators and Move Actions --

infix 0 <?>
infixr 1 <|->
infixr 2 <|>
infixr 3 <++>
infixr 4 <:>
infixr 5 <+>

-- |COMBINATOR: Siblings (lhs not list) -- matches the left-hand parser followed immediately by the right-hand parser
(<:>) :: (NaryTree tr cst nv) => (TreeParser tr cst st nv) -> (TreeParser tr cst st [nv]) -> (TreeParser tr cst st [nv])
tp1 <:> tp2 = do
  r1 <- tp1
  visitNextSibling
  r2 <- tp2
  return (r1:r2)

-- |COMBINATOR: Siblings (rhs not list) -- matches the left-hand parser followed immediately by the right-hand parser
(<++:>) :: (NaryTree tr cst nv) => (TreeParser tr cst st [nv]) -> (TreeParser tr cst st nv) -> (TreeParser tr cst st [nv])
tp1 <++:> tp2 = do
  r1 <- tp1
  visitNextSibling
  r2 <- tp2
  return (r1 ++ [r2])

-- |COMBINATOR: Siblings (neither side list) -- matches the left-hand parser followed immediately by the right-hand parser
(<+>) :: (NaryTree tr cst nv) => (TreeParser tr cst st m) -> (TreeParser tr cst st m) -> (TreeParser tr cst st [m])
tp1 <+> tp2 = do
  r1 <- tp1
  visitNextSibling
  r2 <- tp2
  return [r1,r2]

-- |COMBINATOR: Siblings (both sides list) -- matches the left-hand parser followed immediately by the right-hand parser
(<++>) :: (NaryTree tr cst nv) => (TreeParser tr cst st [m]) -> (TreeParser tr cst st [m]) -> (TreeParser tr cst st [m])
tp1 <++> tp2 = do
  r1 <- tp1
  visitNextSibling
  r2 <- tp2
  return (r1 ++ r2)

-- used below in `many`
manyPart zip sofar tp st =
  let
    parser = do { visitNextSibling; tp } 
    res = parseTree parser zip st
    in case res of
      Ok newzip st pv -> manyPart newzip (sofar `mappend` [pv]) tp st
      DidNothing newzip st -> manyPart newzip sofar tp st
      Error newzip st _ -> case sofar of
        [] -> DidNothing newzip st
        other -> Ok newzip st sofar


-- |COMBINATOR: Many? (0 or more) -- matches the given parser 0 or more times, returning the accumulated results when it fails to match (or `DidNothing` if no matches occured, or if none of the matches returned any values)
many :: (NaryTree tr cst nv) => (TreeParser tr cst st m) -> (TreeParser tr cst st [m])
many tp = TreeParser (\zip st ->
  case parseTree tp zip st of
    Ok newzip st val -> manyPart newzip [val] tp st
    DidNothing newzip st -> manyPart newzip [] tp st
    Error newzip st _ -> DidNothing newzip st)

-- |COMBINATOR: Many (at least 1) -- matches the given parser 1 or more times, but is otherwise identical to `many`
many1 :: (NaryTree tr cst nv) => (TreeParser tr cst st m) -> (TreeParser tr cst st [m])
many1 tp = tp >>= (\x -> TreeParser (\zip st -> manyPart zip [x] tp st))

-- |MOVE ACTION: Down -- navigates to the first child of the current node
_V_ :: (NaryTree tr cst nv) => TreeParser tr cst st a
_V_ = visitNthChild 0

-- |MOVE ACTION: Up -- navigates to the parent of the current node
_A_ :: (NaryTree tr cst nv) => TreeParser tr cst st a
_A_ = TreeParser (\zip st -> DidNothing (ntToUp zip) st)

-- |COMBINATOR: Alternation -- tries to match the lhs, and if it fails or does nothing (`Error` or `DidNothing`) then tries to match the rhs
(<|>) :: (NaryTree tr cst nv) => TreeParser tr cst st a -> TreeParser tr cst st a -> TreeParser tr cst st a
tp1 <|> tp2 = tp1 `mplus` tp2

-- |COMBINATOR: With Children -- matches the lhs against the current node, and then matches the rhs against the children of the current node (leaves the parser state as it was before this parser was run), and return the results (if any) of the child nodes
(<|->) :: (NaryTree tr cst nv) => TreeParser tr cst st a -> TreeParser tr cst st b -> TreeParser tr cst st b
ptp <|-> ctp = do
  ptp
  _V_
  res <- ctp
  _A_
  return res

-- |COMBINATOR: Error Message -- specifies an error message prefix in case the given parser throws an error
(<?>) :: TreeParser tr cst st a -> String -> TreeParser tr cst st a
tp <?> msg = TreeParser (\zip st -> case parseTree tp zip st of
  Error nz st oldmsg -> Error nz st (msg ++ ": " ++ oldmsg)
  other -> other)

-- |COMBINATOR: Try -- attempts to run the given parser, returning the result if successful, or `DidNothing` otherwise
try :: TreeParser tr cst st a -> TreeParser tr cst st a
try p = TreeParser (\zip st -> 
  let res = parseTree p zip st
    in case res of
      Error z st _ -> DidNothing z st
      other -> res)
