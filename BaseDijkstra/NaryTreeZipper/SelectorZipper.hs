module BaseDijkstra.NaryTreeZipper.SelectorZipper (NaryTreeSelector(..)) where

import BaseDijkstra.NaryTreeZipper

data NaryTreeSelector tr cst = NarySiblingSelector [tr] [NaryTreeCrumb tr cst] | NaryAbitraryNodeSelector [NaryTreeSelector tr cst]

