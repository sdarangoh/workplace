module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Composition
import Numeric.Natural
import Language.Mira.FA.Types

fa1 :: FA Natural

fa1 = MkFA 
  { statesFA = Set.fromList [1,2,3,4]
  , movesFA = Set.fromList [ Move 1 'a' 2, Emove 3 3]
  , startStateFA = 2
  , finalStatesFA = Set.fromList [2,4] 
  }

fa2 :: FA Natural

fa2 = MkFA 
  { statesFA = Set.fromList [1,2,4,5,3]
  , movesFA = Set.fromList [ Move 1 'a' 2, Move 2 'a' 5, Move 5 'a' 2, Emove 3 3]
  , startStateFA = 2
  , finalStatesFA = Set.fromList [3,4,5] 
  }

main :: IO ()
main = print $ final [1,2,3,45,105,98] [(1,4),(3,5)]

