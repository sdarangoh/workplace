module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Composition
import Numeric.Natural
import Language.Mira.FA.Types

fa1 :: FA Natural

fa1 = MkFA 
  { statesFA = Set.fromList [1,2,3]
  , movesFA = Set.fromList [ Move 1 'a' 2, Emove 3 3]
  , startStateFA = 2
  , finalStatesFA = Set.fromList [2] 
  }

fa2 :: FA Natural

fa2 = MkFA 
  { statesFA = Set.fromList [1,2,4,5,3]
  , movesFA = Set.fromList [ Move 1 'a' 2, Move 2 'a' 5, Move 5 'a' 2, Emove 3 3]
  , startStateFA = 2
  , finalStatesFA = Set.fromList [3] 
  }

main :: IO ()
main = print $ estad fa1 fa2
--(Set.fromList [1,2,3,45,105,98]) (Set.fromList [1,3,5,7,98,90,304])

