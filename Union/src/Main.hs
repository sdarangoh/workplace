module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Composition

import Language.Mira.FA.Types

fa1 :: FA Int

fa1 = MkFA 
  { statesFA = Set.fromList [1,2,3]
  , movesFA = Set.fromList [ Move 1 'a' 2, Emove 3 3]
  , startStateFA = 2
  , finalStatesFA = Set.fromList [2] 
  }

main :: IO ()
main = print $ fa1
