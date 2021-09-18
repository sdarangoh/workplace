module Composition where

--Librearies we used for the program
import Numeric.Natural 
import Language.Mira.FA.Types
import Language.Mira.FA.Lib
import Data.Set (Set)
import GHC.List
import GHC.Real
import GHC.Num
import Data.Map as Map
import qualified GHC.Num as Num
import qualified GHC.Real as Real
import qualified Data.Set as Set
import qualified GHC.List as List

-------------------------------------------------------------------------------------------------
--                                                                                             -- 
-- This function takes the states of two automatas and creates a new set whit all the          --
-- states of those automatas. If one or more states of the given automatas have the same name, --
-- the function will change one of the names so there is no posibility to lose an state.       --
--                                                                                             --
-------------------------------------------------------------------------------------------------

estad :: FA Natural -> FA Natural -> Set Natural
estad ( MkFA estados1 _ _ _ ) (MkFA estados2 _ _ _ )
  = Set.union estados1 (Set.union (Set.difference estados2 
  ( Set.intersection estados1 estados2)) (s estados1 estados2)) 

-------------------------------------------------------------------------------------------------
--                                                                                             -- 
-- setIni is a function that creates the new initial state for the new automata, it check all  --
-- the set  of states of the new automta and then it creates a new state that is not on that   --
-- set.                                                                                        --
--                                                                                             --
-------------------------------------------------------------------------------------------------

posible :: FA Natural -> Set Natural
posible (MkFA estados1 _ _ _ ) = estados1

moves :: FA Natural -> Set (Move Natural)
moves (MkFA _ moves1 _ _ ) = moves1

setIni :: FA Natural -> FA Natural -> Set Natural
setIni (MkFA states1 _ _ _) (MkFA states2 _ _ _) 
  = Set.fromList [(Set.findMax (s states1 states2)) + 1] 

finals :: FA Natural -> [Natural]
finals (MkFA _ _ _ fin) = Set.toList fin

union :: FA Natural -> FA Natural -> FA Natural
union  a b = MkFA (estad a b) (moves a) (Set.elemAt 0 (setIni a b)) (Set.fromList (finals a ))

start :: FA Natural -> Natural
start ( MkFA _ _ init _) = init

add :: [Natural] -> [(Natural, Natural)] -> [Natural]
add a b = tail (a++([snd (head b)]))

dic :: [Natural] -> Natural -> Map
dic [] _ = Map.empty 
dic (x:xs) max = Map.insert x max (dic xs (max + 1))

bound :: Set Natural -> Set Natural -> Natural
bound a b 
  = List.maximum [(Set.findMax a), (Set.findMax b)]
 
s :: Set Natural -> Set Natural -> Set Natural
s a b = Set.fromList [ (Num.fromInteger (Real.toInteger x)) 
      + (bound a b) | x <- [1.. Set.size(Set.intersection a b)]] 

dic :: Set Natural -> Set Natural -> [(Natural, Natural)]
dic a b = zip (Set.toList (Set.intersection a b)) (Set.toList (s a b))


f :: [Natural] -> [Natural]-> [Natural]
f a b = [x + y | x <- a , y <- b]
