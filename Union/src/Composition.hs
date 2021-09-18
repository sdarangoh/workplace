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
import qualified Data.Map as Map
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

setIni :: FA Natural -> FA Natural -> Set Natural
setIni (MkFA states1 _ _ _) (MkFA states2 _ _ _) 
  = Set.fromList [(Set.findMax (s states1 states2)) + 1] 

-- returns the set of final states of the automata

finals :: FA Natural -> [Natural]
finals (MkFA _ _ _ fin) = Set.toList fin

-- makes the union between two FA

union :: FA Natural -> FA Natural -> FA Natural
union  a b = MkFA (estad a b) (moves a) (Set.elemAt 0 (setIni a b)) (Set.fromList (finals a ))

-- returns theinicial state of the FA

start :: FA Natural -> Natural
start ( MkFA _ _ init _) = init

-- makes a dicctionarie

dic :: [Natural] -> Natural -> Map Natural Natural
dic [] _ = Map.empty
dic (x:xs) max = Map.insert x max (dic xs (max + 1))

-- returns the biggest natural number of the set of posible states

bound :: Set Natural -> Set Natural -> Natural
bound a b 
  = List.maximum [(Set.findMax a), (Set.findMax b)]
 
-- returns the set of new values asigned to the elements on the intersection 
-- of the sets of posible states on both FA

s :: Set Natural -> Set Natural -> Set Natural
s a b = Set.fromList [ (Num.fromInteger (Real.toInteger x)) 
      + (bound a b) | x <- [1.. Set.size(Set.intersection a b)]] 

-- give the set of possible states of one FA
posible :: FA Natural -> Set Natural
posible (MkFA estados1 _ _ _ ) = estados1

-- Returns the set of moves defined for one FA

moves :: FA Natural -> Set (Move Natural)
moves (MkFA _ moves1 _ _ ) = moves1

droper :: [Natural] -> Map Natural Natural -> Set Natural
droper [] _ = Set.fromList[]
droper (x:xs) k = Set.fromList ([k ! x] ++ Set.toList (droper xs k))

finalA :: FA Natural -> FA Natural -> Map Natural Natural -> Set Natural
finalA (MkFA _ _ _ final1 ) (MkFA _ _ _ final2) k 
   = Set.union (Set.difference final2 (Set.intersection final1 final2)) (droper (Set.toList (Set.intersection final1 final2)) k )

changeMove :: Move Natural -> Map Natural Natural -> [Natural] -> Move Natural 
changeMove (Move i c f) m interceccion 
   = Move (change m i (aux i interceccion)) c (change m f (aux f interceccion)) 
changeMove (Emove i f) m interceccion 
   = Emove (change m i (aux i interceccion)) (change m f (aux f interceccion)) 


aux :: Natural -> [Natural] -> Bool
aux n (x:xs) = (n==x) || (aux n xs)

change :: Map Natural Natural -> Natural -> Bool -> Natural
change m n True = m ! n
change m n False = n


