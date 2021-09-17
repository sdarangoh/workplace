module Composition where


import Numeric.Natural 
import Language.Mira.FA.Types
import Language.Mira.FA.Lib
import Data.Set (Set)
import GHC.List
import GHC.Real
import GHC.Num
import qualified GHC.Num as Num
import qualified GHC.Real as Real
import qualified Data.Set as Set
import qualified GHC.List as List

estad :: FA Natural -> FA Natural -> Set Natural
estad ( MkFA estados1 _ _ _ ) (MkFA estados2 _ _ _ )
  = Set.union estados1 (Set.union (Set.difference estados2 
  ( Set.intersection estados1 estados2)) (s estados1 estados2)) 

func :: FA a -> FA a -> (Set (Move a), Set (Move a))
func (MkFA _ moves _ _ ) (MkFA _ moves1 _ _) = (moves , moves1)

start :: FA Natural -> Natural
start ( MkFA _ _ init _) = init

final :: Ord a => FA a -> FA a -> Set a
final ( MkFA _ _ _ fin1 ) (MkFA _ _ _ fin2 ) = Set.union fin1 fin2 

bound :: Set Natural -> Set Natural -> Natural
bound a b 
  = List.maximum [(Set.findMax a), (Set.findMax b)]
 
s :: Set Natural -> Set Natural -> Set Natural
s a b = Set.fromList [ (Num.fromInteger (Real.toInteger x)) 
      + (bound a b) | x <- [1.. Set.size(Set.intersection a b)]] 

