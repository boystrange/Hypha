-- This file is part of Hypha.
--
-- Hypha is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Hypha is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Hypha.  If not, see <http://www.gnu.org/licenses/>.
--
-- Copyright 2013-2014 Luca Padovani

module Linearity.Combination
    (T, empty, singleton, fromList, toList, distinctElems, union,
     Linearity.Combination.map, Linearity.Combination.partition,
     insert, flatten, Linearity.Combination.null, Linearity.Combination.size,
     Linearity.Combination.findMin, Linearity.Combination.all,
     Linearity.Combination.delete
    )
where

import qualified Data.List as L
import qualified Data.MultiSet as MS
import qualified Data.Map as M

newtype T a = Combination (MS.MultiSet a)
    deriving (Eq)

instance Ord a => Ord (T a) where
    compare (Combination ms1) (Combination ms2) = case MS.size ms1 `compare` MS.size ms2 of
                                                    EQ -> compare ms1 ms2
                                                    ord -> ord

instance Show a => Show (T a) where
    show (Combination ms) | MS.null ms = "•"
    show (Combination ms) =
        concat $ L.intersperse " + " $ L.map ppElement (MS.toOccurList ms)
        where
          ppElement (t, 1) = show t
          ppElement (t, 2) = "2·" ++ show t
          ppElement _ = error "fatal internal error (Combination representation)"

null :: T a -> Bool
null (Combination ms) = MS.null ms

size :: T a -> Int
size (Combination ms) = MS.size ms

empty :: T a
empty = Combination MS.empty

singleton :: Ord a => a -> T a
singleton = Combination . MS.singleton

fromList :: Ord a => [a] -> T a
fromList = bound . Combination . MS.fromList

toList :: T a -> [a]
toList (Combination ms) = MS.toList ms

distinctElems :: T a -> [a]
distinctElems (Combination ms) = MS.distinctElems ms

findMin :: Ord a => T a -> a
findMin (Combination ms) = MS.findMin ms

partition :: Ord a => (a -> Bool) -> T a -> (T a, T a)
partition f (Combination ms) = (Combination ms1, Combination ms2)
    where
      (ms1, ms2) = MS.partition f ms

member :: Ord a => a -> T a -> Bool
member t (Combination ms) = t `MS.member` ms

all :: (a -> Bool) -> T a -> Bool
all f = L.all f . Linearity.Combination.distinctElems

includes :: Ord a => T a -> T a -> Bool
includes (Combination ms1) (Combination ms2) = ms2 `MS.isSubsetOf` ms1

bound :: Ord a => T a -> T a
bound (Combination ms) = Combination $ MS.foldOccur (\elem n -> MS.insertMany elem (min n 2)) MS.empty ms

insert :: Ord a => a -> T a -> T a
insert elem (Combination ms) = bound (Combination (MS.insert elem ms))

union :: Ord a => T a -> T a -> T a
union (Combination ms1) (Combination ms2) = bound (Combination $ ms1 `MS.union` ms2)

delete :: Ord a => a -> T a -> T a
delete elem (Combination ms) = Combination (MS.delete elem ms)

difference :: Ord a => T a -> T a -> T a
difference (Combination ms1) (Combination ms2) = Combination $ MS.difference ms1 ms2

map :: (Ord a, Ord b) => (a -> b) -> T a -> T b
map f (Combination ms) = bound (Combination $ MS.map f ms)

flatten :: Ord a => T (T a) -> T a
flatten = foldl Linearity.Combination.union Linearity.Combination.empty . Linearity.Combination.toList 

