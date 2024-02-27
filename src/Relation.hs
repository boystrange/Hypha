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

module Relation where

import qualified Data.List as L;
import qualified Data.Set as S;

type T a b = S.Set (a, b)

-- COMMON OPERATIONS

empty :: T a b
empty = S.empty

insert :: (Ord a, Ord b) => a -> b -> T a b -> T a b
insert x y = S.insert (x, y)

toList :: T a b -> [(a, b)]
toList r = S.toList r

fromList :: (Ord a, Ord b) => [(a, b)] -> T a b
fromList = S.fromList

domain :: Eq a => T a b -> [a]
domain = L.nub . L.map fst . toList

codomain :: Eq b => T a b -> [b]
codomain = L.nub . L.map snd . toList

inverse :: (Ord a, Ord b) => T a b -> T b a
inverse = S.map (\(x, y) -> (y, x))

intersection :: (Ord a, Ord b) => T a b -> T a b -> T a b
intersection = S.intersection

difference :: (Ord a, Ord b) => T a b -> T a b -> T a b
difference = S.difference

union :: (Ord a, Ord b) => T a b -> T a b -> T a b
union = S.union

filter :: (Ord a, Ord b) => (a -> b -> Bool) -> T a b -> T a b
filter f = S.filter (uncurry f)

compose :: (Ord a, Ord b, Ord c) => T a b -> T b c -> T a c
compose r s = S.fromList [ (x, z) | (x, y) <- S.toList r, (y', z) <- S.toList s, y == y' ]

closure :: (Ord a, Ord b) => (T a b -> T a b) -> T a b -> T a b
closure generator = limit (\r -> r `Relation.union` (generator r))
    where
      limit :: Eq a => (a -> a) -> a -> a
      limit f x =
          let y = f x in
          if x == y then x else limit f y

image :: (Ord a, Ord b) => a -> T a b -> S.Set b
image x = S.map snd . S.filter ((== x) . fst)

-- inverseImage :: (Ord a, Ord b) => S.Set b -> T a b -> S.Set a
-- inverseImage s = image s . inverse

-- TERNARY RELATIONS

rotateLeft :: (Ord a, Ord b, Ord c) => T a (b, c) -> T b (c, a)
rotateLeft = S.map (\(x, (y, z)) -> (y, (z, x)))

rotateRight :: (Ord a, Ord b, Ord c) => T a (b, c) -> T c (a, b)
rotateRight = S.map (\(x, (y, z)) -> (z, (x, y)))

compose2 :: (Ord a, Ord b, Ord c) => T b b -> T a (b, c) -> T a (b, c)
compose2 r = rotateRight . compose r . rotateLeft

compose3 :: (Ord a, Ord b, Ord c) => T c c -> T a (b, c) -> T a (b, c)
compose3 r = rotateLeft . compose r . rotateRight

support3 :: Ord a => T a (a, a) -> S.Set a
support3 = S.unions . L.map (\(x, (y, z)) -> S.fromList [x, y, z]) . S.toList

-- ENDORELATIONS

support :: Ord a => T a a -> S.Set a
support = S.unions . L.map (\(x, y) -> S.fromList [x, y]) . S.toList

reflexiveClosure :: Ord a => T a a -> T a a
reflexiveClosure f = Relation.union f $ S.map (\x -> (x, x)) (support f)

symmetricClosure :: Ord a => T a a -> T a a
symmetricClosure f = Relation.union f $ inverse f

transitiveClosure :: Ord a => T a a -> T a a
transitiveClosure = closure (\r -> compose r r)

generate :: (Ord a, Ord b, Ord c, Ord d) => ((a, b) -> T c d) -> T a b -> T c d
generate f = S.fold (\(x, y) s -> S.union (f (x, y)) s) S.empty 

map :: (Ord a, Ord b, Ord c, Ord d) => ((a, b) -> (c, d)) -> T a b -> T c d
map = S.map

-- UNCHECKED

-- select :: Ord a => S.Set a -> T a -> T a
-- select xs = Relation.filter (\x y -> x `S.member` xs || y `S.member` xs)

remove :: Ord a => S.Set a -> T a a -> T a a
remove xs = Relation.filter (\x y -> not (x `S.member` xs) && not (y `S.member` xs))

partition :: Ord a => T a a -> [S.Set a]
partition r | S.null r = []
partition r = let (e, _) = S.findMin r in
              let eclass = image e r in
              eclass : partition (remove eclass r)
