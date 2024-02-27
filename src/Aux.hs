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

module Aux (limit, showSubScript, mapPair, mapFst, lookupBy) where

import Data.Char
import Data.Bool.Unicode

limit :: Eq a => (a -> a) -> a -> a
limit f x = let y = f x in
            if x == y then x else limit f y

showSubScript :: Int -> String
showSubScript n | n < 0 = "₋" ++ showSubScript (-n)
showSubScript n | n >= 0 ∧ n < 10 = [digit n]
showSubScript n = showSubScript (n `div` 10) ++ [digit (n `mod` 10)]

digit :: Int -> Char
digit n = chr $ ord '₀' + n

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

lookupBy :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy _ _ [] = Nothing
lookupBy eq x ((k, v) : l) | x `eq` k = Just v
                           | otherwise = lookupBy eq x l
