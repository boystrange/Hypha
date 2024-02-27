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

module DeadLock.ValueExpression where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.MultiSet as MS

import Data.Set.Unicode
import Data.Bool.Unicode

{-|
Value expressions class.
-}
class ValueExpression v where
  valueNil :: v

  valueAdd :: v -> v -> v
  valueSub :: v -> v -> v
  valueSub v1 v2 = valueAdd v1 $ valueNeg v2

  valueNeg :: v -> v
  valueNeg = valueSub valueNil

-- infix value expressions operations

(@+@) :: ValueExpression v => v -> v -> v
(@+@) = valueAdd

(@-@) :: ValueExpression v => v -> v -> v
(@-@) = valueSub

(-@) :: ValueExpression v => v -> v
(-@) = valueNeg

{-|
Datatype used to implement expressions.
An expression is modelled using a linear combination
of variables (partitioned in two set: pos, that
contains the positive variables, and neg, that
contains the negate variables) and a numeric constant.

e.g.

x1 - x3
is internally implemented as
  pos = { x1 }
  neg = { x3 }
  const = 0

x1 + x2 + x3 - x4 + x5 + 5 = 0
is internally implemented as
  pos = { x1, x2, x3, x5 }
  neg = { x4 }
  const = 5

5
is internally implemented as
  pos = { }
  neg = { }
  const = 5

-}
data ValueE n lvar = ValueE n (MS.MultiSet lvar) (MS.MultiSet lvar)
                   deriving (Eq, Ord)

instance (Num n, Ord lvar) => ValueExpression (ValueE n lvar) where
  valueNil = ValueE 0 MS.empty MS.empty

  valueAdd (ValueE n pos1 neg1) (ValueE m pos2 neg2) =
    ValueE (n + m) (MS.difference pos1 neg2 `MS.union` MS.difference pos2 neg1)
                   (MS.difference neg1 pos2 `MS.union` MS.difference neg2 pos1)

  valueNeg (ValueE n pos neg) = ValueE (negate n) neg pos

instance (Num n, Ord n, Show n, Show lvar) => Show (ValueE n lvar) where
  show (ValueE n pos neg) | MS.null pos ∧ MS.null neg = show n
  show (ValueE 0 pos neg) | MS.null neg = showMS "+" pos
  show (ValueE 0 pos neg) | MS.null pos = "-" ++ showMS "-" neg
  show (ValueE 0 pos neg) = showMS "+" pos ++ " - " ++ showMS "-" neg
  show (ValueE n pos neg) | MS.null neg = show n ++ " + " ++ showMS "+" pos
  show (ValueE n pos neg) = show n ++ " + " ++ showMS "+" pos ++ " - " ++ showMS "-" neg

showMS :: Show lvar => String -> MS.MultiSet lvar -> String
showMS op = L.intercalate (" " ++ op ++ " ") . L.map showElement . MS.toOccurList
  where
    showElement (x, 1) = show x
    showElement (x, n) = show n ++ "·" ++ show x

{-|
Return an instance of ValueE corresponding to the given numeric value.
-}
valueC :: Num n => n -> ValueE n lvar
valueC n = ValueE n MS.empty MS.empty

{-|
Return an instance of ValueE corresponding to the given variable.
-}
valueV :: (Num n, Ord lvar) => lvar -> ValueE n lvar
valueV lvar = ValueE 0 (MS.singleton lvar) MS.empty

{-|
Return the set of variables of a ValueE.
-}
varsE :: Ord lvar => ValueE n lvar -> S.Set lvar
varsE (ValueE _ pos neg) = MS.toSet pos ∪ MS.toSet neg

