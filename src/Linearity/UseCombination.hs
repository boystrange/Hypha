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

module Linearity.UseCombination where

import Linearity.Use
import qualified Linearity.Combination as Combination

import qualified Data.Set as S

type UseC = Combination.T Use

fuv :: UseC -> S.Set UseV
fuv = S.unions . map Linearity.Use.fuv . Combination.distinctElems

substUU :: UseEnvironment -> UseC -> Use
substUU env (uc) = foldl (+) 0 $ Combination.toList $ Combination.map (substUE env) uc

evaluateUC :: Combination.T Use -> Use
evaluateUC = foldl (+) 0 . Combination.toList

subst :: UseV -> UseC -> UseC -> UseC
subst uvar uc = Combination.flatten . Combination.map aux
    where
      aux (UVar uvar') | uvar == uvar' = uc
      aux u = Combination.singleton u
