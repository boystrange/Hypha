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

module Linearity.Partitioner where

import Linearity.Use
import Linearity.UseCombination
import qualified Relation
import qualified Data.Set as S
import Data.Set.Unicode

import Debug.Trace

findVarRelations :: Relation.T UseC UseC -> Relation.T UseV UseV
findVarRelations = S.unions . map aux . Relation.toList
    where
      aux :: (UseC, UseC) -> Relation.T UseV UseV
      aux (u1, u2) = Relation.fromList [ (uvar1, uvar2) | uvar1 <- uvars, uvar2 <- uvars ]
          where
            uvars = S.toList (Linearity.UseCombination.fuv u1 ∪ Linearity.UseCombination.fuv u2)

partitionUseConstraints :: Relation.T UseC UseC -> [Relation.T UseC UseC]
partitionUseConstraints eqU = map (\uvset -> Relation.filter (check uvset) eqU) (Relation.partition uvrel)
    where
      uvrel = Relation.transitiveClosure $ Relation.symmetricClosure $ findVarRelations eqU

      check uvset u1 u2 = (Linearity.UseCombination.fuv u1 ∪ Linearity.UseCombination.fuv u2) ⊆ uvset
