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

module Linearity.UseSolver where

import Aux
import Linearity.Use
import Linearity.UseCombination
import qualified Relation
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Linearity.Combination as Combination
import Data.Set.Unicode

import Debug.Trace

generateAssignments :: [(UseV, [Use])] -> [UseEnvironment]
generateAssignments = map M.fromList . aux
    where
      aux [] = [[]]
      aux ((uvar, us) : domains) = [ (uvar, u) : env | u <- us, env <- aux domains ]

bruteForceSolutions :: Relation.T UseC UseC -> [UseEnvironment]
bruteForceSolutions eqU =
    let support = Relation.support eqU in
    let uvars = S.toList $ S.unions $ map Linearity.UseCombination.fuv $ S.toList support in
    let domains = map (\uvar -> (uvar, [0, 1, ω])) uvars in
    filter checkUseSolution (generateAssignments domains)
    where
      checkUseSolution env = all (checkEQ env) (Relation.toList eqU)

      checkEQ env (u1, u2) = (substUU env) u1 == (substUU env) u2

bruteForceSolution :: Bool -> Relation.T UseC UseC -> UseEnvironment
bruteForceSolution minomega = findBestSolution . bruteForceSolutions
    where
      cost :: UseEnvironment -> (Int, Int)
      cost = M.foldl (\(omegas, ones) u -> case u of
                                             Zero -> (omegas, ones)
                                             One -> (omegas, ones + 1)
                                             Omega -> (omegas + 1, ones)) (0, 0)

      findBestSolution [] = error "internal error: no use solution"
      findBestSolution (uenv0 : _) | not minomega = uenv0
      findBestSolution (uenv0 : uenvs) =
          snd $
          foldl (\(ecost, uenv) uenv' -> let ecost' = cost uenv' in
                                         if ecost' < ecost then (ecost', uenv') else (ecost, uenv)) (cost uenv0, uenv0) uenvs

findUseSolution :: Bool -> Relation.T UseC UseC -> UseEnvironment
findUseSolution minomega = aux Relation.empty . Relation.toList
    where
      aux :: Relation.T UseC UseC -> [(UseC, UseC)] -> UseEnvironment
      aux rel [] = bruteForceSolution minomega rel
      aux rel ((uc1, uc2) : cs) | uc1 == uc2 = aux rel cs
      aux rel ((uc1, uc2) : cs) =
          case (Combination.toList uc1, Combination.toList uc2) of
            ([UVar uvar], _) | not (uvar ∈ Linearity.UseCombination.fuv uc2) -> auxSubst rel uvar uc2 cs
            (_, [UVar uvar]) | not (uvar ∈ Linearity.UseCombination.fuv uc1) -> auxSubst rel uvar uc1 cs
            _ -> aux (Relation.insert uc1 uc2 rel) cs

      auxSubst rel uvar uc cs =
          let env = aux (substRel uvar uc rel) (substAll uvar uc cs) in
          let env' = completeUseEnvironment env (Linearity.UseCombination.fuv uc) in
          M.insert uvar (substUU env' uc) env'

      substAll :: UseV -> UseC -> [(UseC, UseC)] -> [(UseC, UseC)]
      substAll uvar uc = L.map (\(uc1, uc2) -> (Linearity.UseCombination.subst uvar uc uc1,
                                                Linearity.UseCombination.subst uvar uc uc2))

      substRel :: UseV -> UseC -> Relation.T UseC UseC -> Relation.T UseC UseC
      substRel uvar uc = Relation.map (\(uc1, uc2) -> (Linearity.UseCombination.subst uvar uc uc1,
                                                       Linearity.UseCombination.subst uvar uc uc2))

-- arrangeConstraints :: Relation.T UseC UseC -> M.Map UseV [UseC]
-- arrangeConstraints eqU = M.unionWith (++) m $ M.fromList $ L.map (\uvar -> (uvar, [Combination.empty])) uvars
--     where
--       uvars :: [UseV]
--       uvars = S.toList $ S.unions $ map Linearity.UseCombination.fuv $ S.toList $ Relation.support eqU

--       m :: M.Map UseV [UseC]
--       m = M.fromListWith (++) $ snd $ foldl aux (1, []) $ Relation.toList eqU

--       aux :: (Int, [(UseV, [UseC])]) -> (UseC, UseC) -> (Int, [(UseV, [UseC])])
--       aux (nextId, res) (uc1, uc2) =
--           case (Combination.toList uc1, Combination.toList uc2) of
--             ([UVar uvar], _) -> (nextId, (uvar, [uc2]) : res)
--             (_, [UVar uvar]) -> (nextId, (uvar, [uc1]) : res)
--             _ -> (nextId + 1, (-nextId, [uc1, uc2]) : res)
                         
-- findUseSolution :: Relation.T UseC UseC -> UseEnvironment
-- findUseSolution eqU = limit generator base
--     where
--       m :: M.Map UseV [UseC]
--       m = arrangeConstraints eqU

--       base :: UseEnvironment
--       base = M.map (const 0) m

--       generator :: UseEnvironment -> UseEnvironment
--       generator uenv = M.map (maximum . L.map (substUU uenv)) m
