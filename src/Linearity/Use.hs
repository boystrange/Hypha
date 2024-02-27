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

module Linearity.Use where

import Aux
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set.Unicode

type UseV = Int

data Use = UVar UseV
         | Zero
         | One
         | Omega
         deriving (Eq, Ord)

ω :: Use
ω = Omega

instance Show Use where
    show (UVar n) = "ρ" ++ show n
    show Zero = "0"
    show One = "1"
    show Omega = "ω"

instance Num Use where
    Zero + k = k
    k + Zero = k
    _ + _ = Omega

    Zero * _ = Zero
    _ * Zero = Zero
    One * k = k
    k * One = k
    _ * _ = Omega

    fromInteger 0 = Zero
    fromInteger 1 = One
    fromInteger _ = Omega

    abs k = k
    negate k = k

    signum _ = 0

fuv :: Use -> S.Set UseV
fuv (UVar uvar) = S.singleton uvar
fuv _ = (∅)

type UseEnvironment = M.Map UseV Use

substUE :: UseEnvironment -> Use -> Use
substUE env (UVar uvar) | Just k <- M.lookup uvar env = k
substUE _ u = u

completeUseEnvironment :: UseEnvironment -> S.Set UseV -> UseEnvironment
completeUseEnvironment env poU = M.union env env'
    where
      unassignedUVars = S.difference poU (M.keysSet env)
      env' = M.fromList $ map (\uvar -> (uvar, 0)) $ S.toList unassignedUVars
