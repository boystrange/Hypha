--
-- WARNING!
-- This Module is not used!
--


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

module DeadLock.Level where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.MultiSet as MS
import Data.Set.Unicode

type Level = Int
type LevelV = Int

data LevelE = LBottom
            | Level Level
            | LVar LevelV
            | LevelE :+: LevelE
              deriving (Eq, Ord)

type LevelNE = Maybe (Level, MS.MultiSet LevelV)

instance Show LevelE where
    show LBottom = "⊥"
    show (LVar lvar) = "ℓ" ++ show lvar
    show (Level l) = show l
    show (l1 :+: l2) = show l1 ++ " + " ++ show l2

findL :: M.Map LevelV LevelV -> LevelV -> LevelV
findL m lvar | Just lvar' <- M.lookup lvar m = findL m lvar'
             | otherwise = lvar

substL :: M.Map LevelV LevelV -> LevelE -> LevelE
substL m (LVar lvar) = LVar (findL m lvar)
substL m (l1 :+: l2) = substL m l1 :+: substL m l2
substL _ l = l

normalizeL :: LevelE -> LevelNE
normalizeL LBottom = Nothing
normalizeL (Level n) = Just (n, MS.empty)
normalizeL (LVar lvar) = Just (0, MS.singleton lvar)
normalizeL (l1 :+: l2) = case (normalizeL l1, normalizeL l2) of
                           (Nothing, _) -> Nothing
                           (_, Nothing) -> Nothing
                           (Just (n, ms1), Just (m, ms2)) -> Just (n + m, MS.union ms1 ms2)

simplifyL :: LevelE -> LevelE
simplifyL l = case normalizeL l of
                Nothing -> LBottom
                Just (n, ms) | MS.null ms -> Level n
                Just (0, ms) | (lvar : lvars) <- MS.toList ms -> foldl (:+:) (LVar lvar) (map LVar lvars)
                Just (n, ms) -> foldl (:+:) (Level n) (map LVar $ MS.toList ms)

varsL :: LevelE -> S.Set LevelV
varsL LBottom = S.empty
varsL (Level _) = S.empty
varsL (LVar lvar) = S.singleton lvar
varsL (l1 :+: l2) = varsL l1 ∪ varsL l2

