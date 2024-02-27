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

module DeadLock.Interface (processM) where

import Data.Ord.Unicode
import Data.Bool.Unicode
import Data.Set.Unicode

import Process
import BasicType
import Linearity.Use
import qualified Linearity.Type as LT
import DeadLock.Polarity
import DeadLock.Type

import qualified Data.Map as M
import qualified Data.Set as S

useM :: Use -> Use -> Polarity
useM Zero Zero = S.empty
useM _ Zero = S.singleton CInput
useM Zero _ = S.singleton COutput
useM _ _ = S.fromList [CInput, COutput]

typeM :: LT.Type -> TypeN
typeM (LT.TVar tvar) = TVar tvar
typeM (LT.TBasic bt) = TBasic bt
typeM (LT.TChannel t u1 u2) | u1 ≤ 1 ∧ u2 ≤ 1 = TChannel (useM u1 u2) (typeM t) (Linear () ())
                            | otherwise = TChannel (useM u1 u2) (typeM t) Unlimited
typeM (LT.TProduct t1 t2) = TProduct (typeM t1) (typeM t2)
typeM (LT.TVariant m _) = TVariant $ M.map typeM m
typeM (LT.TRec tvar t) = TRec tvar $ typeM t

bumpM :: M.Map Name LT.Type -> Name -> LT.Type -> LT.Type
bumpM env x t = case M.lookup x env of
                  Nothing -> error "impossible"
                  Just s | LT.unlimitedChannel s -> LT.selfCombine t
                  Just s -> t

processM :: M.Map Name LT.Type -> Process LT.Type -> Process TypeN
processM env = mapProcess typeM . mapProcessX True bumpM env
