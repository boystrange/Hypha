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

module Session.Decompiler where

import Aux
import BasicType
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Set.Unicode
import Data.Bool.Unicode

import qualified Linearity.Type as LT
import qualified Session.Type as ST

import Debug.Trace

isSessionType :: LT.Type -> Bool
isSessionType = auxT S.empty
  where
    auxT tset t | t ∈ tset = True
    auxT tset t =
      let tset' = S.insert t tset in
      auxT0 tset' (LT.unfold t)

    auxT0 _ (LT.TBasic TUnit) = True
    auxT0 tset (LT.TChannel t u1 u2) = u1 + u2 == 1
    auxT0 _ _ = False
    
decompile :: LT.Type -> ST.Type
decompile = auxT 0 M.empty ST.In
  where
    auxT _ tmap pol t | Just tvar <- M.lookup (pol, t) tmap = ST.TVar tvar
    auxT next tmap pol t =
      let next' = next + 1 in
      let tmap' = M.insert (pol, t) next tmap in
      recT next $ auxT0 next' tmap' pol (LT.unfold t)

    auxT0 _ _ _ (LT.TBasic TUnit) = ST.CEnd
    auxT0 _ _ _ (LT.TBasic bt) = ST.TBasic bt
    auxT0 next tmap pol (LT.TChannel s u1 u2) | u1 == 1 ∧ u2 == 0 = auxM next tmap (polarity pol ST.In) s
    auxT0 next tmap pol (LT.TChannel s u1 u2) | u1 == 0 ∧ u2 == 1 = auxM next tmap (polarity pol ST.Out) s
    auxT0 next tmap _ (LT.TChannel s u1 u2) = ST.TChannel u1 u2 (auxT next tmap ST.In s)
    auxT0 next tmap _ (LT.TProduct t1 t2) = ST.TProduct (auxT next tmap ST.In t1) (auxT next tmap ST.In t2)
    auxT0 next tmap _ (LT.TVariant m _) = ST.TVariant (M.map (auxT next tmap ST.In) m)

    polarity ST.In = id
    polarity ST.Out = ST.dual
    
    auxM next tmap pol t =
      case LT.unfold t of
        LT.TProduct t1 t2 | isSessionType t2 -> ST.CMessage pol (auxT next tmap ST.In t1) (auxT next tmap pol t2)
        LT.TVariant m _ -> ST.CVariant pol (M.map (auxT next tmap pol) m)
        _ -> ST.CMessage pol (auxT next tmap ST.In t) ST.CEnd

    recT tvar t | tvar ∈ ST.ftv t = ST.TRec tvar t
    recT _ t = t
    
