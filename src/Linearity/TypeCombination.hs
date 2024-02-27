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

module Linearity.TypeCombination where

import Linearity.Use
import Linearity.Type
import qualified Linearity.Combination as Combination

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.MultiSet as MS

type TypeC = Combination.T TypeE
type TypeS = Combination.T Type

vv :: Combination.T (PreType u) -> S.Set TypeV
vv = S.unions . map Linearity.Type.vv . Combination.distinctElems

ftv :: Combination.T (PreType u) -> S.Set TypeV
ftv = S.unions . map Linearity.Type.ftv . Combination.distinctElems

fuv :: TypeC -> S.Set UseV
fuv = S.unions . map Linearity.Type.fuv . Combination.distinctElems 

proper :: TypeC -> Bool
proper = all Linearity.Type.proper . Combination.distinctElems

data CType u = CEmpty
             | CBasic (PreType u)
             | CChannel (PreType u) (Combination.T u) (Combination.T u)
             | CProd (Combination.T (PreType u)) (Combination.T (PreType u))
             | CVariant (M.Map Tag (Combination.T (PreType u))) 
             | CVars (Combination.T (PreType u))
             | CPartial (Combination.T (PreType u)) (Combination.T (PreType u))
             deriving Show

substUC :: UseEnvironment -> TypeC -> Combination.T Type
substUC uenv = Combination.map (substUT uenv)

gather :: (Ord u, Show u) => Combination.T (PreType u) -> CType u
gather tc = if Combination.null tvarc then aux (Combination.toList tproperc)
            else if Combination.null tproperc then CVars tvarc
            else CPartial tvarc tproperc
    where
      (tproperc, tvarc) = Combination.partition Linearity.Type.proper tc

      aux [] = CEmpty
      aux ts@(t : _) | basic t = CBasic t
      aux ts@(TChannel t _ _ : _) =
          let (u1, u2) = foldl auxChannel (Combination.empty, Combination.empty) ts in
          CChannel t u1 u2
      aux ts@(TProduct _ _ : _) = 
          let (tc1, tc2) = foldl auxProd (Combination.empty, Combination.empty) ts in
          CProd tc1 tc2
      aux (TVariant m _ : ts) = CVariant (foldl auxVariant (M.map Combination.singleton m) ts)

      auxChannel (uc1, uc2) (TChannel _ u1 u2) = (Combination.insert u1 uc1, Combination.insert u2 uc2)
      auxChannel _ t = error $ "gather channel: " ++ show t

      auxProd (tc1, tc2) (TProduct t1 t2) = (Combination.insert t1 tc1, Combination.insert t2 tc2)
      auxProd _ t = error $ "gather pair: " ++ show t

      auxVariant m (TVariant m' _) = M.intersectionWith Combination.insert m' m
      auxVariant _ t = error $ "gather variant: " ++ show t
