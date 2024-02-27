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

module Linearity.TypeSolver where

import Aux
import Linearity.Use
import Linearity.Type
import Linearity.Unicode
import Linearity.UseCombination
import Linearity.TypeCombination
import qualified Linearity.Combination as C

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Function.Unicode
import Data.Bool.Unicode
import Data.Set.Unicode

import Debug.Trace

variables :: M.Map TypeV (PreType a) -> M.Map TypeV (PreType a) -> M.Map TypeV (C.T (PreType a)) -> S.Set TypeV
variables eqM stM coM =
    Linearity.Type.variablesM Linearity.Type.ftv eqM ∪
    Linearity.Type.variablesM Linearity.Type.ftv stM ∪
    Linearity.Type.variablesM Linearity.TypeCombination.ftv coM

nullTypes :: Ord a => M.Map TypeV (PreType a) -> M.Map TypeV (C.T (PreType a)) -> S.Set (PreType a)
nullTypes eqM coM = limit generator base
    where
      base = S.fromList $ L.map (find eqM . TVar . fst) $ L.filter (C.null . snd) $ M.toList coM

      generator nset =
          nset ∪ (S.fromList $ L.map (TVar . fst)
                             $ L.filter ((∈ nset) . snd)
                             $ M.toList eqM)
               ∪ (S.fromList $ L.map (find eqM . TVar . fst)
                             $ L.filter (all (∈ nset) . C.distinctElems . snd)
                             $ M.toList coM)

type DefinitionMap = M.Map TypeS TypeV
type ZeroMap = M.Map Type TypeV

-- definition :: Bool -> M.Map TypeV Type -> M.Map TypeV Type -> M.Map TypeV TypeS -> Type -> Type
-- definition mp eqM stM coM t = defT (-1) M.empty M.empty t
--     where
--       bestD :: Type -> TypeS
--       bestD t = case find eqM t of
--                   TVar tvar | Just tc <- M.lookup tvar coM -> tc
--                   s -> C.singleton s

--       unlimited :: TypeV -> Bool
--       unlimited tvar | Just tc <- M.lookup tvar coM = tc == C.fromList [TVar tvar, TVar tvar]
--       unlimited _ = False

--       bestZ :: Type -> Type
--       bestZ t = case find stM t of
--                   TVar tvar | unlimited tvar -> TUnit
--                   s -> s

--       closeD :: TypeS -> TypeS
--       closeD = foldl (⊎) C.empty . map bestD . C.toList

--       defT :: TypeV -> DefinitionMap -> ZeroMap -> Type -> Type
--       defT nextVar defM zeroM = defC nextVar defM zeroM . C.singleton

--       nullT :: Type -> Bool
--       nullT = (∈ nullTypes eqM coM)

--       unconstrainedT :: Type -> Bool
--       unconstrainedT (TVar tvar) = not (tvar ∈ M.keysSet stM)
--       unconstrainedT _ = False

--       zeroT :: TypeV -> DefinitionMap -> ZeroMap -> Type -> Type
--       zeroT nextVar defM zeroM t | Just tvar <- M.lookup t zeroM = TVar tvar
--       zeroT nextVar defM zeroM t =
--           let zeroM' = M.insert t nextVar zeroM in
--           let nextVar' = nextVar + 1 in
--           case bestZ t of
--             TChannel s _ _ -> TChannel (defT nextVar' defM zeroM' s) 0 0
--             t1 :×: t2 -> (zeroT nextVar' defM zeroM' t1) :×: (zeroT nextVar' defM zeroM' t2)
--             t1 :⊕: t2 -> (zeroT nextVar' defM zeroM' t1) :⊕: (zeroT nextVar' defM zeroM' t2)
--             TVariant m _ -> TVariant (M.map (zeroT nextVar' defM zeroM') m) Nothing
--             t -> t

definitions :: M.Map TypeV Type -> M.Map TypeV Type -> M.Map TypeV TypeS -> M.Map TypeV Type
definitions eqM stM coM = defineM
    where
      defineM :: M.Map TypeV Type
      defineM = M.fromList $ map (\tvar -> (tvar, defineV (-1) M.empty M.empty tvar)) tvars

      expandC :: TypeS -> TypeS
      expandC = C.flatten . C.map expandT

      expandT :: Type -> TypeS
      expandT (TVar tvar) | Just tc <- M.lookup tvar coM = tc
      expandT t = C.singleton $ find eqM t -- BOH! DOVE VA FATTO IL FIND???

      defineT :: Int -> DefinitionMap -> ZeroMap -> Type -> Type
      defineT nextVar defM zeroM (TVar tvar) = defineV nextVar defM zeroM tvar
      defineT nextVar defM zeroM t = defineC nextVar defM zeroM $ C.singleton t

      defineV :: Int -> DefinitionMap -> ZeroMap -> TypeV -> Type
      defineV nextVar defM zeroM tvar | Just tc <- M.lookup tvar coM =
                                                   if C.null tc then zeroV nextVar defM zeroM tvar
                                                   else defineC nextVar defM zeroM tc
      defineV _ _ _ tvar = error $ "cannot find any definition for " ++ show (TVar tvar :: Type)

      defineC :: Int -> DefinitionMap -> ZeroMap -> TypeS -> Type
      defineC nextVar defM zeroM tc =
          let tc' = expandC tc in
          case M.lookup tc' defM of
            Just tvar -> TVar tvar
            Nothing ->
                let defM' = M.insert tc' nextVar defM in
                let nextVar' = nextVar - 1 in
                possiblyRec nextVar $
                case gather tc' of
                  CEmpty -> zeroT nextVar defM' zeroM $ C.findMin tc'
                  CBasic t -> t
                  CChannel t uc1 uc2 -> TChannel (defineT nextVar' defM' zeroM t) (evaluateUC uc1) (evaluateUC uc2)
                  CProd tc1 tc2 -> TProduct (defineC nextVar' defM' zeroM tc1) (defineC nextVar' defM' zeroM tc2)
                  CVariant m -> TVariant (M.map (defineC nextVar' defM' zeroM) m) Nothing
                  blob -> error $ "don't know what to do with " ++ show blob ++ " while expanding " ++ show tc'

      possiblyRec :: TypeV -> Type -> Type
      possiblyRec tvar t | tvar ∈ Linearity.Type.ftv t = TRec tvar t
                         | otherwise = t

      tvars = M.keys coM

      zeroV :: Int -> DefinitionMap -> ZeroMap -> TypeV -> Type
      zeroV nextVar defM zeroM tvar | Just t <- M.lookup tvar stM = zeroT nextVar defM zeroM t
      zeroV _ _ _ tvar = error $ "there is no structural information for " ++ show (TVar tvar :: Type)

      zeroT :: Int -> DefinitionMap -> ZeroMap -> Type -> Type
      zeroT nextVar defM zeroM t | Just tvar <- M.lookup t zeroM = TVar tvar
      zeroT nextVar defM zeroM t =
          let zeroM' = M.insert t nextVar zeroM in
          let nextVar' = nextVar - 1 in
          possiblyRec nextVar $
          case find stM t of
            TChannel s _ _ -> TChannel (defineT nextVar' defM zeroM' s) 0 0
            TProduct t1 t2 -> TProduct (zeroT nextVar' defM zeroM' t1) (zeroT nextVar' defM zeroM' t2)
            TVariant m _ -> TVariant (M.map (zeroT nextVar' defM zeroM') m) Nothing
            t -> t
