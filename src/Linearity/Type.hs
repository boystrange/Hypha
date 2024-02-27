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

module Linearity.Type where

import Aux
import BasicType
import Linearity.Use
import Linearity.Combination
import Linearity.UseCombination

import Data.Char (chr, ord)
import Data.Bool.Unicode
import Data.Set.Unicode
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

type Tag = String
type TypeV = Int

data PreType u = TVar TypeV
               | TBasic BasicType
               | TChannel (PreType u) u u
               | TProduct (PreType u) (PreType u)
               | TVariant (M.Map Tag (PreType u)) (Maybe TypeV)
               | TRec TypeV (PreType u)
            deriving (Eq, Ord)

type TypeE = PreType UseC
type Type = PreType Use

tunknown :: PreType u
tunknown = TVar 0

tunit :: PreType u
tunit = TBasic TUnit

tbool :: PreType u
tbool = TBasic TBool

tint :: PreType u
tint = TBasic TInt

instance Show u => Show (PreType u) where
    show = showPretty

showPolarity :: Show u => u -> u -> String
showPolarity u1 u2 = "{" ++ show u1 ++ "," ++ show u2 ++ "}"

showPretty :: Show u => PreType u -> String
showPretty = auxS 0 []
    where
      auxS next vmap (TVariant m Nothing) =
          L.intercalate " ⊕ " $ L.map (auxTag next vmap) $ M.toList m
      auxS next vmap (TVariant m _) =
          (L.intercalate " ⊕ " $ L.map (auxTag next vmap) $ M.toList m) ++ " ⊕ ⋯"
      auxS next vmap t = auxP next vmap t

      auxTag next vmap (tag, TBasic TUnit) = tag
      auxTag next vmap (tag, t) = tag ++ " " ++ auxA next vmap t

      auxP next vmap (TProduct t s) = auxA next vmap t ++ " × " ++ auxP next vmap s
      auxP next vmap t = auxA next vmap t

      auxA _ _ (TBasic bt) = show bt
      auxA _ vmap (TVar n) =
          case lookup n vmap of
            Nothing -> "τ" ++ show n
            Just m -> nameOfVar m
      auxA next vmap (TChannel t u1 u2) =
          showPolarity u1 u2 ++ "[" ++ auxS next vmap t ++ "]"
      auxA next vmap (TRec n t) = let next' = next + 1 in
          let vmap' = (n, next) : vmap in
          "μ" ++ nameOfVar next ++ "." ++ auxA next' vmap' t
      auxA next vmap t = "(" ++ auxS next vmap t ++ ")"

      nameOfVar :: Int -> String
      nameOfVar n = [chr $ ord 'α' + n `mod` 4] ++ if n < 4 then "" else show (n `div` 4)

basic :: PreType u -> Bool
basic (TBasic _) = True
basic _ = False

proper :: PreType u -> Bool
proper (TVar _) = False
proper _ = True

almostKnown :: TypeE -> Bool
almostKnown = auxT
  where
    auxT (TBasic _) = True
    auxT (TChannel _ _ _) = True
    auxT (TProduct t1 t2) = auxT t1 ∧ auxT t2
    auxT _ = False

vv :: PreType u -> S.Set TypeV
vv (TChannel t _ _) = vv t
vv (TProduct t s) = vv t ∪ vv s
vv (TVariant m Nothing) = S.unions $ L.map vv $ M.elems m
vv (TVariant m (Just tvar)) = S.insert tvar $ S.unions $ L.map vv $ M.elems m
vv (TRec _ t) = vv t
vv _ = (∅)

ftv :: PreType u -> S.Set TypeV
ftv (TVar tvar) = S.singleton tvar
ftv (TChannel t _ _) = ftv t
ftv (TProduct t s) = ftv t ∪ ftv s
ftv (TVariant m _) = S.unions $ L.map ftv $ M.elems m
ftv (TRec tvar t) = S.delete tvar $ ftv t
ftv _ = (∅)

fuv :: TypeE -> S.Set UseV
fuv (TChannel t u1 u2) = Linearity.Type.fuv t ∪
                         Linearity.UseCombination.fuv u1 ∪
                         Linearity.UseCombination.fuv u2
fuv (TProduct t s) = Linearity.Type.fuv t ∪ Linearity.Type.fuv s
fuv (TVariant m _) = S.unions $ L.map Linearity.Type.fuv $ M.elems m
fuv (TRec _ t) = Linearity.Type.fuv t
fuv _ = (∅)

substUT :: UseEnvironment -> TypeE -> Type
substUT _ (TBasic bt) = TBasic bt
substUT _ (TVar tvar) = TVar tvar
substUT env (TChannel t uc1 uc2) = TChannel (substUT env t) (substUU env uc1) (substUU env uc2)
substUT env (TProduct t1 t2) = TProduct (substUT env t1) (substUT env t2)
substUT env (TVariant m mvars) = TVariant (M.map (substUT env) m) mvars
substUT env (TRec tvar t) = TRec tvar (substUT env t)

type PreTypeEnvironment a = M.Map TypeV (PreType a)
type TypeEnvironment = PreTypeEnvironment UseC

variablesM :: (a -> S.Set TypeV) -> M.Map TypeV a -> S.Set TypeV
variablesM ftv = S.unions . L.map ftv . M.elems

findV :: PreTypeEnvironment a -> TypeV -> TypeV
findV m tvar | Just (TVar tvar') <- M.lookup tvar m = findV m tvar'
findV _ tvar = tvar

find :: PreTypeEnvironment a -> PreType a -> PreType a
find m (TVar tvar) | Just s <- M.lookup tvar m = find m s
find m (TVariant _ (Just tvar)) | Just s <- M.lookup tvar m = find m s
find _ t = t

substTT :: PreTypeEnvironment a -> PreType a -> PreType a
substTT env (TVar tvar) | Just t <- M.lookup tvar env = t
substTT env (TChannel t u1 u2) = TChannel (substTT env t) u1 u2
substTT env (TProduct t1 t2) = TProduct (substTT env t1) (substTT env t2)
substTT env (TVariant m mvars) = TVariant (M.map (substTT env) m) mvars
substTT env (TRec tvar t) = TRec tvar (substTT (M.delete tvar env) t)
substTT _ t = t

unfold :: PreType a -> PreType a
unfold t@(TRec tvar s) = unfold $ substTT (M.singleton tvar t) s
unfold t = t

equiv :: Ord a => PreType a -> PreType a -> Bool
equiv = aux S.empty
  where
    aux checked t1 t2 | (t1, t2) ∈ checked = True
    aux checked t1 t2 =
      let checked' = S.insert (t1, t2) checked in
      aux0 checked' (unfold t1) (unfold t2)

    aux0 _ (TBasic bt1) (TBasic bt2) = bt1 == bt2
    aux0 checked (TChannel t1 u11 u12) (TChannel t2 u21 u22) =
      u11 == u21 ∧ u12 == u22 ∧ aux checked t1 t2
    aux0 checked (TProduct t11 t12) (TProduct t21 t22) =
      aux checked t11 t21 ∧ aux checked t12 t22
    aux0 checked (TVariant m1 Nothing) (TVariant m2 Nothing) =
      M.keysSet m1 == M.keysSet m2 ∧ Prelude.all (uncurry $ aux checked) (M.elems $ M.intersectionWith (,) m1 m2)
    aux0 _ _ _ = False

unlimitedChannel :: Type -> Bool
unlimitedChannel (TVar _) = error "this should not happen"
unlimitedChannel (TRec _ t) = unlimitedChannel t
unlimitedChannel (TChannel _ u1 u2) = u1 == ω ∨ u2 == ω
unlimitedChannel _ = False

selfCombine :: Type -> Type
selfCombine (TChannel t u1 u2) = TChannel t (u1 + u1) (u2 + u2)
selfCombine (TProduct t1 t2) = TProduct (selfCombine t1) (selfCombine t2)
selfCombine (TVariant m mvars) = TVariant (M.map selfCombine m) mvars
selfCombine (TRec tvar t) = TRec tvar (selfCombine t)
selfCombine t = t
