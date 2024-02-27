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

module DeadLock.Type where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (chr, ord)
import qualified Data.List as L

import Data.Set.Unicode
import Data.Bool.Unicode

import Process (Tag)
import BasicType
import DeadLock.Polarity

type TypeV = Int

data Kind l tic = Unlimited
                | Linear l tic
                deriving (Eq, Ord)

instance (Show l, Show tic) => Show (Kind l tic) where
  show Unlimited = ""
  show (Linear l tic) = "{" ++ show l ++ ", " ++ show tic ++ "}"
  
data PreType l tic = TVar TypeV
                   | TBasic BasicType
                   | TChannel Polarity (PreType l tic) (Kind l tic)
                   | TProduct (PreType l tic) (PreType l tic)
                   | TVariant (M.Map Tag (PreType l tic))
                   | TRec TypeV (PreType l tic)
                   deriving (Eq, Ord)

instance (Show l, Show tic) => Show (PreType l tic) where
  show = auxS 0 []
    where
      auxS next vmap (TVariant m) =
        L.intercalate " ⊕ " $ L.map (auxTag next vmap) $ M.toList m
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
      auxA next vmap (TChannel pol t kind) =
        showPolarity pol ++ "[" ++ auxS next vmap t ++ "]" ++ show kind
      auxA next vmap (TRec n t) =
        let next' = next + 1 in
        let vmap' = (n, next) : vmap in
        "μ" ++ nameOfVar next ++ "." ++ auxA next' vmap' t
      auxA next vmap t = "(" ++ auxS next vmap t ++ ")"
        
      nameOfVar :: Int -> String
      nameOfVar n = [chr $ ord 'α' + n `mod` 4] ++ if n < 4 then "" else show (n `div` 4)

type TypeN = PreType () ()
type TypeC = PreType Int Int

data NakedLevel = NakedBottom
                | NakedLinear
                | NakedTop
                  deriving (Eq, Ord)

instance Show NakedLevel where
  show NakedBottom = "⊥"
  show NakedLinear = "L"
  show NakedTop = "⊤"
  
nakedLevel :: (Ord l, Ord tic, Show l, Show tic) => PreType l tic -> NakedLevel
nakedLevel = aux S.empty
  where
    aux tset t | t ∈ tset = NakedTop
               | otherwise = auxT (S.insert t tset) (unfold t)

    auxT _ (TBasic _) = NakedTop
    auxT _ (TChannel pol _ (Linear _ _)) | S.null pol = NakedTop
                                         | otherwise = NakedLinear
    auxT _ (TChannel pol _ Unlimited) | CInput ∈ pol = NakedBottom
                                      | otherwise = NakedTop
    auxT tset (TProduct t1 t2) = min (aux tset t1) (aux tset t2)
    auxT tset (TVariant m) = minimum $ map (aux tset) (M.elems m)
    auxT _ t = error $ "cannot compute naked level of " ++ show t
  
unlimited :: (Ord l, Ord tic, Show l, Show tic) => PreType l tic -> Bool
unlimited = (== NakedTop) . nakedLevel

equal :: (Ord l, Ord tic, Show l, Show tic) => PreType l tic -> PreType l tic -> Bool
equal = aux S.empty
  where
    aux tset t1 t2 | (t1, t2) ∈ tset = True
                   | otherwise = auxE (S.insert (t1, t2) tset) (unfold t1) (unfold t2)

    auxE _ (TBasic bt1) (TBasic bt2) = bt1 == bt2
    auxE tset (TChannel pol1 t1 kind1) (TChannel pol2 t2 kind2) =
      pol1 == pol2 ∧ aux tset t1 t2 ∧ kind1 == kind2
    auxE tset (TProduct t1 t2) (TProduct s1 s2) = aux tset t1 s1 ∧ aux tset t2 s2
    auxE tset (TVariant m1) (TVariant m2) =
      M.keysSet m1 == M.keysSet m2 ∧ all (uncurry $ aux tset) (M.elems $ M.intersectionWith (,) m1 m2)
    auxE _ t1 t2 = error $ "equal: cannot decide whether " ++ show t1 ++ " and " ++ show t2 ++ " are equal"
  
combine :: TypeN -> TypeN -> TypeN
combine = aux M.empty 0
  where
    aux :: M.Map (TypeN, TypeN) Int -> Int -> TypeN -> TypeN -> TypeN
    aux m _ t1 t2 | Just tvar <- M.lookup (t1, t2) m = TVar tvar
    aux m next t1 t2 =
      let m' = M.insert (t1, t2) next m in
      auxC m' (next + 1) (unfold t1) (unfold t2)

    auxC :: M.Map (TypeN, TypeN) Int -> Int -> TypeN -> TypeN -> TypeN
    auxC _ _ (TBasic bt1) (TBasic bt2) | bt1 == bt2 = TBasic bt1
    auxC _ _ (TChannel pol1 t1 Unlimited) (TChannel pol2 t2 Unlimited) | equal t1 t2 =
      TChannel (pol1 ∪ pol2) t1 Unlimited
    auxC _ _ (TChannel pol1 t1 (Linear () ())) (TChannel pol2 t2 (Linear () ())) | S.null (pol1 ∩ pol2) ∧ equal t1 t2 =
      TChannel (pol1 ∪ pol2) t1 (Linear () ())
    auxC m next (TProduct t11 t12) (TProduct t21 t22) =
      let s1 = auxC m next t11 t21 in
      let s2 = auxC m next t12 t22 in
      TProduct s1 s2
    auxC m next (TVariant m1) (TVariant m2) | M.keysSet m1 == M.keysSet m2 =
      TVariant $ M.intersectionWith (auxC m next) m1 m2
    auxC _ _ t1 t2 = error $ "cannot combine " ++ show t1 ++ " and " ++ show t2
      
-- levelsOfType :: TypeE -> S.Set LevelE
-- levelsOfType (LChannel _ _ l _) = S.singleton l
-- levelsOfType (UChannel _ _) = S.empty -- BEWARE: only finite levels are returned
-- levelsOfType (t1 :×: t2) = levelsOfType t1 ∪ levelsOfType t2
-- levelsOfType (TVariant m) = S.unions $ map levelsOfType $ M.elems m
-- levelsOfType (TRec _ t) = levelsOfType t
-- levelsOfType _ = S.empty

unfold :: PreType l tic -> PreType l tic
unfold t@(TRec tvar s) = unfold (subst t tvar s)
unfold t = t

-- substLTT :: (l -> l) -> (tic -> tic) -> PreType tvar l tic -> PreType tvar l tic
-- substLTT ml mtic (TRec tvar t) = TRec tvar (substLTT ml mtic t)
-- substLTT ml mtic (LChannel pol s l tic) = LChannel pol (substLTT ml mtic s) (ml l) (mtic tic)
-- substLTT ml mtic (UChannel pol s) = UChannel pol (substLTT ml mtic s)
-- substLTT ml mtic (t1 :×: t2) = substLTT ml mtic t1 :×: substLTT ml mtic t2
-- substLTT ml mtic (TVariant m') = TVariant $ M.map (substLTT ml mtic) m'
-- substLTT _ _ t = t

subst :: PreType l tic -> TypeV -> PreType l tic -> PreType l tic
subst t tvar (TVar tvar') | tvar == tvar' = t
                          | otherwise = TVar tvar'
subst _ _ t@(TBasic _) = t
subst _ tvar t@(TRec tvar' _) | tvar == tvar' = t
subst t tvar (TRec tvar' s) = TRec tvar' $ subst t tvar s
subst t tvar (TChannel pol s kind) = TChannel pol (subst t tvar s) kind
subst t tvar (TProduct t1 t2) = TProduct (subst t tvar t1) (subst t tvar t2)
subst t tvar (TVariant m) = TVariant $ M.map (subst t tvar) m

-- ftv :: Ord tvar => PreType tvar l t -> S.Set tvar
-- ftv (TVar tvar) = S.singleton tvar
-- ftv (TRec tvar t) = S.delete tvar $ ftv t
-- ftv (LChannel _ t _ _) = ftv t
-- ftv (UChannel _ t) = ftv t
-- ftv (t :×: s) = ftv t ∪ ftv s
-- ftv (TVariant m) = S.unions $ map ftv $ M.elems m
-- ftv _ = S.empty

