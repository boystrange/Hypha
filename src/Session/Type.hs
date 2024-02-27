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

module Session.Type where

import Aux
import BasicType
import Linearity.Use

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Char (chr, ord)
import Data.Set.Unicode
import Data.Bool.Unicode

import Debug.Trace
import Text.PrettyPrint.Leijen

type TypeV = Int
type Tag = String

data Polarity = In
              | Out
                deriving (Eq, Ord)

data Type = TVar TypeV
          | TBasic BasicType
          | TChannel Use Use Type
          | TProduct Type Type
          | TVariant (M.Map Tag Type)
          | TRec TypeV Type
            -- session types
          | CEnd
          | CMessage Polarity Type Type
          | CVariant Polarity (M.Map Tag Type)
            deriving (Eq, Ord)

render :: Doc -> String
render x = displayS (renderPretty (2/3) 100 x) ""

dual :: Polarity -> Polarity
dual In = Out
dual Out = In

showBinding :: String -> Type -> String
showBinding x t = render $ ppB x t

ppB :: String -> Type -> Doc
ppB x t = indent 2 $ text x <+> colon <+> ppT t

ppT :: Type -> Doc
ppT = auxS 0 M.empty
  where
    auxS next vmap (TVariant m) = align $ sep $ punctuate (text " ⊕") $ L.map (auxTag next vmap) $ M.toList m
    auxS next vmap t = auxP next vmap t

    auxTag next vmap (tag, TBasic TUnit) = text tag
    auxTag next vmap (tag, t) = text tag <+> auxA next vmap t

    auxP next vmap (TProduct t s) = auxA next vmap t <+> text "×" <+> auxP next vmap s
    auxP next vmap t = auxA next vmap t

    auxA _ _ (TBasic bt) = text $ show bt
    auxA _ _ CEnd = text "end"
    auxA _ vmap (TVar n) = text $ auxV vmap n
    auxA next vmap (CMessage pol t CEnd) = auxPolarity pol <> brackets (auxS next vmap t)
    auxA next vmap (CMessage pol t st)   = align $ cat [auxPolarity pol <> brackets (auxS next vmap t) <> text ".", auxA next vmap st]
    auxA next vmap (CVariant pol m)      = auxPolarity pol <> braces (align $ sep $ punctuate comma $ L.map (auxTagC next vmap) $ M.toList m)
    auxA next vmap (TChannel u1 u2 t) = braces (text (show u1) <> comma <> text (show u2)) <> brackets (auxS next vmap t)
    auxA next vmap (TRec n t) =
      let next' = next + 1 in
      let vmap' = M.insert n next vmap in
      text ("μ" ++ nameOfVar next ++ ".") <> auxA next' vmap' t
    auxA next vmap t = parens $ auxS next vmap t

    auxV vmap n | Just m <- M.lookup n vmap = nameOfVar m
                | otherwise = "τ" ++ show n

    auxPolarity In   = text "?"
    auxPolarity Out  = text "!"

    auxTagC next vmap (tag, st) = text tag <> colon <+> auxA next vmap st

    nameOfVar :: Int -> String
    nameOfVar n =
      [chr $ ord 'α' + n `mod` 4] ++ if n < 4 then "" else show (n `div` 4)

unfold :: Type -> Type
unfold t@(TRec tvar s) = unfold $ subst t tvar s
unfold t = t

subst :: Type -> TypeV -> Type -> Type
subst t tvar s@(TVar tvar') | tvar == tvar' = t
subst t tvar (TChannel u1 u2 s) = TChannel u1 u2 $ subst t tvar s
subst t tvar (TProduct t1 t2) = TProduct (subst t tvar t1) (subst t tvar t2)
subst t tvar (TVariant m) = TVariant $ M.map (subst t tvar) m
subst t tvar (TRec tvar' s) | tvar /= tvar' = TRec tvar' $ subst t tvar s
subst t tvar (CMessage pol t1 t2) = CMessage pol (subst t tvar t1) (subst t tvar t2)
subst t tvar (CVariant pol m) = CVariant pol $ M.map (subst t tvar) m
subst _ _ t = t

equiv :: Type -> Type -> Bool
equiv = aux S.empty
  where
    aux tset t1 t2 | (t1, t2) ∈ tset = True
    aux tset t1 t2 =
      let tset' = S.insert (t1, t2) tset in
      auxT tset' (unfold t1) (unfold t2)

    auxT _ CEnd CEnd = True
    auxT _ (TBasic bt1) (TBasic bt2) = bt1 == bt2
    auxT tset (TChannel u11 u12 t1) (TChannel u21 u22 t2) = u11 == u21 ∧ u12 == u22 ∧ aux tset t1 t2
    auxT tset (TProduct t11 t12) (TProduct t21 t22) = aux tset t11 t21 ∧ aux tset t12 t22
    auxT tset (TVariant m1) (TVariant m2) = M.keysSet m1 == M.keysSet m2 ∧ all (uncurry $ aux tset) (M.elems $ M.intersectionWith (,) m1 m2)
    auxT tset (CMessage pol1 t11 t12) (CMessage pol2 t21 t22) = pol1 == pol2 ∧ aux tset t11 t21 ∧ aux tset t12 t22
    auxT tset (CVariant pol1 m1) (CVariant pol2 m2) = pol1 == pol2 ∧ all (uncurry $ aux tset) (M.elems $ M.intersectionWith (,) m1 m2)
    auxT _ _ _ = False

refold :: Type -> Type
refold = aux 0 []
  where
    aux next ts t | Just tvar <- lookupBy equiv t ts = TVar tvar
    aux next ts t =
      let next' = next + 1 in
      let ts' = (t, next) : ts in
      possiblyRec next $ aux0 next' ts' (unfold t)

    aux0 _ _ CEnd = CEnd
    aux0 _ _ t@(TBasic _) = t
    aux0 next ts (TChannel u1 u2 t) = TChannel u1 u2 (aux next ts t)
    aux0 next ts (TProduct t1 t2) = TProduct (aux next ts t1) (aux next ts t2)
    aux0 next ts (TVariant m) = TVariant (M.map (aux next ts) m)
    aux0 next ts (CMessage pol t1 t2) = CMessage pol (aux next ts t1) (aux next ts t2)
    aux0 next ts (CVariant pol m) = CVariant pol (M.map (aux next ts) m)

    possiblyRec tvar t | tvar ∈ ftv t = TRec tvar t
                       | otherwise = t

instance Show Type where
  show = render . ppT

ftv :: Type -> S.Set TypeV
ftv (TVar tvar) = S.singleton tvar
ftv (TBasic _) = S.empty
ftv (TChannel _ _ t) = ftv t
ftv (TProduct t1 t2) = ftv t1 ∪ ftv t2
ftv (TVariant m) = S.unions $ map ftv (M.elems m)
ftv (TRec tvar t) = S.delete tvar $ ftv t
ftv CEnd = S.empty
ftv (CMessage _ t s) = ftv t ∪ ftv s
ftv (CVariant _ m) = S.unions $ map ftv (M.elems m)
