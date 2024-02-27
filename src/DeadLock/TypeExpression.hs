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

module DeadLock.TypeExpression where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.MultiSet as MS

import Data.Set.Unicode
import Data.Bool.Unicode

import Aux
import Process (Tag)
import BasicType
import DeadLock.Polarity
import DeadLock.Type
import DeadLock.ValueExpression

import Debug.Trace

data LevelV = LVar Int
              deriving (Eq, Ord)

instance Show LevelV where
  show (LVar n) = "l" ++ show n

instance Read LevelV where
  readsPrec n ('l' : cs) = map (mapFst LVar) $ readsPrec n cs
  readsPrec _ _ = []
    
data TicketV = TiVar Int
               deriving (Eq, Ord)

instance Show TicketV where
  show (TiVar n) = (if n < 0 then "t" else "-t") ++ show (abs n) -- ticket variables are always negative

instance Read TicketV where
  readsPrec n ('t' : cs) = map (mapFst (\x -> TiVar (-x))) $ readsPrec n cs
  readsPrec _ _ = []
  
type LevelE = ValueE Int LevelV
type TicketE = ValueE Int TicketV

data TypeE = TEBasic BasicType
           | TEVar TypeV LevelE TicketE
           | TEChannel Polarity TypeE (Kind LevelE TicketE)
           | TEProduct TypeE TypeE
           | TEVariant (M.Map Tag TypeE) (Maybe TypeV)
             deriving (Eq, Ord)

instance Show TypeE where
  show = auxS
    where
      auxS (TEVariant m Nothing) = L.intercalate " ⊕ " $ L.map auxTag $ M.toList m
      auxS (TEVariant m (Just n)) = (L.intercalate " ⊕ " $ L.map auxTag $ M.toList m) ++ " ⋮ " ++ auxV n
      auxS t = auxP t

      auxTag (tag, TEBasic TUnit) = tag
      auxTag (tag, t) = tag ++ " " ++ auxA t

      auxP (TEProduct t s) = auxA t ++ " × " ++ auxP s
      auxP t = auxA t

      auxA (TEBasic bt) = show bt
      auxA (TEVar n l tic) = auxV n ++ auxShift l tic
      auxA (TEChannel pol t kind) = showPolarity pol ++ "[" ++ auxS t ++ "]" ++ show kind
      auxA t = "(" ++ auxS t ++ ")"

      auxV n = "τ" ++ show n
      
      auxShift l tic | l == valueNil ∧ tic == valueNil = ""
                     | otherwise = "${" ++ show l ++ ", " ++ show tic ++ "}"

basic :: TypeE -> Bool
basic (TEBasic _) = True
basic _ = False

shift :: LevelE -> TicketE -> TypeE -> TypeE
shift dl dtic (TEVar tvar l tic) = TEVar tvar (dl @+@ l) (dtic @+@ tic)
shift dl dtic (TEChannel pol t (Linear l tic)) = TEChannel pol t (Linear (dl @+@ l) (dtic @+@ tic))
shift dl dtic (TEProduct t1 t2) = TEProduct (shift dl dtic t1) (shift dl dtic t2)
shift dl dtic (TEVariant m topt) = TEVariant (M.map (shift dl dtic) m) topt
shift _ _ t = t

data Difference = Wrong | Any | Exactly LevelE TicketE

unshift :: TypeE -> TypeE -> Maybe (LevelE, TicketE)
unshift t1 t2 =
  case aux t1 t2 of
    Wrong -> Nothing
    Any -> Just (valueNil, valueNil)
    Exactly dl dtic -> Just (dl, dtic)
  where  
    aux (TEBasic bt1) (TEBasic bt2) | bt1 == bt2 = Any
    aux (TEVar tvar1 l1 tic1) (TEVar tvar2 l2 tic2) | tvar1 == tvar2 = Exactly (l1 @-@ l2) (tic1 @-@ tic2)
    aux (TEChannel pol1 t1 Unlimited) (TEChannel pol2 t2 Unlimited) | pol1 == pol2 ∧ t1 == t2 = Any
    aux (TEChannel pol1 t1 (Linear l1 tic1)) (TEChannel pol2 t2 (Linear l2 tic2)) | pol1 == pol2 ∧ t1 == t2 =
      Exactly (l1 @-@ l2) (tic1 @-@ tic2)
    aux (TEProduct t11 t12) (TEProduct t21 t22) = aux t11 t21 `merge` aux t12 t22
    aux (TEVariant m1 topt1) (TEVariant m2 topt2) | M.keysSet m1 == M.keysSet m2 ∧ topt1 == topt2 =
      foldl merge Any (M.elems $ M.intersectionWith aux m1 m2)
    aux _ _ = Wrong

    merge Any dist = dist
    merge dist Any = dist
    merge (Exactly dl1 dtic1) (Exactly dl2 dtic2) | dl1 == dl2 ∧ dtic1 == dtic2 = Exactly dl1 dtic1
    merge _ _ = Wrong

tunit :: TypeE
tunit = TEBasic TUnit

tbool :: TypeE
tbool = TEBasic TBool

tint :: TypeE
tint = TEBasic TInt

findD :: M.Map TypeV TypeE -> TypeE -> TypeE
findD m s@(TEVar tvar l tic) | Just t <- M.lookup tvar m = findD m $ shift l tic t
findD m (TEVariant _ (Just tvar)) | Just t <- M.lookup tvar m = findD m t
findD _ t = t

approximate :: TypeE -> Maybe (LevelE, TicketE)
approximate (TEChannel _ _ (Linear l t)) = Just (l,t)
approximate _ = Nothing

approximate' :: TypeE -> M.Map LevelV Int -> M.Map TicketV Int -> Maybe (LevelE, TicketE)
approximate' (TEChannel _ _ (Linear lexp texp)) lmap tmap = Just (mapEnv lexp lmap, mapEnv texp tmap)
  where
    mapEnv :: (Ord a) => ValueE Int a -> M.Map a Int -> ValueE Int a
    mapEnv (ValueE n pos neg) sol = let (ppos, pn) = mapMS pos sol
                                        (nneg, nn) = mapMS neg sol
                                    in
                                     ValueE (n + pn - nn) ppos nneg

    mapMS :: (Ord a) => MS.MultiSet a -> M.Map a Int -> (MS.MultiSet a, Int)
    mapMS vars sol = let (ms, n) = aux (MS.toOccurList vars) sol
                     in
                      (MS.fromOccurList ms, n)
      where
        aux :: (Ord a) => [(a, Int)] -> M.Map a Int -> ([(a, Int)], Int)
        aux [] sol = ([], 0)
        aux (x:xs) sol = let (var, n) = x
                             (occ, acc) = aux xs sol
                         in case (M.lookup var sol) of
                           Just m -> (occ, m * n + acc)
                           Nothing -> (x:occ, acc)
approximate' _ _ _ = Nothing


showMS :: Show lvar => String -> MS.MultiSet lvar -> String
showMS op = L.intercalate (" " ++ op ++ " ") . L.map showElement . MS.toOccurList
  where
    showElement (x, 1) = show x
    showElement (x, n) = show n ++ "." ++ show x

levelVars :: TypeE -> S.Set LevelV
levelVars (TEChannel _ _ (Linear l _)) = varsE l
levelVars (TEProduct t1 t2) = levelVars t1 ∪ levelVars t2
levelVars (TEVariant m _) = S.unions $ map levelVars $ M.elems m
levelVars _ = S.empty

ticketVars :: TypeE -> S.Set TicketV
ticketVars (TEChannel _ _ (Linear _ tic)) = varsE tic
ticketVars (TEProduct t1 t2) = ticketVars t1 ∪ ticketVars t2
ticketVars (TEVariant m _) = S.unions $ map ticketVars $ M.elems m
ticketVars _ = S.empty
