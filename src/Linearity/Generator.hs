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
-- Copyright 2013 Luca Padovani

module Linearity.Generator where

import Aux
import Process
import Linearity.Use
import Linearity.Type
import Linearity.Process
import Linearity.Unicode
import Linearity.UseCombination
import Linearity.TypeCombination

import qualified Relation
import qualified Linearity.Combination as Combination

import qualified Data.List as L
import qualified Data.Set as S
import Data.Bool.Unicode
import Data.Set.Unicode
import qualified Control.Monad.State.Lazy as ST
import qualified Data.Map.Strict as M
import Control.Monad

import Debug.Trace

altConstraints :: Bool
altConstraints = False

type NameEnvironment = M.Map Name TypeE

type Constraints = (M.Map TypeV TypeE,
                    M.Map TypeV TypeE,
                    M.Map TypeV TypeC,
                    M.Map TypeV (S.Set TypeC),
                    S.Set (UseC, UseC))

data GeneratorState = GeneratorState {
      -- eqrel :: S.Set (TypeE, TypeE) -- types that must be equal
      -- strel :: S.Set (TypeE, TypeE) -- types that must be structurally coherent
      eqmap :: M.Map TypeV TypeE, -- type variables that have been unified
      stmap :: M.Map TypeV TypeE, -- type variables that are structurally equivalent to a type
      comap :: M.Map TypeV TypeC, -- type variables expressing type combinations
      equse :: S.Set (UseC, UseC), -- use constraints
      nextId :: Int               -- next identifier for use/type variable
    }

emptyState :: GeneratorState
emptyState = GeneratorState {
               eqmap = M.empty,
               stmap = M.empty,
               comap = M.empty,
               equse = S.empty,
               nextId = 0
             }

data Mode = ModeEQ | ModeST
          deriving (Eq, Ord)

instance Show Mode where
    show ModeEQ = "="
    show ModeST = "~"

type Generator a = ST.State GeneratorState a

selectMap :: Mode -> GeneratorState -> M.Map Int TypeE
selectMap ModeEQ = eqmap
selectMap ModeST = stmap

undefinedTypes :: Generator (S.Set TypeV)
undefinedTypes = do state <- ST.get
                    return $ M.keysSet (stmap state) ∖ (M.keysSet (eqmap state) ∪ M.keysSet (comap state))

findM :: Mode -> TypeE -> Generator TypeE
findM mode t = do state <- ST.get
                  return $ find (selectMap mode state) t

getM :: Mode -> Generator (M.Map TypeV TypeE)
getM mode = do state <- ST.get
               return $ selectMap mode state

makeOpenVariant :: Process.Tag -> TypeE -> Generator TypeE
makeOpenVariant tag t = do tvar <- getNextId
                           return $ TVariant (M.singleton tag t) (Just tvar)

addEQU :: UseC -> UseC -> Generator ()
addEQU u1 u2 = ST.modify (\st -> st { equse = S.insert (u1, u2) (equse st) })

unifyVar :: Mode -> TypeV -> TypeE -> Generator ()
unifyVar mode tvar t = do ST.modify (\st -> st { stmap = M.insert tvar t (stmap st) })
                          when (mode == ModeEQ) (ST.modify (\st -> st { eqmap = M.insert tvar t (eqmap st) }))

unify :: Mode -> TypeE -> TypeE -> Generator ()
unify = aux (∅)
    where
      aux mem mode t s =
          do t' <- findM mode t
             s' <- findM mode s
             let mem' = S.insert (mode, t', s') mem
             case (mode, t', s') of
               mts | mts ∈ mem -> return ()
               (_, TBasic bt1, TBasic bt2) | bt1 == bt2 -> return ()
               (_, TVar tvar, _) | Linearity.Type.proper s' -> unifyVar mode tvar s'
               (_, _, TVar tvar) | Linearity.Type.proper t' -> unifyVar mode tvar t'
               (_, TVar tvar1, TVar tvar2) | tvar1 < tvar2 -> unifyVar mode tvar2 t'
                                           | tvar1 > tvar2 -> unifyVar mode tvar1 s'
                                           | otherwise -> return ()
               (ModeEQ, TChannel t'' u1 u2, TChannel s'' v1 v2) -> do aux mem' mode t'' s''
                                                                      addEQU u1 v1
                                                                      addEQU u2 v2
               (ModeST, TChannel t'' _ _, TChannel s'' _ _) -> do aux mem' mode t'' s''
                                                                  aux (∅) ModeEQ t'' s''
               (_, TProduct t1 t2, TProduct s1 s2) -> aux mem' mode t1 s1 >> aux mem' mode t2 s2
               (_, TVariant m1 Nothing, TVariant m2 Nothing) | M.keysSet m1 == M.keysSet m2 ->
                     do let m = M.intersectionWith (,) m1 m2
                        forM_ (M.elems m) (uncurry $ aux mem' mode)
               (_, TVariant m1 (Just tvar), TVariant m2 Nothing) | M.keysSet m1 ⊆ M.keysSet m2 -> 
                     do unifyVar mode tvar s'
                        let m = M.intersectionWith (,) m1 m2
                        forM_ (M.elems m) (uncurry $ aux mem' mode)
               (_, TVariant m1 Nothing, TVariant m2 (Just tvar)) | M.keysSet m2 ⊆ M.keysSet m1 -> 
                     do unifyVar mode tvar t'
                        let m = M.intersectionWith (,) m1 m2
                        forM_ (M.elems m) (uncurry $ aux mem' mode)
               (_, TVariant m1 (Just tvar1), TVariant m2 (Just tvar2)) ->
                     do newt <- liftM (TVariant (M.union m1 m2) . Just) getNextId
                        let m = M.intersectionWith (,) m1 m2
                        forM_ (M.elems m) (uncurry $ aux mem' mode)
                        unifyVar mode tvar1 newt
                        unifyVar mode tvar2 newt
               _ -> error $ "type error: cannot unify " ++ show t' ++ " with " ++ show s'

unifyST :: TypeE -> TypeE -> Generator ()
unifyST = unify ModeST

unifyEQ :: TypeE -> TypeE -> Generator ()
unifyEQ t s = do unify ModeST t s
                 unify ModeEQ t s

combineT :: TypeV -> TypeE -> TypeE -> Generator ()
combineT tvar t1 t2 = do ST.modify (\st -> st { comap = M.insert tvar (Combination.fromList [t1, t2]) (comap st) })
                         unifyST (TVar tvar) t1
                         unifyST (TVar tvar) t2

newCombination :: TypeE -> TypeE -> Generator TypeE
newCombination t1 t2 = do tvar <- getNextId
                          combineT tvar t1 t2
                          return $ TVar tvar

getNextId :: Generator Int
getNextId = do old <- ST.get
               ST.modify (\st -> st { nextId = nextId st + 1 })
               return $ nextId old

instantiateType :: TypeE -> Generator TypeE
instantiateType = auxT
  where
    auxT (TVar _) = newType
    auxT (TBasic bt) = return $ TBasic bt
    auxT (TChannel t u1 u2) = do t' <- auxT t
                                 return $ TChannel t' u1 u2
    auxT _ = error "not implemented"
      
newType :: Generator TypeE
newType = liftM TVar getNextId

newUse :: Generator UseC
newUse = liftM (Combination.singleton . UVar) getNextId

unlimitedT :: TypeE -> Generator ()
unlimitedT t = do s <- newUnlimitedType
                  unifyEQ t s

newUnlimitedType :: Generator TypeE
newUnlimitedType = if altConstraints then
                       do tvar <- getNextId
                          let t = TVar tvar
                          combineT tvar t t
                          return t
                   else
                       do t <- newType
                          newCombination t t

unlimitedE :: NameEnvironment -> Generator NameEnvironment
unlimitedE env | altConstraints = do forM_ (M.elems env) unlimitedT
                                     return env
unlimitedE env = combine env env

combine :: NameEnvironment -> NameEnvironment -> Generator NameEnvironment
combine env1 env2 = mapM aux (S.toList $ M.keysSet env1 ∪ M.keysSet env2) >>= (return . M.fromList)
    where
      aux :: Name -> Generator (Name, TypeE)
      aux x = case (M.lookup x env1, M.lookup x env2) of
                (Nothing, Nothing) -> error "impossible!!!"
                (Just t, Nothing) -> return (x, t)
                (Nothing, Just t) -> return (x, t)
                (Just t1, Just t2) | almostKnown t1 ∧ almostKnown t2 ->
                  do s <- auxT t1 t2
                     return (x, s)
                (Just t1, Just t2) -> do t <- newCombination t1 t2
                                         return (x, t)

      auxT :: TypeE -> TypeE -> Generator TypeE
      auxT (TBasic bt1) (TBasic bt2) | bt1 == bt2 = return $ TBasic bt1
      auxT (TProduct t11 t12) (TProduct t21 t22) = do s1 <- auxT t11 t21
                                                      s2 <- auxT t21 t22
                                                      return $ TProduct s1 s2
      auxT (TChannel t1 uc11 uc12) (TChannel t2 uc21 uc22) =
        do unifyEQ t1 t2
           return $ TChannel t1 (Combination.union uc11 uc21) (Combination.union uc12 uc22)
      auxT _ _ = error "combine.auxT: impossible!!!"
            
merge :: NameEnvironment -> NameEnvironment -> Generator NameEnvironment
merge env1 env2 = mapM aux (S.toList $ (M.keysSet env1 ∪ M.keysSet env2)) >>= (return . M.fromList)
    where
      aux :: Name -> Generator (Name, TypeE)
      aux x = case (M.lookup x env1, M.lookup x env2) of
                (Nothing, Nothing) -> error "impossible!!!"
                (Just t, Nothing) | altConstraints -> do unlimitedT t
                                                         return (x, t)
                (Nothing, Just t) | altConstraints -> do unlimitedT t
                                                         return (x, t)
                (Just t, Nothing) -> do s <- newCombination t t
                                        return (x, s)
                (Nothing, Just t) -> do s <- newCombination t t
                                        return (x, s)
                (Just t, Just s) -> do unifyEQ t s
                                       return (x, t)

getType :: Name -> Maybe TypeE -> NameEnvironment -> Generator (TypeE, NameEnvironment)
getType x topt env = case (M.lookup x env, topt) of
                       (Just xt, Nothing) -> return (xt, env)
                       (Just xt, Just tx) -> do tx' <- instantiateType tx
                                                unifyEQ xt tx'
                                                return (tx', env)
                       (Nothing, Just tx) -> do tx' <- instantiateType tx
                                                unlimitedT tx'
                                                return (tx', M.insert x tx' env)
                       (Nothing, Nothing) -> do xt <- newUnlimitedType
                                                return (xt, M.insert x xt env)

combinations :: M.Map TypeV TypeE -> M.Map TypeV TypeC -> M.Map TypeV (S.Set TypeC)
combinations eqM coM = m
    where
      m :: M.Map TypeV (S.Set TypeC)
      m = M.fromList [ (tvar, Relation.image (find eqM (TVar tvar)) crel) | tvar <- S.toList tvars ]

      tvars :: S.Set TypeV
      tvars = (M.keysSet eqM ∪ M.keysSet coM ∪
               variablesM Linearity.Type.ftv eqM ∪
               variablesM Linearity.TypeCombination.ftv coM)
              ∖ (variablesM Linearity.Type.vv eqM ∪ variablesM Linearity.TypeCombination.vv coM)

      crel :: Relation.T TypeE TypeC
      crel = limit generator base

      base :: Relation.T TypeE TypeC
      base = (S.unions $ L.map (\(tvar, t) -> (createR (TVar tvar) (Combination.singleton t))) $ M.toList eqM) ∪
             (S.unions $ L.map (\(tvar, tc) -> (createR (TVar tvar) tc)) $ M.toList coM)

      generator = transitivity . symmetry . congruence

      symmetry :: Relation.T TypeE TypeC -> Relation.T TypeE TypeC
      symmetry rel =
          S.foldl (\res (t, tc) -> if Combination.size tc == 1 then
                                       S.insert (Combination.findMin tc, Combination.singleton t) res
                                   else
                                       res) rel rel

      congruence :: Relation.T TypeE TypeC -> Relation.T TypeE TypeC
      congruence rel =
          S.foldl (\res (t, tc) -> case (t, gather tc) of
                                     (TProduct t1 t2, CProd tc1 tc2) -> res ∪ createR t1 tc1 ∪ createR t2 tc2
                                     (TVariant m _, CVariant mc) -> res ∪ (S.unions $ M.elems $ M.intersectionWith createR m mc)
                                     _ -> res) rel rel

      createR :: TypeE -> TypeC -> Relation.T TypeE TypeC
      createR t tc = S.singleton (find eqM t, Combination.map (find eqM) tc)

      transitivity :: Relation.T TypeE TypeC -> Relation.T TypeE TypeC
      transitivity rel = Relation.map (\(t, tc) -> (t, Combination.flatten $ Combination.map (canonicalT rel) tc)) rel

      canonicalT :: Relation.T TypeE TypeC -> TypeE -> TypeC
      canonicalT _ t | Linearity.Type.proper t = Combination.singleton t
      canonicalT rel t = case L.filter Linearity.TypeCombination.proper $ S.toAscList $ Relation.image t rel of
                           [] -> Combination.singleton t
                           (tc : _) -> tc

useConstraints :: M.Map TypeV (S.Set TypeC) -> Relation.T UseC UseC
useConstraints = S.unions . L.map auxCS . M.elems
  where
    auxCS :: S.Set TypeC -> Relation.T UseC UseC
    auxCS tcset = 
      let ptcset = S.filter Linearity.TypeCombination.proper tcset in
      if S.null ptcset then Relation.empty
      else case gather $ S.findMin ptcset of
        CEmpty ->
          foldl (auxP Combination.empty Combination.empty)
                Relation.empty (S.toList $ S.deleteMin ptcset)
        CChannel _ uc1 uc2 ->
          foldl (auxP (Combination.flatten uc1) (Combination.flatten uc2))
                Relation.empty (S.toList $ S.deleteMin ptcset)
        _ -> Relation.empty

    auxP :: UseC -> UseC -> Relation.T UseC UseC -> TypeC -> Relation.T UseC UseC
    auxP uc1 uc2 rel tc =
      case gather tc of
        CChannel _ vc1 vc2 -> S.insert (uc1, Combination.flatten vc1) $
                              S.insert (uc2, Combination.flatten vc2) rel
        _ -> Relation.empty

addMissingDefinitions :: Generator ()
addMissingDefinitions = undefinedTypes >>= (mapM_ aux . S.toList)
  where
    aux :: TypeV -> Generator ()
    aux tvar =
      do t <- buildV M.empty tvar
         unifyEQ (TVar tvar) t

    buildV :: M.Map TypeV TypeV -> TypeV -> Generator TypeE
    buildV mem tvar | Just tvar' <- M.lookup tvar mem = return $ TVar tvar'
    buildV mem tvar =
      do stM <- getM ModeST
         case M.lookup tvar stM of
           Nothing -> error $ "there is no structural information about " ++ show (TVar tvar :: TypeE)
           Just t ->
             do tvar' <- getNextId
                let mem' = M.insert tvar tvar' mem
                s <- buildT mem' t
                unifyEQ (TVar tvar') s
                return (TVar tvar')

    buildT :: M.Map TypeV TypeV -> TypeE -> Generator TypeE
    buildT mem t | basic t = return t
    buildT mem (TVar tvar) = buildV mem tvar
    buildT mem (TChannel s _ _) =
      do u1 <- newUse
         u2 <- newUse
         return $ TChannel s u1 u2
    buildT mem (TProduct t1 t2) =
      do s1 <- buildT mem t1
         s2 <- buildT mem t2
         return $ TProduct s1 s2
    buildT mem (TVariant m _) =
      do bs <- mapM (\(tag, t) -> buildT mem t >>= \s -> return (tag, s)) (M.toList m)
         return $ TVariant (M.fromList bs) Nothing

generate :: Bool -> UntypedProcess -> (TypedProcess, NameEnvironment, Constraints)
generate mp p =
    let ((q, env), st) = ST.runState (doAll p) emptyState in
    let eqm = eqmap st in
    let stm = stmap st in
    let cot = comap st in
    let equ = equse st in
    let com = combinations eqm (comap st) in
    (q, env, (eqm, stm, cot, com, equ ∪ useConstraints com))
    where
      doAll :: UntypedProcess -> Generator (TypedProcess, NameEnvironment)
      doAll p = do (q, env) <- auxP p
                   addMissingDefinitions
                   return (q, env)

      auxP :: UntypedProcess -> Generator (TypedProcess, NameEnvironment)
      auxP Idle = return $ (Idle, M.empty)
      auxP (Receive se x tx p) =
          do (p', penv) <- auxP p
             (xt, penv') <- getType x tx penv
             (se', st, senv) <- auxE se
             env <- combine senv (M.delete x penv')
             u1 <- newUse
             u2 <- newUse
             unifyEQ st $ TChannel xt (Combination.singleton 1 ⊎ u1) (u2 ⊎ u2)
             return (Receive se' x xt p', env)
      auxP (Send se oe p) =
          do (p', penv) <- auxP p
             (se', st, senv) <- auxE se
             (oe', ot, oenv) <- auxE oe
             env <- combine senv oenv
             env <- combine env penv
             u1 <- newUse
             u2 <- newUse
             unifyEQ st $ TChannel ot (u1 ⊎ u1) (Combination.singleton 1 ⊎ u2)
             return (Send se' oe' p', env)
      auxP (Group p) =
          do (q, env) <- auxP p
             return (Group q, env)
      auxP (Par p q) =
          do (p', env1) <- auxP p
             (q', env2) <- auxP q
             env <- combine env1 env2
             return (Par p' q', env)
      auxP (Star p) =
          do (q, env) <- auxP p
             env' <- unlimitedE env
             return (Star q, env')
      auxP (New a ta p) =
          do (q, env) <- auxP p
             (at, env) <- getType a ta env
             let env' = M.delete a env
             t <- newType
             u1 <- newUse
             u2 <- if mp then newUse else return u1
             unifyEQ at $ TChannel t u1 u2
             return (New a at q, env')
      auxP (Let x tx e p) =
          do (e', t, env) <- auxE e
             (q, env') <- auxP p
             (xt, env') <- getType x tx env'
             env <- combine env (M.delete x env')
             unifyEQ t xt
             return (Let x xt e' q, env)
      auxP (Split e x tx y ty p) =
          do (e', t, env) <- auxE e
             (q, env') <- auxP p
             (xt, env') <- getType x tx env'
             (yt, env') <- getType y ty env'
             env <- combine env (M.delete x $ M.delete y env')
             unifyEQ t (TProduct xt yt)
             return (Split e' x xt y yt q, env)
      auxP (If e p q) =
          do (e', t, env) <- auxE e
             (p', env1) <- auxP p
             (q', env2) <- auxP q
             env' <- merge env1 env2
             env' <- combine env env'
             unifyEQ t tbool
             return (If e' p' q', env')
      auxP (Case e rules) =
          do (e', t, env) <- auxE e
             (rules', s, env') <- auxRules rules
             unifyEQ t s
             env' <- combine env env'
             return (Case e' rules', env')

      auxRules :: [UntypedRule] -> Generator ([TypedRule], TypeE, NameEnvironment)
      auxRules rs = do res <- mapM auxRule rs
                       let (rules, env : envs) = unzip res
                       env' <- foldM merge env envs
                       let m = M.fromList $ [ (tag, t) | (tag, _, t, _) <- rules ]
                       return (rules, TVariant m Nothing, env')

      auxRule :: UntypedRule -> Generator (TypedRule, NameEnvironment)
      auxRule (tag, x, tx, p) =
          do (q, env) <- auxP p
             (xt, env') <- getType x tx env
             return ((tag, x, xt, q), M.delete x env')

      auxUO :: UnOp -> TypeE -> Generator (UnOp, TypeE)
      auxUO NOT t =
          do unifyEQ t tbool
             return (NOT, tbool)
      auxUO FST t =
          do t1 <- newType
             t2 <- newUnlimitedType
             unifyEQ t $ TProduct t1 t2
             return (FST, t1)
      auxUO SND t =
          do t1 <- newUnlimitedType
             t2 <- newType
             unifyEQ t $ TProduct t1 t2
             return (SND, t2)

      auxBO :: BinOp -> TypeE -> TypeE -> Generator TypeE
      auxBO op t1 t2 | op `elem` [AND, OR] =
                         do unifyEQ t1 tbool
                            unifyEQ t2 tbool
                            return tbool
                     | op `elem` [Process.LT, LE, Process.GT, GE] =
                         do unifyEQ t1 tint
                            unifyEQ t2 tint
                            return tbool
                     | op `elem` [Process.EQ, NE] =
                         do unifyEQ t1 t2
                            unlimitedT t1
                            return tbool
                     | op `elem` [ADD, SUB, MUL, DIV, MOD] =
                         do unifyEQ t1 tint
                            unifyEQ t2 tint
                            return tint
                     | op `elem` [PAIR] =
                         return $ TProduct t1 t2

      auxC :: Const -> Generator TypeE
      auxC UNIT = return tunit
      auxC (INT _) = return tint
      auxC (BOOL _) = return tbool

      auxE :: UntypedExpression -> Generator (TypedExpression, TypeE, NameEnvironment)
      auxE (Id x Nothing) =
          do t <- newType
             return (Id x t, t, M.fromList [(x, t)])

      auxE (Id x (Just typ)) =
        do t <- instantiateType typ
           return (Id x t, t, M.singleton x t)
             
      auxE (Const c) =
          do t <- auxC c
             return (Const c, t, M.empty)

      auxE (UnOp op e) =
          do (e', t, env) <- auxE e
             (op', t') <- auxUO op t
             return (UnOp op' e', t', env)

      auxE (BinOp op e1 e2) =
          do (e1', t1, env1) <- auxE e1
             (e2', t2, env2) <- auxE e2
             t <- auxBO op t1 t2
             env <- combine env1 env2
             return (BinOp op e1' e2', t, env)

      auxE (Tag tag e) =
          do (e', t, env) <- auxE e
             vt <- makeOpenVariant tag t
             return (Tag tag e', vt, env)
