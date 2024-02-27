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

module DeadLock.Generator where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Control.Monad.State.Lazy as ST
import Control.Monad
import Data.Maybe (mapMaybe)

import Data.Ord.Unicode
import Data.Set.Unicode
import Data.Bool.Unicode

import Aux
import Process
import DeadLock.Polarity
import DeadLock.Type
import DeadLock.ValueExpression
import DeadLock.TypeExpression
import qualified Render

import Debug.Trace

type UntypedExpression = Expression TypeN
type UntypedProcess = Process TypeN
type UntypedRule = Rule TypeN

type TypedExpression = Expression TypeE
type TypedProcess = Process TypeE
type TypedRule = Rule TypeE

type NameEnvironment = M.Map Name TypeE

type Constraints = (M.Map TypeV TypeE,
                    M.Map TypeV TypeE,
                    S.Set (TypeE, TypeE, TypeE),
                    S.Set (LevelE, LevelE),
                    S.Set (LevelE, LevelE),
                    S.Set (TicketE, TicketE))

data GeneratorState = GeneratorState {
      eqrel  :: S.Set (TypeE, TypeE),        -- type equalities
      corel  :: S.Set (TypeE, TypeE, TypeE), -- type compositions
      stmap  :: M.Map TypeV TypeN,           -- type structure
      reqmap :: M.Map TypeV TypeE,
      rstmap :: M.Map TypeV TypeE,
      ltlevt :: S.Set (LevelE, TypeE),    -- level less-than
      eqlev  :: S.Set (LevelE, LevelE),   -- level equalities (expressions)
      ltlev  :: S.Set (LevelE, LevelE),   -- level disequalities (expressions)
      eqtic  :: S.Set (TicketE, TicketE), -- ticket equalities
      nextId :: Int                       -- next identifier for level/ticket variable
    }

emptyState :: GeneratorState
emptyState = GeneratorState { eqrel = S.empty,
                              corel = S.empty,
                              stmap  = M.empty,
                              rstmap = M.empty,
                              reqmap = M.empty,
                              ltlevt = S.empty,
                              eqlev  = S.empty,
                              ltlev  = S.empty,
                              eqtic  = S.empty,
                              nextId = 1
                            }

type Generator a = ST.State GeneratorState a

addEQT :: TypeE -> TypeE -> Generator ()
addEQT t1 t2 = ST.modify (\st -> st { eqrel = S.insert (t1, t2) (eqrel st) })

addEQL :: LevelE -> LevelE -> Generator ()
addEQL l1 l2 = ST.modify (\st -> st { eqlev = S.insert (l1, l2) (eqlev st) })

addLTLT :: LevelE -> TypeE -> Generator ()
addLTLT l t = ST.modify (\st -> st { ltlevt = S.insert (l, t) (ltlevt st) })

addLTL :: LevelE -> LevelE -> Generator ()
addLTL l1 l2 = ST.modify (\st -> st { ltlev = S.insert (l1, l2) (ltlev st) })

addLTLs :: LevelE -> S.Set LevelE -> Generator ()
addLTLs l lset = forM_ (S.elems lset) (addLTL l)

addEQTic :: TicketE -> TicketE -> Generator ()
addEQTic tic1 tic2 = ST.modify (\st -> st { eqtic = S.insert (tic1, tic2) (eqtic st) })

findG :: (GeneratorState -> M.Map TypeV TypeE) -> TypeE -> Generator TypeE
findG s t = do m <- ST.gets s
               return $ findD m t

setMapMaybe :: (Ord a, Ord b) => (a -> Maybe b) -> S.Set a -> S.Set b
setMapMaybe f = S.fromList . mapMaybe f . S.elems

memberModuloD :: S.Set (TypeE, TypeE) -> TypeE -> TypeE -> Maybe (LevelE, TicketE, LevelE, TicketE)
memberModuloD tset t1 t2 | S.null cset = Nothing
                         | otherwise = Just $ S.findMin cset
  where
    cset = setMapMaybe (unshiftPair (t1, t2)) tset

    unshiftPair (t1, t2) (s1, s2) =
      case (unshift t1 s1, unshift t2 s2) of
        (Just (dl1, dtic1), Just (dl2, dtic2)) -> Just (dl1, dtic1, dl2, dtic2)
        _ -> Nothing


unifyST :: Generator ()
unifyST = do eqset <- ST.gets eqrel
             forM_ (S.toList eqset) (uncurry $ auxT S.empty)
             coset <- ST.gets corel
             forM_ (S.toList coset) (\(t, t1, t2) -> auxT S.empty t t1 >> auxT S.empty t t2)
  where
    auxV :: TypeV -> TypeE -> Generator ()
    auxV tvar t = ST.modify (\st -> st { rstmap = M.insert tvar t (rstmap st) })

    auxT :: S.Set (TypeE, TypeE) -> TypeE -> TypeE -> Generator ()
    auxT tset t1 t2 | (t1, t2) ∈ tset = return ()
    auxT tset t1 t2 | Just (dl1, dtic1, dl2, dtic2) <- memberModuloD tset t1 t2 =
      do traceM $ "WARNING: NON REGULAR PATTERN DETECTED " ++ show t1 ++ "   " ++ show t2 ++ " at " ++ show (dl1, dtic1) ++ ", " ++ show (dl2, dtic2)
         addEQL dl1 valueNil
         addEQL dl2 valueNil
    auxT tset t1 t2 =
      do let tset' = S.insert (t1, t2) tset
         s1 <- findG rstmap t1
         s2 <- findG rstmap t2
         case (s1, s2) of
           (TEBasic bt1, TEBasic bt2) | bt1 == bt2 -> return ()
           (TEVar tvar1 dl _, TEVar tvar2 _ _) | tvar2 < tvar1 ->
             auxV tvar1 $ shift (valueNeg dl) valueNil s2
           (TEVar tvar1 _ _, TEVar tvar2 dl dtic) | tvar1 < tvar2 ->
             auxV tvar2 $ shift (valueNeg dl) valueNil s1
           (TEVar tvar dl1 _, TEVar _ dl2 _) ->
             do stm <- ST.gets stmap
                let Just t0 = M.lookup tvar stm
                when (not $ unlimited t0)
                  (addEQL dl1 dl2)
           (TEVar tvar dl _, _) -> auxV tvar $ shift (valueNeg dl) valueNil s2
           (_, TEVar tvar dl _) -> auxV tvar $ shift (valueNeg dl) valueNil s1
           (TEChannel _ t1' Unlimited, TEChannel _ t2' Unlimited) ->
             do addEQT t1' t2'
                auxT tset' t1' t2'
           (TEChannel _ t1' (Linear l1 _), TEChannel _ t2' (Linear l2 _)) ->
             do auxT tset' t1' t2'
                addEQT t1' t2'
                addEQL l1 l2
           (TEProduct t11 t12, TEProduct t21 t22) ->
             do auxT tset' t11 t21
                auxT tset' t12 t22
           (TEVariant m1 Nothing, TEVariant m2 Nothing) | M.keysSet m1 == M.keysSet m2 ->
             auxM tset' m1 m2
           (TEVariant m1 (Just tvar), TEVariant m2 Nothing) | M.keysSet m1 ⊆ M.keysSet m2 ->
             do auxV tvar s2
                auxM tset' m1 m2
           (TEVariant m1 Nothing, TEVariant m2 (Just tvar)) | M.keysSet m2 ⊆ M.keysSet m1 ->
             do auxV tvar s1
                auxM tset' m1 m2
           (TEVariant m1 (Just tvar1), TEVariant m2 (Just tvar2)) ->
             do newt <- makeOpenVariant $ M.union m1 m2
                auxV tvar1 newt
                auxV tvar2 newt
                auxM tset' m1 m2
           _ -> error $ "unifyST: " ++ show s1 ++ " ### " ++ show s2

    auxM :: S.Set (TypeE, TypeE) -> M.Map Tag TypeE -> M.Map Tag TypeE -> Generator ()
    auxM tset m1 m2 = forM_ (M.elems $ M.intersectionWith (,) m1 m2) (uncurry $ auxT tset)

unifyEQ :: TypeE -> TypeE -> Generator ()
unifyEQ = auxT S.empty
  where
    auxV :: TypeV -> TypeE -> Generator ()
    auxV tvar t = ST.modify (\st -> st { reqmap = M.insert tvar t (reqmap st) })

    auxT :: S.Set (TypeE, TypeE) -> TypeE -> TypeE -> Generator ()
    auxT tset t1 t2 | (t1, t2) ∈ tset = return ()
    auxT tset t1 t2 | Just (dl1, dtic1, dl2, dtic2) <- memberModuloD tset t1 t2 =
      do traceM $ "WARNING: NON REGULAR PATTERN DETECTED " ++ show t1 ++ "   " ++ show t2 ++ " at " ++ show (dl1, dtic1) ++ ", " ++ show (dl2, dtic2)
         addEQTic dtic1 valueNil
         addEQTic dtic2 valueNil
    auxT tset t1 t2 =
      do let tset' = S.insert (t1, t2) tset
         s1 <- findG reqmap t1
         s2 <- findG reqmap t2
         case (s1, s2) of
           (TEBasic bt1, TEBasic bt2) | bt1 == bt2 -> return ()
           (TEVar tvar1 dl dtic, TEVar tvar2 _ _) | tvar2 < tvar1 ->
             auxV tvar1 $ shift (valueNeg dl) (valueNeg dtic) s2
           (TEVar tvar1 _ _, TEVar tvar2 dl dtic) | tvar1 < tvar2 ->
             auxV tvar2 $ shift (valueNeg dl) (valueNeg dtic) s1
           (TEVar tvar dl1 dtic1, TEVar _ dl2 dtic2) ->
             do stm <- ST.gets stmap
                let Just t0 = M.lookup tvar stm
                when (not $ unlimited t0)
                  (do addEQTic dtic1 dtic2
                      addEQL dl1 dl2)
           (TEVar tvar dl dtic, _) -> auxV tvar $ shift (valueNeg dl) (valueNeg dtic) s2
           (_, TEVar tvar dl dtic) -> auxV tvar $ shift (valueNeg dl) (valueNeg dtic) s1
           (TEProduct t11 t12, TEProduct t21 t22) ->
             do auxT tset' t11 t21
                auxT tset' t12 t22
           (TEVariant m1 Nothing, TEVariant m2 Nothing) | M.keysSet m1 == M.keysSet m2 ->
             auxM tset' m1 m2
           (TEVariant m1 (Just tvar), TEVariant m2 Nothing) | M.keysSet m1 ⊆ M.keysSet m2 ->
             do auxV tvar s2
                auxM tset' m1 m2
           (TEVariant m1 Nothing, TEVariant m2 (Just tvar)) | M.keysSet m2 ⊆ M.keysSet m1 ->
             do auxV tvar s1
                auxM tset' m1 m2
           (TEVariant m1 (Just tvar1), TEVariant m2 (Just tvar2)) ->
             do newt <- makeOpenVariant $ M.union m1 m2
                auxV tvar1 newt
                auxV tvar2 newt
                auxM tset' m1 m2
           (TEChannel pol1 t1' Unlimited, TEChannel pol2 t2' Unlimited) | pol1 == pol2 ->
             auxT tset' t1' t2'
           (TEChannel pol1 t1' (Linear _ tic1), TEChannel pol2 t2' (Linear _ tic2)) | pol1 == pol2 ->
             do auxT tset' t1' t2'
                addEQTic tic1 tic2
           _ -> error $ "unifyEQ: " ++ show s1 ++ " ### " ++ show s2

    auxM tset m1 m2 = forM_ (M.elems $ M.intersectionWith (,) m1 m2) (uncurry $ auxT tset)
  
unifyAll :: Generator ()
unifyAll = do eqset <- ST.gets eqrel
              forM_ (S.toList eqset) (uncurry unifyEQ)

getNextId :: Generator Int
getNextId = do old <- ST.get
               ST.modify (\st -> st { nextId = nextId st + 1 })
               return $ nextId old

newTypeVar :: TypeN -> Generator TypeV
newTypeVar t0 = do tvar <- getNextId
                   ST.modify (\st -> st { stmap = M.insert tvar t0 (stmap st) })
                   return tvar

mapSetM :: (Monad m, Ord a, Ord b) => (a -> m b) -> S.Set a -> m (S.Set b)
mapSetM f set = mapM f (S.toList set) >>= (return . S.fromList)

mapMapM :: (Monad m, Ord k) => (a -> m b) -> M.Map k a -> m (M.Map k b)
mapMapM f map = mapM (\(k, v) -> f v >>= \v' -> return (k, v')) (M.toList map) >>= (return . M.fromList)

stripG :: TypeE -> Generator TypeN
stripG = auxT
  where
    auxT :: TypeE -> Generator TypeN
    auxT (TEBasic bt) = return $ TBasic bt
    auxT (TEVar tvar _ _) =
      do stm <- ST.gets stmap
         let Just t = M.lookup tvar stm
         return t
    auxT (TEChannel pol t kind) =
      do t' <- auxT t
         return $ TChannel pol t' (auxK kind)
    auxT (TEProduct t1 t2) =
      do t1' <- auxT t1
         t2' <- auxT t2
         return $ TProduct t1' t2'
    auxT (TEVariant m _) =
      do m' <- mapMapM auxT m
         return $ TVariant m'

    auxK Unlimited = Unlimited
    auxK (Linear _ _) = Linear () ()
    
newType :: TypeN -> Generator TypeE
newType (TBasic bt) = return $ TEBasic bt
newType t0 = do tvar <- newTypeVar t0
                return $ TEVar tvar valueNil valueNil

newLevel :: Generator LevelE
newLevel = liftM (valueV . LVar) getNextId

newTicket :: Generator TicketE
newTicket = liftM (valueV . TiVar . negate) getNextId

splitE :: NameEnvironment -> Name -> TypeN -> Generator (TypeE, NameEnvironment)
splitE env x _ | Just t <- M.lookup x env = return (t, M.delete x env)
splitE env x t0 = do t <- newType t0
                     return (t, env)

combineT :: TypeE -> TypeE -> Generator TypeE
combineT t1 t2 = do t1' <- stripG t1
                    t2' <- stripG t2
                    t <- newType $ combine t1' t2'
                    ST.modify (\st -> st { corel = S.insert (t, t1, t2) (corel st) })
                    return t
    
combineE :: NameEnvironment -> NameEnvironment -> Generator NameEnvironment
combineE env1 env2 = mapM aux (S.toList $ M.keysSet env1 ∪ M.keysSet env2) >>= (return . M.fromList)
    where
      aux :: Name -> Generator (Name, TypeE)
      aux x = case (M.lookup x env1, M.lookup x env2) of
                (Just t, Nothing) -> return (x, t)
                (Nothing, Just t) -> return (x, t)
                (Just t1, Just t2) -> do t <- combineT t1 t2
                                         return (x, t)
                _ -> error "combineE: impossible"

mergeE :: NameEnvironment -> NameEnvironment -> Generator NameEnvironment
mergeE env1 env2 = mapM aux (S.toList $ (M.keysSet env1 ∪ M.keysSet env2)) >>= (return . M.fromList)
    where
      aux :: Name -> Generator (Name, TypeE)
      aux x = case (M.lookup x env1, M.lookup x env2) of
                (Just t, Nothing) ->
                  do unlimitedT t
                     return (x, t)
                (Nothing, Just t) ->
                  do unlimitedT t
                     return (x, t)
                (Just t, Just s) ->
                  do addEQT t s
                     return (x, t)
                _ -> error "mergeE: impossible"

checkLevelT :: NakedLevel -> TypeE -> Generator ()
checkLevelT nl t = do t0 <- stripG t
                      if nl ≤ nakedLevel t0 
                        then return ()
                        else error $ "the type " ++ show t ++ " does not have naked level ≥ " ++ show nl
  
checkLevelE :: NakedLevel -> NameEnvironment -> Generator ()
checkLevelE nl env = forM_ (M.elems env) (checkLevelT nl)

unlimitedT :: TypeE -> Generator ()
unlimitedT = checkLevelT NakedTop

unlimitedE :: NameEnvironment -> Generator ()
unlimitedE = checkLevelE NakedTop

levelsT :: TypeE -> Generator (S.Set LevelE)
levelsT t = do t0 <- stripG t
               aux S.empty t0 t
  where
    aux :: S.Set TypeE -> TypeN -> TypeE -> Generator (S.Set LevelE)
    aux _ t0 _ | unlimited t0 = return S.empty
    aux tset _ t | t ∈ tset = return S.empty
    aux tset t0 t =
      do let tset' = S.insert t tset
         s <- findG rstmap t
         case (unfold t0, s) of
           (TBasic bt0, TEBasic bt1) | bt0 == bt1 -> return S.empty
           (TChannel pol _ Unlimited, TEChannel _ _ Unlimited) | not (CInput ∈ pol) -> return S.empty
           (TChannel pol _ _, TEChannel _ _ _) | S.null pol -> return S.empty
           (TChannel _ _ (Linear () ()), TEChannel _ _ (Linear l _)) -> return $ S.singleton l
           (TProduct t01 t02, TEProduct t1 t2) ->
             do lset1 <- aux tset' t01 t1
                lset2 <- aux tset' t02 t2
                return $ lset1 ∪ lset2
           (TVariant m0, TEVariant m _) ->
             do lm <- mapMapM (uncurry $ aux tset') $ M.intersectionWith (,) m0 m
                return $ S.unions $ M.elems lm

computeLevelConstraints :: Generator ()
computeLevelConstraints =
  do ltltrel <- ST.gets ltlevt
     forM_ (S.elems ltltrel) (\(l, t) -> (levelsT >=> addLTLs l) t)

makeOpenVariant :: M.Map Tag TypeE -> Generator TypeE
makeOpenVariant m = liftM (TEVariant m . Just) getNextId

undefinedTypes :: Generator (S.Set TypeV)
undefinedTypes = do state <- ST.get
                    return $ M.keysSet (rstmap state) ∖ M.keysSet (reqmap state)

addMissingDefinitions :: Generator ()
addMissingDefinitions =
  do undefinedTypes >>= (mapM_ aux . S.toList)
  where
    aux :: TypeV -> Generator ()
    aux tvar =
      do t <- buildV tvar
         unifyEQ (TEVar tvar valueNil valueNil) t

    buildV :: TypeV -> Generator TypeE
    buildV tvar = findG rstmap (TEVar tvar valueNil valueNil) >>= buildT

    buildT :: TypeE -> Generator TypeE
    buildT t | basic t = return t
    buildT t@(TEChannel _ _ Unlimited) = return t
    buildT (TEChannel pol t (Linear l _)) =
      do tic <- newTicket
         return $ TEChannel pol t (Linear l tic)
    buildT t = error $ "buildT: don't know what to do with " ++ show t

computeTicketConstraints :: Generator ()
computeTicketConstraints = do cor <- ST.gets corel
                              forM_ (S.elems cor) aux
  where
    aux :: (TypeE, TypeE, TypeE) -> Generator ()
    aux (t, t1, t2) =
      do s <- findG reqmap t
         s1 <- findG reqmap t1
         s2 <- findG reqmap t2
         case (s, s1, s2) of
           (TEChannel _ _ (Linear _ tic),
            TEChannel _ _ (Linear _ tic1),
            TEChannel _ _ (Linear _ tic2)) -> addEQTic tic (tic1 @+@ tic2)
           _ -> return ()
        
generate :: Bool -> UntypedProcess -> (TypedProcess, NameEnvironment, Constraints)
generate lock p =
    let ((q, env), st) = ST.runState (auxA p) emptyState in
    let eqm = reqmap st in
    let stm = rstmap st in
    let cor = corel st in
    let eql = eqlev st in
    let ltl = ltlev st in
    let etic = eqtic st in
    (q, env, (eqm, stm, cor, eql, ltl, etic))
    where
      cost = valueC $ if lock then 1 else 0
      
      auxA :: UntypedProcess -> Generator (TypedProcess, NameEnvironment)
      auxA p = do (q, env) <- auxP p
                  unifyST
                  unifyAll
                  when lock addMissingDefinitions
                  computeLevelConstraints
                  when lock computeTicketConstraints
                  return (q, env)
        
      auxP :: UntypedProcess -> Generator (TypedProcess, NameEnvironment)
      auxP Idle = return $ (Idle, M.empty)
      auxP (Receive e x s0 p) =
        do (e', t, env1) <- auxE e
           (p', penv) <- auxP p
           (s, env2) <- splitE penv x s0
           checkLevelE NakedLinear penv
           env <- combineE env1 env2
           t0 <- stripG t
           case unfold t0 of
             TChannel pol t0' (Linear () ()) | pol == S.singleton CInput ->
               do t' <- newType t0'
                  l <- newLevel
                  tic <- newTicket
                  addEQT t (TEChannel pol t' (Linear l tic))
                  addEQT s (shift l valueNil t')
                  forM_ (M.elems penv) (addLTLT l)
                  return (Receive e' x s p', env)
             _ -> error $ "generate: ill-typed input"
      auxP (Star (Receive e x s0 p)) =
        do (e', t, env1) <- auxE e
           (p', penv) <- auxP p
           (s, env2) <- splitE penv x s0
           unlimitedE env2
           env <- combineE env1 env2
           t0 <- stripG t
           case unfold t0 of
             TChannel pol _ Unlimited | CInput ∈ pol ->
               do addEQT t (TEChannel pol s Unlimited)
                  return (Star $ Receive e' x s p', env)
             _ -> error "generate: ill-typed replicated input"
      auxP (Star _) = error "generate: only inputs can be replicated"
      auxP (Send e f p) =
        do (e', t, env1) <- auxE e
           (f', s, env2) <- auxE f
           (p', env3) <- auxP p
           checkLevelE NakedLinear env2
           checkLevelE NakedLinear env3
           env' <- combineE env1 env2
           env <- combineE env' env3
           t0 <- stripG t
           case unfold t0 of
             TChannel pol t0' (Linear () ()) | pol == S.singleton COutput ->
               do t' <- newType t0'
                  l <- newLevel
                  tic <- newTicket
                  addEQT t (TEChannel pol t' (Linear l tic))
                  addEQT s (shift l cost t')
                  forM_ (M.elems env2) (addLTLT l)
                  forM_ (M.elems env3) (addLTLT l)
                  return (Send e' f' p', env)
             TChannel pol t0' Unlimited | pol == S.singleton COutput ->
               do t' <- newType t0'
                  l <- newLevel
                  addEQT t (TEChannel pol t' Unlimited)
                  addEQT s (shift l cost t')
                  return (Send e' f' p', env)
             _ -> error "generate: ill-typed output"
      auxP (Group p) =
        do (q, env) <- auxP p
           return (Group q, env)
      auxP (Par p q) =
        do (p', env1) <- auxP p
           (q', env2) <- auxP q
           env <- combineE env1 env2
           return (Par p' q', env)
      auxP (New a t0 p) =
        do (q, penv) <- auxP p
           (t, env) <- splitE penv a t0
           return (New a t q, env)
      auxP (Let x t0 e p) =
        do (e', s, env1) <- auxE e
           (p', penv) <- auxP p
           (t, env2) <- splitE penv x t0
           addEQT t s
           env <- combineE env1 env2
           return (Let x t e' p', env)
      auxP (Split e x xt0 y yt0 p) =
        do (e', t, env1) <- auxE e
           (p', penv) <- auxP p
           (t1, env') <- splitE penv x xt0
           (t2, env2) <- splitE env' y yt0
           env <- combineE env1 env2
           addEQT t (TEProduct t1 t2)
           return (Split e' x t1 y t2 p', env)
      auxP (If e p q) =
        do (e', t, env1) <- auxE e
           (p', penv) <- auxP p
           (q', qenv) <- auxP q
           env2 <- mergeE penv qenv
           env <- combineE env1 env2
           addEQT t tbool
           return (If e' p' q', env)
      auxP (Case e rules) =
        do (e', t, env1) <- auxE e
           (rules', s, env2) <- auxRules rules
           env <- combineE env1 env2
           addEQT t s
           return (Case e' rules', env)

      auxRules :: [UntypedRule] -> Generator ([TypedRule], TypeE, NameEnvironment)
      auxRules rs =
        do res <- mapM auxRule rs
           let (rules, env : envs) = unzip res
           env' <- foldM mergeE env envs
           let m = M.fromList [ (tag, t) | (tag, _, t, _) <- rules ]
           return (rules, TEVariant m Nothing, env')

      auxRule :: UntypedRule -> Generator (TypedRule, NameEnvironment)
      auxRule (tag, x, t0, p) =
        do (p', penv) <- auxP p
           (t, env) <- splitE penv x t0
           return ((tag, x, t, p'), env)

      auxUO :: UnOp -> TypeE -> Generator (UnOp, TypeE)
      auxUO NOT t =
        do addEQT t tbool
           return (NOT, tbool)
      auxUO FST t =
        do t0 <- stripG t
           case unfold t0 of
             TProduct t10 t20 | unlimited t20 ->
               do t1 <- newType t10
                  t2 <- newType t20
                  addEQT t (TEProduct t1 t2)
                  return (FST, t1)
             _ -> error "auxUO: type error"
      auxUO SND t =
        do t0 <- stripG t
           case unfold t0 of
             TProduct t10 t20 | unlimited t10 ->
               do t1 <- newType t10
                  t2 <- newType t20
                  addEQT t (TEProduct t1 t2)
                  return (SND, t2)

      auxBO :: BinOp -> TypeE -> TypeE -> Generator TypeE
      auxBO op t1 t2 =
        case op of
          _ | op `elem` [AND, OR] ->
            do addEQT t1 tbool
               addEQT t2 tbool
               return tbool
          _ | op `elem` [Process.LT, LE, Process.GT, GE] ->
            do addEQT t1 tint
               addEQT t2 tint
               return tbool
          _ | op `elem` [Process.EQ, NE] ->
            do addEQT t1 t2
               return tbool
          _ | op `elem` [ADD, SUB, MUL, DIV, MOD] ->
            do addEQT t1 tint
               addEQT t2 tint
               return tint
          _ | op `elem` [PAIR] ->
            return $ TEProduct t1 t2

      auxC :: Const -> Generator TypeE
      auxC UNIT = return tunit
      auxC (INT _) = return tint
      auxC (BOOL _) = return tbool

      auxE :: UntypedExpression -> Generator (TypedExpression, TypeE, NameEnvironment)
      auxE (Const c) =
        do t <- auxC c
           return (Const c, t, M.empty)
      auxE (Id x t0) =
        do t <- newType t0
           return (Id x t, t, M.fromList [(x, t)])
      auxE (UnOp op e) =
        do (e', t, env) <- auxE e
           (op', s) <- auxUO op t
           return (UnOp op' e', s, env)
      auxE (BinOp op e1 e2) =
          do (e1', t1, env1) <- auxE e1
             (e2', t2, env2) <- auxE e2
             t <- auxBO op t1 t2
             env <- combineE env1 env2
             return (BinOp op e1' e2', t, env)
      auxE (Tag tag e) = 
          do (e', t, env) <- auxE e
             vt <- makeOpenVariant $ M.singleton tag t
             return (Tag tag e', vt, env)
