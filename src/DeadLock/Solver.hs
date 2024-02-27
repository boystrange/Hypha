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

module DeadLock.Solver where

import Data.Tuple (swap)
import Data.Either
import Data.Maybe
import qualified Data.Char
import qualified Data.Algebra as A
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.MultiSet as MS

import Control.Monad
import System.Process (callCommand)    -- to execute shell commands
import System.IO                       -- to perform IO on files
import Text.ParserCombinators.Parsec   -- to parse solvers output
import Control.Exception.Base          -- to handle exceptions

import qualified Data.LinearProgram.Common as LP
import qualified Data.LinearProgram.GLPK.Solver as GLPK
import Data.LinearProgram.GLPK.IO

import Data.Set.Unicode
import Data.Ord.Unicode

import DeadLock.ValueExpression
import DeadLock.TypeExpression


{-|
Supported solvers.
-}
data Solver = GLPK
            | GLPSol
            | LPSolve
            | CPlex
            deriving(Eq, Ord, Show)

{-|
Kind of analysis supported by solvers.
-}
data Analysis = DeadlockFreedom
              | LockFreedom
              deriving(Eq, Ord, Show)


{-|
Directory in which to create temporary files.
-}
tmpDir :: FilePath
tmpDir = "/tmp"

{-|
Compute an integer solution to the given set of contrainsts using the specified solver.
-}
solve :: (Ord var, Show var, Read var, Integral n, Show n, Read n) =>
         Solver -> Analysis -> Bool -> S.Set (ValueE n var, ValueE n var) -> S.Set (ValueE n var, ValueE n var) -> IO (Maybe (M.Map var n))
solve solver analysis verbose = solve' solver analysis verbose round

{-|
Compute a solution to the given set of constraints using the specified solver.
The solution is normalized with the given 'convert' function.
-}
solve' :: (Ord var, Show var, Read var, Real n, Show n, Read n) =>
          Solver -> Analysis -> Bool -> (Double -> n) -> S.Set (ValueE n var, ValueE n var) -> S.Set (ValueE n var, ValueE n var) -> IO (Maybe (M.Map var n))
solve' solver analysis verbose convert eq lt =
  case (feq, flt) of
    (Nothing, _) -> return Nothing
    (_, Nothing) -> return Nothing
    (Just leq, Just llt) ->
      let eq = S.fromList leq
          lt = S.fromList llt
      in if S.null $ getRelationsVars (eq ∪ lt)
         then return $ Just M.empty
         else case solver of
           GLPK -> do solution <- solveWithGLPK analysis verbose eq lt
                      return $ convertSolution solution convert
           GLPSol -> do solution <- solveWithGLPSol analysis verbose eq lt
                        return $ convertSolution solution convert
           LPSolve -> do solution <- solveWithLPSolve analysis verbose eq lt
                         return $ convertSolution solution convert
           _ -> error "Not supported solver"
  where
    feq = filterEqRelations $ S.toList eq
    flt = filterLtRelations $ S.toList lt
    --feq = Just $ S.toList eq
    --flt = Just $ S.toList lt

    filterEqRelations :: (Ord var, Real n) =>
                        [(ValueE n var, ValueE n var)] -> Maybe [(ValueE n var, ValueE n var)]
    filterEqRelations [] =  Just []
    filterEqRelations ((e1,e2):xs)
      | e1 == e2 = filterEqRelations xs
      | otherwise =
        let (ValueE n1 pos1 neg1, ValueE n2 pos2 neg2) = (e1, e2)
        in if pos1 == pos2 && neg1 == neg2
           then Nothing
           else
             let rs = filterEqRelations xs
             in case rs of
               Nothing -> Nothing
               Just ls -> Just $ (e1,e2):ls


    filterLtRelations :: (Ord var, Real n) =>
                        [(ValueE n var, ValueE n var)] -> Maybe [(ValueE n var, ValueE n var)]
    filterLtRelations [] =  Just []
    filterLtRelations ((e1,e2):xs)
      | e1 == e2 = Nothing
      | otherwise =
        let (ValueE n1 pos1 neg1, ValueE n2 pos2 neg2) = (e1, e2)
        in if pos1 == pos2 && neg1 == neg2 && n1 >= n2
           then Nothing
           else
             let rs = filterLtRelations xs
             in case rs of
               Nothing -> Nothing
               Just ls -> Just $ (e1,e2):ls

    convertSolution :: (Ord v, Real n) => Maybe (M.Map v Double) -> (Double -> n) -> Maybe (M.Map v n)
    convertSolution solution convert =
      case solution of
        Nothing  -> Nothing
        Just sol -> Just $ M.map convert sol

--
-- Common functions
--

{-|
Return the list of variables that appears in a relation
between two value expressions.
-}
getRelationVars :: (Ord var) => (ValueE n var, ValueE n var) -> S.Set var
getRelationVars = \(l1, l2) -> S.union (varsE l1) (varsE l2)

{-|
Return the set of vars that appear in a set of relations
bewtween value expressions.
-}
getRelationsVars :: (Ord var) => S.Set (ValueE n var, ValueE n var) -> S.Set var
getRelationsVars s = S.foldl (\x -> \y -> S.union x (getRelationVars y)) S.empty s

{-|
Show a ValueExpression in a format readable from solvers.
-}
showValueExpression :: (Ord var, Show var, Real n, Show n) => ValueE n var -> String
showValueExpression (ValueE n pos neg) | MS.null pos && MS.null neg = show n
showValueExpression (ValueE 0 pos neg) | MS.null neg = showVarsMS "+" pos
showValueExpression (ValueE 0 pos neg) | MS.null pos = "-" ++ showVarsMS "-" neg
showValueExpression (ValueE 0 pos neg) = showVarsMS "+" pos ++ " - " ++ showVarsMS "-" neg
showValueExpression (ValueE n pos neg) | MS.null neg = show n ++ " + " ++ showVarsMS "+" pos
showValueExpression (ValueE n pos neg) = show n ++ " + " ++ showVarsMS "+" pos ++ " - " ++ showVarsMS "-" neg

showVarsMS :: Show lvar => String -> MS.MultiSet lvar -> String
showVarsMS op = L.intercalate (" " ++ op ++ " ") . L.map showElement . MS.toOccurList
  where
    showElement (x, 1) = show x
    showElement (x, n) = show n ++ " " ++ show x

{-|
Write a linear program to a file using the 'CPlex' LP format.
-}
writeCPlexProblem :: (Ord var, Show var, Real n, Show n) =>
                     FilePath -> Analysis -> S.Set (ValueE n var, ValueE n var) -> S.Set (ValueE n var, ValueE n var) -> IO()
writeCPlexProblem file analysis eq lt = do
  let lpStr =
        let varSet = S.toList $ getRelationsVars (eq ∪ lt)
        in
         "Minimize\n" ++                      -- our objective is to minimize the solution (we do not define an objective function)
         " 0 " ++ show (head varSet) ++ "\n" ++
         "Subject To\n" ++
         showConstraints eq "=" 0 ++
         showConstraints lt "<" (-1) ++
         case analysis of
             DeadlockFreedom ->                 -- in case of DeadlockFreedom
               "Bounds\n" ++                    -- the variables must be 'free'
               (concat $ map (\v -> " " ++ show v ++ " free\n") varSet) ++
               "End\n"
             LockFreedom ->                     -- in case of LockFreedom
               "General\n" ++                   -- the variables must be integer (NB: 0 <= var <= +inf is the default bound for CPlex)
               (concat $ map (\v -> " " ++ show v ++ "\n") varSet) ++
               "End\n"
  writeFile file lpStr
  where
    showConstraints :: (Ord var, Show var, Real n, Show n) => S.Set (ValueE n var, ValueE n var) -> String -> n -> String
    showConstraints constr rel c
      | S.null constr = ""
      | otherwise = concat $ map (\x -> aux x rel c) (S.toList constr)
        where
          aux :: (Ord var, Show var, Real n, Show n) => (ValueE n var, ValueE n var) -> String -> n -> String
          aux (e1, e2) rel c = let (ValueE n pos neg) = (e1 @-@ e2)
                         in " " ++ showValueExpression (ValueE 0 pos neg) ++ " " ++ rel ++ " " ++ show (c - n) ++ "\n"

--
-- Constraints resolution with the 'GLPK' haskell binder
--

coefficients :: (Ord var, Real n) => MS.MultiSet var -> MS.MultiSet var -> A.LinFunc var n
coefficients pos neg = A.linCombination $ map swap (M.toList m)
  where
    m = M.mergeWithKey (\_ n1 n2 -> Just $ fromIntegral $ n1 - n2)
                       (M.map fromIntegral)
                       (M.map (negate . fromIntegral))
                       (MS.toMap pos)
                       (MS.toMap neg)

constraintEQL :: (Ord var, Real n) => ValueE n var -> ValueE n var -> LP.Constraint var n
constraintEQL l1 l2 = LP.Constr Nothing (coefficients pos neg) (LP.Equ $ negate n)
  where
    ValueE n pos neg = l1 @-@ l2

constraintLTL :: (Ord var, Real n) => ValueE n var -> ValueE n var -> LP.Constraint var n
constraintLTL l1 l2 = LP.Constr Nothing (coefficients pos neg) (LP.UBound $ negate n - 1)
  where
    ValueE n pos neg = l1 @-@ l2


{-|
Solve the constraints using the haskell binder for the 'GLPK' solver.
-}
solveWithGLPK :: (Show var, Ord var, Real n) =>
                    Analysis -> Bool -> S.Set (ValueE n var, ValueE n var) -> S.Set (ValueE n var, ValueE n var) -> IO (Maybe (M.Map var Double))
solveWithGLPK analysis verbose eq lt =
  do (_, solution) <- GLPK.glpSolveVars options lp
     case solution of
       Nothing -> return Nothing
       Just (_, m) -> return (Just m)

  where
    options :: GLPK.GLPOpts      -- the GLPKLib runtime options
    options = if verbose then
                 GLPK.mipDefaults {
                   GLPK.msgLev = GLPK.MsgAll
                   -- GLPK.presolve = False -- disabling preprocessing to avoid loops
                   -- GLPK.tmLim = 60 -- solutions must be found in max 60 sec
                 }
              else
                GLPK.mipDefaults {
                  GLPK.msgLev = GLPK.MsgOff
                }

    lp = LP.LP { LP.direction = LP.Min,
                 LP.objective = objective,
                 LP.constraints = constraints,
                 LP.varBounds = varBounds,
                 LP.varTypes = varTypes }

    objective = LP.linCombination []

    constraints = map (uncurry constraintEQL) (S.elems eq) ++ map (uncurry constraintLTL) (S.elems lt)

    varBounds = M.fromList $ map (\var -> (var, LP.LBound 0)) nonNegativeVars

    varKind = case analysis of
      LockFreedom -> LP.IntVar
      DeadlockFreedom -> LP.ContVar

    varTypes = M.fromList $ map (\var -> (var, varKind)) allVars

    nonNegativeVars = case analysis of
      LockFreedom -> allVars
      DeadlockFreedom -> []

    allVars = S.toList $ getRelationsVars (eq ∪ lt)

--
-- Constraints resolution with the 'GLPK' cli tool
--

{-|
Solve the constraints using the 'glpsol' solver.
-}
solveWithGLPSol :: (Ord var, Show var, Read var, Real n, Show n, Read n) =>
                   Analysis -> Bool -> S.Set (ValueE n var, ValueE n var) -> S.Set (ValueE n var, ValueE n var) -> IO (Maybe (M.Map var Double))
solveWithGLPSol analysis verbose eq lt =
  do (constrFileName, constrFile) <- openTempFile tmpDir "glpsol_constr.lp"
     (solutionFileName, solutionFile) <- openTempFile tmpDir "glpsol_solution.lp"
     hClose constrFile
     hClose solutionFile

     when verbose
       (do putStrLn $ "==== Solver GLPSOL ===="
           putStrLn $ "Problem file: " ++ constrFileName
           putStrLn $ "Solution file: " ++ solutionFileName
           putStrLn $ "======================="
       )

     writeCPlexProblem constrFileName analysis eq lt

     callCommand $ "glpsol" ++ " --lp " ++ constrFileName ++ " -w " ++ solutionFileName ++ if verbose then "" else " > /dev/null 2>&1"

     parseGLPSolOutput solutionFileName $ S.toList $ getRelationsVars (eq ∪ lt)


{-|
Parse the 'glpsol' output and return an assignment for the given variables.
-}
parseGLPSolOutput :: (Read var, Ord var) => String -> [var] -> IO (Maybe (M.Map var Double))
parseGLPSolOutput name vars =

  do s <- readFile name
     let ls = lines s
         ctrlChr = head (ls !! 1)
       in case ctrlChr of
       '2' -> return $ Just $ M.fromList $ zip vars $ map readSimplexSolution $ reverse $ take (length vars) $ reverse $ lines s -- simplex solution
       '5' -> return $ Just $ M.fromList $ zip vars $ map readImmediateSolution $ reverse $ take (length vars) $ reverse $ lines s -- int solver solution
       _ -> return Nothing
  where
    readSimplexSolution :: String -> Double
    readSimplexSolution solStr =
      let ns = words solStr
      in
       if (head ns) == "4" then 0::Double --when the variable is unbounded the solution string match "4 sol x" where sol is the variable solution
       else read (ns !! 1)

    readImmediateSolution :: String -> Double
    readImmediateSolution = read

--
-- Constraints resolution with 'lp_solve'
--

{-|
Solve the constraints using the 'lp_solve' solver.
-}
solveWithLPSolve :: (Ord var, Show var, Read var, Real n, Show n, Read n) =>
                    Analysis -> Bool -> S.Set (ValueE n var, ValueE n var) -> S.Set (ValueE n var, ValueE n var) -> IO (Maybe (M.Map var Double))
solveWithLPSolve analysis verbose eq lt =
    do (constrFileName, constrFile) <- openTempFile tmpDir "lp_solve_constr.lp"
       (solutionFileName, solutionFile) <- openTempFile tmpDir "lp_solve_solution.lp"
       hClose constrFile
       hClose solutionFile

       when verbose
         (do putStrLn $ "==== Solver LPSolve ===="
             putStrLn $ "Problem file: " ++ constrFileName
             putStrLn $ "Solution file: " ++ solutionFileName
             putStrLn $ "========================"
         )

       writeLPSolveProblem constrFileName analysis eq (forceLtConstraints lt)

       catch
         (do callCommand $ "lp_solve" ++ " " ++ constrFileName ++ " > " ++ solutionFileName
             parseLPSolveOutput solutionFileName
         )
         (\e -> do let _ = e :: IOException
                   return Nothing
         )
  where
    -- force the lt relation to be '<'
    forceLtConstraints :: (Ord var, Real n) => S.Set (ValueE n var, ValueE n var) ->  S.Set (ValueE n var, ValueE n var)
    forceLtConstraints = S.map (\(e1, e2) -> (e1 @+@ (valueC 1), e2))


{-|
Write a linear program to a file using the 'lp_solve' LP format.
-}
writeLPSolveProblem :: (Ord var, Show var, Real n, Show n) =>
                       FilePath -> Analysis -> S.Set (ValueE n var, ValueE n var) -> S.Set (ValueE n var, ValueE n var) -> IO()
writeLPSolveProblem file analysis eq lt = do
  let lpStr =
        "min: ;\n" ++                           -- our objective is to minimize the solution (we do not define an objective function)
        showConstraints (S.toList eq) " = " ++
        showConstraints (S.toList lt) " < " ++
        let varSet = S.toList $ getRelationsVars (eq ∪ lt)
        in
         case analysis of
             DeadlockFreedom ->                 -- in case of DeadlockFreedom
               showVarTypes varSet "free"       -- all the variables should be 'free'
             LockFreedom ->                     -- in case of LockFreedom the variables should be
               (showConstraints (L.map (\var -> (valueV var, valueC 0)) varSet) " > ") ++  -- greater than 0
               (showVarTypes varSet "int")                                                 -- and 'int'
  writeFile file lpStr
  where
    showConstraints :: (Ord var, Show var, Real n, Show n) => [(ValueE n var, ValueE n var)] -> String -> String
    showConstraints constr operator
      | L.null constr = ""
      | otherwise =  concat $ map (\(e1, e2) -> showValueExpression e1 ++ operator ++ showValueExpression e2 ++ ";\n") constr

    showVarTypes :: (Ord var, Show var) => [var] -> String -> String
    showVarTypes vars t = concat $ map (\v -> t ++ " " ++ show v ++ ";\n") vars


{-|
Parse the 'lp_solve' output and returns a map of variables assignments.
-}
parseLPSolveOutput :: (Read var, Ord var, Read n) => String -> IO (Maybe (M.Map var n))
parseLPSolveOutput name =
  do s <- readFile name
     if ("This problem is infeasible" <= s)
       then return Nothing
       else return $ Just $ M.fromList $ map parseLine $ tail $ tail $ tail $ tail $ lines s
  where
    parseLine :: (Read var, Read n) => String -> (var, n)
    parseLine s = (read f1, read f2)
      where
        (f1, f2) = break Data.Char.isSpace s
