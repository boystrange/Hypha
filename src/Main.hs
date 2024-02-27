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

module Main (main) where

import Aux
import Process
import Render
import Linearity.Use
import Linearity.Type
import Linearity.UseCombination
import Linearity.TypeCombination
import Linearity.Process

import qualified Session.Type
import qualified Session.Decompiler

import qualified DeadLock.Interface
import qualified DeadLock.Generator
import qualified DeadLock.Type as DT
import BasicType
import qualified DeadLock.ValueExpression as VE
import qualified DeadLock.TypeExpression as TE
import qualified DeadLock.Solver as DS

import qualified Parser
import qualified Lexer
import qualified Linearity.Generator as Generator
import qualified Relation
import Control.Monad
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.Set.Unicode
import Data.Bool.Unicode
import Data.Char
import Data.Time
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Linearity.UseSolver as UseSolver
import qualified Linearity.TypeSolver as TypeSolver
import qualified Linearity.Partitioner as Partitioner
import qualified Linearity.Combination as Combination

import Debug.Trace

basicStage :: String
basicStage = "LINEARITY ANALYSIS"

sessionStage :: String
sessionStage = "PROTOCOL ANALYSIS"

deadlockStage :: String
deadlockStage = "DEADLOCK ANALYSIS"

lockStage :: String
lockStage = "LOCK ANALYSIS"

versionInfo :: String
versionInfo =
    "Hypha 0.5 - Type Reconstruction for the Linear π-Calculus\n"
    ++ "Copyright © 2013-2014 Luca Padovani\n"
    ++ "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
    ++ "This is free software: you are free to change and redistribute it.\n"
    ++ "There is NO WARRANTY, to the extent permitted by law."

showPRED :: (Show a) => String -> a -> String
showPRED pred x = pred ++ " " ++ show x

showREL :: (Show a, Show b) => String -> (a, b) -> String
showREL rel (x, y) = "  " ++ show x ++ " " ++ rel ++ " " ++ show y

showREL3 :: (Show a, Show b, Show c) => String -> String -> (a, b, c) -> String
showREL3 rel op (x, y, z) = "  " ++ show x ++ " " ++ rel ++ " " ++ show y ++ " " ++ op ++ " " ++ show z

showSDV :: (TypeV, (Type, Type)) -> String
showSDV (tvar, (t, s)) = "  " ++ show (TVar tvar :: Type) ++ " = " ++ show t ++ " - " ++ show s

showC :: (TypeV, S.Set TypeC) -> String
showC (tvar, tcset) = "  " ++ show (TVar tvar :: Type) ++ " = " ++ showSet tcset
    where
      showSet :: Show a => S.Set a -> String
      showSet s = L.intercalate " = " (L.map show $ S.toList s)

putSectionTitle :: String -> String -> IO ()
putSectionTitle s t = do putStrLn $ "┌─" ++ L.replicate sl '─' ++ "─┬─" ++ L.replicate tl '─' ++ "─╖"
                         putStrLn $ "│ " ++ s ++ " │ " ++ t ++ " ║"
                         putStrLn $ "╘═" ++ L.replicate sl '═' ++ "═╧═" ++ L.replicate tl '═' ++ "═╝"
  where
    sl = length s
    tl = length t

printUseEnvironment :: Use -> UseEnvironment -> IO ()
printUseEnvironment u env =
    do let us = map (show . UVar . fst) $ M.toList $ M.filter (== u) env
       if us /= [] then putStrLn $ "  " ++ show u ++ (concat $ map (" = " ++) us)
       else return ()

printEnvironment :: (Ord v, Show v) => (k -> String) -> M.Map k v -> IO ()
printEnvironment showKey env = forM_ im (\(v, keys) -> putStrLn $ "  " ++ show v ++ (concat $ map (\k -> " = " ++ showKey k) keys))
    where
      im = map (\v -> (v, M.keys $ M.filter (== v) env)) values

      values = L.nub $ M.elems env

{-|
Return the string representation of the given Process.
The lmap, tmap and typeq arguments are used to display instead
of level or type variables the actual level or type value.
-}
showProcess :: Bool -> Process TE.TypeE -> M.Map TE.LevelV Int -> M.Map TE.TicketV Int -> M.Map DT.TypeV TE.TypeE -> String
showProcess lf p lenv tenv tpenv =
    -- Render.renderProcess show p
    Render.renderProcess showType (Process.mapProcess (\x -> TE.approximate' (TE.findD tpenv x) lenv tenv) p)
  where
    showType Nothing = Nothing
    showType (Just (l, tick)) | lf = Just $ "[" ++ show l ++ "," ++ show tick ++ "]"
                              | otherwise = Just $ "[" ++ show l ++ "]"

printTime :: String -> UTCTime -> IO ()
printTime msg t0 =
    do t1 <- getCurrentTime
       putStrLn $ "⌚ [" ++ msg ++ ": " ++ show (diffUTCTime t1 t0) ++ "]"

reconstruction :: [Flag] -> UntypedProcess -> IO (Process Type, M.Map Name Type)
reconstruction args p =
    do timeGenerateStart <- getCurrentTime
       let (q, env, (eqm0, stm0, cot0, com0, eqU)) = Generator.generate (OddChannels `elem` args) p
       when (Verbose `elem` args)
            (do putSectionTitle basicStage "PROCESS"
                putStrLn $ Render.renderProcess (Just . show) q
                putSectionTitle basicStage "UNSOLVED TYPE ENVIRONMENT"
                forM_ (M.toAscList env) (\(u, t) -> putStrLn $ "  " ++ u ++ " : " ++ show t)
                putSectionTitle basicStage "TYPE CONSTRAINTS"
                forM_ (map (\(tvar, t) -> (TVar tvar :: Type, t)) $ M.toList eqm0) (putStrLn . showREL "=")
                forM_ (map (\(tvar, t) -> (TVar tvar :: Type, t)) $ M.toList $ M.difference stm0 eqm0) (putStrLn . showREL "~")
                forM_ (map (\(tvar, tc) -> (TVar tvar :: Type, tc)) $ M.toList cot0) (putStrLn . showREL "=")
                putSectionTitle basicStage "ALL TYPE COMBINATIONS"
                forM_ (M.toList com0) (putStrLn . showC)
                putSectionTitle basicStage "USE CONSTRAINTS"
                forM_ (S.toList eqU) (putStrLn . showREL "=")
              )

       when (Timing `elem` args)
            (do when (not $ Verbose `elem` args) (putSectionTitle basicStage "TIMING")
                printTime "CONSTRAINT GENERATOR" timeGenerateStart)


       timeUseSolutionStart <- getCurrentTime
       let ucS = if Partition `elem` args then Partitioner.partitionUseConstraints eqU else [eqU]
       let uenv0 = M.unions $ map (UseSolver.findUseSolution (MinUses `elem` args)) ucS
       let uvarset = (S.unions $ map Linearity.Type.fuv $ M.elems eqm0) ∪
                     (S.unions $ map Linearity.Type.fuv $ M.elems stm0)
       let uenv = M.union uenv0 $ M.fromList [(uvar, 0) | uvar <- S.toList uvarset ]

       when (Verbose `elem` args)
            (do putSectionTitle basicStage "USE SOLUTION"
                putStrLn $ "  [found " ++ show (S.size uvarset) ++ " use variables and "
                                       ++ show (length ucS) ++ " partition(s) of use constraints]"
                let nlargest = maximum $ map (S.size .
                                              S.unions .
                                              map (uncurry S.union) .
                                              S.elems .
                                              S.map (mapPair Linearity.UseCombination.fuv)) (S.empty : ucS)
                putStrLn $ "  [the largest partition has " ++ show nlargest ++ " use variables]"
                printUseEnvironment 0 uenv
                printUseEnvironment 1 uenv
                printUseEnvironment ω uenv
            )

       when (Timing `elem` args)
            (printTime "USE SOLVER" timeUseSolutionStart)


       let eqm = M.map (substUT uenv) eqm0
       let stm = M.map (substUT uenv) stm0
       -- lazyness alert: if there is no solution for use constraints but there are
       -- empty combinations in front of those where the unsolvable use variables occur
       -- then the program will not signal unsatisfiability and will output a (wrong) result
       -- the following line will FORCE evaluation of all the use constraints even when
       -- hypha has been invoked without the -v option
       when (minimum (M.elems uenv) >= Zero) (return ())
       let com = M.map (Linearity.TypeCombination.substUC uenv . S.findMin) com0
       timeTypeSolutionStart <- getCurrentTime
       let defm = TypeSolver.definitions eqm stm com
       when (Verbose `elem` args)
            (do putSectionTitle basicStage "TYPE SOLUTION"
                forM_ (map (\(tvar, t) -> (TVar tvar :: Type, t)) $ M.toList defm) (putStrLn . showREL "↦")
            )
       when (Timing `elem` args)
                     (do printTime "TYPE SOLVER" timeTypeSolutionStart
                         printTime "RECONSTRUCTION" timeGenerateStart)

       let f = substTT defm . substUT uenv
       return (mapProcess f q, M.map f env)

sessionAnalysis :: [Flag] -> M.Map Name Type -> Process Type -> IO ()
sessionAnalysis args env p =
  do let env' = M.map (Session.Type.refold . Session.Decompiler.decompile) env
     let p' = Process.mapProcess (Session.Type.refold . Session.Decompiler.decompile) p
     putSectionTitle sessionStage "TYPE ENVIRONMENT"
     forM_ (M.toAscList env') (putStrLn . uncurry Session.Type.showBinding)
     when (Verbose `elem` args)
          (do putSectionTitle sessionStage "PROCESS"
              putStrLn $ Render.renderProcess (Just . show) p'
          )

deadlockAnalysis :: [Flag] -> M.Map Name Type -> Process Type -> IO ()
deadlockAnalysis args env0 p0 =
    do let stage = if Lock `elem` args then lockStage else deadlockStage
       timeGeneratorStart <- getCurrentTime
       let (p, env, (eqm, stm, cor, eql, ltl, eqtic)) = DeadLock.Generator.generate (Lock `elem` args) $
                                                        DeadLock.Interface.processM env0 p0
       when (Verbose `elem` args)
         (do putSectionTitle stage "PROCESS"
             putStrLn $ Render.renderProcess (Just . show) p
             putSectionTitle stage "UNSOLVED TYPE ENVIRONMENT"
             forM_ (M.toAscList env) (\(u, t) -> putStrLn $ "  " ++ u ++ " : " ++ show t)
             putSectionTitle stage "TYPE CONSTRAINTS"
             forM_ (map (\(tvar, t) -> (TE.TEVar tvar VE.valueNil VE.valueNil, t)) $ M.toList eqm) (putStrLn . showREL "=")
             forM_ (map (\(tvar, t) -> (TE.TEVar tvar VE.valueNil VE.valueNil, t)) $ M.toList $ M.difference stm eqm) (putStrLn . showREL "~")
             forM_ (S.toList cor) (putStrLn . showREL3 "=" "+")
             putSectionTitle stage "LEVEL CONSTRAINTS"
             forM_ (S.toList eql) (putStrLn . showREL "=")
             forM_ (S.toList ltl) (putStrLn . showREL "<")
             when (Lock `elem` args)
               (do putSectionTitle stage "TICKET CONSTRAINTS"
                   forM_ (S.toList eqtic) (putStrLn . showREL "=")
               )
         )

       when (Timing `elem` args)
           (do when (not $ Verbose `elem` args) (putSectionTitle stage "TIMING")
               printTime "CONSTRAINT GENERATOR" timeGeneratorStart)

       timeSolverStart <- getCurrentTime
       let analysis = if (Lock `elem` args) then DS.LockFreedom else DS.DeadlockFreedom
       let solver = getSolverOpt args

       lsol <- solveLvl solver analysis (Verbose `elem` args) eql ltl

       when (Timing `elem` args)
               (printTime "LEVEL SOLVER" timeSolverStart)

       when (Verbose `elem` args)
         (do putSectionTitle stage "LEVEL SOLUTION"
             printEnvironment show lsol
         )

       tsol <- solveTick solver analysis (Verbose `elem` args) eqtic
       when (Timing `elem` args && Lock `elem` args)
               (printTime "TICKET SOLVER" timeSolverStart)
       when (Verbose `elem` args && Lock `elem` args)
         (do putSectionTitle "LOCK" "TICKET SOLUTION"
             printEnvironment show tsol
         )

       let tpenv = M.union eqm stm -- contraints of the kind tx = ty and tx ~ ty where tx and ty are type expressions
       let lvarset = S.unions $ map TE.levelVars $ M.elems tpenv
       let lenv = M.union lsol $ M.fromList [(lvar, 0) | lvar <- S.toList lvarset ]
       let tvarset = S.unions $ map TE.ticketVars $ M.elems tpenv
       let tenv = M.union tsol $ M.fromList [(tvar, 0) | tvar <- S.toList tvarset ]
       --putSectionTitle stage "LENV"
       --putStrLn $ "lsol: " ++ show lsol
       --putSectionTitle "LOCK" "TENV"
       --putStrLn $ "tsol: " ++ show tsol

       when (Verbose `elem` args)
         (do putSectionTitle stage "PROCESS SOLUTION"
             putStrLn $ showProcess (Lock `elem` args) p lenv tenv tpenv
         )

       when (not (Verbose `elem` args)) (putSectionTitle stage "OK")

  where
    solveLvl solver analysis verbose eql ltl = do
      solutionL <- DS.solve solver analysis verbose eql ltl
      case solutionL of
        Nothing | Lock `elem` args -> error "no solution for lock freedom"
        Nothing -> error "no solution for deadlock freedom"
        Just lenv -> return lenv
    solveTick solver analysis verbose eqtic =
      if (Lock `elem` args) then
        do solutionTic <- DS.solve solver analysis verbose eqtic S.empty
           case solutionTic of
             Nothing -> error "no solution for lock freedom"
             Just ticenv -> return ticenv
      else do return M.empty

main :: IO ()
main = do
  (args, file) <- getArgs >>= parse
  source <- if file == "-" then getContents else readFile file
  let p = Parser.process $ Lexer.alexScanTokens source

  timeProcessStart <- getCurrentTime
  (q1, env1) <- reconstruction args p
  when (not (Session `elem` args) && not (DeadLock `elem` args) && not (Lock `elem` args))
    (do putSectionTitle basicStage "TYPE ENVIRONMENT"
        forM_ (M.toAscList env1) (\(u, t) -> putStrLn $ "  " ++ u ++ " : " ++ show t)
    )

  when (Session `elem` args) $ sessionAnalysis args env1 q1

  when (DeadLock `elem` args ∨ Lock `elem` args) $ deadlockAnalysis args env1 q1

  timeProcessEnd <- getCurrentTime
  when (Timing `elem` args)
       (hPutStrLn stderr $ show (diffUTCTime timeProcessEnd timeProcessStart) ++ " for processing " ++ file)

{-|
Supported command-line options datatype.
-}
data Flag = Partition         --    --partition
          | OddChannels       -- -o --odd-channels
          | MinUses           -- -m --minimum-uses
          | Session           -- -s
          | DeadLock          -- -d
          | Lock              -- -l
          | Solver DS.Solver  --    --solver=SOLVER
          | Timing            --    --timing
          | Verbose           -- -v --verbose
          | Version           -- -V --version
          | Help              --    --help
            deriving (Eq, Ord, Show)

{-|
List of the supported command-line options.
-}
flags :: [OptDescr Flag]
flags =
   [Option ""  ["partition"]      (NoArg Partition)   "Partition use constraints",
    Option "o" ["odd-channels"]   (NoArg OddChannels) "Allow different input/output uses for restricted channels",
    Option "m" ["minimum-uses"]   (NoArg MinUses)     "Minimize the number of ωs in uses",
    Option "s" []                 (NoArg Session)     "Enable session analysis",
    Option "d" []                 (NoArg DeadLock)    "Enable deadlock freedom analysis",
    Option "l" []                 (NoArg Lock)        "Enable lock freedom analysis",
    Option ""  ["solver"]         (ReqArg parseSolverArgv "SOLVER")     "The solver of level/ticket contraints; SOLVER can be 'glpk', 'glpsol', or 'lp_solve'",
    Option ""  ["timing"]         (NoArg Timing)      "Show timing",
    Option "v" ["verbose"]        (NoArg Verbose)     "Print use and type constraints",
    Option "V" ["version"]        (NoArg Version)     "Print version information",
    Option "h" ["help"]           (NoArg Help)        "Print this help message"
   ]

{-|
Parse the command-line arguments, and return a pair containing
the list of runtime options enabled by the user and
the name of the file containing the target process.
-}
parse argv =
    case getOpt Permute flags argv of
      (args, files, []) -> do
        when (Version `elem` args)
          (do
              hPutStrLn stderr versionInfo
              exitWith ExitSuccess)
        when (null files || L.length files > 1 || Help `elem` args)
          (do
              hPutStrLn stderr (usageInfo header flags)
              exitWith ExitSuccess)
        return (args, head files)

      (_, _, errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where
      header = "Usage: hypha [options] FILE"

{-|
Parse a string containing the name of a solver and return
the corresponding instance of the Flag datatype.
-}
parseSolverArgv :: String -> Flag
parseSolverArgv str = Solver (parseSolverName str)
  where
    parseSolverName :: String -> DS.Solver
    parseSolverName str = case (map toLower str) of
      "glpsol"   -> DS.GLPSol
      "glpk"     -> DS.GLPK
      "lp_solve" -> DS.LPSolve
      "cplex"    -> DS.CPlex
      _          -> error $ "Solver " ++ str ++ " not supported"


{-|
The default solver.
-}
defaultSolver :: DS.Solver
defaultSolver = DS.GLPK

{-|
Extract the type of solver that has to be used
for the (dead)lock analysis from the lis
of command-line options.
-}
getSolverOpt :: [Flag] -> DS.Solver
getSolverOpt args = (
  case (L.find aux args) of
    Just (Solver x) -> x
    Nothing -> defaultSolver)
  where
    aux :: Flag -> Bool
    aux (Solver _) = True
    aux _ = False

