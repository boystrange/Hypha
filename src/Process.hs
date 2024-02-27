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

module Process where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Set.Unicode

type Tag = String
type Name = String

type Rule t = (Tag, Name, t, Process t)

data Process t =
    Idle
  | Send (Expression t) (Expression t) (Process t)
  | Receive (Expression t) Name t (Process t)
  | Star (Process t)
  | Par (Process t) (Process t)
  | New Name t (Process t)
  | Group (Process t)
  | Let Name t (Expression t) (Process t)
  | Split (Expression t) Name t Name t (Process t)
  | If (Expression t) (Process t) (Process t)
  | Case (Expression t) [Rule t]
    deriving Show

data Const =
    UNIT
  | INT Int
  | BOOL Bool
    deriving (Show, Eq)

data UnOp =
    NOT
  | FST
  | SND
    deriving Show

data BinOp =
    AND
  | OR
  | LT
  | LE
  | GT
  | GE
  | EQ
  | NE
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | PAIR
    deriving (Show, Eq)

data Expression t =
    Id Name t
  | Const Const
  | UnOp UnOp (Expression t)
  | BinOp BinOp (Expression t) (Expression t)
  | Tag Tag (Expression t)
    deriving Show

mapProcessX :: Bool -> (M.Map Name t -> Name -> t -> t') -> M.Map Name t -> Process t -> Process t'
mapProcessX collect m = auxP
    where
      auxP _ Idle = Idle
      auxP env (Send e f p) = Send (auxE env e) (auxE env f) (auxP env p)
      auxP env (Receive e x typ p) = let env' = update x typ env in
                                     Receive (auxE env e) x (m env' x typ) (auxP env' p)
      auxP env (Star p) = Star (auxP env p)
      auxP env (Par p q) = Par (auxP env p) (auxP env q)
      auxP env (New x typ p) = let env' = update x typ env in
                               New x (m env' x typ) (auxP env' p)
      auxP env (Group p) = Group (auxP env p)
      auxP env (Let x typ e p) = let env' = update x typ env in
                                 Let x (m env' x typ) (auxE env e) (auxP env' p)
      auxP env (Split e x xt y yt p) = let env' = update x xt $ update y yt env in
                                       Split (auxE env e) x (m env' x xt) y (m env' y yt) (auxP env' p)
      auxP env (If e p q) = If (auxE env e) (auxP env p) (auxP env q)
      auxP env (Case e rules) = Case (auxE env e) (map (auxR env) rules)

      auxE env (Id x typ) = Id x (m env x typ)
      auxE _ (Const c) = Const c
      auxE env (UnOp op e) = UnOp (auxUO op) (auxE env e)
      auxE env (BinOp op e1 e2) = BinOp op (auxE env e1) (auxE env e2)
      auxE env (Tag tag e) = Tag tag (auxE env e)

      auxUO NOT = NOT
      auxUO FST = FST
      auxUO SND = SND

      auxR env (tag, x, typ, p) = let env' = update x typ env in
                                  (tag, x, (m env' x typ), auxP env' p)

      update x typ env | collect = M.insert x typ env
                       | otherwise = M.delete x env

mapProcess :: (t -> t') -> Process t -> Process t'
mapProcess m = mapProcessX False (const $ const m) M.empty

assignType :: M.Map Name (Maybe t) -> Process (Maybe t) -> Process (Maybe t)
assignType = mapProcessX False m
  where
    m env x Nothing | Just typ <- M.lookup x env = typ
    m _ _ typ = typ               

fn :: Process t -> S.Set Name
fn = auxP
    where
      auxP Idle = (∅)
      auxP (Send e1 e2 p) = auxE e1 ∪ auxE e2 ∪ auxP p
      auxP (Receive e x _ p) = auxE e ∪ (S.delete x $ auxP p)
      auxP (Star p) = auxP p
      auxP (Par p q) = auxP p ∪ auxP q
      auxP (New x _ p) = S.delete x $ auxP p
      auxP (Group p) = auxP p
      auxP (Let x _ e p) = auxE e ∪ (S.delete x $ auxP p)
      auxP (Split e x _ y _ p) = auxE e ∪ (S.delete x $ S.delete y $ auxP p)
      auxP (If e p q) = auxE e ∪ auxP p ∪ auxP q
      auxP (Case e rules) = auxE e ∪ S.unions (map auxR rules)

      auxE (Id x _) = S.singleton x
      auxE (Const _) = S.empty
      auxE (UnOp _ e) = auxE e
      auxE (BinOp _ e1 e2) = auxE e1 ∪ auxE e2
      auxE (Tag _ e) = auxE e

      auxR (_, x, _, p) = S.delete x $ auxP p
