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

{
module Parser where

import Lexer
import Process
import BasicType
import Linearity.Use
import Linearity.Combination as C
import Linearity.Type
import Linearity.Process

import qualified Data.Map as M

import Data.Bool.Unicode

}

%name process
%tokentype { Token }
%error { throwError }

%token 
  new       { NEW }
  intval    { Lexer.INT $$ }
  true      { TRUE }
  false     { FALSE }
  lid       { LID $$ }
  uid       { UID $$ }
  let       { LET }
  in        { IN }
  fst       { Lexer.FST }
  snd       { Lexer.SND }
  case      { CASE }
  of        { OF }
  if        { IF }
  then      { THEN }
  else      { ELSE }
  def       { DEF }
  as        { AS }
  arrow     { ARROW }
  '='       { Lexer.EQ }
  '|'       { PAR }
  '('       { LPAR }
  ')'       { RPAR }
  '<'       { Lexer.LT }
  '>'       { Lexer.GT }
  le        { Lexer.LE }
  ge        { Lexer.GE }
  ne        { Lexer.NE }
  andkw     { Lexer.ANDKW }
  orkw      { Lexer.ORKW }
  not       { Lexer.NOT }
  '∧'       { Lexer.AND }
  and       { Lexer.AND }
  '∨'       { Lexer.OR }
  or        { Lexer.OR }
  '¬'       { Lexer.NOT }
  mod       { Lexer.MOD }
  unit      { Lexer.TUNIT }
  int       { Lexer.TINT }
  bool      { Lexer.TBOOL }
  '['       { LBRACK }
  ']'       { RBRACK }
  '{'       { LBRACE }
  '}'       { RBRACE }
  '!'       { EMARK }
  '?'       { QMARK }
  ','       { COMMA }
  '.'       { DOT }
  ':'       { COLON }
  ';'       { SEMICOLON }
  '*'       { STAR }
  '+'       { PLUS }
  '-'       { MINUS }
  '/'       { SLASH }
  '_'       { UNDERSCORE }
  '×'       { TPAIR }
  '⊕'        { TSUM }
  'ω'       { OMEGA }

%left '|'
%nonassoc in as
%right ','
%nonassoc inl inr fst snd uid
%left and or '∧' '∨'
%nonassoc '<' '>' '=' le ge ne
%left '+' '-' '⊕'
%left '*' '/' mod
%right '×'
%nonassoc not '¬'
%right BANG
%right '.'

%%

Program : Process { $1 }

Process : Expr '!' ExprOpt Continuation { Send $1 $3 $4 }
        | Expr '?' PatternOpt Continuation { 
            let (x, t) = getBinding $3 in
            Receive $1 x t $ expand (Id x t) $3 $4
          }
        | let Pattern '=' Expr in Process { expand $4 $2 $6 }
        | case Expr of '{' Rules '}' { Case $2 $5 }
        | case Expr '?' of '{' Rules '}' { Receive $2 "%val" Nothing $ Case (Id "%val" Nothing) $6 }
        | if Expr then Process else Process { If $2 $4 $6 }
        | Process '|' Process { Par $1 $3 } 
        | new lid TypeOpt in Process { New $2 $3 $5 }
        | '{' ProcessOpt '}' { Group $2 }
        | '*' Process %prec BANG { Star $2 }
        | def Definitions in Process
          { let t = TChannel tunknown (C.singleton Omega) (C.singleton Omega) in
            let s = TChannel tunknown C.empty (C.singleton Omega) in
            let env = M.fromList $ Prelude.map (\(x, _) -> (x, Just s)) $2 in
            foldr (\var -> New var (Just t))
                  (foldr Par (Process.assignType env $4) (Prelude.map (Process.assignType env . snd) $2))
                  (Prelude.map fst $2)
          }

Definitions : Definition { [$1] }
            | Definition andkw Definitions { $1 : $3 }

Definition : lid '?' PatternOpt '=' Process
           { let (x, t) = getBinding $3 in
             ($1, Star $ Receive (Id $1 (Just $ TChannel tunknown (C.singleton Omega) (C.singleton Omega))) x t $ expand (Id x t) $3 $5) }

Rules   : { [] }
        | RulesNE { $1 }

RulesNE : Rule { [$1] }
        | Rule ';' RulesNE { $1 : $3 }

Rule    : uid PatternOpt arrow Process {
              let (x, t) = getBinding $2 in
              ($1, x, t, expand (Id x t) $2 $4)
          }

Continuation : { Idle }
        | '.' Process { $2 }

ProcessOpt :  { Idle }
        | Process { $1 }
        
PatternOpt : { PUnit }
           | Pattern { $1 }

Pattern : '(' ')' { PUnit }
        | '_' TypeOpt { PAny $2 }
        | lid TypeOpt { PVar $1 $2 }
        | Pattern as lid TypeOpt { PAs $1 $3 $4 }
        | Pattern ',' Pattern { PPair $1 $3 }
        | '(' Pattern ')' { $2 }

Expr    : '(' ')' { Const UNIT }
        | intval   { Const $ Process.INT $1 }
        | true { Const $ Process.BOOL True }
        | false { Const $ Process.BOOL False }
        | lid { Id $1 Nothing }
        | '(' lid ':' Type ')' { Id $2 (Just $4) }
	| Expr and Expr { BinOp Process.AND $1 $3 }
	| Expr '∧' Expr { BinOp Process.AND $1 $3 }
	| Expr or Expr { BinOp Process.OR $1 $3 }
	| Expr '∨' Expr { BinOp Process.OR $1 $3 }
	| Expr '<' Expr { BinOp Process.LT $1 $3 }
	| Expr '>' Expr { BinOp Process.GT $1 $3 }
	| Expr '=' Expr { BinOp Process.EQ $1 $3 }
	| Expr le Expr { BinOp Process.LE $1 $3 }
	| Expr ge Expr { BinOp Process.GE $1 $3 }
	| Expr ne Expr { BinOp Process.NE $1 $3 }
	| not Expr { UnOp Process.NOT $2 }
	| '¬' Expr { UnOp Process.NOT $2 }
	| Expr '+' Expr { BinOp Process.ADD $1 $3 }
	| Expr '-' Expr { BinOp Process.SUB $1 $3 }
	| Expr '*' Expr { BinOp Process.MUL $1 $3 }
	| Expr '×' Expr { BinOp Process.MUL $1 $3 }
	| Expr '/' Expr { BinOp Process.DIV $1 $3 }
        | Expr mod Expr { BinOp Process.MOD $1 $3 }
        | fst Expr { UnOp Process.FST $2 }
        | snd Expr { UnOp Process.SND $2 }
        | '(' Expr ')' { $2 }
        | Expr ',' Expr { BinOp PAIR $1 $3 }
        | uid Expr { Tag $1 $2 }

ExprOpt : { Const UNIT }
        | Expr { $1 }

TypeOpt : { Nothing }
        | ':' Type { Just $2 }

BasicType : unit { TUnit }
          | int { TInt }
          | bool { TBool }

Type    : BasicType { TBasic $1 }
        | '_' { tunknown }
        | '[' Type ']' '{' Use ',' Use '}' { TChannel $2 $5 $7 }
        | Type '×' Type { TProduct $1 $3 }
        | Type '*' Type { TProduct $1 $3 }

Use     : intval { case $1 of
                     0 -> C.singleton Zero
                     1 -> C.singleton One
                     _ -> C.singleton Omega }
        | 'ω' { C.singleton Omega }
{

data Pattern = PUnit
             | PAny (Maybe TypeE)
             | PVar String (Maybe TypeE)
             | PAs Pattern String (Maybe TypeE)
             | PPair Pattern Pattern

getBinding :: Pattern -> (String, Maybe TypeE)
getBinding (PVar x topt) = (x, topt)
getBinding _ = ("%val", Nothing)

expand :: UntypedExpression -> Pattern -> UntypedProcess -> UntypedProcess
expand (Id x tx) (PVar y ty) p | (x == y) ∧ (tx == ty) = p
expand e PUnit p = Let "_" (Just tunit) e p
expand e (PAny topt) p = Let "_" topt e p
expand e (PVar x topt) p = Let x topt e p 
expand e (PAs pattern x topt) p = Let x topt e (expand e pattern p)
-- expand e (PPair pattern1 pattern2) p = expand (UnOp Process.FST e) pattern1 $
--                                        expand (UnOp Process.SND e) pattern2 p
expand e (PPair pattern1 pattern2) p = Split e "%1" Nothing "%2" Nothing $
                                       expand (Id "%1" Nothing) pattern1 $
                                       expand (Id "%2" Nothing) pattern2 p

throwError :: [Token] -> a
throwError tokens = error ("Parse error on "++ (show $ head tokens) ++ "\n")

}
