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
module Lexer (Token(..), alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9                    -- digits
$firstlc = [a-z]               -- alphabetic characters
$firstuc = [A-Z]
$next  = [0-9a-zA-Z\_\']

tokens :-

  $white+ ;
  "--".*  ;
  $digit+ { \s -> INT (read s) }
  "!"     { \_ -> EMARK }
  "?"     { \_ -> QMARK }
  "+"     { \_ -> PLUS }
  "-"     { \_ -> MINUS }
  "*"     { \_ -> STAR }
  "/"     { \_ -> SLASH }
  "|"     { \_ -> PAR }
  "("     { \_ -> LPAR }
  ")"     { \_ -> RPAR }
  "="     { \_ -> Lexer.EQ }
  "<"     { \_ -> Lexer.LT }
  ">"     { \_ -> Lexer.GT }
  "<="    { \_ -> Lexer.LE }
  ">="    { \_ -> Lexer.GE }
  "≤"     { \_ -> Lexer.LE }
  "≥"     { \_ -> Lexer.GE }
  "<>"    { \_ -> Lexer.NE }
  "≠"     { \_ -> Lexer.NE }
  "∧"     { \_ -> Lexer.AND }
  "/\"    { \_ -> Lexer.AND }
  "∨"     { \_ -> Lexer.OR }
  "\/"    { \_ -> Lexer.OR }
  "¬"     { \_ -> Lexer.NOT }
  "["     { \_ -> LBRACK }
  "]"     { \_ -> RBRACK }
  "{"     { \_ -> LBRACE }
  "}"     { \_ -> RBRACE }
  ","     { \_ -> COMMA }
  "."     { \_ -> DOT }
  ":"     { \_ -> COLON }
  ";"     { \_ -> SEMICOLON }
  "=>"    { \_ -> ARROW }
  "⇒"     { \_ -> ARROW }
  "_"     { \_ -> UNDERSCORE }
  "×"     { \_ -> TPAIR }
  "⊕"     { \_ -> TSUM }
  "ω"     { \_ -> OMEGA }
  $firstlc $next* { \s -> lookupId LID s }
  $firstuc $next* { \s -> lookupId UID s }  
{

keywords :: [(String, Token)]
keywords = [("true", TRUE),
            ("false", FALSE),
            ("if", IF),
            ("then", THEN),
            ("else", ELSE),
            ("def", DEF),
            ("as", AS),
            ("let", LET),
            ("in", IN),
            ("fst", FST),
            ("snd", SND),
            ("mod", MOD),
            ("new", NEW),
            ("case", CASE),
            ("of", OF),
            ("and", ANDKW),
            ("or", ORKW),
            ("not", NOT),
            ("Unit", TUNIT),
            ("Int", TINT),
            ("Bool", TBOOL)]

lookupId :: (String -> Token) -> String -> Token
lookupId ctor s = case lookup s keywords of
                    Nothing -> ctor s
                    Just tok -> tok

-- Each right-hand side has type :: String -> Token

-- The token type:
data Token =
    INT Int     |
    TRUE        |
    FALSE       |
    EMARK       |
    QMARK       |
    STAR        |
    IF          |
    LET         |
    CASE        |
    OF          |
    THEN        |
    ELSE        |    
    DEF         |
    TUNIT       |
    TINT        |
    TBOOL       |
    TPAIR       |
    TSUM        |
    OMEGA       |
    PAR         |
    NEW         |
    LPAR        |
    RPAR        |
    EQ          |
    IN          |
    AS          |
    FST         |
    SND         |
    MOD         |
    GT          |
    LT          |
    GE          |
    LE          |
    NE          |
    AND         |
    OR          |
    ANDKW       |
    ORKW        |
    NOT         |
    LBRACK      |
    RBRACK      |
    LBRACE      |
    RBRACE      |
    PLUS        |
    MINUS       |
    SLASH       |
    COMMA       |
    DOT         |
    COLON       |
    SEMICOLON   |
    ARROW       |
    UNDERSCORE  |
    LID String  |
    UID String  |
    ERR
    deriving (Eq,Show)
}
