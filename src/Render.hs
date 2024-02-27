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

module Render (renderProcess) where

import Process
import Text.PrettyPrint.Leijen as L

myIndent :: Doc -> Doc -> [Doc] -> [Doc]
myIndent _ _ [] = []
myIndent first next (d : ds) = first <+> d : map (next <+>) ds

ppP :: (a -> Maybe String) -> Process a -> Doc
ppP ppT = auxP
    where
      auxF x _ = text x

      auxB x typ =
        case ppT typ of
          Nothing -> text x
          Just typ' -> text x <+> colon <+> text typ'

      auxUO :: UnOp -> Doc
      auxUO NOT = text "not"
      auxUO FST = text "fst"
      auxUO SND = text "snd"

      auxBO :: BinOp -> Doc
      auxBO op = case lookup op ops of
                   Nothing -> error "IMPOSSIBLE"
                   Just s -> text s
          where
            ops = [(AND, "∧"),
                   (OR, "∨"),
                   (Process.LT, "<"),
                   (LE, "≤"),
                   (Process.GT, ">"),
                   (GE, "≥"),
                   (Process.EQ, "="),
                   (NE, "≠"),
                   (ADD, "+"),
                   (SUB, "-"),
                   (MUL, "×"),
                   (DIV, "/"),
                   (MOD, "mod"),
                   (PAIR, ",")]

      auxC :: Const -> Doc
      auxC UNIT = text "()"
      auxC (INT n) = int n
      auxC (BOOL False) = text "False"
      auxC (BOOL True) = text "True"

      auxE (Id x typ) = auxB x typ
      auxE (Const c) = auxC c
      auxE (UnOp op e) = auxUO op <> parens (auxE e)
      auxE (BinOp op e1 e2) = parens $ auxE e1 <+> auxBO op <+> auxE e2
      auxE (Tag tag e) = text tag <+> auxE e

      auxP Idle = text "{}"
      auxP (Send e f Idle) = auxE e <> text "!" <> auxE f
      auxP (Send e f p) = cat [auxE e <> text "!" <> auxE f <> text ".", auxP p]
      auxP (Receive e x typ p) = cat [auxE e <> text "?(" <> auxB x typ <> text ").", auxP p]
      auxP (Star p) = text "*" <> (align $ auxP p)
      auxP (Par p q) = sep [lbrace <+> nest 2 (auxP p), text "|" <+> nest 2 (auxP q), rbrace]
      auxP (New a typ p) = align $ sep [text "new" <+> auxB a typ <+> text "in", auxP p]
      auxP (Group p) = auxP p
      auxP (Let x typ e p) = sep [text "let" <+> auxB x typ <+> equals <+> auxE e <+> text "in", auxP p]
      auxP (Split e x xt y yt p) = vsep [text "split" <+> auxE e <+> text "as" <+> auxB x xt <> text "," <+> auxB y yt <+> text "in", auxP p]
      auxP (If e p q) = sep [text "if" <+> auxE e <+> text "then" <+> auxP p,
                             text "else" <+> auxP q]
      auxP (Case e rules) = sep $ [text "case" <+> auxE e <+> text "of"] ++
                                   myIndent lbracket (text ";") (map auxR rules) ++ [rbracket]

      auxR (tag, x, xt, p) = nest 2 $ hang 2 $ sep [text tag <+> auxB x xt <+> text "=>", auxP p]

renderProcess :: (a -> Maybe String) -> Process a -> String
renderProcess ppT proc = displayS (renderPretty (2/3) 100 $ indent 2 $ ppP ppT proc) ""
