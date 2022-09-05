{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDPrettyCCG where

import qualified Data.Sequence as DS
import Data.List (intercalate)
import TDParseCCG
import Lambda_calc
import Prelude hiding ((<>), (^), and)
import Text.PrettyPrint hiding (Mode, cat)


{- Various pretty printers -}

prettyTy :: Doc -> Type -> Doc
prettyTy a = \case
  E           -> "e"
  T           -> "t"
  (Eff f t)   -> prettyF a f <+> prettyParam a t
  (t1 :-> t2) -> prettyArrow a t1 t2
  _ -> "trying to print a category"

prettyArrow a t1 t2 = case t1 of
  t3 :-> t4 -> parens (prettyTy a t1) <> a <> prettyTy a t2
  _         ->         prettyTy a t1  <> a <> prettyTy a t2

prettyF :: Doc -> F -> Doc
prettyF a = \case
  S     -> "S"
  R r   -> "R" -- <+> prettyParam a r
  W w   -> "W" -- <+> prettyParam a w
  C r o -> "C" -- <+> prettyParam a r <+> prettyParam a o

prettyParam a r@(t1 :-> t2) = parens (prettyTy a r)
prettyParam a r@(Eff f t)   = parens (prettyTy a r)
prettyParam a r             = prettyTy a r

showType :: Doc -> Type -> String
showType a = show . prettyTy a

arrow = " -> "

prettyProof :: Proof -> Doc
prettyProof (Proof phrase val ty daughters) =
  let details =
        text phrase <> " :: " <>
        prettyTy arrow (cat2type ty) <> " = " <> text (show (eval (semTerm val)))
   in case daughters of -- no unary inferences
        []     -> "  " <> details
        [a, b] -> "  " <> (details $+$ prettyProof a $+$ prettyProof b)
        _      -> "  wrong number of daughters somehow"


{- Outputting latex -}

prettyOp :: Op -> Doc
prettyOp = \case
  BA  -> "\\comb{<}"
  FA  -> "\\comb{>}"
  PM  -> "\\comb{\\&}"
  FC  -> "\\comb{\\circ}"
  ML  -> "\\comb{L},"
  MR  -> "\\comb{R},"
  UL  -> "\\comb{UL},"
  UR  -> "\\comb{UR},"
  A   -> "\\comb{A},"
  J   -> "\\comb{J},"
  Eps -> "\\comb{Eps},"
  D   -> "\\comb{D},"

prettyMode :: Mode -> Doc
prettyMode [] = empty
prettyMode (x:xs) = prettyOp x <+> prettyMode xs

prettyVal :: Bool -> Sem -> Doc
prettyVal norm v
  | norm      = text $ show_hs (eval (semTerm v)) 100
  | otherwise = text $ show v


-- Proofs displayed as trees with normalization controlled by `norm`
-- Requires package forest with a command \comb{} defined to format modes
prettyProofTree :: Bool -> Proof -> Doc
prettyProofTree norm proof =
  "\\begin{forest}" $+$
  "for tree={parent anchor=south, child anchor=north, align=center}" $+$
  forest proof $+$
  "\\end{forest}"

  where
    forest = \case
      Proof word v@(Lex w) ty _ ->
        "[" <>
        "$" <> label v "\\texttt{" <>
        prettyTy arrow (cat2type ty) <> "}$" <>
        "\\\\" $+$
        "\\comb{Lex}" $+$
        brackets ("\\texttt{" <> text (show w) <> "}") <>
        "]"

      Proof phrase v@(Comb op _ _) ty [l, r] ->
        "[" <>
        "$" <> label v "\\texttt{" <>
        prettyTy arrow (cat2type ty) <> "}$" <>
        "\\\\" $+$
        braces (prettyMode op) $+$
        forest l $+$ forest r $+$
        "]"

      _ -> "[[wrong] [[number] [[of] [daughters]]]]"

    label v
      | norm = ("\\texttt{" <> prettyVal norm v <> "}:" <+>)
      | otherwise = id

-- Proofs displayed as proofs with some normalization
-- Requires package bussproofs with \EnableBpAbbreviations
prettyProofBuss :: Proof -> Doc
prettyProofBuss proof = "\\begin{prooftree}" $+$ bp proof $+$ "\\end{prooftree}"
  where
    bp = \case
      Proof word v@(Lex w) ty _ ->
        "\\AXC{$\\mathstrut\\text{" <> text w <> "}" <>
        "\\vdash " <>
        "\\texttt{" <> prettyVal True v <> "}" <> ":" <+>
        "\\texttt{" <> prettyTy arrow (cat2type ty) <> "}$}"

      Proof phrase v@(Comb op _ _) ty [l, r] ->
        bp l $+$
        bp r $+$
        "\\RightLabel{\\tiny " <> prettyMode op <> "}" $+$
        "\\BIC{$\\mathstrut\\text{" <> text phrase <> "}" <+>
        "\\vdash" <+>
        "\\texttt{" <> prettyVal True v <> "}:" <+>
        "\\texttt{" <> prettyTy arrow (cat2type ty) <> "}$}"

      _ -> "\\AXC{wrong number of daughters}"

showProof :: (Proof -> Doc) -> Proof -> String
showProof disp = show . (<> "\n\n") . disp

showParse' :: (Proof -> Bool) -> (Proof -> Doc) -> Phrase -> [String]
showParse' p disp input = go $ parse input
  where
    go = map (showProof disp) . filter p

showParse = showParse' (const True) prettyProof
showParseTree' norm p = showParse' p (prettyProofTree norm)
showParseTree = showParse' (const True) (prettyProofTree False)
showParseBuss' p = showParse' p prettyProofBuss
showParseBuss = showParse' (const True) prettyProofBuss

standaloneTrees trees =
  "\\documentclass{article}" $+$
  "\\usepackage{forest}" $+$
  "\\usepackage[margin=1in]{geometry}" $+$
  "\\newcommand{\\comb}[1]{\\textbf{\\textsf{#1}}}" $+$
  "\\usepackage[T1]{fontenc}" $+$
  "\\begin{document}\\scriptsize\n" $+$
  text (if null trees then "Nothing doing" else (intercalate "\n\n" trees)) $+$
  "\n\\end{document}"

typeTrees', denTrees', outTrees' :: (Proof -> Bool) -> Phrase -> IO ()
typeTrees' p ws = mapM_ print $ showParseTree' False p ws
denTrees'  p ws = mapM_ print $ showParseTree' True  p ws
outTrees' p ws =
  let path = "test/out.tex"
   in writeFile path . show . standaloneTrees $ showParseTree' False p ws

typeTrees = typeTrees' (const True)
denTrees  = denTrees'  (const True)
outTrees  = outTrees'  (const True)
