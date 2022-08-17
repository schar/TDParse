{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDPretty where

import TDParseCFG
import Lambda_calc
import Prelude hiding ((<>), (^), and)
import Text.PrettyPrint hiding (Mode, cat)


{- Various pretty printers -}

prettyTy :: Doc -> Type -> Doc
prettyTy a = \case
  E           -> "e"
  T           -> "t"
  (Eff f t)   -> prettyF a f <+> showParam a t
  (t1 :-> t2) ->
    case t1 of
      t3 :-> t4 -> parens (prettyTy a t1) <> a <> prettyTy a t2
      _         -> prettyTy a t1 <> a <> prettyTy a t2

prettyF :: Doc -> F -> Doc
prettyF a = \case
  S     -> "S"
  R r   -> "R" -- <+> showParam a r
  W w   -> "W" -- <+> showParam a w
  C r o -> "K" <+> showParam a r <+> showParam a o

showParam a r@(t1 :-> t2) = parens (prettyTy a r)
showParam a r@(Eff f t)   = parens (prettyTy a r)
showParam a r             = prettyTy a r

arrow = " -> "

prettyProof :: Proof -> Doc
prettyProof (Proof phrase val ty daughters) =
  let details =
        text phrase <> " :: " <>
        prettyTy arrow ty <> "   = " <> text (show (eval (semTerm val)))
   in case daughters of -- no unary inferences
        []     -> "  " <> details
        [a, b] -> "  " <> (details $+$ prettyProof a $+$ prettyProof b)
        _      -> "  wrong number of daughters somehow"

prettyParse :: CFG -> Phrase -> [Doc]
prettyParse cfg s =
  map (\x -> "\n" <+> prettyProof x) (concatMap synsem $ parse cfg s)

showMode :: Mode -> Doc
showMode = \case
  BA     -> "\\comb{<}"
  FA     -> "\\comb{>}"
  PM     -> "\\comb{PM}"
  FC     -> "\\comb{FC}"
  LL op  -> "$\\uparrow$\\comb{L}," <+> showMode op
  LR op  -> "$\\uparrow$\\comb{R}," <+> showMode op
  A op   -> "\\comb{A}," <+> showMode op
  J op   -> "$\\mu$," <+> showMode op
  -- Z op   -> "\\comb{Z}," <+> showMode op
  Eps op -> "$\\epsilon$," <+> showMode op
  D op   -> "$\\downarrow$," <+> showMode op

showVal :: Bool -> Sem -> Doc
showVal norm v
  | norm      = text $ show_hs (eval (semTerm v)) 100
  | otherwise = text $ show v


{- Outputting latex -}

-- Semantic trees
-- Requires forest
semTrees' :: Bool -> [Syn] -> [Doc]
semTrees' norm = map tree . concatMap synsem
  where
    tree proof =
      "\\begin{forest}" $+$
      "for tree={parent anchor=south, child anchor=north, align=center}" $+$
      forest proof $+$
      "\\end{forest}"

    forest = \case
      Proof word v@(Lex w) ty _ ->
        "[" <>
        "$" <> label v
        "\\texttt{" <> prettyTy " -> " ty <> "}$" <>
        "\\\\" $+$
        "\\comb{Lex}" $+$
        brackets ("\\texttt{" <> text (show w) <> "}") <> "]"

      Proof phrase v@(Comp op _ _) ty [l, r] ->
        "[" <>
        "$" <> label v
        "\\texttt{" <> prettyTy " -> " ty <> "}$" <>
        "\\\\" $+$
        braces (showMode op) $+$
        forest l $+$ forest r $+$ "]"

      _ -> "[[wrong] [[number] [[of] [daughters]]]]"

    label v
      | norm = ("\\texttt{" <> showVal norm v <> "}:" <+>)
      | otherwise = id

semTrees :: [Syn] -> IO ()
semTrees = mapM_ print . punctuate "\n" . semTrees' False

-- Proofs with some normalization
-- Requires bussproofs with \EnableBpAbbreviations, \strut for vert spacing
semProofs :: [Syn] -> IO ()
semProofs = mapM_ print . punctuate "\n" . map proofTree . concatMap synsem
  where
    proofTree proof = bp proof $+$ "\\DP"

    bp = \case
      Proof word v@(Lex w) ty _ ->
        "\\AXC{\\strut$\\text{" <> text w <> "}" <> "\\vdash " <>
        "\\texttt{" <> showVal True v <> "}" <> ":" <+>
        "\\texttt{" <> prettyTy " -> " ty <> "}$}"

      Proof phrase v@(Comp op _ _) ty [l, r] ->
        bp l $+$
        bp r $+$
        "\\RightLabel{\\tiny " <> showMode op <> "}" $+$
        "\\BIC{\\strut$\\text{" <> text phrase <> "}" <+>
        "\\vdash" <+>
        "\\texttt{" <> showVal True v <> "}:" <+>
        "\\texttt{" <> prettyTy " -> " ty <> "}$}"

      _ -> "\\AXC{wrong number of daughters}"
