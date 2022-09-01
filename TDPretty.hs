{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDPretty where

import qualified Data.Sequence as DS
import Data.List (intercalate)
import TDParseCFG hiding ((<+>), (<**>))
import Lambda_calc
import Prelude hiding ((<>), (^), and)
import Text.PrettyPrint hiding (Mode, cat)


{- Various pretty printers -}

prettyTy :: Doc -> Type -> Doc
prettyTy a = \case
  E           -> "e"
  T           -> "t"
  (Eff f t)   -> prettyF a f <+> prettyParam a t
  (t1 :-> t2) ->
    case t1 of
      t3 :-> t4 -> parens (prettyTy a t1) <> a <> prettyTy a t2
      _         -> prettyTy a t1 <> a <> prettyTy a t2

prettyF :: Doc -> F -> Doc
prettyF a = \case
  S     -> "S"
  R r   -> "R" -- <+> prettyParam a r
  W w   -> "W" -- <+> prettyParam a w
  C r o -> "K" <+> prettyParam a r <+> prettyParam a o

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
        prettyTy arrow ty <> " = " <> text (show (eval (semTerm val)))
   in case daughters of -- no unary inferences
        []     -> "  " <> details
        [a, b] -> "  " <> (details $+$ prettyProof a $+$ prettyProof b)
        _      -> "  wrong number of daughters somehow"


{- Outputting latex -}

prettyMode :: Mode -> Doc
prettyMode = \case
  BA      -> "\\comb{<}"
  FA      -> "\\comb{>}"
  PM      -> "\\comb{\\&}"
  FC      -> "\\comb{\\circ}"
  ML _ op -> "\\comb{L}," <+> prettyMode op
  MR _ op -> "\\comb{R}," <+> prettyMode op
  UL _ op -> text "$\\eta_{\\comb{L}}$," <+> prettyMode op
  UR _ op -> text "$\\eta_{\\comb{R}}$," <+> prettyMode op
  A  _ op -> "\\comb{A}," <+> prettyMode op
  J op    -> "$\\mu$," <+> prettyMode op
  -- Z op    -> "\\comb{Z}," <+> prettyMode op
  Eps op  -> "$\\epsilon$," <+> prettyMode op
  D op    -> "$\\downarrow$," <+> prettyMode op

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
        prettyTy arrow ty <> "}$" <>
        "\\\\" $+$
        "\\comb{Lex}" $+$
        brackets ("\\texttt{" <> text (show w) <> "}") <>
        "]"

      Proof phrase v@(Comb op _ _) ty [l, r] ->
        "[" <>
        "$" <> label v "\\texttt{" <>
        prettyTy arrow ty <> "}$" <>
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
        "\\texttt{" <> prettyTy arrow ty <> "}$}"

      Proof phrase v@(Comb op _ _) ty [l, r] ->
        bp l $+$
        bp r $+$
        "\\RightLabel{\\tiny " <> prettyMode op <> "}" $+$
        "\\BIC{$\\mathstrut\\text{" <> text phrase <> "}" <+>
        "\\vdash" <+>
        "\\texttt{" <> prettyVal True v <> "}:" <+>
        "\\texttt{" <> prettyTy arrow ty <> "}$}"

      _ -> "\\AXC{wrong number of daughters}"

showProof :: (Proof -> Doc) -> Proof -> String
showProof disp = show . (<> "\n\n") . disp

showParse' :: CFG -> (Proof -> Bool) -> (Proof -> Doc) -> Phrase -> [String]
showParse' cfg p disp input = go $ parse cfg input
  where
    go = map (showProof disp) . filter p . concatMap synsem

showParse cfg = showParse' cfg (const True) prettyProof
showParseTree' norm cfg p = showParse' cfg p (prettyProofTree norm)
showParseTree cfg = showParse' cfg (const True) (prettyProofTree False)
showParseBuss' cfg p = showParse' cfg p prettyProofBuss
showParseBuss cfg = showParse' cfg (const True) prettyProofBuss

standaloneTrees trees =
  "\\documentclass{article}" $+$
  "\\usepackage{forest}" $+$
  "\\usepackage[margin=1in]{geometry}" $+$
  "\\newcommand{\\comb}[1]{\\textbf{\\textsf{#1}}}" $+$
  "\\usepackage[T1]{fontenc}" $+$
  "\\begin{document}\\scriptsize\n" $+$
  text (intercalate "\n\n" trees) $+$
  "\n\\end{document}"

typeTrees', denTrees', outTrees' :: CFG -> (Proof -> Bool) -> Phrase -> IO ()
typeTrees' cfg p ws = mapM_ print $ showParseTree' False cfg p ws
denTrees'  cfg p ws = mapM_ print $ showParseTree' True  cfg p ws
outTrees' cfg p ws =
  let path = "test/out.tex"
   in writeFile path . show . standaloneTrees $ showParseTree' False cfg p ws

typeTrees cfg = typeTrees' cfg (const True)
denTrees  cfg = denTrees'  cfg (const True)
outTrees  cfg = outTrees'  cfg (const True)

showParses' :: CFG -> (Proof -> Bool) -> (Proof -> Doc) -> Phrase -> [[String]]
showParses' cfg p disp input = go <$> parse cfg input
  where
    go = map (showProof disp) . filter p . synsem

outTreesBatch' :: CFG -> (Proof -> Bool) -> Phrase -> IO ()
outTreesBatch' cfg p ws = do
  DS.traverseWithIndex go . DS.fromList $ showParses' cfg p (prettyProofTree False) ws
  return ()
  where
    go n trees =
      let path = "test/out" ++ (show n) ++ ".tex"
       in writeFile path . show . standaloneTrees $ trees
