{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDPretty where

import qualified Data.Sequence as DS
import Data.List (intercalate)
import Data.Maybe (Maybe, fromMaybe)
import TDParseCFG
import LambdaCalc (Term, eval, evalFinal, showTerm, showHask, showTex)
import Prelude hiding ((<>))
import Text.PrettyPrint hiding (Mode, cat)


{- Various pretty printers -}

arrow = " -> "

prettyTy :: Doc -> Ty -> Doc
prettyTy a = \case
  E           -> "e"
  T           -> "t"
  (Eff f t)   -> prettyF a f <+> prettyParam a t
  (t1 :-> t2) ->
    case t1 of
      t3 :-> t4 -> parens (prettyTy a t1) <> a <> prettyTy a t2
      _         -> prettyTy a t1 <> a <> prettyTy a t2
  where
    prettyParam a r@(_ :-> _) = parens (prettyTy a r)
    prettyParam a r@(Eff _ _) = parens (prettyTy a r)
    prettyParam a r           =         prettyTy a r

prettyF :: Doc -> F -> Doc
prettyF a = \case
  S     -> "S"
  R r   -> "R" -- <+> prettyParam a r
  W w   -> "W" -- <+> prettyParam a w
  C r o -> "C" -- <+> prettyParam a r <+> prettyParam a o
  U     -> "_"

prettyVal :: Bool -> (Term -> String) -> Sem -> Doc
prettyVal norm disp v
  | norm      = text $ disp (evalFinal $ semTerm v)
  | otherwise = text $ show v

prettyProof :: Proof -> Doc
prettyProof (Proof phrase val ty daughters) =
  let details =
        text phrase <> " :: " <>
        prettyTy arrow ty <> " = " <> prettyVal True showTerm val
   in case daughters of -- no unary inferences
        []     -> "  " <> details
        [a, b] -> "  " <> (details $+$ prettyProof a $+$ prettyProof b)
        _      -> "  wrong number of daughters somehow"


{- Outputting latex -}

prettyOp :: Op -> Doc
prettyOp = \case
  BA   -> "\\comb{<}"
  FA   -> "\\comb{>}"
  PM   -> "\\comb{\\&}"
  FC   -> "\\comb{\\circ}"
  ML f   -> "\\comb{L},"
  MR f   -> "\\comb{R},"
  UL f   -> "\\comb{UL},"
  UR f   -> "\\comb{UR},"
  A  f   -> "\\comb{A},"
  J  f   -> "\\comb{J},"
  Eps    -> "\\comb{Eps},"
  D      -> "\\comb{D},"
  XL f o -> "\\comb{XL}" <+> prettyOp o

prettyMode :: Mode -> Doc
prettyMode [] = empty
prettyMode (x:xs) = prettyOp x <+> prettyMode xs

-- Proofs displayed as trees with normalization controlled by `norm`
-- Requires package forest with a command \comb{} defined to format modes
-- as provided by `standaloneTrees` below
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
        "$" <> "\\texttt{" <>
        prettyTy arrow ty <> "}$" <>
        "\\\\" $+$
        label v ("\\comb{Lex}") $+$
        brackets ("\\texttt{" <> text (show word) <> "}") <>
        "]"

      Proof phrase v@(Comb op _) ty [l, r] ->
        "[" <>
        "$" <> "\\texttt{" <>
        prettyTy arrow ty <> "}$" <>
        "\\\\" $+$
        label v (braces (prettyMode op)) $+$
        forest l $+$ forest r $+$
        "]"

      _ -> "[[wrong] [[number] [[of] [daughters]]]]"

    label v
      | norm = ("{$" <> prettyVal norm showTex v <> "$}\\\\" $+$)
      | otherwise = id

-- Proofs displayed as proofs with some normalization
-- Requires package bussproofs with \EnableBpAbbreviations
prettyProofBuss :: Proof -> Doc
prettyProofBuss proof = "\\begin{prooftree}" $+$ bp proof $+$ "\\end{prooftree}"
  where
    bp = \case
      Proof word v@(Lex w) ty _ ->
        "\\AXC{$\\mathstrut\\text{" <> text word <> "}" <>
        "\\vdash " <>
        "\\texttt{" <> prettyVal True showTerm v <> "}" <> ":" <+>
        "\\texttt{" <> prettyTy arrow ty <> "}$}"

      Proof phrase v@(Comb op _) ty [l, r] ->
        bp l $+$
        bp r $+$
        "\\RightLabel{\\tiny " <> prettyMode op <> "}" $+$
        "\\BIC{$\\mathstrut\\text{" <> text phrase <> "}" <+>
        "\\vdash" <+>
        "\\texttt{" <> prettyVal True showTerm v <> "}:" <+>
        "\\texttt{" <> prettyTy arrow ty <> "}$}"

      _ -> "\\AXC{wrong number of daughters}"

showMode :: Mode -> String
showMode mode = intercalate ", " (map show mode)

showTy :: Doc -> Ty -> String
showTy a = show . prettyTy a

showProof :: (Proof -> Doc) -> Proof -> String
showProof disp = show . (<> "\n\n") . disp

showParse' :: CFG -> Lexicon -> (Proof -> Bool) -> (Proof -> Doc) -> String -> Maybe [String]
showParse' cfg lex p disp input = go <$> parse cfg lex input
  where
    go = map (showProof disp) . filter p . concatMap synsem

showParse cfg lex = showParse' cfg lex (const True) prettyProof
showParseTree' norm cfg lex p = showParse' cfg lex p (prettyProofTree norm)
showParseTree cfg lex = showParse' cfg lex (const True) (prettyProofTree False)
showParseBuss' cfg lex p = showParse' cfg lex p prettyProofBuss
showParseBuss cfg lex = showParse' cfg lex (const True) prettyProofBuss

standaloneTrees trees =
  "\\documentclass{article}" $+$
  "\\usepackage{forest}" $+$
  "\\usepackage[margin=1in]{geometry}" $+$
  "\\newcommand{\\comb}[1]{\\textbf{\\textsf{#1}}}" $+$
  "\\usepackage[T1]{fontenc}" $+$
  "\\begin{document}\\scriptsize\n" $+$
  text (if null trees then "Nothing doing" else (intercalate "\n\\bigskip\n" trees)) $+$
  "\n\\end{document}"

typeTrees', denTrees' :: CFG -> Lexicon -> (Proof -> Bool) -> String -> IO ()
typeTrees' cfg lex p ws = mapM_ print . fromMaybe ["Nothin"] $ showParseTree' False cfg lex p ws
denTrees'  cfg lex p ws = mapM_ print . fromMaybe ["Nothin"] $ showParseTree' True  cfg lex p ws
outTrees' norm cfg lex p ws =
  let path = "test/out.tex"
   in writeFile path . show . standaloneTrees . fromMaybe ["Nothin"] $ showParseTree' norm cfg lex p ws

typeTrees cfg lex = typeTrees' cfg lex (const True)
denTrees  cfg lex = denTrees'  cfg lex (const True)
outTrees  cfg lex = outTrees' False cfg lex (const True)

showParses' :: CFG -> Lexicon -> (Proof -> Bool) -> (Proof -> Doc) -> String -> Maybe [[String]]
showParses' cfg lex p disp input = fmap go <$> parse cfg lex input
  where
    go = map (showProof disp) . filter p . synsem

outTreesBatch' :: CFG -> Lexicon -> (Proof -> Bool) -> String -> IO ()
outTreesBatch' cfg lex p ws = do
  DS.traverseWithIndex go . DS.fromList . fromMaybe [["Double Nothing"]] $ showParses' cfg lex p (prettyProofTree False) ws
  return ()
  where
    go n trees =
      let path = "test/out" ++ (show n) ++ ".tex"
       in writeFile path . show . standaloneTrees $ trees

withDens = True
withoutDens = False
