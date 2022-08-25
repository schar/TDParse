{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDPretty where

import TDParseCFG
import LambdaCalc
import Prelude
import Data.List
import Data.Maybe
import Effect ( Effect )
import Effect.Console ( log )
import Data.Foldable ( traverse_ )
import Text.Pretty ( Doc ) as PP
import Text.Pretty
import Text.Pretty.String ( parens, brackets, braces )

type Doc = PP.Doc String

{- Various pretty printers -}

prettyTy :: Doc -> Ty -> Doc
prettyTy a = case _ of
  E           -> text "e"
  T           -> text "t"
  (Eff f t)   -> prettyF a f <+> showParam a t
  (t1 :-> t2) ->
    case t1 of
      t3 :-> t4 -> parens (prettyTy a t1) <> a <> prettyTy a t2
      _         -> prettyTy a t1 <> a <> prettyTy a t2

prettyF :: Doc -> F -> Doc
prettyF a = case _ of
  S     -> text "S"
  R r   -> text "R" -- <+> showParam a r
  W w   -> text "W" -- <+> showParam a w
  C r o -> text "C" -- <+> showParam a r <+> showParam a o

showParam a r@(t1 :-> t2) = parens (prettyTy a r)
showParam a r@(Eff f t)   = parens (prettyTy a r)
showParam a r             = prettyTy a r

arrow = text " -> "

showMode :: Mode -> Doc
showMode = case _ of
  BA     -> text "$\\comb{<}$"
  FA     -> text "$\\comb{>}$"
  PM     -> text "$\\comb{PM}$"
  FC     -> text "$\\comb{FC}$"
  LL op  -> text "$\\uparrow\\comb{L}$," <+> showMode op
  LR op  -> text "$\\uparrow\\comb{R}$," <+> showMode op
  UL op  -> text "$\\eta\\comb{L}$," <+> showMode op
  UR op  -> text "$\\eta\\comb{R}$," <+> showMode op
  A op   -> text "$\\comb{A},$" <+> showMode op
  J op   -> text "$\\mu$," <+> showMode op
  Eps op -> text "$\\epsilon$," <+> showMode op
  D op   -> text "$\\downarrow$," <+> showMode op

showVal :: Boolean -> Sem -> Doc
showVal norm v
  | norm      = text $ show_hs (eval (semTerm v)) 100
  | otherwise = text $ show v


prettyProof :: Proof -> Doc
prettyProof (Proof phrase val ty daughters) =
  let details =
        text phrase <> text " :: " <>
        prettyTy arrow ty <> text "  = " <> text (show (eval (semTerm val)))
   in case daughters of -- no unary inferences
        Nil       -> text "  " <> details
        (a:b:Nil) -> text "  " <> (vcat $ details : prettyProof a : prettyProof b : Nil)
        _         -> text "  wrong number of daughters somehow"


{- Outputting latex -}

prettyProofTree :: Boolean -> Proof -> Doc
prettyProofTree norm proof =
  vcat $
    text "\\begin{forest}" :
    text "for tree={parent anchor=south, child anchor=north, align=center}" :
    forest proof :
    text "\\end{forest}" :
    Nil
  where
    forest = case _ of
      Proof word v@(Lex w) ty _ ->
        text "[" <>
        text "$" <> label v (text "\\texttt{") <>
        prettyTy arrow ty <> text "}$" <>
        vcat (text "\\\\" : text "\\comb{Lex}" :
        brackets (text "\\texttt{" <> text (show w) <> text "}") : Nil) <>
        text "]"

      Proof phrase v@(Comb op _ _) ty (l:r:Nil) ->
        text "[" <>
        text "$" <> label v (text "\\texttt{") <>
        prettyTy arrow ty <> text "}$" <>
        vcat (text "\\\\" : braces (showMode op) : forest l : forest r : text "]" : Nil)

      _ -> text "[[wrong] [[number] [[of] [daughters]]]]"

    label v
      | norm = \x -> text "\\texttt{" <> showVal norm v <> text "}:" <+> x
      | otherwise = identity

prettyProofBuss :: Proof -> Doc
prettyProofBuss proof = text "\\begin{prooftree}" <> line' <> bp proof <> line' <> text "\\end{prooftree}"
  where
    bp = case _ of
      Proof word v@(Lex w) ty _ ->
        text "\\AXC{$\\mathstrut\\text{" <> text w <> text "}" <>
        text "\\vdash " <>
        text "\\texttt{" <> showVal true v <> text "}" <> text ":" <+>
        text "\\texttt{" <> prettyTy arrow ty <> text "}$}"

      Proof phrase v@(Comb op _ _) ty (l:r:Nil) ->
        vcat (bp l : bp r :
          (text "\\RightLabel{\\small " <> showMode op <> text "}") :
          (text "\\BIC{$\\mathstrut\\text{" <> text phrase <> text "}" <+>
          text "\\vdash" <+>
          text "\\texttt{" <> showVal true v <> text "}:" <+>
          text "\\texttt{" <> prettyTy arrow ty <> text "}$}") :
          Nil)

      _ -> text "\\AXC{wrong number of daughters}"

showProof :: (Proof -> Doc) -> Proof -> String
showProof disp = render 100 <<< (_ <> text "\n") <<< disp

prettyParse' :: CFG -> Lexicon -> (Proof -> Boolean) -> (Proof -> Doc) -> String -> Maybe (Array String)
prettyParse' cfg lex p disp input = go <$> parse cfg lex input
  where
    go = toUnfoldable <<< map (showProof disp) <<< filter p <<< concatMap synsem

prettyParse cfg lex = prettyParse' cfg lex (const true) prettyProof
prettyParseTree' norm cfg lex p = prettyParse' cfg lex p (prettyProofTree norm)
prettyParseTree cfg lex = prettyParse' cfg lex (const true) (prettyProofTree false)
prettyParseBuss' cfg lex p = prettyParse' cfg lex p prettyProofBuss
prettyParseBuss cfg lex = prettyParse' cfg lex (const true) prettyProofBuss

-- -- Proofs with some normalization
-- -- Requires bussproofs with \EnableBpAbbreviations, \strut for vert spacing
-- semProofs' :: (Proof -> Boolean) -> List Syn -> List String
-- semProofs' p = map (render 100) <<< punctuate (text "\n") <<< map prettyProofBuss <<< filter p <<< concatMap synsem
-- -- Semantic trees
-- -- Requires forest
-- semTrees' :: Boolean -> List Syn -> List Doc
-- semTrees' norm = map (prettyProofTree norm) <<< concatMap synsem

-- semTrees :: List Syn -> Effect Unit
-- semTrees = traverse_ (log <<< render 100) <<< punctuate (text "\n") <<< semTrees' false
-- denTrees :: List Syn  -> Effect Unit
-- denTrees = traverse_ (log <<< render 100) <<< punctuate (text "\n") <<< semTrees' true


-- semProofs :: List Syn -> List String
-- semProofs = semProofs' (const true)

-- semProofsDisplay :: List Syn -> Effect Unit
-- semProofsDisplay = traverse_ log <<< semProofs

-- prettyParse' :: CFG -> Lexicon -> (Proof -> Boolean) -> String -> Maybe (List Doc)
-- prettyParse' cfg lex p input = process <$> parse cfg lex input
--   where
--     process =  map (\x -> text "\n" <+> prettyProof x) <<< filter p <<< concatMap synsem

