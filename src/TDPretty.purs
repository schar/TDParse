{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TDPretty where

import Data.List
import Data.Maybe
import LambdaCalc
import Prelude
import TDParseCFG
import Text.Pretty

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Console (log)
import Flame (QuerySelector(..), Html, Key, mount_)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Text.Pretty (Doc) as PP
import Text.Pretty.String (parens, brackets, braces)

type Doc = PP.Doc String

{- Various pretty printers -}

prettyTy :: Doc -> Ty -> Doc
prettyTy a = case _ of
  E           -> text "e"
  T           -> text "t"
  (Eff f t)   -> prettyF a f <+> prettyParam a t
  (t1 :-> t2) ->
    case t1 of
      t3 :-> t4 -> parens (prettyTy a t1) <> a <> prettyTy a t2
      _         -> prettyTy a t1 <> a <> prettyTy a t2

prettyF :: Doc -> F -> Doc
prettyF a = case _ of
  S     -> text "S"
  R r   -> text "R" -- <+> prettyParam a r
  W w   -> text "W" -- <+> prettyParam a w
  C r o -> text "C" -- <+> prettyParam a r <+> prettyParam a o

prettyParam a r@(_ :-> _) = parens (prettyTy a r)
prettyParam a r@(Eff _ _) = parens (prettyTy a r)
prettyParam a r           = prettyTy a r

showTy :: Doc -> Ty -> String
showTy a = render 100 <<< prettyTy a

arrow = text " -> "

prettyMode :: Mode -> Doc
prettyMode = case _ of
  BA      -> text "$\\comb{<}$"
  FA      -> text "$\\comb{>}$"
  PM      -> text "$\\comb{PM}$"
  FC      -> text "$\\comb{FC}$"
  ML _ op -> text "$\\comb{L}$,"         <+> prettyMode op
  MR _ op -> text "$\\comb{R}$,"         <+> prettyMode op
  UL _ op -> text "$\\eta_{\\comb{L}}$," <+> prettyMode op
  UR _ op -> text "$\\eta_{\\comb{R}}$," <+> prettyMode op
  A  _ op -> text "$\\comb{A},$"         <+> prettyMode op
  J op    -> text "$\\mu$,"              <+> prettyMode op
  Eps op  -> text "$\\epsilon$,"         <+> prettyMode op
  D op    -> text "$\\downarrow$,"       <+> prettyMode op

prettyVal :: Boolean -> Sem -> Doc
prettyVal norm v
  | norm      = text $ show_hs (eval (semTerm v)) 100
  | otherwise = text $ show v


prettyProof :: Proof -> Doc
prettyProof (Proof phrase val ty daughters) =
  let details =
        text phrase <> text " :: " <>
        prettyTy arrow ty <> text " = " <> text (show (eval (semTerm val)))
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
        vcat (text "\\\\" : braces (prettyMode op) : forest l : forest r : text "]" : Nil)

      _ -> text "[[wrong] [[number] [[of] [daughters]]]]"

    label v
      | norm = \x -> text "\\texttt{" <> prettyVal norm v <> text "}:" <+> x
      | otherwise = identity

prettyProofBuss :: Proof -> Doc
prettyProofBuss proof = text "\\begin{prooftree}" <> line' <> bp proof <> line' <> text "\\end{prooftree}"
  where
    bp = case _ of
      Proof word v@(Lex w) ty _ ->
        text "\\AXC{$\\mathstrut\\text{" <> text w <> text "}" <>
        text "\\vdash " <>
        text "\\texttt{" <> prettyVal true v <> text "}" <> text ":" <+>
        text "\\texttt{" <> prettyTy arrow ty <> text "}$}"

      Proof phrase v@(Comb op _ _) ty (l:r:Nil) ->
        vcat (bp l : bp r :
          (text "\\RightLabel{\\small " <> prettyMode op <> text "}") :
          (text "\\BIC{$\\mathstrut\\text{" <> text phrase <> text "}" <+>
          text "\\vdash" <+>
          text "\\texttt{" <> prettyVal true v <> text "}:" <+>
          text "\\texttt{" <> prettyTy arrow ty <> text "}$}") :
          Nil)

      _ -> text "\\AXC{wrong number of daughters}"

prettyProofHTML :: forall m. Proof -> Html m
prettyProofHTML proof = HE.div [HA.class' "tf-tree tf-gap-sm parse" ] [HE.ul_ [ html proof ] ]
  where
    html = case _ of
      Proof word v@(Lex w) ty _ ->
        HE.li_
          [ HE.div [HA.class' "tf-nc"]
              [ HE.span [HA.class' "type"] [HE.text $ showTy arrow ty]
              , HE.br
              , HE.span [HA.class' "mode"] [HE.text $ "Lex"]
              ]
          , HE.ul [HA.class' "parse-lex"]
              [ HE.li_ [HE.span [HA.class' "type"] [HE.text $ show w]] ]
          ]

      Proof phrase v@(Comb op _ _) ty (l:r:Nil) ->
        HE.li_
          [ HE.div [HA.class' "tf-nc"]
              [ HE.span [HA.class' "type"] [HE.text $ showTy arrow ty]
              , HE.br
              , HE.span [HA.class' "mode"] [HE.text $ show op]
              ]
          , HE.ul_ [ html l, html r ]
          ]

      _ -> HE.li_ [ HE.span [HA.class' "tf-nc"] [HE.text $ "wrong number of daughters"] ]


showProof :: (Proof -> Doc) -> Proof -> String
showProof disp = render 100 <<< (_ <> text "\n") <<< disp

showParse' :: CFG -> Lexicon -> (Proof -> Boolean) -> (Proof -> Doc) -> String -> Maybe (Array String)
showParse' cfg lex p disp input = go <$> parse cfg lex input
  where
    go = toUnfoldable <<< map (showProof disp) <<< filter p <<< concatMap synsem

showParse cfg lex = showParse' cfg lex (const true) prettyProof
showParseTree' norm cfg lex p = showParse' cfg lex p (prettyProofTree norm)
showParseTree cfg lex = showParse' cfg lex (const true) (prettyProofTree false)
showParseBuss' cfg lex p = showParse' cfg lex p prettyProofBuss
showParseBuss cfg lex = showParse' cfg lex (const true) prettyProofBuss

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

