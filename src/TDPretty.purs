
module TDPretty where

import Data.List
import Data.Maybe
import LambdaCalc
import Prelude
import TDParseCFG
import Text.Pretty

import Data.Foldable (sequence_, traverse_)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (log, logShow)
import Flame (QuerySelector(..), Html, Key, mount_)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Text.Pretty (Doc) as PP
import Text.Pretty.String (parens, brackets, braces)
import Utils ((^), type (^))

type Doc = PP.Doc String


{- Various pretty printers -}

arrow = text " -> "

prettyTy :: Doc -> Ty -> Doc
prettyTy a = case _ of
  E           -> text "e"
  T           -> text "t"
  (Eff f t)   -> prettyF a f <+> prettyParam a t
  (t1 :-> t2) ->
    case t1 of
      t3 :-> t4 -> parens (prettyTy a t1) <> a <> prettyTy a t2
      _         -> prettyTy a t1 <> a <> prettyTy a t2
  where
    prettyParam a = case _ of
      r@(_ :-> _) -> parens (prettyTy a r)
      r@(Eff _ _) -> parens (prettyTy a r)
      r           ->         prettyTy a r

prettyF :: Doc -> F -> Doc
prettyF a = case _ of
  S     -> text "S"
  R r   -> text "R" -- <+> prettyParam a r
  W w   -> text "W" -- <+> prettyParam a w
  C r o -> text "C" -- <+> prettyParam a r <+> prettyParam a o
  U     -> text "_"

-- this is painfully duplicative
displayTy :: forall m. Ty -> Html m
displayTy ty = HE.span [HA.class' "type"] $ go ty
  where
    go = case _ of
      E           -> [HE.span [HA.class' "atom"] [HE.text "e"]]
      T           -> [HE.span [HA.class' "atom"] [HE.text "t"]]
      (Eff f t)   -> [displayF f] <> {- [HE.text " "] <> -} displayParam t
      (t1 :-> t2) -> displayLeft t1 (go t1) <> ar <> go t2

    displayLeft = case _ of
      _ :-> _ -> parens
      _       -> identity

    displayParam = case _ of
      r@(_ :-> _) -> parens (go r)
      r@(Eff _ _) -> parens (go r)
      r           ->         go r

    parens s =
      [HE.span [HA.class' "ty-punct"] [HE.text "("]]
      <> s
      <> [HE.span [HA.class' "ty-punct"] [HE.text ")"]]

    ar =
      [HE.span [HA.class' "ty-punct"] [HE.text $ render 100 arrow]]

-- splitting this out in case we want to toggle the Effect indices at some point
displayF :: forall m. F -> Html m
displayF f = HE.span [HA.class' "constructor"] [HE.text $ showNoIndices f]

displayVal :: forall m. Sem -> Html m
displayVal v = HE.span [HA.class' "den"] $ displayTerm (evalFinal (semTerm v)) 100

displayTerm :: forall m. Term -> Int -> Array (Html m)
displayTerm _    depth | depth <= 0 = [ HE.text "..." ]
displayTerm term depth              = go term
  where
    go = case _ of
      (Var v) ->
        [ HE.text (show v) ]       -- show the variable regardless of depth
      (Lam v body) ->
        [ HE.span [HA.class' "den-punct"] [HE.text "λ"] ]
        <> [ HE.text (show v) ]
        <> [ HE.span [HA.class' "den-punct"] [HE.text ". "] ]
        <> go' body
      (App t1 t2) ->
        displayLeft t1 (go' t1)
        <> [ HE.text " " ]
        <> displayRight t2 (go' t2)
      (Pair t1 t2) ->
        [ HE.span [HA.class' "den-punct"] [HE.text "⟨"] ]
        <> go' t1
        <> [ HE.span [HA.class' "den-punct"] [HE.text ", "] ]
        <> go' t2
        <> [ HE.span [HA.class' "den-punct"] [HE.text "⟩"] ]
      (Fst p) ->
        [ HE.span [HA.class' "den-op"] [HE.text "fst "] ]
        <> displayRight p (go p)
      (Snd p) ->
        [ HE.span [HA.class' "den-op"] [HE.text "snd "] ]
        <> displayRight p (go p)
      e@(Set dom cond) ->
        let vars' = enough_vars dom
            occs = map (\s -> occurs e s ^ s) vars'
            vars = map (\((f^v')^s) -> Var $ if f then bump_color v' s else s) occs
            getvar vs = fromMaybe (make_var "s") (head vs) ^ fromMaybe Nil (tail vs)
            unrollDom t vs apps = let (v^rest) = getvar vs in
              case t of
                Pair t1 t2 ->
                  go v
                  <> [ HE.span [HA.class' "den-punct"] [HE.text " <- "] ]
                  <> go' (eval $ unwind t1 apps)
                  <> [ HE.span [HA.class' "den-punct"] [HE.text ", "] ]
                  <> unrollDom t2 rest (v:apps)
                _ ->
                  go v
                  <> [ HE.span [HA.class' "den-punct"] [HE.text " <- "] ]
                  <> go' (eval $ unwind t apps)
         in [ HE.span [HA.class' "den-punct"] [HE.text "["] ]
            <> go' (eval $ unwind cond vars)
            <> [ HE.span [HA.class' "den-punct"] [HE.text " | "] ]
            <> unrollDom dom vars Nil
            <> [ HE.span [HA.class' "den-punct"] [HE.text "]"] ]
      (Domain p) ->
        [ HE.span [HA.class' "den-op"] [HE.text "dom "] ]
        <> displayRight p (go p)
      (Range p) ->
        [ HE.span [HA.class' "den-op"] [HE.text "rng "] ]
        <> displayRight p (go p)

    go' term = displayTerm term (depth - 1)

    displayRight = case _ of
      (App _ _)  -> parens
      (Fst _)    -> parens
      (Snd _)    -> parens
      (Lam _ _)  -> parens
      _          -> identity

    displayLeft = case _ of
      (Lam _ _) -> parens
      _         -> identity

    parens s =
      [HE.span [HA.class' "den-punct"] [HE.text "("]]
      <> s
      <> [HE.span [HA.class' "den-punct"] [HE.text ")"]]

prettyOp :: Op -> Doc
prettyOp = case _ of
  BA   -> text "$\\comb{<}$"
  FA   -> text "$\\comb{>}$"
  PM   -> text "$\\comb{PM}$"
  FC   -> text "$\\comb{FC}$"
  ML _ -> text "$\\comb{L}$,"
  MR _ -> text "$\\comb{R}$,"
  UL _ -> text "$\\eta_{\\comb{L}}$,"
  UR _ -> text "$\\eta_{\\comb{R}}$,"
  A  _ -> text "$\\comb{A},$"
  J  _ -> text "$\\comb{J}$,"
  Eps  -> text "$\\comb{Eps}$,"
  D    -> text "$\\comb{D}$,"

prettyMode :: Mode -> Doc
prettyMode Nil = mempty
prettyMode (x:xs) = prettyOp x <+> prettyMode xs

prettyVal :: Boolean -> (Term -> String) -> Sem -> Doc
prettyVal norm disp v
  | norm      = text $ disp (evalFinal (semTerm v))
  | otherwise = text $ show v


prettyProof :: Proof -> Doc
prettyProof (Proof phrase val ty daughters) =
  let details =
        text phrase <> text " :: " <>
        prettyTy arrow ty <> text " = " <> -- text (show (eval (semTerm val)))
                                           prettyVal true show_term val
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
        text "$" <> text "\\texttt{" <>
        prettyTy arrow ty <> text "}$" <>
        vcat (text "\\\\" : label v (text "\\comb{Lex}") :
        brackets (text "\\texttt{" <> text (show word) <> text "}") : Nil) <>
        text "]"

      Proof phrase v@(Comb op d) ty (l:r:Nil) ->
        text "[" <>
        text "$" <> text "\\texttt{" <>
        prettyTy arrow ty <> text "}$" <>
        vcat (text "\\\\" : label v (braces (prettyMode op)) :
              forest l : forest r :
              text "]" : Nil)

      _ -> text "[[wrong] [[number] [[of] [daughters]]]]"

    label v
      | norm = \x -> text "$\\texttt{" <> prettyVal norm show_tex v <> text "}$\\\\" <+> x
      | otherwise = identity

prettyProofBuss :: Proof -> Doc
prettyProofBuss proof = text "\\begin{prooftree}" <> line' <> bp proof <> line' <> text "\\end{prooftree}"
  where
    bp = case _ of
      Proof word v@(Lex w) ty _ ->
        text "\\AXC{$\\mathstrut\\text{" <> text word <> text "}" <>
        text "\\vdash " <>
        text "\\texttt{" <> prettyVal true show_term v <> text "}" <> text ":" <+>
        text "\\texttt{" <> prettyTy arrow ty <> text "}$}"

      Proof phrase v@(Comb op _) ty (l:r:Nil) ->
        vcat (bp l : bp r :
          (text "\\RightLabel{\\small " <> prettyMode op <> text "}") :
          (text "\\BIC{$\\mathstrut\\text{" <> text phrase <> text "}" <+>
          text "\\vdash" <+>
          text "\\texttt{" <> prettyVal true show_term v <> text "}:" <+>
          text "\\texttt{" <> prettyTy arrow ty <> text "}$}") :
          Nil)

      _ -> text "\\AXC{wrong number of daughters}"

displayProof :: forall m. Boolean -> Int -> Proof -> Html m
displayProof dens i proof =
  HE.div [HA.class' "tf-tree tf-gap-sm parse"]
    [ HE.span [HA.class' "parse-number"] [HE.text $ show (i + 1) <> "."]
    , HE.ul_ [ html proof ]
    ]
  where
    html = case _ of
      Proof word v@(Lex w) ty _ ->
        HE.li_
          [ HE.div [HA.class' "tf-nc"] $
            [ displayTy ty ]
            <> (if dens then [ HE.br, displayVal v ] else [])
            <> [ HE.br ]
            <> [ HE.span [HA.class' "mode"] [HE.text $ "Lex"] ]
          , HE.ul [HA.class' "parse-lex"]
            [ HE.li_ [HE.span [HA.class' "leaf"] [HE.text $ show word]] ]
          ]

      Proof phrase v@(Comb m d) ty (l:r:Nil) ->
        HE.li_
          [ HE.div [HA.class' "tf-nc"] $
            [ displayTy ty ]
            <> (if dens then [ HE.br, displayVal v ] else [])
            <> [ HE.br ]
            <> [ HE.span [HA.class' "mode"] [HE.text $ showMode m] ]
          , HE.ul_ [ html l, html r ]
          ]

      _ -> HE.li_ [ HE.span [HA.class' "tf-nc"] [HE.text $ "wrong number of daughters"] ]

showMode :: Mode -> String
showMode mode = intercalate ", " (map show mode)

showTy :: Doc -> Ty -> String
showTy a = render 100 <<< prettyTy a

showProof :: (Proof -> Doc) -> Proof -> String
showProof disp = render 100 <<< (_ <> text "\n\n") <<< disp

showParse' :: CFG -> Lexicon -> (Proof -> Boolean) -> (Proof -> Doc) -> String -> Maybe (Array String)
showParse' cfg lex p disp input = go <$> parse cfg lex input
  where
    go = toUnfoldable <<< map (showProof disp) <<< filter p <<< concatMap synsem

showParse cfg lex = showParse' cfg lex (const true) prettyProof
showParseTree' norm cfg lex p = showParse' cfg lex p (prettyProofTree norm)
showParseTree cfg lex = showParse' cfg lex (const true) (prettyProofTree false)
showParseBuss' cfg lex p = showParse' cfg lex p prettyProofBuss
showParseBuss cfg lex = showParse' cfg lex (const true) prettyProofBuss
