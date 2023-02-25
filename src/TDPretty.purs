
module TDPretty where

import Data.List hiding (null)
import Data.Maybe
import LambdaCalc
import Prelude
import TDParseCFG
import Text.Pretty
import Effect.Exception.Unsafe (unsafeThrow)

import Data.Foldable (sequence_, traverse_, null)
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
  G           -> text "g"
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
  D i j -> text "D" -- <+> prettyParam a i <+> prettyParam a j
  U     -> text "_"

prettyVal :: Boolean -> (Term -> String) -> Sem -> Doc
prettyVal norm disp v
  | norm      = text $ disp (evalFinal (semTerm v))
  | otherwise = text $ show v

-- this is painfully duplicative
displayTy :: forall m. Boolean -> Ty -> Html m
displayTy b ty = HE.span [HA.class' "type"] $ go ty
  where
    go = case _ of
      E           -> [HE.span [HA.class' "atom"] [HE.text "e"]]
      T           -> [HE.span [HA.class' "atom"] [HE.text "t"]]
      G           -> [HE.span [HA.class' "atom"] [HE.text "g"]]
      (Eff f t)   -> [displayF b f] <> [HE.text $ if b then "" else ""] <>  displayParam t
      (t1 :-> t2) -> displayLeft t1 (go t1) <> ar <> go t2

    displayF b f =
      HE.span [HA.class' "constructor"] $
        if not b then [HE.text $ showNoIndices f] else
          case f of
            S -> [HE.text "S"]
            R i -> [HE.text "R"] <> displayParam i
            W i -> [HE.text "W"] <> displayParam i
            C i j -> [HE.text "C"] <> displayParam i <> displayParam j
            D i j -> [HE.text "D"] <> displayParam i <> displayParam j
            U -> [HE.text "U"]

    displayParam = case _ of
      r@(_ :-> _) -> parens (go r)
      r@(Eff _ _) -> parens (go r)
      r           ->         go r

    displayLeft = case _ of
      _ :-> _ -> parens
      _       -> identity

    parens s =
      [HE.span [HA.class' "ty-punct"] [HE.text "("]]
      <> s
      <> [HE.span [HA.class' "ty-punct"] [HE.text ")"]]

    ar =
      [HE.span [HA.class' "ty-punct"] [HE.text $ render 100 arrow]]


displayVal :: forall m. Sem -> Html m
displayVal v = HE.span [HA.class' "den"] $ displayTerm (evalFinal (semTerm v)) 100

displayTerm :: forall m. Term -> Int -> Array (Html m)
displayTerm _    depth | depth <= 0 = [ HE.text "..." ]
displayTerm term depth              = go term
  where
    go = case _ of
      (Con c) ->
        [ HE.text c ]
      (Var v) ->
        [ HE.text (showVar v) ]
      (Lam v body) ->
        [ HE.span [HA.class' "den-punct"] [HE.text "λ"] ]
        <> [ HE.text (showVar v) ]
        <> [ HE.span [HA.class' "den-punct"] [HE.text ". "] ]
        <> go' body
      (App t1 t2) ->
        displayLeft go' t1
        <> [ HE.text " " ]
        <> displayRight go' t2
      (Pair t1 t2) ->
        [ HE.span [HA.class' "den-punct"] [HE.text "⟨"] ]
        <> go' t1
        <> [ HE.span [HA.class' "den-punct"] [HE.text ", "] ]
        <> go' t2
        <> [ HE.span [HA.class' "den-punct"] [HE.text "⟩"] ]
      (Fst p) ->
        [ HE.span [HA.class' "den-op"] [HE.text "fst "] ]
        <> displayRight go p
      (Snd p) ->
        [ HE.span [HA.class' "den-op"] [HE.text "snd "] ]
        <> displayRight go p
      e@(Set _ cond) ->
        let (dom ^ vars) = unrollDom eval e var_stock
            getvar (v:vs) = (Var v ^ vs)
            getvar Nil = unsafeThrow "getvar error in displayTerm"
            showNext q c = if null q then q else c <> q
            showDom t vs = let (v^rest) = getvar vs in
              case t of
                Pair a b ->
                  if a == Con "_"
                  then showDom b rest
                  else
                    go v
                    <> [ HE.span [HA.class' "den-punct"] [HE.text " <- "] ]
                    <> go' a
                    <> showNext (showDom b rest) [ HE.span [HA.class' "den-punct"] [HE.text ", "] ]
                _ ->
                  if t == Con "_"
                  then []
                  else
                    go v
                    <> [ HE.span [HA.class' "den-punct"] [HE.text " <- "] ]
                    <> go' t
            d = showDom dom vars
         in [ HE.span [HA.class' "den-punct"] [HE.text "["] ]
            <> go' (eval $ cond % (tuple $ map Var vars))
            <> (if null d then d else [ HE.span [HA.class' "den-punct"] [HE.text " | "] ] <> d)
            <> [ HE.span [HA.class' "den-punct"] [HE.text "]"] ]
      (Dom p) ->
        [ HE.span [HA.class' "den-op"] [HE.text "dom "] ]
        <> displayRight go p
      (Rng p) ->
        [ HE.span [HA.class' "den-op"] [HE.text "rng "] ]
        <> displayRight go p
      (Cct p) ->
        [ HE.span [HA.class' "den-op"] [HE.text "concat "] ]
        <> displayRight go p
      (Spl n p) ->
        [ HE.span [HA.class' "den-op"] [HE.text $ "splitAt " <> show n <> " "] ]
        <> displayRight go p
      (Push x g) ->
        [ HE.span [HA.class' "den-punct"] [HE.text "("] ]
        <> go' x
        <> [ HE.span [HA.class' "den-punct"] [HE.text ":"] ]
        <> go' g
        <> [ HE.span [HA.class' "den-punct"] [HE.text ")"] ]
      (Proj n g) ->
        go g <> [ HE.span [HA.class' "den-punct"] [HE.text $ "_" <> show n] ]


    go' term = displayTerm term (depth - 1)

    displayRight disp = case _ of
      t@(Set _ _)  -> disp t
      t@(Pair _ _) -> disp t
      t@(Var _)    -> disp t
      t@(Con _)    -> disp t
      t            -> parens (disp t)

    displayLeft disp = case _ of
      t@(Lam _ _) -> parens (disp t)
      t           -> disp t

    parens s =
      [HE.span [HA.class' "den-punct"] [HE.text "("]]
      <> s
      <> [HE.span [HA.class' "den-punct"] [HE.text ")"]]

prettyProof :: Proof -> Doc
prettyProof (Proof phrase val ty daughters) =
  let details =
        text phrase <> text " :: " <>
        prettyTy arrow ty <> text " = " <> prettyVal true show_term val
   in case daughters of -- no unary inferences
        Nil       -> text "  " <> details
        (a:b:Nil) -> text "  " <> (vcat $ details : prettyProof a : prettyProof b : Nil)
        _         -> text "  wrong number of daughters somehow"


{- Outputting latex -}

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
  Z    -> text "$\\comb{Z}$,"
  A  _ -> text "$\\comb{A},$"
  J  _ -> text "$\\comb{J}$,"
  Eps  -> text "$\\comb{Eps}$,"
  DN   -> text "$\\comb{D}$,"
  XL _ o -> text "$\\comb{XL}($" <> prettyOp o <> text "$)$,"

prettyMode :: Mode -> Doc
prettyMode Nil = mempty
prettyMode (x:xs) = prettyOp x <+> prettyMode xs

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
      | norm = \x -> text "{$" <> prettyVal norm show_tex v <> text "$}\\\\" <+> x
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

displayProof :: forall m. Boolean -> Boolean -> Int -> Proof -> Html m
displayProof dens params i proof =
  HE.div [HA.class' "tf-tree tf-gap-sm parse"]
    [ HE.span [HA.class' "parse-number"] [HE.text $ show (i + 1) <> "."]
    , HE.ul_ [ html proof ]
    ]
  where
    html = case _ of
      Proof word v@(Lex w) ty _ ->
        HE.li_
          [ HE.div [HA.class' "tf-nc"] $
            [ displayTy params ty ]
            <> (if dens then [ HE.br, displayVal v ] else [])
            <> [ HE.br ]
            <> [ HE.span [HA.class' "mode"] [HE.text $ "Lex"] ]
          , HE.ul [HA.class' "parse-lex"]
            [ HE.li_ [HE.span [HA.class' "leaf"] [HE.text $ show word]] ]
          ]

      Proof phrase v@(Comb m d) ty (l:r:Nil) ->
        HE.li_
          [ HE.div [HA.class' "tf-nc"] $
            [ displayTy params ty ]
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
    go = toUnfoldable <<< map (showProof disp) <<< filter p <<< concatMap (synsem allBins allUns)

showParse cfg lex = showParse' cfg lex (const true) prettyProof
showParseTree' norm cfg lex p = showParse' cfg lex p (prettyProofTree norm)
showParseTree cfg lex = showParse' cfg lex (const true) (prettyProofTree false)
showParseBuss' cfg lex p = showParse' cfg lex p prettyProofBuss
showParseBuss cfg lex = showParse' cfg lex (const true) prettyProofBuss
