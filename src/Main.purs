-- | Counter example using side effects free updating
module Main where

import Data.Array
import Data.List (fromFoldable) as List
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Flame.Types
import Prelude
import TDParseCFG
import TDParseTy
import TDPretty
import Utils

import Data.Foldable (or)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame (QuerySelector(..), Html, Key)
import Flame.Application.NoEffects as FAN
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Lexicon.Pure (pureLex)
import Lexicon.Pro (proLex)
import Lexicon.Indef (indefLex)
import Lexicon.Dyn (dynLex)
import Lexicon.Quant (quantLex)
import Lexicon.Push (pushLex)
import Lexicon.Demo (demoLex)
import TDDemo (demoCFG) as Demo

type Lex = Array Word
data LexName = PureLex | ProLex | DynLex | IndefLex | QuantLex | PushLex | DemoLex
derive instance Eq LexName
lexInventory =
  [ (ProLex   ^ fromFoldable proLex   )
  , (IndefLex ^ fromFoldable indefLex )
  , (DynLex   ^ fromFoldable dynLex   )
  , (QuantLex ^ fromFoldable quantLex )
  , (PushLex  ^ fromFoldable pushLex  )
  , (PureLex  ^ fromFoldable pureLex  )
  , (DemoLex  ^ fromFoldable demoLex  )
  ]
data CombName = MLComb | MRComb | ULComb | URComb | ZComb | AComb | EpsComb | JComb | DComb
derive instance Eq CombName
binsInventory =
  [ (MLComb  ^ addML    )
  , (MRComb  ^ addMR    )
  , (ULComb  ^ addUL    )
  , (URComb  ^ addUR    )
  , (ZComb   ^ addZ     )
  , (AComb   ^ addA     )
  , (EpsComb ^ addEps   )
  ]
unsInventory =
  [ (JComb   ^ addJ     )
  , (DComb   ^ addD     )
  ]

-- | The model represents the state of the app
type Model =
  { currentPhrase :: String
  , typeOfInterest :: Proof -> Boolean
  , currentProofs :: Maybe (Array Proof)
  , customLex :: Lex
  , lexFeedback :: Maybe String
  , opts :: { showOpts :: Boolean
            , showDens :: Boolean
            , showParams :: Boolean
            , showLex :: Boolean
            , lexItems :: LexName -> Boolean
            , combs :: CombName -> Boolean
            }
  }

-- | This datatype is used to signal events to `update`
data Message
  = PhraseInput (Key ^ String)
  | TypeInput (Key ^ String)
  | ToggleLex
  | ToggleDen
  | ToggleOpts
  | ToggleParams
  | AddLex (Key ^ String)
  | LexChoice LexName
  | CombChoice CombName

-- | Initial state of the app
init :: Model
init =
  { currentPhrase: ""
  , typeOfInterest: const true
  , currentProofs: Just []
  , customLex: []
  , lexFeedback: Nothing
  , opts: { showOpts: true, showDens: true, showParams: false, showLex: true
          , lexItems: \l -> if l `elem` defLexes then true else false
          , combs: \c -> if c `elem` defCombs then true else false
          }
  }

defLexes = [PureLex, DynLex]
defCombs = [MRComb, MLComb, AComb, JComb]

-- proofs :: Lexicon -> String -> Maybe (Array Proof)
proofs l bins uns s = fromFoldable <$> prove Demo.demoCFG l bins uns s
  -- where bins = List.fromFoldable [addML , addMR , addUR , addUL , addEps]
  --       uns  = List.fromFoldable [addD , addJ , pure ]

buildLex :: Model -> Lex
buildLex m = concat $
  m.customLex : map (\(l ^ lex) -> if m.opts.lexItems l then lex else []) lexInventory

buildBins m = binsInventory >>= \(c ^ comb) -> if m.opts.combs c then [comb] else []
buildUns  m = unsInventory  >>= \(c ^ comb) -> if m.opts.combs c then [comb] else []

-- | `update` is called to handle events
update :: Model -> Message -> Model
update model = case _ of
  PhraseInput ("Enter" ^ s) ->
                  model { currentPhrase = "\"" <> s <> "\""
                        , currentProofs = proofs (List.fromFoldable $ buildLex model) (List.fromFoldable $ buildBins model) (List.fromFoldable $ buildUns model) s
                        }

  PhraseInput (_ ^ s) ->
                  model

  TypeInput (_ ^ t) ->
    case tyParse t of
      Left _   -> model { typeOfInterest = const true }
      Right ty -> model { typeOfInterest = \p -> or $ map hasType ty <@> p }

  ToggleLex ->    model { opts = model.opts { showLex = not model.opts.showLex } }

  ToggleDen ->    model { opts = model.opts { showDens = not model.opts.showDens } }

  ToggleParams -> model { opts = model.opts { showParams = not model.opts.showParams } }

  ToggleOpts ->   model { opts = model.opts { showOpts = not model.opts.showOpts } }

  AddLex ("Enter" ^ s) ->
    case lexParse s of
      Left e   -> model { lexFeedback = Just e }
      Right l  -> model { lexFeedback = Nothing, customLex = l : model.customLex }

  AddLex (_ ^ s) ->
                  model

  LexChoice n  -> model { opts = model.opts { lexItems = switch n model.opts.lexItems } }
    where switch n items = \l -> (if l == n then not else identity) (items l)

  CombChoice n -> model { opts = model.opts { combs = switch n model.opts.combs } }
    where switch n items = \c -> (if c == n then not else identity) (items c)


-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view model =
  HE.div [HA.id "parser"]
    [ HE.input
      [ HA.type' "text", HA.id "phraseInput", HA.placeholder "Enter a sentence"
      , HA.onKeyup PhraseInput
      ]

    , HE.input
      [ HA.type' "text", HA.id "typeInput", HA.placeholder "Filter by type"
      , HA.onKeyup TypeInput
      ]

    , HE.button [HA.id "lex-button", HA.onClick ToggleLex]
        [ HE.text $ (if model.opts.showLex then "hide" else "show") <> " lexicon" ]

    , HE.button [HA.id "opts-button", HA.onClick ToggleOpts]
        [ HE.text "≡" ]

    , HE.p "current"
       [ HE.text $ "Showing "
       , HE.span [HA.style {color: "var(--accent)"}]
         [ HE.text $ show $
           min 200 $ maybe 0 (length <<< filter model.typeOfInterest) model.currentProofs ]
       , HE.text " of "
       , HE.span [HA.style {color: "var(--accent)"}]
         [ HE.text $ show $
           maybe 0 length model.currentProofs ]
       , HE.text $ " parses for: " <> model.currentPhrase
       ]

    , HE.div "content"

      [ HE.div "parses" $
          fromMaybe [HE.text "No parse"] $
            model.currentProofs <#>
              (filter model.typeOfInterest >>> take 100 >>> mapWithIndex (displayProof model.opts.showDens model.opts.showParams))

      , HE.div [HA.id "lexicon", HA.style {display: if model.opts.showLex then "block" else "none"}] $
        addLexText (fromMaybe "" model.lexFeedback) : addLexInput : map (displayLexItem model.opts.showParams) (buildLex model)

      , HE.div [HA.id "options", HA.style {display: if model.opts.showOpts then "block" else "none"}]

        [ HE.div [HA.id "denInput", HA.class' "opt-group"]
          [ HE.div_
            [ HE.input [HA.class' "opt-switch", HA.type' "checkbox", HA.checked true, HA.onClick ToggleDen]
            , HE.span_ [HE.text "show meanings"]
            ]
          , HE.div_
            [ HE.input [HA.class' "opt-switch", HA.type' "checkbox", HA.checked false, HA.onClick ToggleParams]
            , HE.span_ [HE.text "show eff params"]
            ]
          ]

        , HE.div [HA.id "lexInventory", HA.class' "opt-group"] $
          [ HE.text "Select fragments:" ]
          <> map (addSwitch LexChoice (_ `elem` defLexes))
          [ ("pure"  ^ PureLex )
          , ("pro"   ^ ProLex  )
          , ("indef" ^ IndefLex)
          , ("dyn"   ^ DynLex  )
          , ("quant" ^ QuantLex)
          , ("push"  ^ PushLex )
          , ("demo"  ^ DemoLex )
          ]

        , HE.div [HA.id "combsInventory", HA.class' "opt-group"] $
          [ HE.text "Select combinators:" ]
          <> map (addSwitch CombChoice (_ `elem` defCombs))
          [ ("R (map right)"   ^ MRComb )
          , ("L (map left)"    ^ MLComb )
          , ("UR (unit right)" ^ URComb )
          , ("UL (unit left)"  ^ ULComb )
          -- , ("Z (binding)"     ^ ZComb  )
          , ("A (apply)"       ^ AComb  )
          , ("Eps (counit)"    ^ EpsComb)
          , ("J (join)"        ^ JComb  )
          , ("D (lower)"       ^ DComb  )
          ]
        ]
      ]
    ]

addSwitch action toggle (s ^ l) =
  HE.div_
    [ HE.input [HA.class' "opt-switch", HA.type' "checkbox", HA.checked (toggle l), HA.onClick $ action l]
    , HE.span_ [HE.text s]
    ]

addLexText m =
  HE.p [HA.style {marginBottom: "0px"}]
    [ HE.text "Add item: ", HE.span "lexFeedback" [HE.text m] ]
addLexInput =
  HE.input
    [ HA.type' "text", HA.id "lexname", HA.placeholder "(name, cat, type)", HA.onKeyup AddLex ]

displayLexItem :: forall m. Boolean -> Word -> Html m
displayLexItem b (s ^ w) = let item = fromFoldable w in
  HE.div [HA.class' "lexitem"] $ case length item of
    1 ->
      [ HE.span_ [HE.text (s <> ": ")]
      , HE.div_ $ map (\(_^_^ty) -> displayTy b ty) item
      ]
    _ ->
      [ HE.span [HA.style {marginRight: "20px", minWidth: "61px"}] [HE.text (s <> ": ")]
      , HE.div_ $ item <#> \(_^_^ty) ->
          HE.ul [HA.style {paddingLeft: "0px", marginBottom: "0px"}]
            [HE.li [HA.style {marginBottom: "0px"}] [displayTy b ty]]
      ]

-- | Mount the application on the given selector
main :: Effect Unit
main = FAN.mount_ (QuerySelector "#home")
       { init
       , subscribe: []
       , update
       , view
       }
