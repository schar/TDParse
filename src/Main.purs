-- | Counter example using side effects free updating
module Main where

import Data.Array
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Mathjax
import Prelude
import TDParseCFG
import TDParseTy
import TDPretty

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame (QuerySelector(..), Html, Key, mount_)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import TDDemo (productions, lexicon) as Demo

-- | The model represents the state of the app
type Model =
  { currentPhrase :: String
  , typeOfInterest :: Proof -> Boolean
  , currentProofs :: Maybe (Array Proof)
  , lex :: Array (String /\ Word) /\ Boolean
  }

-- | This datatype is used to signal events to `update`
data Message
  = PhraseInput (Key /\ String)
  | TypeInput (Key /\ String)
  | ToggleLex
  | AddLex (Key /\ String)

-- | Initial state of the app
init :: Model
init =
  { currentPhrase: ""
  , typeOfInterest: const true
  , currentProofs: Just []
  , lex: fromFoldable Demo.lexicon /\ false
  }

proofs :: Lexicon -> String -> Maybe (Array Proof)
proofs l s = fromFoldable <$> prove Demo.productions l s

-- | `update` is called to handle events
update :: Model -> Message -> Model /\ Array (Aff (Maybe Message))
update model = case _ of
  PhraseInput ("Enter" /\ s) ->
                  model { currentPhrase = s, currentProofs = proofs (toUnfoldable $ fst model.lex) s }
                  /\ [liftEffect $ typeset *> clearPhrase $> Nothing]

  PhraseInput (_ /\ s) ->
                  model { currentProofs = Just [] }
                  /\ []

  TypeInput (_ /\ t) ->
    case tyParse t of
      Left _   -> model { typeOfInterest = const true }
                  /\ [liftEffect $ typeset $> Nothing]
      Right ty -> model { typeOfInterest = (_ == ty) <<< getProofType }
                  /\ [liftEffect $ typeset $> Nothing]

  ToggleLex ->    model { lex = not <$> model.lex }
                  /\ [liftEffect $ typeset $> Nothing]

  AddLex ("Enter" /\ s) ->
    case lexParse s of
      Left e   -> model
                  /\ [liftEffect $ lexFeedback e $> Nothing]
      Right l  -> model { lex = (l : fst model.lex) /\ (snd model.lex) }
                  /\ [liftEffect $ typeset *> lexFeedback "" $> Nothing]

  AddLex (_ /\ s) ->
                  model
                  /\ []

-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view model =
  HE.div_
    [ HE.input
        [ HA.type' "text", HA.id "phraseInput", HA.style {width: "500px"}, HA.placeholder "Enter a sentence"
        , HA.onKeyup PhraseInput
        ]
    , HE.input
        [ HA.type' "text", HA.id "typeInput", HA.placeholder "Filter by type"
        , HA.onKeyup TypeInput
        ]
    , HE.button [HA.onClick ToggleLex, HA.style {width: "100px"}]
        [HE.text $ (if snd (model.lex) then "hide" else "show") <> " lexicon"]
    , HE.p "current" [HE.text $ "Showing parses for: " <> model.currentPhrase]
    , HE.div "content"
      [ HE.div "parses" $
          map (\p -> HE.div [HA.class' "parse", HA.style {paddingBottom: "24px"}] [HE.text p]) $
            fromMaybe (pure "No parse") $ model.currentProofs <#> (map displayProof <<< filter model.typeOfInterest)
      , HE.div [HA.id "lexicon", HA.style {visibility: if snd (model.lex) then "visible" else "collapse"}] $
          [HE.p [HA.style {marginBottom: "0px"}] [HE.text "Add item: ", HE.span' "lexFeedback"]]
          <>
          [ HE.input
            [ HA.type' "text", HA.id "lexname", HA.style {marginBottom: "2em"}, HA.placeholder "(name, cat, type)"
            , HA.onKeyup AddLex
            ]
          ]
          <>
          map (\l -> HE.p [HA.class' "lexitem"] [HE.text $ displayLexItem l]) (fst model.lex)
      ]
    ]

displayProof :: Proof -> String
displayProof p = "$$" <> showProof prettyProofBuss p <> "$$"

displayLexItem :: String /\ Word -> String
displayLexItem (s /\ w) =
  s <> ": " <> intercalate "; " (fromFoldable w <#> \(_ /\ _ /\ t) -> showTy arrow t)

-- | Mount the application on the given selector
main :: Effect Unit
main = mount_ (QuerySelector "#parser")
       { init: init /\ [],
         subscribe: []
       , update
       , view
       }
