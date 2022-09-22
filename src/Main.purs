-- | Counter example using side effects free updating
module Main where

import Data.Array
import Data.Foldable (or)
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Flame.Types
-- import Mathjax
import Prelude
import TDParseCFG
import TDParseTy
import TDPretty
import Utils

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame (QuerySelector(..), Html, Key) 
import Flame.Application.NoEffects as FAN
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import TDDemo (demoCFG, demoLex) as Demo

-- | The model represents the state of the app
type Model =
  { currentPhrase :: String
  , typeOfInterest :: Proof -> Boolean
  , currentProofs :: Maybe (Array Proof)
  , lex :: Array Word ^ Boolean
  , lexFeedback :: Maybe String
  , densOn :: Boolean
  }

-- | This datatype is used to signal events to `update`
data Message
  = PhraseInput (Key ^ String)
  | TypeInput (Key ^ String)
  | ToggleLex
  | ToggleDen
  | AddLex (Key ^ String)

-- | Initial state of the app
init :: Model
init =
  { currentPhrase: ""
  , typeOfInterest: const true
  , currentProofs: Just []
  , lex: fromFoldable Demo.demoLex ^ false
  , lexFeedback: Nothing
  , densOn: true
  }

proofs :: Lexicon -> String -> Maybe (Array Proof)
proofs l s = fromFoldable <$> prove Demo.demoCFG l s

-- | `update` is called to handle events
update :: Model -> Message -> Model
update model = case _ of
  PhraseInput ("Enter" ^ s) ->
                  model { currentPhrase = "\"" <> s <> "\""
                        , currentProofs = proofs (toUnfoldable $ fst model.lex) s
                        }

  PhraseInput (_ ^ s) ->
                  model

  TypeInput (_ ^ t) ->
    case tyParse t of
      Left _   -> model { typeOfInterest = const true }
      Right ty -> model { typeOfInterest = \p -> or $ map hasType ty <@> p }

  ToggleLex ->    model { lex = not <$> model.lex }

  ToggleDen ->    model { densOn = not model.densOn }

  AddLex ("Enter" ^ s) ->
    case lexParse s of
      Left e   -> model { lexFeedback = Just e }
      Right l  -> model { lexFeedback = Nothing, lex = (l : fst model.lex) ^ (snd model.lex) }

  AddLex (_ ^ s) ->
                  model

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
        [ HE.text $ (if snd (model.lex) then "hide" else "show") <> " lexicon" ]

    , HE.span [HA.id "denInput"]
      [ HE.text "show meanings"
      , HE.input [HA.id "den-button", HA.type' "checkbox", HA.onClick ToggleDen, HA.checked true]
      ]

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
              (filter model.typeOfInterest >>> take 100 >>> mapWithIndex (displayProof model.densOn))

      , HE.div [HA.id "lexicon", HA.style {display: if snd (model.lex) then "block" else "none"}] $
        addLexText (fromMaybe "" model.lexFeedback) : addLexInput : map displayLexItem (fst model.lex)
      ]
    ]

addLexText m =
  HE.p [HA.style {marginBottom: "0px"}]
    [ HE.text "Add item: ", HE.span "lexFeedback" [HE.text m] ]
addLexInput =
  HE.input
    [ HA.type' "text", HA.id "lexname", HA.placeholder "(name, cat, type)", HA.onKeyup AddLex ]

displayLexItem :: forall m. Word -> Html m
displayLexItem (s ^ w) =
  HE.p [HA.class' "lexitem"] $
    HE.text (s <> ": ") :
      intersperse (HE.text "; ")
        (fromFoldable w <#> \(_^_^ty) -> displayTy ty)

-- | Mount the application on the given selector
main :: Effect Unit
main = FAN.mount_ (QuerySelector "#home")
       { init
       , subscribe: []
       , update
       , view
       }
