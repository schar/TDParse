-- | Counter example using side effects free updating
module Main where

import Data.Array
import Data.Either
import Data.Maybe
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
import TDDemo (productions, lexicon)
import Web.DOM.Document (doctype)
import Web.HTML.Event.EventTypes (offline)

-- | The model represents the state of the app
type Model =
  { currentPhrase :: String,
    typeOfInterest :: Proof -> Boolean
  , currentProofs :: Maybe (Array Proof)
  }

-- | This datatype is used to signal events to `update`
data Message
  = PhraseInput (Key /\ String)
  | TypeInput (Key /\ String)

-- | Initial state of the app
init :: Model
init =
  { currentPhrase: "",
    typeOfInterest: const true
  , currentProofs: Just []
  }

-- derive :: String -> Maybe (Array Proof)

proofs :: String -> Maybe (Array Proof)
proofs s = fromFoldable <$> prove productions lexicon s

displayProof :: Proof -> String
displayProof p = "$$" <> showProof prettyProofBuss p <> "$$"

-- | `update` is called to handle events
update :: Model -> Message -> Model /\ Array (Aff (Maybe Message))
update model = case _ of
  PhraseInput ("Enter" /\ s) -> model { currentPhrase = s, currentProofs = proofs s }
                                /\ [ liftEffect $ typeset $> Nothing,
                                     liftEffect $ clearPhrase $> Nothing
                                   ]

  PhraseInput (_       /\ s) -> model { currentProofs = Just [] }
                                /\ []

  TypeInput (_       /\ t) ->
    case tyParse t of
      Left _   -> model { typeOfInterest = const true }
                  /\ [liftEffect $ typeset >>= \_ -> pure Nothing]
      Right ty -> model { typeOfInterest = (_ == ty) <<< getProofType }
                  /\ [liftEffect $ typeset >>= \_ -> pure Nothing]

-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view model =
  HE.div_
    [ HE.input [HA.type' "text", HA.id "phraseInput", HA.placeholder "Enter a sentence", HA.onKeyup PhraseInput],
      HE.input [HA.type' "text", HA.id "typeInput", HA.placeholder "Filter by type", HA.onKeyup TypeInput]
    , HE.p "current" [HE.text $ "Showing parses for: " <> model.currentPhrase]
    , HE.div "parses" $
      map (\p -> HE.div [HA.id "parse", HA.style {paddingBottom: "24px"}] [HE.text p]) $
        fromMaybe (pure "No parse") $ model.currentProofs <#> (map displayProof <<< filter model.typeOfInterest)
    ]

-- | Mount the application on the given selector
main :: Effect Unit
main = mount_ (QuerySelector "#parser")
       { init: init /\ [],
         subscribe: []
       , update
       , view
       }
