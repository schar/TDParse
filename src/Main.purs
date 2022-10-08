-- | Counter example using side effects free updating
module Main where

import Data.Array
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
import Lexicon.Quant (quantLex)
import Lexicon.Push (pushLex)
import Lexicon.Demo (demoLex)
import TDDemo (demoCFG) as Demo

type Lex = Array Word
data LexName = PureLex | ProLex | IndefLex | QuantLex | PushLex | DemoLex
derive instance Eq LexName
lexInventory =
  [ (PureLex  ^ fromFoldable pureLex  )
  , (ProLex   ^ fromFoldable proLex   )
  , (IndefLex ^ fromFoldable indefLex )
  , (QuantLex ^ fromFoldable quantLex )
  , (PushLex  ^ fromFoldable pushLex  )
  , (DemoLex  ^ fromFoldable demoLex  )
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
            , showLex :: Boolean
            , lexItems :: LexName -> Boolean
            }
  }

-- | This datatype is used to signal events to `update`
data Message
  = PhraseInput (Key ^ String)
  | TypeInput (Key ^ String)
  | ToggleLex
  | ToggleDen
  | ToggleOpts
  | AddLex (Key ^ String)
  | LexChoice LexName

-- | Initial state of the app
init :: Model
init =
  { currentPhrase: ""
  , typeOfInterest: const true
  , currentProofs: Just []
  , customLex: []
  , lexFeedback: Nothing
  , opts: { showOpts: true, showDens: true, showLex: true
          , lexItems: \l -> if l `elem` [PureLex, ProLex] then true else false
          }
  }

proofs :: Lexicon -> String -> Maybe (Array Proof)
proofs l s = fromFoldable <$> prove Demo.demoCFG l s

buildLex :: Model -> Lex
buildLex m = concat $
  m.customLex : map (\(l ^ lex) -> if m.opts.lexItems l then lex else []) lexInventory

-- | `update` is called to handle events
update :: Model -> Message -> Model
update model = case _ of
  PhraseInput ("Enter" ^ s) ->
                  model { currentPhrase = "\"" <> s <> "\""
                        , currentProofs = proofs (toUnfoldable $ buildLex model) s
                        }

  PhraseInput (_ ^ s) ->
                  model

  TypeInput (_ ^ t) ->
    case tyParse t of
      Left _   -> model { typeOfInterest = const true }
      Right ty -> model { typeOfInterest = \p -> or $ map hasType ty <@> p }

  ToggleLex ->    model { opts = model.opts { showLex = not model.opts.showLex } }

  ToggleDen ->    model { opts = model.opts { showDens = not model.opts.showDens } }

  ToggleOpts ->   model { opts = model.opts { showOpts = not model.opts.showOpts } }

  AddLex ("Enter" ^ s) ->
    case lexParse s of
      Left e   -> model { lexFeedback = Just e }
      Right l  -> model { lexFeedback = Nothing, customLex = l : model.customLex }

  AddLex (_ ^ s) ->
                  model

  LexChoice n  -> model { opts = model.opts { lexItems = switch n model.opts.lexItems } }
    where switch n items = \l -> (if l == n then not else identity) (items l)


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
        [ HE.text "⚙" ]

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
              (filter model.typeOfInterest >>> take 100 >>> mapWithIndex (displayProof model.opts.showDens))

      , HE.div [HA.id "lexicon", HA.style {display: if model.opts.showLex then "block" else "none"}] $
        addLexText (fromMaybe "" model.lexFeedback) : addLexInput : map displayLexItem (buildLex model)

      , HE.div [HA.id "options", HA.style {display: if model.opts.showOpts then "block" else "none"}]

        [ HE.div [HA.id "denInput", HA.class' "opt-group"]
          [ HE.input [HA.class' "opt-switch", HA.type' "checkbox", HA.onClick ToggleDen, HA.checked true]
          , HE.span_ [HE.text "show meanings"]
          ]

        , HE.div [HA.id "lexInventory", HA.class' "opt-group"] $
          [ HE.text "Select fragments:" ]
          <> map addFragmentSwitch
          [ ("pure"  ^ PureLex  ^ true)
          , ("pro"   ^ ProLex   ^ true)
          , ("indef" ^ IndefLex ^ false)
          , ("quant" ^ QuantLex ^ false)
          , ("push"  ^ PushLex  ^ false)
          , ("demo"  ^ DemoLex  ^ false)
          ]
        ]
      ]
    ]

addFragmentSwitch (s ^ l ^ c) =
  HE.div_
    [ HE.input [HA.class' "opt-switch", HA.type' "checkbox", HA.checked c, HA.onClick $ LexChoice l]
    , HE.span_ [HE.text s]
    ]

addLexText m =
  HE.p [HA.style {marginBottom: "0px"}]
    [ HE.text "Add item: ", HE.span "lexFeedback" [HE.text m] ]
addLexInput =
  HE.input
    [ HA.type' "text", HA.id "lexname", HA.placeholder "(name, cat, type)", HA.onKeyup AddLex ]

displayLexItem :: forall m. Word -> Html m
displayLexItem (s ^ w) = let item = fromFoldable w in
  HE.div [HA.class' "lexitem"] $ case length item of
    1 ->
      [ HE.span_ [HE.text (s <> ": ")]
      , HE.div_ $ map (\(_^_^ty) -> displayTy ty) item
      ]
    _ ->
      [ HE.span [HA.style {marginRight: "20px", minWidth: "61px"}] [HE.text (s <> ": ")]
      , HE.div_ $ item <#> \(_^_^ty) ->
          HE.ul [HA.style {paddingLeft: "0px", marginBottom: "0px"}]
            [HE.li [HA.style {marginBottom: "0px"}] [displayTy ty]]
      ]

-- | Mount the application on the given selector
main :: Effect Unit
main = FAN.mount_ (QuerySelector "#home")
       { init
       , subscribe: []
       , update
       , view
       }
