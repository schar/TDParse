module Main where

import Prelude
import Data.Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import TDParseCFG
import TDParseLex
import TDPretty
import Utils

import Data.Foldable (or)
import Data.List (fromFoldable) as List
import Data.Filterable (partitionMap)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (stripPrefix) as SCU
import Effect (Effect)
import Effect.Class (liftEffect)
import FileInput (getFileFromEvent, readFileAsText)
import Flame as F
import Flame.Types (Html, Key)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Lexicon.Demo (demoLex)
import Lexicon.Dyn (dynLex)
import Lexicon.Indef (indefLex)
import Lexicon.Pro (proLex)
import Lexicon.Pure (pureLex)
import Lexicon.Push (pushLex)
import Lexicon.Quant (quantLex)
import TDDemo (demoCFG) as Demo
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event)
import Web.File.File as File

type Lex = Array Word
-- data LexName = PureLex | ProLex | DynLex | IndefLex | QuantLex | PushLex | DemoLex
data LexName = LexID String
derive instance Eq LexName
lexID (LexID s) = s
data CombName = MLComb | MRComb | ULComb | URComb | ZComb | AComb | JComb |
                EpsComb | ELComb | ERComb | DComb
derive instance Eq CombName
binsInventory =
  [ (MLComb  ^ addML    )
  , (MRComb  ^ addMR    )
  , (ULComb  ^ addUL    )
  , (URComb  ^ addUR    )
  , (ZComb   ^ addZ     )
  , (AComb   ^ addA     )
  , (EpsComb ^ addEps   )
  , (ELComb  ^ addEL    )
  , (ERComb  ^ addER    )
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
  , lexInventory :: Array (LexName ^ Lex)
  , opts :: { showOpts :: Boolean
            , showDens :: Boolean
            , showParams :: Boolean
            , showLex :: Boolean
            , islands :: Boolean
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
  | ToggleIslands
  | AddLex (Key ^ String)
  | LexChoice LexName
  | CombChoice CombName
  | UploadLex Event
  | BatchLex String String

-- | Initial state of the app
init :: Model
init =
  { currentPhrase: ""
  , typeOfInterest: const true
  , currentProofs: Just []
  , customLex: []
  , lexFeedback: Nothing
  , lexInventory:
      [ (LexID "pure"  ^ fromFoldable pureLex  )
      , (LexID "pro"   ^ fromFoldable proLex   )
      , (LexID "indef" ^ fromFoldable indefLex )
      , (LexID "quant" ^ fromFoldable quantLex )
      , (LexID "push"  ^ fromFoldable pushLex  )
      , (LexID "dyn"   ^ fromFoldable dynLex   )
      , (LexID "demo"  ^ fromFoldable demoLex  )
      ]
  , opts: { showOpts: true, showDens: true, showParams: false, showLex: true, islands: false
          , lexItems: (_ `elem` [LexID "pure"])
          , combs: (_ `elem` defCombs)
          }
  }

defCombs = [MRComb, MLComb, AComb, JComb]

-- proofs :: Lexicon -> String -> Maybe (Array Proof)
proofs l isles bins uns s = fromFoldable <$> prove Demo.demoCFG l isles bins uns s
  -- where bins = List.fromFoldable [addML , addMR , addUR , addUL , addEps]
  --       uns  = List.fromFoldable [addD , addJ , pure ]

buildLex :: Model -> Lex
buildLex m = concat $
  m.customLex : map (\(l ^ lex) -> if m.opts.lexItems l then lex else []) m.lexInventory

buildBins m = binsInventory >>= \(c ^ comb) -> if m.opts.combs c then [comb] else []
buildUns  m = unsInventory  >>= \(c ^ comb) -> if m.opts.combs c then [comb] else []

-- | `update` is called to handle events
update :: F.Update Model Message
update model = case _ of
  PhraseInput ("Enter" ^ s) -> F.noMessages $
    model { currentPhrase = "\"" <> s <> "\""
          , currentProofs = proofs
              (List.fromFoldable $ buildLex model)
              (List.fromFoldable $ if model.opts.islands then [CP] else [])
              (List.fromFoldable $ buildBins model)
              (List.fromFoldable $ buildUns model)
              s
          }

  PhraseInput (_ ^ s) -> F.noMessages $
    model

  TypeInput (_ ^ t) -> F.noMessages $
    case tyParse t of
      Left _   -> model { typeOfInterest = const true }
      Right ty -> model { typeOfInterest = \p -> or $ map hasType ty <@> p }

  ToggleLex -> F.noMessages $
    model { opts = model.opts { showLex = not model.opts.showLex } }

  ToggleDen -> F.noMessages $
    model { opts = model.opts { showDens = not model.opts.showDens } }

  ToggleParams -> F.noMessages $ 
    model { opts = model.opts { showParams = not model.opts.showParams } }

  ToggleOpts -> F.noMessages $
    model { opts = model.opts { showOpts = not model.opts.showOpts } }

  ToggleIslands -> F.noMessages $
    model { opts = model.opts { islands = not model.opts.islands } }

  AddLex ("Enter" ^ s) -> F.noMessages $
    case lexParse s of
      Left e   -> model { lexFeedback = Just e }
      Right l  -> model { lexFeedback = Nothing, customLex = l : model.customLex }

  AddLex (_ ^ s) -> F.noMessages $
    model

  LexChoice n -> F.noMessages $
    model { opts = model.opts { lexItems = switch n model.opts.lexItems } }
    where switch n items = \l -> (if l == n then not else identity) (items l)

  CombChoice n -> F.noMessages $
    model { opts = model.opts { combs = switch n model.opts.combs } }
    where switch n items = \c -> (if c == n then not else identity) (items c)

  UploadLex event -> model ^ 
    [ do
        file <- liftEffect $ getFileFromEvent event
        content <- readFileAsText file
        pure $ Just $ BatchLex content (File.name file)
    ]

  BatchLex content name ->
    if null left
      then model { lexInventory = newLI, lexFeedback = feedback } ^ [ pure $ Just $ LexChoice (LexID name) ]
      else model { lexFeedback = feedback } ^ []
    where
      lines = split (Pattern "\n") content
      lexEntry s = s /= "" && (SCU.stripPrefix (Pattern "#") s == Nothing)
      {left, right} = partitionMap lexParse (filter lexEntry lines)
      newLI = (LexID name ^ right) : model.lexInventory
      feedback = if null left
        then Just $ "Added " <> show (length right) <> " items"
        else Just $ "Error: " <> fromMaybe "" (head left) 


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

    , HE.p [HA.id "current"]
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

    , HE.div [HA.id "content"]

      [ HE.div [HA.id "parses"] $
          fromMaybe [HE.text "No parse"] $
            model.currentProofs <#>
              (filter model.typeOfInterest >>> take 100 >>> mapWithIndex (displayProof model.opts.showDens model.opts.showParams))

      , HE.div [HA.id "lexicon", HA.style {display: if model.opts.showLex then "block" else "none"}] $
        [ addLexText (fromMaybe "" model.lexFeedback)
        , addLexInput
        ]
        <> map (displayLexItem model.opts.showParams) (buildLex model)

      , HE.div [HA.id "options", HA.style {display: if model.opts.showOpts then "block" else "none"}]

        [ HE.div [HA.id "denInput", HA.class' "opt-group"]
          [ HE.div_
            [ HE.input [HA.class' "opt-switch", HA.type' "checkbox", HA.checked true, HA.onClick ToggleDen]
            , HE.span_ [HE.text "show meanings"]
            ]
          , HE.div_
            [ HE.input [HA.class' "opt-switch", HA.type' "checkbox", HA.checked false, HA.onClick ToggleParams]
            , HE.span_ [HE.text "show full types"]
            ]
          , HE.div_
            [ HE.input [HA.class' "opt-switch", HA.type' "checkbox", HA.checked false, HA.onClick ToggleIslands]
            , HE.span_ [HE.text "islands"]
            ]
          ]

        , HE.div [HA.id "lexInventory", HA.class' "opt-group"] $
          [ HE.text "Select fragments:" ]
          <> map (\(l ^ _) -> addSwitch LexChoice model.opts.lexItems ([HE.text (lexID l)] ^ l)) model.lexInventory
          <> [addLexFile]

        , HE.div [HA.id "combsInventory", HA.class' "opt-group"] $
          [ HE.text "Select combinators:" ]
          <> map (addSwitch CombChoice (_ `elem` defCombs))
          [ ([ HE.span [HA.class' "mode"] [displayOp MRComb], HE.text " (map right)"  ]  ^ MRComb )
          , ([ HE.span [HA.class' "mode"] [displayOp MLComb], HE.text " (map left)" ]  ^ MLComb )
          , ([ HE.span [HA.class' "mode"] [displayOp URComb], HE.text " (unit right)" ]  ^ URComb )
          , ([ HE.span [HA.class' "mode"] [displayOp ULComb], HE.text " (unit left)"  ]  ^ ULComb )
          -- , ([HE.strong_ [HE.text "Z"], HE.text " (binding)"    ]  ^ ZComb  )
          , ([ HE.span [HA.class' "mode"] [displayOp AComb], HE.text " (apply)"      ]  ^ AComb  )
          , ([ HE.span [HA.class' "mode"] [displayOp EpsComb], HE.text " (counit)"     ]  ^ EpsComb)
          , ([ HE.span [HA.class' "mode"] [displayOp ERComb], HE.text " (eject right)"]  ^ ERComb )
          , ([ HE.span [HA.class' "mode"] [displayOp ELComb], HE.text " (eject left)" ]  ^ ELComb )
          , ([ HE.span [HA.class' "mode"] [displayOp JComb], HE.text " (join)"       ]  ^ JComb  )
          , ([ HE.span [HA.class' "mode"] [displayOp DComb], HE.text " (lower)"      ]  ^ DComb  )
          ]
        ]
      ]
    ]

addSwitch action toggle (s ^ l) =
  HE.div_ 
    [ HE.input [HA.class' "opt-switch", HA.type' "checkbox", HA.checked (toggle l), HA.onClick $ action l]
    , HE.span_ s
    ]

addLexFile =
  HE.div [HA.style {marginTop: "1em"}]
    [ HE.label [HA.for "lexFileInput"]
      [ HE.text "Upload fragment" ]
    , HE.input
      [ HA.type' "file"
      , HA.id "lexFileInput"
      , HA.accept ".txt"
      , HA.onChange' UploadLex
      ]
    ]

addLexText m =
  HE.p [HA.style {marginBottom: "0px"}]
    [ HE.text "Add item: ", HE.span [HA.id "lexFeedback"] [HE.text m] ]
addLexInput =
  HE.input
    [ HA.type' "text", HA.id "lexname", HA.placeholder "(name, cat, type (, den)?)"
    , HA.onKeyup AddLex ]

displayLexItem :: forall m. Boolean -> Word -> Html m
displayLexItem b (s ^ w) = let item = fromFoldable w in
  HE.div [HA.class' "lexitem"] $ case length item of
    1 ->
      [ HE.span_ [HE.text (s <> "  ")]
      , HE.div_ $ map (\(_^_^ty) -> displayTy b ty) item
      ]
    _ ->
      [ HE.span [HA.style {marginRight: "20px", minWidth: "61px"}] [HE.text (s <> "  ")]
      , HE.div_ $ item <#> \(_^_^ty) ->
          HE.ul [HA.style {paddingLeft: "0px", marginBottom: "0px"}]
            [HE.li [HA.style {marginBottom: "0px"}] [displayTy b ty]]
      ]

displayOp :: forall m. CombName -> Html m
displayOp = case _ of
  MRComb  -> mkDir true "F"
  MLComb  -> mkDir false "F"
  URComb  -> mkDir true "U"
  ULComb  -> mkDir false "U"
  ZComb   -> mkText "Z"
  AComb   -> mkText "A"
  JComb   -> mkText "J"
  EpsComb -> mkText "C"
  DComb   -> mkText "D"
  ERComb  -> mkDir true "E"
  ELComb  -> mkDir false "E"
  where
    mkDir p o =
      HE.span [HA.class' "mode-op"]
      [ HE.text o
      , HE.span [HA.class' "mode-dir"]
        [HE.text if p then "→" else "←"]
      ]
    mkText o = HE.span [ HA.class' "mode-op" ] [ HE.text o ]
      
  

-- | Mount the application on the given selector
main :: Effect Unit
main = F.mount_ (QuerySelector "#home")
       { model: init
       , subscribe: []
       , update
       , view
       }
