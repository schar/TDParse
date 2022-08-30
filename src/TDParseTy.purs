module TDParseTy where

import Data.Array
import Data.Either
import Data.Tuple
import Parsing
import Parsing.Combinators
import Parsing.Expr
import Prelude hiding (between)
import Control.Apply (lift2)
import TDParseCFG

import Control.Lazy (fix)
import Data.Enum (enumFromTo)
import Parsing.Language (haskellDef)
import Parsing.Token (makeTokenParser)



-- The lexer
tokenParser = makeTokenParser haskellDef
parens      = tokenParser.parens
symbol      = tokenParser.symbol
whiteSpace  = tokenParser.whiteSpace
identifier  = tokenParser.identifier
comma       = tokenParser.comma

mkOp name op = symbol name *> pure op
binary name op assoc = Infix (mkOp name op) assoc
prefix  p = Prefix <<< chainl1 p $ pure (<<<)

ats = fromFoldable atomicTypes

{- Nondeterministic type parser for filtering against partially specified types -}

effCons = choice [ mkOp "C" $ \ts -> lift2 effC ats ats <*> ts
                 , mkOp "S" $ \ts -> pure effS <*> ts
                 , mkOp "W" $ \ts -> map effW ats <*> ts
                 , mkOp "R" $ \ts -> map effR ats <*> ts
                 ]
atom = choice [ mkOp "e" $ pure E
              , mkOp "E" $ pure E
              , mkOp "t" $ pure T
              , mkOp "T" $ pure T
              ]

table = [ [ prefix effCons ]
        , [ binary "->" (lift2 Arr) AssocRight ]
        ]

tyExp p = buildExprParser table (atom <|> parens p <|> fail "Unrecognized type")
tyParser = whiteSpace *> fix tyExp

tyParse t = runParser t tyParser


{- Deterministic type parser for specifying lexical items -}
-- TODO: allow specification of the Effect indices, rather than making these aribitrary choices

effConsD = choice [ mkOp "C" (effC T T) , mkOp "S" effS , mkOp "W" (effW E) , mkOp "R" (effR E)]
atomD = choice [ mkOp "e" E , mkOp "E" E , mkOp "t" T , mkOp "T" T]

tableD = [ [ prefix effConsD ]
         , [ binary "->" Arr AssocRight ]
         ]

tyExpD p = buildExprParser tableD (atomD <|> parens p <|> fail "Unrecognized type")
tyParserD = whiteSpace *> fix tyExpD

tyParseD t = runParser t tyParserD


cats :: Array (Tuple String Cat)
cats = map (\c -> Tuple (show c) c) $ enumFromTo bottom top

catParser = choice $ map (\(Tuple s c) -> symbol s $> c) cats

lexParser = parens do
  s <- identifier
  void comma
  c <- catParser <|> fail "Unrecognized category"
  void comma
  t <- tyParserD <|> fail "Unrecognized type"
  pure $ Tuple s (pure $ Tuple s (Tuple c t))

lexParse w = case runParser w lexParser of
  Left e  -> Left (parseErrorMessage e)
  Right a -> Right a
