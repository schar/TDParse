module TDParseLex where

import Prelude hiding (between)
import Control.Apply (lift2)
import Control.Lazy (defer, fix)
import Data.Array (fromFoldable, many)
import Data.String.CodePoints (splitAt, length)
import Data.String.CodeUnits (singleton, fromCharArray)
import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Control.Alternative (guard)

import TDParseCFG
import LambdaCalc (make_var, make_con, (!), (%))

import Parsing (Parser, fail, runParser, parseErrorMessage)
import Parsing.Combinators (chainl1, option, try, choice, (<|>))
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Language (haskellStyle)
import Parsing.String (char)
import Parsing.String.Basic (alphaNum, letter)
import Parsing.Token (makeTokenParser)



-- basic lexing/identifier/whitespace rules follow haskell conventions
tokenParser = makeTokenParser haskellStyle
parens      = tokenParser.parens
symbol      = tokenParser.symbol
whiteSpace  = tokenParser.whiteSpace
identifier  = tokenParser.identifier
comma       = tokenParser.comma
dot         = tokenParser.dot
lexeme      = tokenParser.lexeme

mkOp name op = symbol name *> pure op
binary name op assoc = Infix (mkOp name op) assoc
prefix  p = Prefix <<< chainl1 p $ pure (<<<)

ats = fromFoldable atomicTypes

{- Nondeterministic type parser for filtering against partially specified types -}

effCons = choice [ mkOp "C" $ \ts -> lift2 effC ats ats <*> ts
                 , mkOp "D" $ \ts -> pure (effD G G) <*> ts
                 , mkOp "S" $ \ts -> pure effS <*> ts
                 , mkOp "W" $ \ts -> map effW ats <*> ts
                 , mkOp "R" $ \ts -> map effR ats <*> ts
                 ]
atom = choice [ mkOp "e" $ pure E
              , mkOp "E" $ pure E
              , mkOp "t" $ pure T
              , mkOp "T" $ pure T
              , mkOp "g" $ pure G
              , mkOp "G" $ pure G
              ]

table = [ [ prefix effCons ]
        , [ binary "->" (lift2 Arr) AssocRight ]
        ]

tyExp p = buildExprParser table (atom <|> parens p <|> fail "Unrecognized type")
tyParser = whiteSpace *> fix tyExp

tyParse t = runParser t tyParser


{- Deterministic type parser for specifying lexical items -}
-- TODO: allow specification of the Effect indices, rather than making these aribitrary choices

effConsD = choice [ mkOp "C" (effC T T) , mkOp "D" (effD G G) , mkOp "S" effS , mkOp "W" (effW E) , mkOp "R" (effR E) ]
atomD = choice [ mkOp "e" E , mkOp "E" E , mkOp "t" T , mkOp "T" T ]

tableD = [ [ prefix effConsD ]
         , [ binary "->" Arr AssocRight ]
         ]

tyExpD p = buildExprParser tableD (atomD <|> parens p <|> fail "Unrecognized type")
tyParserD = whiteSpace *> fix tyExpD

tyParseD t = runParser t tyParserD


{- Parser for lexical categories -}

cats :: Array (Tuple String Cat)
cats = map (\c -> Tuple (show c) c) $ enumFromTo bottom top

catParser = choice $ map (\(Tuple s c) -> symbol s $> c) cats


{- Parser for user-specified lambda terms -}

varParser = lexeme $ try do
  c <- letter
  cs <- many (alphaNum <|> char '_')
  pure <<< make_var $ singleton c <> fromCharArray cs

conParser = lexeme $ try do
  n <- identifier
  let {before, after} = splitAt (length n - 1) n
  guard $ after == "\'"
  pure (make_con before)

absParser = do
  void (symbol "\\")
  v <- varParser
  void dot
  term <- appParser
  pure $ v ! term

valParser = defer $ \_ -> parens appParser <|> absParser <|> conParser <|> varParser
appParser = defer $ \_ -> chainl1 valParser (pure (%))

lamParser = appParser

lamParse w = case runParser w lamParser of 
   Left e -> Left (parseErrorMessage e)
   Right a -> Right a


{- Parser for user-specified lexical entries -}
-- format: (string, category, type)
--     or: (string, category, type, meaning)
-- in the former case, a constant is assigned as meaning

lexParser ∷ Parser String Word
lexParser = parens do
  s <- identifier
  void comma
  c <- catParser <|> fail "Unrecognized category"
  void comma
  t <- tyParserD <|> fail "Unrecognized type"
  d <- option (make_con s) $ void comma *> lamParser 
  pure $ Tuple s (Tuple d (Tuple c t) : Nil)

lexParse w = case runParser w lexParser of
  Left e  -> Left (parseErrorMessage e)
  Right a -> Right a