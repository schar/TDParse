module TDParseTy where

import Data.Either
import Data.Tuple
import Parsing
import Parsing.Combinators
import Parsing.Expr
import Prelude hiding (between)
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

effCons = choice [ mkOp "C" (effC T T), mkOp "S" effS, mkOp "W" (effW E), mkOp "R" (effR E) ]
atom = choice [ mkOp "e" E, mkOp "E" E, mkOp "t" T, mkOp "T" T ]

mkOp name op = symbol name *> pure op
binary name op assoc = Infix (mkOp name op) assoc
prefix  p = Prefix <<< chainl1 p $ pure (<<<)

table = [ [ prefix effCons ]
        , [ binary "->" Arr AssocRight ]
        ]

tyExp p = buildExprParser table (atom <|> parens p <|> fail "Unrecognized type")
tyParser = whiteSpace *> fix tyExp

tyParse t = runParser t tyParser

cats :: Array (Tuple String Cat)
cats = map (\c -> Tuple (show c) c) $ enumFromTo bottom top

catParser = choice $ map (\(Tuple s c) -> symbol s $> c) cats

lexParser = parens do
  s <- identifier
  void comma
  c <- catParser <|> fail "Unrecognized category"
  void comma
  t <- tyParser <|> fail "Unrecognized type"
  pure $ Tuple s (pure $ Tuple s (Tuple c t))

lexParse w = case runParser w lexParser of
  Left e  -> Left (parseErrorMessage e)
  Right a -> Right a
