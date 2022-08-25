module TDParseTy where

import Prelude hiding (between)
import Parsing
import Parsing.Expr
import Parsing.Combinators
import Parsing.Language (haskellDef)
import Parsing.Token (makeTokenParser)
import Control.Lazy (fix)
import TDParseCFG



effCons = choice [ mkOp "C" (effC T T), mkOp "S" effS, mkOp "W" (effW E), mkOp "R" (effR E) ]
atom = choice [ mkOp "e" E, mkOp "E" E, mkOp "t" T, mkOp "T" T ]

mkOp name op = symbol name *> pure op
binary name op assoc = Infix (mkOp name op) assoc
prefix  p = Prefix <<< chainl1 p $ pure (<<<)

table = [ [ prefix effCons ]
        , [ binary "->" Arr AssocRight ]
        ]

exp p = buildExprParser table (atom <|> parens p)

expr = whiteSpace *> fix exp

-- The lexer
tokenParser = makeTokenParser haskellDef
parens      = tokenParser.parens
symbol      = tokenParser.symbol
whiteSpace  = tokenParser.whiteSpace

tyParse t = runParser t expr
