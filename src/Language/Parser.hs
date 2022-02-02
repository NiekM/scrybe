{-# LANGUAGE TemplateHaskell #-}
module Language.Parser where

import Import hiding (some, many, parens, braces, lift)
import RIO.Partial (read)
import Language.Syntax
import Language.Utils
import Data.Data (cast)
import qualified RIO.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (Type)
import GHC.TypeLits

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

ident :: Parser Text
ident = lexeme $ fromString <$> ((:) <$> letterChar <*> many alphaNumChar)

number :: Parser Int
number = lexeme $ read <$> some digitChar

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

interleaved :: Parser a -> Parser b -> Parser (NonEmpty a)
interleaved p q = (:|) <$> p <*> many (q *> p)

class Parse a where
  parser :: Parser a

instance KnownSymbol s => Parse (NumVar s) where
  parser = MkNumVar <$ symbol (fromString s) <*> number
    where s = symbolVal (undefined :: proxy s)

instance Parse Bound where
  parser = Bound <$> ident

deriving via NumVar "_" instance Parse Free

var :: Parser (Either Bound Free)
var = Left <$> parser <|> Right <$> parser

-- * Types

typeApps :: Parser Type
typeApps = tApps <$> interleaved (parens typeApps <|> TVar <$> var) (pure ())

instance Parse Type where
  parser = tArrs <$> interleaved (parens parser <|> typeApps) (symbol "->")

instance Parse Binding where
  parser = Binding <$> (Bound <$> ident) <* symbol "::" <*> parser

-- * Expressions

deriving via NumVar "?" instance Parse Hole

simpleExpr :: Parse a => Parser (Expr a)
simpleExpr = EHole <$> braces parser
  <|> EVar <$> var
  <|> ELam <$ symbol "\\" <*> parser <* symbol "." <*> parser

instance Parse a => Parse (Expr a) where
  parser = eApps <$> interleaved (parens parser <|> simpleExpr) (pure ())

instance Parse a => Parse (Decl a) where
  parser = Decl <$> parser <* symbol "=" <*> parser

-- * Quasi quoters

liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (fmap liftText . cast)

basic :: QuasiQuoter
basic = QuasiQuoter
  { quoteExp = undefined
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

quasiExp :: Data a => Parser a -> String -> QuasiQuoter
quasiExp p name = basic
  { quoteExp = \s -> case runParser p name (fromString s) of
    Left e -> error (show e)
    Right x -> liftDataWithText x
  }

ty :: QuasiQuoter
ty = quasiExp (parser @Type) "Type"

ex :: QuasiQuoter
ex = quasiExp (parser @(Expr Hole)) "Expr"

bi :: QuasiQuoter
bi = quasiExp (parser @Binding) "Binding"
