{-# LANGUAGE TemplateHaskell #-}
module Language.Parser where

import Import hiding (some, many, parens, lift)
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

interleaved :: Parser a -> Parser b -> Parser (NonEmpty a)
interleaved p q = (:|) <$> p <*> many (q *> p)

var :: Parser (Either Bound Free)
var = Left . Bound <$> ident <|> Right . Free <$> number

-- * Types

typeApps :: Parser Type
typeApps = tApps <$> interleaved (parens typeApps <|> TVar <$> var) (pure ())

arrs :: Parser Type
arrs = tArrs <$> interleaved (parens arrs <|> typeApps) (symbol "->")

parseType :: Parser Type
parseType = arrs

parseBinding :: Parser Binding
parseBinding = Binding <$> (Bound <$> ident) <* symbol "::" <*> parseType

-- * Expressions

hole :: Parser Expr
hole = EHole . Hole <$> (char '?' *> number)

lam :: Parser Expr
lam = ELam <$ symbol "\\" <*> parseBinding <* symbol "." <*> parseExpr

simpleExpr :: Parser Expr
simpleExpr = hole <|> EVar <$> var <|> lam

exprApps :: Parser Expr
exprApps = eApps <$> interleaved (parens exprApps <|> simpleExpr) (pure ())

parseExpr :: Parser Expr
parseExpr = exprApps

parseDecl :: Parser Decl
parseDecl = Decl <$> parseBinding <* symbol "=" <*> parseExpr

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
quasiExp parser name = basic
  { quoteExp = \s -> case runParser parser name (fromString s) of
    Left e -> error (show e)
    Right x -> liftDataWithText x
  }

ty :: QuasiQuoter
ty = quasiExp parseType "Type"

ex :: QuasiQuoter
ex = quasiExp parseExpr "Expr"

bi :: QuasiQuoter
bi = quasiExp parseBinding "Binding"

