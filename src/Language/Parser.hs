{-# LANGUAGE FlexibleInstances #-}
module Language.Parser where

import Import hiding (some, many, lift)
import RIO.Partial (read)
import Language.Syntax
import Language.Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

ident :: Parser Text
ident = lexeme $ fromString <$> ((:) <$> letterChar <*> many alphaNumChar)

int :: Parser Int
int = lexeme $ read <$> some digitChar

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

interleaved :: Parser a -> Parser b -> Parser (NonEmpty a)
interleaved p q = (:|) <$> p <*> many (q *> p)

class Parse a where
  parser :: Parser a

parseUnsafe :: Parse a => Text -> a
parseUnsafe = fromRight undefined . parse parser ""

instance Parse Void where
  parser = mzero

instance Parse Var where
  parser = MkVar <$> ident

instance Parse Hole where
  parser = MkHole <$> int

instance Parse a => Parse (Binding a) where
  parser = Bind <$> parser <* symbol "::" <*> parser

instance Parse a => Parse (Branch a) where
  parser = Branch <$> parser <* symbol "=>" <*> parser

class ParseAtom l where
  parseAtom :: Parse a => Parser (Expr l a)

instance ParseAtom 'Term where
  parseAtom = Hole <$> braces parser <|> Var <$> parser
    <|> Lam <$ symbol "\\" <*> parser <* symbol "." <*> parser
    <|> Case . toList <$> brackets (interleaved parser (symbol ";"))

instance ParseAtom 'Type where
  parseAtom = Hole <$> braces parser <|> Var <$> parser

parseApps :: (Parse a, App l, ParseAtom l) => Parser (Expr l a)
parseApps = apps <$> interleaved (parens parseApps <|> parseAtom) (pure ())

instance Parse a => Parse (Term a) where
  parser = parseApps

instance Parse a => Parse (Type a) where
  parser = arrs <$> interleaved (parens parser <|> parseApps) (symbol "->")
