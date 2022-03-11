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

var :: Parser Text
var = lexeme $ fromString <$> ((:) <$> lowerChar <*> many alphaNumChar)

ctr :: Parser Text
ctr = lexeme $ fromString <$> ((:) <$> upperChar <*> many alphaNumChar)

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

parseUnsafe :: Parser a -> Text -> a
parseUnsafe p = either (error . show) id . parse p ""

instance Parse Void where
  parser = mzero

instance Parse Unit where
  parser = Unit <$> sc

instance Parse Var where
  parser = MkVar <$> var

instance Parse Ctr where
  parser = MkCtr <$> ctr

instance Parse Hole where
  parser = MkHole <$> int

instance Parse Free where
  parser = MkFree <$> int

instance Parse a => Parse (Branch a) where
  parser = Branch <$> parser <* symbol "=>" <*> parser

class ParseAtom l where
  parseAtom :: Parse a => Parser (Expr l a)

instance ParseAtom 'Pattern where
  parseAtom = Hole <$> braces parser <|> Ctr <$> parser

num :: Int -> Term a
num 0 = Ctr "Zero"
num n = App (Ctr "Succ") (num $ n - 1)

instance ParseAtom 'Term where
  parseAtom = Lam <$ symbol "\\" <*> parser <* symbol "." <*> parser
    <|> Case <$ symbol "[" <*> parser <* symbol "]" <*>
      (toList <$> interleaved parser (symbol ";"))
    <|> Let <$ symbol "@" <*> parser <* symbol "=" <*> parser
      <* symbol "," <*> parser
    <|> Hole <$> braces parser <|> Var <$> parser <|> Ctr <$> parser
    <|> num <$> int

instance ParseAtom 'Type where
  parseAtom = Hole <$> braces parser <|> Var <$> parser <|> Ctr <$> parser

parseApps :: (Parse a, HasApp l, ParseAtom l) => Parser (Expr l a)
parseApps = apps <$> interleaved (parens parseApps <|> parseAtom) (pure ())

instance Parse a => Parse (Pattern a) where
  parser = parseApps

instance Parse a => Parse (Term a) where
  parser = parseApps

instance Parse a => Parse (Type a) where
  parser = arrs <$> interleaved (parens parser <|> parseApps) (symbol "->")

instance Parse Poly where
  parser = Poly <$ symbol "forall"
    <*> many parser
    <* symbol "." <*> parser

instance Parse Def where
  parser = Def <$> parser <* symbol "::" <*> parser <* symbol "=" <*> parser

instance Parse Sketch where
  parser = Sketch <$> parser <* symbol "::" <*> parser
