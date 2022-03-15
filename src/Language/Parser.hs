{-# LANGUAGE FlexibleInstances #-}
module Language.Parser where

import Import hiding (some, many, lift, bracket)
import RIO.Partial (read)
import Language.Syntax
import Language.Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified RIO.Set as Set

type Lexer = Parsec Void Text

data Lexeme
  = Keyword Text
  | Identifier Text
  | Constructor Text
  | Operator Text
  | Separator Text
  | Bracket Bracket
  | Literal Int
  deriving (Eq, Ord, Show, Read)

type Bracket = (Shape, Position)

data Shape = Round | Curly | Square
  deriving (Eq, Ord, Show, Read)

data Position = Open | Close
  deriving (Eq, Ord, Show, Read)

sc :: Lexer ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

identChar :: Lexer Char
identChar = alphaNumChar <|> char '_' <|> char '\''

keywords :: [Text]
keywords = ["case", "of", "let", "in", "forall", "data"]

ident :: Lexer Char -> Lexer Text
ident start = fmap fromString $ (:) <$> start <*> many identChar

identOrKeyword :: Lexer Lexeme
identOrKeyword = ident lowerChar <&> \case
  t | t `elem` keywords -> Keyword t
    | otherwise -> Identifier t

operator :: Lexer Text
operator = fmap fromString . some . choice . fmap char $
  ("!$%^&*-=+\\:<>." :: String)

separator :: Lexer Text
separator = string "," <|> string ";"

bracket :: Lexer Bracket
bracket = (Round, Open) <$ char '('
  <|> (Round, Close) <$ char ')'
  <|> (Curly, Open) <$ char '{'
  <|> (Curly, Close) <$ char '}'
  <|> (Square, Open) <$ char '['
  <|> (Square, Close) <$ char ']'

literal :: Lexer Int
literal = read <$> some digitChar

lex :: Lexer [Lexeme]
lex = many . choice . fmap (L.lexeme sc) $
  [ identOrKeyword
  , Constructor <$> ident upperChar
  , Operator <$> operator
  , Separator <$> separator
  , Bracket <$> bracket
  , Literal <$> literal
  ]

type Parser = Parsec Void [Lexeme]

brackets :: Shape -> Parser a -> Parser a
brackets sh = between
  (single $ Bracket (sh, Open))
  (single $ Bracket (sh, Close))

interleaved :: Parser a -> Parser b -> Parser (NonEmpty a)
interleaved p q = (:|) <$> p <*> many (q *> p)

class Parse a where
  parser :: Parser a

parseUnsafe :: Parser a -> Text -> a
parseUnsafe p t = either (error . show)
  (either (error . show) id . parse p "") $ parse lex "" t

identifier :: Parser Text
identifier = flip token Set.empty \case
  Identifier i -> Just i
  _ -> Nothing

constructor :: Parser Text
constructor = flip token Set.empty \case
  Constructor i -> Just i
  _ -> Nothing

int :: Parser Int
int = flip token Set.empty \case
  Literal i -> Just i
  _ -> Nothing

sep :: Text -> Parser Lexeme
sep = single . Separator

op :: Text -> Parser Lexeme
op = single . Operator

key :: Text -> Parser Lexeme
key = single . Keyword

instance Parse Void where
  parser = mzero

instance Parse Unit where
  parser = return $ Unit ()

instance Parse Var where
  parser = MkVar <$> identifier

instance Parse Ctr where
  parser = MkCtr <$> constructor

instance Parse Hole where
  parser = MkHole <$> int

instance Parse Free where
  parser = MkFree <$> int

instance Parse a => Parse (Branch a) where
  parser = Branch <$> parser <* op "=>" <*> parser

class ParseAtom l where
  parseAtom :: Parse a => Parser (Expr l a)

instance ParseAtom 'Pattern where
  parseAtom = Hole <$> brackets Curly parser <|> Ctr <$> parser

num :: Int -> Term a
num 0 = Ctr "Zero"
num n = App (Ctr "Succ") (num $ n - 1)

instance ParseAtom 'Term where
  parseAtom = choice
    [ Lam <$ op "\\" <*> parser <* op "." <*> parser
    , Case <$ key "case" <*> parser <* key "of" <*>
      (toList <$> interleaved parser (sep ";"))
    , Let <$ key "let" <*> parser <* op "=" <*> parser <* key "in" <*> parser
    , Hole <$> brackets Curly parser
    , Var <$> parser
    , Ctr <$> parser
    , num <$> int
    ]

instance ParseAtom 'Type where
  parseAtom = choice
    [ Hole <$> brackets Curly parser
    , Var <$> parser
    , Ctr <$> parser
    ]

parseApps :: (Parse a, HasApp l, ParseAtom l) => Parser (Expr l a)
parseApps =
  apps <$> interleaved (brackets Round parseApps <|> parseAtom) (pure ())

instance Parse a => Parse (Pattern a) where
  parser = parseApps

instance Parse a => Parse (Term a) where
  parser = parseApps

instance Parse a => Parse (Type a) where
  parser = arrs <$> interleaved (brackets Round parser <|> parseApps) (op "->")

instance Parse Poly where
  parser = Poly <$ key "forall"
    <*> many parser
    <* op "." <*> parser

instance Parse Def where
  parser = Def <$> parser <* op "::" <*> parser <* op "=" <*> parser

instance Parse Sketch where
  parser = Sketch <$> parser <* op "::" <*> parser
