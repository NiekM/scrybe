{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Language.Parser where

import Import hiding (some, many, lift, bracket)
import RIO.Partial (read)
import Language.Syntax
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
  | Newline
  deriving (Eq, Ord, Show, Read)

type Bracket = (Shape, Position)

data Shape = Round | Curly | Square
  deriving (Eq, Ord, Show, Read)

data Position = Open | Close
  deriving (Eq, Ord, Show, Read)

sc :: Lexer ()
sc = L.space hspace1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

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
  ("!$%&*+-.:<=>\\^|" :: String)

separator :: Lexer Text
separator = string "," <|> string ";"

bracket :: Lexer Bracket
bracket = choice
  [ (Round , Open) <$ char '(', (Round , Close) <$ char ')'
  , (Curly , Open) <$ char '{', (Curly , Close) <$ char '}'
  , (Square, Open) <$ char '[', (Square, Close) <$ char ']'
  ]

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
  , Newline <$ eol
  ]

type Parser = Parsec Void [Lexeme]

brackets :: Shape -> Parser a -> Parser a
brackets sh = between
  (single $ Bracket (sh, Open))
  (single $ Bracket (sh, Close))

alt :: Parser a -> Parser b -> Parser [a]
alt p q = toList <$> alt1 p q <|> mempty

alt1 :: Parser a -> Parser b -> Parser (NonEmpty a)
alt1 p q = (:|) <$> p <*> many (q *> p)

class Parse a where
  parser :: Parser a

parseUnsafe :: Parser a -> Text -> a
parseUnsafe p = maybe undefined
  (fromMaybe undefined . parseMaybe p) . parseMaybe lex

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
  parser = mempty

instance Parse Var where
  parser = MkVar <$> identifier

instance Parse Ctr where
  parser = MkCtr <$> constructor

instance Parse Hole where
  parser = MkHole <$> int

instance Parse Free where
  parser = MkFree <$> int

instance (Parse a, Parse (Expr l a b)) => Parse (Branch l a b) where
  parser = Branch <$> parser <* op "->" <*> parser

class ParseAtom l where
  parseAtom :: (Parse a, Parse b) => Parser (Expr l a b)

nat :: HasApp l => Int -> Expr l a b
nat 0 = Ctr "Zero"
nat n = App (Ctr "Succ") (nat $ n - 1)

parseNat :: HasApp l => Parser (Expr l a b)
parseNat = nat <$> int

list :: (Foldable f, HasApp l) => f (Expr l a b) -> Expr l a b
list = foldr (\x r -> apps [Ctr "Cons", x, r]) (Ctr "Nil")

parseList :: HasApp l => Parser (Expr l a b) -> Parser (Expr l a b)
parseList p = list <$> brackets Square (alt p (sep ","))

instance ParseAtom 'Pattern where
  parseAtom = choice
    [ Hole <$> brackets Curly parser
    , Var <$> parser
    , Ctr <$> parser
    , parseNat
    , parseList parser
    ]

instance ParseAtom 'Term where
  parseAtom = choice
    [ lams <$ op "\\" <*> some parser <* op "->" <*> parser
    , Case <$ key "case" <*> parser <* key "of" <*> alt parser (sep ";")
    , Let <$ key "let" <*> parser <* op "=" <*> parser <* key "in" <*> parser
    , Hole <$> brackets Curly parser
    , Var <$> parser
    , Ctr <$> parser
    , parseNat
    , parseList parser
    ]

instance ParseAtom 'Type where
  parseAtom = choice
    [ Hole <$> brackets Curly parser
    , Var <$> parser
    , Ctr <$> parser
    ]

parseApps :: (Parse a, Parse b, HasApp l, ParseAtom l) => Parser (Expr l a b)
parseApps = apps <$> some (brackets Round parseApps <|> parseAtom)

instance (Parse a, Parse b) => Parse (Expr 'Pattern a b) where
  parser = parseApps

instance (Parse a, Parse b) => Parse (Expr 'Term a b) where
  parser = parseApps

instance (Parse a, Parse b) => Parse (Expr 'Type a b) where
  parser = arrs <$> alt1 (brackets Round parser <|> parseApps) (op "->")

instance Parse Poly where
  parser = Poly <$ key "forall" <*> many parser <* op "." <*> parser
    <|> Poly [] <$> parser

instance Parse Sketch where
  parser = Sketch <$> parser <* op "::" <*> parser

instance Parse Datatype where
  parser = MkDatatype <$ key "data" <*> parser <*> many parser <*> choice
    [ do
      _ <- op "="
      cs <- alt1
        ((,) <$> parser <*> many (brackets Round parser <|> parseAtom))
        (op "|")
      return . toList $ cs
    , mempty
    ]

instance Parse Definition where
  parser = choice
    [ Datatype <$> parser
    , do
      x <- parser
      choice
        [ Signature x <$ op "::" <*> parser
        , Binding   x <$ op "="  <*> parser
        ]
    ]

instance Parse Module where
  parser = Module . catMaybes <$> alt (optional parser) (single Newline)
