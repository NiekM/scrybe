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

comment :: Lexer ()
comment = L.skipLineComment "--" <|> L.skipBlockComment "{-" "-}"

identChar :: Lexer Char
identChar = alphaNumChar <|> char '_' <|> char '\''

keywords :: [Text]
keywords = ["case", "of", "let", "in", "forall", "data", "fix"]

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
lex = (optional comment *>) . many . choice . fmap (L.lexeme sc) $
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

lexParse :: Parser a -> Text -> Maybe a
lexParse p t = parseMaybe lex t >>= parseMaybe p

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

class ParseAtom l where
  parseAtom :: Parser (Expr l)

parseNat :: (HasCtr l Ctr, HasApp l) => Parser (Expr l)
parseNat = nat <$> int

parseList :: (HasCtr l Ctr, HasApp l) =>
  Parser (Expr l) -> Parser (Expr l)
parseList p = list <$> brackets Square (alt p (sep ","))

parseBranch :: Parse h => Parser (Ctr, Term h)
parseBranch = (,) <$> parser <*> (lams <$> many parser <* op "->" <*> parser)

instance Parse h => ParseAtom ('Term h) where
  parseAtom = choice
    [ lams <$ op "\\" <*> some parser <* op "->" <*> parser
    , Case <$ key "case" <*> parser <* key "of" <*> alt parseBranch (sep ";")
    , Let <$ key "let" <*> parser <*>
      (lams <$> many parser <* op "=" <*> parser) <* key "in" <*> parser
    , Hole <$> brackets Curly parser
    , Var <$> parser
    , Ctr <$> parser
    , Fix <$ key "fix"
    , parseNat
    , parseList parser
    ]

instance ParseAtom 'Type where
  parseAtom = choice
    [ Var <$> parser
    , Ctr <$> parser
    ]

instance ParseAtom 'Value where
  parseAtom = choice
    [ Ctr <$> parser
    , parseNat
    , parseList parser
    ]

instance ParseAtom 'Example where
  parseAtom = choice
    [ lams <$ op "\\" <*> some (brackets Round parser <|> parseAtom)
      <* op "->" <*> parser
    , Hole <$> brackets Curly parser
    , Ctr <$> parser
    , parseNat
    , parseList parser
    ]

parseApps :: (May Parse (Hole' l), HasApp l, ParseAtom l)
  => Parser (Expr l)
parseApps = apps <$> atom <*> many atom
  where atom = brackets Round parseApps <|> parseAtom

instance Parse h => Parse (Term h) where
  parser = parseApps

instance Parse Type where
  parser = arrs <$> alt1 (brackets Round parser <|> parseApps) (op "->")

instance Parse Value where
  parser = parseApps

instance Parse Example where
  parser = parseApps

instance Parse a => Parse (Annot a Type) where
  parser = Annot <$> parser <* op "::" <*> parser

instance Parse Poly where
  parser = Poly <$ key "forall" <*> many parser <* op "." <*> parser
    <|> poly <$> parser

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

instance Parse a => Parse (Definition a) where
  parser = choice
    [ Datatype <$> parser
    , do
      x <- parser
      choice
        [ Signature . MkSignature x <$ op "::" <*> parser
        , Binding . MkBinding x <$> (lams <$> many parser <* op "=" <*> parser)
        ]
    ]

instance Parse a => Parse (Module a) where
  parser = Module . catMaybes <$> alt (optional parser) (single Newline)

instance Parse Sketch where
  parser = do
    ([MkSignature x s], [MkBinding y b], []) <- sepModule <$> parser
    guard (x == y)
    return $ Sketch x s b
