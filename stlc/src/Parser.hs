{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import Syntax

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

dot :: Parser Text
dot = symbol "." <?> "dot"

colon :: Parser Text
colon = symbol ":" <?> "colon"

kw :: Text -> Parser Text
kw keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

true :: Parser Bool
true = kw "True" *> return True <?> "True"

false :: Parser Bool
false = kw "False" *> return False <?> "False"

ifKW :: Parser Text
ifKW = kw "If" <?> "If"

thenKW :: Parser Text
thenKW = kw "Then" <?> "then"

elseKW :: Parser Text
elseKW = kw "Else" <?> "Else"

boolKW :: Parser Text
boolKW = kw "Bool" <?> "Bool"

intKW :: Parser Text
intKW = kw "Int" <?> "Int"

letKW :: Parser Text
letKW = kw "Let" <?> "Let"

inKW :: Parser Text
inKW = kw "in" <?> "in"

equals :: Parser Text
equals = symbol "=" <?> "equals"

lambda :: Parser Text
lambda = symbol "λ" <|> symbol "\\" <?> "lambda symbol"

ident :: Parser String
ident = lexeme ((:) <$> lowerChar <*> many alphaNumChar <?> "variable")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pLambdaTerm :: Parser (Term String)
pLambdaTerm = choice
  [ pVar
  , pAbstraction
  , pApplication
  , pBoolLit
  , pIntLit
  , pIf
  , pLet
  ]
  <?> "lambda term"

pVar :: Parser (Term String)
pVar =
  Var <$> ident <?> "variable"

pAbstraction :: Parser (Term String)
pAbstraction =
  Abs <$> (lambda *> ident) <*> (colon *> pType) <*> (dot *> pLambdaTerm)
  <?> "abstraction"

pApplication :: Parser (Term String)
pApplication =
  foldl1 App <$> some (parens pLambdaTerm)
  <?> "application"

pBoolLit :: Parser (Term String)
pBoolLit =
  BoolLit <$> (true <|> false)
  <?> "boolean literal"

pIntLit :: Parser (Term String)
pIntLit =
  IntLit <$> integer
  <?> "integer literal"

pIf :: Parser (Term String)
pIf =
  If <$> (ifKW *> pLambdaTerm) <*> (thenKW *> pLambdaTerm) <*> (elseKW *> pLambdaTerm)
  <?> "if expression"

pLet :: Parser (Term String)
pLet =
  Let <$> (letKW *> ident) <*> (equals *> pLambdaTerm) <*> (inKW *> pLambdaTerm)
  <?> "let expression"

pAdd :: Parser (Term String)
pAdd =
  Add <$> parens pLambdaTerm <*> (symbol "+" *> parens pLambdaTerm)
  <?> "addition expression"

pSub :: Parser (Term String)
pSub =
  Sub <$> parens pLambdaTerm <*> (symbol "-" *> parens pLambdaTerm)
  <?> "subtraction expression"

pMul :: Parser (Term String)
pMul =
  Mul <$> parens pLambdaTerm <*> (symbol "*" *> parens pLambdaTerm)
  <?> "multiplication expression"

pDiv :: Parser (Term String)
pDiv =
  Div <$> parens pLambdaTerm <*> (symbol "/" *> parens pLambdaTerm)
  <?> "division expression"

pType :: Parser Type
pType =
    makeExprParser pBaseType [[binary "->" Arrow]]
    <?> "arrow"
  where
    binary name f = InfixR (f <$ symbol name)

pBaseType :: Parser Type
pBaseType = pTyVar <|> pIntType <|> pBoolType <|> parens pType <?> "base type"

pTyVar :: Parser Type
pTyVar = TyVar <$> ident <?> "type variable"

pBoolType :: Parser Type
pBoolType = Bool <$ boolKW <?> "Bool type tag"

pIntType :: Parser Type
pIntType = Int <$ intKW <?> "Int type tag"

parseEof :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
parseEof p = runParser (p <* eof) ""

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

parseLambdaTerm :: Text -> Either String (Term String)
parseLambdaTerm input =
  mapLeft errorBundlePretty $ parseEof pLambdaTerm input