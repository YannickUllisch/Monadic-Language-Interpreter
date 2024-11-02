module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "put",
    "get",
    "loop",
    "for",
    "do",
    "while"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

pBool :: Parser Bool
pBool =
  choice
    [ True <$ lKeyword "true",
      False <$ lKeyword "false"
    ]

-- | We extract comma separated expression parsing into its own lexer function
-- to make it reusable in the future for e.g. list parsing 
lCommaSepExpr :: Parser [Exp]
lCommaSepExpr = do
  e <- pExp
  rest <- many $ lString "," *> pExp
  pure (e : rest)

lTuple :: Parser Exp
lTuple = lexeme $ Tuple <$> (lString "(" *> lCommaSepExpr <* lString ")")

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName,
      try $ Tuple [] <$ (lString "(" *> lString ")"), -- Parsing empty tuples
      try $ lString "(" *> pExp <* lString ")",
      try lTuple, 
      KvPut <$> (lKeyword "put" *> pAtom) <*> pAtom,
      KvGet <$> (lKeyword "get" *> pAtom)
    ]

pPExp :: Parser Exp
pPExp = chain =<< pAtom
  where
    chain x =
      choice
        [ do
            lString "."
            i <- lInteger
            chain $ Project x i,
          pure x
        ]

pFExp :: Parser Exp
pFExp = chain =<< pPExp
  where
    chain x =
      choice
        [ do
            y <- pPExp
            chain $ Apply x y,
          pure x
        ]

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
      Lambda
        <$> (lString "\\" *> lVName)
        <*> (lString "->" *> pExp),
      Let
        <$> (lKeyword "let" *> lVName)
        <*> (lString "=" *> pExp)
        <*> (lKeyword "in" *> pExp),
      try $ ForLoop
        <$> ((,) <$> (lKeyword "loop" *> lVName) <*> (lKeyword "=" *> pExp)) -- We pair parsed into a tuple to conform AST definition
        <*> ((,) <$> (lKeyword "for" *> lVName) <*> (lString "<" *> pExp)) -- Again pair parsed into a tuple
        <*> (lKeyword "do" *> pExp),
      try $ WhileLoop
        <$> ((,) <$> (lKeyword "loop" *> lVName) <*> (lString "=" *> pExp))
        <*> (lKeyword "while" *> pExp)
        <*> (lKeyword "do" *> pExp),
      pFExp
    ]

pExp3 :: Parser Exp
pExp3 = pLExp >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pLExp
            chain $ Mul x y,
          do
            lString "/"
            y <- pLExp
            chain $ Div x y,
          pure x
        ]

pExp2 :: Parser Exp
pExp2 = pExp3 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp3
            chain $ Add x y,
          do
            lString "-"
            y <- pExp3
            chain $ Sub x y,
          pure x
        ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "=="
            y <- pExp2
            chain $ Eql x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "&&"
            y <- pExp1
            chain $ BothOf x y,
          do
            lString "||"
            y <- pExp1
            chain $ OneOf x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
