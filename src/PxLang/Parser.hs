{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module PxLang.Parser where

import           Control.Monad                  (msum)
import           Control.Monad.Combinators.Expr (makeExprParser)
import           Data.Bifunctor                 (bimap)
import           Data.Char                      (isUpper)
import           Data.Fix                       (Fix (..))
import           Data.Void                      (Void)
import           Text.Megaparsec                (MonadParsec, ParseErrorBundle,
                                                 Parsec, Token, Tokens, between,
                                                 errorBundlePretty, many,
                                                 notFollowedBy, parse, satisfy,
                                                 some, try, (<?>), (<|>))
import           Text.Megaparsec.Char           (alphaNumChar, letterChar,
                                                 space, space1, string)
import qualified Text.Megaparsec.Char.Lexer     as L

import           PxLang.Syntax


type CharToken s = (Token s ~ Char, Tokens s ~ String)


type PxParser e s m = (MonadParsec e s m, CharToken s)


-- $setup
-- >>> import Data.Either      (isLeft)
-- >>> :set -XOverloadedStrings -XTypeApplications


docParse :: Parsec e String a -> String -> Either (ParseErrorBundle String e) a
docParse p = parse p "<doc comments>"


roundtrip
  :: Parsec e String (Fix Expr) -> String -> Either (ParseErrorBundle String e) String
roundtrip p = fmap pxPretty . docParse p


testParse :: Parsec e String (Fix Expr) -> String -> IO ()
testParse p = sequence_ . fmap putStr . roundtrip p


replParse :: Parsec Void String (Fix Expr) -> String -> Either String (Fix Expr)
replParse p = bimap errorBundlePretty id . parse p "repl"


prettyReplParse :: String -> Either String (Fix Expr)
prettyReplParse = replParse (space *> expr)


--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------


sc :: PxParser e s m => m ()
sc = L.space space1 lineComment blockComment
  where lineComment  = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"


lexeme :: PxParser e s m => m a -> m a
lexeme = L.lexeme sc


symbol :: PxParser e s m => String -> m String
symbol = L.symbol sc


parens :: PxParser e s m => m a -> m a
parens = between (symbol "(") (symbol ")")


col :: PxParser e s m => m String
col = symbol ":"


primVars :: [String]
primVars = ["#add", "#sub", "#mult", "#div", "#equal", "#if"]


rws :: [String]
rws = ["let", "in"] ++ primVars


rwordOf :: PxParser e s m => String -> m String
rwordOf w = (lexeme . try) (string w <* notFollowedBy alphaNumChar)


rword :: PxParser e s m => String -> m ()
rword w = rwordOf w *> pure ()


-- | Parse an identifier that is not infix.
--
-- >>> docParse identifier "hiThere boom"
-- Right "hiThere"
--
-- >>> isLeft $ docParse identifier "!*! x"
-- True
identifier :: PxParser e s m => m String
identifier = (lexeme . try) (p >>= check)
  where p = (:) <$> letterChar <*> many alphaNumChar
        check x
          | x `elem` rws = fail $ unwords [ "keyword"
                                          , show x
                                          , "cannot be an identifier"
                                          ]
          | isUpper $ head x = fail "identifier cannot start with an upper case letter"
          | otherwise = return x


-- | Parse an infix identifier.
--
-- >>> isLeft $ docParse identifierInfix "hiThere boom"
-- True
--
-- >>> docParse identifierInfix "*** x"
-- Right "***"
identifierInfix :: PxParser e s m => m String
identifierInfix = lexeme $ some $ satisfy (`elem` chars)
  where chars :: String
        chars = "!#$%&*+,-./:<=>?@^_'~"


--------------------------------------------------------------------------------
-- Parsing Syntax
--------------------------------------------------------------------------------


-- | Parse an int.
--
-- >>> docParse integer "-230"
-- Right (-230)
-- >>> docParse integer "230"
-- Right 230
integer :: PxParser e s m => m Int
integer = L.signed sc $ lexeme L.decimal


-- | Parse a float.
--
-- >>> docParse float "-666.0"
-- Right (-666.0)
float :: PxParser e s m => m Float
float = L.signed sc $ lexeme L.float


name :: PxParser e s m => m Name
name = Name <$> identifier


reservedVariable :: PxParser e s m => m (Fix Expr)
reservedVariable = Fix . Var . Name <$> msum (map rwordOf primVars) <?> "reservedVariable"


-- | Parse a variable expression.
--
-- >>> unFix <$> replParse variable "thisIsAVar"
-- Right (Var (Name {unName = "thisIsAVar"}))
variable :: PxParser e s m => m (Fix Expr)
variable = Fix . Var <$> name <?> "variable"


-- | Parse a number expression.
--
-- >>> unFix <$> docParse number "32"
-- Right (Lit (LitInt 32))
number :: PxParser e s m => m (Fix Expr)
number = msum [ Fix . Lit . LitInt <$> integer
              , Fix . Lit . LitFloat <$> float
              ] <?> "number"


-- | Parse a bool expression
--
-- >>> unFix <$> docParse bool "True"
-- Right (Lit (LitBool True))
bool :: PxParser e s m => m (Fix Expr)
bool = msum [ string "True"  *> my True
            , string "False" *> my False
            ] <* space <?> "bool"
  where my = pure . Fix . Lit . LitBool


-- | Parse a literal expression.
--
-- >>> unFix <$> docParse lit "1000"
-- Right (Lit (LitInt 1000))
--
-- >>> unFix <$> docParse lit "False"
-- Right (Lit (LitBool False))
lit :: PxParser e s m => m (Fix Expr)
lit = (number <|> bool) <?> "lit"


-- | Parse a lambda expression.
--
-- >>> testParse lambda "\\x y z -> 1000"
-- \x -> \y -> \z -> 1000
--
-- >>> testParse lambda "\\x y -> add x y"
-- \x -> \y -> add x y
lambda :: PxParser e s m => m (Fix Expr)
lambda = flip (<?>) "lambda" $ do
  _    <- symbol "\\"
  args <- some name
  _    <- symbol "->"
  body <- expr
  return $ foldr ((Fix .) . Lam) body args


-- | Parse a let binding expression.
--
-- >>> testParse letin "let x = 1000 in y"
-- let x = 1000 in y
letin :: PxParser e s m => m (Fix Expr)
letin = do
  _   <- rword "let"
  var <- name
  _   <- symbol "="
  lhs <- expr
  _   <- rword "in"
  rhs <- expr
  return $ Fix $ Let var lhs rhs


-- | Can parse multi part expressions (expressions with multiple branches of
-- sub expressions).
--
-- >>> docParse expr "fun 2"
-- Right (Fix (App (Fix (Var (Name {unName = "fun"}))) (Fix (Lit (LitInt 2)))))
--
-- >>> testParse expr "let x = 2 in f x"
-- let x = 2 in f x
--
-- >>> testParse expr "\\x y z -> 1000"
-- \x -> \y -> \z -> 1000
--
-- >>> testParse expr "equal True False"
-- equal True False
expr :: PxParser e s m => m (Fix Expr)
expr = makeExprParser term []


-- | Parse one single expression.
--
-- >>> :{
-- let Right asts = docParse (some aexpr) "x y z 1000 (let x = 5 in x) False blah"
-- in putStr $ unlines $ map pxPretty asts
-- >>> :}
-- x
-- y
-- z
-- 1000
-- let x = 5 in x
-- False
-- blah
--
-- >>> testParse aexpr "2"
-- 2
--
-- >>> testParse aexpr "(\\x -> \\y -> y)"
-- \x -> \y -> y
--
-- >>> testParse aexpr "(\\x y -> y)"
-- \x -> \y -> y
--
-- >>> testParse aexpr "(blah x y z)"
-- blah x y z
--
-- >>> testParse aexpr "()"
--
--
-- >>> testParse term "equal True False"
-- equal True False
--
-- >>> testParse term "#equal True False"
-- #equal True False
aexpr :: PxParser e s m => m (Fix Expr)
aexpr = parens expr
    <|> lit
    <|> letin
    <|> lambda
    <|> variable
    <|> reservedVariable


-- | A term parser parses one single expression or a series of
-- applications.
-- >>> testParse term "blah 5 6 7"
-- blah 5 6 7
--
-- >>> testParse term "blah (let x = 3 in x) 25"
-- blah (let x = 3 in x) 25
--
-- >>> testParse term "blah True False"
-- blah True False
--
-- Symbols are not valid identifiers (yet).
-- >>> isLeft $ docParse term "* 2 2"
-- True
term :: PxParser e s m => m (Fix Expr)
term = some aexpr >>= \case
  [e]  -> return e
  e:es -> return $ foldl ((Fix .) . App) e es
  []   -> fail "absurd"


--------------------------------------------------------------------------------
-- Parsing Types
--------------------------------------------------------------------------------
