module Main where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import Control.Monad.State
import qualified Data.Map as M

data Expression = Constant Double
                | Identifier String
                | Negation Expression
                | Addition Expression Expression
                | Subtraction Expression Expression
                | Multiplication Expression Expression
                | Division Expression Expression
                deriving (Show)

data Statement = PrintStatement Expression
               | AssigmentStatement String Expression
               deriving (Show)

lexer :: TokenParser ()
lexer = makeTokenParser javaStyle

parseNumber :: Parser Expression
parseNumber = do
  r <- naturalOrFloat lexer
  case r of
    Left n -> return $ Constant $ fromIntegral n
    Right f -> return $ Constant f

parseToken :: Parser Expression
parseToken = parens lexer parseExpression <|> parseNumber <|> (identifier lexer >>= return . Identifier)

parseExpression :: Parser Expression
parseExpression = (flip buildExpressionParser) parseToken [
    [ Prefix (reservedOp lexer "-" >> return Negation),
      Prefix (reservedOp lexer "+" >> return id)],
    [ Infix (reservedOp lexer "*" >> return Multiplication) AssocLeft,
      Infix (reservedOp lexer "/" >> return Division) AssocLeft],
    [ Infix (reservedOp lexer "+" >> return Addition) AssocLeft,
      Infix (reservedOp lexer "-" >> return Subtraction) AssocLeft]
  ]

parsePrintStatement = do
  reserved lexer "print"
  e <- parseExpression
  return $ PrintStatement e

parseAssigmentStatement = do
  reserved lexer "let"
  id <- identifier lexer
  reserved lexer "="
  expr <- parseExpression
  return $ AssigmentStatement id expr

parseLine :: Parser Statement
parseLine = do
  whiteSpace lexer
  s <- parsePrintStatement <|> parseAssigmentStatement
  eof
  return s

-- interpreter
type Calculator a = StateT (M.Map String Expression) IO a

defaultVars :: M.Map String Expression
defaultVars = M.fromList [("pi", Constant 3.141)]

interpretExpression :: Expression -> Calculator Double
interpretExpression (Constant d) = return d
interpretExpression (Identifier i) = do
  vars <- get
  case M.lookup i vars of
    Nothing -> fail $ "Variable not found id: " ++ i
    Just v -> interpretExpression v

interpretExpression (Addition e1 e2) = evalExpression (+) e1 e2
interpretExpression (Subtraction e1 e2) = evalExpression (-) e1 e2
interpretExpression (Multiplication e1 e2) = evalExpression (*) e1 e2
interpretExpression (Division e1 e2) = evalExpression (/) e1 e2

evalExpression :: (Double -> Double -> Double) -> Expression -> Expression -> Calculator Double
evalExpression op e1 e2 = do
  v1 <- interpretExpression e1
  v2 <- interpretExpression e2
  return $ op v1 v2

interpretStatement :: Statement -> Calculator ()
interpretStatement (PrintStatement expr) = do
  n <- interpretExpression expr
  liftIO $ print n
interpretStatement (AssigmentStatement ident expr) = do
  n <- interpretExpression expr
  modify (M.insert ident (Constant $ n))


calculate :: String -> Calculator ()
calculate s = do
  case ret of
    Left e -> liftIO $ print $ "error: " ++ (show e)
    Right v -> interpretStatement v
  where
    ret = parse parseLine "" s

calculator :: Calculator ()
calculator =
  liftIO getContents >>= (mapM_ calculate) . lines

main :: IO ()
main = evalStateT calculator defaultVars
