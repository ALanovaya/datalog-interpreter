module Parser where

import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Data.Void
import DatalogAST
import Text.Megaparsec
import Text.Megaparsec.Char
  ( alphaNumChar
  , char
  , digitChar
  , letterChar
  , lowerChar
  , space
  , space1
  , string
  , upperChar
  )

type Parser = Parsec Void String

termParser :: Parser Term
termParser = (Variable <$> variableParser) <|> (Constant <$> constantParser)

-- Parse a variable (assuming it starts with an uppercase letter).
variableParser :: Parser String
variableParser = (:) <$> (space *> upperChar) <*> many alphaNumChar <* space

-- Parse a constant (assuming it starts with a lowercase letter).
constantParser :: Parser String
constantParser =
  (:) <$> (space *> (lowerChar <|> digitChar)) <*> many alphaNumChar <* space

-- Parser for arithmetic operators
arithmeticOperatorParser :: Parser ArithmeticOperator
arithmeticOperatorParser =
  (char '+' *> pure Plus)
    <|> (char '-' *> pure Minus)
    <|> (char '*' *> pure Times)
    <|> (char '/' *> pure Divide)
    <|> (string ">=" *> pure GreaterThanOrEqual)
    <|> (string "<=" *> pure LessThanOrEqual)
    <|> (char '>' *> pure GreaterThan)
    <|> (char '<' *> pure LessThan)
    <|> (string "!=" *> pure NotEquals)
    <|> (string "is" *> pure Is)
    <|> (char '=' *> pure Equals)

-- Parser for arithmetic expressions
arithmeticExprParser :: Parser ArithmeticExpr
arithmeticExprParser = makeExprParser recTermParser operatorTable
  where
    recTermParser = ArithmeticTerm <$> termParser
    operatorTable =
      [ [ binary "*" (ArithmeticBinOp Times)
        , binary "/" (ArithmeticBinOp Divide)
        ]
      , [binary "+" (ArithmeticBinOp Plus), binary "-" (ArithmeticBinOp Minus)]
      , [ binary ">" (ArithmeticBinOp GreaterThan)
        , binary "<" (ArithmeticBinOp LessThan)
        , binary ">=" (ArithmeticBinOp GreaterThanOrEqual)
        , binary "<=" (ArithmeticBinOp LessThanOrEqual)
        , binary "!=" (ArithmeticBinOp NotEquals)
        , binary "=" (ArithmeticBinOp Equals)
        ]
      , [binary "is" (ArithmeticBinOp Is)]
      ]
    binary name f = InfixL (f <$ (space *> string name <* space))

-- Parse an atom or a negated atom.
literalParser :: Parser Literal
literalParser = do
  negation <- optional $ string "not" >> space
  _ <- optional $ char '(' >> space
  atom <- atomParser
  _ <- optional $ space >> char ')'
  return
    $ case negation of
        Just _ -> Negative atom
        Nothing -> Positive atom

-- Parse an atom.
atomParser :: Parser Atom
atomParser = try parsePredicateAtom <|> parseArithmeticAtom

parsePredicateAtom :: Parser Atom
parsePredicateAtom = do
  pr <- predicateParser
  _ <- char '('
  ts <- termParser `sepBy` (char ',' >> space)
  _ <- char ')'
  return $ PredicateAtom pr ts

parseArithmeticAtom :: Parser Atom
parseArithmeticAtom = ArithmeticAtom <$> arithmeticExprParser

-- Parse a predicate name.
predicateParser :: Parser String
predicateParser = some letterChar <* space

-- Parse a rule.
ruleParser :: Parser Rule
ruleParser = do
  h <- space >> atomParser
  _ <- space >> string ":-" >> space
  b <- literalParser `sepBy` (char ',' >> space)
  _ <- char '.' >> space
  return $ Rule h b

-- Parse a fact (an atom followed by a dot).
factParser :: Parser Fact
factParser = atomParser <* char '.' <* space

-- Parse a clause, which can be a fact or a rule.
clauseParser :: Parser Clause
clauseParser = (ClauseFact <$> try factParser) <|> (ClauseRule <$> ruleParser)

-- Parse a Datalog program (a list of clauses).
datalogProgramParser :: Parser DatalogProgram
datalogProgramParser = DatalogProgram <$> many clauseParser <* eof

-- Parse a query.
queryParser :: Parser Query
queryParser = do
  _ <- string "?-" >> space1
  atom <- atomParser
  _ <- char '.'
  return $ Query atom
