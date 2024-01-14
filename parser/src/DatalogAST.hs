module DatalogAST where

-- Define a type for a variable or constant
data Term
  = Variable String
  | Constant String
  deriving (Show, Eq, Ord)

-- Define a type for an atom (a predicate with its arguments)
data Atom
  = PredicateAtom
      { predicate :: String
      , terms :: [Term]
      }
  | ArithmeticAtom ArithmeticExpr
  deriving (Show, Eq, Ord)

data ArithmeticOperator
  = Plus
  | Minus
  | Times
  | Divide
  | GreaterThan
  | LessThan
  | Equals
  | NotEquals
  | GreaterThanOrEqual
  | Is
  | LessThanOrEqual
  deriving (Show, Eq, Ord)

data ArithmeticExpr
  = ArithmeticTerm Term
  | ArithmeticBinOp ArithmeticOperator ArithmeticExpr ArithmeticExpr
  deriving (Show, Eq, Ord)

data Literal
  = Positive Atom
  | Negative Atom
  deriving (Show, Eq)

-- Define a type for a rule (a head atom and a list of body atoms)
data Rule = Rule
  { head :: Atom
  , body :: [Literal]
  } deriving (Show, Eq)

-- Define a type for a Fact, which is essentially an Atom
type Fact = Atom

-- Define a type for a Clause, which can be either a Fact or a Rule
data Clause
  = ClauseFact Fact
  | ClauseRule Rule
  deriving (Show, Eq)

-- Modify the DatalogProgram type to hold a list of Clauses
newtype DatalogProgram =
  DatalogProgram [Clause]
  deriving (Show, Eq)

-- Define a type for a query (an atom representing the query to be evaluated)
newtype Query =
  Query Atom
  deriving (Show, Eq)
