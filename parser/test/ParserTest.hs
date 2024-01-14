module Main where

import Data.Void (Void)
import DatalogAST
import Parser
import Test.Hspec
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (ParseErrorBundle, eof, parse)

main :: IO ()
main =
  hspec $ do
    describe "Datalog Parser" $ do
      let parseDatalog = parse (datalogProgramParser <* eof) ""
      let parseQuery = parse (queryParser <* eof) ""
      it "parses an empty Datalog program" $ do
        parseDatalog "" `shouldParse` DatalogProgram []
      it "parses a fact" $ do
        let factStr = "parent(alice, bob)."
        let expected =
              DatalogProgram
                [ ClauseFact
                    (PredicateAtom "parent" [Constant "alice", Constant "bob"])
                ]
        parseDatalog factStr `shouldParse` expected
      it "parses a rule" $ do
        let ruleStr = "ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y)."
        let expected =
              DatalogProgram
                [ ClauseRule
                    (Rule
                       (PredicateAtom "ancestor" [Variable "X", Variable "Y"])
                       [ Positive
                           (PredicateAtom "parent" [Variable "X", Variable "Z"])
                       , Positive
                           (PredicateAtom
                              "ancestor"
                              [Variable "Z", Variable "Y"])
                       ])
                ]
        parseDatalog ruleStr `shouldParse` expected
      it "parses a complex rule with multiple body predicates" $ do
        let ruleStr =
              "descendant(X, Y) :- parent(X, Z), ancestor(Z, Y), not(criminal(Z))."
        let expected =
              DatalogProgram
                [ ClauseRule
                    (Rule
                       (PredicateAtom "descendant" [Variable "X", Variable "Y"])
                       [ Positive
                           (PredicateAtom "parent" [Variable "X", Variable "Z"])
                       , Positive
                           (PredicateAtom
                              "ancestor"
                              [Variable "Z", Variable "Y"])
                       , Negative (PredicateAtom "criminal" [Variable "Z"])
                       ])
                ]
        parseDatalog ruleStr `shouldParse` expected
      it "parses a query" $ do
        let queryStr = "?- sibling(X, Y)."
        let expected =
              Query (PredicateAtom "sibling" [Variable "X", Variable "Y"])
        parseQuery queryStr `shouldParse` expected
      it "parses a list of facts" $ do
        let factsStr =
              "parent(alice, bob). parent(bob, charlie). parent(charlie, dana)."
        let expected =
              DatalogProgram
                [ ClauseFact
                    (PredicateAtom "parent" [Constant "alice", Constant "bob"])
                , ClauseFact
                    (PredicateAtom "parent" [Constant "bob", Constant "charlie"])
                , ClauseFact
                    (PredicateAtom
                       "parent"
                       [Constant "charlie", Constant "dana"])
                ]
        parseDatalog factsStr `shouldParse` expected
      it "parses a program with facts and rules" $ do
        let programStr =
              "parent(alice, bob). grandparent(X, Y) :- parent(X, Z), parent(Z, Y)."
        let expected =
              DatalogProgram
                [ ClauseFact
                    (PredicateAtom "parent" [Constant "alice", Constant "bob"])
                , ClauseRule
                    (Rule
                       (PredicateAtom "grandparent" [Variable "X", Variable "Y"])
                       [ Positive
                           (PredicateAtom "parent" [Variable "X", Variable "Z"])
                       , Positive
                           (PredicateAtom "parent" [Variable "Z", Variable "Y"])
                       ])
                ]
        parseDatalog programStr `shouldParse` expected
      it "fails to parse a rule with missing dot" $ do
        let invalidRuleStr = "ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y)"
        parseDatalog `shouldFailOn` invalidRuleStr
    -- Add more negative test cases to ensure parser fails correctly
      it "fails to parse when there is an unexpected symbol" $ do
        let invalidStr = "parent(alice, bob)@"
        parseDatalog `shouldFailOn` invalidStr
      it "parses a factorial rule" $ do
        let factorialStr =
              "factorial(0, 1). factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1."
        let expected =
              DatalogProgram
                [ ClauseFact
                    (PredicateAtom "factorial" [Constant "0", Constant "1"])
                , ClauseRule
                    (Rule
                       (PredicateAtom "factorial" [Variable "N", Variable "F"])
                       [ Positive
                           (ArithmeticAtom
                              (ArithmeticBinOp
                                 GreaterThan
                                 (ArithmeticTerm (Variable "N"))
                                 (ArithmeticTerm (Constant "0"))))
                       , Positive
                           (ArithmeticAtom
                              (ArithmeticBinOp
                                 Is
                                 (ArithmeticTerm (Variable "N1"))
                                 (ArithmeticBinOp
                                    Minus
                                    (ArithmeticTerm (Variable "N"))
                                    (ArithmeticTerm (Constant "1")))))
                       , Positive
                           (PredicateAtom
                              "factorial"
                              [Variable "N1", Variable "F1"])
                       , Positive
                           (ArithmeticAtom
                              (ArithmeticBinOp
                                 Is
                                 (ArithmeticTerm (Variable "F"))
                                 (ArithmeticBinOp
                                    Times
                                    (ArithmeticTerm (Variable "N"))
                                    (ArithmeticTerm (Variable "F1")))))
                       ])
                ]
        parseDatalog factorialStr `shouldParse` expected
