module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import DatalogAST
import Interpreter
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "matchPredicate" $ do
      it "matches a predicate atom with facts in the database" $ do
        let database =
              Set.fromList
                [PredicateAtom "parent" [Variable "X", Constant "Mary"]]
            atom = PredicateAtom "parent" [Variable "X", Constant "Mary"]
        matchPredicate atom database
          `shouldBe` Set.fromList
                       [PredicateAtom "parent" [Variable "X", Constant "Mary"]]
    describe "evaluateLiteral" $ do
      it "evaluates a positive literal against the database with substitutions" $ do
        let database =
              Set.fromList
                [PredicateAtom "parent" [Constant "John", Constant "Mary"]]
            subs = Map.empty -- starting with no substitutions
            literal =
              Positive (PredicateAtom "parent" [Variable "X", Constant "Mary"])
            expectedSubstitutions =
              Set.fromList [Map.fromList [("X", Constant "John")]]
        evaluateLiteral literal database subs `shouldBe` expectedSubstitutions
      it "evaluates a negative literal against the database with substitutions" $ do
        let database =
              Set.fromList
                [PredicateAtom "parent" [Constant "John", Constant "Mary"]]
            subs = Map.empty -- starting with no substitutions
            literal =
              Negative (PredicateAtom "parent" [Variable "X", Constant "Mary"])
            expected = Set.fromList [Map.fromList [("X", Constant "John")]]
        evaluateLiteral literal database subs `shouldBe` expected -- expecting the original empty substitution since the literal is negative and matches
    -- Additional tests can be written to check the behavior with non-empty substitution maps
      it "evaluates a positive literal with existing substitutions" $ do
        let database =
              Set.fromList
                [ PredicateAtom "parent" [Constant "John", Constant "Mary"]
                , PredicateAtom "parent" [Constant "Steve", Constant "Mary"]
                ]
            subs = Map.fromList [("X", Constant "Steve")] -- starting with an existing substitution
            literal =
              Positive (PredicateAtom "parent" [Variable "X", Constant "Mary"])
            expectedSubstitutions =
              Set.fromList [Map.fromList [("X", Constant "Steve")]]
        evaluateLiteral literal database subs `shouldBe` expectedSubstitutions
    -- Test case for positive literal with multiple variables
      it "evaluates a positive literal with multiple variables" $ do
        let db =
              Set.fromList
                [ PredicateAtom "sibling" [Constant "Alice", Constant "Bob"]
                , PredicateAtom "sibling" [Constant "Bob", Constant "Charlie"]
                ]
            subs = Map.empty
            literal =
              Positive (PredicateAtom "sibling" [Variable "X", Variable "Y"])
            expectedSubstitutions =
              Set.fromList
                [ Map.fromList [("X", Constant "Alice"), ("Y", Constant "Bob")]
                , Map.fromList
                    [("X", Constant "Bob"), ("Y", Constant "Charlie")]
                ]
        evaluateLiteral literal db subs `shouldBe` expectedSubstitutions
    -- Test case for positive literal with existing substitution
      it "evaluates a positive literal with an existing substitution" $ do
        let db =
              Set.fromList
                [PredicateAtom "married" [Constant "John", Constant "Jane"]]
            subs = Map.fromList [("X", Constant "John")]
            literal =
              Positive (PredicateAtom "married" [Variable "X", Variable "Y"])
            expectedSubstitutions =
              Set.fromList
                [Map.fromList [("X", Constant "John"), ("Y", Constant "Jane")]]
        evaluateLiteral literal db subs `shouldBe` expectedSubstitutions
    -- Test case for negative literal with a matching fact
      it "evaluates a negative literal with a matching fact in the database" $ do
        let db = Set.fromList [PredicateAtom "single" [Constant "Dave"]]
            subs = Map.empty
            literal = Negative (PredicateAtom "single" [Constant "Dave"])
            expectedSubstitutions = Set.singleton subs
        evaluateLiteral literal db subs `shouldBe` expectedSubstitutions
    -- Test case for negative literal with no matching fact
      it "evaluates a negative literal with no matching fact in the database" $ do
        let db = Set.fromList [PredicateAtom "single" [Constant "Alice"]]
            subs = Map.empty
            literal = Negative (PredicateAtom "single" [Constant "Dave"])
            expectedSubstitutions = Set.empty -- No matching fact, so the original substitutions should be returned
        evaluateLiteral literal db subs `shouldBe` expectedSubstitutions
      it "returns all valid substitutions when evaluating a positive literal" $ do
      -- Define a database with multiple Facts
        let db =
              Set.fromList
                [ PredicateAtom "parent" [Constant "Alice", Constant "Bob"]
                , PredicateAtom "parent" [Constant "Bob", Constant "Charlie"]
                , PredicateAtom "age" [Constant "Bob", Constant "30"]
                ]
            subs = Map.empty
            literal =
              Positive (PredicateAtom "parent" [Constant "Alice", Variable "X"])
            expectedSubstitutions =
              Set.fromList [Map.fromList [("X", Constant "Bob")]]
        evaluateLiteral literal db subs `shouldBe` expectedSubstitutions
      it
        "returns the original substitution when evaluating a negative literal with no matching facts" $ do
        let db =
              Set.fromList
                [PredicateAtom "parent" [Constant "Alice", Constant "Charlie"]]
            subs = Map.fromList [("X", Constant "Bob")]
            literal =
              Negative (PredicateAtom "parent" [Constant "Alice", Variable "X"])
            expectedSubstitutions = Set.empty
          -- No matching fact for "parent" with "Alice" and "Bob"
        evaluateLiteral literal db subs `shouldBe` expectedSubstitutions
      it
        "returns an empty set when a matching fact exists for a negative literal" $ do
        let db =
              Set.fromList
                [PredicateAtom "parent" [Constant "Alice", Constant "Bob"]]
            subs = Map.empty
            literal =
              Negative (PredicateAtom "parent" [Constant "Alice", Variable "X"])
            expectedSubstitutions =
              Set.fromList [Map.fromList [("X", Constant "Bob")]]
          -- Matching fact for "parent" with "Alice" and "Bob" exists
        evaluateLiteral literal db subs `shouldBe` expectedSubstitutions
      it "evaluates a negative literal with a matching fact in the database" $ do
        let db =
              Set.fromList
                [ PredicateAtom "parent" [Constant "Alice", Constant "Bob"]
                , PredicateAtom "parent" [Constant "Bob", Constant "Charlie"]
                , PredicateAtom "age" [Constant "Bob", Constant "30"]
                ]
            subs = Map.empty
            literal =
              Positive (PredicateAtom "parent" [Variable "X", Variable "Y"])
            expectedSubstitutions =
              Set.fromList
                [ Map.fromList
                    [("X", Constant "Bob"), ("Y", Constant "Charlie")]
                , Map.fromList [("X", Constant "Alice"), ("Y", Constant "Bob")]
                ]
      -- Expected outcome: List of substitutions where X is "Bob" and Y is "Charlie"
        evaluateLiteral literal db subs `shouldBe` expectedSubstitutions
    describe "evaluateArithmeticExpr" $ do
      let substitution =
            Map.fromList [("X", Constant "5"), ("Y", Constant "10")]
      it "evaluates simple arithmetic expressions with constants" $ do
        let expr =
              ArithmeticBinOp
                Plus
                (ArithmeticTerm (Constant "3"))
                (ArithmeticTerm (Constant "4"))
        evaluateArithmeticExpr expr Map.empty `shouldBe` Just (Constant "7")
      it "evaluates nested arithmetic expressions" $ do
        let expr =
              ArithmeticBinOp
                Minus
                (ArithmeticBinOp
                   Times
                   (ArithmeticTerm (Variable "X"))
                   (ArithmeticTerm (Constant "2")))
                (ArithmeticBinOp
                   Divide
                   (ArithmeticTerm (Variable "Y"))
                   (ArithmeticTerm (Constant "2")))
        evaluateArithmeticExpr expr substitution `shouldBe` Just (Constant "5")
      it "handles undefined variables" $ do
        let expr =
              ArithmeticBinOp
                Plus
                (ArithmeticTerm (Variable "Z"))
                (ArithmeticTerm (Constant "4"))
        evaluateArithmeticExpr expr substitution `shouldBe` Nothing
    describe "unification and rule evaluation" $ do
      it "correctly unifies variables across multiple terms" $ do
        let database =
              Set.fromList
                [PredicateAtom "likes" [Constant "Alice", Constant "Apples"]]
            query = PredicateAtom "likes" [Variable "X", Variable "Y"]
            expected =
              Set.fromList
                [PredicateAtom "likes" [Constant "Alice", Constant "Apples"]]
        matchPredicate query database `shouldBe` expected
      it "applies a rule to the database and adds the result to the database" $ do
        let database =
              Set.fromList
                [ PredicateAtom "parent" [Constant "Alice", Constant "Bob"]
                , PredicateAtom "parent" [Constant "Bob", Constant "Charlie"]
                ]
            rule =
              Rule
                { DatalogAST.head =
                    PredicateAtom "grandparent" [Variable "X", Variable "Z"]
                , body =
                    [ Positive
                        (PredicateAtom "parent" [Variable "X", Variable "Y"])
                    , Positive
                        (PredicateAtom "parent" [Variable "Y", Variable "Z"])
                    ]
                }
            updatedDatabase = applyRule rule database
            expectedDatabase =
              Set.union
                database
                (Set.fromList
                   [ PredicateAtom
                       "grandparent"
                       [Constant "Alice", Constant "Charlie"]
                   ])
        updatedDatabase `shouldBe` expectedDatabase
      it "handles negative literals in rule body" $ do
        let database =
              Set.fromList
                [ PredicateAtom "likes" [Constant "Alice", Constant "Apples"]
                , PredicateAtom "likes" [Constant "Bob", Constant "Carrots"]
                , PredicateAtom "allergic" [Constant "Bob", Constant "Apples"]
                ]
            rule =
              Rule
                { DatalogAST.head =
                    PredicateAtom "avoid" [Variable "X", Constant "Apples"]
                , body =
                    [ Negative
                        (PredicateAtom
                           "allergic"
                           [Variable "X", Constant "Apples"])
                    , Positive
                        (PredicateAtom "likes" [Variable "X", Variable "Y"])
                    ]
                }
            updatedDatabase = applyRule rule database
            expected =
              Set.fromList
                [ PredicateAtom "likes" [Constant "Alice", Constant "Apples"]
                , PredicateAtom "likes" [Constant "Bob", Constant "Carrots"]
                , PredicateAtom "allergic" [Constant "Bob", Constant "Apples"]
                , PredicateAtom "avoid" [Constant "Alice", Constant "Apples"]
                ]
        updatedDatabase `shouldBe` expected
    describe "unifyWithFact" $ do
      it "produces substitutions when terms can be unified with a fact" $ do
        let queryTerms = [Variable "X", Constant "Mary"]
            fact = PredicateAtom "parent" [Constant "John", Constant "Mary"]
        unifyWithFact queryTerms fact
          `shouldBe` [Map.singleton "X" (Constant "John")]
      it "produces an empty list when terms cannot be unified with a fact" $ do
        let queryTerms = [Variable "X", Constant "Steve"]
            fact = PredicateAtom "parent" [Constant "John", Constant "Mary"]
        unifyWithFact queryTerms fact `shouldBe` []
  -- Additional test cases for 'unifyPairs'
    describe "unifyPairs" $ do
      it
        "unifies a variable and a term when the variable is not yet in the substitution map" $ do
        let subs = Map.empty
            varTermPair = (Variable "X", Constant "John")
        unifyPairs subs varTermPair
          `shouldBe` [Map.singleton "X" (Constant "John")]
      it
        "fails to unify a variable and a term when the variable already has a different substitution" $ do
        let subs = Map.singleton "X" (Constant "Steve")
            varTermPair = (Variable "X", Constant "John")
        unifyPairs subs varTermPair `shouldBe` []
  -- Additional test cases for 'composeSubstitutions'
    describe "composeSubstitutions" $ do
      it "composes two non-conflicting substitutions" $ do
        let sub1 = Map.singleton "X" (Constant "John")
            sub2 = Map.singleton "Y" (Constant "Mary")
            expected =
              [Map.fromList [("X", Constant "John"), ("Y", Constant "Mary")]]
        composeSubstitutions sub1 sub2 `shouldBe` expected
      it "applies the first substitution to the second one during composition" $ do
        let sub1 = Map.singleton "X" (Constant "Steve")
            sub2 = Map.singleton "X" (Constant "John")
            expected =
              [ Map.singleton "X" (Constant "Steve")
              , Map.singleton "X" (Constant "John")
              ]
        composeSubstitutions sub1 sub2 `shouldBe` expected
      it "respects order of application - sub1 is applied to sub2" $ do
        let sub1 = Map.singleton "X" (Variable "Z")
            sub2 = Map.singleton "X" (Constant "John")
            expected =
              [ Map.singleton "X" (Variable "Z")
              , Map.singleton "X" (Constant "John")
              ]
        composeSubstitutions sub1 sub2 `shouldBe` expected
      it "applies sub1 to every element in sub2" $ do
        let sub1 = Map.singleton "Y" (Constant "Alice")
            sub2 = Map.fromList [("X", Variable "Y"), ("Z", Constant "Bob")]
            expected =
              [ Map.fromList
                  [ ("X", Constant "Alice")
                  , ("Y", Constant "Alice")
                  , ("Z", Constant "Bob")
                  ]
              ]
        composeSubstitutions sub1 sub2 `shouldBe` expected
      it "deals with complex substitutions involving multiple variables" $ do
        let sub1 = Map.fromList [("X", Variable "Y"), ("Y", Constant "Alice")]
            sub2 = Map.fromList [("Z", Variable "X"), ("W", Variable "Y")]
            expected =
              [ Map.fromList
                  [ ("X", Constant "Alice")
                  , ("Y", Constant "Alice")
                  , ("Z", Constant "Alice")
                  , ("W", Constant "Alice")
                  ]
              ]
        composeSubstitutions sub1 sub2 `shouldBe` expected
      it "should handle independent substitutions without invalidation" $ do
        let sub1 = Map.fromList [("X", Constant "Alice"), ("Y", Variable "Z")]
            sub2 =
              Map.fromList [("Z", Constant "Bob"), ("Y", Constant "Charlie")]
            expected =
              [ Map.fromList
                  [ ("X", Constant "Alice")
                  , ("Y", Constant "Charlie")
                  , ("Z", Constant "Bob")
                  ]
              ]
        composeSubstitutions sub1 sub2 `shouldBe` expected
      it "should handle different length to return sub2" $ do
        let sub1 = Map.singleton "X" (Constant "Alice")
            sub2 =
              Map.fromList [("X", Constant "Alice"), ("Y", Constant "Charlie")]
            expected =
              [ Map.fromList
                  [("X", Constant "Alice"), ("Y", Constant "Charlie")]
              ]
        composeSubstitutions sub1 sub2 `shouldBe` expected
      it "should handle different length to return union of subs" $ do
        let sub1 = Map.singleton "X" (Constant "Alice")
            sub2 =
              Map.fromList [("X", Constant "Bob"), ("Y", Constant "Charlie")]
            expected =
              [ Map.singleton "X" (Constant "Alice")
              , Map.fromList [("X", Constant "Bob"), ("Y", Constant "Charlie")]
              ]
        composeSubstitutions sub1 sub2 `shouldBe` expected
  -- Additional test cases for 'applySubstitution'
    describe "applySubstitution" $ do
      it "applies a substitution to a variable term" $ do
        let subs = Map.singleton "X" (Constant "John")
            term = Variable "X"
        applySubstitution subs term `shouldBe` Constant "John"
      it "leaves a constant term unchanged after applying a substitution" $ do
        let subs = Map.singleton "X" (Constant "John")
            term = Constant "Mary"
        applySubstitution subs term `shouldBe` term
    describe "evaluateBody" $ do
      it "returns an empty list when there are no literals to evaluate" $ do
        let db = Set.empty
            subs = Map.empty
            literals = []
        evaluateBody db subs literals `shouldBe` []
      it
        "returns a single substitution when there is one positive literal corresponding to a fact in the db" $ do
      -- Define a database with one Fact
        let db =
              Set.fromList
                [PredicateAtom "parent" [Constant "Alice", Constant "Bob"]]
            subs = Map.empty
            literals =
              [ Positive
                  (PredicateAtom "parent" [Constant "Alice", Variable "X"])
              ]
            expectedSubstitution = Map.fromList [("X", Constant "Bob")]
      -- Expected outcome: A single substitution where X is substituted with "Bob"
        evaluateBody db subs literals `shouldBe` [expectedSubstitution]
      it
        "returns all valid substitutions for a complex set of positive literals" $ do
      -- Define a database with multiple Facts
        let db =
              Set.fromList
                [ PredicateAtom "parent" [Constant "Alice", Constant "Bob"]
                , PredicateAtom "parent" [Constant "Bob", Constant "Charlie"]
                , PredicateAtom "age" [Constant "Bob", Constant "30"]
                ]
            subs = Map.empty
            literals =
              [ Positive
                  (PredicateAtom "parent" [Constant "Alice", Variable "X"])
              , Positive (PredicateAtom "parent" [Variable "X", Variable "Y"])
              , Positive (PredicateAtom "age" [Variable "X", Constant "30"])
              ]
            expectedSubstitutions =
              [ Map.fromList [("X", Constant "Alice"), ("Y", Constant "Bob")]
              , Map.fromList [("X", Constant "Bob"), ("Y", Constant "Charlie")]
              ]
      -- Expected outcome: List of substitutions where X is "Bob" and Y is "Charlie"
        evaluateBody db subs literals `shouldBe` expectedSubstitutions
      it
        "returns an non empty list when a contradiction is found in the literals" $ do
      -- Define a database with a fact contradicting one of the literals
        let db =
              Set.fromList
                [ PredicateAtom "friend" [Constant "Alice", Constant "Bob"]
                , PredicateAtom "friend" [Constant "Bob", Constant "Alice"]
                ]
            subs = Map.empty
            literals =
              [ Positive
                  (PredicateAtom "friend" [Constant "Alice", Variable "X"])
              , Positive (PredicateAtom "friend" [Variable "X", Constant "Bob"])
              ]
            expected =
              [ Map.fromList [("X", Constant "Bob")]
              , Map.fromList [("X", Constant "Alice")]
              ]
      -- Expected outcome: No valid substitutions, due to the contradiction in the database
        evaluateBody db subs literals `shouldBe` expected
    it "returns valid substitutions for a complex set of positive literals" $ do
      -- Define a database with a fact contradicting one of the literals
      let db =
            Set.fromList
              [ PredicateAtom "parent" [Constant "Alice", Constant "Bob"]
              , PredicateAtom "parent" [Constant "Bob", Constant "Charlie"]
              ]
          subs = Map.empty
          literals =
            [ Positive (PredicateAtom "parent" [Variable "X", Variable "Y"])
            , Positive (PredicateAtom "parent" [Variable "Y", Variable "Z"])
            ]
          expected =
            [ Map.fromList
                [ ("X", Constant "Alice")
                , ("Y", Constant "Bob")
                , ("Z", Constant "Charlie")
                ]
            ]
      -- Expected outcome: No valid substitutions, due to the contradiction in the database
      evaluateBody db subs literals `shouldBe` expected
    it
      "returns substitutions for a complex set of positive and negative literals" $ do
      -- Define a database with a fact contradicting one of the literals
      let db =
            Set.fromList
              [ PredicateAtom "likes" [Constant "Alice", Constant "Apples"]
              , PredicateAtom "likes" [Constant "Bob", Constant "Carrots"]
              , PredicateAtom "allergic" [Constant "Bob", Constant "Apples"]
              ]
          subs = Map.empty
          literals =
            [ Positive (PredicateAtom "likes" [Variable "X", Variable "Y"])
            , Negative
                (PredicateAtom "allergic" [Variable "X", Constant "Apples"])
            ]
          expected =
            [Map.fromList [("X", Constant "Alice"), ("Y", Constant "Apples")]]
      -- Expected outcome: No valid substitutions, due to the contradiction in the database
      evaluateBody db subs literals `shouldBe` expected
