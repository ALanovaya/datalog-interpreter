module Interpreter where

import Control.Monad (foldM, guard)
import Data.List ((\\), delete, nub, partition)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import DatalogAST

type Database = Set.Set Fact

type Substitution = Map.Map String Term

-- Evaluate an arithmetic expression
evaluateArithmeticExpr :: ArithmeticExpr -> Substitution -> Maybe Term
evaluateArithmeticExpr (ArithmeticTerm term) subs =
  case term of
    Variable v -> Map.lookup v subs
    Constant _ -> Just term
evaluateArithmeticExpr (ArithmeticBinOp op expr1 expr2) subs = do
  term1 <- evaluateArithmeticExpr expr1 subs
  term2 <- evaluateArithmeticExpr expr2 subs
  evalBinOp op term1 term2

evalBinOp :: ArithmeticOperator -> Term -> Term -> Maybe Term
evalBinOp op (Constant c1) (Constant c2) =
  case op of
    Plus -> Just . Constant $ show ((read c1 :: Integer) + (read c2 :: Integer))
    Minus ->
      Just . Constant $ show ((read c1 :: Integer) - (read c2 :: Integer))
    Times ->
      Just . Constant $ show ((read c1 :: Integer) * (read c2 :: Integer))
    Divide ->
      if c2 /= "0"
        then Just . Constant
               $ show ((read c1 :: Integer) `div` (read c2 :: Integer))
        else Nothing
    GreaterThanOrEqual ->
      Just . Constant $ show ((read c1 :: Integer) >= (read c2 :: Integer))
    LessThanOrEqual ->
      Just . Constant $ show ((read c1 :: Integer) <= (read c2 :: Integer))
    GreaterThan ->
      Just . Constant $ show ((read c1 :: Integer) > (read c2 :: Integer))
    LessThan ->
      Just . Constant $ show ((read c1 :: Integer) < (read c2 :: Integer))
    NotEquals ->
      Just . Constant $ show ((read c1 :: Integer) /= (read c2 :: Integer))
    Equals ->
      Just . Constant $ show ((read c1 :: Integer) == (read c2 :: Integer))
    Is ->
      Just . Constant
        $ if c1 == c2
            then "true"
            else "false"
evalBinOp _ _ _ = Nothing

-- Evaluate an Atom
evaluateAtom :: Atom -> Database -> Substitution -> [Substitution]
evaluateAtom (PredicateAtom pr ts) db subs =
  let matchedFacts = matchPredicate (PredicateAtom pr ts) db
      unifiedSubstitutions = concatMap (unifyWithFact ts) matchedFacts
   in concatMap (composeSubstitutions subs) unifiedSubstitutions
evaluateAtom (ArithmeticAtom expr) _ subs =
  case evaluateArithmeticExpr expr subs of
    Just term -> [Map.insert (show expr) term subs] -- Assuming you want to insert the result back into the substitution
    Nothing -> []

-- Unify a list of terms with a fact to produce a substitution
unifyWithFact :: [Term] -> Fact -> [Substitution]
unifyWithFact queryTerms (PredicateAtom _ factTerms) =
  foldM unifyPairs Map.empty (zip queryTerms factTerms)
unifyWithFact _ (ArithmeticAtom _) = []

-- Try to unify two terms and update the substitution
unifyPairs :: Substitution -> (Term, Term) -> [Substitution]
unifyPairs subs (Variable var, term) =
  case Map.lookup var subs of
    Just term' -> ([subs | term == term'])
    Nothing -> [Map.insert var term subs]
unifyPairs subs (term, Variable var) = unifyPairs subs (Variable var, term)
unifyPairs subs (Constant const1, Constant const2) = [subs | const1 == const2]

-- Compose two substitutions together
composeSubstitutions :: Substitution -> Substitution -> [Substitution]
composeSubstitutions sub1 sub2
  | hasConflicts sub1 sub2 = [sub1, sub2]
  | otherwise =
    let sub1AppliedToSub2 = Map.map (recursiveApply sub1) sub2
        combinedSubs = Map.union sub1AppliedToSub2 sub1
     in [Map.map (recursiveApply combinedSubs) combinedSubs]

-- Check for conflicts between the two substitutions
hasConflicts :: Substitution -> Substitution -> Bool
hasConflicts sub1 sub2 =
  allKeysInAnotherMap && noConflictingValues && noVariableAsKey
  where
    allKeysInAnotherMap =
      Map.keysSet sub1 `Set.isSubsetOf` Map.keysSet sub2
        || Map.keysSet sub2 `Set.isSubsetOf` Map.keysSet sub1
    noConflictingValues =
      any conflicts (Map.keysSet sub1 `Set.union` Map.keysSet sub2)
    conflicts key =
      case (Map.lookup key sub1, Map.lookup key sub2) of
        (Just (Constant val1), Just (Constant val2)) -> val1 /= val2
        (Just (Variable _), Just _) -> True
        (Just _, Just (Variable _)) -> True
        _ -> False
    noVariableAsKey =
      let allVars = [v | Variable v <- Map.elems sub1 ++ Map.elems sub2]
       in not
            $ any (`Map.member` sub1) allVars || any (`Map.member` sub2) allVars

-- Recursive function to apply substitutions until a constant is found or no further substitutions can be made
recursiveApply :: Substitution -> Term -> Term
recursiveApply subs term@(Variable var) =
  case Map.lookup var subs of
    Just nextTerm@(Variable _) -> recursiveApply subs nextTerm
    Just constant@(Constant _) -> constant
    Nothing -> term
recursiveApply _ term@(Constant _) = term

-- Apply a substitution to a term
applySubstitution :: Substitution -> Term -> Term
applySubstitution subs term@(Variable var) = Map.findWithDefault term var subs
applySubstitution _ term@(Constant _) = term

-- Filter the database for facts that match the given predicate atom
matchPredicate :: Atom -> Database -> Set.Set Fact
matchPredicate (PredicateAtom name ts) database = Set.filter matchFact database
  where
    matchFact (PredicateAtom factName factTerms) =
      name == factName && length ts == length factTerms
    matchFact _ = False -- Exclude non-predicate atoms
matchPredicate _ _ = Set.empty -- No matches for non-predicate atoms

evaluateLiteral :: Literal -> Database -> Substitution -> Set.Set Substitution
evaluateLiteral literal db subs =
  let substitutedAtom = applySubstitutionToAtom subs (getAtom literal)
   in Set.fromList $ do
    -- Find facts in the database that match the substituted atom
        fact <- Set.toList db
        guard $ atomsMatch substitutedAtom fact
    -- Generate new substitutions based on the match
        let newSubs = generateSubstitutions substitutedAtom fact
    -- Only return new substitutions that are compatible with the current ones
        guard $ isCompatible subs newSubs
        return $ Map.union subs newSubs
  where
    getAtom (Positive atom) = atom
    getAtom (Negative atom) = atom

-- Helper function to determine if two atoms match
atomsMatch :: Atom -> Atom -> Bool
atomsMatch (PredicateAtom predName1 terms1) (PredicateAtom predName2 terms2) =
  predName1 == predName2 && allTermsMatch terms1 terms2
atomsMatch _ _ = False

-- Helper function to determine if all terms in two lists match
allTermsMatch :: [Term] -> [Term] -> Bool
allTermsMatch [] [] = True
allTermsMatch (Constant a:as) (Constant b:bs) = a == b && allTermsMatch as bs
allTermsMatch (Variable _:as) (_:bs) = allTermsMatch as bs -- Variable matches anything
allTermsMatch _ _ = False

-- Helper function to generate new substitutions based on two matching atoms
generateSubstitutions :: Atom -> Atom -> Substitution
generateSubstitutions (PredicateAtom _ terms1) (PredicateAtom _ terms2) =
  Map.fromList $ catMaybes $ zipWith matchTerms terms1 terms2
  where
    matchTerms (Variable v) (Constant c) = Just (v, Constant c)
    matchTerms _ _ = Nothing
generateSubstitutions _ _ = Map.empty

-- Helper function to check if two substitutions are compatible
isCompatible :: Substitution -> Substitution -> Bool
isCompatible s1 s2 =
  all
    (\(k, v) -> Map.lookup k s1 == Just v || Map.notMember k s1)
    (Map.toList s2)

collectUnifications ::
     Atom -> Fact -> Set.Set Substitution -> Set.Set Substitution
collectUnifications (PredicateAtom name ts) (PredicateAtom factName factTerms) acc
  | name == factName && length ts == length factTerms =
    Set.union acc (unifyTerms Map.empty (zip ts factTerms))
  | otherwise = acc
collectUnifications _ _ acc = acc

unifyTerms :: Substitution -> [(Term, Term)] -> Set.Set Substitution
unifyTerms subs [] = Set.singleton subs
unifyTerms subs ((t1, t2):rest) =
  let possibleSubs = map (`unifyTerms` rest) (unifyPairs subs (t1, t2))
   in Set.fromList (concatMap Set.toList possibleSubs)

-- Apply a substitution to an atom
applySubstitutionToAtom :: Substitution -> Atom -> Atom
applySubstitutionToAtom subs (PredicateAtom predName ts) =
  PredicateAtom predName (map (applySubstitution subs) ts)
applySubstitutionToAtom _ atom@(ArithmeticAtom _) = atom -- Assuming no variables in ArithmeticAtom for simplicity

-- Evaluate a Rule
evaluateRule :: Database -> Substitution -> Rule -> Bool
evaluateRule db subs (Rule {DatalogAST.head = headAtom, body = bodyLiterals}) =
  all (\lit -> not (Set.null (evaluateLiteral lit db subs))) bodyLiterals
    && isHeadSatisfied
  where
    substitutedHead = applySubstitutionToAtom subs headAtom
    isHeadSatisfied = Set.member substitutedHead db

-- Apply a Rule to a Database
applyRule :: Rule -> Database -> Database
applyRule rule db =
  let bodySubstitutions = evaluateBody db Map.empty (body rule)
      newFacts =
        filter (not . (`Set.member` db))
          $ map
              (\subs -> applySubstitutionToAtom subs (DatalogAST.head rule))
              bodySubstitutions
   in foldr Set.insert db newFacts

-- Check if the first substitution is a subset of the second
isSubsetOf :: Substitution -> Substitution -> Bool
isSubsetOf sub1 sub2 =
  all (`Map.member` sub2) (Map.keys sub1)
    && all (\k -> Map.lookup k sub1 == Map.lookup k sub2) (Map.keys sub1)

evaluateBody :: Database -> Substitution -> [Literal] -> [Substitution]
evaluateBody db initialSubs literals =
  let (positiveLiterals, negativeLiterals) =
        partition isPositiveLiteral literals
      allVars = Set.fromList $ concatMap getVarsFromLiteral literals
      uniqueVarCount = Set.size allVars
      posSubs =
        concatMap
          (\lit -> Set.toList (evaluateLiteral lit db initialSubs))
          positiveLiterals
      negSubs =
        concatMap
          (\lit -> Set.toList (evaluateLiteral lit db initialSubs))
          negativeLiterals
      allSubs =
        filter (\posSub -> not $ any (`isSubsetOf` posSub) negSubs) posSubs
      filteredSubs =
        filter
          (\sub -> not $ any (sub `isSubsetOf`) (delete sub allSubs))
          allSubs
      composedSubs = composeAllSubs filteredSubs
      consistentSubs = filter isConsistent composedSubs
      correctSizeSubs =
        filter (\sub -> Map.size sub == uniqueVarCount) consistentSubs
   in nub correctSizeSubs
  where
    composeAllSubs subs =
      let pairs = [(x, y) | x <- subs, y <- subs, x /= y]
          composedPairs = nub $ concatMap (uncurry composeSubstitutions) pairs
          newSubs = nub (subs ++ composedPairs)
       in if null (newSubs \\ subs)
            then subs
            else composeAllSubs newSubs
    isConsistent sub =
      let constants = Map.elems sub
       in length constants == length (nub constants)
    isPositiveLiteral (Positive _) = True
    isPositiveLiteral (Negative _) = False

getVarsFromLiteral :: Literal -> [String]
getVarsFromLiteral (Positive atom) = getVarsFromAtom atom
getVarsFromLiteral (Negative atom) = getVarsFromAtom atom

getVarsFromAtom :: Atom -> [String]
getVarsFromAtom (PredicateAtom _ ts) = concatMap getVarsFromTerm ts
getVarsFromAtom (ArithmeticAtom expr) = getVarsFromArithmeticExpr expr

getVarsFromTerm :: Term -> [String]
getVarsFromTerm (Variable var) = [var]
getVarsFromTerm (Constant _) = []

getVarsFromArithmeticExpr :: ArithmeticExpr -> [String]
getVarsFromArithmeticExpr (ArithmeticTerm term) = getVarsFromTerm term
getVarsFromArithmeticExpr (ArithmeticBinOp _ expr1 expr2) =
  getVarsFromArithmeticExpr expr1 ++ getVarsFromArithmeticExpr expr2

-- Evaluate a query
evaluateQuery :: Query -> Database -> [Substitution]
evaluateQuery (Query atom) db = evaluateAtom atom db Map.empty

-- Fixpoint algorithm
evaluateProgram :: DatalogProgram -> Database
evaluateProgram program = fixpoint (map applyRule (rules program)) Set.empty
  where
    -- Extract rules from the program
    rules (DatalogProgram clauses) = [r | ClauseRule r <- clauses]
    -- Fixpoint function to apply the rules until no changes occur
    fixpoint rs db
      | db' == db = db
      | otherwise = fixpoint rs db'
      where
        db' = foldr ($) db rs
