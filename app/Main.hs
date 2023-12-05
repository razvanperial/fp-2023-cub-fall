{-# LANGUAGE DeriveFunctor #-}

module Main where

-- property based testing
import Test.QuickCheck

-- Define the regular expression data type
data RegExp a
  = Empty
  | Epsilon
  | Literal a
  | Concat (RegExp a) (RegExp a)
  | Alt (RegExp a) (RegExp a)
  | Repetition (RegExp a)
  | Optional (RegExp a)
  | Wildcard
  | RepetitionRange Int Int (RegExp a)
  deriving (Functor, Show)

-- Define a smart constructor for repetition with a specified range
repetitionRange :: Int -> Int -> RegExp a -> RegExp a
repetitionRange minRange maxRange re
  | minRange > maxRange = error "Invalid repetition range"
  | minRange == 0 && maxRange == 0 = Epsilon
  | minRange == 0 && maxRange == 1 = Optional re
  | minRange == 0 = Optional (RepetitionRange 1 maxRange re)
  | minRange == maxRange = Repetition re
  | otherwise = RepetitionRange minRange maxRange re

-- Match function using Brzozowski derivatives
match :: Eq a => RegExp a -> [a] -> Bool
match re input = nullable (foldl derivative re input)

-- Nullable function checks if the regular expression can match an empty string
nullable :: Eq a => RegExp a -> Bool
nullable Empty = False
nullable Epsilon = True
nullable (Literal _) = False
nullable (Concat r1 r2) = nullable r1 && nullable r2
nullable (Alt r1 r2) = nullable r1 || nullable r2
nullable (Repetition _) = True
nullable (Optional _) = True
nullable Wildcard = True
nullable (RepetitionRange _ _ _) = True

-- Derivative function computes the Brzozowski derivative of a regular expression with respect to a symbol
derivative :: Eq a => RegExp a -> a -> RegExp a
derivative Empty _ = Empty
derivative Epsilon _ = Empty
derivative (Literal a) b
  | a == b = Epsilon
  | otherwise = Empty
derivative (Concat r1 r2) a
  | nullable r1 = Alt (Concat (derivative r1 a) r2) (derivative r2 a)
  | otherwise = Concat (derivative r1 a) r2
derivative (Alt r1 r2) a = Alt (derivative r1 a) (derivative r2 a)
derivative (Repetition r) a = Concat (derivative r a) (Repetition r)
derivative (Optional r) a = Optional (derivative r a)
derivative Wildcard _ = Epsilon
derivative (RepetitionRange minRange maxRange r) a =
  repetitionRange (max 0 (minRange - 1)) (max 0 (maxRange - 1)) (derivative r a)



-- Property based testing

-- Property 1: Matching epsilon should always return True
prop_epsilonAlwaysMatches :: Property
prop_epsilonAlwaysMatches = property $ match Epsilon "" === True

-- Property 2: Matching any literal character should return False for an empty string
prop_literalNeverMatchesEmpty :: Char -> Property
prop_literalNeverMatchesEmpty c = property $ not (match (Literal c) "")

-- Property 3: Matching a single literal character should return True for a string containing only that character
prop_literalMatchesSingleChar :: Char -> Property
prop_literalMatchesSingleChar c = property $ match (Literal c) [c] === True

-- Property 4: A repeated character should match a string with multiple occurrences of that character
prop_repetitionMatchesMultiple :: Char -> Int -> Property
prop_repetitionMatchesMultiple c n =
  property $ match (Repetition (Literal c)) (replicate n c) === True

-- Property 5: Either of the alternatives in Alt should match the string
prop_alternativeMatchesEither :: Char -> Char -> String -> Property
prop_alternativeMatchesEither c1 c2 input =
  property $ match (Alt (Literal c1) (Literal c2)) input === (input == [c1] || input == [c2])

-- Property 6: Concatenating two literals and matching against the concatenated string
-- should be equivalent to matching each literal separately.
prop_concatenationOfLiterals :: Char -> Char -> Property
prop_concatenationOfLiterals c1 c2 =
  property $ match (Concat (Literal c1) (Literal c2)) [c1, c2] === True


main :: IO ()
main = do
  quickCheck prop_epsilonAlwaysMatches
  quickCheck prop_literalNeverMatchesEmpty
  quickCheck prop_literalMatchesSingleChar
  quickCheck prop_repetitionMatchesMultiple
  quickCheck prop_alternativeMatchesEither
  quickCheck prop_concatenationOfLiterals