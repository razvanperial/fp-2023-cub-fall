{-# LANGUAGE DeriveFunctor #-}

module Main where

import Test.QuickCheck

data RegExp a
  = Empty
  | Epsilon
  | Literal a
  | Concat (RegExp a) (RegExp a)
  | Alt (RegExp a) (RegExp a)
  | Iteration (RegExp a)
  deriving (Functor, Show)

-- Smart constructor for repetition (0 or more)
iteration :: RegExp a -> RegExp a
iteration re = Alt Epsilon (Concat re (Iteration re))

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
nullable (Iteration _) = True

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
derivative (Iteration r) a = Concat (derivative r a) (Iteration r)

-- Intersect function returns a regular expression that matches the intersection of two regular expressions
intersect :: RegExp a -> RegExp a -> RegExp a
intersect re1 re2 = Alt (Concat (intersect' re1) re2) (Concat re1 (intersect' re2))
  where
    intersect' Empty = Empty
    intersect' Epsilon = Epsilon
    intersect' (Literal a) = Literal a
    intersect' (Concat r1 r2) = Concat (intersect' r1) (intersect' r2)
    intersect' (Alt r1 r2) = Alt (intersect' r1) (intersect' r2)
    intersect' (Iteration r) = Iteration (intersect' r)

-- Unite function returns a regular expression that matches the union of two regular expressions
unite :: RegExp a -> RegExp a -> RegExp a
unite re1 re2 = Alt re1 re2

-- Complement function returns a regular expression that matches the complement of the given regular expression
complement :: RegExp a -> RegExp a
complement re = Alt (complement' re) Epsilon
  where
    complement' Empty = Epsilon
    complement' Epsilon = Empty
    complement' (Literal a) = Literal a
    complement' (Concat r1 r2) = Concat (complement' r1) (complement' r2)
    complement' (Alt r1 r2) = Alt (complement' r1) (complement' r2)
    complement' (Iteration r) = Iteration (complement' r)

-- Difference function returns a regular expression that matches the difference of two regular expressions
difference :: RegExp a -> RegExp a -> RegExp a
difference re1 re2 = intersect re1 (complement re2)

-- Simplify function returns a regular expression that is equivalent to the given regular expression but is simplified
simplify :: RegExp a -> RegExp a
simplify Empty = Empty
simplify Epsilon = Epsilon
simplify (Literal a) = Literal a
simplify (Concat r1 r2) = Concat (simplify r1) (simplify r2)
simplify (Alt r1 r2) = Alt (simplify r1) (simplify r2)
simplify (Iteration r) = Iteration (simplify r)

-- Property based testing

-- Arbitrary instance for RegExp
instance Arbitrary a => Arbitrary (RegExp a) where
  arbitrary = oneof [ return Empty
                    , return Epsilon
                    , Literal <$> arbitrary
                    , Concat <$> arbitrary <*> arbitrary
                    , Alt <$> arbitrary <*> arbitrary
                    , Iteration <$> arbitrary
                    ]

-- Property for the nullable function
prop_nullable :: RegExp Char -> Bool
prop_nullable re = nullable re == (match re "")

-- Property for the match function
prop_match :: RegExp Char -> [Char] -> Bool
prop_match re input = match re input == match (simplify re) input

-- Property for the simplify function
prop_simplify :: RegExp Char -> Bool
prop_simplify re = match re "" == match (simplify re) ""

-- Property for the unite function
prop_unite :: RegExp Char -> RegExp Char -> [Char] -> Bool
prop_unite re1 re2 input = match (unite re1 re2) input == (match re1 input || match re2 input)

-- Property for the intersect function
prop_intersect :: RegExp Char -> RegExp Char -> [Char] -> Bool
prop_intersect re1 re2 input = match (intersect re1 re2) input == (match re1 input && match re2 input)

main :: IO ()
main = do
  quickCheck prop_nullable
  quickCheck prop_match
  quickCheck prop_simplify
  quickCheck prop_unite
  quickCheck prop_intersect
