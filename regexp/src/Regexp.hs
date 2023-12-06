module Regexp where

import Prelude hiding (seq, repeat)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Eq, Ord, Show)

a = Char 'a'
r = Seq a (Star a)

nullable :: Regexp -> Bool
nullable Empty = False
nullable Epsilon = True
nullable (Char _) = False
nullable (Seq p q) = nullable p && nullable q
nullable (Alt p q) = nullable p || nullable q
nullable (Star _) = True

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

derivative :: Char -> Regexp -> Regexp
derivative _ Empty = empty
derivative _ Epsilon = empty
derivative c (Char a) | a /= c = empty
derivative _ (Char _) = epsilon
derivative c (Seq p q) | not (nullable p) = seq (derivative c p) q
derivative c (Seq p q) = alt (seq (derivative c p) q) (derivative c q)
derivative c (Alt p q) = alt (derivative c p) (derivative c q)
derivative c (Star p) = seq (derivative c p) (star p)

empty :: Regexp
empty = Empty

epsilon :: Regexp
epsilon = Epsilon

char :: Char -> Regexp
char = Char

seq :: Regexp -> Regexp -> Regexp
seq Empty _ = Empty
seq _ Empty = Empty
seq Epsilon p = p
seq p Epsilon = p
seq p q = Seq p q

alt :: Regexp -> Regexp -> Regexp
alt Empty p = p
alt p Empty = p
alt Epsilon p = if nullable p then p else Alt Epsilon p
alt p Epsilon = if nullable p then p else Alt Epsilon p
alt p q | p == q = p
alt p q = Alt p q

star :: Regexp -> Regexp
star Empty = Epsilon
star Epsilon = Epsilon
star (Star p) = Star p
star p = Star p

opt :: Regexp -> Regexp
opt = alt epsilon

repeat :: Int -> Regexp -> Regexp
repeat 0 _ = epsilon
repeat 1 p = p
repeat n p = seq p (repeat (n - 1) p)

-- nullable :: Regexp -> Bool
-- nullable Empty = False
-- nullable Epsilon = True
-- nullable (Char _) = False
-- nullable (Seq p q) = nullable p && nullable q
-- nullable (Alt p q) = nullable p || nullable q
-- nullable (Star _) = True
