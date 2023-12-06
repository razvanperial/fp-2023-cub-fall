module Regexp where

import Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Eq, Ord, Show)

a = Char 'a'
r = Seq a (Star a)

-- stops working at "a*a" and 24 'a'
match :: Regexp -> String -> Bool
match r s = case nullable (foldl (flip derivative) r s) of Epsilon -> True; _ -> False

derivative :: Char -> Regexp -> Regexp
derivative _ Empty = Empty
derivative _ Epsilon = Empty
derivative c (Char a) | a /= c = Empty
                      | otherwise = Epsilon
derivative c (Seq p q) = Alt (Seq (derivative c p) q) (Seq (nullable p) (derivative c q))
derivative c (Alt p q) = Alt (derivative c p) (derivative c q)
derivative c (Star p) = Seq (derivative c p) (Star p)

nullable :: Regexp -> Regexp
nullable Empty = Empty
nullable Epsilon = Epsilon
nullable (Char _) = Empty
nullable (Seq p q) = nullable p `intersect` nullable q
nullable (Alt p q) = nullable p `unite` nullable q
nullable (Star _) = Epsilon

intersect :: Regexp -> Regexp -> Regexp
intersect Epsilon Epsilon = Epsilon
intersect _ _ = Empty

unite :: Regexp -> Regexp -> Regexp
unite Empty Empty = Empty
unite _ _ = Epsilon

-- match :: Regexp -> String -> Bool
-- match r s = nullable (foldl (flip derivative) r s)

-- -- stops working at "a*a" and 8514 'a'
-- derivative :: Char -> Regexp -> Regexp
-- derivative _ Empty = Empty
-- derivative _ Epsilon = Empty
-- derivative c (Char a) | a /= c = Empty
--                       | otherwise = Epsilon
-- derivative c (Seq p q) | not (nullable p) = Seq (derivative c p) q
--                        | otherwise = Alt (Seq (derivative c p) q) (derivative c q)
-- derivative c (Alt p q) = Alt (derivative c p) (derivative c q)
-- derivative c (Star p) = Seq (derivative c p) (Star p)

-- nullable :: Regexp -> Bool
-- nullable Empty = False
-- nullable Epsilon = True
-- nullable (Char _) = False
-- nullable (Seq p q) = nullable p && nullable q
-- nullable (Alt p q) = nullable p || nullable q
-- nullable (Star _) = True

-- match :: Regexp -> String -> Bool
-- match r s = nullable (foldl (flip derivative) r s)

-- derivative :: Char -> Regexp -> Regexp
-- derivative _ Empty = empty
-- derivative _ Epsilon = empty
-- derivative c (Char a) | a /= c = empty
-- derivative _ (Char _) = epsilon
-- derivative c (Seq p q) | not (nullable p) = seq (derivative c p) q
-- derivative c (Seq p q) = alt (seq (derivative c p) q) (derivative c q)
-- derivative c (Alt p q) = alt (derivative c p) (derivative c q)
-- derivative c (Star p) = seq (derivative c p) (star p)

-- empty :: Regexp
-- empty = Empty

-- epsilon :: Regexp
-- epsilon = Epsilon

-- char :: Char -> Regexp
-- char = Char

-- seq :: Regexp -> Regexp -> Regexp
-- seq Empty _ = Empty
-- seq _ Empty = Empty
-- seq Epsilon p = p
-- seq p Epsilon = p
-- seq p q = Seq p q

-- alt :: Regexp -> Regexp -> Regexp
-- alt Empty p = p
-- alt p Empty = p
-- alt Epsilon p = if nullable p then p else Alt Epsilon p
-- alt p Epsilon = if nullable p then p else Alt Epsilon p
-- alt p q | p == q = p
-- alt p q = Alt p q

-- star :: Regexp -> Regexp
-- star Empty = Epsilon
-- star Epsilon = Epsilon
-- star (Star p) = Star p
-- star p = Star p

-- opt :: Regexp -> Regexp
-- opt = alt epsilon

-- nullable :: Regexp -> Bool
-- nullable Empty = False
-- nullable Epsilon = True
-- nullable (Char _) = False
-- nullable (Seq p q) = nullable p && nullable q
-- nullable (Alt p q) = nullable p || nullable q
-- nullable (Star _) = True
