# Regular Expression Matcher

This Haskell program provides a simple implementation of a regular expression matcher using Brzozowski derivatives. The code defines a data type `RegExp` that represents regular expressions and includes combinators such as `Alt` (alternative), `Repetition`, `Optional`, and more. The matcher is implemented using Brzozowski derivatives, allowing for pattern matching against input strings.

## Design Overview

### Data Types
The `RegExp` data type includes the following constructors:
- `Empty`: Represents an empty expression.
- `Epsilon`: Represents an epsilon (empty string) expression.
- `Literal a`: Represents a literal character.
- `Concat (RegExp a) (RegExp a)`: Represents the concatenation of two expressions.
- `Alt (RegExp a) (RegExp a)`: Represents an alternative between two expressions.
- `Repetition (RegExp a)`: Represents zero or more repetitions of an expression.
- `Optional (RegExp a)`: Represents an optional occurrence of an expression.
- `Wildcard`: Represents any character.
- `RepetitionRange Int Int (RegExp a)`: Represents a repetition with a specified range.

### Smart Constructor
The `repetitionRange` function serves as a smart constructor for the `RepetitionRange` constructor. It handles invalid repetition ranges and simplifies the creation of repetition expressions.

### Matcher and Derivative
The `match` function uses Brzozowski derivatives to determine if a regular expression matches a given input string. The `derivative` function computes the Brzozowski derivative of a regular expression with respect to a symbol.

### Nullable
The `nullable` function checks whether a regular expression can match an empty string. It recursively evaluates the structure of the expression to determine its nullability.

## Example Properties

The included property-based tests use QuickCheck to verify some fundamental properties of the regular expression matcher. Properties include checking the behavior of epsilon matching, literal matching, repetition matching, alternative matching, and concatenation matching.


# Properties which relate all combinators

## Union Properties

### Associativity

(r1 `Union` r2) `Union` r3 == r1 `Union` (r2 `Union` r3)

### Commutativity

r1 `Union` r2 == r2 `Union` r1

### Idempotence

r `Union` r == r

### Absorption

r1 `Union` (r1 `Union` r2) == r1

### Distributivity

r1 `Union` (r2 `Intersect` r3) == (r1 `Union` r2) `Intersect` (r1 `Union` r3)

### Identity

r `Union` Empty == r

### Domination

r `Union` Full == Full

## Intersection Properties

### Associativity

(r1 `Intersect` r2) `Intersect` r3 == r1 `Intersect` (r2 `Intersect` r3)

### Commutativity

r1 `Intersect` r2 == r2 `Intersect` r1

### Idempotence

r `Intersect` r == r

### Absorption

r1 `Intersect` (r1 `Union` r2) == r1

### Distributivity

r1 `Intersect` (r2 `Union` r3) == (r1 `Intersect` r2) `Union` (r1 `Intersect` r3)

### Identity

r `Intersect` Full == r

### Domination

r `Intersect` Empty == Empty

## Complement Properties

### Involution

Complement (Complement r) == r

### Intersection

r `Intersect` Complement r == Empty

## Reverse Properties

### Involution

Reverse (Reverse r) == r

### Union

Reverse (r1 `Union` r2) == Reverse r1 `Union` Reverse r2

### Intersection

Reverse (r1 `Intersect` r2) == Reverse r1 `Intersect` Reverse r2

### Complement

Reverse (Complement r) == Complement (Reverse r)

## General Properties

### Concatenation with Epsilon

r `Concat` Epsilon == r

### Concatenation with Empty

r `Concat` Empty == Empty









