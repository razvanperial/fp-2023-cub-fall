# HW11

## Deadline: 05.12.2023, 23:59

### Assigned by @kajigor

1. Design a library for regular expressions.
   * There should be a set of combinators which faces user which includes those not used in the core regular expression language, such as `?`, `+`, wildcard, `{min, max}` for repetitions and so forth.
   * The core language should be small.
   * Come up with the properties which relate all combinators, including the extra user-facing combinators together.
   * Implement the matcher for regular expressions using [Brzozowski derivatives](https://en.wikipedia.org/wiki/Brzozowski_derivative).
   * Implement property-based test.
   * Supplement your code with a README which goes over the design, including the discovered properties.