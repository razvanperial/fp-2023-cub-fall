1. Select a functional language you want (or several) =)
2. Implement RB-trees in both functional and imperative-like faction.
   1. implement function `fromOrdList :: List -> Tree` which converts sorted list with no repeats to a RB-tree running in `O(n)`
   2. Reduce number of unnecessary checks in function `balance`
      1. split `balance` onto two functions `lbalance` and `rbalance` (checks that left and right child ensures invariants); use them in `ins` function instead of `balance`
      2. Rewrite `ins` in a way it never checks the colors of his grandchildren
   3. Implement benchmarks that compare the actual execution times of these implementations
3. Implement all 4 queues; Implement benchmarks that compare the actual execution times of these implementations