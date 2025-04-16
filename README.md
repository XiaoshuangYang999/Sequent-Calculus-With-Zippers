# Sequent Calculus with Zippers

## References

The code in this repository was originally developed as part of the following master thesis:

- Xiaoshuang Yang: *Sequent Calculus with Zippers*.
  University of Amsterdam, 2024.
  <https://eprints.illc.uva.nl/id/eprint/2354>

## How to

You should have the Haskell build tool `stack` installed, via [ghcup](https://www.haskell.org/ghcup/).
For proof visualization, optonally you may want to install [`graphviz`](https://graphviz.org/).

To build the project run `stack build`.

You can use `stack ghci` to run examples like this:

    stack ghci lib/ML.hs lib/Example.hs

    ghci> formForK 3
    (( ☐ (c → (d → (e → a)))) → (( ☐ c) → (( ☐ d) → (( ☐ e) → ( ☐ a)))))
    ghci> nFormForK 3
    (( ☐ (c → (d → (e → (b → a))))) → (( ☐ c) → (( ☐ d) → (( ☐ e) → ( ☐ a)))))

    ghci> isProvableT modal (Example.formForK 3)
    True
    ghci> isProvableZ modal (Example.formForK 3)
    True
    ghci> isProvableT modal (Example.nFormForK 3)
    False
    ghci> isProvableZ modal (Example.nFormForK 3)
    False

In the above `modal` is the name of the logic and proof system.

To run all tests, run `stack test`.

To run all benchmarks, run `stack bench`.
Note that this can take quite long.
