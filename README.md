# Sequent Calculus with Zippers

This repository contains the Haskell implementation of Xiaoshuang Yang's Master's thesis. The project develops a sequent calculus prover using **zippers** — a family of data structures that simplify navigation and structural updates in tree.

📄 [Read the thesis (PDF)](https://eprints.illc.uva.nl/id/eprint/2354/1/MoL-2024-23.text.pdf)

> Yang, X. (2024). *Sequent Calculus with Zippers*. Master of Logic Thesis, University of Amsterdam.

We are continuing the development of this prover.
The `thesis-version` branch contains the original code from the thesis,
while ongoing updates and improvements will be done in the `main` branch.

## Bash Instructions

Use `stack build` to build the project.

Use `stack test` to run tests.

Use `stack bench` to run all benchmarks

Use `stack bench :Bench` to run time benchmark.

Use `stack bench :Memory` to run memory benchmark.
