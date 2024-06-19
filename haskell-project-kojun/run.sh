#!/bin/bash

# Compiles all modules into a KojunSolver executable
cd modules
ghc -o KojunSolver Main.hs

# Executes the Solver
exec ./KojunSolver