# smartviews
A Smart View on Datatypes

Benchmarking code from https://www.fceia.unr.edu.ar/~mauro/pubs/smartviews/smartviews.pdf by Mauro Jaskelioff and Exequiel Rivas

Left-nested list concatenations, left-nested binds on the free monad, and left-nested choices in many non-determinism monads have an algorithmically bad performance. Can we solve this problem without losing the ability to pattern-match on the computation? Surprisingly, there is a deceptively simple solution: use a smart view to pattern-match on the datatype. We introduce the notion of smart view and show how it solves the problem of slow left- nested operations. In particular, we use the technique to obtain fast and simple implementations of lists, of free monads, and of two non-determinism monads.

The original code can be found at https://www.fceia.unr.edu.ar/~mauro/pubs/smartviews/. This version has been updated to use cabal new-build and ghc 8.6.1
