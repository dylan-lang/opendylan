  This library contains a straightforward conversion of most of
  the Gabriel benchmarks [1] from Common Lisp to Dylan.  (Not all of
  the Gabriel benchmarks are useful in Dylan.)  We tried to only
  add type declarations where they were also in the Common Lisp
  version.

  It would perhaps be useful to create a second version of some
  of the benchmarks after adding a few strategic optimizations
  that are shown by dispatch coloring.

  The library is defined as a testworks benchmark suite. Repeat counts
  are based on those used by the cl-bench [2] benchmark suite for
  Common Lisp for ease of comparison.

[1] Gabriel, Richard P. Performance and Evaluation of Lisp Systems
    (MIT Press: Cambridge, Mass, 1985). Available at
    http://dreamsongs.com/Files/Timrep.pdf

[2] https://gitlab.common-lisp.net/ansi-test/cl-bench
