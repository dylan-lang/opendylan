carlg 99-01-05
  This compound contains a straightforward conversion of some of
  the Gabriel benchmarks from Common Lisp to Dylan.  (Not all of
  the Gabriel benchmarks are useful in Dylan.)  I tried to only
  add type declarations where they were also in the Common Lisp
  version.

  It would perhaps be useful to create a second version of some
  of the benchmarks after adding a few strategic optimizations
  that are shown up by the colorizer.

  For now the puzzle benchmark hasn't been added to the project
  because it contains a bug that I haven't been able to track
  down.  If anyone knows what this benchmark is actually doing
  then feel free to fix it.  I just did a straight translation
  and was unable to track down the bug.
