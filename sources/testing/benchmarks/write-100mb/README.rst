Write 10_000MB, best of three runs on an Apple M3 Pro, May 2024:

       Python  SBCL     Dylan    Racket
real   7.500s  13.837s  23.425s  30.620s
       1.0x     1.8x     3.1x     4.1x
user   4.837s  11.334s   6.455s  21.053s
       1.0x     2.3x     1.3x     4.4x
sys    2.286s   2.368s  10.749s   7.031s


Note that the other implementations are using Unicode so comparison isn't
really fair. In particular, Racket does better than SBCL if the test is changed
to use a byte string (#"xxx") but has a particularly pessimized Unicode path,
it seems.
