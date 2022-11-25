[![Gitter](https://badges.gitter.im/dylan-lang/general.svg)](https://gitter.im/dylan-lang/general?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge) [![libraries-test-suite](https://github.com/dylan-lang/opendylan/actions/workflows/libraries-test-suite.yml/badge.svg)](https://github.com/dylan-lang/opendylan/actions/workflows/libraries-test-suite.yml)

# Welcome to Open Dylan!

Open Dylan is a compiler and a set of libraries for the [Dylan programming
language](http://opendylan.org/books/drm).

If you're reading this inside of a binary release that you just downloaded and
unpacked, then this is all you need to do to get started:

  ```
  $ export PATH=/path/to/opendylan-2022.1/bin:$PATH    # for bash
  ```

Verify that the downloaded version is working correctly by building a
hello-world binary:

  ```
  make-dylan-app hello-world
  cd hello-world
  dylan-compiler -build hello-world
  _build/bin/hello-world
  ```

Note: if there is no `_build` directory already, dylan-compiler will create it
and build all used libraries.  Subsequent builds will be much faster since they
won't need to rebuild core libraries (as long as you always run the compiler in
the same directory).

## What Next?

If this is your first time trying Open Dylan, take a look at the [Getting
Started guide](http://opendylan.org/documentation/getting-started-cli/).

See also:

*  [BUILDING.rst](BUILDING.rst) - how to build the compiler and IDE
*  [How to Contribute to Open
   Dylan](https://opendylan.org/documentation/hacker-guide/contribute.html)
*  [opendylan.org](https://opendylan.org) - our main website
