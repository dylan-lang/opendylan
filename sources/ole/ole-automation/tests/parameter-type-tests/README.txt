This is in a little test suite I wrote to exercise various types as
parameters in vtable COM interfaces.  It defines a macro for the
tests, and various parameter types to test.  I've commented out the
tests which fail, along with the reason for their failures.  There
are a few failures which I didn't track down and bug-report - the
next COM person should take a look at them.

Building and executing:

  - Build the server (parameter-type-tests-server).  Register it via
    'regsvr32 parameter-type-tests-server.dll'.  Due to bug 3915, you
    might have to eviscerate the 'define interface-test' temporarily.

  - Build and execute the server.

Here are some of the problems I encountered:

  This is really frustrating - I've run into an awful lot of
  problems testing custom interfaces in their current state.  Here
  are some of the issues I encountered trying to get the test suite
  working:

    - Bug 4225: <C-float> and <C-double> parameters NAM compiler.
    - Various types (<C-unsigned-long>, others) result in odd
      compiler errors.  Haven't tracked down well enough for a bug
      report.
    - Can't use numbers outside the range of an <integer> with a
      custom-interface <C-int> or similar parameter.
    - Bug 3915 causes problems for certain types.
    - Bug 3913 (custom COM interface only works in-process) makes it
      harder to test.

  Currently information about mapping between C designator types and
  Dylan types, and C designator types and pointer types is encoded
  in several places, and not entirely consistently.  To fix this, I
  recommend we make the C FFI supply a function to map from a C
  designator type to the Dylan types it can map to/from.  (E.g.,
  <C-int> maps both to and from type-union(<machine-word>,
  <integer>).) and use this function in the macros.

  Bug 3915 means that if any member function uses a type which our
  type info support can't handle (such as <C-unsigned-char>), the
  program crashes on startup.  I think it's pretty critical to fix
  this (I don't think it's P3, more like P1 or P2).  A temporary
  "fix" might just consist of having the type info support print a
  warning and insert the wrong type into the type library (as it
  does for pointer types) rather than crashing, since the type
  library details aren't important for many uses of
  vtable-interfaces.
