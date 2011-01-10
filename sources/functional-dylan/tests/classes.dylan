Module:    functional-dylan-test-suite
Synopsis:  Functional Objects extensions library test suite
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Class test cases

define functional-extensions class-test <byte-character> ()
  check-true("byte-string[0] is a <byte-character>",
             instance?("abc"[0], <byte-character>))
end class-test <byte-character>;

define functional-extensions class-test <format-string-condition> ()
  //---*** Fill this in...
end class-test <format-string-condition>;

define functional-extensions class-test <object-set> ()
  //---*** Fill this in...
end class-test <object-set>;

define functional-extensions class-test <set> ()
  //---*** Fill this in...
end class-test <set>;

define functional-extensions class-test <simple-condition> ()
  //---*** Fill this in...
end class-test <simple-condition>;

define functional-extensions class-test <stretchy-sequence> ()
  //---*** Fill this in...
end class-test <stretchy-sequence>;


/// simple-random classes

define simple-random class-test <random> ()
  //---*** Fill this in...
end class-test <random>;
