module: dylan-user
language: infix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library doss-tests
  use functional-dylan;
  use set;
  use equal-table;
  use doss;
  use simple-streams;
  use simple-format;
  use io;
  export doss-tests;
end;

define module doss-tests
  use functional-dylan,
    exclude: {position};
  use simple-streams, exclude: { <stream>, force-output };
  use simple-format;
  use streams,
        import: all,
        rename: {stream-position => position,
                 stream-position-setter => position-setter};
  use mop;
  use set;
  use equal-table;
  use byte-vector;
  use doss-internals;

  export
    do-doss-tests;
end module doss-testing;

// eof
