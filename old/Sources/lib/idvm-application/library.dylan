module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library idvm-application

  use dylan;
  use simple-streams;
  use simple-format;
  use socket-streams;
  use c-ffi;
  use idvm-namespace;
  use doss;
  use tcp-streams;

  use idvm;
  use idvm-loader;
  use doss-names;
  use idvm-names;

  export idvm-application;

end library idvm-application;

define module idvm-application

  use dylan;
  use simple-streams, rename: { <stream> => <simple-stream> };
  use simple-format;
  use socket-streams;
  use c-ffi;
  use idvm-namespace, import: { *namespace-debug-print*, query-error, *idvm-library-namespace*, idvm-namespace };
  use doss;
  use tcp-streams;

  use idvm;
  use idvm-loader;
  use doss-names;
  use idvm-names;


end module idvm-application;