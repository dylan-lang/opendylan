module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library news-app
  use apple-dylan;
  use c-ffi;
  // use mac-toolbox;
  // use Dylan-Framework;
  // use apple-tcp-streams;
  
  export news-app;
end library;

define module news-app
  use apple-dylan;
  use c-ffi;
  // use mac-toolbox;
  // use Dylan-Framework,
  //   export: { start };
  // use apple-tcp-streams,
  //   exclude: {read-string, write-string};
  
  export 
     // init-app, run-app;
     set-query,   // (query-string :: <string>) => c-boolean :: <integer>
     query-text;  // (text :: <string>) => c-boolean :: <integer>
end module;
