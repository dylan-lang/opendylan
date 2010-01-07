Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library ppml
  use functional-dylan;
  use io;
  export ppml;
end library;

define module ppml
  use functional-dylan;
  use streams;
  use format;
  use print;
  use standard-io;
  use format-out;

  export <ppml>,
           <ppml-string>,
           <ppml-break>,
           <ppml-block>,
             <ppml-separator-block>,
           <ppml-suspension>,
           <ppml-browser-aware-object>,
         <ppml-break-type>,
         $line-break,
         ppml-format-string,
         format-to-ppml;

  export ppml-string,
         ppml-break,
         ppml-block,
         ppml-separator-block,
         ppml-suspension,
         ppml-browser-aware-object;

  export <ppml-printer>,
         ppml-print,
         ppml-print-one-line;
end module;
