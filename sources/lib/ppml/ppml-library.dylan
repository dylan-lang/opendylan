Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library ppml
  use common-dylan;
  use io;
  export ppml;
end library;

define module ppml
  use common-dylan, exclude: { format-to-string };
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
