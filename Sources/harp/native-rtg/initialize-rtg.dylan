module:    native-rtg
Synopsis:  Functions to initialize Dylan Native runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Support for a dummy initialization of the base runtime (to a bit bucket). 
// This will be necessary in order to permit generation of a client 
// runtime without generating a base first.


define method make-null-outputter (back-end) => (outputter)
  make-harp-outputter(back-end, "splunge", print-harp?: #f, type: #());
end method;



define method dummy-generate-runtime
    (back-end,
     #key outputter = make-null-outputter(back-end),
          limit = 1,
          client? = #f)

  let output-one-fn =
    method (name :: <byte-string>, fn :: <function>, #rest keys)
      apply(invoke-harp, back-end, fn, name, 
            outputter: outputter, harp-debug: #t, keys);
    end method;

  output-data(back-end, outputter, client?: client?);
  output-functions(back-end, outputter, output-one-fn, limit: limit, client?: client?);
end method;


