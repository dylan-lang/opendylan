module:    native-rtg
Synopsis:  Functions to generate the HARP for the Dylan runtime
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




///// These functions generate low-level code for the Dylan runtime
///// system - including external entry points etc.




//// This one starts the whole shooting match.

define method create-dylan-runtime 
    (#key back-end-class = <harp-back-end>,
      base-file-name = "runtime",
      client-file-name = "dylan-support",
      print-harp? = #t,
      client? = #t,
      base? = #t,
      type)
  let back-end = make(back-end-class);
  if (base?)
    generate-runtime(back-end, base-file-name, print-harp?, #f, type);
  end if;
  if (client?)
    generate-runtime(back-end, client-file-name, print-harp?, #t, type);
  end if;
end method;


define method generate-runtime 
    (back-end :: <harp-back-end>,
     file-name :: <byte-string>,
     print-harp? :: <boolean>,
     client? :: <boolean>,
     type)
  let make-keys = if (type) vector(type: type) else #[] end;
  let outputter =  apply(make-harp-outputter, 
                         back-end, file-name, 
                         print-harp?: print-harp?, 
                         make-keys);
  block ()
    output-dylan-runtime(back-end, outputter, client?: client?);
  cleanup
    close-harp-outputter(back-end, outputter, filename: file-name);
  end block;
end method;



//// The top level for code generation.
//// Call this to create the code for the entire low-level runtime support.


define method output-dylan-runtime 
    (be :: <harp-back-end>, outputter, #key client? = #f)

  let output-one-fn =
    method (name :: <byte-string>, fn :: <function>, #rest keys)
      apply(invoke-harp, be, fn, name, 
            outputter: outputter, harp-debug: #t, keys);
    end method;

  output-header(be, outputter);
  output-data-start(be, outputter);
  output-data(be, outputter, client?: client?);
  output-glue(be, outputter);
  output-code-start(be, outputter);
  output-functions(be, outputter, output-one-fn, client?: client?);
  output-footer(be, outputter);

end method;







//// Testing

/*
define method make-null-outputter (back-end) => (outputter)
  make-harp-outputter(back-end, "splunge", print-harp?: #f, type: #());
end method;


define method test-data
    (#key back-end = make(<harp-back-end>),
          outputter = make-interactive-outputter(),
          client? = #f)
  output-data(back-end, outputter, client?: client?);
end method;


define method test-functions
    (#key back-end = make(<harp-back-end>),
          outputter = make-interactive-outputter(),
          limit = 2,
          client? = #f)

  let output-one-fn =
    method (name :: <byte-string>, fn :: <function>, #rest keys)
      apply(invoke-harp, back-end, fn, name, 
            outputter: outputter, harp-debug: #t, keys);
    end method;

  output-data(back-end, make-null-outputter(back-end), client?: client?);
  output-functions(back-end, outputter, output-one-fn, limit: limit, client?: client?);
end method;
*/

