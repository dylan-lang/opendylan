module:    idvm-harp
Synopsis:  DOSS generation from the HARP outputter interface
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// The interface between the harp back end and DOSS
////
//// This file defines the outputter types #"doss" and #"doss-no-stream"

define constant $doss-type$ = #"doss";

define constant $doss-no-stream-type$ = #"doss-no-stream";

// Class <idvm-unstreamed-doss-dumper> behaves just like its superclass
// - except that idvm-harp will not try to open or close its stream.
// The stream can be set with doss-stream-setter.
//
define class <idvm-unstreamed-doss-dumper> (<doss-dumper>)
end class;


define method doss-stream-setter (new-stream :: <stream>, outputters)
  do-outputters(method (outputter)
                  if (instance?(outputter, <idvm-unstreamed-doss-dumper>))
                    outputter.stream := new-stream;
                  end if;
                end method,
                outputters);
end method;


define method file-extension-for-outputter-type
       (backend :: <idvm-back-end>, type == #"doss") 
       => (extension :: <byte-string>)
  "doss";
end method;


define method open-doss-stream
      (back-end :: <idvm-back-end>, 
       file-name :: <byte-string>, 
       type == $doss-type$) 
      => (s :: <stream>)
  let extension = file-extension-for-outputter-type(back-end, type);
  let full-name = concatenate(file-name, ".doss");
  make(<file-stream>, locator: as(<file-locator>, full-name),
       if-exists: #"truncate", element-type: <byte>, direction: #"output");
end method;


define method open-doss-stream
      (back-end :: <idvm-back-end>, 
       file-name :: <locator>, 
       type) 
      => (s :: <stream>)
  open-doss-stream(back-end, as(<string>, file-name), type);
end method;


define method make-harp-outputter-by-type
    (backend :: <idvm-back-end>, 
     filename,
     type == $doss-type$)
    => (outputter :: <doss-dumper>)
  make(<doss-dumper>, 
       stream: open-doss-stream(backend, filename, type),
       policy: make(<idvm-code-dumping-policy>));
end method;


define method make-harp-outputter-by-type
    (backend :: <idvm-back-end>, 
     filename,
     type == $doss-no-stream-type$)
    => (outputter :: <idvm-unstreamed-doss-dumper>)
  make(<idvm-unstreamed-doss-dumper>, 
       policy: make(<idvm-code-dumping-policy>));
end method;



define method close-harp-outputter
    (backend :: <idvm-back-end>, outputter :: <doss-dumper>) => ()
  close-output-stream(outputter.stream);
end method;

define method close-harp-outputter
    (backend :: <idvm-back-end>, outputter :: <idvm-unstreamed-doss-dumper>)
     => ()
  force-output(outputter.stream);
end method;



// Most of the rest of the protocol is empty, because the IDVM dumping
// model is tailored to support DOSS is a more direct manner. Objects
// are only dumped as variable and constant definitions - and these
// only happen after all code generation has been performed.

define method output-code-start 
    (be :: <idvm-back-end>, outputter :: <doss-dumper>)
end method;

define method output-data-start 
    (be :: <idvm-back-end>, outputter :: <doss-dumper>)
end method;


define method output-header
    (be :: <idvm-back-end>, outputter :: <doss-dumper>)
  put-header(outputter);
end method;

define method output-footer
    (be :: <idvm-back-end>, outputter :: <doss-dumper>)
end method;

define method output-footer
    (be :: <idvm-back-end>, outputter :: <idvm-unstreamed-doss-dumper>)
  put-footer(outputter);
end method;

define method output-external 
    (be :: <idvm-back-end>, outputter :: <doss-dumper>,  name)
end method;

define method output-public
    (be :: <idvm-back-end>, outputter :: <doss-dumper>, name)
end method;

define method output-definition
    (be :: <idvm-back-end>, 
     outputter :: <doss-dumper>, 
     name,
     #key section) => ()
end method;


define method output-definition
    (be :: <idvm-back-end>, 
     outputter :: <doss-dumper>, 
     definer :: <definition-ref>,
     #key section) => ()
  put-object(definer, outputter);
end method;


// hijack output-definition to support the calling of top-level forms. 
// This is somewhat of a hack - but at least it avoids having to
// introduce a new function into the outputter protocol.

define method output-definition
    (be :: <idvm-back-end>, 
     outputter :: <doss-dumper>, 
     top-level-lambda :: <local-constant-reference>,
     #key section) => ()
  put-apply(#[], outputter, top-level-lambda);
end method;

define method output-variable
    (be :: <idvm-back-end>, outputter :: <doss-dumper>, 
     name, initial-value,
     #key repeat, section, public?) => ()
end method;


define method output-comment
    (be :: <idvm-back-end>, outputter :: <doss-dumper>, comment :: <string>) 
     => ()
end method;


// And the code generation support:


define method output-lambda-preamble 
      (be :: <idvm-back-end>, outputter :: <doss-dumper>)
end method;

define method output-lambda-postamble 
      (be :: <idvm-back-end>, outputter :: <doss-dumper>)
end method;

define method output-data-item  
    (be :: <idvm-back-end>, outputter :: <doss-dumper>, item)
end method;

define method output-data-byte 
    (be :: <idvm-back-end>, outputter :: <doss-dumper>, item)
end method;

define method output-code-item  
    (be :: <idvm-back-end>, outputter :: <doss-dumper>, item)
end method;

