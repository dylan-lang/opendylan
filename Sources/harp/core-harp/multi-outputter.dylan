module:    harp-outputter
Synopsis:  HARP output generation
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// The harp-outputter module provides no direct output targets,
//// but it does provide support for outputting to multiple targets
//// at a time. The mechanism is invoked by passing a sequence as the
//// type to make-harp-outputter. The support is provided here.


define sealed class <harp-multiple-outputter> (<harp-outputter>)
  constant slot internal-outputters :: <simple-object-vector>,
    required-init-keyword: outputters:;
end class;

define method multiplex-outputters 
      (#rest outputters) => (mux :: <harp-multiple-outputter>)
  make(<harp-multiple-outputter>, 
       outputters: as(<simple-object-vector>, outputters));
end method;


define method do-outputters 
    (function :: <function>, multi :: <harp-multiple-outputter>) => ()
  for (outputter in multi.internal-outputters)
    do-outputters(function, outputter);
  end for;
end method;

define method do-outputters 
    (function :: <function>, outputter :: <harp-outputter>) => ()
  function(outputter);
end method;


define method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type :: <sequence>)
    => (output-stream :: <harp-multiple-outputter>)
  make(<harp-multiple-outputter>,
       outputters: 
       map-as(<simple-object-vector>,
	      method (val)
	        make-harp-outputter-by-type(backend, filename, val);
              end method,
              type));
end method;

define method close-harp-outputter
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>,
     #rest keys, #key, #all-keys) => ()
  for (outputter in multi.internal-outputters)
    apply(close-harp-outputter, backend, outputter, keys);
  end for;
end method;


define method model-object-protocol
    (multi :: <harp-multiple-outputter>) => ()
  for (outputter in multi.internal-outputters)
    model-object-protocol(outputter);
  end for;
end method;


define method dynamic-linking-protocol
    (multi :: <harp-multiple-outputter>) => ()
  for (outputter in multi.internal-outputters)
    dynamic-linking-protocol(outputter);
  end for;
end method;


define method output-comment
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>, 
     comment :: <string>) => ();
  for (outputter in multi.internal-outputters)
    output-comment(backend, outputter, comment);
  end for;
end method;

define method output-line-comment
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>, 
     comment :: <string>) => ();
  for (outputter in multi.internal-outputters)
    output-line-comment(backend, outputter, comment);
  end for;
end method;


define method output-external 
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>, name,
    #rest all-keys, #key, #all-keys)
    => ()
  for (outputter in multi.internal-outputters)
    apply(output-external, backend, outputter, name, all-keys)
  end for;
end method;


define method output-public
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>, name,
     #rest all-keys, #key, #all-keys) => ()
  for (outputter in multi.internal-outputters)
    apply(output-public, backend, outputter, name, all-keys);
  end for;
end method;


define method output-export
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>, name) => ()
  for (outputter in multi.internal-outputters)
    output-export(backend, outputter, name);
  end for;
end method;


define method output-definition
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>, name,
     #rest all-keys, #key, #all-keys)
     => ()
  for (outputter in multi.internal-outputters)
    apply(output-definition, backend, outputter, name, all-keys);
  end for;
end method;


define method output-variable
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>, 
     name, initial-value, 
     #rest all-keys, #key, #all-keys) => ()
  for (outputter in multi.internal-outputters)
    apply(output-variable, backend, outputter, name, initial-value, all-keys);
  end for;
end method;


define method output-header 
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>) => ()
  for (outputter in multi.internal-outputters)
    output-header(backend, outputter);
  end for;
end method;


define method output-footer 
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>) => ()
  for (outputter in multi.internal-outputters)
    output-footer(backend, outputter);
  end for;
end method;


define method output-data-start 
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>) => ()
  for (outputter in multi.internal-outputters)
    output-data-start(backend, outputter);
  end for;
end method;


define method output-code-start 
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>) => ()
  for (outputter in multi.internal-outputters)
    output-code-start(backend, outputter);
  end for;
end method;


define method output-glue-symbols
    (backend :: <harp-back-end>, multi :: <harp-multiple-outputter>,
     #rest all-keys,
     #key, #all-keys) => ()
  for (outputter in multi.internal-outputters)
    apply(output-glue-symbols, backend, outputter, all-keys);
  end for;
end method;


define method output-data-item
    (backend :: <harp-back-end>, 
     multi :: <harp-multiple-outputter>, 
     item,
     #rest all-keys, #key, #all-keys) => ()
  for (outputter in multi.internal-outputters)
    apply(output-data-item, backend, outputter, item, all-keys);
  end for;
end method;

define method output-data-footer
    (backend :: <harp-back-end>, 
     multi :: <harp-multiple-outputter>, 
     item,
     #rest all-keys, #key, #all-keys) => ()
  for (outputter in multi.internal-outputters)
    apply(output-data-footer, backend, outputter, item, all-keys);
  end for;
end method;


define method output-data-byte
    (backend :: <harp-back-end>, 
     multi :: <harp-multiple-outputter>, 
     byte) => ()
  for (outputter in multi.internal-outputters)
    output-data-byte(backend, outputter, byte);
  end for;
end method;


define method output-compiled-lambda
    (backend :: <harp-back-end>, 
     multi :: <harp-multiple-outputter>, 
     item :: <fully-compiled-lambda>,
     #rest all-keys, #key, #all-keys) => ()
  for (outputter in multi.internal-outputters)
    apply(output-compiled-lambda, backend, outputter, item, all-keys);
  end for;
end method;



define method outputter-downloadable-data
    (be :: <harp-back-end>, multi :: <harp-multiple-outputter>) => (data)
 any?(curry(outputter-downloadable-data, be), multi.internal-outputters)
end method;

