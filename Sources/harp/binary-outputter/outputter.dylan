module:    binary-outputter
Synopsis:  Support for assembling and dumping BINARY files
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Binary Outputter


define open class <harp-binary-builder>(<binary-builder>, <harp-outputter>)
  slot model-object-protocol? = #f;
end class;

define method model-object-protocol(outputter :: <harp-binary-builder>) => ()
    outputter.model-object-protocol? := #t;
end method;

define method dynamic-linking-protocol(builder :: <harp-binary-builder>) => ()
    builder.dynamic-linking? := #t;
end method;

define method close-harp-outputter
    (backend :: <harp-back-end>, outputter :: <harp-binary-builder>,
     #key filename) => ()
  let dest = outputter.destination;
  let def = outputter.def-file;
  if (dest) close-output-stream(dest) end;
  if (def) close-output-stream(def) end;
  assemble-harp-outputter(outputter, filename);
end method;

define open generic assemble-harp-outputter
    (outputter :: <binary-builder>, filename) => ();

define method assemble-harp-outputter
    (outputter :: <binary-builder>, filename) => ()
end method;

define method output-data-item  
    (be :: <harp-back-end>,
     builder :: <harp-binary-builder>,
     item :: <abstract-integer>,
     #key model-object = unsupplied(),
     #all-keys) => ()
  add-data(builder, item, model-object);
end method;

define method output-data-item  
    (be :: <harp-back-end>, builder :: <harp-binary-builder>, item :: <byte-string>,
     #key import?,
          model-object = unsupplied(),
          offset) => ()
  if (import? & *dll-support*)
    add-imported-data(builder, item, model-object, offset);
  else
    add-data(builder, item, model-object);
  end if;
end method;

define method output-data-item  
    (be :: <harp-back-end>, 
     builder :: <harp-binary-builder>, 
     item :: <constant-reference>,
     #key import?, offset,
     #all-keys) => ()
  let import? = import? | instance?(item, <imported-constant-reference>);
  let (name, model-object) = canonical-code-object(builder, item);

  if (import? & *dll-support*)
    add-imported-data(builder, name, model-object, offset);
  else
    add-data(builder, name, model-object);
  end if;
end method;

define method output-data-byte 
    (be :: <harp-back-end>, builder :: <harp-binary-builder>, item :: <integer>)
 => ()
  add-data-byte(builder, item);
end method;

define method output-data-byte 
    (be :: <harp-back-end>, builder :: <harp-binary-builder>, item :: <string>)
 => ()
  add-data-string(builder, item);
end method;


define open generic do-export
    (export?, builder :: <harp-binary-builder>, name :: <byte-string>) => ();

define method do-export
    (export?, builder :: <harp-binary-builder>, name :: <byte-string>) => ()
  // If the value of export? is the symbol #"code-stub", then
  // the export will include a code stub in the client library.
  // Otherwise it will not, and the client must indirect through the 
  // import table directly
  if (export?)
    let code-stub? = export? == #"code-stub";
    add-symbol-export(builder, name, code-stub?: code-stub?);
    add-symbol-def(builder, name);
  end if;
end method do-export;

define method output-export
    (be :: <harp-back-end>, builder :: <harp-binary-builder>, name :: <byte-string>)
 => ()
  do-export(#t, builder, name);
end method;

define method output-export
    (be :: <harp-back-end>, 
     builder :: <harp-binary-builder>, 
     name :: <constant-reference>) => ()
  output-export(be, builder, name.cr-refers-to);
end method;

define method output-variable
    (be :: <harp-back-end>, builder :: <harp-binary-builder>, 
     name :: <byte-string>, initial-value,
     #key repeat, section, import-value?, public?, export? = public?.and-force-dll-exports?,
          model-object = unsupplied()) => ()
  output-definition(be, builder, name,
                    section: section, public?: public?,
                    model-object: model-object, export?: export?);
  if (repeat)
    for (i from 0 below repeat)
      output-data-item(be, builder, initial-value, import?: import-value?);
    end for;
  else
    output-data-item(be, builder, initial-value, import?: import-value?);
  end if;
end method;

define method output-variable
    (be :: <harp-back-end>, builder :: <harp-binary-builder>, 
     name :: <constant-reference>, initial-value,
     #rest all-keys,
     #key repeat, section, import-value?, public?, export?,
     #all-keys) => ()
  let (name, model-object) = canonical-code-object(builder, name);

  apply(output-variable, be, builder, 
        name, initial-value, model-object: model-object,
        all-keys);
end method;
