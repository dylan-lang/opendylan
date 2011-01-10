module:    harp-outputter
Synopsis:  HARP debug printing support via the outputter protocol
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// The harp-outputter module provides support for printing HARP
//// instructions etc. via output-compiled-lambda et al. This is a slightly
//// special implementation of the protocol (in that the implementation
//// responds to and ignores different function calls). However, it is 
//// convenient to create and close the streams with the same mechanism.
////


//// This file defines the outputter type #"print-harp"

define constant $harp-type$ = #"print-harp";


/// Back ends may specialize FILE-EXTENSION-FOR-OUTPUTTER-TYPE
/// to get different file extensions.
///

define method file-extension-for-outputter-type
       (backend :: <harp-back-end>, type == $harp-type$) 
       => (extension :: <byte-string>)
  "harp";
end method;



/// This abstract class serves just to distinguish the created object from
/// a stream, so that we can do something different at closing time.
///
define abstract primary class <stream-wrapper-outputter> (<harp-outputter>)
  slot outputter-line-pos :: <integer> = 0;
  constant slot outputter-stream :: <stream>,
    required-init-keyword: stream:;
end class;

/// This instantiable class is used as the outputter for HARP debug printing.
///
define sealed class <harp-print-outputter> (<stream-wrapper-outputter>)
end class;



define method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type == $harp-type$)
    => (output-stream :: <harp-print-outputter>)
  make(<harp-print-outputter>,
       stream: open-output-stream(backend, filename, type));
end method;



define method close-harp-outputter
    (backend :: <harp-back-end>, outputter :: <harp-print-outputter>,
     #key, #all-keys) => ()
  close-output-stream(outputter.outputter-stream);
end method;




define method output-comment
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, comment) => ()
  format(outputter.outputter-stream, "\n\n+++++ %s +++++\n", comment);
end method;

define method output-line-comment
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, comment) => ()
  format(outputter.outputter-stream, "  +++++ %s +++++", comment);
end method;


define method output-external 
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, name,
     #key import?, #all-keys) => ()
  let prefix = if (import?) "imported " else "" end;
  format(outputter.outputter-stream, "+++ %sexternal: %= +++\n", prefix, name);
end method;

define method output-external 
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, name :: <constant-reference>,
     #rest all-keys, #key import?, #all-keys) => ()
     apply(output-external, be, outputter, name.cr-refers-to, all-keys);
end method;


define method output-public 
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, name,
     #key export?,
     #all-keys) => ()
  let export-str = if (export?) " (exported)" else "" end;
  format(outputter.outputter-stream, "+++ public: %=%s +++\n", name, export-str);
end method;

define method output-public 
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, name :: <constant-reference>,
     #rest all-keys, #key, #all-keys) => ()
     apply(output-public, be, outputter, name.cr-refers-to, all-keys);
end method;


define method output-export 
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, name) => ()
  format(outputter.outputter-stream, "+++ export: %= +++\n", name);
end method;


define method output-definition
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, name,
     #key section, public?, export?,
          model-object = unsupplied()) => ()
  format(outputter.outputter-stream, 
         "\n\n+++ defining %s%s%=%s +++",
         if (public?) "(public) " else "" end,
         if (export?) "(export) " else "" end,
         canonical-data-object(name, model-object),
         if (section)
           concatenate(" in section ", as(<string>, section))
         else "" 
         end if);
end method;

define method output-definition
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, name :: <constant-reference>,
     #rest all-keys, #key section, public?, export?,
     #all-keys) => ()
    apply(output-definition, be, outputter, name.cr-refers-to, all-keys);
end method;


define method output-variable
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, 
     name, initial-value, 
     #key repeat, section, import-value?, public?, export?,
          model-object = unsupplied()) => ()
  format(outputter.outputter-stream, 
         "\n+++ %s%svariable%s %= = %=%s +++\n", 
         if (public?) "(public) " else "" end,
         if (export?) "(export) " else "" end,
         canonical-data-object(name, model-object), 
         if (section)
           concatenate(" in section ", as(<string>, section))
         else "" 
         end if,
         initial-value,
         if (import-value?) " (imported)" else "" end);
end method;


define method output-header 
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>) => ()
end method;


define method output-footer 
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>) => ()
end method;


define method output-data-start 
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>) => ()
  format(outputter.outputter-stream, "\n+++ start of data +++\n\n");
end method;


define method output-data-item
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, item,
     #key import? = #f,
          model-object = unsupplied(),
          offset) => ()
  let prefix = if (import?) "(imported) " else "" end;
  let offset =
    if (offset)
      concatenate("(offset ", integer-to-string(offset), ") ")
    else "" end;
  format(outputter.outputter-stream, "\n\tDATA %s%s%=",
	 prefix, offset,
         canonical-data-object(item, model-object));
end method;

define method output-data-item
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, item :: <constant-reference>,
     #key import? = #f, offset, #all-keys) => ()
    output-data-item(be, outputter, item.cr-refers-to,
    		     import?: import?, offset: offset);
end method;


define method output-data-byte
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, byte) => ()
  format(outputter.outputter-stream, "\n\tBYTE %=", byte);
end method;


// This is the method definition that the outputter type exists to enable:

define method output-compiled-lambda
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>, 
     item :: <fully-compiled-lambda>,
     #key section = #"code",
     #all-keys)
    => ()
  let stream = outputter.outputter-stream;
  let name = item.lambda-name;
  unless (section == #"code")
    format(stream, "\n+++ Defining function %s in section %s +++\n", 
           name, as(<string>, section));
  end unless;
  unless (item.lambda-is-public?)
    format(stream, "\n+++ Static function definition %s +++\n", name);
  end unless;
  if (item.lambda-is-export?)
    format(stream, "\n+++ Exported function definition %s +++\n", name);
  end if;
  if (item.lambda-referenced-data)
    let len = item.lambda-referenced-data.size;
    format(stream, "\n+++ Referenced data of size %d for %s +++\n", len, name);
  end if;
  if (item.lambda-harp-print-info)
    format(stream, "%s\n", item.lambda-harp-print-info);
  else
    format(stream, "\n+++ No HARP print for %s +++\n", name);
  end if;
  unless (item.lambda-externals.empty?)
    format(stream, "\n+++ External references for %s +++\n", name);
    for (ext :: <constant-reference> in item.lambda-externals)
      format(stream, "  +++ external: %= +++\n", ext);
    end for;
  end;
  unless (item.lambda-frame-g-size.zero? & item.lambda-frame-n-size.zero?)
    format(stream, "\n+++ Frame sizes for %s: GC: %d, NC: %d +++\n", 
           name, item.lambda-frame-g-size, item.lambda-frame-n-size);
  end;
  let debug-scopes :: <debug-scopes> = item.lambda-variable-scopes-internal;
  let all-scopes :: <simple-object-vector> = item.lambda-all-variable-scopes;
  let all-names :: <simple-object-vector> = item.lambda-all-variable-names;
  unless (debug-scopes.empty-debug-scopes?)
    format(stream, "\n+++ Live variable scopes for %s +++\n", name);
    print-debug-scopes(be, debug-scopes, stream,
		       all-scopes: all-scopes,
		       all-names: all-names);
  end;
  let (start-line, end-line, file-name) = external-lambda-location(item);
  if (start-line)
    format(stream, 
           "\n+++ Source Code Locators for %s, lines %d to %d in %s +++\n", 
           name, start-line, end-line, file-name);
    for (locator in item.lambda-all-locators)
      format(stream, "  Line %d at index %d\n", 
             locator.function-relative-line-number, 
             locator.function-relative-code-position);
    end for;
    format(stream, "\n+++ Selected Source Code Locators for %s +++\n", name);
    for (locator in item.lambda-selected-locators)
      format(stream, "  Line %d at index %d\n", 
             locator.function-relative-line-number, 
             locator.function-relative-code-position);
    end for;
  else
    format(stream, 
           "\n+++ No Source Code Locators for %s +++\n",
           name);
  end if;
  format(stream, "\n");
end method;


define method output-code-start 
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>) => ()
  format(outputter.outputter-stream, "\n+++ start of code +++\n\n");
end method;


define method output-glue-symbols
    (be :: <harp-back-end>, outputter :: <harp-print-outputter>,
     #key data-start, data-end, 
          variables-start, variables-end,
          objects-start, objects-end, 
          fixup-start, fixup-end, 
          import-start, import-end) => ()
  let stream = outputter.outputter-stream;
  local method display (name, value)
          if (value)
            format(stream, "  %s is %=\n", name, value); 
          end if;
        end method;
  format(stream, "\n+++ Defining glue symbols +++\n");
  display("Data start", data-start);
  display("Data end", data-end);
  display("Variables start", variables-start);
  display("Variables end", variables-end);
  display("Objects start", objects-start);
  display("Objects end", objects-end);
  display("Fixup start", fixup-start);
  display("Fixup end", fixup-end);
  display("Import start", import-start);
  display("Import end", import-end);
  format(stream, "\n");
end method;
