Module: file-reader
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Protocol:

define generic read-source (#key stream, module, header?);
define generic read-source-file (name, #key );
define generic read-source-file-header (name);

define generic open-source-file (file-designator, #key);
define generic close-source-file (stream);
define generic read-form (#key);

// !@#$ TEMPORARY COMPATIBILITY HACK

define method open (name :: <locator>, #rest all-keys, #key #all-keys)
  apply(make, <file-stream>, locator: as(<physical-locator>, name), all-keys)
end method open;

// Conditions:

define class <source-file-error> (<simple-error>)
end class <source-file-error>;

define method source-file-error (string, #rest args)
  error(make(<source-file-error>,
             format-string: string,
             format-arguments: args))
end method source-file-error;

// Implementation:

// Hack!!! Until the lexer API gives us access to its underlying stream,
// we have to pass a pair of stream and state around so that we can 
// close the stream at the end. 

define method open-source-file (name :: <locator>, #key)
  let stream = open(name, direction: input:);
  let header-table = read-source-file-header(stream);
  let module-strings = element(header-table, #"module", default: #f);
  if (~module-strings)
    source-file-error("No module specified in header on %=", stream);
  end;
  let module-name = as(<symbol>, module-strings.first);
  let state = make(<lexer-state>, stream: stream);
  values(pair(state, stream), module-name, #"infix-dylan")
end method;

define method close-source-file (id)
  close(id.tail);
end method;

// Hack!!! All this eoi: stuff is a nasty hack.

define method read-form 
    (#key stream, language, features, end-of-stream-value)
  let state = stream.head;
  let stream = stream.tail;
  let form = read-source-form(state);
  if (form == eoi:)
    end-of-stream-value
  else
    form
  end
end method;

define method read-source (#key stream, module, header?, features)
  let header-table = read-source-file-header(stream);
  let module-strings = element(header-table, #"module", default: #f);
  if (~module-strings)
    source-file-error("No module specified in header on %=", stream);
  end;
  let module-name = as(<symbol>, module-strings.first);
  // A lexer state records position information and the like
  let state = make(<lexer-state>, stream: stream);
  let forms = #();
  for (form = read-source-form(state) then read-source-form(state),
       while: form ~== eoi:) // Hack!!!
    forms := pair(form, forms);
  finally
    forms := reverse!(forms);
  end;
  values(forms, module-name)
end method read-source;

define method read-source-file (name :: <locator>, #key features = #())
  let stream = open(name, direction: input:);
  block ()
    read-source(stream: stream, header?: #t, features: features)
  cleanup
    close(stream)
  end block
end method read-source-file;

/*
define method read-source-file (name :: <locator>, #key features = #())
//  read-source-file
//    (as(<string>, as(<physical-locator>, name)), features: features)
end method read-source-file;
*/

define method read-source-file-header (name :: <locator>)
  let stream = open(name, direction: input:);
  block ()
    read-source-file-header(stream);
  cleanup
    stream & close(stream);
  end block
end method read-source-file-header;

define method read-source-file-header (stream :: <stream>)
  let header-options = read-file-header(stream);
  tablify-header-options(header-options)
end method read-source-file-header;

define method tablify-header-options (options)
  let table = make(<object-table>);
  for (option in options)
    let key     = option.head;
    let strings = option.tail;
    let existing-strings = element(table, key, default: #());
    table[key] := concatenate(existing-strings, strings);
  end;
  table
end method tablify-header-options;

define method read-da-big-form (filename)
  let (forms, source-module) = read-source-file(filename);
  values(pair(#"begin", forms), source-module)
end method read-da-big-form;

define method read-source-form (stream :: <stream>)
  read-source-form(make(<lexer-state>, stream: stream));
end method read-source-form;

define method read-source-form (s :: <lexer-state>)
  local lexer () lex(s) end;
  run-parser(infix-dylan-parser, lexer);
end method read-source-form;

// eof
