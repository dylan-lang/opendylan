Module:    file-reader
Synopsis:  Source code manipulation
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Protocol:

define generic read-source (#key stream, module, header?);

define generic read-source-file (name, #key );

define generic read-source-file-header (name);

// !@#$ TEMPORARY COMPATIBILITY HACK

define method open (name :: <locator>, #rest all-keys, #all-keys)
  apply(open, as(<string>, as(<physical-locator>, name)), all-keys)
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

define method read-source-file-header-defaulting (stream)
  let table = maybe-read-file-header(stream);
  if (~table)
    values(#"dylan-user", #"prefix-dylan")
  else
    let module = element(table, module:, default: #f);
    let language = element(table, language:, default: #("infix-dylan"));
    unless (module)
      source-file-error("No module specified for %=", stream)
    end unless;
    values(as(<symbol>, module.first), as(<symbol>, language.first))
  end if
end method read-source-file-header-defaulting;

define method read-source-file-header (name :: <locator>)
  let stream = open(name, direction: input:);
  block ()
    maybe-read-file-header(stream);
  cleanup
    stream & close(stream)
  end block
end method read-source-file-header;

define method read-form
    (#key stream = *standard-input*, language = #"infix-dylan", 
          features = #(), end-of-stream-value = $eof)
  let reader
    = select (language)
        #"prefix-dylan" =>
          read;
        #"infix-dylan" =>
          read-infix
      end select;
  block ()
    reader(stream: stream, features: features, eof-value: end-of-stream-value)
  exception (<end-of-file>)
    end-of-stream-value
  end block
end method read-form;

define method read-source
    (#key stream = *standard-input*,
          header?,
          module = unsupplied(),
          language = unsupplied(),
          features = #())
  unless (header?)
    if (module.unsupplied? | language.unsupplied?)
      source-file-error
        ("Module and language must be specified to reading source from %=",
         stream)
    end if
  end unless;
  let (module, language)
    = if (header?)
        read-source-file-header-defaulting(stream)
      else
        values(module, language)
      end if;
  // Hack!!!
  iterate read-forms (forms = #())
    let form
      = read-form
          (stream: stream,
           module: module,
           language: language,
           features: features);
    if (form == $eof)
      values(reverse!(forms), module)
    else
      pair(form, forms).read-forms
    end if
  end iterate
end method read-source;

define method read-source-file (name :: <string>, #key features = #())
  let stream = open(name, direction: input:);
  block ()
    read-source(stream: stream, header?: #t, features: features)
  cleanup
    close(stream)
  end block
end method read-source-file;

define method read-source-file (name :: <locator>, #key features = #())
  read-source-file
    (as(<string>, as(<physical-locator>, name)), features: features)
end method read-source-file;

define method open-source-file (name :: <string>, #key features = #())
  let stream = open(name, direction: input:);
  let (module, language) = read-source-file-header-defaulting(stream);
  values(stream, module, language)
end method open-source-file;

define method close-source-file (stream :: <stream>)
  close(stream);
end method;

define method open-source-file (name :: <locator>, #key features = #())
  open-source-file
    (as(<string>, as(<physical-locator>, name)), features: features)
end method open-source-file;

define method read-da-big-form (filename)
  let (forms, source-module) = read-source-file(filename);
  values(pair(#"begin", forms), source-module)
end method read-da-big-form;

/*
define method compiler-features ()
  let features = *features*.copy-sequence;
  features := add-new!(features, #"native");
  features := add-new!(features, #"compiler");
  features := remove!(features, #"harlequin-common-lisp");
  features := remove!(features, #"harlequin-lispworks-dylan-translator");
  features
end method compiler-features;

define method read-source-with-features (filename)
  let input-stream = #f;
  let dylan-locator = override-locator-extension(filename, "dylan");
  fluid-bind (*features* = compiler-features())
    block ()
      input-stream := make(<file-stream>, locator: dylan-locator);
      let (forms, module)
        = read-source(stream: input-stream, header?: #t, features: *features*);
      values(forms, module)
    cleanup
      if (input-stream)
        close(input-stream)
      end if
    end block
  end fluid-bind
end method read-source-with-features;
*/

// eof
