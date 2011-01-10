Module:    scepter-file-front-end
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <file-source> (<scepter-source>)
  slot file-source-file :: <locator>, required-init-keyword: file:;
  slot file-source-line :: <integer> = -1, init-keyword: line:;
  slot file-source-modified :: <date>, init-keyword: modified:;
end class;
define sealed domain make (singleton(<file-source>));
define sealed domain initialize (<file-source>);

define method initialize (source :: <file-source>, #key)
  next-method();
  unless(slot-initialized?(source, file-source-modified))
    source.file-source-modified
      := file-property(source.file-source-file, #"modification-date");
  end unless;
end method;

define sealed method shallow-copy (source :: <file-source>)
 => (new-source :: <file-source>)
  make(<file-source>,
       file: source.file-source-file,
       line: source.file-source-line,
       modified: source.file-source-modified);
end method;

define sealed method as (class == <string>, source :: <file-source>)
 => (string :: <string>)
  let filename = as(<string>, source.file-source-file);
  if (source.file-source-line > -1)
    concatenate!(filename, ":", integer-to-string(source.file-source-line));
  else
    filename;
  end if;
end method;

define method scepter-source-modified (source :: <file-source>)
 => (timestamp :: <date>)
  source.file-source-modified;
end method;

define method scepter-source-base-name (source :: <file-source>)
 => (base :: <string>)
  locator-base(source.file-source-file);
end method;


define scepter-front-end <file-front-end>
  name: "file"
end scepter-front-end;

define constant $front-end-version = "1.1";

define method scepter-front-end-banner
    (front-end :: subclass(<file-front-end>), stream :: <stream>)
 => ()
  let scepter = get-scepter();
  format(stream, "\n%s, file front-end version %s",
         scepter.scepter-program-name,
         $front-end-version);
end method;

define sealed class <non-existent-idl-file> (<option-error>)
  inherited slot idl-condition-string = "Unable to open file ";
  constant slot condition-file :: <locator>, required-init-keyword: file:;
end class;
define sealed domain make (singleton(<non-existent-idl-file>));
define sealed domain initialize (<non-existent-idl-file>);

define method idl-condition-body (stream :: <stream>, condition :: <non-existent-idl-file>)
 => ()
  format(stream, "'%s'", as(<string>, condition.condition-file));
end method;

define method scepter-front-end-make-source (front-end :: <file-front-end>, filename :: <string>)
 => (source :: <file-source>)
  let scepter = get-scepter();
  let file = merge-locators(as(<file-locator>, filename), scepter.scepter-directory);
  unless(file-exists?(file))
    error(make(<non-existent-idl-file>, file: file))
  end unless;
  make(<file-source>, file: file)
end method;

define method scepter-source-output-directory (source :: <file-source>, scepter :: <scepter>)
 => (directory :: <locator>)
  if (scepter.scepter-output-directory)
    scepter.scepter-output-directory;
  else
    locator-directory(source.file-source-file)
  end if;
end method;

define method scepter-front-end-generate-ast
    (front-end :: <file-front-end>, source :: <file-source>)
 => (root :: <ast-root>)
  parse-idl(as(<string>, source.file-source-file));
  let scepter = get-scepter();
  scepter.scepter-root;
end method;

