Module:    scepter-front-end-implementation
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class <scepter-source> (<object>)
end class;

define open generic scepter-source-modified
    (source :: <scepter-source>)
 => (timestamp :: <date>);

define open generic scepter-source-base-name
    (source :: <scepter-source>)
 => (base :: <string>);

define open generic scepter-source-output-directory
    (source :: <scepter-source>, scepter :: <scepter>)
 => (directory :: <locator>);


define open abstract class <scepter-front-end> (<object>)
  constant slot scepter-front-end-scepter, required-init-keyword: scepter:;
end class;

define open generic scepter-front-end-banner
    (front-end :: subclass(<scepter-front-end>), stream :: <stream>)
 => ();

define open generic scepter-front-end-make-source
    (front-end :: <scepter-front-end>, name :: <string>)
 => (source :: <scepter-source>);

define open generic scepter-front-end-generate-ast
    (front-end :: <scepter-front-end>, source :: <scepter-source>)
 => (root :: <scepter-ast-root>);

define variable *front-end-class-table* :: <table> = make(<table>);

define inline-only function front-end-class-table ()
  *front-end-class-table*;
end function;

define sealed method scepter-front-end-setup-class (class :: subclass(<scepter-front-end>), name :: <string>)
 => ()
  element(front-end-class-table(), as(<symbol>, name)) := class;
end method;

define sealed method all-scepter-front-end-classes ()
 => (front-ends :: <sequence>)
  key-sequence(front-end-class-table());
end method;

define sealed class <unknown-front-end> (<option-error>)
  inherited slot idl-condition-string = "unknown front end, ";
  constant slot condition-front-end-name :: <string>, required-init-keyword: name:;
end class;
//define sealed domain make (singleton(<unknown-front-end>));
//define sealed domain initialize (<unknown-front-end>);

define method idl-condition-body (stream :: <stream>, condition :: <unknown-front-end>)
 => ()
  format(stream, "%s", condition.condition-front-end-name);
end method;

define sealed method scepter-front-end-class (name :: <string>)
 => (front-end :: subclass(<scepter-front-end>))
  element(front-end-class-table(), as(<symbol>, name), default: #f)
    | error(make(<unknown-front-end>, name: name));
end method;

define macro scepter-front-end-definer
    { define scepter-front-end ?:name name: ?string:expression ?slots:* end }
      =>
      { define class ?name (<scepter-front-end>) ?slots end;
        scepter-front-end-setup-class(?name, ?string);
      }
end macro;

define method setup-scepter-front-end (option :: <scepter-option>, scepter :: <scepter>)
  let name = option.scepter-option-values.first;
  let class = scepter-front-end-class(name);
  scepter.scepter-front-end := make(class, scepter: scepter);
end method;

define method reset-scepter-front-end (option, scepter)
  let class = scepter-default-front-end-class(scepter);
  scepter.scepter-front-end := make(class, scepter: scepter);
end method;

