Module:    scepter-back-end-implementation
Author:    Jason Trenouth, Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open class <scepter-back-end-result> (<object>)
  constant slot scepter-back-end-success? :: <boolean>,
    required-init-keyword: success?:;
end class;

define open generic scepter-back-end-success?
    (result :: <scepter-back-end-result>)
 => (success? :: <boolean>);

define open generic scepter-back-end-dylan-subprojects
    (result :: <scepter-back-end-result>)
 => (projects :: <sequence>);

define open generic scepter-back-end-modified-dylan-projects
    (result :: <scepter-back-end-result>)
 => (projects :: <sequence>);

define method scepter-back-end-dylan-subprojects (result :: <scepter-back-end-result>)
 => (projects :: <sequence>)
  #();
end method;

define method scepter-back-end-modified-dylan-projects (result :: <scepter-back-end-result>)
 => (projects :: <sequence>)
  #();
end method;


define variable *back-end-class-table* :: <table> = make(<table>);

define inline-only function back-end-class-table ()
  *back-end-class-table*;
end function;

define macro scepter-back-end-definer
  { define scepter-back-end ?name:name
      command-line-syntax: ?syntax:expression,
      filename-extension: ?ext:expression
      ?slots:*
    end}
  =>
  { define class ?name (<scepter-back-end>) ?slots end class; 
    setup-scepter-back-end-class(?name, ?syntax);
    define method scepter-back-end-command-line-syntax (back-end :: ?name)
     => (syntax :: <string>)
      ?syntax;
    end method;
    define method scepter-back-end-filename-extension (back-end :: ?name)
     => (extension :: <string>)
      ?ext;
    end method;
  }
end macro;

define open abstract class <scepter-back-end> (<object>)
  constant slot scepter-back-end-scepter :: <scepter>,
    required-init-keyword: scepter:;
end class;

define method setup-scepter-back-end-class
    (class :: subclass(<scepter-back-end>), name :: <string>)
 => ()
  element(back-end-class-table(), as(<symbol>, name)) := class;
end method;

define method all-scepter-back-end-classes ()
 => (classes :: <sequence>)
  key-sequence(back-end-class-table());
end method;

define sealed class <unknown-back-end> (<option-error>)
  inherited slot idl-condition-string = "unknown back-end, ";
  constant slot condition-back-end-name :: <string>,
    required-init-keyword: name:;
end class;
//define sealed domain make (singleton(<unknown-back-end>));
//define sealed domain initialize (<unknown-back-end>);

define method idl-condition-body (stream :: <stream>, condition :: <unknown-back-end>)
 => ()
  format(stream, "%s", condition.condition-back-end-name);
end method;

define method scepter-back-end-class (name :: <string>)
 => (back-end :: subclass(<scepter-back-end>))
  element(back-end-class-table(), as(<symbol>, name), default: #f)
    | error(make(<unknown-back-end>, name: name));
end method;

// PROTOCOL

define open generic scepter-back-end-banner
    (back-end :: subclass(<scepter-back-end>), stream :: <stream>)
 => ();

define open generic scepter-back-end-command-line-syntax
    (back-end :: <scepter-back-end>)
 => (syntax :: <string>);

define open generic scepter-back-end-filename-extension
    (back-end :: <scepter-back-end>)
 => (extension :: <string>);

define sealed generic run-scepter-back-end-emitter
    (back-end :: <scepter-back-end>, root :: <scepter-ast-root>, front-end :: <scepter-front-end>, source :: <scepter-source>)
 => (result :: <object>);

define open generic scepter-back-end-emitter?
    (back-end :: <scepter-back-end>, conditions :: <collection>)
 => (run? :: <boolean>);

define open generic scepter-back-end-emit
    (back-end :: <scepter-back-end>, root :: <scepter-ast-root>, front-end :: <scepter-front-end>, source :: <scepter-source>)
 => (result :: <scepter-back-end-result>);

define open generic initialize-scepter-back-end
    (back-end :: <scepter-back-end>) => ();

define open generic finalize-scepter-back-end
    (back-end :: <scepter-back-end>) => ();

define open generic emit-declarator?
    (back-end :: <scepter-back-end>, declarator :: <scepter-declarator>)
 => (emit? :: <boolean>);

// DEFAULT METHODS

define method scepter-back-end-emitter?
    (back-end :: <scepter-back-end>, conditions :: <collection>)
 => (run? :: <boolean>)
  #t;
end method;

define method scepter-back-end-banner
    (back-end :: subclass(<scepter-back-end>), stream :: <stream>)
 => ()
end method;

define method initialize-scepter-back-end
    (back-end :: <scepter-back-end>)
 => ()
end method;  

define method finalize-scepter-back-end
    (back-end :: <scepter-back-end>)
 => ()
end method;

define method run-scepter-back-end-emitter
    (back-end :: <scepter-back-end>, root :: <scepter-ast-root>, front-end :: <scepter-front-end>, source :: <scepter-source>)
 => (result :: <object>)
  let result = #f;
  let scepter = back-end.scepter-back-end-scepter;
  block ()
    with-progress (scepter, "Emitting %s as %s", as(<string>, source), back-end.scepter-back-end-command-line-syntax)
      result := scepter-back-end-emit(back-end, root, front-end, source);
    end with-progress;
  exception (<abort>)
  end block;
  result;
end method;

define method emit-declarator? (back-end :: <scepter-back-end>, declarator :: <scepter-declarator>) => (emit? :: <boolean>)
  let scepter = back-end.scepter-back-end-scepter;
  scepter.scepter-emit-imported? | ~declarator-imported?(declarator)
end method;

define method setup-scepter-back-ends (option :: <scepter-option>, scepter :: <scepter>)
  let back-end-names = option.scepter-option-values;
  let back-end-classes = map(scepter-back-end-class, back-end-names);
  do(method (back-end-class) push(scepter.scepter-back-ends, make(back-end-class, scepter: scepter)) end,
     back-end-classes);
end method;

define method reset-scepter-back-ends (option, scepter :: <scepter>)
  scepter.scepter-back-ends := make(<deque>);
end method;

// This reference to setup-scepter-back-end-class is
// to suppress a compiler warning.

begin
  setup-scepter-back-end-class;
end;

