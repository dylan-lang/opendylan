Module:    scepter-file-front-end
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <idl-illegal-inheritance> (<idl-error>)
  inherited slot idl-condition-string = "cannot inherit from interface: ";
  constant slot idl-condition-declarator-name, init-keyword: name:;
  constant slot idl-condition-declarator, init-keyword: declarator:;
end class;

define method idl-condition-body (stream :: <stream>, condition :: <idl-illegal-inheritance>)
 => ()
  format(stream, "%s attempts to inherit from %s",
         condition.idl-condition-declarator-name,
         declarator-name-as-string(condition.idl-condition-declarator));
end method;

define class <idl-scope-conflict> (<idl-declarators-error>)
  inherited slot idl-condition-string = "definition scope is different than forward declared scope, ";
end class;

define class <idl-illegal-type> (<idl-error>)
  inherited slot idl-condition-string = "illegal type used in expression, ";
  constant slot idl-condition-declarator, init-keyword: declarator:;
end class;

define method idl-condition-body (stream :: <stream>, condition :: <idl-illegal-type>)
 => ()
  let name = condition.idl-condition-declarator.declarator-scoped-name;
  if (name)
    format(stream, "%s", scoped-name-as-string(name));
  else
    format(stream, "unknown symbol");
  end if;
end method;

define class <idl-illegal-recursive-type> (<idl-declarators-error>)
  inherited slot idl-condition-string = "illegal recursive use of type: ";
end class;

define class <idl-not-a-type> (<idl-declarators-error>)
  inherited slot idl-condition-string = "specified symbol is not a type: ";
end class;

define class <idl-syntax-error> (<idl-error>)
  constant slot parser-state :: <idl-parser-state>,
       init-function: method () *parser-state* end method,
       init-keyword: parser-state:;
  constant slot idl-parser-tag :: <integer>,
       init-keyword: parser-tag:;
  constant slot idl-lexer-string :: <object>,
       init-keyword: lexer-string:;
end class;

define method idl-condition-similar? (condition1 :: <idl-syntax-error>, condition2 :: <idl-syntax-error>)
 => (similar? :: <boolean>)
  condition1.parser-state == condition2.parser-state;
end method;

define method idl-condition-body (stream :: <stream>, condition :: <idl-syntax-error>)
 => ()
  format(stream, "%s: \"%s\"",
         condition.parser-state.syntax-error-message,
         condition.idl-lexer-string);
end method;

// PARSER ERROR HANDLING

define class <idl-syntax-error-restart> (<simple-restart>)
  keyword format-string: = "Try to continue from IDL syntax error";
end class;

define method handle-idl-syntax-error 
  (condition :: <idl-syntax-error>, next-handler :: <function>, token-stream :: <IDL-lexer>)
  let similar-error? = scepter-condition(get-scepter(), condition);
  if (condition.idl-parser-tag == $EOI-token)
    abort();
  else
    unless (similar-error?)
      fixup-token-stream(condition, token-stream);
    end unless;
    signal(make(<idl-syntax-error-restart>));
  end if;
end method;

define method handle-condition? (condition)
  let scepter = get-scepter();
  ~scepter.scepter-break-on-errors?;
end method;

define method handle-idl-condition
  (condition :: <idl-condition>, next-handler :: <function>, token-stream :: <IDL-lexer>)
  ignore(token-stream);
  scepter-condition(get-scepter(), condition);
  signal(make(<idl-condition-restart>));
end method;

define method fixup-token-stream (condition :: <idl-syntax-error>, token-stream :: <IDL-lexer>)
  let parser-state = condition.parser-state;
  if (parser-state.insert-token)
//    format(scepter.scepter-output*, " [inserting: \'%s\']", parser-state.insert-token.lexer-string);
    push(token-stream.fake-tokens, token-stream.last-token);
    push(token-stream.fake-tokens, parser-state.insert-token);
  end if;
end method;

define method recover-idl (symbol, value, history)
  ignore(symbol, history);
  block ()
    signal(make(<idl-syntax-error>, parser-tag: symbol, lexer-string: value));
  exception (condition :: <idl-syntax-error-restart>)
  end block;
end method;

// PRAGMA ERRORS

define class <idl-pragma-error> (<idl-error>)
end class;

define class <idl-empty-pragma-body> (<idl-pragma-error>)
end class;

define class <idl-unknown-pragma> (<idl-pragma-error>)
  constant slot idl-condition-pragma :: <string>, required-init-keyword: pragma:;
end class;

define class <idl-extra-pragma-tokens> (<idl-pragma-error>)
  constant slot idl-condition-pragma :: <string>, required-init-keyword: pragma:;
  constant slot idl-condition-tokens :: <sequence>, required-init-keyword: tokens:;
end class;

define class <idl-expected-string-literal> (<idl-pragma-error>)
end class;

define class <idl-expected-version-number> (<idl-pragma-error>)
end class;

define class <idl-expected-double-colon> (<idl-pragma-error>)
end class;

define class <idl-expected-identifier> (<idl-pragma-error>)
end class;

define class <idl-expected-scoped-name> (<idl-pragma-error>)
end class;

define method idl-condition-body (stream :: <stream>, condition :: <idl-empty-pragma-body>)
 => ()
  format(stream, "Empty pragma body");
end method;

define method idl-condition-body (stream :: <stream>, condition :: <idl-unknown-pragma>)
 => ()
  format(stream, "Unknown pragma \"%s\" - ignoring", condition.idl-condition-pragma);
end method;

define method idl-condition-body (stream :: <stream>, condition :: <idl-extra-pragma-tokens>)
 => ()
  format(stream, "Extra tokens, %=, after pragma %s - ignoring", condition.idl-condition-tokens, condition.idl-condition-pragma);
end method;

define method idl-condition-body (stream :: <stream>, condition :: <idl-expected-string-literal>)
 => ()
  format(stream, "Missing string literal or illegal syntax in pragma");
end method;

define method idl-condition-body (stream :: <stream>, condition :: <idl-expected-version-number>)
 => ()
  format(stream, "Missing version number or illegal syntax in pragma");
end method;

define method idl-condition-body (stream :: <stream>, condition :: <idl-expected-double-colon>)
 => ()
  format(stream, "Missing '::' or illegal syntax in scoped name");
end method;

define method idl-condition-body (stream :: <stream>, condition :: <idl-expected-identifier>)
 => ()
  format(stream, "Missing identifier or illegal syntax in pragma");
end method;

define method idl-condition-body (stream :: <stream>, condition :: <idl-expected-scoped-name>)
 => ()
  format(stream, "Missing scoped name or illegal syntax in pragma");
end method;
