Module: scepter-file-front-end
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// PARSE-IDL
//
// "run-parser" and "<lexer>" are from parser-run-time library
//
// "parser" is global variable containing parser
// built by parsergen
//

define method parse-idl (filename)
  let scepter = get-scepter();
  scepter.scepter-nodes := make(<stretchy-vector>);
  initialize-global-scope();
  block ()
    with-open-file (file-stream = filename, direction: #"input")
      with-token-stream (token-stream, file-stream, filename)
        with-progress (scepter, "Parsing %s", filename)
	  local method get-token ()
	    let token = read-element(token-stream);
//	    format(scepter.scepter-output, "\nTOKEN: %=, %s\n",
//		   token.idl-parser-tag,
//		   token.idl-lexer-string);
	    values(token.idl-parser-tag, token.idl-lexer-string);
	  end method;
	  let handler (<idl-condition>, test: handle-condition?) = rcurry(handle-idl-condition, token-stream);
	  let handler (<idl-syntax-error>, test: handle-condition?) = rcurry(handle-idl-syntax-error, token-stream);
	  run-parser(#f, idl-parser, get-token, on-error: recover-idl);
	  call-add(scepter.scepter-root);
        end with-progress;
      end with-token-stream;
    end with-open-file;
  exception (<idl-condition-restart>)
  exception (c :: <abort>) // ---*** workaround compiler bug by naming variable
  end block;
end method;

// $PREDEFINED-PSEUDO-TYPE-NAMES

define constant $predefined-pseudo-type-names = #["attribute",
                                                  "case",
                                                  "const",
                                                  "context",
                                                  "default",
                                                  "enum",
                                                  "exception",
                                                  "in",
                                                  "out",
                                                  "inout",
                                                  "interface",
                                                  "module",
                                                  "oneway",
                                                  "raises",
                                                  "readonly",
                                                  "sequence",
                                                  "string",
                                                  "wstring_t",
                                                  "struct",
                                                  "switch",
                                                  "typedef",
                                                  "union",
                                                  "unsigned",
                                                  "TRUE",
                                                  "FALSE"];

// POPULATE-GLOBAL-SCOPE

define method populate-global-scope (module :: <ast-module>)
  for (type in $predefined-types)
    add-declarator(module,
                   make(<ast-predefined-type>,
                        type: type,
                        local-name: make(<ast-identifier>, label: type.idl-type-name)));
  end for;
  for (name in $predefined-pseudo-type-names)
    add-declarator(module,
                   make(<ast-predefined-type>,
                        type: $pseudo-idl-type,
                        local-name: make(<ast-identifier>, label: name)));
  end for;
end method;

// INITIALIZE-GLOBAL-SCOPE

define method initialize-global-scope ()
  let scepter = get-scepter();
  scepter.scepter-errors.size := 0;
  scepter.scepter-scopes.size := 0;
  scepter.scepter-root := make(<ast-root>, local-name: make(<ast-identifier>, label: ""));
  push(scepter.scepter-scopes, scepter.scepter-root);
  populate-global-scope(scepter.scepter-root);
  scepter.scepter-in-main-file? := #t;
end method;

