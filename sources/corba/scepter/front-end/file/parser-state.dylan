Module: scepter-file-front-end
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <idl-parser-state> (<object>)
  constant slot syntax-error-message :: <string> = "Some syntax error", init-keyword: syntax-error-message:;
  constant slot insert-token :: false-or(<token>) = #f, init-keyword: insert-token:;
end class;

define constant $idl-parser-no-state =
  make(<idl-parser-state>, syntax-error-message: "Statement can not be parsed");

define constant $idl-parser-type-declaration-seen =
  make(<idl-parser-state>, syntax-error-message: "Malformed typedef declaration");

define constant $idl-parser-constant-declaration-seen =
  make(<idl-parser-state>, syntax-error-message: "Malformed const declaration");

define constant $idl-parser-exception-declaration-seen =
  make(<idl-parser-state>, syntax-error-message: "Malformed exception declaration");

define constant $idl-parser-interface-declaration-seen =
  make(<idl-parser-state>, syntax-error-message: "Malformed interface declaration");

define constant $idl-parser-module-declaration-seen =
  make(<idl-parser-state>, syntax-error-message: "Malformed module declaration");

define constant $idl-parser-attribute-declaration-seen =
  make(<idl-parser-state>, syntax-error-message: "Malformed attribute declaration");

define constant $idl-parser-operation-declaration-seen =
  make(<idl-parser-state>, syntax-error-message: "Malformed operation declaration");

define constant $idl-parser-module-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing module identifier following MODULE keyword");

define constant $idl-parser-module-id-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing \'{\' or illegal syntax following module identifier",
                           insert-token: make(<open-brace>));

define constant $idl-parser-module-sq-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following module \'{\' opener");

define constant $idl-parser-module-qs-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following module \'}\' closer");

define constant $idl-parser-module-body-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following module body statement(s)");

define constant $idl-parser-interface-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing interface identifier following INTERFACE keyword");

define constant $idl-parser-interface-id-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following interface identifier");

define constant $idl-parser-inheritance-spec-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing \'{\' or illegal syntax following inheritance spec",
                           insert-token: make(<open-brace>));

define constant $idl-parser-forward-declaration-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing \';\' following forward interface declaration",
                           insert-token: make(<semi-colon>));

define constant $idl-parser-interface-sq-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following interface \'{\' opener");

define constant $idl-parser-interface-qs-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following interface \'}\' closer");

define constant $idl-parser-interface-body-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following interface body statement(s)");

define constant $idl-parser-inheritance-spec-colon-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following \':\' starting inheritance list");

define constant $idl-parser-scoped-names-comma-seen =
  make(<idl-parser-state>, syntax-error-message: "Found illegal scoped name in scoped name list");

define constant $idl-parser-scoped-name-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing \',\' following scoped name in scoped name list",
                           insert-token: make(<comma>));

define constant $idl-parser-scoped-name-id-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal component in scoped name");

define constant $idl-parser-scope-delimiter-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal component in scoped name following \'::\'");

define constant $idl-parser-constant-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing type or illegal syntax following CONST keyword");

define constant $idl-parser-constant-type-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing identifier or illegal syntax following const type");

define constant $idl-parser-constant-id-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing \'=\' or illegal syntax after const identifier",
                           insert-token: make(<equal>));

define constant $idl-parser-constant-assignment-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing value expr or illegal syntax following \'=\'");

define constant $idl-parser-constant-expression-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing \';\' or illegal syntax following value expr in const",
                           insert-token: make(<semi-colon>));

define constant $idl-parser-typedef-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing type or illegal syntax following TYPEDEF keyword");

define constant $idl-parser-type-spec-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing declarators or illegal syntax following type spec");

define constant $idl-parser-declarators-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following declarators in TYPEDEF declaration");

define constant $idl-parser-struct-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing struct identifier following STRUCT keyword");

define constant $idl-parser-struct-id-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing \'{\' or illegal syntax following struct identifier",
                           insert-token: make(<open-brace>));

define constant $idl-parser-struct-sq-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following struct \'{\' opener");

define constant $idl-parser-struct-qs-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following struct \'}\' closer");

define constant $idl-parser-struct-body-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following struct body statement(s)");

define constant $idl-parser-member-type-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax of missing identifier following member type");

define constant $idl-parser-member-declarators-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following member declarator(s)");

define constant $idl-parser-member-declarators-completed =
  make(<idl-parser-state>, syntax-error-message: "Missing \',\' between member declarators of same type(?)",
                           insert-token: make(<comma>));

define constant $idl-parser-union-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing identifier following UNION keyword");

define constant $idl-parser-union-id-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following union identifier");

define constant $idl-parser-switch-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following SWITCH keyword");

define constant $idl-parser-switch-open-paren-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following \'(\' in switch in union");

define constant $idl-parser-switch-type-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following type decl in switch in union");

define constant $idl-parser-switch-close-paren-seen =
  make(<idl-parser-state>, syntax-error-message: "Missing union \'{\' opener",
                           insert-token: make(<open-brace>));

define constant $idl-parser-union-sq-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following union \'{\' opener");

define constant $idl-parser-union-qs-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following union \'}\' closer");

define constant $idl-parser-default-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing \':\' following DEFAULT keyword",
                           insert-token: make(<colon>));

define constant $idl-parser-union-label-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following branch label in union");

define constant $idl-parser-label-colon-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following \':\' in branch label in union");

define constant $idl-parser-label-expression-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following label expression in union");

define constant $idl-parser-union-element-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following union element");

define constant $idl-parser-union-element-completed =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following union element");

define constant $idl-parser-case-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following CASE keyword in union");

define constant $idl-parser-union-element-type-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following type decl in union element");

define constant $idl-parser-union-element-declarator-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following declarator in union element");

define constant $idl-parser-union-body-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following union body statement(s)");

define constant $idl-parser-enum-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing identifier following ENUM keyword");

define constant $idl-parser-enum-id-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing \'{\' following enum identifier",
                           insert-token: make(<open-brace>));

define constant $idl-parser-enum-sq-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following enum \'{\' opener");

define constant $idl-parser-enum-qs-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following enum \'}\' closer");

define constant $idl-parser-enum-body-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following enum body statement(s)");

define constant $idl-parser-enum-comma-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing identifier following \',\' in enum");

define constant $idl-parser-sequence-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing \'<\' following SEQUENCE keyword",
                           insert-token: make(<less-than>));

define constant $idl-parser-sequence-sq-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing type following \'<\' in sequence");

define constant $idl-parser-sequence-qs-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following \'>\' in sequence");

define constant $idl-parser-sequence-type-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following sequence type declaration");

define constant $idl-parser-sequence-comma-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax of missing size expr following \',\' in sequence");

define constant $idl-parser-sequence-expression-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing \'>\' following size expr in sequence",
                           insert-token: make(<greater-than>));

define constant $idl-parser-string-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing \'<\' following STRING keyword",
                           insert-token: make(<less-than>));

define constant $idl-parser-string-sq-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing size expr following \'<\' in string");

define constant $idl-parser-string-qs-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following \'>\' in string");

define constant $idl-parser-string-expression-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing \'>\' after size expr in string",
                           insert-token: make(<greater-than>));

define constant $idl-parser-string-completed =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after string declaration");

define constant $idl-parser-array-id-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing dimensions after array identifier");

define constant $idl-parser-array-completed =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after array declaration");

define constant $idl-parser-dimension-sq-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing size expr after \'[\' in array declaration");

define constant $idl-parser-dimension-qs-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after \']\' in array declaration");

define constant $idl-parser-dimension-expression-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing \']\' after size expr in array declaration",
                           insert-token: make(<close-bracket>));

define constant $idl-parser-attribute-ro-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after READONLY keyword");

define constant $idl-parser-attribute-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after ATTRIBUTE keyword");

define constant $idl-parser-attribute-type-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after type in attribute declaration");

// UNUSED
//define constant $idl-parser-attribute-declarators-seen =
//  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after attribute declarators");

define constant $idl-parser-attribute-completed =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after attribute declaration");

define constant $idl-parser-exception-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing identifier after EXCEPTION keyword");

define constant $idl-parser-exception-id-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing \'{\' after exception identifier",
                           insert-token: make(<open-brace>));

define constant $idl-parser-exception-sq-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after exception \'{\' opener");

define constant $idl-parser-exception-qs-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after exception \'}\' closer");

define constant $idl-parser-exception-body-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after exception body statement(s)");

define constant $idl-parser-operation-attribute-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing type after operation attribute");

define constant $idl-parser-operation-type-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing identifier after operation type");

define constant $idl-parser-operation-id-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing \'(\' after operation identifier",
                           insert-token: make(<open-parenthesis>));

define constant $idl-parser-operation-parameters-completed =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after operation parameter list");

define constant $idl-parser-operation-raises-completed =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after optional RAISES in operation declaration");

// UNUSED
//define constant $idl-parser-operation-context-completed =
//  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after optional CONTEXT in operation declaration");

define constant $idl-parser-operation-completed =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after operation declaration");

define constant $idl-parser-operation-sq-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after operation parameter list \'(\' opener");

define constant $idl-parser-operation-qs-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after operation parameter list \')\' closer");

define constant $idl-parser-operation-parameters-comma-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing direction in parameter declaration");

define constant $idl-parser-operation-parameter-direction-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing type in parameter declaration");

define constant $idl-parser-operation-parameter-type-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing declarator in parameter declaration");

define constant $idl-parser-operation-parameter-declarator-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax following parameter declarator");

define constant $idl-parser-operation-raises-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing \'(\' after RAISES keyword",
                           insert-token: make(<open-parenthesis>));

define constant $idl-parser-operation-raises-sq-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after RAISES \'(\' opener");

define constant $idl-parser-operation-raises-qs-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after RAISES \')\' closer");

define constant $idl-parser-operation-context-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax or missing \'(\' after CONTEXT keyword",
                           insert-token: make(<open-parenthesis>));

define constant $idl-parser-operation-context-sq-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after CONTEXT \'(\' opener");

define constant $idl-parser-operation-context-qs-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after CONTEXT \')\' closer");

define constant $idl-parser-operation-context-comma-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after \',\' in CONTEXT in operation declaration");

define constant $idl-parser-declarators-comma-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after \',\' in declarators list");

define constant $idl-parser-declarators-declarator-seen =
  make(<idl-parser-state>, syntax-error-message: "Illegal syntax after declarator in declarators list");

// *PARSER-STATE*

define variable *parser-state* :: <idl-parser-state> = $idl-parser-no-state;

define method update-parser-state (state :: <idl-parser-state>)
//  format(*idl-output*, "\nUPDATE-PARSER-STATE: %s",
//	 syntax-error-message(state));
  *parser-state* := state;
end method;

