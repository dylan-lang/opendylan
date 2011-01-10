Module: scepter-file-front-end
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// From collage this file needs:
// 	<token>
//	<identifier>
//	lexer-string
//	source-line
//	parser-tag

// define constant $default-last-tokens-buffer-max :: <integer> = 5;

define class <IDL-lexer> (<wrapper-stream>)
  constant slot reserved-words :: <table> = make(<table>);
  slot last-token :: false-or(<token>) = #f;
//  slot last-tokens :: <deque> = make(<deque>); // stack
//  slot last-tokens-buffer-max :: <integer> = $default-last-tokens-buffer-max;
  constant slot fake-tokens :: <deque> = make(<deque>); // queue
  slot token-count :: <integer> = 0;
end class;

define class <IDL-reserved-word> (<reserved-word>)
  constant slot idl-parser-tag :: <integer>, required-init-keyword: parser-tag:;
  constant slot idl-lexer-string :: <string>, required-init-keyword: lexer-string:;
end class;

define method parser-tag (token :: <IDL-reserved-word>)
 => (result :: <symbol>)
  as(<symbol>, token.idl-lexer-string);
end method;

define macro initialize-table
  { initialize-table ( ?table-name:expression) ?entries:* end }
    => {  let the-table = ?table-name; ?entries; the-table }
entries:
  { }
    => { }
  { ?key:expression => ?value:expression; ... }
    => { the-table[ ?key ] := ?value; ... }
end macro;

define method initialize (stream :: <IDL-lexer>, #key)
  next-method();
  initialize-table (stream.reserved-words)
    #"any" => pair($ANY-token,"any");
    #"attribute" => pair($ATTRIBUTE-token,"attribute");
    #"boolean" => pair($BOOLEAN-token,"boolean");
    #"case" => pair($CASE-token,"case");
    #"char" => pair($CHAR-token,"char");
    #"const" => pair($CONST-token,"const");
    #"context" => pair($CONTEXT-token,"context");
    #"default" => pair($DEFAULT-token,"default");
    #"double" => pair($DOUBLE-token,"double");
    #"enum" => pair($ENUM-token,"enum");
    #"exception" => pair($EXCEPTION-token,"exception");
    #"false" => pair($FALSE-token,"false"); 
    #"float" => pair($FLOAT-token,"float");
    #"idempotent" => pair($IDEMPOTENT-token,"idempotent");
    #"in" => pair($IN-token,"in");
    #"inout" => pair($INOUT-token,"inout");
    #"interface" => pair($INTERFACE-WORD-token,"interface");
    #"long" => pair($LONG-token,"long");
    #"module" => pair($MODULE-WORD-token,"module");
    #"octet" => pair($OCTET-token,"octet");
    #"oneway" => pair($ONEWAY-token,"oneway");
    #"out" => pair($OUT-token,"out");
    #"raises" => pair($RAISES-token,"raises");
    #"readonly" => pair($READONLY-token,"readonly");
    #"sequence" => pair($SEQUENCE-token,"sequence");
    #"short" => pair($SHORT-token,"short");
    #"string" => pair($STRING-token,"string");
    #"struct" => pair($STRUCT-token,"struct");
    #"switch" => pair($SWITCH-token,"switch");
    #"true" => pair($TRUE-token,"true");
    #"typedef" => pair($TYPEDEF-token,"typedef");
    #"union" => pair($UNION-token,"union");
    #"unsigned" => pair($UNSIGNED-token,"unsigned");
    #"void" => pair($VOID-token,"void");
    #"wchar" => pair($WCHAR-token,"wchar");
    #"wstring" => pair($WSTRING-token,"wstring");
  end;
end method;

define method read-element (stream :: <IDL-lexer>, #key on-end-of-stream)
 => (elmt)
  ignore(on-end-of-stream);
  let scepter = get-scepter();
  let next-token = 
    if (stream.fake-tokens.empty?)
      remap-reserved-words(stream, 
        if (scepter.scepter-failure-mode & stream.token-count == scepter.scepter-failure-token)
          broken-read-element(stream);
        else
          read-element(stream.inner-stream);
        end if);
    else
      stream.fake-tokens.pop;
    end if;
//  push(stream.last-tokens, next-token);
//  if (stream.last-tokens.size > stream.last-tokens-buffer-max)
//    pop-last(stream.last-tokens);
//  end if;
  stream.last-token := next-token;
  stream.token-count := stream.token-count + 1;
  if (next-token.source-line)
    scepter.scepter-source.file-source-line := next-token.source-line;
  end if;
  next-token;
end method;

define method broken-read-element (stream :: <IDL-lexer>)
  let scepter = get-scepter();
  select (scepter.scepter-failure-mode)
    deletion: => read-element(stream.inner-stream); read-element(stream.inner-stream);
    insertion: => make(<identifier>, lexer-string: "__SPURIOUS_TOKEN__", source-line: scepter.scepter-source.file-source-line);
    termination: => make(<eoi>);
  end select;
end method;

define method remap-reserved-words (stream :: <IDL-lexer>, token :: <token>)
  token;
end method;

define method remap-reserved-words (stream :: <IDL-lexer>, token :: <identifier>)
  let scepter = get-scepter();
  let entry = element(stream.reserved-words, as(<symbol>, token.lexer-string), default: #f);
  if (entry &
      ((~scepter.scepter-case-sensitive-reserved-words?) | (entry.tail = token.lexer-string)))
    make(<IDL-reserved-word>,
         parser-tag: entry.head,
         lexer-string: entry.tail,
         source-line: token.source-line);
  else
    token;
  end if;
end method;

define constant $parser-tag-map :: <table> =
  initialize-table (make(<table>))
    #"eoi" => $EOI-token;
    #"::" => $DOUBLE-COLON-token;
    #">>" => $DOUBLE-RIGHT-ANGLE-token;
    #"<<" => $DOUBLE-LEFT-ANGLE-token;
    #"=" => $EQUALS-token;
    #"|" => $VERTICAL-BAR-token;
    #"^" => $CIRCUMFLEX-token;
    #"&" => $AMPERSAND-token;
    #"+" => $PLUS-token;
    #"-" => $MINUS-token;
    #"*" => $ASTERISK-token;
    #"/" => $SLASH-token;
    #"%" => $PERCENT-token;
    #"~" => $TILDE-token;
    #"<" => $LEFT-ANGLE-token;
    #">" => $RIGHT-ANGLE-token;
    #"(" => $LEFT-PAREN-token;
    #")" => $RIGHT-PAREN-token;
    #"{" => $LEFT-BRACE-token;
    #"}" => $RIGHT-BRACE-token;
    #"[" => $LEFT-BRACKET-token;
    #"]" => $RIGHT-BRACKET-token;
    #":" => $COLON-token;
    #";" => $SEMICOLON-token;
    #"," => $COMMA-token;
    #"string-literal" => $STRING-LITERAL-token;
    #"integer-literal" => $INTEGER-LITERAL-token;
    #"character-literal" => $CHARACTER-LITERAL-token;
    #"float-literal" => $FLOATING-PT-LITERAL-token;
    #"identifier" => $IDENTIFIER-token;
  end;

define method idl-lexer-string (token :: <token>)
  token.lexer-string;
end method;

define method idl-lexer-string (token :: <literal-token>)
  token.dylan-value;
end method;

define method idl-lexer-string (token :: <string-literal>)
  token.lexer-string;
end method;

define method idl-parser-tag (token :: <token>)
  remap-parser-tag(token.parser-tag);
end method;

define method remap-parser-tag (tag :: type-union(<character>, <symbol>))
  element($parser-tag-map, tag, default: #f) | tag;
end method;


// <IDL-cpp-stream>
//
define class <IDL-cpp-stream> (<C++-cpp-stream>)
end class;

define method cpp-stream-current-source-file (cpp-stream :: <IDL-cpp-stream>)
 => (file-source :: <locator>)
  let lexer-stream = cpp-stream.inner-stream;
  let pre-lexer-stream = lexer-stream.inner-stream;
  let file-stream = pre-lexer-stream.inner-stream;
  as(<file-locator>, stream-locator(file-stream));
end method;

define method cpp-handle-include-entry (stream :: <IDL-cpp-stream>)
 => ()
  let scepter = get-scepter();
  let source = scepter.scepter-source;
  source.file-source-file := cpp-stream-current-source-file(stream);
  scepter.scepter-imported? := ~empty?(stream.inner-stream-stack);
  let modified = file-property(source.file-source-file, #"modification-date");
  if (modified > source.file-source-modified)
    source.file-source-modified := modified;
  end if;
end method;

define method cpp-handle-include-exit (stream :: <IDL-cpp-stream>)
 => ()
  let scepter = get-scepter();
  scepter.scepter-source.file-source-file := cpp-stream-current-source-file(stream);
  scepter.scepter-imported? := ~empty?(stream.inner-stream-stack);
end method;

define method cpp-handle-pragma (stream :: <IDL-cpp-stream>, pragma-body :: <list>)
 => (token :: false-or(<token>))
  block ()
    if (empty?(pragma-body))
      error(make(<idl-empty-pragma-body>));
    else
      let remaining-tokens :: <list>
	= select (pragma-body.first.lexer-string by \=)
	    "prefix"
	      => parse-prefix-pragma(pragma-body.tail);
	    "ID"
	      => parse-id-pragma(pragma-body.tail);
	    "version"
	      => parse-version-pragma(pragma-body.tail);
	    otherwise
	      => begin
		   error(make(<idl-unknown-pragma>, pragma: pragma-body.first.lexer-string));
		   #();
		 end;
	  end select;
      unless (empty?(remaining-tokens))
	error(make(<idl-extra-pragma-tokens>, pragma: pragma-body.first.lexer-string, tokens: remaining-tokens));
      end unless;
    end if;
  exception (condition :: <idl-condition-restart>)
  end;
  #f;
end method;

define method parse-prefix-pragma (tokens :: <list>)
 => (remaining-tokens :: <list>)
  let scepter = get-scepter();
  let (tokens, prefix) = parse-string-literal(tokens);
  if (prefix)
    assert(~empty?(scepter.scepter-scopes));
    let scope = scepter.scepter-scopes.first;
    scope.scope-pragma-prefix := prefix;
  end if;
  tokens;
end method;

define method parse-ID-pragma (tokens :: <list>)
 => (remaining-tokens :: <list>)
  let scepter = get-scepter();
  let (tokens, scoped-name) = parse-scoped-name(tokens);
  if (scoped-name)
    let (tokens, string) = parse-string-literal(tokens);
    if (string)
      assert(~empty?(scepter.scepter-scopes));
      let declarator = resolve-scoped-name(scepter.scepter-scopes.first, scoped-name, error?: #t);
      declarator & (declarator.declarator-repository-id-internal := string);
    end if;
    tokens;
  else
    tokens;
  end if;
end method;

define method parse-version-pragma (tokens :: <list>)
 => (remaining-tokens :: <list>)
  let scepter = get-scepter();
  let (tokens, scoped-name) = parse-scoped-name(tokens);
  if (scoped-name)
    let (tokens, version) = parse-version-number(tokens);
    if (version)
      assert(~empty?(scepter.scepter-scopes));
      let declarator = resolve-scoped-name(scepter.scepter-scopes.first, scoped-name, error?: #t);
      declarator & (declarator.declarator-repository-id-version := version);
    end if;
    tokens;
  else
    tokens;
  end if;
end method;

define method parse-string-literal (tokens :: <list>)
 => (remaining-tokens :: <list>, string :: false-or(<string>))
  if (empty?(tokens))
    error(make(<idl-expected-string-literal>));
    values(#(), #f);
  else
    let token = tokens.first;
    if (string-literal?(token))
      values(tokens.tail, token.dylan-value);
    else
      error(make(<idl-expected-string-literal>));
      values(tokens, #f);
    end if;
  end if;
end method;

define method parse-version-number (tokens :: <list>)
 => (remaining-tokens :: <list>, string :: false-or(<string>))
  if (empty?(tokens))
    error(make(<idl-expected-version-number>));
    values(#(), #f);
  else
    let token = tokens.first;
    if (floating-point-literal?(token))
      values(tokens.tail, token.lexer-string);
    else
      error(make(<idl-expected-version-number>));
      values(tokens, #f);
    end if;
  end if;
end method;

define method parse-scoped-name (tokens :: <list>)
 => (remaining-tokens :: <list>, scoped-name :: false-or(<ast-scoped-name>))
  if (empty?(tokens))
    error(make(<idl-expected-scoped-name>));
    values(#(), #f);
  else
    let scoped-name = make(<ast-scoped-name>);
    if (double-colon?(tokens.first))
      tokens := parse-full-scoped-name(tokens, scoped-name);
    else
      tokens := parse-partial-scoped-name(tokens, scoped-name);
    end if;
    values(tokens, scoped-name);
  end if;
end method;

define method parse-full-scoped-name (tokens :: <list>, scoped-name :: <ast-scoped-name>)
 => (remaining-tokens :: <list>)
  tokens := parse-double-colon(tokens);
  add!(scoped-name, make(<ast-identifier>, label: "::"));
  parse-partial-scoped-name(tokens, scoped-name);
end method;

define method parse-partial-scoped-name (tokens :: <list>, scoped-name :: <ast-scoped-name>)
 => (remaining-tokens :: <list>)
  tokens := parse-identifier(tokens, scoped-name);
  parse-optional-partial-scoped-name(tokens, scoped-name);
end method;

define method parse-optional-partial-scoped-name (tokens :: <list>, scoped-name :: <ast-scoped-name>)
 => (remaining-tokens :: <list>)
  unless (empty?(tokens) | ~double-colon?(tokens.first))
    tokens := parse-double-colon(tokens);
    tokens := parse-partial-scoped-name(tokens, scoped-name);
  end unless;
  tokens;
end method;

define method parse-identifier (tokens :: <list>, scoped-name :: <ast-scoped-name>)
 => (remaining-tokens :: <list>)
  if (empty?(tokens))
    error(make(<idl-expected-identifier>));
    #();
  else
    let token = tokens.first;
    if (identifier?(token))
      let identifier = make(<ast-identifier>, label: lexer-string(tokens.first));
      add!(scoped-name, identifier);
      tokens.tail;
    else
      error(make(<idl-expected-identifier>));
      tokens;
    end if;
  end if;
end method;

define method parse-double-colon (tokens :: <list>)
 => (remaining-tokens :: <list>)
  if (empty?(tokens) | ~double-colon?(tokens.first))
    error(make(<idl-expected-double-colon>));
    tokens;
  else
    tokens.tail;
  end if;
end method;

define method floating-point-literal? (token :: <token>)
 => (well? :: <boolean>)
    token.idl-parser-tag = $FLOATING-PT-LITERAL-token;
end method;

define method string-literal? (token :: <token>)
 => (well? :: <boolean>)
  token.idl-parser-tag = $STRING-LITERAL-token;
end method;

define method identifier? (token :: <token>)
 => (well? :: <boolean>)
  token.idl-parser-tag = $IDENTIFIER-token;
end method;

define method double-colon? (token :: <token>)
 => (well? :: <boolean>)
  token.idl-parser-tag = $DOUBLE-COLON-token;
end method;



// MAKE-TOKEN-STREAM
//
// built out of stuff from c-lexer lib
//
define method make-token-stream (file-stream :: <file-stream>, filename :: <string>)
//  make-cascaded-stream(vector(<IDL-lexer>, <cpp-stream>, <C++-lexer>, <pre-lexer>), file-stream, filename);
  make-cascaded-stream(vector(<IDL-lexer>, <IDL-cpp-stream>, <C++-lexer>, <pre-lexer>), file-stream, filename);
end method;

define macro with-token-stream
  { with-token-stream (?token-stream:name, ?file-stream:expression, ?file-name:expression ) ?:body end}
  =>
  { let ?token-stream = #f;
    block ()
      ?token-stream := make-token-stream(?file-stream, ?file-name);
      ?body;
    cleanup
      ?token-stream & close(?token-stream);
    end block; }
end macro;

define method make-cascaded-stream (stream-classes :: <sequence>, inner-stream :: <stream>, filename :: <string>)
  for (class in stream-classes using backward-iteration-protocol)
    inner-stream := make(class, inner-stream: inner-stream, source-name: filename);
  end for;
  inner-stream;
end method;

// Reference to $SYNTAX-ERROR-token here is to suppress a compiler warning.
// The code was copied from the C++ parser. I presume that the grammar
// has an error token so that the lexer can signal errors to the
// parser. I don't know when it would want to do this, other than perhaps
// as a test of error handling. For example, we added a mechanism that
// can damage the token stream in order to simulate buggy input.
//
// Alternatively, it may be a useful recovery mechanism. The error token
// is checked for at two important levels: top level level definitions
// outside interfaces, and top level definitions inside
// interfaces. Perhaps introducing the error token into the stream after
// a user syntax error will get it to skip to the next top level somehow?

ignore($SYNTAX-ERROR-token);
