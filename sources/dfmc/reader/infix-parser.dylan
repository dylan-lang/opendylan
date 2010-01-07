Module: dfmc-reader

// Notes: No right arrow for a single component production means just
// return that.

define constant $TOKEN-CONSTRAINT-token = 0;
define constant $token-token = 1;
define constant $END-CONSTRAINT-token = 2;
define constant $NAME-CONSTRAINT-token = 3;
define constant $EXPRESSION-CONSTRAINT-token = 4;
define constant $VARIABLE-CONSTRAINT-token = 5;
define constant $BODY-CONSTRAINT-token = 6;
define constant $CASE-BODY-CONSTRAINT-token = 7;
define constant $PROPERTY-LIST-CONSTRAINT-token = 8;
define constant $FRAGMENT-CONSTRAINT-token = 9;
define constant $SEMICOLON-token = 10;
define constant $EOF-token = 11;
define constant $HASH-T-token = 12;
define constant $HASH-F-token = 13;
define constant $HASH-NEXT-token = 14;
define constant $HASH-REST-token = 15;
define constant $HASH-KEY-token = 16;
define constant $HASH-ALL-KEYS-token = 17;
define constant $BEGIN-WORD-ONLY-token = 18;
define constant $FUNCTION-WORD-ONLY-token = 19;
define constant $DEFINE-BODY-WORD-ONLY-token = 20;
define constant $DEFINE-LIST-WORD-ONLY-token = 21;
define constant $BEGIN-AND-DEFINE-LIST-WORD-token = 22;
define constant $BEGIN-AND-DEFINE-BODY-WORD-token = 23;
define constant $FUNCTION-AND-DEFINE-LIST-WORD-token = 24;
define constant $FUNCTION-AND-DEFINE-BODY-WORD-token = 25;
define constant $LOCAL-DECLARATION-WORD-ONLY-token = 26;
define constant $LOCAL-METHODS-WORD-ONLY-token = 27;
define constant $MACRO-CASE-BEGIN-WORD-ONLY-token = 28;
define constant $MACRO-CASE-BEGIN-AND-DEFINE-MACRO-BODY-WORD-token = 29;
define constant $DEFINE-token = 30;
define constant $end-token = 31;
define constant $OTHERWISE-token = 32;
define constant $UNRESERVED-NAME-token = 33;
define constant $DEFINE-MACRO-BODY-WORD-ONLY-token = 34;
define constant $UNARY-OPERATOR-ONLY-token = 35;
define constant $UNARY-AND-BINARY-OPERATOR-token = 36;
define constant $BINARY-OPERATOR-ONLY-token = 37;
define constant $EQUAL-token = 38;
define constant $EQUAL-EQUAL-token = 39;
define constant $COMMA-token = 40;
define constant $SYMBOL-token = 41;
define constant $PARSED-FUNCTION-CALL-token = 42;
define constant $PARSED-MACRO-CALL-token = 43;
define constant $ESCAPED-SUBSTITUTION-token = 44;
define constant $CONSTRAINED-NAME-token = 45;
define constant $DOT-token = 46;
define constant $COLON-COLON-token = 47;
define constant $EQUAL-GREATER-token = 48;
define constant $QUERY-token = 49;
define constant $QUERY-QUERY-token = 50;
define constant $QUERY-EQUAL-token = 51;
define constant $QUERY-AT-token = 52;
define constant $ELLIPSIS-token = 53;
define constant $HASH-HASH-token = 54;
define constant $LPAREN-token = 55;
define constant $RPAREN-token = 56;
define constant $LBRACKET-token = 57;
define constant $RBRACKET-token = 58;
define constant $LBRACE-token = 59;
define constant $RBRACE-token = 60;
define constant $NUMBER-token = 61;
define constant $CHARACTER-LITERAL-token = 62;
define constant $STRING-token = 63;
define constant $PARSED-LOCAL-DECLARATION-token = 64;
define constant $HASH-LPAREN-token = 65;
define constant $HASH-LBRACKET-token = 66;
define constant $PARSED-LIST-CONSTANT-token = 67;
define constant $PARSED-VECTOR-CONSTANT-token = 68;
define constant $HASH-LBRACE-token = 69;

define function dylan-parser-action0 (arg$1) => (value)
  arg$1
end dylan-parser-action0;

define function dylan-parser-action1 (arg$1, arg$2, arg$3) => (value)
  arg$2
end dylan-parser-action1;

define function dylan-parser-action9 (arg$1, arg$2) => (value)
  arg$1
end dylan-parser-action9;

define function dylan-parser-action11 (arg$1) => (value)
  #f
end dylan-parser-action11;

define function dylan-parser-action46 () => (value)
  #f
end dylan-parser-action46;

define function dylan-parser-action80 () => (value)
  empty-body-fragment()
end dylan-parser-action80;

define function dylan-parser-action82 (arg$1, arg$2) => (value)
  body-fragment(elements(arg$1))
end dylan-parser-action82;

define function dylan-parser-action83 () => (value)
  #()
end dylan-parser-action83;

define function dylan-parser-action85 (arg$1) => (value)
  append-sequence(arg$1)
end dylan-parser-action85;

define function dylan-parser-action86 (arg$1, arg$2, arg$3) => (value)
  append-element!(arg$1, arg$3)
end dylan-parser-action86;

define function dylan-parser-action93 (arg$1, arg$2, arg$3) => (value)
  concatenate(arg$1, pair(arg$2, arg$3))
end dylan-parser-action93;

define function dylan-parser-action94 (arg$1, arg$2) => (value)
  pair(arg$1, arg$2)
end dylan-parser-action94;

define function dylan-parser-action146 (arg$1, arg$2, arg$3) => (value)
  make(<parens-fragment>, 
              record:           fragment-record(arg$1),
	      source-position:  position-between(arg$1, arg$3),
	      left-delimiter:   arg$1,
	      nested-fragments: arg$2,
	      right-delimiter:  arg$3)
end dylan-parser-action146;

define function dylan-parser-action147 (arg$1, arg$2, arg$3) => (value)
  make(<brackets-fragment>, 
              record:           fragment-record(arg$1),
	      source-position:  position-between(arg$1, arg$3),
	      left-delimiter:   arg$1,
	      nested-fragments: arg$2,
	      right-delimiter:  arg$3)
end dylan-parser-action147;

define function dylan-parser-action148 (arg$1, arg$2, arg$3) => (value)
  make(<braces-fragment>, 
              record:           fragment-record(arg$1),
	      source-position:  position-between(arg$1, arg$3),
	      left-delimiter:   arg$1,
	      nested-fragments: arg$2,
	      right-delimiter:  arg$3)
end dylan-parser-action148;

define function dylan-parser-action154 (arg$1, arg$2) => (value)
  make(<local-declaration-fragment>,
              record:          fragment-record(arg$1),
	      source-position: position-between(arg$1, arg$2),
              macro: arg$1,
              list-fragment: arg$2)
end dylan-parser-action154;

define function dylan-parser-action157 (arg$1) => (value)
  list(arg$1)
end dylan-parser-action157;

define function dylan-parser-action158 (arg$1, arg$2, arg$3) => (value)
  pair(arg$1, pair(arg$2, arg$3))
end dylan-parser-action158;

define function dylan-parser-action159 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-definition-tail
	     (arg$1, arg$1, maybe-defined-name(arg$2), arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
  	        source-position: position-between(arg$1, arg$3),
	        macro: arg$1,
                body-fragment: arg$2);
         end
end dylan-parser-action159;

define function dylan-parser-action160 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-definition-tail
	     (arg$1, arg$1, maybe-defined-name(arg$2), arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
	        source-position: position-between(arg$1, arg$3),
	        macro: arg$1,
                body-fragment: arg$2);
         end
end dylan-parser-action160;

define function dylan-parser-action161 (arg$1, arg$2, arg$3) => (value)
  begin
           let implicit-method = dylan-variable-name(#"method");
           verify-definition-tail
	     (arg$1, implicit-method, arg$1, arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
  	        source-position: position-between(arg$1, arg$3),
	        macro:           implicit-method,
                body-fragment:   pair(arg$1, arg$2));
         end
end dylan-parser-action161;

define function dylan-parser-action163 (arg$1, arg$2, arg$3, arg$4, arg$5) => (value)
  begin
           verify-definition-tail
             (arg$1, arg$3, maybe-defined-name(arg$4), arg$5);
	   make(<body-definition-fragment>,
                record:          fragment-record(arg$1),
	        source-position: position-between(arg$1, arg$5),
	        modifiers: arg$2,
	        define-word: arg$3,
	        body-fragment: arg$4);
         end
end dylan-parser-action163;

define function dylan-parser-action164 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<list-definition-fragment>,
              record:          fragment-record(arg$1),
	      source-position: position-between(arg$1, arg$4),
	      modifiers: arg$2,
	      define-word: arg$3,
	      list-fragment: arg$4)
end dylan-parser-action164;

define function dylan-parser-action165 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6) => (value)
  begin
           verify-definition-tail(arg$1, arg$3, arg$4, arg$6);
           make(<macro-body-definition-fragment>,
                record:          fragment-record(arg$1),
	        source-position: position-between(arg$1, arg$6),
	        modifiers: arg$2,
	        define-word: arg$3,
	        macro-body-fragment: pair(arg$4, arg$5));
         end
end dylan-parser-action165;

define function dylan-parser-action171 (arg$1) => (value)
  make(<definition-tail-fragment>, 
              record:          fragment-record(arg$1),
              source-position: fragment-source-position(arg$1),
              end: arg$1)
end dylan-parser-action171;

define function dylan-parser-action172 (arg$1, arg$2) => (value)
  make(<definition-tail-fragment>, 
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              end: arg$1, tail-name-1: arg$2)
end dylan-parser-action172;

define function dylan-parser-action173 (arg$1, arg$2, arg$3) => (value)
  make(<definition-tail-fragment>, 
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              end: arg$1, tail-name-1: arg$2, tail-name-2: arg$3)
end dylan-parser-action173;

define function dylan-parser-action174 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<prefix-call-fragment>, 
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$4),
              function: arg$1, arguments: arg$3)
end dylan-parser-action174;

define function dylan-parser-action175 (arg$1, arg$2, arg$3, arg$4) => (value)
  begin
	   let getter-name 
             = if (size(arg$3) = 1) #"element" else #"aref" end;
           make(<array-call-fragment>, 
                record:          fragment-record(arg$1),
                source-position: position-between(arg$1, arg$4),
                function:
                  make-variable-name-like
                    (arg$2,
                     record:          fragment-record(arg$2),
		     source-position: position-between(arg$2, arg$4),
		     name: getter-name,
		     kind: syntax-for-name(*current-module*, getter-name)),
	        arguments: pair(arg$1, arg$3));
         end
end dylan-parser-action175;

define function dylan-parser-action176 (arg$1, arg$2, arg$3) => (value)
  make(<dot-call-fragment>, 
	      record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              function: arg$3, arguments: list(arg$1))
end dylan-parser-action176;

define function dylan-parser-action178 (arg$1) => (value)
  elements(arg$1)
end dylan-parser-action178;

define function dylan-parser-action181 (arg$1, arg$2) => (value)
  list(arg$1, arg$2)
end dylan-parser-action181;

define function dylan-parser-action184 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<function-macro-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$4),
              macro: arg$1,
              body-fragment: arg$3)
end dylan-parser-action184;

define function dylan-parser-action199 (arg$1, arg$2) => (value)
  make(<string-fragment>, 
	      record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              value: concatenate(fragment-value(arg$1), fragment-value(arg$2)))
end dylan-parser-action199;

define function dylan-parser-action200 (arg$1, arg$2, arg$3, arg$4, arg$5) => (value)
  make(<improper-list-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$5),
	      elements:        arg$2,
	      improper-tail:   arg$4)
end dylan-parser-action200;

define function dylan-parser-action201 (arg$1, arg$2, arg$3) => (value)
  make(<proper-list-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
	      elements:        arg$2)
end dylan-parser-action201;

define function dylan-parser-action202 (arg$1, arg$2, arg$3) => (value)
  make(<vector-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
	      elements:        arg$2)
end dylan-parser-action202;

define function dylan-parser-action217 (arg$1, arg$2, arg$3) => (value)
  list(arg$1, arg$2, arg$3)
end dylan-parser-action217;

define function dylan-parser-action219 (arg$1) => (value)
  make-variable-name-like
	   (arg$1, 
            record:          fragment-record(arg$1),
            source-position: fragment-source-position(arg$1),
	    name: #"...")
end dylan-parser-action219;

define function dylan-parser-action223 (arg$1) => (value)
  binop-fragment(arg$1)
end dylan-parser-action223;

define function dylan-parser-action224 (arg$1) => (value)
  (arg$1)
end dylan-parser-action224;

define function dylan-parser-action225 (arg$1, arg$2, arg$3) => (value)
  append-binop!(arg$1, arg$2, arg$3)
end dylan-parser-action225;

define function dylan-parser-action232 (arg$1, arg$2) => (value)
  make(<unary-operator-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              function: arg$1,
              arguments: list(arg$2))
end dylan-parser-action232;

define function dylan-parser-action233 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-statement-tail(arg$1, arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
  	        source-position: position-between(arg$1, arg$3),
	        macro: arg$1,
                body-fragment: arg$2);
         end
end dylan-parser-action233;

define function dylan-parser-action234 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-statement-tail(arg$1, arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
	        source-position: position-between(arg$1, arg$3),
	        macro: arg$1,
                body-fragment: arg$2);
         end
end dylan-parser-action234;

define function dylan-parser-action235 (arg$1, arg$2, arg$3) => (value)
  make(<statement-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              macro:
                make-variable-name-like
                  (arg$1, 
                   record:          fragment-record(arg$1),
                   source-position: fragment-source-position(arg$1),
                   name:            #"macro-template"),
              body-fragment:   arg$2)
end dylan-parser-action235;

define function dylan-parser-action236 (arg$1, arg$2) => (value)
  arg$2 | arg$1
end dylan-parser-action236;

define function dylan-parser-action240 (arg$1, arg$2) => (value)
  concatenate(arg$1, arg$2)
end dylan-parser-action240;

define function dylan-parser-action242 (arg$1, arg$2, arg$3) => (value)
  concatenate(arg$1, list(arg$2), arg$3)
end dylan-parser-action242;

define function dylan-parser-action243 (arg$1, arg$2, arg$3, arg$4) => (value)
  concatenate(arg$1, list(body-fragment(elements(arg$2))), list(arg$3), arg$4)
end dylan-parser-action243;

define function dylan-parser-action245 (arg$1, arg$2, arg$3) => (value)
  pair(body-fragment(elements(arg$1)), pair(arg$2, arg$3))
end dylan-parser-action245;

define function dylan-parser-action251 (arg$1, arg$2) => (value)
  concatenate(arg$1, list(arg$2))
end dylan-parser-action251;

define function dylan-parser-action252 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6) => (value)
  pair(make(<parens-fragment>, 
                   record:           fragment-record(arg$1),
	           source-position:  position-between(arg$1, arg$5),
	           left-delimiter:   arg$1,
	           nested-fragments: pair(arg$2, pair(arg$3, arg$4)),
	           right-delimiter:  arg$5),
              list(arg$6))
end dylan-parser-action252;

define function dylan-parser-action306 (arg$1, arg$2, arg$3) => (value)
  pair(arg$1, concatenate(arg$2, list(arg$3)))
end dylan-parser-action306;

define function dylan-parser-action336 (arg$1, arg$2, arg$3, arg$4) => (value)
  begin
	   let first = arg$1 | arg$2;
           let last  = arg$4 | arg$3;
	   make(<spliced-pattern-variable-fragment>,
                record:           fragment-record(first),
                source-position:  position-between(first, last),
                prefix:           arg$1 & fragment-value(arg$1),
                suffix:           arg$4 & fragment-value(arg$4),
                pattern-variable: arg$3);
         end
end dylan-parser-action336;

define function dylan-parser-action337 (arg$1, arg$2) => (value)
  make(<sequence-pattern-variable-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              name:            arg$2,
              separator:       #f)
end dylan-parser-action337;

define function dylan-parser-action340 (arg$1, arg$2) => (value)
  make(<unhygienic-name-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              name:            arg$2)
end dylan-parser-action340;

define function dylan-parser-action341 (arg$1, arg$2, arg$3) => (value)
  make(<template-aux-rule-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              rule-name:       arg$2,
              template:        arg$3)
end dylan-parser-action341;

define function dylan-parser-action342 (arg$1, arg$2) => (value)
  make(<template-macro-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              template:        arg$2)
end dylan-parser-action342;

define function dylan-parser-action348 (arg$1, arg$2) => (value)
  arg$2
end dylan-parser-action348;

define function dylan-parser-action353 (arg$1, arg$2, arg$3) => (value)
  arg$1
end dylan-parser-action353;

define constant dylan-parser :: <parser>
  = make(<parser>,
  action-table:
      #[#[3, -24, 4, -66, 5, -61, 6, -23, 7, -7, 8, -44, 9, -22, 11, -63, 12, -29, 13, -17, 18, -62, 19, -51, 21, -53, 22, -10, 24, -54, 25, -55, 26, -37, 28, -3, 29, -16, 30, -43, 33, -56, 34, -32, 35, -38, 36, -15, 41, -21, 42, -47, 43, -31, 53, -39, 55, -6, 61, -5, 20, -35, 62, -59, 27, -12, 63, -45, 66, -65, 64, -68, 0, -18, 23, -67, 65, -46, 68, -25, 67, -50, 69, -40],
	#[65535, 37, 2, 37, 10, 37, 11, 37, 12, 37, 13, 37, 14, 37, 15, 37, 16, 37, 17, 37, 18, 37, 19, 37, 20, 37, 21, 37, 22, 37, 23, 37, 24, 37, 25, 37, 26, 37, 27, 37, 28, 37, 29, 37, 30, 37, 31, 37, 32, 37, 33, 37, 34, 37, 35, 37, 36, 37, 37, 37, 38, 37, 39, 37, 40, 37, 41, 37, 42, 37, 43, 37, 44, 37, 45, 37, 46, 37, 47, 37, 48, 37, 49, 37, 50, 37, 51, 37, 52, 37, 53, 37, 54, 37, 55, 37, 56, 37, 57, 37, 58, 37, 59, 37, 60, 37, 61, 37, 62, 37, 63, 37, 64, 37, 65, 37, 66, 37, 67, 37, 68, 37, 69, 37],
	#[65535, 53, 14, 53, 20, 53, 22, 53, 13, 53, 15, 53, 2, 53, 11, 53, 30, 53, 21, 53, 23, 53, 10, 53, 19, 53, 17, 53, 38, 53, 16, 53, 29, 53, 31, 53, 18, 53, 27, 53, 25, 53, 46, 53, 12, 53, 24, 53, 37, 53, 39, 53, 26, 53, 35, 53, 33, 53, 54, 53, 49, 53, 32, 53, 45, 53, 47, 53, 40, 53, 43, 53, 41, 53, 62, 53, 28, 53, 50, 53, 53, 53, 55, 53, 42, 53, 51, 53, 68, 53, 34, 53, 36, 53, 58, 53, 61, 53, 63, 53, 56, 53, 59, 53, 57, 53, 60, 53, 44, 53, 66, 53, 69, 53, 48, 53, 64, 53, 67, 53, 65, 53, 52, 53],
	#[65535, 162, 2, 162, 10, 162, 11, 162, 31, 162, 56, 162, 58, 162, 60, 162],
	#[65535, 192, 2, 192, 10, 192, 11, 192, 36, 192, 37, 192, 38, 192, 39, 192, 40, 192, 46, 192, 48, 192, 55, 192, 56, 192, 57, 192, 58, 192],
	#[23, -67, 36, -15, 13, -17, 61, -5, 42, -47, 62, -59, 28, -3, 12, -29, 53, -39, 55, -6, 33, -56, 22, -10, 68, -25, 34, -32, 21, -53, 20, -35, 43, -31, 63, -45, 19, -51, 18, -62, 35, -38, 29, -16, 66, -65, 69, -40, 41, -21, 24, -54, 67, -50, 65, -46, 25, -55],
	#[2, 238, 12, -29, 13, -17, 18, -62, 19, -51, 20, -35, 21, -53, 22, -10, 23, -67, 24, -54, 25, -55, 28, -3, 29, -16, 32, -420, 33, -56, 34, -32, 35, -38, 36, -15, 41, -21, 42, -47, 43, -31, 53, -39, 55, -419, 61, -5, 62, -59, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 87, 2, 87, 10, 87, 11, 87],
	#[65535, 189, 38, 189, 11, 189, 39, 189, 36, 189, 10, 189, 40, 189, 2, 189, 58, 189, 55, 189, 37, 189, 46, 189, 48, 189, 57, 189, 56, 189],
	#[65535, 50, 2, 50, 10, 50, 11, 50, 12, 50, 13, 50, 14, 50, 15, 50, 16, 50, 17, 50, 18, 50, 19, 50, 20, 50, 21, 50, 22, 50, 23, 50, 24, 50, 25, 50, 26, 50, 27, 50, 28, 50, 29, 50, 30, 50, 31, 50, 32, 50, 33, 50, 34, 50, 35, 50, 36, 50, 37, 50, 38, 50, 39, 50, 40, 50, 41, 50, 42, 50, 43, 50, 44, 50, 45, 50, 46, 50, 47, 50, 48, 50, 49, 50, 50, 50, 51, 50, 52, 50, 53, 50, 54, 50, 55, 50, 56, 50, 57, 50, 58, 50, 59, 50, 60, 50, 61, 50, 62, 50, 63, 50, 64, 50, 65, 50, 66, 50, 67, 50, 68, 50, 69, 50],
	#[65535, 230, 10, 230, 2, 230, 38, 230, 37, 230, 36, 230, 39, 230, 11, 230, 58, 230, 40, 230, 56, 230, 48, 230],
	#[65535, 64, 29, 64, 28, 64, 21, 64, 18, 64, 22, 64, 34, 64, 53, 64, 23, 64, 20, 64, 33, 64],
	#[65535, 197, 2, 197, 10, 197, 11, 197, 36, 197, 37, 197, 38, 197, 39, 197, 40, 197, 46, 197, 48, 197, 55, 197, 56, 197, 57, 197, 58, 197],
	#[65535, 224, 10, 224, 2, 224, 38, 224, 37, 224, 36, 224, 39, 224, 11, 224, 58, 224, 40, 224, 48, 224, 56, 224],
	#[65535, 69, 61, 69, 62, 69, 19, 69, 22, 69, 42, 69, 29, 69, 23, 69, 55, 69, 18, 69, 28, 69, 21, 69, 63, 69, 67, 69, 65, 69, 24, 69, 69, 69, 13, 69, 34, 69, 53, 69, 12, 69, 25, 69, 43, 69, 68, 69, 66, 69, 20, 69, 33, 69],
	#[65535, 54, 2, 54, 10, 54, 11, 54, 12, 54, 13, 54, 14, 54, 15, 54, 16, 54, 17, 54, 18, 54, 19, 54, 20, 54, 21, 54, 22, 54, 23, 54, 24, 54, 25, 54, 26, 54, 27, 54, 28, 54, 29, 54, 30, 54, 31, 54, 32, 54, 33, 54, 34, 54, 35, 54, 36, 54, 37, 54, 38, 54, 39, 54, 40, 54, 41, 54, 42, 54, 43, 54, 44, 54, 45, 54, 46, 54, 47, 54, 48, 54, 49, 54, 50, 54, 51, 54, 52, 54, 53, 54, 54, 54, 55, 54, 56, 54, 57, 54, 58, 54, 59, 54, 60, 54, 61, 54, 62, 54, 63, 54, 64, 54, 65, 54, 66, 54, 67, 54, 68, 54, 69, 54],
	#[65535, 196, 39, 196, 10, 196, 2, 196, 38, 196, 55, 196, 37, 196, 46, 196, 36, 196, 40, 196, 11, 196, 58, 196, 57, 196, 56, 196, 48, 196],
	#[1, -417],
	#[65535, 194, 2, 194, 10, 194, 11, 194, 36, 194, 37, 194, 38, 194, 39, 194, 40, 194, 46, 194, 48, 194, 55, 194, 56, 194, 57, 194, 58, 194],
	#[10, -268, 12, -77, 13, -92, 14, -82, 15, -80, 16, -102, 17, -96, 18, -62, 19, -51, 20, -35, 21, -53, 22, -10, 23, -67, 24, -54, 25, -55, 26, -37, 27, -12, 28, -3, 29, -16, 30, -43, 31, 256, 32, -165, 33, -56, 34, -32, 35, -176, 36, -170, 37, -157, 38, -154, 39, -175, 40, -284, 41, -168, 42, -285, 43, -280, 44, -276, 45, -271, 46, -194, 47, -188, 48, -181, 49, -180, 50, -161, 51, -196, 52, -193, 53, -192, 54, -179, 55, -269, 57, -288, 59, -141, 61, -156, 62, -191, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 229, 2, 229, 10, 229, 11, 229, 36, 229, 37, 229, 38, 229, 39, 229, 40, 229, 48, 229, 56, 229, 58, 229],
	#[14, -82, 22, -10, 13, -92, 15, -80, 30, -43, 21, -53, 23, -67, 10, -155, 19, -51, 17, -96, 2, 96, 16, -102, 18, -62, 27, -12, 25, -55, 46, -194, 12, -77, 37, -157, 39, -175, 26, -37, 35, -176, 24, -54, 36, -170, 49, -180, 45, -164, 47, -188, 34, -32, 29, -16, 38, -154, 62, -191, 28, -3, 40, -178, 53, -192, 55, -158, 33, -56, 51, -196, 68, -25, 52, -193, 32, -165, 20, -35, 43, -172, 63, -182, 50, -161, 59, -169, 54, -179, 42, -183, 44, -166, 66, -65, 69, -40, 41, -168, 64, -68, 57, -186, 65, -46, 48, -181, 67, -50, 61, -156],
	#[23, -67, 13, -17, 53, -39, 2, 80, 21, -53, 30, -43, 27, -12, 33, -56, 62, -59, 28, -3, 12, -29, 35, -38, 55, -6, 42, -47, 22, -10, 68, -25, 34, -32, 36, -15, 20, -35, 43, -31, 63, -45, 19, -51, 18, -62, 24, -54, 29, -16, 66, -65, 69, -40, 41, -21, 64, -68, 26, -37, 65, -46, 25, -55, 67, -50, 61, -5],
	#[20, -35, 21, -53, 33, -56, 34, -32, 53, -39],
	#[65535, 204, 14, 204, 22, 204, 46, 204, 13, 204, 15, 204, 28, 204, 30, 204, 21, 204, 23, 204, 10, 204, 19, 204, 17, 204, 2, 204, 33, 204, 11, 204, 31, 204, 18, 204, 27, 204, 16, 204, 53, 204, 12, 204, 34, 204, 37, 204, 39, 204, 26, 204, 25, 204, 24, 204, 36, 204, 20, 204, 45, 204, 47, 204, 40, 204, 29, 204, 38, 204, 62, 204, 59, 204, 50, 204, 35, 204, 55, 204, 42, 204, 51, 204, 68, 204, 52, 204, 32, 204, 58, 204, 43, 204, 63, 204, 56, 204, 49, 204, 54, 204, 60, 204, 44, 204, 66, 204, 69, 204, 41, 204, 64, 204, 57, 204, 65, 204, 48, 204, 67, 204, 61, 204],
	#[65535, 177, 39, 177, 36, 177, 10, 177, 38, 177, 55, 177, 37, 177, 46, 177, 2, 177, 40, 177, 11, 177, 58, 177, 57, 177, 48, 177, 56, 177],
	#[2, 223, 10, 223, 11, 223, 36, -88, 37, -74, 38, -127, 39, -119, 40, 223, 48, 223, 56, 223, 58, 223],
	#[#"eoi", #"accept"],
	#[65535, 195, 39, 195, 36, 195, 38, 195, 55, 195, 10, 195, 46, 195, 2, 195, 40, 195, 11, 195, 48, 195, 58, 195, 57, 195, 37, 195, 56, 195],
	#[10, -155, 12, -77, 13, -92, 14, -82, 15, -80, 16, -102, 17, -96, 18, -62, 19, -51, 20, -35, 21, -53, 22, -10, 23, -67, 24, -54, 25, -55, 26, -37, 27, -12, 28, -3, 29, -16, 30, -43, 31, 96, 32, -165, 33, -56, 34, -32, 35, -176, 36, -170, 37, -157, 38, -154, 39, -175, 40, -178, 41, -168, 42, -183, 43, -172, 44, -166, 45, -164, 46, -194, 47, -188, 48, -181, 49, -180, 50, -161, 51, -196, 52, -193, 53, -192, 54, -179, 55, -158, 57, -186, 59, -169, 61, -156, 62, -191, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 191, 39, 191, 10, 191, 2, 191, 38, 191, 55, 191, 37, 191, 46, 191, 36, 191, 40, 191, 11, 191, 58, 191, 57, 191, 48, 191, 56, 191],
	#[65535, 38, 2, 38, 10, 38, 11, 38, 12, 38, 13, 38, 14, 38, 15, 38, 16, 38, 17, 38, 18, 38, 19, 38, 20, 38, 21, 38, 22, 38, 23, 38, 24, 38, 25, 38, 26, 38, 27, 38, 28, 38, 29, 38, 30, 38, 31, 38, 32, 38, 33, 38, 34, 38, 35, 38, 36, 38, 37, 38, 38, 38, 39, 38, 40, 38, 41, 38, 42, 38, 43, 38, 44, 38, 45, 38, 46, 38, 47, 38, 48, 38, 49, 38, 50, 38, 51, 38, 52, 38, 53, 38, 54, 38, 55, 38, 56, 38, 57, 38, 58, 38, 59, 38, 60, 38, 61, 38, 62, 38, 63, 38, 64, 38, 65, 38, 66, 38, 67, 38, 68, 38, 69, 38],
	#[65535, 89, 2, 89, 10, 89, 11, 89],
	#[65535, 88, 2, 88, 10, 88, 11, 88],
	#[65535, 34, 2, 34, 10, 34, 11, 34, 12, 34, 13, 34, 14, 34, 15, 34, 16, 34, 17, 34, 18, 34, 19, 34, 20, 34, 21, 34, 22, 34, 23, 34, 24, 34, 25, 34, 26, 34, 27, 34, 28, 34, 29, 34, 30, 34, 31, 34, 32, 34, 33, 34, 34, 34, 35, 34, 36, 34, 37, 34, 38, 34, 39, 34, 40, 34, 41, 34, 42, 34, 43, 34, 44, 34, 45, 34, 46, 34, 47, 34, 48, 34, 49, 34, 50, 34, 51, 34, 52, 34, 53, 34, 54, 34, 55, 34, 56, 34, 57, 34, 58, 34, 59, 34, 60, 34, 61, 34, 62, 34, 63, 34, 64, 34, 65, 34, 66, 34, 67, 34, 68, 34, 69, 34],
	#[65535, 185, 39, 185, 10, 185, 2, 185, 38, 185, 55, 185, 37, 185, 46, 185, 36, 185, 40, 185, 11, 185, 58, 185, 57, 185, 56, 185, 48, 185],
	#[65535, 63, 22, 63, 15, 63, 2, 63, 11, 63, 21, 63, 23, 63, 10, 63, 19, 63, 14, 63, 38, 63, 16, 63, 29, 63, 31, 63, 13, 63, 25, 63, 46, 63, 12, 63, 37, 63, 39, 63, 17, 63, 35, 63, 24, 63, 18, 63, 49, 63, 32, 63, 45, 63, 47, 63, 40, 63, 33, 63, 41, 63, 62, 63, 28, 63, 50, 63, 53, 63, 55, 63, 42, 63, 51, 63, 68, 63, 34, 63, 36, 63, 20, 63, 43, 63, 63, 63, 56, 63, 59, 63, 54, 63, 60, 63, 44, 63, 66, 63, 69, 63, 48, 63, 58, 63, 57, 63, 65, 63, 52, 63, 67, 63, 61, 63],
	#[65535, 68, 12, 68, 13, 68, 18, 68, 19, 68, 20, 68, 21, 68, 22, 68, 23, 68, 24, 68, 25, 68, 28, 68, 29, 68, 33, 68, 34, 68, 42, 68, 43, 68, 53, 68, 55, 68, 61, 68, 62, 68, 63, 68, 65, 68, 66, 68, 67, 68, 68, 68, 69, 68],
	#[65535, 219, 14, 219, 20, 219, 22, 219, 13, 219, 15, 219, 2, 219, 11, 219, 21, 219, 23, 219, 10, 219, 19, 219, 17, 219, 38, 219, 16, 219, 29, 219, 31, 219, 18, 219, 27, 219, 25, 219, 46, 219, 12, 219, 24, 219, 37, 219, 39, 219, 26, 219, 35, 219, 30, 219, 54, 219, 49, 219, 32, 219, 45, 219, 47, 219, 40, 219, 43, 219, 41, 219, 62, 219, 28, 219, 50, 219, 53, 219, 55, 219, 33, 219, 51, 219, 68, 219, 34, 219, 36, 219, 58, 219, 61, 219, 63, 219, 56, 219, 59, 219, 57, 219, 42, 219, 44, 219, 66, 219, 69, 219, 48, 219, 64, 219, 67, 219, 65, 219, 52, 219],
	#[13, -92, 15, -80, 21, -94, 23, -75, 10, -71, 19, -84, 14, -82, 16, -102, 29, -124, 31, -100, 17, -96, 22, -89, 46, -130, 24, -83, 37, -74, 39, -119, 26, -98, 35, -105, 30, -110, 18, -79, 49, 343, 32, -90, 45, -78, 47, -120, 25, -121, 33, -70, 38, -127, 62, -131, 28, -107, 12, -77, 53, -108, 55, -76, 42, -114, 51, -133, 68, -118, 27, -85, 36, -88, 20, -122, 43, -101, 40, -109, 50, -81, 59, -97, 60, 300, 44, -91, 66, -132, 69, -99, 48, -104, 64, -126, 57, -123, 65, -113, 52, -128, 41, -95, 61, -72, 63, -112, 67, -115],
	#[65535, 188, 2, 188, 10, 188, 11, 188, 36, 188, 37, 188, 38, 188, 39, 188, 40, 188, 46, 188, 48, 188, 55, 188, 56, 188, 57, 188, 58, 188],
	#[23, -67, 62, -59, 28, -3, 12, -29, 55, -6, 33, -56, 22, -10, 68, -25, 34, -32, 21, -53, 20, -35, 43, -31, 63, -45, 19, -51, 18, -62, 42, -47, 29, -16, 66, -65, 69, -40, 13, -17, 24, -54, 53, -39, 65, -46, 25, -55, 67, -50, 61, -5],
	#[23, 167, 22, 167, 21, 167, 20, 167, 33, -240, 29, 167, 34, 167, 24, 167, 25, 167],
	#[2, 90, 41, -389],
	#[10, 198, 2, 198, 39, 198, 11, 198, 38, 198, 37, 198, 36, 198, 40, 198, 46, 198, 48, 198, 55, 198, 56, 198, 57, 198, 58, 198, 63, -45],
	#[12, -29, 13, -17, 41, -197, 56, 205, 61, -5, 62, -59, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25],
	#[65535, 190, 2, 190, 10, 190, 11, 190, 36, 190, 37, 190, 38, 190, 39, 190, 40, 190, 46, 190, 48, 190, 55, 190, 56, 190, 57, 190, 58, 190],
	#[65535, 186, 39, 186, 10, 186, 2, 186, 38, 186, 55, 186, 37, 186, 46, 186, 36, 186, 40, 186, 11, 186, 58, 186, 57, 186, 48, 186, 56, 186],
	#[2, 231, 10, 231, 11, 231, 36, 231, 37, 231, 38, 231, 39, 231, 40, 231, 46, -352, 48, 231, 55, -351, 56, 231, 57, -350, 58, 231],
	#[65535, 203, 14, 203, 2, 203, 15, 203, 10, 203, 17, 203, 29, 203, 23, 203, 12, 203, 18, 203, 25, 203, 11, 203, 22, 203, 31, 203, 20, 203, 26, 203, 19, 203, 30, 203, 39, 203, 28, 203, 33, 203, 13, 203, 27, 203, 38, 203, 16, 203, 32, 203, 37, 203, 36, 203, 40, 203, 21, 203, 35, 203, 34, 203, 24, 203, 41, 203, 42, 203, 43, 203, 44, 203, 45, 203, 46, 203, 47, 203, 48, 203, 49, 203, 50, 203, 51, 203, 52, 203, 53, 203, 54, 203, 55, 203, 56, 203, 57, 203, 58, 203, 59, 203, 60, 203, 61, 203, 62, 203, 63, 203, 64, 203, 65, 203, 66, 203, 67, 203, 68, 203, 69, 203],
	#[65535, 65, 55, 65],
	#[18, -62, 20, -35, 21, -53, 22, -10, 23, -67, 28, -3, 29, -16, 33, -56, 34, -32, 53, -39],
	#[65535, 35, 2, 35, 23, 35, 31, 35, 18, 35, 10, 35, 11, 35, 12, 35, 13, 35, 14, 35, 15, 35, 16, 35, 17, 35, 47, 35, 19, 35, 20, 35, 21, 35, 22, 35, 34, 35, 24, 35, 25, 35, 26, 35, 27, 35, 28, 35, 29, 35, 30, 35, 54, 35, 32, 35, 33, 35, 63, 35, 35, 35, 36, 35, 37, 35, 38, 35, 39, 35, 40, 35, 41, 35, 42, 35, 43, 35, 44, 35, 45, 35, 46, 35, 58, 35, 48, 35, 49, 35, 50, 35, 51, 35, 52, 35, 53, 35, 67, 35, 55, 35, 56, 35, 57, 35, 65, 35, 59, 35, 60, 35, 61, 35, 62, 35, 64, 35, 66, 35, 68, 35, 69, 35],
	#[65535, 67, 55, 67],
	#[65535, 66, 55, 66],
	#[65535, 36, 14, 36, 20, 36, 22, 36, 13, 36, 15, 36, 2, 36, 11, 36, 21, 36, 23, 36, 10, 36, 19, 36, 17, 36, 38, 36, 16, 36, 29, 36, 31, 36, 27, 36, 25, 36, 46, 36, 12, 36, 24, 36, 37, 36, 39, 36, 26, 36, 35, 36, 30, 36, 18, 36, 49, 36, 32, 36, 45, 36, 47, 36, 40, 36, 33, 36, 41, 36, 62, 36, 28, 36, 50, 36, 53, 36, 55, 36, 42, 36, 51, 36, 68, 36, 34, 36, 36, 36, 58, 36, 43, 36, 63, 36, 56, 36, 59, 36, 54, 36, 60, 36, 44, 36, 66, 36, 69, 36, 48, 36, 64, 36, 57, 36, 65, 36, 52, 36, 67, 36, 61, 36],
	#[2, 100, 10, 100, 11, 100, 12, -77, 13, -92, 14, -82, 15, -80, 16, -102, 17, -96, 18, -62, 19, -51, 20, -35, 21, -53, 22, -10, 23, -67, 24, -54, 25, -55, 55, -158, 28, -3, 29, -16, 31, 100, 32, -165, 33, -56, 34, -32, 35, -176, 36, -170, 37, -157, 38, -154, 39, -175, 40, -217, 41, -168, 42, -183, 43, -172, 44, -166, 45, -164, 46, -194, 47, -188, 48, -181, 49, -180, 50, -161, 51, -196, 52, -193, 53, -192, 54, -179, 66, -65, 56, 100, 57, -186, 58, 100, 59, -169, 60, 100, 61, -156, 62, -191, 63, -182, 65, -46, 67, -50, 68, -25, 69, -40],
	#[10, -388, 11, -387],
	#[65535, 193, 2, 193, 10, 193, 11, 193, 36, 193, 37, 193, 38, 193, 39, 193, 40, 193, 46, 193, 48, 193, 55, 193, 56, 193, 57, 193, 58, 193],
	#[55, -153],
	#[20, -35, 21, -53, 33, -56, 34, -32, 53, -39],
	#[65535, 48, 14, 48, 22, 48, 2, 48, 30, 48, 15, 48, 10, 48, 17, 48, 29, 48, 23, 48, 12, 48, 18, 48, 25, 48, 11, 48, 37, 48, 16, 48, 31, 48, 20, 48, 26, 48, 19, 48, 32, 48, 39, 48, 28, 48, 33, 48, 13, 48, 27, 48, 38, 48, 62, 48, 43, 48, 52, 48, 36, 48, 40, 48, 21, 48, 35, 48, 34, 48, 24, 48, 41, 48, 42, 48, 48, 48, 44, 48, 45, 48, 46, 48, 47, 48, 49, 48, 50, 48, 51, 48, 53, 48, 54, 48, 55, 48, 56, 48, 57, 48, 58, 48, 59, 48, 60, 48, 61, 48, 63, 48, 64, 48, 65, 48, 66, 48, 67, 48, 68, 48, 69, 48],
	#[65535, 11, #"eoi", 11],
	#[65535, 218, 14, 218, 22, 218, 13, 218, 15, 218, 2, 218, 11, 218, 21, 218, 23, 218, 10, 218, 19, 218, 17, 218, 20, 218, 16, 218, 29, 218, 31, 218, 27, 218, 25, 218, 46, 218, 12, 218, 24, 218, 37, 218, 39, 218, 26, 218, 35, 218, 30, 218, 18, 218, 49, 218, 32, 218, 45, 218, 47, 218, 40, 218, 43, 218, 38, 218, 62, 218, 28, 218, 50, 218, 53, 218, 55, 218, 33, 218, 41, 218, 68, 218, 34, 218, 36, 218, 58, 218, 61, 218, 63, 218, 56, 218, 59, 218, 54, 218, 42, 218, 44, 218, 66, 218, 51, 218, 48, 218, 64, 218, 57, 218, 65, 218, 52, 218, 67, 218, 69, 218],
	#[62, -59, 13, -17, 68, -25, 61, -5, 63, -45, 66, -65, 12, -29, 41, -197, 58, 205, 65, -46, 67, -50],
	#[12, -29, 13, -17, 18, -62, 19, -51, 20, -35, 21, -53, 22, -10, 23, -67, 24, -54, 25, -55, 28, -3, 29, -16, 33, -56, 34, -32, 35, -38, 36, -15, 41, -21, 42, -47, 43, -31, 53, -39, 55, -6, 61, -5, 62, -59, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 49, 14, 49, 31, 49, 2, 49, 21, 49, 30, 49, 11, 49, 15, 49, 10, 49, 17, 49, 29, 49, 38, 49, 26, 49, 23, 49, 12, 49, 18, 49, 42, 49, 22, 49, 36, 49, 20, 49, 43, 49, 33, 49, 19, 49, 32, 49, 60, 49, 39, 49, 28, 49, 69, 49, 13, 49, 27, 49, 40, 49, 16, 49, 50, 49, 25, 49, 49, 49, 35, 49, 34, 49, 24, 49, 48, 49, 41, 49, 37, 49, 51, 49, 62, 49, 44, 49, 45, 49, 46, 49, 47, 49, 68, 49, 52, 49, 53, 49, 54, 49, 55, 49, 56, 49, 57, 49, 58, 49, 59, 49, 61, 49, 63, 49, 64, 49, 65, 49, 66, 49, 67, 49],
	#[65535, 156, 2, 156, 31, 156, 60, 156, 10, 156, 11, 156, 58, 156, 56, 156],
	#[2, -380],
	#[65535, 39, 14, 39, 22, 39, 13, 39, 15, 39, 30, 39, 21, 39, 23, 39, 10, 39, 19, 39, 17, 39, 16, 39, 29, 39, 31, 39, 27, 39, 25, 39, 46, 39, 12, 39, 24, 39, 37, 39, 39, 39, 26, 39, 35, 39, 33, 39, 18, 39, 20, 39, 32, 39, 45, 39, 47, 39, 40, 39, 43, 39, 38, 39, 49, 39, 28, 39, 50, 39, 53, 39, 55, 39, 42, 39, 41, 39, 68, 39, 34, 39, 36, 39, 58, 39, 61, 39, 63, 39, 56, 39, 59, 39, 54, 39, 60, 39, 44, 39, 66, 39, 69, 39, 48, 39, 64, 39, 57, 39, 62, 39, 52, 39, 51, 39, 65, 39, 67, 39],
	#[65535, 333, 14, 333, 20, 333, 22, 333, 13, 333, 15, 333, 30, 333, 21, 333, 23, 333, 10, 333, 19, 333, 17, 333, 38, 333, 16, 333, 29, 333, 31, 333, 18, 333, 27, 333, 25, 333, 46, 333, 12, 333, 24, 333, 37, 333, 39, 333, 26, 333, 35, 333, 33, 333, 41, 333, 49, 333, 32, 333, 45, 333, 47, 333, 40, 333, 43, 333, 60, 333, 62, 333, 28, 333, 50, 333, 53, 333, 55, 333, 42, 333, 51, 333, 68, 333, 52, 333, 36, 333, 58, 333, 61, 333, 63, 333, 56, 333, 59, 333, 57, 333, 44, 333, 66, 333, 69, 333, 48, 333, 64, 333, 67, 333, 65, 333],
	#[65535, 311, 10, 311, 12, 311, 13, 311, 14, 311, 15, 311, 16, 311, 17, 311, 18, 311, 19, 311, 20, 311, 21, 311, 22, 311, 23, 311, 24, 311, 25, 311, 26, 311, 27, 311, 28, 311, 29, 311, 30, 311, 31, 311, 32, 311, 33, 311, 35, 311, 36, 311, 37, 311, 38, 311, 39, 311, 40, 311, 41, 311, 42, 311, 43, 311, 44, 311, 45, 311, 46, 311, 47, 311, 48, 311, 49, 311, 50, 311, 51, 311, 52, 311, 53, 311, 55, 311, 56, 311, 57, 311, 58, 311, 59, 311, 60, 311, 61, 311, 62, 311, 63, 311, 64, 311, 65, 311, 66, 311, 67, 311, 68, 311, 69, 311],
	#[60, -379],
	#[65535, 70, 14, 70, 20, 70, 22, 70, 10, 70, 13, 70, 15, 70, 30, 70, 18, 70, 21, 70, 23, 70, 16, 70, 19, 70, 17, 70, 38, 70, 26, 70, 29, 70, 24, 70, 27, 70, 25, 70, 46, 70, 12, 70, 34, 70, 37, 70, 39, 70, 32, 70, 35, 70, 33, 70, 41, 70, 49, 70, 42, 70, 45, 70, 47, 70, 40, 70, 43, 70, 60, 70, 62, 70, 28, 70, 50, 70, 53, 70, 55, 70, 51, 70, 68, 70, 52, 70, 36, 70, 58, 70, 61, 70, 63, 70, 56, 70, 59, 70, 57, 70, 44, 70, 66, 70, 69, 70, 48, 70, 64, 70, 67, 70, 65, 70, 31, 70],
	#[65535, 24, 10, 24, 12, 24, 13, 24, 14, 24, 15, 24, 16, 24, 17, 24, 18, 24, 19, 24, 20, 24, 21, 24, 22, 24, 23, 24, 24, 24, 25, 24, 26, 24, 27, 24, 28, 24, 29, 24, 30, 24, 31, 24, 32, 24, 33, 24, 34, 24, 35, 24, 36, 24, 37, 24, 38, 24, 39, 24, 40, 24, 41, 24, 42, 24, 43, 24, 44, 24, 45, 24, 46, 24, 47, 24, 48, 24, 49, 24, 50, 24, 51, 24, 52, 24, 53, 24, 54, 24, 55, 24, 56, 24, 57, 24, 58, 24, 59, 24, 60, 24, 61, 24, 62, 24, 63, 24, 64, 24, 65, 24, 66, 24, 67, 24, 68, 24, 69, 24],
	#[15, -80, 17, -96, 10, -71, 12, -77, 13, -92, 14, -82, 38, -127, 16, -102, 18, -79, 19, -84, 20, -122, 21, -94, 22, -89, 23, -75, 24, -83, 25, -121, 26, -98, 27, -85, 28, -107, 29, -124, 30, -110, 31, -100, 32, -90, 33, -70, 63, -112, 35, -105, 36, -88, 37, -74, 39, -119, 40, -109, 41, -95, 42, -114, 43, -101, 44, -91, 45, -78, 46, -130, 47, -120, 48, -104, 49, 343, 50, -81, 51, -133, 52, -128, 53, -108, 67, -115, 55, -76, 56, 300, 57, -123, 65, -113, 59, -97, 61, -72, 62, -131, 64, -126, 66, -132, 68, -118, 69, -99],
	#[65535, 12, 2, 12, 10, 12, 11, 12, 12, 12, 13, 12, 14, 12, 15, 12, 16, 12, 17, 12, 18, 12, 19, 12, 20, 12, 21, 12, 22, 12, 23, 12, 24, 12, 25, 12, 26, 12, 27, 12, 28, 12, 29, 12, 30, 12, 31, 12, 32, 12, 33, 12, 34, 12, 35, 12, 36, 12, 37, 12, 38, 12, 39, 12, 40, 12, 41, 12, 42, 12, 43, 12, 44, 12, 45, 12, 46, 12, 47, 12, 48, 12, 49, 12, 50, 12, 51, 12, 52, 12, 53, 12, 54, 12, 55, 12, 56, 12, 57, 12, 58, 12, 59, 12, 60, 12, 61, 12, 62, 12, 63, 12, 64, 12, 65, 12, 66, 12, 67, 12, 68, 12, 69, 12],
	#[65535, 325, 15, 325, 10, 325, 29, 325, 23, 325, 18, 325, 22, 325, 31, 325, 19, 325, 30, 325, 39, 325, 25, 325, 12, 325, 13, 325, 14, 325, 26, 325, 16, 325, 17, 325, 52, 325, 36, 325, 20, 325, 21, 325, 35, 325, 46, 325, 24, 325, 33, 325, 27, 325, 28, 325, 42, 325, 32, 325, 37, 325, 38, 325, 50, 325, 40, 325, 41, 325, 61, 325, 43, 325, 44, 325, 45, 325, 59, 325, 47, 325, 48, 325, 49, 325, 51, 325, 53, 325, 55, 325, 56, 325, 57, 325, 58, 325, 60, 325, 62, 325, 63, 325, 64, 325, 65, 325, 66, 325, 67, 325, 68, 325, 69, 325],
	#[65535, 19, 12, 19, 14, 19, 20, 19, 22, 19, 13, 19, 15, 19, 30, 19, 18, 19, 21, 19, 23, 19, 10, 19, 19, 19, 17, 19, 38, 19, 16, 19, 29, 19, 31, 19, 24, 19, 27, 19, 25, 19, 46, 19, 41, 19, 34, 19, 37, 19, 39, 19, 26, 19, 35, 19, 33, 19, 54, 19, 49, 19, 32, 19, 45, 19, 47, 19, 40, 19, 43, 19, 60, 19, 62, 19, 28, 19, 50, 19, 53, 19, 55, 19, 42, 19, 51, 19, 68, 19, 52, 19, 36, 19, 58, 19, 61, 19, 63, 19, 56, 19, 59, 19, 57, 19, 44, 19, 66, 19, 69, 19, 48, 19, 64, 19, 67, 19, 65, 19],
	#[65535, 15, 2, 15, 10, 15, 11, 15, 12, 15, 13, 15, 14, 15, 15, 15, 16, 15, 17, 15, 18, 15, 19, 15, 20, 15, 21, 15, 22, 15, 23, 15, 24, 15, 25, 15, 26, 15, 27, 15, 28, 15, 29, 15, 30, 15, 31, 15, 32, 15, 33, 15, 34, 15, 35, 15, 36, 15, 37, 15, 38, 15, 39, 15, 40, 15, 41, 15, 42, 15, 43, 15, 44, 15, 45, 15, 46, 15, 47, 15, 48, 15, 49, 15, 50, 15, 51, 15, 52, 15, 53, 15, 54, 15, 55, 15, 56, 15, 57, 15, 58, 15, 59, 15, 60, 15, 61, 15, 62, 15, 63, 15, 64, 15, 65, 15, 66, 15, 67, 15, 68, 15, 69, 15],
	#[31, -100, 22, -89, 21, -94, 30, -110, 23, -75, 18, -79, 24, -83, 32, -90, 20, -122, 25, -121, 19, -84, 45, -297, 29, -124, 28, -107, 33, -70, 27, -85, 26, -98],
	#[65535, 14, 12, 14, 14, 14, 20, 14, 22, 14, 13, 14, 15, 14, 2, 14, 11, 14, 30, 14, 18, 14, 21, 14, 23, 14, 10, 14, 19, 14, 17, 14, 38, 14, 16, 14, 29, 14, 31, 14, 24, 14, 27, 14, 25, 14, 46, 14, 41, 14, 34, 14, 37, 14, 39, 14, 26, 14, 35, 14, 33, 14, 54, 14, 49, 14, 32, 14, 45, 14, 47, 14, 40, 14, 43, 14, 60, 14, 62, 14, 28, 14, 50, 14, 53, 14, 55, 14, 42, 14, 51, 14, 68, 14, 52, 14, 36, 14, 58, 14, 61, 14, 63, 14, 56, 14, 59, 14, 57, 14, 44, 14, 66, 14, 69, 14, 48, 14, 64, 14, 67, 14, 65, 14],
	#[65535, 25, 10, 25, 12, 25, 13, 25, 14, 25, 15, 25, 16, 25, 17, 25, 18, 25, 19, 25, 20, 25, 21, 25, 22, 25, 23, 25, 24, 25, 25, 25, 26, 25, 27, 25, 28, 25, 29, 25, 30, 25, 31, 25, 32, 25, 33, 25, 34, 25, 35, 25, 36, 25, 37, 25, 38, 25, 39, 25, 40, 25, 41, 25, 42, 25, 43, 25, 44, 25, 45, 25, 46, 25, 47, 25, 48, 25, 49, 25, 50, 25, 51, 25, 52, 25, 53, 25, 54, 25, 55, 25, 56, 25, 57, 25, 58, 25, 59, 25, 60, 25, 61, 25, 62, 25, 63, 25, 64, 25, 65, 25, 66, 25, 67, 25, 68, 25, 69, 25],
	#[65535, 20, 64, 20, 58, 20, 49, 20, 65, 20, 47, 20, 55, 20, 30, 20, 18, 20, 14, 20, 38, 20, 12, 20, 15, 20, 22, 20, 45, 20, 10, 20, 20, 20, 23, 20, 13, 20, 26, 20, 43, 20, 17, 20, 19, 20, 31, 20, 21, 20, 34, 20, 61, 20, 27, 20, 52, 20, 36, 20, 32, 20, 29, 20, 46, 20, 59, 20, 16, 20, 35, 20, 60, 20, 44, 20, 28, 20, 37, 20, 54, 20, 33, 20, 24, 20, 42, 20, 68, 20, 48, 20, 25, 20, 62, 20, 41, 20, 50, 20, 69, 20, 63, 20, 53, 20, 66, 20, 40, 20, 39, 20, 51, 20, 57, 20, 67, 20, 56, 20],
	#[65535, 28, 13, 28, 21, 28, 30, 28, 12, 28, 29, 28, 27, 28, 15, 28, 38, 28, 20, 28, 19, 28, 37, 28, 17, 28, 23, 28, 46, 28, 28, 28, 55, 28, 10, 28, 45, 28, 25, 28, 31, 28, 26, 28, 36, 28, 35, 28, 18, 28, 53, 28, 16, 28, 14, 28, 39, 28, 34, 28, 44, 28, 43, 28, 32, 28, 61, 28, 24, 28, 22, 28, 47, 28, 42, 28, 52, 28, 51, 28, 40, 28, 41, 28, 48, 28, 65, 28, 50, 28, 60, 28, 59, 28, 49, 28, 63, 28, 58, 28, 68, 28, 64, 28, 69, 28, 33, 28, 54, 28, 67, 28, 62, 28, 56, 28, 57, 28, 66, 28],
	#[65535, 40, 10, 40, 12, 40, 13, 40, 14, 40, 15, 40, 16, 40, 17, 40, 18, 40, 19, 40, 20, 40, 21, 40, 22, 40, 23, 40, 24, 40, 25, 40, 26, 40, 27, 40, 28, 40, 29, 40, 30, 40, 31, 40, 32, 40, 33, 40, 34, 40, 35, 40, 36, 40, 37, 40, 38, 40, 39, 40, 40, 40, 41, 40, 42, 40, 43, 40, 44, 40, 45, 40, 46, 40, 47, 40, 48, 40, 49, 40, 50, 40, 51, 40, 52, 40, 53, 40, 54, 40, 55, 40, 56, 40, 57, 40, 58, 40, 59, 40, 60, 40, 61, 40, 62, 40, 63, 40, 64, 40, 65, 40, 66, 40, 67, 40, 68, 40, 69, 40],
	#[65535, 18, 47, 18, 20, 18, 55, 18, 31, 18, 14, 18, 38, 18, 45, 18, 39, 18, 22, 18, 46, 18, 59, 18, 15, 18, 26, 18, 30, 18, 54, 18, 21, 18, 23, 18, 10, 18, 19, 18, 36, 18, 25, 18, 33, 18, 16, 18, 29, 18, 50, 18, 18, 18, 13, 18, 44, 18, 28, 18, 12, 18, 34, 18, 37, 18, 58, 18, 17, 18, 35, 18, 52, 18, 61, 18, 51, 18, 27, 18, 24, 18, 40, 18, 43, 18, 60, 18, 62, 18, 57, 18, 53, 18, 32, 18, 42, 18, 41, 18, 68, 18, 65, 18, 63, 18, 56, 18, 49, 18, 66, 18, 69, 18, 48, 18, 64, 18, 67, 18],
	#[65535, 71, 20, 71, 12, 71, 15, 71, 34, 71, 18, 71, 31, 71, 14, 71, 38, 71, 17, 71, 26, 71, 39, 71, 22, 71, 46, 71, 10, 71, 13, 71, 35, 71, 28, 71, 30, 71, 27, 71, 33, 71, 21, 71, 23, 71, 16, 71, 19, 71, 36, 71, 45, 71, 62, 71, 41, 71, 29, 71, 50, 71, 24, 71, 60, 71, 25, 71, 53, 71, 43, 71, 49, 71, 37, 71, 58, 71, 32, 71, 68, 71, 52, 71, 61, 71, 51, 71, 42, 71, 65, 71, 47, 71, 40, 71, 57, 71, 56, 71, 55, 71, 64, 71, 63, 71, 59, 71, 44, 71, 66, 71, 69, 71, 48, 71, 67, 71],
	#[65535, 23, 10, 23, 12, 23, 13, 23, 14, 23, 15, 23, 16, 23, 17, 23, 18, 23, 19, 23, 20, 23, 21, 23, 22, 23, 23, 23, 24, 23, 25, 23, 26, 23, 27, 23, 28, 23, 29, 23, 30, 23, 31, 23, 32, 23, 33, 23, 34, 23, 35, 23, 36, 23, 37, 23, 38, 23, 39, 23, 40, 23, 41, 23, 42, 23, 43, 23, 44, 23, 45, 23, 46, 23, 47, 23, 48, 23, 49, 23, 50, 23, 51, 23, 52, 23, 53, 23, 54, 23, 55, 23, 56, 23, 57, 23, 58, 23, 59, 23, 60, 23, 61, 23, 62, 23, 63, 23, 64, 23, 65, 23, 66, 23, 67, 23, 68, 23, 69, 23],
	#[65535, 33, 31, 33, 47, 33, 30, 33, 63, 33, 18, 33, 14, 33, 34, 33, 12, 33, 15, 33, 22, 33, 45, 33, 20, 33, 23, 33, 13, 33, 26, 33, 55, 33, 17, 33, 19, 33, 44, 33, 10, 33, 21, 33, 38, 33, 61, 33, 27, 33, 52, 33, 36, 33, 39, 33, 29, 33, 46, 33, 25, 33, 16, 33, 35, 33, 60, 33, 58, 33, 28, 33, 37, 33, 54, 33, 33, 33, 43, 33, 68, 33, 65, 33, 59, 33, 62, 33, 41, 33, 50, 33, 69, 33, 42, 33, 53, 33, 48, 33, 24, 33, 40, 33, 51, 33, 57, 33, 66, 33, 56, 33, 32, 33, 64, 33, 67, 33, 49, 33],
	#[65535, 324, 10, 324, 12, 324, 13, 324, 14, 324, 15, 324, 16, 324, 17, 324, 18, 324, 19, 324, 20, 324, 21, 324, 22, 324, 23, 324, 24, 324, 25, 324, 26, 324, 27, 324, 28, 324, 29, 324, 30, 324, 31, 324, 32, 324, 33, 324, 35, 324, 36, 324, 37, 324, 38, 324, 39, 324, 40, 324, 41, 324, 42, 324, 43, 324, 44, 324, 45, 324, 46, 324, 47, 324, 48, 324, 49, 324, 50, 324, 51, 324, 52, 324, 53, 324, 55, 324, 56, 324, 57, 324, 58, 324, 59, 324, 60, 324, 61, 324, 62, 324, 63, 324, 64, 324, 65, 324, 66, 324, 67, 324, 68, 324, 69, 324],
	#[65535, 13, 15, 13, 10, 13, 2, 13, 23, 13, 12, 13, 18, 13, 11, 13, 22, 13, 31, 13, 20, 13, 26, 13, 19, 13, 30, 13, 39, 13, 25, 13, 34, 13, 13, 13, 14, 13, 38, 13, 16, 13, 17, 13, 52, 13, 33, 13, 42, 13, 21, 13, 35, 13, 46, 13, 24, 13, 45, 13, 27, 13, 28, 13, 29, 13, 54, 13, 32, 13, 36, 13, 37, 13, 51, 13, 62, 13, 40, 13, 41, 13, 61, 13, 43, 13, 44, 13, 59, 13, 47, 13, 48, 13, 49, 13, 50, 13, 68, 13, 53, 13, 55, 13, 56, 13, 57, 13, 58, 13, 60, 13, 63, 13, 64, 13, 65, 13, 66, 13, 67, 13, 69, 13],
	#[65535, 316, 23, 316, 21, 316, 30, 316, 12, 316, 39, 316, 29, 316, 27, 316, 15, 316, 38, 316, 62, 316, 19, 316, 37, 316, 26, 316, 22, 316, 46, 316, 28, 316, 55, 316, 10, 316, 45, 316, 33, 316, 31, 316, 16, 316, 35, 316, 53, 316, 51, 316, 14, 316, 24, 316, 43, 316, 25, 316, 61, 316, 59, 316, 18, 316, 47, 316, 42, 316, 52, 316, 36, 316, 20, 316, 41, 316, 13, 316, 65, 316, 17, 316, 60, 316, 44, 316, 49, 316, 40, 316, 63, 316, 48, 316, 68, 316, 57, 316, 64, 316, 69, 316, 56, 316, 32, 316, 67, 316, 58, 316, 50, 316, 66, 316],
	#[65535, 22, 10, 22, 12, 22, 13, 22, 14, 22, 15, 22, 16, 22, 17, 22, 18, 22, 19, 22, 20, 22, 21, 22, 22, 22, 23, 22, 24, 22, 25, 22, 26, 22, 27, 22, 28, 22, 29, 22, 30, 22, 31, 22, 32, 22, 33, 22, 34, 22, 35, 22, 36, 22, 37, 22, 38, 22, 39, 22, 40, 22, 41, 22, 42, 22, 43, 22, 44, 22, 45, 22, 46, 22, 47, 22, 48, 22, 49, 22, 50, 22, 51, 22, 52, 22, 53, 22, 54, 22, 55, 22, 56, 22, 57, 22, 58, 22, 59, 22, 60, 22, 61, 22, 62, 22, 63, 22, 64, 22, 65, 22, 66, 22, 67, 22, 68, 22, 69, 22],
	#[65535, 310, 20, 310, 22, 310, 13, 310, 15, 310, 30, 310, 21, 310, 23, 310, 10, 310, 19, 310, 14, 310, 38, 310, 16, 310, 29, 310, 31, 310, 18, 310, 17, 310, 25, 310, 46, 310, 12, 310, 24, 310, 37, 310, 39, 310, 26, 310, 35, 310, 33, 310, 41, 310, 49, 310, 32, 310, 45, 310, 47, 310, 40, 310, 43, 310, 60, 310, 62, 310, 28, 310, 50, 310, 53, 310, 55, 310, 42, 310, 51, 310, 68, 310, 27, 310, 36, 310, 58, 310, 61, 310, 63, 310, 56, 310, 59, 310, 57, 310, 44, 310, 66, 310, 69, 310, 48, 310, 64, 310, 67, 310, 65, 310, 52, 310],
	#[65535, 17, 20, 17, 12, 17, 15, 17, 11, 17, 34, 17, 18, 17, 28, 17, 31, 17, 14, 17, 38, 17, 17, 17, 39, 17, 22, 17, 46, 17, 25, 17, 13, 17, 35, 17, 2, 17, 44, 17, 47, 17, 30, 17, 54, 17, 33, 17, 21, 17, 23, 17, 10, 17, 19, 17, 36, 17, 45, 17, 62, 17, 16, 17, 29, 17, 50, 17, 24, 17, 27, 17, 63, 17, 53, 17, 41, 17, 49, 17, 37, 17, 58, 17, 26, 17, 68, 17, 52, 17, 61, 17, 51, 17, 32, 17, 65, 17, 66, 17, 40, 17, 43, 17, 60, 17, 57, 17, 56, 17, 55, 17, 42, 17, 64, 17, 59, 17, 69, 17, 48, 17, 67, 17],
	#[10, -71, 12, -77, 13, -92, 14, -82, 15, -80, 16, -102, 17, -96, 18, -79, 19, -84, 20, -122, 21, -94, 22, -89, 23, -75, 24, -83, 25, -121, 26, -98, 27, -85, 28, -107, 29, -124, 30, -110, 31, -100, 32, -90, 33, -70, 35, -105, 36, -88, 37, -74, 38, -127, 39, -119, 40, -109, 41, -95, 42, -114, 43, -101, 44, -91, 45, -78, 46, -130, 47, -120, 48, -104, 49, 343, 50, -81, 51, -133, 52, -128, 53, -108, 55, -76, 57, -123, 59, -97, 60, 300, 61, -72, 62, -131, 63, -112, 64, -126, 65, -113, 66, -132, 67, -115, 68, -118, 69, -99],
	#[65535, 27, 12, 27, 14, 27, 20, 27, 22, 27, 13, 27, 15, 27, 30, 27, 18, 27, 21, 27, 23, 27, 10, 27, 19, 27, 17, 27, 38, 27, 16, 27, 29, 27, 31, 27, 24, 27, 27, 27, 25, 27, 46, 27, 41, 27, 34, 27, 37, 27, 39, 27, 26, 27, 35, 27, 33, 27, 54, 27, 49, 27, 32, 27, 45, 27, 47, 27, 40, 27, 43, 27, 60, 27, 62, 27, 28, 27, 50, 27, 53, 27, 55, 27, 42, 27, 51, 27, 68, 27, 52, 27, 36, 27, 58, 27, 61, 27, 63, 27, 56, 27, 59, 27, 57, 27, 44, 27, 66, 27, 69, 27, 48, 27, 64, 27, 67, 27, 65, 27],
	#[13, -92, 36, -88, 21, -94, 30, -110, 12, -77, 29, -124, 15, -80, 38, -127, 19, -84, 37, -74, 17, -96, 23, -75, 46, -130, 28, -107, 27, -85, 10, -71, 45, -78, 25, -121, 31, -100, 20, -122, 18, -79, 53, -108, 16, -102, 14, -82, 39, -119, 43, -101, 26, -98, 61, -72, 24, -83, 22, -89, 47, -120, 42, -114, 52, -128, 51, -133, 33, -70, 41, -95, 32, -90, 55, -76, 40, -109, 60, 300, 44, -91, 35, -105, 63, -112, 48, -104, 68, -118, 49, 343, 65, -113, 64, -126, 59, -97, 50, -81, 67, -115, 62, -131, 57, -123, 69, -99, 66, -132],
	#[65535, 32, 10, 32, 12, 32, 13, 32, 14, 32, 15, 32, 16, 32, 17, 32, 18, 32, 19, 32, 20, 32, 21, 32, 22, 32, 23, 32, 24, 32, 25, 32, 26, 32, 27, 32, 28, 32, 29, 32, 30, 32, 31, 32, 32, 32, 33, 32, 34, 32, 35, 32, 36, 32, 37, 32, 38, 32, 39, 32, 40, 32, 41, 32, 42, 32, 43, 32, 44, 32, 45, 32, 46, 32, 47, 32, 48, 32, 49, 32, 50, 32, 51, 32, 52, 32, 53, 32, 54, 32, 55, 32, 56, 32, 57, 32, 58, 32, 59, 32, 60, 32, 61, 32, 62, 32, 63, 32, 64, 32, 65, 32, 66, 32, 67, 32, 68, 32, 69, 32],
	#[65535, 327, 10, 327, 12, 327, 13, 327, 14, 327, 15, 327, 16, 327, 17, 327, 18, 327, 19, 327, 20, 327, 21, 327, 22, 327, 23, 327, 24, 327, 25, 327, 26, 327, 27, 327, 28, 327, 29, 327, 30, 327, 31, 327, 32, 327, 33, 327, 63, 327, 35, 327, 36, 327, 37, 327, 38, 327, 39, 327, 40, 327, 41, 327, 42, 327, 43, 327, 44, 327, 45, 327, 46, 327, 47, 327, 48, 327, 49, 327, 50, 327, 51, 327, 52, 327, 53, 327, 55, 327, 56, 327, 57, 327, 58, 327, 59, 327, 60, 327, 61, 327, 62, 327, 64, 327, 65, 327, 66, 327, 67, 327, 68, 327, 69, 327],
	#[65535, 16, 15, 16, 14, 16, 2, 16, 20, 16, 29, 16, 26, 16, 23, 16, 17, 16, 11, 16, 10, 16, 31, 16, 30, 16, 39, 16, 28, 16, 12, 16, 13, 16, 27, 16, 38, 16, 16, 16, 43, 16, 18, 16, 19, 16, 42, 16, 21, 16, 22, 16, 34, 16, 24, 16, 25, 16, 33, 16, 32, 16, 50, 16, 41, 16, 35, 16, 36, 16, 37, 16, 51, 16, 40, 16, 49, 16, 48, 16, 44, 16, 45, 16, 46, 16, 47, 16, 52, 16, 53, 16, 54, 16, 55, 16, 56, 16, 57, 16, 58, 16, 59, 16, 60, 16, 61, 16, 62, 16, 63, 16, 64, 16, 65, 16, 66, 16, 67, 16, 68, 16, 69, 16],
	#[65535, 344, 49, 344],
	#[65535, 319, 15, 319, 20, 319, 14, 319, 26, 319, 10, 319, 31, 319, 30, 319, 29, 319, 12, 319, 13, 319, 27, 319, 38, 319, 16, 319, 17, 319, 18, 319, 19, 319, 42, 319, 21, 319, 22, 319, 23, 319, 24, 319, 25, 319, 33, 319, 32, 319, 28, 319, 41, 319, 35, 319, 36, 319, 37, 319, 51, 319, 39, 319, 40, 319, 49, 319, 43, 319, 44, 319, 45, 319, 46, 319, 47, 319, 48, 319, 50, 319, 52, 319, 53, 319, 55, 319, 56, 319, 57, 319, 58, 319, 59, 319, 60, 319, 61, 319, 62, 319, 63, 319, 64, 319, 65, 319, 66, 319, 67, 319, 68, 319, 69, 319],
	#[65535, 314, 10, 314, 12, 314, 13, 314, 14, 314, 15, 314, 16, 314, 17, 314, 18, 314, 19, 314, 20, 314, 21, 314, 22, 314, 23, 314, 24, 314, 25, 314, 26, 314, 27, 314, 28, 314, 29, 314, 30, 314, 31, 314, 32, 314, 33, 314, 35, 314, 36, 314, 37, 314, 38, 314, 39, 314, 40, 314, 41, 314, 42, 314, 43, 314, 44, 314, 45, 314, 46, 314, 47, 314, 48, 314, 49, 314, 50, 314, 51, 314, 52, 314, 53, 314, 55, 314, 56, 314, 57, 314, 58, 314, 59, 314, 60, 314, 61, 314, 62, 314, 63, 314, 64, 314, 65, 314, 66, 314, 67, 314, 68, 314, 69, 314],
	#[65535, 335, 10, 335, 12, 335, 13, 335, 14, 335, 15, 335, 16, 335, 17, 335, 18, 335, 19, 335, 20, 335, 21, 335, 22, 335, 23, 335, 24, 335, 25, 335, 26, 335, 27, 335, 28, 335, 29, 335, 30, 335, 31, 335, 32, 335, 33, 335, 35, 335, 36, 335, 37, 335, 38, 335, 39, 335, 40, 335, 41, 335, 42, 335, 43, 335, 44, 335, 45, 335, 46, 335, 47, 335, 48, 335, 49, 335, 50, 335, 51, 335, 52, 335, 53, 335, 55, 335, 56, 335, 57, 335, 58, 335, 59, 335, 60, 335, 61, 335, 62, 335, 63, 335, 64, 335, 65, 335, 66, 335, 67, 335, 68, 335, 69, 335],
	#[65535, 29, 13, 29, 15, 29, 14, 29, 23, 29, 27, 29, 10, 29, 12, 29, 26, 29, 16, 29, 17, 29, 18, 29, 19, 29, 20, 29, 21, 29, 22, 29, 46, 29, 24, 29, 25, 29, 28, 29, 29, 29, 30, 29, 31, 29, 32, 29, 33, 29, 34, 29, 35, 29, 36, 29, 37, 29, 38, 29, 39, 29, 40, 29, 41, 29, 42, 29, 43, 29, 44, 29, 45, 29, 47, 29, 48, 29, 49, 29, 50, 29, 51, 29, 52, 29, 53, 29, 54, 29, 55, 29, 56, 29, 57, 29, 58, 29, 59, 29, 60, 29, 61, 29, 62, 29, 63, 29, 64, 29, 65, 29, 66, 29, 67, 29, 68, 29, 69, 29],
	#[65535, 339, 10, 339, 12, 339, 13, 339, 14, 339, 15, 339, 16, 339, 17, 339, 18, 339, 19, 339, 20, 339, 21, 339, 22, 339, 23, 339, 24, 339, 25, 339, 26, 339, 27, 339, 28, 339, 29, 339, 30, 339, 31, 339, 32, 339, 33, 339, 35, 339, 36, 339, 37, 339, 38, 339, 39, 339, 40, 339, 41, 339, 42, 339, 43, 339, 44, 339, 45, 339, 46, 339, 47, 339, 48, 339, 49, 339, 50, 339, 51, 339, 52, 339, 53, 339, 55, 339, 56, 339, 57, 339, 58, 339, 59, 339, 60, 339, 61, 339, 62, 339, 63, 339, 64, 339, 65, 339, 66, 339, 67, 339, 68, 339, 69, 339],
	#[65535, 334, 10, 334, 12, 334, 13, 334, 14, 334, 15, 334, 16, 334, 17, 334, 18, 334, 19, 334, 20, 334, 21, 334, 22, 334, 23, 334, 24, 334, 25, 334, 26, 334, 27, 334, 28, 334, 29, 334, 30, 334, 31, 334, 32, 334, 33, 334, 35, 334, 36, 334, 37, 334, 38, 334, 39, 334, 40, 334, 41, 334, 42, 334, 43, 334, 44, 334, 45, 334, 46, 334, 47, 334, 48, 334, 49, 334, 50, 334, 51, 334, 52, 334, 53, 334, 55, 334, 56, 334, 57, 334, 58, 334, 59, 334, 60, 334, 61, 334, 62, 334, 63, 334, 64, 334, 65, 334, 66, 334, 67, 334, 68, 334, 69, 334],
	#[65535, 31, 51, 31, 12, 31, 20, 31, 14, 31, 10, 31, 31, 31, 30, 31, 42, 31, 29, 31, 33, 31, 13, 31, 27, 31, 15, 31, 16, 31, 17, 31, 18, 31, 19, 31, 41, 31, 21, 31, 22, 31, 23, 31, 24, 31, 25, 31, 26, 31, 32, 31, 28, 31, 34, 31, 35, 31, 36, 31, 37, 31, 38, 31, 39, 31, 40, 31, 49, 31, 43, 31, 44, 31, 45, 31, 46, 31, 47, 31, 48, 31, 50, 31, 52, 31, 53, 31, 54, 31, 55, 31, 56, 31, 57, 31, 58, 31, 59, 31, 60, 31, 61, 31, 62, 31, 63, 31, 64, 31, 65, 31, 66, 31, 67, 31, 68, 31, 69, 31],
	#[65535, 323, 10, 323, 12, 323, 13, 323, 14, 323, 15, 323, 16, 323, 17, 323, 18, 323, 19, 323, 20, 323, 21, 323, 22, 323, 23, 323, 24, 323, 25, 323, 26, 323, 27, 323, 28, 323, 29, 323, 30, 323, 31, 323, 32, 323, 33, 323, 35, 323, 36, 323, 37, 323, 38, 323, 39, 323, 40, 323, 41, 323, 42, 323, 43, 323, 44, 323, 45, 323, 46, 323, 47, 323, 48, 323, 49, 323, 50, 323, 51, 323, 52, 323, 53, 323, 55, 323, 56, 323, 57, 323, 58, 323, 59, 323, 60, 323, 61, 323, 62, 323, 63, 323, 64, 323, 65, 323, 66, 323, 67, 323, 68, 323, 69, 323],
	#[10, 313, 12, 313, 13, 313, 14, 313, 15, 313, 16, 313, 17, 313, 18, 313, 19, 313, 20, 313, 21, 313, 22, 313, 23, 313, 24, 313, 25, 313, 26, 313, 27, 313, 28, 313, 29, 313, 30, 313, 31, 313, 32, 313, 33, 313, 35, 313, 36, 313, 37, 313, 38, 313, 39, 313, 40, 313, 41, 313, 42, 313, 43, 313, 44, 313, 45, 313, 46, 313, 47, 313, 48, 313, 49, 313, 50, 313, 51, 313, 52, 313, 53, 313, 54, -293, 55, 313, 56, 313, 57, 313, 58, 313, 59, 313, 60, 313, 61, 313, 62, 313, 63, 313, 64, 313, 65, 313, 66, 313, 67, 313, 68, 313, 69, 313],
	#[55, -76, 37, -74, 31, -100, 38, -127, 45, -78, 59, -97, 44, -91, 28, -107, 30, -110, 50, -81, 23, -75, 10, -71, 52, -128, 36, -88, 20, -122, 56, 300, 29, -124, 57, -123, 22, -89, 32, -90, 43, -101, 19, -84, 39, -119, 26, -98, 35, -105, 33, -70, 18, -79, 15, -80, 42, -114, 27, -85, 47, -120, 40, -109, 14, -82, 41, -95, 62, -131, 13, -92, 12, -77, 17, -96, 25, -121, 48, -104, 24, -83, 46, -130, 16, -102, 21, -94, 61, -72, 49, 343, 51, -133, 53, -108, 63, -112, 64, -126, 65, -113, 66, -132, 67, -115, 68, -118, 69, -99],
	#[65535, 326, 10, 326, 12, 326, 13, 326, 14, 326, 15, 326, 16, 326, 17, 326, 18, 326, 19, 326, 20, 326, 21, 326, 22, 326, 23, 326, 24, 326, 25, 326, 26, 326, 27, 326, 28, 326, 29, 326, 30, 326, 31, 326, 32, 326, 33, 326, 35, 326, 36, 326, 37, 326, 38, 326, 39, 326, 40, 326, 41, 326, 42, 326, 43, 326, 44, 326, 45, 326, 46, 326, 47, 326, 48, 326, 49, 326, 50, 326, 51, 326, 52, 326, 53, 326, 55, 326, 56, 326, 57, 326, 58, 326, 59, 326, 60, 326, 61, 326, 62, 326, 63, 326, 64, 326, 65, 326, 66, 326, 67, 326, 68, 326, 69, 326],
	#[65535, 329, 20, 329, 37, 329, 31, 329, 51, 329, 36, 329, 39, 329, 22, 329, 46, 329, 10, 329, 13, 329, 15, 329, 28, 329, 30, 329, 25, 329, 21, 329, 23, 329, 61, 329, 14, 329, 38, 329, 33, 329, 26, 329, 29, 329, 50, 329, 27, 329, 44, 329, 53, 329, 12, 329, 24, 329, 19, 329, 58, 329, 17, 329, 35, 329, 52, 329, 18, 329, 16, 329, 42, 329, 45, 329, 47, 329, 40, 329, 43, 329, 60, 329, 62, 329, 59, 329, 56, 329, 55, 329, 48, 329, 41, 329, 49, 329, 57, 329, 32, 329, 63, 329, 64, 329, 65, 329, 66, 329, 67, 329, 68, 329, 69, 329],
	#[65535, 315, 10, 315, 12, 315, 13, 315, 14, 315, 15, 315, 16, 315, 17, 315, 18, 315, 19, 315, 20, 315, 21, 315, 22, 315, 23, 315, 24, 315, 25, 315, 26, 315, 27, 315, 28, 315, 29, 315, 30, 315, 31, 315, 32, 315, 33, 315, 35, 315, 36, 315, 37, 315, 38, 315, 39, 315, 40, 315, 41, 315, 42, 315, 43, 315, 44, 315, 45, 315, 46, 315, 47, 315, 48, 315, 49, 315, 50, 315, 51, 315, 52, 315, 53, 315, 55, 315, 56, 315, 57, 315, 58, 315, 59, 315, 60, 315, 61, 315, 62, 315, 63, 315, 64, 315, 65, 315, 66, 315, 67, 315, 68, 315, 69, 315],
	#[49, -145],
	#[65535, 330, 55, 330, 47, 330, 43, 330, 37, 330, 31, 330, 51, 330, 45, 330, 39, 330, 22, 330, 46, 330, 13, 330, 15, 330, 44, 330, 28, 330, 30, 330, 21, 330, 23, 330, 10, 330, 14, 330, 38, 330, 16, 330, 29, 330, 50, 330, 18, 330, 17, 330, 25, 330, 33, 330, 12, 330, 19, 330, 58, 330, 26, 330, 35, 330, 52, 330, 61, 330, 20, 330, 42, 330, 27, 330, 24, 330, 40, 330, 56, 330, 60, 330, 62, 330, 59, 330, 53, 330, 32, 330, 48, 330, 41, 330, 49, 330, 57, 330, 36, 330, 63, 330, 64, 330, 65, 330, 66, 330, 67, 330, 68, 330, 69, 330],
	#[65535, 73, 10, 73, 12, 73, 13, 73, 14, 73, 15, 73, 16, 73, 17, 73, 18, 73, 19, 73, 20, 73, 21, 73, 22, 73, 23, 73, 24, 73, 25, 73, 26, 73, 27, 73, 28, 73, 29, 73, 30, 73, 31, 73, 32, 73, 33, 73, 34, 73, 35, 73, 36, 73, 37, 73, 38, 73, 39, 73, 40, 73, 41, 73, 42, 73, 43, 73, 44, 73, 45, 73, 46, 73, 47, 73, 48, 73, 49, 73, 50, 73, 51, 73, 52, 73, 53, 73, 55, 73, 56, 73, 57, 73, 58, 73, 59, 73, 60, 73, 61, 73, 62, 73, 63, 73, 64, 73, 65, 73, 66, 73, 67, 73, 68, 73, 69, 73],
	#[65535, 318, 10, 318, 12, 318, 13, 318, 14, 318, 15, 318, 16, 318, 17, 318, 18, 318, 19, 318, 20, 318, 21, 318, 22, 318, 23, 318, 24, 318, 25, 318, 26, 318, 27, 318, 28, 318, 29, 318, 30, 318, 31, 318, 32, 318, 33, 318, 35, 318, 36, 318, 37, 318, 38, 318, 39, 318, 40, 318, 41, 318, 42, 318, 43, 318, 44, 318, 45, 318, 46, 318, 47, 318, 48, 318, 49, 318, 50, 318, 51, 318, 52, 318, 53, 318, 55, 318, 56, 318, 57, 318, 58, 318, 59, 318, 60, 318, 61, 318, 62, 318, 63, 318, 64, 318, 65, 318, 66, 318, 67, 318, 68, 318, 69, 318],
	#[65535, 26, 51, 26, 31, 26, 43, 26, 15, 26, 10, 26, 47, 26, 14, 26, 38, 26, 19, 26, 13, 26, 12, 26, 16, 26, 55, 26, 33, 26, 22, 26, 46, 26, 27, 26, 21, 26, 17, 26, 26, 26, 18, 26, 54, 26, 35, 26, 39, 26, 36, 26, 20, 26, 23, 26, 24, 26, 25, 26, 28, 26, 29, 26, 30, 26, 42, 26, 32, 26, 59, 26, 34, 26, 37, 26, 48, 26, 62, 26, 40, 26, 41, 26, 61, 26, 60, 26, 44, 26, 45, 26, 58, 26, 49, 26, 50, 26, 56, 26, 52, 26, 53, 26, 57, 26, 63, 26, 64, 26, 65, 26, 66, 26, 67, 26, 68, 26, 69, 26],
	#[65535, 21, 10, 21, 12, 21, 13, 21, 14, 21, 15, 21, 16, 21, 17, 21, 18, 21, 19, 21, 20, 21, 21, 21, 22, 21, 23, 21, 24, 21, 25, 21, 26, 21, 27, 21, 28, 21, 29, 21, 30, 21, 31, 21, 32, 21, 33, 21, 34, 21, 35, 21, 36, 21, 37, 21, 38, 21, 39, 21, 40, 21, 41, 21, 42, 21, 43, 21, 44, 21, 45, 21, 46, 21, 47, 21, 48, 21, 49, 21, 50, 21, 51, 21, 52, 21, 53, 21, 54, 21, 55, 21, 56, 21, 57, 21, 58, 21, 59, 21, 60, 21, 61, 21, 62, 21, 63, 21, 64, 21, 65, 21, 66, 21, 67, 21, 68, 21, 69, 21],
	#[10, -71, 12, -77, 13, -92, 14, -82, 15, -80, 16, -102, 17, -96, 18, -79, 19, -84, 20, -122, 21, -94, 22, -89, 23, -75, 24, -83, 25, -121, 26, -98, 27, -85, 28, -107, 29, -124, 30, -110, 31, -100, 32, -90, 33, -70, 35, -105, 36, -88, 37, -74, 38, -127, 39, -119, 40, -109, 41, -95, 42, -114, 43, -101, 44, -91, 45, -78, 46, -130, 47, -120, 48, -104, 49, 343, 50, -81, 51, -133, 52, -128, 53, -108, 55, -76, 57, -123, 58, 300, 59, -97, 61, -72, 62, -131, 63, -112, 64, -126, 65, -113, 66, -132, 67, -115, 68, -118, 69, -99],
	#[65535, 30, 15, 30, 14, 30, 20, 30, 29, 30, 26, 30, 23, 30, 17, 30, 10, 30, 31, 30, 30, 30, 39, 30, 28, 30, 12, 30, 13, 30, 27, 30, 38, 30, 16, 30, 43, 30, 18, 30, 19, 30, 42, 30, 21, 30, 22, 30, 34, 30, 24, 30, 25, 30, 33, 30, 32, 30, 50, 30, 41, 30, 35, 30, 36, 30, 37, 30, 51, 30, 40, 30, 49, 30, 48, 30, 44, 30, 45, 30, 46, 30, 47, 30, 52, 30, 53, 30, 54, 30, 55, 30, 56, 30, 57, 30, 58, 30, 59, 30, 60, 30, 61, 30, 62, 30, 63, 30, 64, 30, 65, 30, 66, 30, 67, 30, 68, 30, 69, 30],
	#[10, -71, 12, -77, 13, -92, 14, -82, 15, -80, 16, -102, 17, -96, 18, -79, 19, -84, 20, -122, 21, -94, 22, -89, 23, -75, 24, -83, 25, -121, 26, -98, 27, -85, 28, -107, 29, -124, 30, -110, 31, -100, 32, -90, 33, -70, 35, -105, 36, -88, 37, -74, 38, -127, 39, -119, 40, -109, 41, -95, 42, -114, 43, -101, 44, -91, 45, -78, 46, -130, 47, -120, 48, -104, 49, 343, 50, -81, 51, -133, 52, -128, 53, -108, 55, -76, 56, 302, 57, -123, 58, 302, 59, -97, 60, 302, 61, -72, 62, -131, 63, -112, 64, -126, 65, -113, 66, -132, 67, -115, 68, -118, 69, -99],
	#[65535, 328, 10, 328, 12, 328, 13, 328, 14, 328, 15, 328, 16, 328, 17, 328, 18, 328, 19, 328, 20, 328, 21, 328, 22, 328, 23, 328, 24, 328, 25, 328, 26, 328, 27, 328, 28, 328, 29, 328, 30, 328, 31, 328, 32, 328, 33, 328, 35, 328, 36, 328, 37, 328, 38, 328, 39, 328, 40, 328, 41, 328, 42, 328, 43, 328, 44, 328, 45, 328, 46, 328, 47, 328, 48, 328, 49, 328, 50, 328, 51, 328, 52, 328, 53, 328, 55, 328, 56, 328, 57, 328, 58, 328, 59, 328, 60, 328, 61, 328, 62, 328, 63, 328, 64, 328, 65, 328, 66, 328, 67, 328, 68, 328, 69, 328],
	#[65535, 72, 23, 72, 10, 72, 12, 72, 13, 72, 14, 72, 15, 72, 16, 72, 17, 72, 18, 72, 19, 72, 20, 72, 21, 72, 22, 72, 34, 72, 24, 72, 25, 72, 26, 72, 27, 72, 28, 72, 29, 72, 30, 72, 31, 72, 32, 72, 33, 72, 35, 72, 36, 72, 37, 72, 38, 72, 39, 72, 40, 72, 41, 72, 42, 72, 43, 72, 44, 72, 45, 72, 46, 72, 47, 72, 48, 72, 49, 72, 50, 72, 51, 72, 52, 72, 53, 72, 55, 72, 56, 72, 57, 72, 58, 72, 59, 72, 60, 72, 61, 72, 62, 72, 63, 72, 64, 72, 65, 72, 66, 72, 67, 72, 68, 72, 69, 72],
	#[18, -79, 19, -84, 20, -122, 21, -94, 22, -89, 23, -75, 24, -83, 25, -121, 26, -98, 27, -85, 28, -107, 29, -124, 30, -110, 31, -100, 32, -90, 33, -70, 59, -141],
	#[65535, 309, 47, 309, 43, 309, 55, 309, 51, 309, 46, 309, 61, 309, 35, 309, 10, 309, 12, 309, 13, 309, 14, 309, 15, 309, 16, 309, 17, 309, 18, 309, 19, 309, 20, 309, 21, 309, 22, 309, 23, 309, 24, 309, 25, 309, 26, 309, 27, 309, 28, 309, 29, 309, 30, 309, 31, 309, 32, 309, 33, 309, 49, 309, 36, 309, 37, 309, 38, 309, 39, 309, 40, 309, 41, 309, 42, 309, 60, 309, 44, 309, 45, 309, 59, 309, 58, 309, 48, 309, 50, 309, 56, 309, 52, 309, 53, 309, 57, 309, 62, 309, 63, 309, 64, 309, 65, 309, 66, 309, 67, 309, 68, 309, 69, 309],
	#[65535, 317, 14, 317, 12, 317, 39, 317, 51, 317, 43, 317, 10, 317, 31, 317, 50, 317, 30, 317, 29, 317, 28, 317, 33, 317, 13, 317, 27, 317, 15, 317, 16, 317, 17, 317, 18, 317, 19, 317, 20, 317, 21, 317, 22, 317, 23, 317, 24, 317, 25, 317, 26, 317, 32, 317, 49, 317, 41, 317, 35, 317, 36, 317, 37, 317, 38, 317, 40, 317, 42, 317, 48, 317, 44, 317, 45, 317, 46, 317, 47, 317, 52, 317, 53, 317, 55, 317, 56, 317, 57, 317, 58, 317, 59, 317, 60, 317, 61, 317, 62, 317, 63, 317, 64, 317, 65, 317, 66, 317, 67, 317, 68, 317, 69, 317],
	#[65535, 312, 10, 312, 12, 312, 13, 312, 14, 312, 15, 312, 16, 312, 17, 312, 18, 312, 19, 312, 20, 312, 21, 312, 22, 312, 23, 312, 24, 312, 25, 312, 26, 312, 27, 312, 28, 312, 29, 312, 30, 312, 31, 312, 32, 312, 33, 312, 35, 312, 36, 312, 37, 312, 38, 312, 39, 312, 40, 312, 41, 312, 42, 312, 43, 312, 44, 312, 45, 312, 46, 312, 47, 312, 48, 312, 49, 312, 50, 312, 51, 312, 52, 312, 53, 312, 55, 312, 56, 312, 57, 312, 58, 312, 59, 312, 60, 312, 61, 312, 62, 312, 63, 312, 64, 312, 65, 312, 66, 312, 67, 312, 68, 312, 69, 312],
	#[14, -82, 43, -101, 15, -80, 29, -124, 26, -98, 10, -71, 49, 343, 31, -100, 30, -110, 39, -119, 28, -107, 12, -77, 13, -92, 27, -85, 38, -127, 16, -102, 17, -96, 18, -79, 19, -84, 20, -122, 21, -94, 22, -89, 23, -75, 24, -83, 25, -121, 33, -70, 32, -90, 50, -81, 41, -95, 35, -105, 36, -88, 37, -74, 51, -133, 40, -109, 42, -114, 48, -104, 44, -91, 45, -78, 46, -130, 47, -120, 52, -128, 53, -108, 55, -76, 57, -123, 58, 300, 59, -97, 61, -72, 62, -131, 63, -112, 64, -126, 65, -113, 66, -132, 67, -115, 68, -118, 69, -99],
	#[18, -79, 19, -84, 20, -122, 21, -94, 22, -89, 23, -75, 24, -83, 25, -121, 26, -98, 27, -85, 28, -107, 29, -124, 30, -110, 31, -100, 32, -90, 33, -70],
	#[65535, 301, 56, 301, 58, 301, 60, 301],
	#[15, -80, 14, -82, 13, -92, 22, -89, 46, -130, 27, -85, 21, -94, 26, -98, 16, -102, 10, -71, 12, -77, 17, -96, 18, -79, 19, -84, 20, -122, 23, -75, 24, -83, 25, -121, 28, -107, 29, -124, 30, -110, 31, -100, 32, -90, 33, -70, 35, -105, 36, -88, 37, -74, 38, -127, 39, -119, 40, -109, 41, -95, 42, -114, 43, -101, 44, -91, 45, -78, 47, -120, 48, -104, 49, 343, 50, -81, 51, -133, 52, -128, 53, -108, 55, -76, 56, 304, 57, -123, 58, 304, 59, -97, 60, 304, 61, -72, 62, -131, 63, -112, 64, -126, 65, -113, 66, -132, 67, -115, 68, -118, 69, -99],
	#[65535, 305, 56, 305, 58, 305, 60, 305],
	#[65535, 340, 10, 340, 12, 340, 13, 340, 14, 340, 15, 340, 16, 340, 17, 340, 18, 340, 19, 340, 20, 340, 21, 340, 22, 340, 23, 340, 24, 340, 25, 340, 26, 340, 27, 340, 28, 340, 29, 340, 30, 340, 31, 340, 32, 340, 33, 340, 35, 340, 36, 340, 37, 340, 38, 340, 39, 340, 40, 340, 41, 340, 42, 340, 43, 340, 44, 340, 45, 340, 46, 340, 47, 340, 48, 340, 49, 340, 50, 340, 51, 340, 52, 340, 53, 340, 55, 340, 56, 340, 57, 340, 58, 340, 59, 340, 60, 340, 61, 340, 62, 340, 63, 340, 64, 340, 65, 340, 66, 340, 67, 340, 68, 340, 69, 340],
	#[58, -378],
	#[65535, 342, 10, 342, 12, 342, 13, 342, 14, 342, 15, 342, 16, 342, 17, 342, 18, 342, 19, 342, 20, 342, 21, 342, 22, 342, 23, 342, 24, 342, 25, 342, 26, 342, 27, 342, 28, 342, 29, 342, 30, 342, 31, 342, 32, 342, 33, 342, 35, 342, 36, 342, 37, 342, 38, 342, 39, 342, 40, 342, 41, 342, 42, 342, 43, 342, 44, 342, 45, 342, 46, 342, 47, 342, 48, 342, 49, 342, 50, 342, 51, 342, 52, 342, 53, 342, 55, 342, 56, 342, 57, 342, 58, 342, 59, 342, 60, 342, 61, 342, 62, 342, 63, 342, 64, 342, 65, 342, 66, 342, 67, 342, 68, 342, 69, 342],
	#[59, -141],
	#[10, -71, 12, -77, 13, -92, 14, -82, 15, -80, 16, -102, 17, -96, 18, -79, 19, -84, 20, -122, 21, -94, 22, -89, 23, -75, 24, -83, 25, -121, 26, -98, 27, -85, 28, -107, 29, -124, 30, -110, 31, -100, 32, -90, 33, -70, 35, -105, 36, -88, 37, -74, 38, -127, 39, -119, 40, -109, 41, -95, 42, -114, 43, -101, 44, -91, 45, -78, 46, -130, 47, -120, 48, -104, 49, 343, 50, -81, 51, -133, 52, -128, 53, -108, 55, -76, 57, -123, 59, -97, 60, 300, 61, -72, 62, -131, 63, -112, 64, -126, 65, -113, 66, -132, 67, -115, 68, -118, 69, -99],
	#[60, -376],
	#[65535, 303, 56, 303, 58, 303, 60, 303],
	#[58, -375],
	#[18, -79, 19, -84, 20, -122, 21, -94, 22, -89, 23, -75, 24, -83, 25, -121, 26, -98, 27, -85, 28, -107, 29, -124, 30, -110, 31, -100, 32, -90, 33, -70, 41, -146, 45, -147, 55, -150, 63, -148],
	#[65535, 351, 10, 351, 12, 351, 13, 351, 14, 351, 15, 351, 16, 351, 17, 351, 18, 351, 19, 351, 20, 351, 21, 351, 22, 351, 23, 351, 24, 351, 25, 351, 26, 351, 27, 351, 28, 351, 29, 351, 30, 351, 31, 351, 32, 351, 33, 351, 35, 351, 36, 351, 37, 351, 38, 351, 39, 351, 40, 351, 41, 351, 42, 351, 43, 351, 44, 351, 45, 351, 46, 351, 47, 351, 48, 351, 49, 351, 50, 351, 51, 351, 52, 351, 53, 351, 54, 351, 55, 351, 56, 351, 57, 351, 58, 351, 59, 351, 60, 351, 61, 351, 62, 351, 63, 351, 64, 351, 65, 351, 66, 351, 67, 351, 68, 351, 69, 351],
	#[65535, 352, 62, 352, 10, 352, 12, 352, 13, 352, 14, 352, 15, 352, 16, 352, 17, 352, 18, 352, 19, 352, 20, 352, 21, 352, 22, 352, 23, 352, 24, 352, 25, 352, 26, 352, 27, 352, 28, 352, 29, 352, 30, 352, 31, 352, 32, 352, 33, 352, 35, 352, 36, 352, 37, 352, 38, 352, 39, 352, 40, 352, 41, 352, 42, 352, 43, 352, 44, 352, 45, 352, 46, 352, 47, 352, 48, 352, 49, 352, 50, 352, 51, 352, 52, 352, 53, 352, 54, 352, 55, 352, 56, 352, 57, 352, 58, 352, 59, 352, 60, 352, 61, 352, 63, 352, 64, 352, 65, 352, 66, 352, 67, 352, 68, 352, 69, 352],
	#[65535, 350, 10, 350, 12, 350, 13, 350, 14, 350, 15, 350, 16, 350, 17, 350, 18, 350, 19, 350, 20, 350, 21, 350, 22, 350, 23, 350, 24, 350, 25, 350, 26, 350, 27, 350, 28, 350, 29, 350, 30, 350, 31, 350, 32, 350, 33, 350, 35, 350, 36, 350, 37, 350, 38, 350, 39, 350, 40, 350, 41, 350, 42, 350, 43, 350, 44, 350, 45, 350, 46, 350, 47, 350, 48, 350, 49, 350, 50, 350, 51, 350, 52, 350, 53, 350, 54, 350, 55, 350, 56, 350, 57, 350, 58, 350, 59, 350, 60, 350, 61, 350, 62, 350, 63, 350, 64, 350, 65, 350, 66, 350, 67, 350, 68, 350, 69, 350],
	#[15, 346, 23, 346, 29, 346, 31, 346, 13, 346, 10, 346, 43, 346, 59, 346, 51, 346, 27, 346, 47, 346, 14, 346, 19, 346, 28, 346, 12, 346, 55, 346, 42, 346, 22, 346, 46, 346, 16, 346, 21, 346, 17, 346, 26, 346, 18, 346, 35, 346, 39, 346, 36, 346, 20, 346, 48, 346, 24, 346, 25, 346, 30, 346, 54, -372, 32, 346, 33, 346, 37, 346, 38, 346, 62, 346, 40, 346, 41, 346, 61, 346, 60, 346, 44, 346, 45, 346, 58, 346, 49, 346, 50, 346, 56, 346, 52, 346, 53, 346, 57, 346, 63, 346, 64, 346, 65, 346, 66, 346, 67, 346, 68, 346, 69, 346],
	#[12, -29, 13, -17, 18, -62, 19, -51, 20, -35, 21, -53, 22, -10, 23, -67, 24, -54, 25, -55, 28, -3, 29, -16, 33, -56, 34, -32, 35, -38, 36, -15, 41, -21, 42, -47, 43, -31, 53, -39, 55, -6, 61, -5, 62, -59, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 349, 10, 349, 12, 349, 13, 349, 14, 349, 15, 349, 16, 349, 17, 349, 18, 349, 19, 349, 20, 349, 21, 349, 22, 349, 23, 349, 24, 349, 25, 349, 26, 349, 27, 349, 28, 349, 29, 349, 30, 349, 31, 349, 32, 349, 33, 349, 35, 349, 36, 349, 37, 349, 38, 349, 39, 349, 40, 349, 41, 349, 42, 349, 43, 349, 44, 349, 45, 349, 46, 349, 47, 349, 48, 349, 49, 349, 50, 349, 51, 349, 52, 349, 53, 349, 54, 349, 55, 349, 56, 349, 57, 349, 58, 349, 59, 349, 60, 349, 61, 349, 62, 349, 63, 349, 64, 349, 65, 349, 66, 349, 67, 349, 68, 349, 69, 349],
	#[56, -370],
	#[10, -155, 12, -77, 13, -92, 14, -82, 15, -80, 16, -102, 17, -96, 18, -62, 19, -51, 20, -35, 21, -53, 22, -10, 23, -67, 24, -54, 25, -55, 26, -37, 27, -12, 28, -3, 29, -16, 30, -43, 32, -165, 33, -56, 34, -32, 35, -176, 36, -170, 37, -157, 38, -154, 39, -175, 40, -178, 41, -168, 42, -183, 43, -172, 44, -166, 45, -164, 46, -194, 47, -188, 48, -181, 49, -180, 50, -161, 51, -196, 52, -193, 53, -192, 54, -179, 55, -158, 56, 96, 57, -186, 59, -169, 61, -156, 62, -191, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 77, 2, 77, 10, 77, 11, 77, 12, 77, 13, 77, 14, 77, 15, 77, 16, 77, 17, 77, 18, 77, 19, 77, 20, 77, 21, 77, 22, 77, 23, 77, 24, 77, 25, 77, 26, 77, 27, 77, 28, 77, 29, 77, 30, 77, 31, 77, 32, 77, 33, 77, 34, 77, 35, 77, 36, 77, 37, 77, 38, 77, 39, 77, 40, 77, 41, 77, 42, 77, 43, 77, 44, 77, 45, 77, 46, 77, 47, 77, 48, 77, 49, 77, 50, 77, 51, 77, 52, 77, 53, 77, 54, 77, 55, 77, 56, 77, 57, 77, 58, 77, 59, 77, 60, 77, 61, 77, 62, 77, 63, 77, 64, 77, 65, 77, 66, 77, 67, 77, 68, 77, 69, 77],
	#[62, -191, 2, 96, 30, -43, 55, -158, 31, 96, 61, -156, 58, 96, 45, -164, 46, -194, 59, -169, 56, 96, 53, -192, 57, -186, 37, -157, 54, -179, 23, -67, 10, -155, 52, -193, 36, -170, 20, -35, 35, -176, 41, -168, 29, -16, 51, -196, 60, 96, 44, -166, 28, -3, 43, -172, 49, -180, 19, -51, 39, -175, 48, -181, 50, -161, 18, -62, 15, -80, 42, -183, 27, -12, 34, -32, 14, -82, 38, -154, 26, -37, 13, -92, 12, -77, 17, -96, 25, -55, 33, -56, 22, -10, 40, -178, 16, -102, 21, -53, 24, -54, 32, -165, 47, -188, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 149, 2, 149, 10, 149, 11, 149, 12, 149, 13, 149, 14, 149, 15, 149, 16, 149, 17, 149, 18, 149, 19, 149, 20, 149, 21, 149, 22, 149, 23, 149, 24, 149, 25, 149, 26, 149, 27, 149, 28, 149, 29, 149, 30, 149, 31, 149, 32, 149, 33, 149, 34, 149, 35, 149, 36, 149, 37, 149, 38, 149, 39, 149, 40, 149, 41, 149, 42, 149, 43, 149, 44, 149, 45, 149, 46, 149, 47, 149, 48, 149, 49, 149, 50, 149, 51, 149, 52, 149, 53, 149, 54, 149, 55, 149, 56, 149, 57, 149, 58, 149, 59, 149, 60, 149, 61, 149, 62, 149, 63, 149, 64, 149, 65, 149, 66, 149, 67, 149, 68, 149, 69, 149],
	#[65535, 75, 14, 75, 2, 75, 10, 75, 11, 75, 12, 75, 13, 75, 24, 75, 15, 75, 16, 75, 17, 75, 18, 75, 19, 75, 20, 75, 21, 75, 22, 75, 23, 75, 25, 75, 26, 75, 27, 75, 28, 75, 29, 75, 30, 75, 31, 75, 32, 75, 33, 75, 34, 75, 35, 75, 36, 75, 37, 75, 38, 75, 39, 75, 40, 75, 41, 75, 42, 75, 43, 75, 44, 75, 45, 75, 46, 75, 47, 75, 48, 75, 49, 75, 50, 75, 51, 75, 52, 75, 53, 75, 54, 75, 55, 75, 56, 75, 57, 75, 58, 75, 59, 75, 60, 75, 61, 75, 62, 75, 63, 75, 64, 75, 65, 75, 66, 75, 67, 75, 68, 75, 69, 75],
	#[29, -16, 55, -158, 61, -156, 45, -164, 36, -170, 39, -175, 22, -10, 46, -194, 56, 96, 53, -192, 30, -43, 54, -179, 23, -67, 10, -155, 52, -193, 14, -82, 20, -35, 62, -191, 41, -168, 32, -165, 51, -196, 44, -166, 28, -3, 43, -172, 49, -180, 57, -186, 59, -169, 26, -37, 35, -176, 50, -161, 18, -62, 15, -80, 42, -183, 27, -12, 47, -188, 34, -32, 33, -56, 38, -154, 19, -51, 13, -92, 12, -77, 17, -96, 25, -55, 48, -181, 37, -157, 40, -178, 16, -102, 21, -53, 24, -54, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[2, 115, 10, -256, 31, 115, 56, 115, 58, 115, 60, 115],
	#[2, 108, 30, -43, 55, -158, 31, 108, 38, -154, 61, -156, 58, 108, 45, -164, 56, 108, 53, -192, 57, -186, 37, -157, 54, -179, 21, -53, 43, -172, 10, -155, 52, -193, 59, -169, 20, -35, 62, -191, 41, -168, 51, -196, 44, -166, 46, -194, 12, -77, 49, -180, 19, -51, 39, -175, 26, -37, 35, -176, 50, -161, 36, -170, 15, -80, 42, -183, 27, -12, 47, -188, 34, -32, 14, -82, 60, 108, 48, -181, 13, -92, 17, -96, 25, -55, 33, -56, 24, -54, 40, -178, 16, -102, 32, -165, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25],
	#[65535, 140, 15, 140, 53, 140, 50, 140, 14, 140, 36, 140, 39, 140, 46, 140, 13, 140, 34, 140, 60, 140, 44, 140, 51, 140, 30, 140, 25, 140, 48, 140, 61, 140, 19, 140, 59, 140, 38, 140, 58, 140, 49, 140, 31, 140, 24, 140, 17, 140, 42, 140, 33, 140, 12, 140, 37, 140, 32, 140, 21, 140, 41, 140, 20, 140, 10, 140, 40, 140, 16, 140, 62, 140, 57, 140, 35, 140, 55, 140, 11, 140, 56, 140, 2, 140, 18, 140, 22, 140, 23, 140, 26, 140, 27, 140, 28, 140, 29, 140, 43, 140, 45, 140, 47, 140, 52, 140, 54, 140, 63, 140, 64, 140, 65, 140, 66, 140, 67, 140, 68, 140, 69, 140],
	#[65535, 153, 2, 153, 10, 153, 11, 153, 12, 153, 13, 153, 14, 153, 15, 153, 16, 153, 17, 153, 18, 153, 19, 153, 20, 153, 21, 153, 22, 153, 23, 153, 24, 153, 25, 153, 26, 153, 27, 153, 28, 153, 29, 153, 30, 153, 31, 153, 32, 153, 33, 153, 34, 153, 35, 153, 36, 153, 37, 153, 38, 153, 39, 153, 40, 153, 41, 153, 42, 153, 43, 153, 44, 153, 45, 153, 46, 153, 47, 153, 48, 153, 49, 153, 50, 153, 51, 153, 52, 153, 53, 153, 54, 153, 55, 153, 56, 153, 57, 153, 58, 153, 59, 153, 60, 153, 61, 153, 62, 153, 63, 153, 64, 153, 65, 153, 66, 153, 67, 153, 68, 153, 69, 153],
	#[65535, 98, 31, 98, 58, 98, 60, 98, 56, 98, 2, 98],
	#[65535, 135, 2, 135, 10, 135, 11, 135, 12, 135, 13, 135, 14, 135, 15, 135, 16, 135, 17, 135, 18, 135, 19, 135, 20, 135, 21, 135, 22, 135, 23, 135, 24, 135, 25, 135, 26, 135, 27, 135, 28, 135, 29, 135, 30, 135, 31, 135, 32, 135, 33, 135, 34, 135, 35, 135, 36, 135, 37, 135, 38, 135, 39, 135, 40, 135, 41, 135, 42, 135, 43, 135, 44, 135, 45, 135, 46, 135, 47, 135, 48, 135, 49, 135, 50, 135, 51, 135, 52, 135, 53, 135, 54, 135, 55, 135, 56, 135, 57, 135, 58, 135, 59, 135, 60, 135, 61, 135, 62, 135, 63, 135, 64, 135, 65, 135, 66, 135, 67, 135, 68, 135, 69, 135],
	#[65535, 145, 2, 145, 10, 145, 11, 145, 12, 145, 13, 145, 14, 145, 15, 145, 16, 145, 17, 145, 18, 145, 19, 145, 20, 145, 21, 145, 22, 145, 23, 145, 24, 145, 25, 145, 26, 145, 27, 145, 28, 145, 29, 145, 30, 145, 31, 145, 32, 145, 33, 145, 34, 145, 35, 145, 36, 145, 37, 145, 38, 145, 39, 145, 40, 145, 41, 145, 42, 145, 43, 145, 44, 145, 45, 145, 46, 145, 47, 145, 48, 145, 49, 145, 50, 145, 51, 145, 52, 145, 53, 145, 54, 145, 55, 145, 56, 145, 57, 145, 58, 145, 59, 145, 60, 145, 61, 145, 62, 145, 63, 145, 64, 145, 65, 145, 66, 145, 67, 145, 68, 145, 69, 145],
	#[65535, 134, 23, 134, 55, 134, 28, 134, 38, 134, 62, 134, 27, 134, 45, 134, 50, 134, 39, 134, 46, 134, 13, 134, 15, 134, 2, 134, 44, 134, 47, 134, 30, 134, 54, 134, 43, 134, 61, 134, 14, 134, 20, 134, 35, 134, 29, 134, 31, 134, 18, 134, 60, 134, 22, 134, 53, 134, 41, 134, 49, 134, 37, 134, 16, 134, 26, 134, 21, 134, 52, 134, 11, 134, 51, 134, 42, 134, 48, 134, 24, 134, 34, 134, 56, 134, 58, 134, 19, 134, 59, 134, 12, 134, 17, 134, 32, 134, 33, 134, 10, 134, 40, 134, 57, 134, 36, 134, 25, 134, 63, 134, 64, 134, 65, 134, 66, 134, 67, 134, 68, 134, 69, 134],
	#[65535, 130, 15, 130, 10, 130, 16, 130, 2, 130, 20, 130, 19, 130, 13, 130, 17, 130, 18, 130, 34, 130, 24, 130, 25, 130, 26, 130, 30, 130, 31, 130, 32, 130, 33, 130, 41, 130, 35, 130, 36, 130, 37, 130, 38, 130, 40, 130, 42, 130, 43, 130, 44, 130, 45, 130, 46, 130, 48, 130, 49, 130, 50, 130, 51, 130, 52, 130, 53, 130, 54, 130, 55, 130, 56, 130, 57, 130, 58, 130, 59, 130, 60, 130, 61, 130, 62, 130, 63, 130, 64, 130, 65, 130, 66, 130, 67, 130, 68, 130, 69, 130, 47, 130, 23, 130, 14, 130, 29, 130, 27, 130, 22, 130, 28, 130, 12, 130, 39, 130, 21, 130, 11, 130],
	#[65535, 152, 22, 152, 23, 152, 30, 152, 55, 152, 28, 152, 12, 152, 14, 152, 38, 152, 45, 152, 50, 152, 39, 152, 29, 152, 46, 152, 15, 152, 53, 152, 44, 152, 47, 152, 37, 152, 54, 152, 43, 152, 52, 152, 36, 152, 20, 152, 35, 152, 32, 152, 31, 152, 18, 152, 27, 152, 25, 152, 10, 152, 48, 152, 49, 152, 2, 152, 16, 152, 26, 152, 21, 152, 24, 152, 11, 152, 51, 152, 42, 152, 17, 152, 34, 152, 33, 152, 41, 152, 19, 152, 13, 152, 40, 152, 56, 152, 57, 152, 58, 152, 59, 152, 60, 152, 61, 152, 62, 152, 63, 152, 64, 152, 65, 152, 66, 152, 67, 152, 68, 152, 69, 152],
	#[61, -156, 47, -188, 38, -154, 62, -191, 46, -194, 60, 96, 30, -43, 54, -179, 39, -175, 29, -16, 52, -193, 36, -170, 53, -192, 49, -180, 59, -169, 23, -67, 41, -168, 28, -3, 55, -158, 10, -155, 45, -164, 43, -172, 19, -51, 26, -37, 32, -165, 35, -176, 40, -178, 51, -196, 14, -82, 37, -157, 57, -186, 44, -166, 21, -53, 12, -77, 15, -80, 50, -161, 22, -10, 42, -183, 48, -181, 24, -54, 20, -35, 27, -12, 13, -92, 17, -96, 18, -62, 34, -32, 25, -55, 33, -56, 16, -102, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 76, 23, 76, 27, 76, 2, 76, 17, 76, 42, 76, 10, 76, 11, 76, 12, 76, 13, 76, 14, 76, 15, 76, 16, 76, 18, 76, 19, 76, 20, 76, 21, 76, 22, 76, 46, 76, 24, 76, 25, 76, 26, 76, 44, 76, 28, 76, 29, 76, 30, 76, 31, 76, 32, 76, 33, 76, 34, 76, 35, 76, 36, 76, 37, 76, 38, 76, 39, 76, 40, 76, 41, 76, 43, 76, 45, 76, 47, 76, 48, 76, 49, 76, 50, 76, 51, 76, 52, 76, 53, 76, 54, 76, 55, 76, 56, 76, 57, 76, 58, 76, 59, 76, 60, 76, 61, 76, 62, 76, 63, 76, 64, 76, 65, 76, 66, 76, 67, 76, 68, 76, 69, 76],
	#[65535, 129, 30, 129, 18, 129, 15, 129, 14, 129, 44, 129, 23, 129, 12, 129, 22, 129, 27, 129, 2, 129, 32, 129, 42, 129, 10, 129, 11, 129, 34, 129, 13, 129, 24, 129, 26, 129, 16, 129, 17, 129, 25, 129, 19, 129, 20, 129, 21, 129, 41, 129, 28, 129, 29, 129, 43, 129, 31, 129, 33, 129, 35, 129, 36, 129, 37, 129, 38, 129, 39, 129, 40, 129, 45, 129, 46, 129, 47, 129, 48, 129, 49, 129, 50, 129, 51, 129, 52, 129, 53, 129, 54, 129, 55, 129, 56, 129, 57, 129, 58, 129, 59, 129, 60, 129, 61, 129, 62, 129, 63, 129, 64, 129, 65, 129, 66, 129, 67, 129, 68, 129, 69, 129],
	#[65535, 133, 15, 133, 23, 133, 46, 133, 28, 133, 55, 133, 13, 133, 53, 133, 39, 133, 44, 133, 43, 133, 12, 133, 61, 133, 50, 133, 22, 133, 47, 133, 52, 133, 36, 133, 27, 133, 32, 133, 30, 133, 54, 133, 17, 133, 18, 133, 37, 133, 42, 133, 31, 133, 14, 133, 16, 133, 62, 133, 26, 133, 45, 133, 20, 133, 29, 133, 33, 133, 35, 133, 2, 133, 58, 133, 51, 133, 25, 133, 48, 133, 21, 133, 49, 133, 57, 133, 19, 133, 34, 133, 38, 133, 40, 133, 11, 133, 24, 133, 60, 133, 56, 133, 10, 133, 41, 133, 59, 133, 63, 133, 64, 133, 65, 133, 66, 133, 67, 133, 68, 133, 69, 133],
	#[65535, 127, 15, 127, 54, 127, 23, 127, 14, 127, 18, 127, 27, 127, 22, 127, 10, 127, 43, 127, 39, 127, 21, 127, 11, 127, 51, 127, 42, 127, 47, 127, 29, 127, 19, 127, 28, 127, 53, 127, 55, 127, 52, 127, 2, 127, 20, 127, 12, 127, 13, 127, 16, 127, 17, 127, 34, 127, 24, 127, 25, 127, 26, 127, 30, 127, 31, 127, 32, 127, 33, 127, 41, 127, 35, 127, 36, 127, 37, 127, 38, 127, 40, 127, 49, 127, 48, 127, 44, 127, 45, 127, 46, 127, 50, 127, 56, 127, 57, 127, 58, 127, 59, 127, 60, 127, 61, 127, 62, 127, 63, 127, 64, 127, 65, 127, 66, 127, 67, 127, 68, 127, 69, 127],
	#[2, 115, 60, 115, 10, -256, 56, 115, 31, 115, 58, 115],
	#[65535, 78, 31, 78, 14, 78, 43, 78, 12, 78, 15, 78, 22, 78, 46, 78, 36, 78, 20, 78, 30, 78, 53, 78, 50, 78, 25, 78, 10, 78, 21, 78, 34, 78, 48, 78, 16, 78, 39, 78, 29, 78, 19, 78, 13, 78, 35, 78, 44, 78, 51, 78, 37, 78, 32, 78, 33, 78, 41, 78, 42, 78, 56, 78, 59, 78, 24, 78, 58, 78, 49, 78, 57, 78, 60, 78, 2, 78, 17, 78, 40, 78, 62, 78, 11, 78, 18, 78, 23, 78, 26, 78, 27, 78, 28, 78, 38, 78, 45, 78, 47, 78, 52, 78, 54, 78, 55, 78, 61, 78, 63, 78, 64, 78, 65, 78, 66, 78, 67, 78, 68, 78, 69, 78],
	#[65535, 74, 54, 74, 46, 74, 23, 74, 55, 74, 28, 74, 31, 74, 38, 74, 45, 74, 36, 74, 39, 74, 22, 74, 42, 74, 15, 74, 53, 74, 44, 74, 47, 74, 30, 74, 27, 74, 21, 74, 43, 74, 52, 74, 14, 74, 2, 74, 35, 74, 29, 74, 51, 74, 18, 74, 13, 74, 25, 74, 10, 74, 12, 74, 49, 74, 37, 74, 16, 74, 26, 74, 48, 74, 50, 74, 11, 74, 20, 74, 32, 74, 17, 74, 34, 74, 33, 74, 41, 74, 19, 74, 24, 74, 40, 74, 56, 74, 57, 74, 58, 74, 59, 74, 60, 74, 61, 74, 62, 74, 63, 74, 64, 74, 65, 74, 66, 74, 67, 74, 68, 74, 69, 74],
	#[65535, 126, 23, 126, 55, 126, 61, 126, 22, 126, 47, 126, 27, 126, 30, 126, 43, 126, 28, 126, 12, 126, 14, 126, 62, 126, 26, 126, 45, 126, 50, 126, 39, 126, 29, 126, 46, 126, 59, 126, 15, 126, 53, 126, 44, 126, 51, 126, 37, 126, 54, 126, 18, 126, 21, 126, 42, 126, 10, 126, 36, 126, 38, 126, 35, 126, 16, 126, 11, 126, 31, 126, 13, 126, 25, 126, 49, 126, 2, 126, 58, 126, 17, 126, 48, 126, 52, 126, 20, 126, 32, 126, 24, 126, 34, 126, 56, 126, 60, 126, 19, 126, 57, 126, 40, 126, 33, 126, 41, 126, 63, 126, 64, 126, 65, 126, 66, 126, 67, 126, 68, 126, 69, 126],
	#[2, 96, 10, -155, 12, -77, 13, -92, 14, -82, 15, -80, 16, -102, 17, -96, 18, -62, 19, -51, 20, -35, 21, -53, 22, -10, 23, -67, 24, -54, 25, -55, 26, -37, 27, -12, 28, -3, 29, -16, 30, -43, 31, 96, 32, -165, 33, -56, 34, -32, 35, -176, 36, -170, 37, -157, 38, -154, 39, -175, 40, -178, 41, -168, 42, -183, 43, -172, 44, -166, 45, -164, 46, -194, 47, -188, 48, -181, 49, -180, 50, -161, 51, -196, 52, -193, 53, -192, 54, -179, 55, -158, 56, 96, 57, -186, 58, 96, 59, -169, 60, 96, 61, -156, 62, -191, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 144, 15, 144, 44, 144, 23, 144, 12, 144, 42, 144, 22, 144, 27, 144, 2, 144, 17, 144, 43, 144, 18, 144, 24, 144, 10, 144, 11, 144, 34, 144, 13, 144, 14, 144, 26, 144, 16, 144, 32, 144, 25, 144, 19, 144, 20, 144, 21, 144, 41, 144, 28, 144, 29, 144, 30, 144, 31, 144, 33, 144, 35, 144, 36, 144, 37, 144, 38, 144, 39, 144, 40, 144, 45, 144, 46, 144, 47, 144, 48, 144, 49, 144, 50, 144, 51, 144, 52, 144, 53, 144, 54, 144, 55, 144, 56, 144, 57, 144, 58, 144, 59, 144, 60, 144, 61, 144, 62, 144, 63, 144, 64, 144, 65, 144, 66, 144, 67, 144, 68, 144, 69, 144],
	#[65535, 139, 62, 139, 22, 139, 47, 139, 23, 139, 30, 139, 43, 139, 50, 139, 28, 139, 14, 139, 61, 139, 27, 139, 45, 139, 36, 139, 39, 139, 29, 139, 46, 139, 15, 139, 53, 139, 44, 139, 51, 139, 12, 139, 54, 139, 18, 139, 21, 139, 42, 139, 10, 139, 55, 139, 38, 139, 35, 139, 26, 139, 11, 139, 31, 139, 13, 139, 25, 139, 37, 139, 59, 139, 17, 139, 48, 139, 52, 139, 20, 139, 32, 139, 24, 139, 34, 139, 16, 139, 60, 139, 19, 139, 57, 139, 40, 139, 33, 139, 41, 139, 2, 139, 58, 139, 49, 139, 56, 139, 63, 139, 64, 139, 65, 139, 66, 139, 67, 139, 68, 139, 69, 139],
	#[65535, 138, 2, 138, 10, 138, 11, 138, 12, 138, 13, 138, 14, 138, 15, 138, 16, 138, 17, 138, 18, 138, 19, 138, 20, 138, 21, 138, 22, 138, 23, 138, 24, 138, 25, 138, 26, 138, 27, 138, 28, 138, 29, 138, 30, 138, 31, 138, 32, 138, 33, 138, 34, 138, 35, 138, 36, 138, 37, 138, 38, 138, 39, 138, 40, 138, 41, 138, 42, 138, 43, 138, 44, 138, 45, 138, 46, 138, 47, 138, 48, 138, 49, 138, 50, 138, 51, 138, 52, 138, 53, 138, 54, 138, 55, 138, 56, 138, 57, 138, 58, 138, 59, 138, 60, 138, 61, 138, 62, 138, 63, 138, 64, 138, 65, 138, 66, 138, 67, 138, 68, 138, 69, 138],
	#[65535, 151, 55, 151, 47, 151, 23, 151, 30, 151, 54, 151, 28, 151, 14, 151, 27, 151, 45, 151, 50, 151, 20, 151, 22, 151, 46, 151, 15, 151, 53, 151, 44, 151, 51, 151, 37, 151, 25, 151, 21, 151, 43, 151, 36, 151, 38, 151, 35, 151, 29, 151, 31, 151, 18, 151, 13, 151, 42, 151, 10, 151, 12, 151, 49, 151, 2, 151, 39, 151, 26, 151, 48, 151, 52, 151, 11, 151, 16, 151, 32, 151, 17, 151, 34, 151, 33, 151, 41, 151, 19, 151, 24, 151, 40, 151, 56, 151, 57, 151, 58, 151, 59, 151, 60, 151, 61, 151, 62, 151, 63, 151, 64, 151, 65, 151, 66, 151, 67, 151, 68, 151, 69, 151],
	#[65535, 132, 47, 132, 46, 132, 61, 132, 54, 132, 15, 132, 45, 132, 23, 132, 13, 132, 30, 132, 55, 132, 28, 132, 31, 132, 14, 132, 38, 132, 62, 132, 52, 132, 39, 132, 22, 132, 42, 132, 10, 132, 16, 132, 35, 132, 53, 132, 44, 132, 51, 132, 12, 132, 50, 132, 18, 132, 21, 132, 43, 132, 57, 132, 36, 132, 20, 132, 58, 132, 41, 132, 29, 132, 48, 132, 27, 132, 25, 132, 49, 132, 37, 132, 56, 132, 26, 132, 33, 132, 32, 132, 17, 132, 34, 132, 2, 132, 60, 132, 19, 132, 59, 132, 40, 132, 11, 132, 24, 132, 63, 132, 64, 132, 65, 132, 66, 132, 67, 132, 68, 132, 69, 132],
	#[56, -344],
	#[65535, 131, 23, 131, 28, 131, 45, 131, 44, 131, 43, 131, 22, 131, 47, 131, 52, 131, 27, 131, 30, 131, 55, 131, 50, 131, 18, 131, 37, 131, 42, 131, 14, 131, 38, 131, 26, 131, 36, 131, 39, 131, 29, 131, 46, 131, 13, 131, 15, 131, 53, 131, 57, 131, 51, 131, 54, 131, 48, 131, 49, 131, 61, 131, 19, 131, 34, 131, 20, 131, 35, 131, 16, 131, 32, 131, 24, 131, 60, 131, 25, 131, 33, 131, 12, 131, 2, 131, 58, 131, 17, 131, 21, 131, 41, 131, 40, 131, 62, 131, 59, 131, 11, 131, 10, 131, 56, 131, 31, 131, 63, 131, 64, 131, 65, 131, 66, 131, 67, 131, 68, 131, 69, 131],
	#[55, -158, 54, -179, 30, -43, 53, -192, 51, -196, 45, -164, 39, -175, 49, -180, 47, -188, 37, -157, 50, -161, 23, -67, 10, -155, 52, -193, 36, -170, 20, -35, 35, -176, 41, -168, 29, -16, 48, -181, 40, -178, 22, -10, 28, -3, 43, -172, 24, -54, 19, -51, 16, -102, 32, -165, 21, -53, 33, -56, 18, -62, 15, -80, 42, -183, 27, -12, 17, -96, 34, -32, 14, -82, 38, -154, 26, -37, 13, -92, 12, -77, 25, -55, 44, -166, 46, -194, 57, -186, 58, 96, 59, -169, 61, -156, 62, -191, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[55, -158, 54, -179, 62, -191, 61, -156, 45, -164, 2, 96, 53, -192, 60, 96, 31, 96, 38, -154, 51, -196, 58, 96, 43, -172, 46, -194, 59, -169, 56, 96, 15, -80, 49, -180, 57, -186, 40, -178, 30, -43, 50, -161, 23, -67, 10, -155, 52, -193, 36, -170, 20, -35, 35, -176, 41, -168, 29, -16, 48, -181, 24, -54, 27, -12, 44, -166, 28, -3, 12, -77, 37, -157, 39, -175, 32, -165, 25, -55, 18, -62, 42, -183, 17, -96, 34, -32, 14, -82, 19, -51, 13, -92, 33, -56, 22, -10, 16, -102, 21, -53, 26, -37, 47, -188, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 137, 54, 137, 23, 137, 22, 137, 18, 137, 43, 137, 42, 137, 12, 137, 29, 137, 27, 137, 15, 137, 38, 137, 47, 137, 37, 137, 26, 137, 50, 137, 46, 137, 28, 137, 55, 137, 45, 137, 48, 137, 30, 137, 59, 137, 35, 137, 24, 137, 53, 137, 51, 137, 14, 137, 39, 137, 62, 137, 44, 137, 16, 137, 25, 137, 61, 137, 41, 137, 49, 137, 32, 137, 52, 137, 36, 137, 20, 137, 2, 137, 13, 137, 57, 137, 17, 137, 19, 137, 10, 137, 21, 137, 34, 137, 58, 137, 33, 137, 56, 137, 60, 137, 11, 137, 40, 137, 31, 137, 63, 137, 64, 137, 65, 137, 66, 137, 67, 137, 68, 137, 69, 137],
	#[65535, 128, 22, 128, 15, 128, 23, 128, 46, 128, 28, 128, 45, 128, 29, 128, 54, 128, 53, 128, 14, 128, 39, 128, 44, 128, 43, 128, 61, 128, 50, 128, 49, 128, 47, 128, 32, 128, 36, 128, 27, 128, 13, 128, 30, 128, 55, 128, 17, 128, 18, 128, 37, 128, 42, 128, 12, 128, 57, 128, 38, 128, 62, 128, 26, 128, 52, 128, 20, 128, 48, 128, 24, 128, 10, 128, 33, 128, 35, 128, 2, 128, 58, 128, 51, 128, 25, 128, 21, 128, 40, 128, 19, 128, 34, 128, 16, 128, 11, 128, 31, 128, 60, 128, 56, 128, 41, 128, 59, 128, 63, 128, 64, 128, 65, 128, 66, 128, 67, 128, 68, 128, 69, 128],
	#[65535, 97, 60, 97, 2, 97, 58, 97, 56, 97, 31, 97],
	#[65535, 150, 54, 150, 45, 150, 23, 150, 52, 150, 22, 150, 46, 150, 18, 150, 43, 150, 42, 150, 44, 150, 30, 150, 12, 150, 29, 150, 27, 150, 15, 150, 38, 150, 47, 150, 37, 150, 26, 150, 50, 150, 51, 150, 28, 150, 55, 150, 17, 150, 48, 150, 19, 150, 59, 150, 35, 150, 24, 150, 53, 150, 16, 150, 14, 150, 39, 150, 62, 150, 2, 150, 21, 150, 25, 150, 61, 150, 41, 150, 49, 150, 32, 150, 11, 150, 36, 150, 20, 150, 56, 150, 13, 150, 57, 150, 60, 150, 10, 150, 40, 150, 58, 150, 33, 150, 34, 150, 31, 150, 63, 150, 64, 150, 65, 150, 66, 150, 67, 150, 68, 150, 69, 150],
	#[65535, 143, 23, 143, 46, 143, 45, 143, 15, 143, 47, 143, 27, 143, 30, 143, 55, 143, 18, 143, 28, 143, 12, 143, 14, 143, 38, 143, 26, 143, 52, 143, 50, 143, 43, 143, 22, 143, 19, 143, 41, 143, 53, 143, 44, 143, 51, 143, 16, 143, 54, 143, 48, 143, 21, 143, 42, 143, 61, 143, 36, 143, 20, 143, 35, 143, 29, 143, 31, 143, 13, 143, 25, 143, 32, 143, 49, 143, 37, 143, 39, 143, 17, 143, 33, 143, 57, 143, 24, 143, 34, 143, 56, 143, 60, 143, 62, 143, 59, 143, 40, 143, 11, 143, 10, 143, 2, 143, 58, 143, 63, 143, 64, 143, 65, 143, 66, 143, 67, 143, 68, 143, 69, 143],
	#[65535, 142, 23, 142, 28, 142, 45, 142, 43, 142, 22, 142, 47, 142, 27, 142, 30, 142, 55, 142, 50, 142, 42, 142, 14, 142, 26, 142, 52, 142, 36, 142, 39, 142, 29, 142, 46, 142, 13, 142, 15, 142, 53, 142, 44, 142, 51, 142, 16, 142, 54, 142, 48, 142, 49, 142, 19, 142, 34, 142, 38, 142, 35, 142, 32, 142, 31, 142, 18, 142, 60, 142, 25, 142, 33, 142, 12, 142, 37, 142, 58, 142, 17, 142, 21, 142, 41, 142, 20, 142, 57, 142, 24, 142, 40, 142, 2, 142, 62, 142, 59, 142, 11, 142, 10, 142, 56, 142, 61, 142, 63, 142, 64, 142, 65, 142, 66, 142, 67, 142, 68, 142, 69, 142],
	#[65535, 136, 23, 136, 47, 136, 27, 136, 28, 136, 26, 136, 45, 136, 50, 136, 22, 136, 46, 136, 15, 136, 53, 136, 44, 136, 51, 136, 30, 136, 54, 136, 42, 136, 19, 136, 14, 136, 38, 136, 16, 136, 29, 136, 31, 136, 18, 136, 13, 136, 25, 136, 32, 136, 43, 136, 49, 136, 37, 136, 39, 136, 17, 136, 21, 136, 52, 136, 36, 136, 20, 136, 57, 136, 48, 136, 24, 136, 34, 136, 33, 136, 60, 136, 62, 136, 59, 136, 12, 136, 35, 136, 55, 136, 11, 136, 10, 136, 40, 136, 56, 136, 2, 136, 61, 136, 41, 136, 58, 136, 63, 136, 64, 136, 65, 136, 66, 136, 67, 136, 68, 136, 69, 136],
	#[65535, 125, 23, 125, 45, 125, 15, 125, 22, 125, 46, 125, 27, 125, 13, 125, 30, 125, 55, 125, 50, 125, 28, 125, 12, 125, 14, 125, 38, 125, 26, 125, 52, 125, 16, 125, 43, 125, 29, 125, 19, 125, 41, 125, 53, 125, 44, 125, 47, 125, 37, 125, 54, 125, 48, 125, 21, 125, 42, 125, 61, 125, 36, 125, 20, 125, 35, 125, 32, 125, 51, 125, 18, 125, 60, 125, 25, 125, 10, 125, 49, 125, 2, 125, 39, 125, 17, 125, 33, 125, 57, 125, 24, 125, 34, 125, 56, 125, 58, 125, 62, 125, 59, 125, 40, 125, 11, 125, 31, 125, 63, 125, 64, 125, 65, 125, 66, 125, 67, 125, 68, 125, 69, 125],
	#[65535, 141, 23, 141, 13, 141, 52, 141, 22, 141, 42, 141, 29, 141, 27, 141, 15, 141, 43, 141, 47, 141, 26, 141, 50, 141, 46, 141, 28, 141, 55, 141, 45, 141, 48, 141, 30, 141, 54, 141, 32, 141, 20, 141, 18, 141, 53, 141, 51, 141, 14, 141, 39, 141, 62, 141, 44, 141, 58, 141, 12, 141, 61, 141, 59, 141, 49, 141, 35, 141, 10, 141, 25, 141, 16, 141, 57, 141, 17, 141, 60, 141, 37, 141, 31, 141, 21, 141, 38, 141, 2, 141, 24, 141, 40, 141, 36, 141, 56, 141, 41, 141, 33, 141, 19, 141, 34, 141, 11, 141, 63, 141, 64, 141, 65, 141, 66, 141, 67, 141, 68, 141, 69, 141],
	#[65535, 211, 46, 211, 56, 211, 40, 211, 58, 211],
	#[65535, 206, 58, 206],
	#[65535, 210, 40, 210, 46, 210, 56, 210, 58, 210],
	#[58, -212],
	#[65535, 208, 46, 208, 58, 208, 56, 208, 40, 208],
	#[46, 207, 56, 207, 40, -203, 58, 207],
	#[62, -59, 12, -29, 41, -197, 61, -5, 13, -17, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25],
	#[65535, 209, 58, 209, 56, 209, 46, 209, 40, 209],
	#[65535, 199, 38, 199, 37, 199, 46, 199, 55, 199, 39, 199, 57, 199, 10, 199, 36, 199, 58, 199, 40, 199, 11, 199, 2, 199, 48, 199, 56, 199],
	#[46, -209, 56, 206],
	#[56, -208],
	#[65535, 201, 39, 201, 23, 201, 46, 201, 55, 201, 45, 201, 38, 201, 57, 201, 15, 201, 22, 201, 47, 201, 11, 201, 36, 201, 27, 201, 30, 201, 43, 201, 50, 201, 37, 201, 28, 201, 10, 201, 14, 201, 56, 201, 62, 201, 48, 201, 26, 201, 52, 201, 20, 201, 29, 201, 42, 201, 40, 201, 13, 201, 35, 201, 53, 201, 44, 201, 51, 201, 12, 201, 54, 201, 18, 201, 21, 201, 49, 201, 61, 201, 19, 201, 34, 201, 2, 201, 58, 201, 16, 201, 32, 201, 31, 201, 24, 201, 60, 201, 25, 201, 33, 201, 41, 201, 17, 201, 59, 201, 63, 201, 64, 201, 65, 201, 66, 201, 67, 201, 68, 201, 69, 201],
	#[61, -5, 62, -59, 13, -17, 12, -29, 41, -197, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25],
	#[56, -211],
	#[65535, 200, 44, 200, 30, 200, 23, 200, 10, 200, 14, 200, 11, 200, 18, 200, 27, 200, 25, 200, 28, 200, 26, 200, 21, 200, 52, 200, 15, 200, 34, 200, 16, 200, 19, 200, 13, 200, 12, 200, 42, 200, 22, 200, 2, 200, 20, 200, 32, 200, 24, 200, 17, 200, 41, 200, 49, 200, 29, 200, 43, 200, 31, 200, 33, 200, 35, 200, 36, 200, 37, 200, 38, 200, 39, 200, 40, 200, 45, 200, 46, 200, 47, 200, 48, 200, 50, 200, 51, 200, 53, 200, 54, 200, 55, 200, 56, 200, 57, 200, 58, 200, 59, 200, 60, 200, 61, 200, 62, 200, 63, 200, 64, 200, 65, 200, 66, 200, 67, 200, 68, 200, 69, 200],
	#[65535, 202, 23, 202, 47, 202, 36, 202, 27, 202, 57, 202, 55, 202, 37, 202, 28, 202, 10, 202, 14, 202, 11, 202, 26, 202, 50, 202, 39, 202, 22, 202, 46, 202, 40, 202, 13, 202, 15, 202, 53, 202, 44, 202, 51, 202, 30, 202, 54, 202, 48, 202, 42, 202, 56, 202, 52, 202, 17, 202, 38, 202, 62, 202, 16, 202, 29, 202, 31, 202, 18, 202, 60, 202, 25, 202, 33, 202, 43, 202, 19, 202, 58, 202, 21, 202, 41, 202, 20, 202, 32, 202, 45, 202, 24, 202, 2, 202, 59, 202, 12, 202, 35, 202, 49, 202, 34, 202, 61, 202, 63, 202, 64, 202, 65, 202, 66, 202, 67, 202, 68, 202, 69, 202],
	#[65535, 102, 60, 102, 10, 102, 2, 102, 11, 102, 56, 102, 31, 102, 58, 102],
	#[31, 118, 62, -191, 52, -193, 2, 118, 55, -158, 60, 118, 10, 118, 61, -156, 58, 118, 45, -164, 39, -175, 46, -194, 59, -169, 15, -80, 53, -192, 44, -166, 51, -196, 56, 118, 54, -179, 43, -172, 57, -186, 36, -170, 20, -35, 35, -176, 41, -168, 11, 118, 48, -181, 40, -217, 25, -55, 49, -180, 37, -157, 16, -102, 32, -165, 50, -161, 42, -183, 17, -96, 34, -32, 14, -82, 38, -154, 19, -51, 13, -92, 12, -77, 33, -56, 24, -54, 21, -53, 47, -188, 63, -182, 65, -46, 66, -65, 67, -50, 68, -25],
	#[65535, 101, 60, 101, 2, 101, 11, 101, 10, 101, 56, 101, 31, 101, 58, 101],
	#[65535, 154, 10, 154, 11, 154, 31, 154, 2, 154, 56, 154, 58, 154, 60, 154],
	#[53, -192, 51, -196, 2, 100, 49, -180, 10, 100, 11, 100, 12, -77, 13, -92, 14, -82, 15, -80, 16, -102, 17, -96, 18, -62, 19, -51, 20, -35, 21, -53, 22, -10, 23, -67, 24, -54, 25, -55, 41, -168, 28, -3, 29, -16, 31, 100, 32, -165, 33, -56, 34, -32, 35, -176, 36, -170, 37, -157, 38, -154, 39, -175, 40, -217, 42, -183, 43, -172, 44, -166, 45, -164, 46, -194, 47, -188, 48, -181, 50, -161, 52, -193, 54, -179, 55, -158, 56, 100, 57, -186, 58, 100, 59, -169, 60, 100, 61, -156, 62, -191, 63, -182, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[55, -158, 31, 100, 54, -179, 62, -191, 46, -194, 2, 100, 53, -192, 60, 100, 10, 100, 38, -154, 61, -156, 58, 100, 43, -172, 42, -183, 59, -169, 56, 100, 49, -180, 11, 100, 51, -196, 37, -157, 50, -161, 23, -67, 57, -186, 52, -193, 36, -170, 20, -35, 35, -176, 41, -168, 29, -16, 48, -181, 13, -92, 22, -10, 28, -3, 12, -77, 19, -51, 39, -175, 32, -165, 25, -55, 18, -62, 15, -80, 45, -164, 24, -54, 34, -32, 14, -82, 17, -96, 33, -56, 40, -217, 16, -102, 21, -53, 44, -166, 47, -188, 63, -182, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[58, -343],
	#[65535, 112, 31, 112, 60, 112, 56, 112, 2, 112, 58, 112],
	#[51, -196, 41, -168, 10, -268, 12, -77, 13, -92, 14, -82, 15, -80, 16, -102, 17, -96, 18, -62, 19, -51, 20, -35, 21, -53, 22, -10, 23, -67, 24, -54, 25, -55, 26, -37, 27, -12, 28, -3, 29, -16, 30, -43, 31, 256, 32, -165, 33, -56, 34, -32, 35, -176, 36, -170, 37, -157, 38, -154, 39, -175, 40, -284, 42, -285, 43, -280, 44, -276, 45, -271, 46, -194, 47, -188, 48, -181, 49, -180, 50, -161, 52, -193, 53, -192, 54, -179, 55, -269, 57, -288, 59, -141, 61, -156, 62, -191, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[31, 157, 10, 157, 60, 157, 11, 157, 2, 157, 58, 157, 56, 157, 40, -339],
	#[55, -158, 54, -179, 30, -43, 53, -192, 31, 96, 51, -196, 45, -164, 46, -194, 49, -180, 44, -166, 47, -188, 37, -157, 25, -55, 40, -178, 10, -155, 52, -193, 36, -170, 20, -35, 35, -176, 41, -168, 29, -16, 48, -181, 27, -12, 22, -10, 28, -3, 43, -172, 24, -54, 19, -51, 39, -175, 32, -165, 21, -53, 33, -56, 18, -62, 15, -80, 42, -183, 17, -96, 34, -32, 14, -82, 38, -154, 26, -37, 23, -67, 12, -77, 13, -92, 16, -102, 50, -161, 57, -186, 59, -169, 61, -156, 62, -191, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 155, 2, 155, 60, 155, 11, 155, 10, 155, 31, 155, 58, 155, 56, 155],
	#[53, -192, 30, -43, 44, -166, 23, -67, 22, -10, 34, -32, 31, 96, 26, -37, 41, -168, 32, -165, 24, -54, 10, -155, 25, -55, 12, -77, 13, -92, 14, -82, 15, -80, 16, -102, 17, -96, 18, -62, 19, -51, 20, -35, 21, -53, 27, -12, 28, -3, 29, -16, 43, -172, 42, -183, 33, -56, 35, -176, 36, -170, 37, -157, 38, -154, 39, -175, 40, -178, 45, -164, 46, -194, 47, -188, 48, -181, 49, -180, 50, -161, 51, -196, 52, -193, 54, -179, 55, -158, 57, -186, 59, -169, 61, -156, 62, -191, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[31, -227],
	#[2, 171, 20, -232, 10, 171, 11, 171, 18, -62, 21, -53, 22, -10, 23, -235, 25, -230, 28, -3, 29, -16, 31, 171, 33, -56, 34, -32, 40, 171, 56, 171, 58, 171, 60, 171],
	#[65535, 159, 10, 159, 11, 159, 31, 159, 56, 159, 2, 159, 40, 159, 58, 159, 60, 159],
	#[28, -3, 20, -35, 29, -16, 23, -67, 18, -62, 33, -56, 22, -10, 34, -32, 21, -53],
	#[65535, 58, 22, 58, 23, 58, 55, 58, 20, 58, 19, 58, 28, 58, 39, 58, 29, 58, 38, 58, 62, 58, 47, 58, 59, 58, 35, 58, 21, 58, 46, 58, 61, 58, 12, 58, 10, 58, 45, 58, 34, 58, 31, 58, 54, 58, 36, 58, 13, 58, 17, 58, 53, 58, 33, 58, 14, 58, 37, 58, 57, 58, 44, 58, 43, 58, 32, 58, 15, 58, 50, 58, 18, 58, 42, 58, 52, 58, 24, 58, 40, 58, 41, 58, 49, 58, 30, 58, 25, 58, 16, 58, 51, 58, 48, 58, 27, 58, 26, 58, 63, 58, 64, 58, 65, 58, 66, 58, 67, 58, 68, 58, 69, 58],
	#[65535, 44, 2, 44, 31, 44, 60, 44, 11, 44, 10, 44, 58, 44, 40, 44, 56, 44],
	#[23, 57, 28, 57, 18, 57, 34, 57, 58, 34, 40, 34, 33, 57, 20, 57, 29, 57, 31, 34, 60, 34, 22, 57, 10, 34, 2, 34, 21, 57, 11, 34, 56, 34],
	#[65535, 172, 11, 172, 2, 172, 58, 172, 10, 172, 60, 172, 40, 172, 31, 172, 56, 172],
	#[65535, 43, 31, 43, 60, 43, 40, 43, 58, 43, 11, 43, 10, 43, 56, 43, 2, 43],
	#[2, 49, 10, 49, 11, 49, 18, 59, 20, 59, 21, 59, 22, 59, 23, 59, 28, 59, 29, 59, 31, 49, 33, 59, 34, 59, 40, 49, 56, 49, 58, 49, 60, 49],
	#[65535, 45, 2, 45, 60, 45, 31, 45, 11, 45, 58, 45, 10, 45, 56, 45, 40, 45],
	#[65535, 173, 31, 173, 2, 173, 60, 173, 10, 173, 40, 173, 11, 173, 58, 173, 56, 173],
	#[29, -250, 25, -230, 20, -245, 23, -251, 24, -243, 21, -247, 22, -252, 34, -249],
	#[22, 169, 20, 169, 29, 169, 23, 169, 25, 169, 33, -240, 24, 169, 34, 169, 21, 169],
	#[65535, 166, 22, 166, 29, 166, 20, 166, 23, 166, 21, 166, 34, 166, 25, 166, 33, 166, 24, 166],
	#[65535, 168, 21, 168, 22, 168, 23, 168, 24, 168, 25, 168, 20, 168, 29, 168, 34, 168],
	#[65535, 170, 22, 170, 21, 170, 24, 170, 20, 170, 34, 170, 29, 170, 23, 170, 25, 170],
	#[65535, 62, 54, 62, 23, 62, 13, 62, 52, 62, 22, 62, 42, 62, 12, 62, 18, 62, 15, 62, 38, 62, 47, 62, 37, 62, 50, 62, 46, 62, 28, 62, 55, 62, 45, 62, 43, 62, 29, 62, 59, 62, 32, 62, 35, 62, 24, 62, 53, 62, 51, 62, 14, 62, 39, 62, 60, 62, 44, 62, 17, 62, 25, 62, 61, 62, 49, 62, 10, 62, 36, 62, 20, 62, 16, 62, 57, 62, 33, 62, 31, 62, 21, 62, 11, 62, 2, 62, 58, 62, 41, 62, 48, 62, 56, 62, 40, 62, 19, 62, 34, 62, 62, 62, 63, 62, 65, 62, 66, 62, 67, 62, 68, 62, 69, 62],
	#[31, -100, 30, -110, 27, -85, 26, -98, 19, -84, 22, -89, 29, -124, 20, -122, 23, -75, 24, -83, 18, -79, 33, -70, 28, -107, 25, -121, 32, -90, 21, -94],
	#[65535, 57, 55, 57, 20, 57, 41, 57, 21, 57, 19, 57, 34, 57, 39, 57, 38, 57, 62, 57, 47, 57, 59, 57, 37, 57, 40, 57, 25, 57, 22, 57, 46, 57, 61, 57, 12, 57, 10, 57, 45, 57, 43, 57, 30, 57, 54, 57, 36, 57, 35, 57, 17, 57, 53, 57, 51, 57, 32, 57, 27, 57, 57, 57, 44, 57, 28, 57, 26, 57, 15, 57, 24, 57, 42, 57, 52, 57, 29, 57, 33, 57, 23, 57, 49, 57, 50, 57, 18, 57, 31, 57, 14, 57, 16, 57, 48, 57, 13, 57, 63, 57, 64, 57, 65, 57, 66, 57, 67, 57, 68, 57, 69, 57],
	#[55, -158, 54, -179, 53, -192, 47, -188, 2, 100, 43, -172, 31, 100, 11, 100, 51, -196, 45, -164, 50, -161, 39, -175, 46, -194, 10, 100, 49, -180, 44, -166, 40, -217, 25, -55, 18, -62, 23, -67, 52, -193, 36, -170, 20, -35, 35, -176, 41, -168, 29, -16, 48, -181, 13, -92, 22, -10, 28, -3, 12, -77, 24, -54, 37, -157, 16, -102, 32, -165, 21, -53, 33, -56, 15, -80, 42, -183, 17, -96, 34, -32, 14, -82, 38, -154, 19, -51, 56, 100, 57, -186, 58, 100, 59, -169, 60, 100, 61, -156, 62, -191, 63, -182, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 60, 46, 60, 28, 60, 45, 60, 54, 60, 43, 60, 15, 60, 22, 60, 47, 60, 23, 60, 49, 60, 55, 60, 18, 60, 44, 60, 42, 60, 12, 60, 14, 60, 38, 60, 62, 60, 52, 60, 50, 60, 39, 60, 29, 60, 17, 60, 13, 60, 35, 60, 53, 60, 51, 60, 37, 60, 48, 60, 40, 60, 61, 60, 19, 60, 36, 60, 16, 60, 11, 60, 31, 60, 24, 60, 60, 60, 25, 60, 32, 60, 41, 60, 34, 60, 58, 60, 21, 60, 33, 60, 20, 60, 57, 60, 56, 60, 59, 60, 10, 60, 2, 60, 63, 60, 65, 60, 66, 60, 67, 60, 68, 60, 69, 60],
	#[61, -156, 46, -194, 31, 96, 41, -168, 57, -186, 59, -169, 30, -43, 54, -179, 39, -175, 51, -196, 50, -161, 32, -165, 52, -193, 38, -154, 62, -191, 47, -188, 48, -181, 37, -157, 22, -10, 44, -166, 28, -3, 55, -158, 45, -164, 43, -172, 19, -51, 26, -37, 36, -170, 35, -176, 53, -192, 33, -56, 14, -82, 27, -12, 34, -32, 40, -178, 21, -53, 12, -77, 15, -80, 24, -54, 42, -183, 10, -155, 29, -16, 20, -35, 23, -67, 49, -180, 17, -96, 18, -62, 16, -102, 25, -55, 13, -92, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 41, 29, 41, 23, 41, 31, 41, 27, 41, 28, 41, 19, 41, 22, 41, 24, 41, 20, 41, 30, 41, 18, 41, 25, 41, 33, 41, 32, 41, 26, 41, 21, 41],
	#[65535, 42, 28, 42, 22, 42, 18, 42, 19, 42, 29, 42, 23, 42, 20, 42, 31, 42, 30, 42, 26, 42, 24, 42, 33, 42, 21, 42, 27, 42, 32, 42, 25, 42],
	#[65535, 59, 62, 59, 21, 59, 34, 59, 20, 59, 53, 59, 35, 59, 38, 59, 36, 59, 61, 59, 39, 59, 44, 59, 15, 59, 17, 59, 42, 59, 10, 59, 37, 59, 23, 59, 19, 59, 59, 59, 55, 59, 14, 59, 57, 59, 45, 59, 51, 59, 31, 59, 33, 59, 41, 59, 40, 59, 16, 59, 46, 59, 50, 59, 25, 59, 52, 59, 32, 59, 48, 59, 30, 59, 54, 59, 27, 59, 26, 59, 47, 59, 18, 59, 12, 59, 29, 59, 13, 59, 49, 59, 22, 59, 24, 59, 28, 59, 43, 59, 63, 59, 64, 59, 65, 59, 66, 59, 67, 59, 68, 59, 69, 59],
	#[65535, 61, 50, 61, 23, 61, 55, 61, 54, 61, 14, 61, 18, 61, 42, 61, 43, 61, 45, 61, 22, 61, 29, 61, 44, 61, 15, 61, 13, 61, 47, 61, 37, 61, 52, 61, 36, 61, 59, 61, 61, 61, 35, 61, 62, 61, 31, 61, 28, 61, 12, 61, 49, 61, 16, 61, 46, 61, 53, 61, 39, 61, 10, 61, 11, 61, 51, 61, 41, 61, 24, 61, 57, 61, 19, 61, 38, 61, 33, 61, 34, 61, 2, 61, 58, 61, 20, 61, 21, 61, 40, 61, 56, 61, 32, 61, 48, 61, 25, 61, 60, 61, 17, 61, 63, 61, 65, 61, 66, 61, 67, 61, 68, 61, 69, 61],
	#[31, -227],
	#[65535, 163, 11, 163, 10, 163, 60, 163, 2, 163, 56, 163, 58, 163, 31, 163],
	#[65535, 113, 60, 113, 31, 113, 2, 113, 56, 113, 58, 113],
	#[61, -156, 54, -179, 52, -193, 55, -158, 62, -191, 60, 96, 2, 96, 58, 96, 22, -10, 46, -194, 53, -192, 39, -175, 42, -183, 32, -165, 30, -43, 15, -80, 27, -12, 47, -188, 57, -186, 38, -154, 23, -67, 41, -168, 49, -180, 51, -196, 36, -170, 20, -35, 37, -157, 31, 96, 43, -172, 10, -155, 59, -169, 44, -166, 35, -176, 12, -77, 26, -37, 40, -178, 50, -161, 18, -62, 14, -82, 33, -56, 34, -32, 48, -181, 56, 96, 21, -53, 28, -3, 16, -102, 45, -164, 19, -51, 13, -92, 17, -96, 25, -55, 24, -54, 29, -16, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 116, 31, 116, 2, 116, 56, 116, 58, 116, 60, 116],
	#[65535, 111, 31, 111, 58, 111, 60, 111, 56, 111, 2, 111],
	#[65535, 117, 31, 117, 58, 117, 60, 117, 2, 117, 56, 117],
	#[31, -261],
	#[55, 46, 29, -16, 39, 46, 28, -3, 38, 46, 10, 46, 11, 46, 36, 46, 20, 46, 46, 46, 21, 46, 19, 46, 60, 46, 34, 46, 30, 46, 41, 46, 40, 46, 57, 46, 48, 46, 43, 46, 62, 46, 47, 46, 59, 46, 37, 46, 35, 46, 25, 46, 23, -67, 44, 46, 61, 46, 12, 46, 56, 46, 45, 46, 31, 46, 54, 46, 32, 46, 13, 46, 17, 46, 53, 46, 42, 46, 49, 46, 2, 46, 58, 46, 26, 46, 15, 46, 24, 46, 22, -10, 52, 46, 51, 46, 33, 46, 50, 46, 18, -62, 14, 46, 16, 46, 27, 46, 63, 46, 64, 46, 65, 46, 66, 46, 67, 46, 68, 46],
	#[65535, 233, 55, 233, 14, 233, 46, 233, 39, 233, 12, 233, 38, 233, 10, 233, 35, 233, 13, 233, 11, 233, 57, 233, 20, 233, 37, 233, 19, 233, 61, 233, 34, 233, 30, 233, 54, 233, 24, 233, 56, 233, 48, 233, 52, 233, 40, 233, 62, 233, 47, 233, 59, 233, 17, 233, 25, 233, 21, 233, 41, 233, 49, 233, 42, 233, 16, 233, 45, 233, 31, 233, 26, 233, 36, 233, 50, 233, 53, 233, 32, 233, 27, 233, 60, 233, 44, 233, 43, 233, 15, 233, 51, 233, 33, 233, 2, 233, 58, 233, 63, 233, 64, 233, 65, 233, 66, 233, 67, 233, 68, 233],
	#[65535, 236, 54, 236, 58, 236, 47, 236, 45, 236, 14, 236, 52, 236, 26, 236, 31, 236, 42, 236, 60, 236, 30, 236, 12, 236, 39, 236, 51, 236, 50, 236, 27, 236, 15, 236, 38, 236, 62, 236, 19, 236, 37, 236, 35, 236, 32, 236, 46, 236, 57, 236, 17, 236, 43, 236, 2, 236, 41, 236, 59, 236, 20, 236, 24, 236, 53, 236, 16, 236, 10, 236, 44, 236, 21, 236, 25, 236, 61, 236, 49, 236, 36, 236, 34, 236, 13, 236, 55, 236, 56, 236, 48, 236, 33, 236, 40, 236, 11, 236, 63, 236, 64, 236, 65, 236, 66, 236, 67, 236, 68, 236],
	#[65535, 237, 45, 237, 60, 237, 2, 237, 52, 237, 42, 237, 21, 237, 54, 237, 58, 237, 26, 237, 47, 237, 43, 237, 14, 237, 38, 237, 62, 237, 13, 237, 49, 237, 53, 237, 36, 237, 27, 237, 31, 237, 41, 237, 19, 237, 61, 237, 44, 237, 30, 237, 12, 237, 11, 237, 51, 237, 50, 237, 32, 237, 15, 237, 10, 237, 20, 237, 34, 237, 37, 237, 35, 237, 16, 237, 46, 237, 55, 237, 17, 237, 33, 237, 59, 237, 57, 237, 24, 237, 56, 237, 39, 237, 40, 237, 25, 237, 48, 237, 63, 237, 64, 237, 65, 237, 66, 237, 67, 237, 68, 237],
	#[65535, 47, 47, 47, 60, 47, 54, 47, 12, 47, 31, 47, 42, 47, 43, 47, 45, 47, 2, 47, 26, 47, 15, 47, 13, 47, 53, 47, 30, 47, 37, 47, 52, 47, 58, 47, 21, 47, 50, 47, 14, 47, 62, 47, 51, 47, 44, 47, 27, 47, 24, 47, 49, 47, 16, 47, 46, 47, 19, 47, 25, 47, 39, 47, 32, 47, 17, 47, 61, 47, 33, 47, 48, 47, 56, 47, 59, 47, 40, 47, 11, 47, 36, 47, 20, 47, 41, 47, 57, 47, 35, 47, 38, 47, 34, 47, 55, 47, 10, 47, 63, 47, 64, 47, 65, 47, 66, 47, 67, 47, 68, 47],
	#[60, -267],
	#[65535, 148, 47, 148, 23, 148, 30, 148, 28, 148, 14, 148, 27, 148, 45, 148, 50, 148, 43, 148, 22, 148, 46, 148, 15, 148, 44, 148, 51, 148, 37, 148, 54, 148, 42, 148, 52, 148, 36, 148, 38, 148, 35, 148, 16, 148, 29, 148, 31, 148, 18, 148, 13, 148, 25, 148, 53, 148, 48, 148, 49, 148, 39, 148, 26, 148, 21, 148, 33, 148, 11, 148, 20, 148, 32, 148, 10, 148, 17, 148, 34, 148, 2, 148, 41, 148, 19, 148, 24, 148, 12, 148, 55, 148, 40, 148, 56, 148, 57, 148, 58, 148, 59, 148, 60, 148, 61, 148, 62, 148, 63, 148, 64, 148, 65, 148, 66, 148, 67, 148, 68, 148, 69, 148],
	#[45, -271, 62, -191, 59, -141, 30, -43, 47, -188, 43, -280, 38, -154, 23, -67, 55, -269, 57, -288, 22, -10, 31, 256, 41, -168, 42, -285, 19, -51, 61, -156, 44, -276, 35, -176, 54, -179, 39, -175, 49, -180, 50, -161, 52, -193, 20, -35, 48, -181, 37, -157, 21, -53, 46, -194, 28, -3, 27, -12, 10, -268, 32, -165, 34, -32, 24, -54, 29, -16, 26, -37, 36, -170, 53, -192, 51, -196, 14, -82, 40, -284, 12, -77, 33, -56, 18, -62, 13, -92, 17, -96, 16, -102, 25, -55, 15, -80, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[31, -100, 61, -72, 28, -107, 39, -119, 51, -133, 38, -127, 47, -120, 37, -74, 50, -81, 46, -130, 57, -123, 10, -71, 45, -78, 43, -101, 30, -110, 52, -128, 36, -88, 35, -105, 40, -109, 42, -114, 14, -82, 27, -85, 62, -131, 44, -91, 21, -94, 33, -70, 59, -97, 22, -89, 48, -104, 29, -124, 20, -122, 23, -75, 26, -98, 55, -76, 56, 300, 32, -90, 12, -77, 16, -102, 17, -96, 25, -121, 24, -83, 41, -95, 18, -79, 13, -92, 15, -80, 19, -84, 49, 343, 53, -108, 63, -112, 64, -126, 65, -113, 66, -132, 67, -115, 68, -118, 69, -99],
	#[31, 275, 10, -306],
	#[65535, 295, 55, 295, 18, 295, 42, 295, 43, 295, 45, 295, 22, 295, 29, 295, 44, 295, 15, 295, 13, 295, 47, 295, 30, 295, 54, 295, 52, 295, 23, 295, 26, 295, 61, 295, 14, 295, 38, 295, 62, 295, 31, 295, 27, 295, 12, 295, 16, 295, 46, 295, 59, 295, 39, 295, 35, 295, 51, 295, 17, 295, 33, 295, 48, 295, 24, 295, 28, 295, 19, 295, 10, 295, 53, 295, 36, 295, 20, 295, 50, 295, 32, 295, 49, 295, 34, 295, 41, 295, 57, 295, 25, 295, 37, 295, 21, 295, 40, 295, 63, 295, 64, 295, 65, 295, 66, 295, 67, 295, 68, 295, 69, 295],
	#[65535, 298, 52, 298, 23, 298, 38, 298, 45, 298, 29, 298, 22, 298, 46, 298, 18, 298, 39, 298, 42, 298, 30, 298, 54, 298, 47, 298, 44, 298, 28, 298, 14, 298, 62, 298, 35, 298, 13, 298, 55, 298, 53, 298, 26, 298, 20, 298, 50, 298, 31, 298, 43, 298, 49, 298, 61, 298, 34, 298, 12, 298, 41, 298, 51, 298, 16, 298, 27, 298, 48, 298, 15, 298, 59, 298, 37, 298, 25, 298, 32, 298, 57, 298, 10, 298, 17, 298, 24, 298, 36, 298, 40, 298, 21, 298, 33, 298, 19, 298, 63, 298, 64, 298, 65, 298, 66, 298, 67, 298, 68, 298, 69, 298],
	#[65535, 288, 23, 288, 45, 288, 28, 288, 22, 288, 47, 288, 27, 288, 30, 288, 50, 288, 37, 288, 42, 288, 14, 288, 26, 288, 52, 288, 36, 288, 43, 288, 29, 288, 46, 288, 15, 288, 53, 288, 44, 288, 51, 288, 54, 288, 48, 288, 49, 288, 32, 288, 38, 288, 35, 288, 16, 288, 31, 288, 18, 288, 13, 288, 25, 288, 12, 288, 34, 288, 39, 288, 17, 288, 21, 288, 20, 288, 10, 288, 24, 288, 40, 288, 33, 288, 41, 288, 19, 288, 55, 288, 57, 288, 59, 288, 61, 288, 62, 288, 63, 288, 64, 288, 65, 288, 66, 288, 67, 288, 68, 288, 69, 288],
	#[42, -285, 55, -269, 54, -179, 31, 256, 61, -156, 43, -280, 45, -271, 19, -51, 29, -16, 44, -276, 53, -192, 27, -12, 30, -43, 37, -157, 52, -193, 23, -67, 20, -35, 59, -141, 50, -161, 38, -154, 62, -191, 10, -268, 25, -55, 28, -3, 41, -168, 40, -284, 22, -10, 46, -194, 18, -62, 39, -175, 35, -176, 57, -288, 34, -32, 15, -80, 33, -56, 47, -188, 49, -180, 14, -82, 51, -196, 36, -170, 32, -165, 48, -181, 17, -96, 26, -37, 16, -102, 13, -92, 24, -54, 21, -53, 12, -77, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[45, -271, 30, -43, 43, -280, 38, -154, 62, -191, 55, -269, 31, 268, 41, -168, 59, -141, 44, -276, 35, -176, 54, -179, 39, -175, 51, -196, 52, -193, 47, -188, 57, -288, 37, -157, 50, -161, 46, -194, 61, -156, 42, -285, 10, -268, 32, -165, 34, -32, 19, -51, 26, -37, 36, -170, 13, -92, 24, -54, 53, -192, 14, -82, 27, -12, 40, -284, 21, -53, 12, -77, 15, -80, 49, -180, 48, -181, 20, -35, 17, -96, 16, -102, 25, -55, 33, -56, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25],
	#[65535, 294, 29, 294, 47, 294, 45, 294, 28, 294, 14, 294, 38, 294, 23, 294, 52, 294, 26, 294, 22, 294, 46, 294, 18, 294, 43, 294, 30, 294, 54, 294, 39, 294, 51, 294, 50, 294, 27, 294, 15, 294, 62, 294, 37, 294, 32, 294, 41, 294, 61, 294, 42, 294, 17, 294, 48, 294, 24, 294, 31, 294, 59, 294, 57, 294, 35, 294, 53, 294, 16, 294, 10, 294, 44, 294, 21, 294, 12, 294, 33, 294, 49, 294, 36, 294, 20, 294, 13, 294, 55, 294, 40, 294, 19, 294, 25, 294, 34, 294, 63, 294, 64, 294, 65, 294, 66, 294, 67, 294, 68, 294, 69, 294],
	#[65535, 290, 14, 290, 38, 290, 52, 290, 22, 290, 30, 290, 54, 290, 29, 290, 27, 290, 15, 290, 62, 290, 47, 290, 37, 290, 26, 290, 23, 290, 46, 290, 28, 290, 42, 290, 45, 290, 43, 290, 31, 290, 59, 290, 50, 290, 18, 290, 53, 290, 51, 290, 41, 290, 39, 290, 44, 290, 21, 290, 12, 290, 61, 290, 49, 290, 35, 290, 32, 290, 10, 290, 36, 290, 20, 290, 13, 290, 48, 290, 55, 290, 17, 290, 19, 290, 40, 290, 25, 290, 24, 290, 16, 290, 34, 290, 57, 290, 33, 290, 63, 290, 64, 290, 65, 290, 66, 290, 67, 290, 68, 290, 69, 290],
	#[65535, 289, 29, 289, 30, 289, 47, 289, 45, 289, 28, 289, 14, 289, 38, 289, 23, 289, 13, 289, 52, 289, 26, 289, 22, 289, 46, 289, 18, 289, 43, 289, 42, 289, 35, 289, 54, 289, 39, 289, 51, 289, 50, 289, 27, 289, 15, 289, 62, 289, 49, 289, 37, 289, 32, 289, 41, 289, 61, 289, 55, 289, 16, 289, 17, 289, 48, 289, 31, 289, 59, 289, 57, 289, 20, 289, 24, 289, 53, 289, 10, 289, 44, 289, 21, 289, 12, 289, 33, 289, 36, 289, 34, 289, 40, 289, 19, 289, 25, 289, 63, 289, 64, 289, 65, 289, 66, 289, 67, 289, 68, 289, 69, 289],
	#[31, -261],
	#[65535, 293, 23, 293, 46, 293, 45, 293, 54, 293, 43, 293, 15, 293, 22, 293, 47, 293, 51, 293, 27, 293, 30, 293, 55, 293, 18, 293, 28, 293, 14, 293, 26, 293, 52, 293, 50, 293, 39, 293, 29, 293, 19, 293, 35, 293, 53, 293, 44, 293, 12, 293, 25, 293, 48, 293, 42, 293, 16, 293, 34, 293, 38, 293, 49, 293, 31, 293, 24, 293, 13, 293, 10, 293, 37, 293, 17, 293, 21, 293, 36, 293, 20, 293, 32, 293, 33, 293, 41, 293, 40, 293, 57, 293, 59, 293, 61, 293, 62, 293, 63, 293, 64, 293, 65, 293, 66, 293, 67, 293, 68, 293, 69, 293],
	#[65535, 287, 54, 287, 28, 287, 14, 287, 23, 287, 13, 287, 52, 287, 22, 287, 46, 287, 18, 287, 42, 287, 30, 287, 12, 287, 29, 287, 27, 287, 15, 287, 38, 287, 47, 287, 37, 287, 26, 287, 50, 287, 51, 287, 49, 287, 55, 287, 45, 287, 43, 287, 19, 287, 16, 287, 20, 287, 53, 287, 10, 287, 39, 287, 44, 287, 17, 287, 25, 287, 24, 287, 35, 287, 32, 287, 36, 287, 34, 287, 41, 287, 31, 287, 21, 287, 33, 287, 48, 287, 40, 287, 57, 287, 59, 287, 61, 287, 62, 287, 63, 287, 64, 287, 65, 287, 66, 287, 67, 287, 68, 287, 69, 287],
	#[31, 275, 10, -306],
	#[65535, 286, 22, 286, 51, 286, 10, 286, 12, 286, 13, 286, 14, 286, 15, 286, 16, 286, 17, 286, 18, 286, 19, 286, 20, 286, 21, 286, 35, 286, 23, 286, 24, 286, 25, 286, 26, 286, 27, 286, 28, 286, 29, 286, 30, 286, 31, 286, 32, 286, 33, 286, 34, 286, 36, 286, 37, 286, 38, 286, 39, 286, 40, 286, 41, 286, 42, 286, 43, 286, 44, 286, 45, 286, 46, 286, 47, 286, 48, 286, 49, 286, 50, 286, 52, 286, 53, 286, 54, 286, 55, 286, 57, 286, 59, 286, 61, 286, 62, 286, 63, 286, 64, 286, 65, 286, 66, 286, 67, 286, 68, 286, 69, 286],
	#[38, -154, 55, -269, 22, -10, 50, -161, 31, 256, 43, -280, 42, -285, 44, -276, 30, -43, 54, -179, 39, -175, 51, -196, 15, -80, 36, -170, 41, -168, 47, -188, 48, -181, 37, -157, 21, -53, 46, -194, 28, -3, 40, -284, 10, -268, 45, -271, 34, -32, 19, -51, 52, -193, 35, -176, 18, -62, 53, -192, 33, -56, 14, -82, 27, -12, 17, -96, 26, -37, 24, -54, 16, -102, 32, -165, 29, -16, 20, -35, 49, -180, 12, -77, 25, -55, 13, -92, 23, -67, 57, -288, 59, -141, 61, -156, 62, -191, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 292, 23, 292, 46, 292, 45, 292, 54, 292, 43, 292, 15, 292, 22, 292, 47, 292, 51, 292, 27, 292, 30, 292, 55, 292, 50, 292, 18, 292, 28, 292, 14, 292, 26, 292, 52, 292, 36, 292, 39, 292, 29, 292, 19, 292, 35, 292, 53, 292, 44, 292, 12, 292, 25, 292, 48, 292, 42, 292, 16, 292, 34, 292, 38, 292, 49, 292, 31, 292, 24, 292, 13, 292, 10, 292, 37, 292, 17, 292, 21, 292, 40, 292, 20, 292, 32, 292, 33, 292, 41, 292, 57, 292, 59, 292, 61, 292, 62, 292, 63, 292, 64, 292, 65, 292, 66, 292, 67, 292, 68, 292, 69, 292],
	#[65535, 257, 31, 257],
	#[65535, 291, 22, 291, 38, 291, 23, 291, 46, 291, 28, 291, 45, 291, 54, 291, 18, 291, 14, 291, 39, 291, 43, 291, 12, 291, 15, 291, 49, 291, 47, 291, 29, 291, 27, 291, 13, 291, 30, 291, 55, 291, 50, 291, 16, 291, 44, 291, 42, 291, 31, 291, 21, 291, 17, 291, 26, 291, 52, 291, 36, 291, 20, 291, 48, 291, 24, 291, 33, 291, 35, 291, 53, 291, 51, 291, 37, 291, 32, 291, 40, 291, 10, 291, 19, 291, 34, 291, 41, 291, 25, 291, 57, 291, 59, 291, 61, 291, 62, 291, 63, 291, 64, 291, 65, 291, 66, 291, 67, 291, 68, 291, 69, 291],
	#[22, -89, 52, -128, 51, -133, 50, -81, 38, -127, 23, -75, 35, -105, 55, -76, 53, -108, 20, -122, 46, -130, 31, -100, 43, -101, 49, 343, 30, -110, 39, -119, 40, -109, 29, -124, 42, -114, 36, -88, 47, -120, 37, -74, 26, -98, 21, -94, 18, -79, 28, -107, 12, -77, 10, -71, 45, -78, 48, -104, 33, -70, 19, -84, 13, -92, 17, -96, 25, -121, 16, -102, 14, -82, 27, -85, 44, -91, 15, -80, 41, -95, 24, -83, 32, -90, 57, -123, 58, 300, 59, -97, 61, -72, 62, -131, 63, -112, 64, -126, 65, -113, 66, -132, 67, -115, 68, -118, 69, -99],
	#[65535, 285, 23, 285, 22, 285, 46, 285, 30, 285, 45, 285, 29, 285, 18, 285, 52, 285, 38, 285, 47, 285, 37, 285, 50, 285, 51, 285, 28, 285, 27, 285, 17, 285, 43, 285, 31, 285, 54, 285, 20, 285, 24, 285, 53, 285, 16, 285, 14, 285, 39, 285, 44, 285, 21, 285, 12, 285, 15, 285, 41, 285, 49, 285, 35, 285, 42, 285, 10, 285, 36, 285, 13, 285, 26, 285, 55, 285, 40, 285, 19, 285, 33, 285, 25, 285, 48, 285, 34, 285, 32, 285, 57, 285, 59, 285, 61, 285, 62, 285, 63, 285, 64, 285, 65, 285, 66, 285, 67, 285, 68, 285, 69, 285],
	#[65535, 258, 31, 258],
	#[58, -303],
	#[56, -302],
	#[65535, 345, 49, 345],
	#[60, -301],
	#[60, -300],
	#[65535, 338, 21, 338, 46, 338, 31, 338, 28, 338, 22, 338, 47, 338, 20, 338, 30, 338, 55, 338, 19, 338, 37, 338, 41, 338, 35, 338, 14, 338, 38, 338, 32, 338, 26, 338, 52, 338, 36, 338, 39, 338, 29, 338, 25, 338, 15, 338, 53, 338, 51, 338, 12, 338, 27, 338, 33, 338, 23, 338, 10, 338, 45, 338, 16, 338, 50, 338, 18, 338, 13, 338, 44, 338, 43, 338, 24, 338, 40, 338, 17, 338, 48, 338, 49, 338, 42, 338, 56, 338, 57, 338, 58, 338, 59, 338, 60, 338, 61, 338, 62, 338, 63, 338, 64, 338, 65, 338, 66, 338, 67, 338, 68, 338, 69, 338],
	#[65535, 337, 31, 337, 36, 337, 46, 337, 18, 337, 21, 337, 19, 337, 60, 337, 30, 337, 58, 337, 26, 337, 51, 337, 27, 337, 38, 337, 20, 337, 47, 337, 37, 337, 35, 337, 56, 337, 23, 337, 28, 337, 55, 337, 10, 337, 32, 337, 25, 337, 29, 337, 59, 337, 50, 337, 17, 337, 53, 337, 42, 337, 14, 337, 39, 337, 24, 337, 12, 337, 61, 337, 22, 337, 45, 337, 52, 337, 40, 337, 41, 337, 48, 337, 43, 337, 44, 337, 49, 337, 16, 337, 33, 337, 13, 337, 15, 337, 57, 337, 62, 337, 63, 337, 64, 337, 65, 337, 66, 337, 67, 337, 68, 337, 69, 337],
	#[56, -299],
	#[65535, 320, 47, 320, 46, 320, 39, 320, 44, 320, 43, 320, 12, 320, 15, 320, 49, 320, 35, 320, 10, 320, 51, 320, 23, 320, 13, 320, 53, 320, 16, 320, 37, 320, 42, 320, 31, 320, 14, 320, 38, 320, 45, 320, 36, 320, 22, 320, 17, 320, 40, 320, 33, 320, 41, 320, 48, 320, 28, 320, 30, 320, 25, 320, 18, 320, 21, 320, 20, 320, 26, 320, 29, 320, 24, 320, 19, 320, 32, 320, 27, 320, 50, 320, 52, 320, 55, 320, 56, 320, 57, 320, 58, 320, 59, 320, 60, 320, 61, 320, 62, 320, 63, 320, 64, 320, 65, 320, 66, 320, 67, 320, 68, 320, 69, 320],
	#[65535, 322, 15, 322, 55, 322, 59, 322, 14, 322, 51, 322, 45, 322, 39, 322, 46, 322, 10, 322, 13, 322, 35, 322, 53, 322, 44, 322, 47, 322, 50, 322, 48, 322, 43, 322, 61, 322, 52, 322, 36, 322, 38, 322, 62, 322, 31, 322, 60, 322, 16, 322, 28, 322, 12, 322, 49, 322, 37, 322, 58, 322, 17, 322, 21, 322, 30, 322, 41, 322, 20, 322, 42, 322, 27, 322, 40, 322, 29, 322, 26, 322, 24, 322, 18, 322, 25, 322, 33, 322, 22, 322, 56, 322, 32, 322, 19, 322, 23, 322, 57, 322, 63, 322, 64, 322, 65, 322, 66, 322, 67, 322, 68, 322, 69, 322],
	#[65535, 308, 42, 308, 49, 308, 59, 308, 55, 308, 52, 308, 22, 308, 46, 308, 43, 308, 21, 308, 61, 308, 45, 308, 39, 308, 51, 308, 50, 308, 15, 308, 36, 308, 62, 308, 47, 308, 37, 308, 60, 308, 12, 308, 17, 308, 48, 308, 29, 308, 26, 308, 13, 308, 24, 308, 53, 308, 16, 308, 14, 308, 27, 308, 44, 308, 58, 308, 25, 308, 19, 308, 18, 308, 35, 308, 10, 308, 20, 308, 23, 308, 30, 308, 33, 308, 28, 308, 31, 308, 38, 308, 40, 308, 56, 308, 41, 308, 32, 308, 57, 308, 63, 308, 64, 308, 65, 308, 66, 308, 67, 308, 68, 308, 69, 308],
	#[65535, 306, 12, 306, 15, 306, 46, 306, 43, 306, 42, 306, 31, 306, 14, 306, 38, 306, 51, 306, 45, 306, 39, 306, 17, 306, 13, 306, 35, 306, 48, 306, 44, 306, 47, 306, 37, 306, 25, 306, 33, 306, 23, 306, 10, 306, 36, 306, 20, 306, 16, 306, 29, 306, 40, 306, 22, 306, 28, 306, 41, 306, 24, 306, 19, 306, 26, 306, 21, 306, 30, 306, 18, 306, 32, 306, 27, 306, 49, 306, 50, 306, 52, 306, 53, 306, 55, 306, 56, 306, 57, 306, 58, 306, 59, 306, 60, 306, 61, 306, 62, 306, 63, 306, 64, 306, 65, 306, 66, 306, 67, 306, 68, 306, 69, 306],
	#[65535, 297, 23, 297, 47, 297, 27, 297, 54, 297, 28, 297, 14, 297, 26, 297, 45, 297, 50, 297, 43, 297, 22, 297, 46, 297, 15, 297, 44, 297, 51, 297, 30, 297, 25, 297, 48, 297, 42, 297, 36, 297, 38, 297, 35, 297, 16, 297, 29, 297, 31, 297, 18, 297, 13, 297, 53, 297, 12, 297, 49, 297, 37, 297, 39, 297, 17, 297, 21, 297, 52, 297, 20, 297, 32, 297, 10, 297, 24, 297, 34, 297, 33, 297, 41, 297, 19, 297, 55, 297, 40, 297, 57, 297, 59, 297, 61, 297, 62, 297, 63, 297, 64, 297, 65, 297, 66, 297, 67, 297, 68, 297, 69, 297],
	#[65535, 273, 31, 273],
	#[65535, 276, 31, 276],
	#[59, -141, 31, 256, 61, -156, 44, -276, 28, -3, 39, -175, 57, -288, 38, -154, 62, -191, 47, -188, 46, -194, 55, -269, 10, -268, 45, -271, 43, -280, 30, -43, 54, -179, 36, -170, 35, -176, 42, -285, 41, -168, 27, -12, 34, -32, 40, -284, 21, -53, 32, -165, 48, -181, 50, -161, 22, -10, 17, -96, 29, -16, 20, -35, 23, -67, 26, -37, 24, -54, 18, -62, 37, -157, 12, -77, 14, -82, 16, -102, 25, -55, 33, -56, 52, -193, 13, -92, 15, -80, 19, -51, 49, -180, 51, -196, 53, -192, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 271, 31, 271],
	#[65535, 277, 31, 277],
	#[65535, 259, 31, 259],
	#[65535, 269, 31, 269],
	#[65535, 272, 31, 272],
	#[65535, 270, 31, 270],
	#[56, -314],
	#[65535, 296, 47, 296, 23, 296, 28, 296, 45, 296, 43, 296, 12, 296, 15, 296, 22, 296, 35, 296, 27, 296, 30, 296, 54, 296, 50, 296, 42, 296, 31, 296, 14, 296, 26, 296, 52, 296, 36, 296, 39, 296, 29, 296, 46, 296, 16, 296, 34, 296, 53, 296, 44, 296, 51, 296, 37, 296, 48, 296, 21, 296, 49, 296, 19, 296, 38, 296, 33, 296, 41, 296, 18, 296, 13, 296, 25, 296, 10, 296, 40, 296, 17, 296, 20, 296, 32, 296, 24, 296, 55, 296, 57, 296, 59, 296, 61, 296, 62, 296, 63, 296, 64, 296, 65, 296, 66, 296, 67, 296, 68, 296, 69, 296],
	#[65535, 274, 31, 274],
	#[65535, 234, 11, 234, 39, 234, 38, 234, 21, 234, 46, 234, 55, 234, 19, 234, 54, 234, 36, 234, 35, 234, 53, 234, 37, 234, 34, 234, 43, 234, 12, 234, 15, 234, 24, 234, 40, 234, 47, 234, 10, 234, 51, 234, 20, 234, 49, 234, 33, 234, 41, 234, 31, 234, 14, 234, 16, 234, 27, 234, 52, 234, 48, 234, 17, 234, 25, 234, 13, 234, 32, 234, 26, 234, 30, 234, 45, 234, 44, 234, 2, 234, 42, 234, 50, 234, 56, 234, 57, 234, 58, 234, 59, 234, 60, 234, 61, 234, 62, 234, 63, 234, 64, 234, 65, 234, 66, 234, 67, 234, 68, 234],
	#[65535, 109, 31, 109, 2, 109, 56, 109, 58, 109, 60, 109],
	#[65535, 99, 2, 99, 31, 99, 56, 99, 58, 99, 60, 99],
	#[65535, 110, 31, 110, 2, 110, 56, 110, 58, 110, 60, 110],
	#[56, -321],
	#[65535, 146, 47, 146, 45, 146, 28, 146, 14, 146, 23, 146, 52, 146, 22, 146, 46, 146, 18, 146, 43, 146, 42, 146, 44, 146, 30, 146, 12, 146, 29, 146, 27, 146, 15, 146, 38, 146, 53, 146, 19, 146, 48, 146, 37, 146, 26, 146, 50, 146, 49, 146, 10, 146, 17, 146, 25, 146, 2, 146, 31, 146, 54, 146, 36, 146, 13, 146, 24, 146, 11, 146, 16, 146, 39, 146, 21, 146, 35, 146, 32, 146, 20, 146, 55, 146, 41, 146, 33, 146, 51, 146, 34, 146, 40, 146, 56, 146, 57, 146, 58, 146, 59, 146, 60, 146, 61, 146, 62, 146, 63, 146, 64, 146, 65, 146, 66, 146, 67, 146, 68, 146, 69, 146],
	#[65535, 114, 31, 114, 2, 114, 56, 114, 58, 114, 60, 114],
	#[65535, 164, 31, 164, 10, 164, 2, 164, 11, 164, 56, 164, 58, 164, 60, 164],
	#[65535, 120, 11, 120, 2, 120, 31, 120, 10, 120, 56, 120, 58, 120, 60, 120],
	#[65535, 121, 10, 121, 31, 121, 2, 121, 11, 121, 56, 121, 58, 121, 60, 121],
	#[65535, 119, 31, 119, 2, 119, 10, 119, 11, 119, 56, 119, 58, 119, 60, 119],
	#[65535, 103, 2, 103, 11, 103, 31, 103, 10, 103, 56, 103, 58, 103, 60, 103],
	#[55, -269, 54, -179, 31, 256, 42, -285, 43, -280, 45, -271, 22, -10, 53, -192, 10, -268, 30, -43, 37, -157, 52, -193, 36, -170, 40, -284, 50, -161, 14, -82, 38, -154, 62, -191, 51, -196, 44, -276, 29, -16, 41, -168, 49, -180, 13, -92, 46, -194, 17, -96, 59, -141, 39, -175, 35, -176, 20, -35, 57, -288, 34, -32, 15, -80, 27, -12, 32, -165, 47, -188, 18, -62, 12, -77, 23, -67, 24, -54, 26, -37, 25, -55, 33, -56, 48, -181, 19, -51, 61, -156, 16, -102, 28, -3, 21, -53, 63, -182, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[31, -330],
	#[23, -67, 22, -10, 28, -3, 29, -335, 60, 354, 18, -62, 31, 354, 20, -35, 10, 354, 2, 354, 21, -53, 11, 354, 56, 354, 33, -56, 58, 354, 34, -334],
	#[65535, 165, 31, 165, 10, 165, 60, 165, 2, 165, 56, 165, 58, 165, 11, 165],
	#[22, -10, 23, -67, 28, -3, 33, -56, 29, -16, 20, -35, 34, -32, 21, -53, 18, -62],
	#[65535, 355, 56, 355, 58, 355, 60, 355, 11, 355, 2, 355, 10, 355, 31, 355],
	#[28, 41, 20, 41, 22, 41, 23, 41, 18, 41, 29, 41, 2, 38, 11, 38, 10, 38, 58, 38, 31, 38, 33, 41, 21, 41, 34, 41, 60, 38, 56, 38],
	#[31, 54, 10, 54, 20, 42, 21, 42, 11, 54, 28, 42, 2, 54, 18, 42, 60, 54, 58, 54, 56, 54, 22, 42, 29, 42, 34, 42, 23, 42, 33, 42],
	#[65535, 356, 2, 356, 31, 356, 11, 356, 10, 356, 56, 356, 58, 356, 60, 356],
	#[31, -227],
	#[65535, 161, 31, 161, 10, 161, 2, 161, 11, 161, 40, 161, 56, 161, 58, 161, 60, 161],
	#[23, -67, 28, -3, 29, -16, 18, -62, 53, -39, 22, -10, 20, -35, 21, -53, 33, -56, 34, -32],
	#[65535, 158, 10, 158, 11, 158, 31, 158, 2, 158, 56, 158, 58, 158, 60, 158],
	#[31, -227],
	#[65535, 160, 31, 160, 10, 160, 11, 160, 40, 160, 2, 160, 56, 160, 58, 160, 60, 160],
	#[65535, 147, 12, 147, 52, 147, 54, 147, 45, 147, 28, 147, 14, 147, 29, 147, 23, 147, 13, 147, 55, 147, 53, 147, 22, 147, 46, 147, 18, 147, 43, 147, 42, 147, 44, 147, 30, 147, 26, 147, 51, 147, 27, 147, 15, 147, 38, 147, 41, 147, 47, 147, 2, 147, 37, 147, 35, 147, 50, 147, 49, 147, 40, 147, 10, 147, 17, 147, 34, 147, 31, 147, 20, 147, 24, 147, 11, 147, 16, 147, 39, 147, 21, 147, 25, 147, 19, 147, 32, 147, 36, 147, 33, 147, 48, 147, 56, 147, 57, 147, 58, 147, 59, 147, 60, 147, 61, 147, 62, 147, 63, 147, 64, 147, 65, 147, 66, 147, 67, 147, 68, 147, 69, 147],
	#[65535, 184, 21, 184, 47, 184, 20, 184, 41, 184, 60, 184, 54, 184, 53, 184, 2, 184, 58, 184, 31, 184, 33, 184, 52, 184, 27, 184, 61, 184, 17, 184, 34, 184, 19, 184, 59, 184, 29, 184, 35, 184, 15, 184, 32, 184, 42, 184, 16, 184, 37, 184, 43, 184, 23, 184, 25, 184, 55, 184, 14, 184, 38, 184, 45, 184, 51, 184, 44, 184, 12, 184, 49, 184, 22, 184, 46, 184, 50, 184, 18, 184, 39, 184, 36, 184, 10, 184, 30, 184, 26, 184, 48, 184, 57, 184, 24, 184, 13, 184, 56, 184, 40, 184, 28, 184, 11, 184, 62, 184, 63, 184, 64, 184, 65, 184, 66, 184, 67, 184, 68, 184, 69, 184],
	#[11, 232, 37, 232, 38, 232, 2, 232, 58, 232, 46, -352, 39, 232, 36, 232, 10, 232, 57, -350, 56, 232, 55, -351, 48, 232, 40, 232],
	#[56, -347],
	#[65535, 187, 55, 187, 36, 187, 39, 187, 2, 187, 46, 187, 37, 187, 48, 187, 38, 187, 40, 187, 58, 187, 57, 187, 11, 187, 10, 187, 56, 187],
	#[53, -39, 23, -67, 13, -17, 36, -15, 29, -16, 43, -31, 19, -51, 61, -5, 24, -54, 18, -62, 42, -47, 55, -6, 28, -3, 12, -29, 35, -38, 21, -53, 20, -35, 25, -55, 41, -21, 33, -56, 34, -32, 22, -10, 62, -59, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 225, 39, 225, 38, 225, 36, 225, 2, 225, 37, 225, 10, 225, 11, 225, 48, 225, 40, 225, 56, 225, 58, 225],
	#[61, -5, 62, -59, 23, -67, 28, -3, 43, -31, 35, -38, 53, -39, 41, -356, 21, -53, 12, -29, 19, -51, 22, -10, 42, -47, 29, -16, 20, -35, 13, -17, 55, -6, 18, -62, 25, -55, 36, -15, 33, -56, 34, -32, 24, -54, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[13, -17, 62, -59, 43, -31, 29, -16, 12, -29, 36, -15, 20, -35, 55, -6, 35, -38, 42, -47, 56, 212, 28, -3, 24, -54, 22, -10, 53, -39, 18, -62, 21, -53, 33, -56, 41, -356, 19, -51, 23, -67, 61, -5, 34, -32, 25, -55, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[20, -35, 33, -56, 34, -32, 21, -53, 53, -39],
	#[65535, 176, 38, 176, 39, 176, 36, 176, 2, 176, 10, 176, 11, 176, 37, 176, 48, 176, 46, 176, 55, 176, 40, 176, 56, 176, 57, 176, 58, 176],
	#[38, -127, 37, -74, 40, 226, 39, -119, 36, -88, 58, 226, 56, 226],
	#[56, -367],
	#[61, -5, 35, -38, 62, -59, 23, -67, 28, -3, 43, -31, 58, 183, 36, -15, 20, -35, 41, -21, 21, -53, 12, -29, 19, -51, 22, -10, 42, -47, 29, -16, 13, -17, 55, -6, 40, 183, 18, -62, 25, -55, 33, -56, 34, -32, 24, -54, 56, 183, 53, -39, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 182, 58, 182, 40, 182, 56, 182],
	#[65535, 179, 58, 179, 40, 179, 56, 179],
	#[40, -362, 56, 178, 58, 178],
	#[65535, 227, 38, 227, 39, 227, 37, 227, 36, 227, 56, 227, 58, 227, 40, 227],
	#[65535, 213, 56, 213],
	#[13, -17, 43, -31, 36, -15, 35, -38, 42, -47, 24, -54, 22, -10, 53, -39, 21, -53, 25, -55, 12, -29, 29, -16, 23, -67, 34, -32, 55, -6, 20, -35, 41, -356, 19, -51, 28, -3, 18, -62, 33, -56, 61, -5, 62, -59, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 180, 40, 180, 58, 180, 56, 180],
	#[65535, 181, 58, 181, 40, 181, 56, 181],
	#[42, -47, 35, -38, 12, -29, 25, -55, 21, -53, 23, -67, 36, -15, 20, -35, 29, -16, 13, -17, 22, -10, 28, -3, 43, -31, 24, -54, 19, -51, 33, -56, 18, -62, 34, -32, 41, -21, 53, -39, 55, -6, 61, -5, 62, -59, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 228, 38, 228, 39, 228, 58, 228, 37, 228, 40, 228, 36, 228, 56, 228],
	#[65535, 174, 55, 174, 38, 174, 2, 174, 46, 174, 39, 174, 37, 174, 10, 174, 57, 174, 58, 174, 36, 174, 40, 174, 48, 174, 11, 174, 56, 174],
	#[58, -369],
	#[65535, 175, 46, 175, 38, 175, 37, 175, 10, 175, 2, 175, 39, 175, 48, 175, 40, 175, 11, 175, 36, 175, 55, 175, 56, 175, 57, 175, 58, 175],
	#[65535, 353, 45, 353, 38, 353, 62, 353, 12, 353, 43, 353, 59, 353, 39, 353, 35, 353, 54, 353, 47, 353, 50, 353, 28, 353, 14, 353, 23, 353, 33, 353, 13, 353, 55, 353, 52, 353, 46, 353, 31, 353, 41, 353, 42, 353, 57, 353, 61, 353, 44, 353, 30, 353, 26, 353, 51, 353, 29, 353, 17, 353, 15, 353, 48, 353, 37, 353, 16, 353, 22, 353, 18, 353, 27, 353, 10, 353, 25, 353, 24, 353, 36, 353, 20, 353, 32, 353, 40, 353, 21, 353, 19, 353, 60, 353, 58, 353, 49, 353, 56, 353, 53, 353, 63, 353, 64, 353, 65, 353, 66, 353, 67, 353, 68, 353, 69, 353],
	#[65535, 347, 58, 347, 21, 347, 18, 347, 23, 347, 20, 347, 29, 347, 60, 347, 28, 347, 56, 347, 32, 347, 31, 347, 38, 347, 62, 347, 61, 347, 39, 347, 19, 347, 22, 347, 46, 347, 35, 347, 15, 347, 13, 347, 27, 347, 30, 347, 37, 347, 36, 347, 26, 347, 33, 347, 55, 347, 14, 347, 45, 347, 10, 347, 25, 347, 41, 347, 40, 347, 43, 347, 50, 347, 59, 347, 52, 347, 16, 347, 57, 347, 24, 347, 47, 347, 12, 347, 51, 347, 48, 347, 17, 347, 44, 347, 42, 347, 49, 347, 53, 347, 63, 347, 64, 347, 65, 347, 66, 347, 67, 347, 68, 347, 69, 347],
	#[63, -374],
	#[65535, 336, 23, 336, 31, 336, 39, 336, 21, 336, 22, 336, 20, 336, 30, 336, 32, 336, 28, 336, 35, 336, 14, 336, 25, 336, 26, 336, 36, 336, 43, 336, 29, 336, 17, 336, 10, 336, 13, 336, 15, 336, 44, 336, 47, 336, 12, 336, 27, 336, 18, 336, 41, 336, 19, 336, 48, 336, 38, 336, 16, 336, 42, 336, 46, 336, 24, 336, 37, 336, 33, 336, 40, 336, 45, 336, 49, 336, 50, 336, 51, 336, 52, 336, 53, 336, 55, 336, 56, 336, 57, 336, 58, 336, 59, 336, 60, 336, 61, 336, 62, 336, 63, 336, 64, 336, 65, 336, 66, 336, 67, 336, 68, 336, 69, 336],
	#[65535, 348, 38, 348, 45, 348, 12, 348, 13, 348, 46, 348, 53, 348, 39, 348, 42, 348, 51, 348, 47, 348, 50, 348, 28, 348, 14, 348, 29, 348, 23, 348, 35, 348, 55, 348, 52, 348, 22, 348, 37, 348, 43, 348, 49, 348, 44, 348, 30, 348, 40, 348, 18, 348, 15, 348, 36, 348, 41, 348, 19, 348, 48, 348, 24, 348, 26, 348, 16, 348, 21, 348, 10, 348, 17, 348, 25, 348, 31, 348, 20, 348, 33, 348, 27, 348, 32, 348, 56, 348, 57, 348, 58, 348, 59, 348, 60, 348, 61, 348, 62, 348, 63, 348, 64, 348, 65, 348, 66, 348, 67, 348, 68, 348, 69, 348],
	#[65535, 321, 47, 321, 45, 321, 39, 321, 12, 321, 46, 321, 42, 321, 20, 321, 13, 321, 43, 321, 37, 321, 28, 321, 10, 321, 14, 321, 38, 321, 51, 321, 52, 321, 49, 321, 17, 321, 40, 321, 16, 321, 15, 321, 53, 321, 44, 321, 26, 321, 30, 321, 25, 321, 48, 321, 24, 321, 23, 321, 19, 321, 36, 321, 35, 321, 29, 321, 31, 321, 22, 321, 41, 321, 21, 321, 18, 321, 32, 321, 27, 321, 33, 321, 50, 321, 55, 321, 56, 321, 57, 321, 58, 321, 59, 321, 60, 321, 61, 321, 62, 321, 63, 321, 64, 321, 65, 321, 66, 321, 67, 321, 68, 321, 69, 321],
	#[65535, 299, 23, 299, 18, 299, 28, 299, 31, 299, 14, 299, 38, 299, 26, 299, 45, 299, 43, 299, 22, 299, 46, 299, 15, 299, 44, 299, 47, 299, 30, 299, 27, 299, 48, 299, 42, 299, 19, 299, 36, 299, 20, 299, 35, 299, 16, 299, 29, 299, 13, 299, 25, 299, 32, 299, 12, 299, 34, 299, 37, 299, 39, 299, 17, 299, 21, 299, 24, 299, 41, 299, 10, 299, 40, 299, 33, 299, 49, 299, 50, 299, 51, 299, 52, 299, 53, 299, 54, 299, 55, 299, 56, 299, 57, 299, 58, 299, 59, 299, 60, 299, 61, 299, 62, 299, 63, 299, 64, 299, 65, 299, 66, 299, 67, 299, 68, 299, 69, 299],
	#[65535, 341, 15, 341, 47, 341, 45, 341, 29, 341, 36, 341, 53, 341, 51, 341, 14, 341, 39, 341, 44, 341, 43, 341, 19, 341, 50, 341, 49, 341, 46, 341, 42, 341, 16, 341, 13, 341, 30, 341, 55, 341, 17, 341, 18, 341, 37, 341, 28, 341, 12, 341, 38, 341, 48, 341, 26, 341, 52, 341, 22, 341, 25, 341, 33, 341, 35, 341, 24, 341, 21, 341, 40, 341, 10, 341, 32, 341, 31, 341, 27, 341, 41, 341, 20, 341, 23, 341, 56, 341, 57, 341, 58, 341, 59, 341, 60, 341, 61, 341, 62, 341, 63, 341, 64, 341, 65, 341, 66, 341, 67, 341, 68, 341, 69, 341],
	#[65535, 307, 12, 307, 42, 307, 47, 307, 45, 307, 14, 307, 38, 307, 13, 307, 55, 307, 52, 307, 50, 307, 43, 307, 49, 307, 44, 307, 30, 307, 39, 307, 51, 307, 29, 307, 15, 307, 36, 307, 53, 307, 19, 307, 48, 307, 35, 307, 22, 307, 46, 307, 28, 307, 40, 307, 16, 307, 17, 307, 25, 307, 24, 307, 31, 307, 26, 307, 20, 307, 18, 307, 33, 307, 37, 307, 21, 307, 32, 307, 10, 307, 27, 307, 41, 307, 23, 307, 56, 307, 57, 307, 58, 307, 59, 307, 60, 307, 61, 307, 62, 307, 63, 307, 64, 307, 65, 307, 66, 307, 67, 307, 68, 307, 69, 307],
	#[65535, 235, 55, 235, 39, 235, 10, 235, 15, 235, 38, 235, 62, 235, 35, 235, 34, 235, 11, 235, 20, 235, 46, 235, 21, 235, 19, 235, 40, 235, 44, 235, 30, 235, 41, 235, 51, 235, 57, 235, 48, 235, 14, 235, 47, 235, 59, 235, 37, 235, 17, 235, 56, 235, 33, 235, 61, 235, 12, 235, 16, 235, 45, 235, 25, 235, 24, 235, 31, 235, 54, 235, 36, 235, 13, 235, 53, 235, 42, 235, 32, 235, 2, 235, 58, 235, 26, 235, 50, 235, 49, 235, 52, 235, 60, 235, 27, 235, 43, 235, 63, 235, 64, 235, 65, 235, 66, 235, 67, 235, 68, 235],
	#[65535, 3, #"eoi", 3],
	#[47, -384, 2, 216],
	#[2, -383],
	#[65535, 4, #"eoi", 4],
	#[13, -17, 29, -16, 12, -29, 43, -31, 23, -67, 55, -6, 62, -59, 28, -3, 22, -10, 18, -62, 42, -47, 19, -51, 33, -56, 53, -39, 20, -35, 61, -5, 34, -32, 25, -55, 24, -54, 21, -53, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[46, -352, 55, -351, 2, 220, 57, -350],
	#[65535, 217, 2, 217],
	#[65535, 10, #"eoi", 10],
	#[65535, 9, #"eoi", 9],
	#[55, -158, 54, -179, 52, -193, 62, -191, 61, -156, 15, -80, 53, -192, 37, -157, 43, -172, 36, -170, 50, -161, 14, -82, 38, -154, 45, -164, 44, -166, 22, -10, 46, -194, 19, -51, 39, -175, 35, -176, 20, -35, 48, -181, 25, -55, 32, -165, 47, -188, 24, -54, 57, -186, 12, -77, 29, -16, 23, -67, 41, -168, 49, -180, 51, -196, 42, -183, 59, -169, 16, -102, 28, -3, 18, -62, 17, -96, 13, -92, 33, -56, 21, -53, 34, -32, 63, -182, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 91, 2, 91],
	#[2, -404],
	#[2, 92, 40, -393],
	#[41, -389],
	#[65535, 93, 2, 93],
	#[54, -179, 63, -182, 62, -191, 44, -166, 67, -50, 43, -172, 37, -157, 41, -168, 40, 122, 38, -154, 32, -165, 45, -164, 36, -170, 39, -175, 46, -194, 59, -169, 35, -176, 2, 122, 47, -188, 12, -77, 50, -161, 21, -53, 42, -183, 19, -51, 14, -82, 20, -35, 33, -56, 48, -181, 17, -96, 25, -55, 24, -54, 16, -102, 15, -80, 34, -32, 13, -92, 49, -180, 51, -196, 52, -193, 53, -192, 55, -158, 57, -186, 61, -156, 65, -46, 66, -65, 68, -25],
	#[65535, 94, 2, 94, 40, 94],
	#[65535, 95, 2, 95, 40, 95],
	#[39, -175, 14, -82, 38, -154, 41, -168, 47, -188, 23, -67, 46, -194, 32, -165, 43, -172, 2, 104, 19, -51, 36, -170, 37, -157, 34, -32, 44, -166, 28, -3, 12, -77, 15, -80, 18, -62, 35, -176, 42, -183, 48, -181, 29, -16, 20, -35, 24, -54, 40, 104, 16, -102, 33, -56, 17, -96, 25, -55, 45, -164, 22, -10, 21, -53, 13, -92, 49, -180, 50, -161, 51, -196, 52, -193, 53, -192, 54, -179, 55, -158, 57, -186, 59, -169, 61, -156, 62, -191, 63, -182, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 106, 2, 106, 40, 106],
	#[65535, 105, 2, 105, 40, 105],
	#[65535, 124, 2, 124, 40, 124],
	#[65535, 123, 2, 123, 40, 123],
	#[65535, 107, 2, 107, 40, 107],
	#[65535, 7, #"eoi", 7],
	#[2, -406],
	#[65535, 2, #"eoi", 2],
	#[2, -414],
	#[2, 214, 10, -412],
	#[65535, 85, 10, 85, 2, 85],
	#[65535, 81, 2, 81],
	#[65535, 82, 2, 82],
	#[22, -10, 12, -29, 36, -15, 53, -39, 23, -67, 55, -6, 43, -31, 30, -43, 13, -17, 18, -62, 25, -55, 42, -47, 27, -12, 34, -32, 21, -53, 26, -37, 19, -51, 35, -38, 29, -16, 20, -35, 24, -54, 33, -56, 28, -3, 2, 215, 41, -21, 61, -5, 62, -59, 63, -45, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 86, 10, 86, 2, 86],
	#[65535, 5, #"eoi", 5],
	#[2, -416],
	#[65535, 8, #"eoi", 8],
	#[2, -418],
	#[65535, 1, #"eoi", 1],
	#[29, -16, 23, -67, 22, -10, 43, -31, 21, -53, 19, -51, 41, -21, 13, -17, 36, -15, 53, -39, 34, -32, 28, -3, 12, -29, 24, -54, 25, -55, 42, -47, 33, -56, 18, -62, 35, -38, 20, -35, 55, -6, 61, -5, 62, -59, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[30, 254, 27, 254, 42, 254, 23, 254, 13, 254, 36, 254, 22, 254, 33, 254, 43, 254, 21, 254, 10, 254, 28, 254, 12, 254, 41, 254, 29, 254, 18, 254, 20, 254, 34, 254, 48, -446, 26, 254, 24, 254, 2, 254, 19, 254, 25, 254, 35, 254, 53, 254, 55, 254, 61, 254, 62, 254, 63, 254, 64, 254, 65, 254, 66, 254, 67, 254, 68, 254, 69, 254],
	#[40, -442, 48, 221, 56, 221],
	#[48, -444],
	#[23, -67, 25, -55, 12, -29, 2, 250, 26, -37, 41, -21, 22, -10, 42, -47, 10, -437, 13, -17, 35, -38, 53, -39, 28, -3, 30, -43, 27, -12, 33, -56, 24, -54, 36, -15, 20, -35, 29, -16, 18, -62, 34, -32, 21, -53, 19, -51, 43, -31, 55, -6, 61, -5, 62, -59, 63, -45, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 241, 2, 241],
	#[23, -67, 13, -17, 26, -37, 21, -53, 36, -15, 25, -55, 22, -10, 29, -16, 28, -3, 12, -29, 53, -39, 27, -12, 30, -43, 43, -31, 41, -21, 35, -38, 24, -54, 42, -47, 10, -429, 33, -56, 19, -51, 20, -35, 2, 250, 18, -62, 34, -32, 55, -6, 61, -5, 62, -59, 63, -45, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 239, 2, 239],
	#[2, -428],
	#[65535, 6, #"eoi", 6],
	#[13, -17, 36, -15, 12, -29, 53, -39, 43, -31, 23, -67, 21, -53, 42, -47, 34, -32, 22, -10, 19, -51, 24, -54, 33, -56, 35, -38, 32, -420, 2, 249, 25, -55, 41, -21, 29, -16, 20, -35, 28, -3, 18, -62, 55, -419, 61, -5, 62, -59, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[10, -434, 2, 214],
	#[65535, 246, 2, 246],
	#[65535, 240, 2, 240],
	#[65535, 248, 2, 248],
	#[55, -419, 12, -29, 13, -17, 53, -39, 62, -59, 43, -31, 42, -47, 61, -5, 36, -15, 26, -37, 27, -12, 23, -67, 30, -43, 28, -3, 2, 215, 32, -420, 21, -53, 20, -35, 25, -55, 19, -51, 41, -21, 29, -16, 35, -38, 24, -54, 34, -32, 18, -62, 33, -56, 22, -10, 63, -45, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[10, 89, 2, 89, 40, -442, 48, 221],
	#[65535, 245, 2, 245],
	#[36, -15, 2, 249, 23, -67, 55, -419, 35, -38, 13, -17, 42, -47, 29, -16, 33, -56, 43, -31, 12, -29, 24, -54, 53, -39, 28, -3, 25, -55, 41, -21, 19, -51, 34, -32, 22, -10, 20, -35, 18, -62, 21, -53, 32, -420, 61, -5, 62, -59, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[2, 214, 10, -440],
	#[65535, 247, 2, 247],
	#[13, -17, 2, 215, 23, -67, 53, -39, 55, -419, 29, -16, 30, -43, 43, -31, 41, -21, 12, -29, 18, -62, 42, -47, 36, -15, 33, -56, 34, -32, 24, -54, 27, -12, 32, -420, 19, -51, 21, -53, 22, -10, 20, -35, 26, -37, 28, -3, 35, -38, 25, -55, 61, -5, 62, -59, 63, -45, 64, -68, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 243, 28, 243, 26, 243, 36, 243, 19, 243, 22, 243, 29, 243, 35, 243, 30, 243, 43, 243, 23, 243, 20, 243, 41, 243, 18, 243, 34, 243, 12, 243, 13, 243, 33, 243, 42, 243, 21, 243, 2, 243, 27, 243, 24, 243, 53, 243, 25, 243, 10, 243, 55, 243, 61, 243, 62, 243, 63, 243, 64, 243, 65, 243, 66, 243, 67, 243, 68, 243, 69, 243],
	#[13, -17, 53, -39, 36, -15, 22, -10, 12, -29, 41, -21, 34, -32, 43, -31, 23, -67, 21, -53, 55, -6, 42, -47, 25, -55, 33, -56, 35, -38, 24, -54, 18, -62, 19, -51, 29, -16, 20, -35, 28, -3, 61, -5, 62, -59, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[65535, 222, 48, 222, 56, 222],
	#[65535, 251, 13, 251, 55, 251, 53, 251, 21, 251, 2, 251, 22, 251, 28, 251, 12, 251, 34, 251, 43, 251, 23, 251, 26, 251, 41, 251, 35, 251, 24, 251, 10, 251, 25, 251, 29, 251, 33, 251, 20, 251, 18, 251, 19, 251, 42, 251, 27, 251, 30, 251, 36, 251, 61, 251, 62, 251, 63, 251, 64, 251, 65, 251, 66, 251, 67, 251, 68, 251, 69, 251],
	#[65535, 253, 55, 253, 13, 253, 21, 253, 61, 253, 23, 253, 43, 253, 30, 253, 28, 253, 12, 253, 2, 253, 18, 253, 62, 253, 20, 253, 19, 253, 22, 253, 29, 253, 24, 253, 10, 253, 34, 253, 36, 253, 25, 253, 35, 253, 42, 253, 27, 253, 33, 253, 41, 253, 26, 253, 53, 253, 63, 253, 64, 253, 65, 253, 66, 253, 67, 253, 68, 253, 69, 253],
	#[65535, 255, 12, 255, 53, 255, 21, 255, 19, 255, 34, 255, 29, 255, 13, 255, 20, 255, 2, 255, 24, 255, 26, 255, 23, 255, 28, 255, 55, 255, 10, 255, 25, 255, 30, 255, 35, 255, 18, 255, 27, 255, 43, 255, 22, 255, 36, 255, 33, 255, 41, 255, 42, 255, 61, 255, 62, 255, 63, 255, 64, 255, 65, 255, 66, 255, 67, 255, 68, 255, 69, 255],
	#[40, -448, 56, -347],
	#[62, -59, 12, -29, 19, -51, 29, -16, 23, -67, 13, -17, 55, -6, 18, -62, 41, -21, 35, -38, 34, -32, 25, -55, 24, -54, 36, -15, 20, -35, 22, -10, 33, -56, 28, -3, 21, -53, 61, -5, 42, -47, 43, -31, 53, -39, 63, -45, 65, -46, 66, -65, 67, -50, 68, -25, 69, -40],
	#[56, -450],
	#[48, -451],
	#[65535, 252, 12, 252, 53, 252, 21, 252, 34, 252, 29, 252, 13, 252, 20, 252, 2, 252, 24, 252, 26, 252, 25, 252, 23, 252, 28, 252, 55, 252, 10, 252, 43, 252, 30, 252, 35, 252, 18, 252, 27, 252, 19, 252, 22, 252, 36, 252, 33, 252, 41, 252, 42, 252, 61, 252, 62, 252, 63, 252, 64, 252, 65, 252, 66, 252, 67, 252, 68, 252, 69, 252],
	#[65535, 242, 23, 242, 20, 242, 12, 242, 13, 242, 22, 242, 21, 242, 30, 242, 29, 242, 42, 242, 33, 242, 19, 242, 35, 242, 41, 242, 28, 242, 27, 242, 10, 242, 34, 242, 24, 242, 36, 242, 18, 242, 25, 242, 2, 242, 26, 242, 43, 242, 53, 242, 55, 242, 61, 242, 62, 242, 63, 242, 64, 242, 65, 242, 66, 242, 67, 242, 68, 242, 69, 242],
	#[65535, 244, 2, 244]],
  goto-table:
      #[#[155, 8, 154, 10, 153, 13, 150, 26, 149, 32, 146, 47, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 130, 48, 125, 3, 124, 7, 121, 33, 98, 57, 90, 41, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 63, 75, 1, 71, 27, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 44, 62, 58, 61, 4, 55, 5, 53, 38, 43, 30, 42, 46, 41, 20, 36, 14, 35, 37, 34, 31, 33, 55, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 13, 16, 12, 28, 11, 62, 9, 21, 8, 43, 7, 6, 6, 22, 5, 60, 4, 65, 3, 23, 0, 17],
	#[],
	#[],
	#[],
	#[],
	#[153, 13, 154, 10, 155, 8, 138, 12, 137, 18, 136, 35, 146, 47, 134, 40, 130, 48, 150, 26, 149, 345, 135, 25, 90, 41, 89, 59, 83, 19, 81, 29, 76, 63, 75, 1, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 63, 44, 62, 58, 61, 4, 55, 5, 53, 38, 43, 30, 42, 46, 41, 20, 36, 14, 35, 37, 34, 31, 18, 61, 12, 28, 33, 55, 29, 15, 24, 53, 19, 50, 23, 66, 21, 52, 13, 16, 25, 54, 20, 34, 28, 2, 22, 9],
	#[163, 422, 161, 423, 158, 425, 157, 426, 155, 8, 154, 10, 153, 13, 150, 26, 149, 420, 148, 421, 146, 47, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 130, 48, 90, 41, 89, 59, 83, 19, 81, 29, 75, 1, 69, 39, 68, 24, 66, 64, 63, 44, 62, 58, 61, 4, 65, 45, 55, 418, 67, 49, 42, 46, 53, 38, 34, 31, 36, 14, 35, 37, 24, 53, 28, 2, 13, 16, 18, 61, 32, 419, 41, 20, 33, 55, 20, 34, 19, 50, 21, 52, 25, 54, 12, 28, 76, 63, 159, 424, 22, 9, 43, 30, 23, 66, 29, 15],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[1, 416],
	#[],
	#[181, 271, 180, 272, 179, 273, 172, 289, 166, 285, 165, 278, 155, 274, 138, 161, 134, 277, 125, 3, 124, 269, 121, 281, 120, 282, 118, 286, 92, 280, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 288, 75, 1, 72, 276, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 140, 57, 287, 55, 268, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 270, 44, 275, 43, 279, 42, 284, 41, 167, 40, 283, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 267],
	#[],
	#[134, 170, 121, 173, 110, 162, 124, 158, 125, 3, 138, 161, 104, 189, 155, 159, 120, 176, 117, 186, 118, 184, 92, 172, 119, 188, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 194, 75, 1, 103, 414, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 177, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 154],
	#[155, 8, 130, 48, 138, 12, 121, 33, 146, 47, 153, 13, 154, 10, 124, 7, 125, 3, 98, 408, 97, 407, 136, 35, 137, 18, 149, 32, 90, 41, 89, 59, 83, 19, 150, 26, 88, 51, 135, 25, 75, 1, 81, 29, 69, 39, 68, 24, 95, 409, 64, 67, 63, 44, 62, 58, 61, 4, 65, 45, 55, 5, 66, 64, 134, 40, 26, 36, 12, 28, 19, 50, 41, 20, 34, 31, 27, 11, 20, 34, 36, 14, 30, 42, 94, 406, 43, 30, 28, 2, 22, 9, 13, 16, 29, 15, 87, 56, 42, 46, 21, 52, 67, 49, 25, 54, 53, 38, 35, 37, 33, 55, 24, 53, 76, 63, 18, 61, 23, 66],
	#[146, 404, 75, 1, 34, 31, 20, 34, 21, 52, 53, 38, 76, 63, 33, 55],
	#[],
	#[],
	#[91, 347, 39, 118, 38, 126, 37, 73, 36, 87],
	#[],
	#[],
	#[155, 159, 138, 161, 134, 170, 125, 3, 124, 158, 121, 173, 120, 176, 119, 188, 118, 184, 117, 186, 110, 162, 104, 189, 103, 259, 92, 172, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 177, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 154],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[190, 102, 189, 116, 188, 110, 187, 115, 185, 124, 184, 134, 183, 133, 182, 72, 91, 105, 77, 128, 74, 86, 69, 98, 68, 117, 73, 85, 64, 125, 63, 111, 72, 92, 61, 71, 65, 112, 57, 122, 67, 114, 37, 73, 66, 131, 48, 103, 24, 82, 41, 94, 62, 130, 59, 96, 52, 127, 43, 100, 21, 93, 33, 69, 50, 80, 30, 109, 51, 132, 44, 90, 53, 107, 35, 104, 40, 108, 46, 129, 29, 123, 39, 118, 36, 87, 45, 77, 27, 84, 32, 89, 38, 126, 31, 99, 42, 113, 25, 120, 17, 95, 55, 75, 26, 97, 13, 91, 23, 74, 20, 121, 19, 83, 12, 76, 16, 101, 18, 78, 22, 88, 28, 106, 10, 70, 14, 81, 47, 119, 15, 79],
	#[],
	#[155, 8, 136, 35, 137, 18, 138, 12, 146, 47, 89, 59, 134, 40, 83, 19, 81, 29, 135, 25, 76, 63, 130, 344, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 63, 44, 75, 1, 61, 4, 55, 5, 53, 38, 43, 30, 42, 46, 62, 58, 34, 31, 33, 55, 29, 15, 28, 2, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 13, 16, 12, 28],
	#[128, 240, 127, 237, 126, 238, 33, 239],
	#[101, 391, 100, 389, 99, 390, 41, 388],
	#[137, 204, 63, 44],
	#[141, 201, 140, 205, 139, 206, 138, 12, 136, 198, 137, 18, 142, 200, 68, 24, 67, 49, 66, 64, 65, 45, 63, 44, 62, 58, 61, 4, 41, 196, 13, 16, 12, 28],
	#[],
	#[],
	#[55, 350, 57, 349, 46, 351],
	#[],
	#[],
	#[146, 222, 123, 221, 122, 223, 81, 224, 76, 63, 75, 1, 83, 220, 33, 55, 20, 34, 34, 31, 22, 9, 53, 38, 28, 2, 29, 15, 18, 61, 21, 52, 23, 66],
	#[],
	#[],
	#[],
	#[],
	#[155, 213, 138, 161, 119, 188, 118, 184, 117, 217, 120, 176, 114, 212, 134, 170, 106, 214, 105, 215, 92, 172, 89, 59, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 48, 180, 44, 165, 43, 171, 49, 179, 41, 167, 40, 216, 50, 160, 51, 195, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 42, 182, 29, 15, 28, 2, 45, 163, 24, 53, 46, 193, 22, 9, 21, 52, 20, 34, 19, 50, 52, 192, 17, 95, 16, 101, 38, 153, 14, 81, 13, 91, 12, 76, 25, 54, 39, 174, 18, 61, 23, 66, 47, 187, 15, 79],
	#[10, 387, 11, 386],
	#[],
	#[55, 152],
	#[146, 380, 145, 381, 76, 63, 75, 1, 53, 38, 34, 31, 33, 55, 21, 52, 20, 34],
	#[],
	#[],
	#[],
	#[137, 18, 138, 12, 136, 198, 140, 197, 141, 201, 142, 200, 139, 199, 68, 24, 67, 49, 66, 64, 65, 45, 63, 44, 62, 58, 61, 4, 41, 196, 13, 16, 12, 28],
	#[138, 12, 136, 35, 146, 47, 134, 40, 154, 10, 149, 68, 155, 8, 150, 26, 153, 13, 137, 18, 130, 48, 135, 25, 90, 41, 89, 59, 83, 19, 81, 29, 76, 63, 75, 1, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 63, 44, 62, 58, 61, 4, 55, 5, 53, 38, 43, 30, 36, 14, 35, 37, 41, 20, 33, 55, 42, 46, 29, 15, 28, 2, 25, 54, 24, 53, 34, 31, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 13, 16, 12, 28, 23, 66],
	#[],
	#[],
	#[2, 379],
	#[],
	#[],
	#[],
	#[60, 378],
	#[],
	#[],
	#[190, 102, 189, 116, 188, 110, 187, 115, 185, 124, 182, 297, 184, 134, 183, 133, 91, 105, 77, 128, 74, 86, 73, 85, 72, 92, 69, 98, 68, 117, 67, 114, 66, 131, 65, 112, 64, 125, 63, 111, 62, 130, 61, 71, 59, 96, 57, 122, 55, 75, 53, 107, 52, 127, 51, 132, 50, 80, 48, 103, 43, 100, 39, 118, 38, 126, 37, 73, 40, 108, 41, 94, 33, 69, 32, 89, 42, 113, 30, 109, 29, 123, 28, 106, 44, 90, 25, 120, 35, 104, 21, 93, 20, 121, 36, 87, 47, 119, 17, 95, 16, 101, 26, 97, 27, 84, 13, 91, 12, 76, 10, 70, 24, 82, 45, 77, 19, 83, 31, 99, 46, 129, 22, 88, 18, 78, 23, 74, 14, 81, 15, 79],
	#[],
	#[],
	#[],
	#[],
	#[77, 295, 74, 86, 73, 85, 45, 296, 33, 69, 32, 89, 31, 99, 30, 109, 29, 123, 28, 106, 27, 84, 26, 97, 25, 120, 24, 82, 23, 74, 22, 88, 21, 93, 20, 121, 19, 83, 18, 78],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[188, 110, 185, 124, 184, 134, 182, 294, 187, 115, 189, 116, 190, 102, 183, 133, 91, 105, 77, 128, 74, 86, 73, 85, 72, 92, 69, 98, 68, 117, 67, 114, 66, 131, 65, 112, 64, 125, 63, 111, 62, 130, 61, 71, 59, 96, 57, 122, 55, 75, 53, 107, 52, 127, 50, 80, 48, 103, 47, 119, 46, 129, 45, 77, 44, 90, 42, 113, 41, 94, 39, 118, 51, 132, 40, 108, 32, 89, 43, 100, 28, 106, 33, 69, 25, 120, 24, 82, 20, 121, 19, 83, 15, 79, 27, 84, 29, 123, 16, 101, 10, 70, 12, 76, 23, 74, 26, 97, 38, 126, 35, 104, 22, 88, 13, 91, 18, 78, 31, 99, 17, 95, 21, 93, 30, 109, 36, 87, 14, 81, 37, 73],
	#[],
	#[190, 102, 189, 116, 188, 110, 185, 124, 184, 134, 183, 133, 182, 293, 187, 115, 91, 105, 77, 128, 74, 86, 73, 85, 72, 92, 69, 98, 68, 117, 67, 114, 66, 131, 65, 112, 64, 125, 63, 111, 62, 130, 61, 71, 59, 96, 57, 122, 55, 75, 53, 107, 52, 127, 51, 132, 50, 80, 48, 103, 47, 119, 46, 129, 45, 77, 44, 90, 43, 100, 42, 113, 41, 94, 40, 108, 39, 118, 38, 126, 37, 73, 36, 87, 35, 104, 33, 69, 32, 89, 31, 99, 30, 109, 29, 123, 28, 106, 27, 84, 26, 97, 25, 120, 24, 82, 23, 74, 22, 88, 21, 93, 20, 121, 19, 83, 18, 78, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 70],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[54, 292],
	#[189, 116, 188, 110, 187, 115, 184, 134, 183, 133, 182, 291, 185, 124, 190, 102, 91, 105, 63, 111, 59, 96, 52, 127, 47, 119, 46, 129, 43, 100, 41, 94, 40, 108, 39, 118, 38, 126, 37, 73, 36, 87, 35, 104, 33, 69, 32, 89, 30, 109, 28, 106, 27, 84, 26, 97, 25, 120, 24, 82, 23, 74, 22, 88, 21, 93, 20, 121, 19, 83, 18, 78, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 70, 51, 132, 64, 125, 61, 71, 73, 85, 67, 114, 48, 103, 65, 112, 53, 107, 45, 77, 66, 131, 77, 128, 72, 92, 69, 98, 74, 86, 29, 123, 57, 122, 68, 117, 50, 80, 44, 90, 42, 113, 31, 99, 55, 75, 62, 130],
	#[],
	#[],
	#[],
	#[49, 144],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[190, 102, 189, 116, 188, 110, 187, 115, 185, 124, 184, 134, 183, 133, 182, 143, 91, 105, 77, 128, 74, 86, 73, 85, 72, 92, 69, 98, 68, 117, 67, 114, 66, 131, 65, 112, 64, 125, 63, 111, 62, 130, 59, 96, 55, 75, 44, 90, 61, 71, 40, 108, 51, 132, 48, 103, 52, 127, 27, 84, 39, 118, 45, 77, 50, 80, 57, 122, 46, 129, 41, 94, 42, 113, 32, 89, 53, 107, 12, 76, 26, 97, 38, 126, 43, 100, 25, 120, 47, 119, 20, 121, 36, 87, 33, 69, 35, 104, 17, 95, 16, 101, 37, 73, 24, 82, 28, 106, 22, 88, 13, 91, 18, 78, 31, 99, 29, 123, 14, 81, 19, 83, 10, 70, 23, 74, 21, 93, 30, 109, 15, 79],
	#[],
	#[190, 102, 189, 116, 188, 110, 187, 115, 185, 124, 184, 134, 183, 142, 91, 105, 77, 128, 74, 86, 73, 85, 72, 92, 69, 98, 68, 117, 67, 114, 66, 131, 65, 112, 64, 125, 63, 111, 62, 130, 61, 71, 59, 96, 57, 122, 55, 75, 53, 107, 52, 127, 51, 132, 50, 80, 48, 103, 47, 119, 46, 129, 45, 77, 44, 90, 43, 100, 42, 113, 41, 94, 40, 108, 39, 118, 38, 126, 37, 73, 36, 87, 35, 104, 33, 69, 32, 89, 31, 99, 30, 109, 29, 123, 28, 106, 27, 84, 26, 97, 25, 120, 24, 82, 23, 74, 22, 88, 21, 93, 20, 121, 19, 83, 18, 78, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 70],
	#[],
	#[],
	#[181, 138, 77, 139, 74, 86, 73, 85, 59, 140, 26, 97, 19, 83, 25, 120, 20, 121, 32, 89, 18, 78, 24, 82, 27, 84, 33, 69, 21, 93, 31, 99, 29, 123, 23, 74, 22, 88, 28, 106, 30, 109],
	#[],
	#[],
	#[],
	#[190, 102, 189, 116, 188, 110, 187, 115, 185, 124, 184, 134, 183, 133, 182, 137, 91, 105, 77, 128, 74, 86, 73, 85, 72, 92, 69, 98, 68, 117, 67, 114, 66, 131, 65, 112, 64, 125, 63, 111, 62, 130, 61, 71, 59, 96, 57, 122, 55, 75, 53, 107, 52, 127, 51, 132, 50, 80, 48, 103, 47, 119, 46, 129, 45, 77, 44, 90, 43, 100, 42, 113, 41, 94, 40, 108, 39, 118, 38, 126, 37, 73, 36, 87, 35, 104, 33, 69, 32, 89, 31, 99, 30, 109, 29, 123, 28, 106, 27, 84, 26, 97, 25, 120, 24, 82, 23, 74, 22, 88, 21, 93, 20, 121, 19, 83, 18, 78, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 70],
	#[77, 136, 74, 86, 73, 85, 33, 69, 32, 89, 31, 99, 30, 109, 29, 123, 28, 106, 27, 84, 26, 97, 25, 120, 24, 82, 23, 74, 22, 88, 21, 93, 20, 121, 19, 83, 18, 78],
	#[],
	#[190, 102, 189, 116, 188, 110, 187, 115, 185, 124, 184, 134, 183, 135, 91, 105, 77, 128, 74, 86, 73, 85, 72, 92, 69, 98, 68, 117, 66, 131, 65, 112, 64, 125, 63, 111, 62, 130, 61, 71, 59, 96, 57, 122, 55, 75, 67, 114, 53, 107, 52, 127, 51, 132, 50, 80, 48, 103, 47, 119, 46, 129, 45, 77, 44, 90, 43, 100, 42, 113, 41, 94, 40, 108, 39, 118, 38, 126, 37, 73, 36, 87, 35, 104, 33, 69, 32, 89, 31, 99, 30, 109, 29, 123, 28, 106, 27, 84, 26, 97, 25, 120, 24, 82, 23, 74, 22, 88, 21, 93, 20, 121, 19, 83, 18, 78, 17, 95, 16, 101, 10, 70, 12, 76, 13, 91, 14, 81, 15, 79],
	#[],
	#[],
	#[58, 377],
	#[],
	#[181, 376, 59, 140],
	#[190, 102, 185, 124, 184, 134, 188, 110, 182, 141, 187, 115, 189, 116, 183, 133, 91, 105, 77, 128, 74, 86, 73, 85, 72, 92, 69, 98, 66, 131, 65, 112, 63, 111, 62, 130, 61, 71, 52, 127, 57, 122, 48, 103, 46, 129, 35, 104, 21, 93, 59, 96, 64, 125, 47, 119, 67, 114, 27, 84, 44, 90, 20, 121, 36, 87, 16, 101, 68, 117, 51, 132, 42, 113, 55, 75, 53, 107, 50, 80, 28, 106, 26, 97, 41, 94, 43, 100, 40, 108, 24, 82, 45, 77, 32, 89, 15, 79, 18, 78, 33, 69, 25, 120, 17, 95, 39, 118, 37, 73, 12, 76, 10, 70, 22, 88, 13, 91, 31, 99, 29, 123, 38, 126, 14, 81, 19, 83, 23, 74, 30, 109],
	#[60, 375],
	#[],
	#[58, 374],
	#[193, 148, 77, 150, 74, 86, 73, 85, 63, 147, 23, 74, 32, 89, 33, 69, 25, 120, 20, 121, 31, 99, 27, 84, 41, 145, 55, 149, 28, 106, 29, 123, 24, 82, 45, 146, 18, 78, 30, 109, 21, 93, 26, 97, 19, 83, 22, 88],
	#[],
	#[],
	#[],
	#[192, 370, 191, 372, 54, 371],
	#[155, 8, 154, 10, 153, 13, 150, 26, 149, 151, 146, 47, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 130, 48, 90, 41, 89, 59, 83, 19, 81, 29, 76, 63, 75, 1, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 63, 44, 62, 58, 61, 4, 55, 5, 53, 38, 43, 30, 42, 46, 41, 20, 36, 14, 35, 37, 34, 31, 33, 55, 29, 15, 28, 2, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 13, 16, 12, 28],
	#[],
	#[56, 369],
	#[155, 159, 138, 161, 134, 170, 125, 3, 124, 158, 121, 173, 120, 176, 119, 188, 118, 184, 117, 186, 110, 162, 104, 189, 103, 183, 92, 172, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 177, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 154],
	#[],
	#[155, 159, 138, 161, 134, 170, 125, 3, 124, 158, 121, 173, 120, 176, 119, 188, 118, 184, 117, 186, 110, 162, 104, 189, 103, 321, 92, 172, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 177, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 154],
	#[],
	#[],
	#[121, 173, 120, 176, 119, 188, 118, 184, 117, 186, 138, 161, 134, 170, 110, 162, 124, 158, 125, 3, 104, 189, 92, 172, 155, 159, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 194, 75, 1, 103, 319, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 177, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 10, 154, 12, 76],
	#[112, 256, 111, 318, 10, 255],
	#[138, 161, 134, 170, 125, 3, 124, 158, 121, 173, 120, 176, 119, 188, 117, 186, 110, 316, 118, 184, 92, 172, 109, 317, 89, 59, 88, 51, 87, 56, 75, 1, 68, 24, 66, 64, 64, 67, 63, 181, 72, 166, 61, 155, 65, 45, 57, 185, 55, 157, 54, 178, 44, 165, 49, 179, 40, 177, 53, 191, 50, 160, 48, 180, 41, 167, 43, 171, 25, 54, 67, 49, 45, 163, 32, 164, 16, 101, 36, 169, 33, 55, 35, 175, 26, 36, 59, 168, 37, 156, 34, 31, 12, 76, 46, 193, 42, 182, 27, 11, 24, 53, 51, 195, 62, 190, 38, 153, 17, 95, 52, 192, 10, 154, 21, 52, 30, 42, 47, 187, 76, 194, 13, 91, 19, 50, 20, 34, 14, 81, 15, 79, 39, 174],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[155, 159, 138, 161, 134, 170, 125, 3, 124, 158, 121, 173, 120, 176, 119, 188, 118, 184, 117, 186, 110, 162, 104, 189, 103, 265, 92, 172, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 177, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 154],
	#[],
	#[],
	#[],
	#[],
	#[112, 256, 111, 257, 10, 255],
	#[],
	#[],
	#[],
	#[155, 159, 138, 161, 134, 170, 125, 3, 124, 158, 121, 173, 120, 176, 119, 188, 118, 184, 117, 186, 110, 162, 104, 189, 103, 254, 92, 172, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 177, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 154],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[56, 343],
	#[],
	#[155, 159, 138, 161, 134, 170, 125, 3, 124, 158, 121, 173, 120, 176, 119, 188, 118, 184, 117, 186, 110, 162, 104, 189, 103, 218, 92, 172, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 66, 64, 65, 45, 64, 67, 52, 192, 57, 185, 61, 155, 62, 190, 37, 156, 36, 169, 43, 171, 49, 179, 48, 180, 51, 195, 28, 2, 44, 165, 59, 168, 41, 167, 63, 181, 25, 54, 20, 34, 67, 49, 27, 11, 40, 177, 24, 53, 42, 182, 55, 157, 35, 175, 50, 160, 19, 50, 32, 164, 33, 55, 47, 187, 45, 163, 54, 178, 17, 95, 29, 15, 30, 42, 53, 191, 26, 36, 10, 154, 16, 101, 14, 81, 21, 52, 22, 9, 23, 66, 12, 76, 18, 61, 15, 79, 39, 174, 46, 193, 13, 91, 34, 31, 38, 153],
	#[155, 159, 138, 161, 134, 170, 125, 3, 124, 158, 121, 173, 120, 176, 119, 188, 118, 184, 117, 186, 110, 162, 104, 189, 103, 219, 92, 172, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 177, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 154],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[58, 211],
	#[],
	#[40, 202],
	#[142, 203, 138, 12, 137, 18, 136, 198, 68, 24, 67, 49, 66, 64, 65, 45, 63, 44, 62, 58, 61, 4, 41, 196, 13, 16, 12, 28],
	#[],
	#[],
	#[46, 208],
	#[56, 207],
	#[],
	#[138, 12, 136, 198, 137, 18, 142, 209, 68, 24, 67, 49, 66, 64, 65, 45, 63, 44, 62, 58, 61, 4, 41, 196, 13, 16, 12, 28],
	#[56, 210],
	#[],
	#[],
	#[],
	#[138, 161, 134, 170, 120, 176, 119, 188, 118, 184, 117, 217, 114, 325, 113, 326, 92, 172, 89, 59, 76, 194, 75, 1, 72, 166, 68, 24, 67, 49, 66, 64, 65, 45, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 216, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 25, 54, 24, 53, 21, 52, 20, 34, 19, 50, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76],
	#[],
	#[],
	#[155, 213, 138, 161, 134, 170, 120, 176, 119, 188, 118, 184, 117, 217, 114, 212, 106, 214, 105, 324, 92, 172, 89, 59, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 216, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 29, 15, 28, 2, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76],
	#[155, 213, 138, 161, 134, 170, 120, 176, 119, 188, 118, 184, 117, 217, 114, 212, 106, 214, 105, 323, 92, 172, 89, 59, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 216, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 29, 15, 28, 2, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76],
	#[58, 342],
	#[],
	#[181, 271, 180, 272, 179, 273, 172, 289, 165, 340, 155, 274, 166, 285, 138, 161, 134, 277, 125, 3, 124, 269, 121, 281, 120, 282, 118, 286, 92, 280, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 288, 75, 1, 72, 276, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 140, 57, 287, 55, 268, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 270, 44, 275, 43, 279, 42, 284, 41, 167, 40, 283, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 267],
	#[40, 338],
	#[155, 159, 134, 170, 138, 161, 125, 3, 124, 158, 121, 173, 120, 176, 119, 188, 118, 184, 117, 186, 110, 162, 104, 189, 103, 336, 92, 172, 88, 51, 87, 56, 89, 59, 81, 29, 72, 166, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 75, 1, 59, 168, 83, 19, 55, 157, 54, 178, 53, 191, 52, 192, 48, 180, 47, 187, 46, 193, 45, 163, 40, 177, 50, 160, 38, 153, 49, 179, 63, 181, 25, 54, 24, 53, 61, 155, 57, 185, 36, 169, 37, 156, 43, 171, 16, 101, 26, 36, 27, 11, 41, 167, 69, 39, 44, 165, 42, 182, 18, 61, 19, 50, 33, 55, 20, 34, 21, 52, 34, 31, 22, 9, 32, 164, 35, 175, 12, 76, 13, 91, 62, 190, 29, 15, 17, 95, 10, 154, 15, 79, 30, 42, 39, 174, 28, 2, 76, 194, 51, 195, 14, 81, 23, 66],
	#[],
	#[155, 159, 138, 161, 134, 170, 125, 3, 124, 158, 121, 173, 120, 176, 119, 188, 118, 184, 117, 186, 110, 162, 104, 189, 103, 225, 88, 51, 83, 19, 89, 59, 81, 29, 76, 194, 92, 172, 72, 166, 69, 39, 68, 24, 63, 181, 75, 1, 61, 155, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 177, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 154, 65, 45, 64, 67, 87, 56, 62, 190, 67, 49, 66, 64, 59, 168],
	#[129, 227, 31, 226],
	#[85, 228, 83, 230, 81, 233, 79, 232, 76, 235, 75, 1, 34, 31, 33, 55, 29, 15, 28, 2, 25, 229, 23, 234, 22, 9, 21, 52, 20, 231, 18, 61],
	#[],
	#[83, 230, 81, 233, 79, 236, 76, 235, 75, 1, 34, 31, 33, 55, 29, 15, 28, 2, 23, 66, 22, 9, 21, 52, 20, 34, 18, 61],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[86, 245, 85, 247, 78, 243, 34, 248, 29, 249, 25, 229, 24, 242, 23, 250, 22, 251, 21, 246, 20, 244],
	#[128, 241, 126, 238, 33, 239],
	#[],
	#[],
	#[],
	#[],
	#[77, 327, 74, 86, 73, 85, 33, 69, 32, 89, 31, 99, 30, 109, 29, 123, 28, 106, 27, 84, 26, 97, 25, 120, 24, 82, 23, 74, 22, 88, 21, 93, 20, 121, 19, 83, 18, 78],
	#[],
	#[155, 213, 138, 161, 134, 170, 120, 176, 119, 188, 118, 184, 117, 217, 114, 212, 106, 214, 105, 322, 92, 172, 89, 59, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 61, 155, 41, 167, 40, 216, 62, 190, 38, 153, 37, 156, 36, 169, 35, 175, 63, 181, 33, 55, 32, 164, 42, 182, 29, 15, 28, 2, 25, 54, 34, 31, 22, 9, 21, 52, 20, 34, 24, 53, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 39, 174, 19, 50, 23, 66],
	#[],
	#[155, 159, 138, 161, 134, 170, 125, 3, 124, 158, 121, 173, 120, 176, 119, 188, 118, 184, 117, 186, 110, 162, 104, 189, 103, 252, 92, 172, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 177, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 154],
	#[],
	#[],
	#[],
	#[],
	#[129, 253, 31, 226],
	#[],
	#[],
	#[155, 159, 138, 161, 134, 170, 125, 3, 124, 158, 121, 173, 120, 176, 119, 188, 118, 184, 117, 186, 110, 162, 104, 189, 103, 258, 92, 172, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 40, 177, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 27, 11, 26, 36, 25, 54, 24, 53, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 154, 28, 2, 23, 66],
	#[],
	#[],
	#[],
	#[156, 261, 31, 260],
	#[83, 263, 81, 264, 80, 262, 29, 15, 28, 2, 23, 66, 22, 9, 18, 61],
	#[],
	#[],
	#[],
	#[],
	#[60, 266],
	#[],
	#[181, 271, 180, 272, 179, 273, 172, 289, 166, 285, 165, 314, 155, 274, 138, 161, 134, 277, 125, 3, 124, 269, 121, 281, 120, 282, 118, 286, 92, 280, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 288, 75, 1, 72, 276, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 140, 57, 287, 55, 268, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 270, 44, 275, 43, 279, 42, 284, 41, 167, 40, 283, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 267],
	#[190, 102, 189, 116, 188, 110, 187, 115, 185, 124, 184, 134, 183, 133, 182, 312, 91, 105, 77, 128, 74, 86, 73, 85, 72, 92, 69, 98, 68, 117, 67, 114, 66, 131, 65, 112, 64, 125, 63, 111, 62, 130, 61, 71, 59, 96, 57, 122, 55, 75, 53, 107, 52, 127, 51, 132, 50, 80, 48, 103, 47, 119, 46, 129, 45, 77, 44, 90, 43, 100, 42, 113, 41, 94, 40, 108, 39, 118, 38, 126, 37, 73, 36, 87, 35, 104, 33, 69, 32, 89, 31, 99, 29, 123, 28, 106, 27, 84, 25, 120, 24, 82, 21, 93, 20, 121, 19, 83, 18, 78, 17, 95, 26, 97, 13, 91, 15, 79, 12, 76, 22, 88, 16, 101, 14, 81, 10, 70, 23, 74, 30, 109],
	#[173, 311, 174, 304, 10, 305],
	#[],
	#[],
	#[],
	#[180, 272, 181, 271, 155, 274, 138, 161, 134, 277, 179, 273, 125, 3, 124, 269, 121, 281, 120, 282, 118, 286, 165, 310, 172, 289, 166, 285, 92, 280, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 288, 75, 1, 72, 276, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 61, 155, 59, 140, 57, 287, 55, 268, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 270, 44, 275, 43, 279, 42, 284, 41, 167, 40, 283, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 21, 52, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 10, 267, 19, 50, 20, 34, 12, 76, 13, 91, 62, 190, 22, 9, 28, 2],
	#[181, 271, 180, 272, 179, 273, 172, 309, 171, 308, 138, 161, 134, 277, 125, 3, 124, 269, 121, 281, 120, 282, 118, 286, 92, 280, 89, 59, 88, 51, 87, 56, 76, 288, 75, 1, 72, 276, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 140, 57, 287, 55, 268, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 270, 44, 275, 43, 279, 42, 284, 41, 167, 40, 283, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 27, 11, 26, 36, 25, 54, 24, 53, 21, 52, 20, 34, 19, 50, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 267],
	#[],
	#[],
	#[],
	#[156, 315, 31, 260],
	#[],
	#[],
	#[173, 306, 174, 304, 10, 305],
	#[],
	#[155, 274, 179, 273, 172, 289, 180, 272, 165, 303, 125, 3, 181, 271, 121, 281, 166, 285, 118, 286, 138, 161, 120, 282, 134, 277, 124, 269, 88, 51, 83, 19, 89, 59, 81, 29, 76, 288, 92, 280, 72, 276, 69, 39, 68, 24, 67, 49, 66, 64, 64, 67, 63, 181, 75, 1, 61, 155, 59, 140, 87, 56, 57, 287, 55, 268, 54, 178, 53, 191, 52, 192, 65, 45, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 270, 44, 275, 43, 279, 42, 284, 41, 167, 40, 283, 62, 190, 38, 153, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 36, 169, 37, 156, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 39, 174, 18, 61, 19, 50, 10, 267, 51, 195],
	#[],
	#[],
	#[],
	#[190, 102, 189, 116, 188, 110, 187, 115, 185, 124, 184, 134, 183, 133, 182, 290, 91, 105, 77, 128, 74, 86, 73, 85, 72, 92, 69, 98, 68, 117, 67, 114, 66, 131, 65, 112, 64, 125, 63, 111, 62, 130, 61, 71, 59, 96, 57, 122, 55, 75, 53, 107, 52, 127, 51, 132, 50, 80, 48, 103, 47, 119, 46, 129, 45, 77, 44, 90, 43, 100, 42, 113, 41, 94, 40, 108, 39, 118, 38, 126, 37, 73, 36, 87, 35, 104, 33, 69, 32, 89, 31, 99, 30, 109, 29, 123, 28, 106, 27, 84, 26, 97, 25, 120, 24, 82, 23, 74, 22, 88, 21, 93, 20, 121, 19, 83, 18, 78, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 70],
	#[],
	#[],
	#[58, 302],
	#[56, 301],
	#[],
	#[60, 300],
	#[60, 299],
	#[],
	#[],
	#[56, 298],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[181, 271, 180, 272, 179, 273, 172, 289, 166, 285, 165, 307, 155, 274, 138, 161, 134, 277, 125, 3, 124, 269, 121, 281, 120, 282, 118, 286, 92, 280, 89, 59, 88, 51, 87, 56, 83, 19, 81, 29, 76, 288, 75, 1, 72, 276, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 140, 57, 287, 55, 268, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 270, 44, 275, 43, 279, 42, 284, 41, 167, 40, 283, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 267],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[56, 313],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[56, 320],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[180, 272, 118, 286, 120, 282, 124, 269, 125, 3, 181, 271, 155, 274, 179, 273, 121, 281, 166, 285, 138, 161, 92, 280, 165, 328, 134, 277, 172, 289, 88, 51, 89, 59, 81, 29, 76, 288, 75, 1, 72, 276, 83, 19, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 181, 62, 190, 61, 155, 59, 140, 87, 56, 57, 287, 55, 268, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 270, 44, 275, 43, 279, 42, 284, 41, 167, 40, 283, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 30, 42, 29, 15, 28, 2, 27, 11, 26, 36, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 12, 76, 10, 267],
	#[194, 330, 31, 329],
	#[83, 230, 81, 233, 79, 332, 78, 331, 76, 235, 75, 1, 34, 333, 33, 55, 29, 334, 28, 2, 23, 66, 22, 9, 21, 52, 20, 34, 18, 61],
	#[],
	#[75, 1, 83, 230, 21, 52, 18, 61, 76, 235, 20, 34, 34, 31, 33, 55, 28, 2, 81, 233, 22, 9, 29, 15, 23, 66, 79, 335],
	#[],
	#[],
	#[],
	#[],
	#[129, 337, 31, 226],
	#[],
	#[146, 222, 123, 221, 122, 339, 83, 220, 81, 224, 76, 63, 75, 1, 53, 38, 34, 31, 33, 55, 29, 15, 28, 2, 23, 66, 22, 9, 21, 52, 20, 34, 18, 61],
	#[],
	#[129, 341, 31, 226],
	#[],
	#[],
	#[],
	#[57, 349, 55, 350, 46, 351],
	#[56, 346],
	#[],
	#[154, 10, 153, 348, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 130, 48, 146, 47, 155, 8, 90, 41, 89, 59, 81, 29, 68, 24, 66, 64, 62, 58, 65, 45, 75, 1, 83, 19, 43, 30, 33, 55, 36, 14, 67, 49, 24, 53, 41, 20, 69, 39, 28, 2, 29, 15, 42, 46, 76, 63, 18, 61, 19, 50, 63, 44, 61, 4, 20, 34, 21, 52, 34, 31, 22, 9, 55, 5, 53, 38, 12, 28, 23, 66, 25, 54, 35, 37, 13, 16],
	#[],
	#[155, 8, 154, 359, 152, 353, 151, 356, 146, 47, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 133, 357, 132, 358, 131, 367, 130, 48, 90, 41, 89, 59, 83, 19, 81, 29, 76, 63, 75, 1, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 63, 44, 62, 58, 61, 4, 55, 5, 53, 38, 43, 30, 42, 46, 41, 355, 36, 14, 35, 37, 34, 31, 33, 55, 29, 15, 28, 2, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 13, 16, 12, 28],
	#[152, 353, 151, 356, 146, 47, 143, 354, 155, 8, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 133, 357, 132, 358, 131, 360, 130, 48, 154, 359, 89, 59, 65, 45, 90, 41, 66, 64, 81, 29, 83, 19, 43, 30, 29, 15, 28, 2, 41, 355, 55, 5, 76, 63, 24, 53, 35, 37, 21, 52, 42, 46, 36, 14, 25, 54, 68, 24, 62, 58, 53, 38, 13, 16, 69, 39, 75, 1, 18, 61, 19, 50, 63, 44, 61, 4, 20, 34, 67, 49, 34, 31, 22, 9, 33, 55, 12, 28, 23, 66],
	#[146, 352, 76, 63, 75, 1, 34, 31, 33, 55, 21, 52, 20, 34, 53, 38],
	#[],
	#[91, 364, 39, 118, 38, 126, 37, 73, 36, 87],
	#[56, 366],
	#[155, 8, 154, 10, 153, 13, 150, 26, 149, 363, 146, 47, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 130, 48, 90, 41, 89, 59, 83, 19, 81, 29, 75, 1, 69, 39, 68, 24, 66, 64, 65, 45, 76, 63, 67, 49, 53, 38, 43, 30, 61, 4, 41, 20, 62, 58, 36, 14, 35, 37, 34, 31, 33, 55, 42, 46, 29, 15, 28, 2, 55, 5, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 13, 16, 12, 28, 63, 44],
	#[],
	#[],
	#[40, 361],
	#[],
	#[],
	#[155, 8, 154, 359, 152, 353, 151, 356, 146, 47, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 133, 362, 130, 48, 90, 41, 89, 59, 83, 19, 81, 29, 76, 63, 75, 1, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 63, 44, 62, 58, 61, 4, 55, 5, 43, 30, 42, 46, 41, 355, 36, 14, 35, 37, 34, 31, 33, 55, 29, 15, 28, 2, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 53, 38, 13, 16, 12, 28],
	#[],
	#[],
	#[136, 35, 137, 18, 146, 47, 154, 10, 153, 365, 130, 48, 135, 25, 138, 12, 155, 8, 90, 41, 89, 59, 134, 40, 83, 19, 81, 29, 75, 1, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 62, 58, 61, 4, 76, 63, 55, 5, 53, 38, 43, 30, 42, 46, 41, 20, 36, 14, 35, 37, 34, 31, 33, 55, 29, 15, 28, 2, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 13, 16, 12, 28, 63, 44],
	#[],
	#[],
	#[58, 368],
	#[],
	#[],
	#[],
	#[63, 373],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[47, 383],
	#[2, 382],
	#[],
	#[155, 8, 147, 385, 146, 47, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 130, 384, 89, 59, 83, 19, 81, 29, 76, 63, 75, 1, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 63, 44, 62, 58, 61, 4, 55, 5, 53, 38, 43, 30, 42, 46, 34, 31, 33, 55, 29, 15, 28, 2, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 13, 16, 12, 28],
	#[46, 351, 57, 349, 55, 350],
	#[],
	#[],
	#[],
	#[155, 394, 138, 161, 134, 170, 120, 176, 119, 188, 118, 184, 117, 397, 116, 398, 108, 396, 102, 395, 92, 172, 83, 19, 81, 29, 75, 1, 72, 166, 69, 39, 68, 24, 66, 64, 65, 45, 63, 181, 61, 155, 59, 168, 55, 157, 54, 178, 53, 191, 50, 160, 49, 179, 48, 180, 45, 163, 44, 165, 43, 171, 41, 167, 89, 59, 39, 174, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 57, 185, 28, 2, 23, 66, 22, 9, 21, 52, 42, 182, 24, 53, 18, 61, 17, 95, 16, 101, 14, 81, 13, 91, 12, 76, 25, 54, 29, 15, 20, 34, 67, 49, 52, 192, 51, 195, 38, 153, 76, 194, 47, 187, 15, 79, 19, 50, 62, 190, 46, 193],
	#[],
	#[2, 403],
	#[40, 392],
	#[101, 391, 100, 393, 41, 388],
	#[],
	#[138, 161, 134, 170, 120, 176, 119, 188, 118, 184, 117, 397, 68, 24, 75, 1, 48, 180, 72, 166, 46, 193, 45, 163, 44, 165, 43, 171, 61, 155, 41, 167, 89, 59, 50, 160, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 59, 168, 32, 164, 54, 178, 65, 45, 57, 185, 49, 179, 55, 157, 25, 54, 92, 172, 66, 64, 21, 52, 42, 182, 24, 53, 47, 187, 17, 95, 16, 101, 53, 191, 14, 81, 13, 91, 12, 76, 115, 402, 39, 174, 76, 194, 19, 50, 63, 181, 20, 34, 67, 49, 52, 192, 51, 195, 33, 55, 116, 401, 62, 190, 15, 79],
	#[],
	#[],
	#[155, 394, 138, 161, 134, 170, 120, 176, 119, 188, 118, 184, 117, 397, 116, 398, 108, 399, 107, 400, 92, 172, 89, 59, 83, 19, 81, 29, 76, 194, 75, 1, 72, 166, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 63, 181, 62, 190, 61, 155, 59, 168, 57, 185, 55, 157, 54, 178, 53, 191, 52, 192, 51, 195, 50, 160, 49, 179, 48, 180, 47, 187, 46, 193, 45, 163, 44, 165, 43, 171, 42, 182, 41, 167, 39, 174, 38, 153, 37, 156, 36, 169, 35, 175, 34, 31, 33, 55, 32, 164, 29, 15, 28, 2, 25, 54, 24, 53, 23, 66, 22, 9, 21, 52, 19, 50, 18, 61, 17, 95, 16, 101, 15, 79, 14, 81, 13, 91, 20, 34, 12, 76],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[2, 405],
	#[],
	#[2, 413],
	#[144, 410, 10, 411],
	#[],
	#[],
	#[],
	#[155, 8, 153, 13, 150, 26, 149, 32, 146, 47, 154, 10, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 125, 3, 124, 7, 121, 33, 130, 48, 98, 412, 90, 41, 88, 51, 87, 56, 89, 59, 81, 29, 75, 1, 83, 19, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 63, 44, 62, 58, 76, 63, 55, 5, 53, 38, 43, 30, 35, 37, 41, 20, 33, 55, 27, 11, 25, 54, 24, 53, 61, 4, 22, 9, 21, 52, 36, 14, 18, 61, 26, 36, 13, 16, 12, 28, 28, 2, 29, 15, 42, 46, 30, 42, 19, 50, 34, 31, 23, 66, 20, 34],
	#[],
	#[],
	#[2, 415],
	#[],
	#[2, 417],
	#[],
	#[153, 13, 138, 12, 137, 18, 136, 35, 130, 48, 155, 8, 150, 26, 146, 47, 154, 10, 134, 40, 135, 25, 83, 19, 76, 63, 81, 29, 149, 446, 69, 39, 68, 24, 67, 49, 66, 64, 65, 45, 63, 44, 75, 1, 61, 4, 90, 41, 55, 5, 89, 59, 53, 38, 43, 30, 42, 46, 62, 58, 36, 14, 35, 37, 34, 31, 33, 55, 29, 15, 28, 2, 25, 54, 24, 53, 22, 9, 21, 52, 20, 34, 19, 50, 18, 61, 12, 28, 13, 16, 41, 20, 23, 66],
	#[164, 444, 48, 445],
	#[40, 441],
	#[48, 443],
	#[153, 13, 137, 18, 136, 35, 135, 25, 134, 40, 154, 10, 149, 32, 155, 8, 138, 12, 125, 3, 124, 7, 121, 33, 130, 48, 162, 438, 146, 47, 150, 26, 95, 432, 97, 437, 88, 51, 87, 56, 89, 59, 81, 29, 90, 41, 68, 24, 67, 49, 66, 64, 65, 45, 64, 67, 75, 1, 76, 63, 83, 19, 55, 5, 98, 408, 53, 38, 35, 37, 41, 20, 25, 54, 24, 53, 61, 4, 22, 9, 36, 14, 18, 61, 43, 30, 62, 58, 26, 36, 27, 11, 13, 16, 69, 39, 28, 2, 10, 436, 42, 46, 30, 42, 20, 34, 21, 52, 34, 31, 33, 55, 12, 28, 23, 66, 19, 50, 29, 15, 63, 44],
	#[],
	#[162, 430, 160, 431, 155, 8, 153, 13, 150, 26, 149, 32, 146, 47, 154, 10, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 130, 48, 125, 3, 124, 7, 121, 33, 98, 408, 97, 429, 90, 41, 89, 59, 88, 51, 87, 56, 81, 29, 76, 63, 75, 1, 69, 39, 68, 24, 67, 49, 95, 432, 64, 67, 62, 58, 65, 45, 41, 20, 33, 55, 83, 19, 43, 30, 27, 11, 55, 5, 25, 54, 22, 9, 21, 52, 42, 46, 36, 14, 18, 61, 26, 36, 24, 53, 13, 16, 34, 31, 28, 2, 10, 428, 35, 37, 30, 42, 19, 50, 63, 44, 61, 4, 20, 34, 66, 64, 53, 38, 12, 28, 23, 66, 29, 15],
	#[],
	#[2, 427],
	#[],
	#[159, 424, 155, 8, 153, 13, 150, 26, 149, 420, 148, 421, 161, 423, 137, 18, 136, 35, 135, 25, 134, 40, 154, 10, 130, 48, 138, 12, 163, 422, 146, 47, 158, 452, 90, 41, 81, 29, 76, 63, 65, 45, 83, 19, 55, 418, 89, 59, 53, 38, 35, 37, 25, 54, 24, 53, 75, 1, 22, 9, 36, 14, 18, 61, 32, 419, 62, 58, 67, 49, 12, 28, 28, 2, 29, 15, 42, 46, 41, 20, 63, 44, 61, 4, 20, 34, 21, 52, 34, 31, 68, 24, 33, 55, 13, 16, 69, 39, 43, 30, 66, 64, 19, 50, 23, 66],
	#[144, 410, 10, 433],
	#[],
	#[],
	#[],
	#[163, 422, 161, 423, 159, 424, 158, 435, 155, 8, 154, 10, 153, 13, 150, 26, 149, 434, 148, 421, 146, 47, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 130, 48, 125, 3, 124, 7, 121, 33, 98, 412, 87, 56, 76, 63, 81, 29, 83, 19, 65, 45, 69, 39, 25, 54, 22, 9, 88, 51, 32, 419, 62, 58, 27, 11, 13, 16, 12, 28, 28, 2, 75, 1, 35, 37, 30, 42, 61, 4, 20, 34, 68, 24, 42, 46, 55, 418, 53, 38, 24, 53, 26, 36, 41, 20, 29, 15, 34, 31, 33, 55, 21, 52, 90, 41, 64, 67, 89, 59, 63, 44, 19, 50, 43, 30, 18, 61, 36, 14, 67, 49, 66, 64, 23, 66],
	#[40, 441],
	#[],
	#[163, 451, 155, 8, 154, 10, 153, 13, 150, 26, 149, 420, 148, 421, 146, 47, 137, 18, 136, 35, 135, 25, 134, 40, 138, 12, 130, 48, 89, 59, 32, 419, 66, 64, 41, 20, 33, 55, 53, 38, 19, 50, 43, 30, 65, 45, 42, 46, 90, 41, 83, 19, 28, 2, 63, 44, 13, 16, 76, 63, 29, 15, 75, 1, 35, 37, 34, 31, 61, 4, 23, 66, 21, 52, 12, 28, 24, 53, 36, 14, 67, 49, 69, 39, 81, 29, 55, 418, 68, 24, 22, 9, 25, 54, 62, 58, 18, 61, 20, 34],
	#[144, 410, 10, 439],
	#[],
	#[163, 440, 155, 8, 154, 10, 153, 13, 150, 26, 146, 47, 138, 12, 137, 18, 136, 35, 135, 25, 134, 40, 148, 421, 149, 434, 125, 3, 124, 7, 121, 33, 130, 48, 98, 412, 89, 59, 55, 418, 35, 37, 88, 51, 32, 419, 83, 19, 43, 30, 28, 2, 27, 11, 26, 36, 90, 41, 75, 1, 21, 52, 41, 20, 36, 14, 81, 29, 68, 24, 65, 45, 53, 38, 24, 53, 13, 16, 12, 28, 25, 54, 29, 15, 42, 46, 76, 63, 30, 42, 19, 50, 63, 44, 61, 4, 20, 34, 67, 49, 34, 31, 87, 56, 22, 9, 33, 55, 18, 61, 23, 66, 69, 39, 66, 64, 64, 67, 62, 58],
	#[],
	#[155, 8, 154, 10, 153, 13, 150, 26, 146, 47, 138, 12, 137, 18, 136, 35, 135, 25, 148, 442, 130, 48, 89, 59, 28, 2, 90, 41, 66, 64, 24, 53, 81, 29, 65, 45, 18, 61, 19, 50, 61, 4, 20, 34, 21, 52, 41, 20, 53, 38, 12, 28, 13, 16, 62, 58, 34, 31, 42, 46, 33, 55, 68, 24, 83, 19, 43, 30, 63, 44, 76, 63, 134, 40, 29, 15, 75, 1, 35, 37, 25, 54, 36, 14, 23, 66, 67, 49, 149, 420, 22, 9, 55, 5, 69, 39],
	#[],
	#[],
	#[],
	#[],
	#[40, 447, 56, 346],
	#[138, 12, 137, 18, 136, 35, 130, 48, 150, 26, 146, 47, 154, 10, 153, 13, 155, 8, 148, 448, 134, 40, 135, 25, 149, 420, 90, 41, 89, 59, 83, 19, 81, 29, 76, 63, 75, 1, 67, 49, 62, 58, 61, 4, 66, 64, 65, 45, 69, 39, 43, 30, 41, 20, 36, 14, 35, 37, 53, 38, 33, 55, 42, 46, 29, 15, 25, 54, 22, 9, 21, 52, 68, 24, 12, 28, 24, 53, 18, 61, 19, 50, 63, 44, 55, 5, 13, 16, 34, 31, 28, 2, 20, 34, 23, 66],
	#[56, 449],
	#[48, 450],
	#[],
	#[],
	#[]],
  action-function-table:
	 vector(dylan-parser-action0,
		dylan-parser-action1,
		dylan-parser-action1,
		dylan-parser-action1,
		dylan-parser-action1,
		dylan-parser-action1,
		dylan-parser-action1,
		dylan-parser-action1,
		dylan-parser-action1,
		dylan-parser-action9,
		dylan-parser-action9,
		dylan-parser-action11,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action80,
		dylan-parser-action0,
		dylan-parser-action82,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action85,
		dylan-parser-action86,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action93,
		dylan-parser-action94,
		dylan-parser-action0,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action94,
		dylan-parser-action94,
		dylan-parser-action94,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action146,
		dylan-parser-action147,
		dylan-parser-action148,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action154,
		dylan-parser-action154,
		dylan-parser-action0,
		dylan-parser-action157,
		dylan-parser-action158,
		dylan-parser-action159,
		dylan-parser-action160,
		dylan-parser-action161,
		dylan-parser-action0,
		dylan-parser-action163,
		dylan-parser-action164,
		dylan-parser-action165,
		dylan-parser-action0,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action157,
		dylan-parser-action94,
		dylan-parser-action171,
		dylan-parser-action172,
		dylan-parser-action173,
		dylan-parser-action174,
		dylan-parser-action175,
		dylan-parser-action176,
		dylan-parser-action0,
		dylan-parser-action178,
		dylan-parser-action85,
		dylan-parser-action86,
		dylan-parser-action181,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action184,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action1,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action199,
		dylan-parser-action200,
		dylan-parser-action201,
		dylan-parser-action202,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action178,
		dylan-parser-action85,
		dylan-parser-action86,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action157,
		dylan-parser-action217,
		dylan-parser-action0,
		dylan-parser-action219,
		dylan-parser-action0,
		dylan-parser-action157,
		dylan-parser-action158,
		dylan-parser-action223,
		dylan-parser-action224,
		dylan-parser-action225,
		dylan-parser-action223,
		dylan-parser-action0,
		dylan-parser-action225,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action232,
		dylan-parser-action233,
		dylan-parser-action234,
		dylan-parser-action235,
		dylan-parser-action236,
		dylan-parser-action9,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action240,
		dylan-parser-action0,
		dylan-parser-action242,
		dylan-parser-action243,
		dylan-parser-action94,
		dylan-parser-action245,
		dylan-parser-action0,
		dylan-parser-action240,
		dylan-parser-action157,
		dylan-parser-action157,
		dylan-parser-action83,
		dylan-parser-action251,
		dylan-parser-action252,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action157,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action94,
		dylan-parser-action94,
		dylan-parser-action94,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action94,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action94,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action146,
		dylan-parser-action147,
		dylan-parser-action0,
		dylan-parser-action148,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action157,
		dylan-parser-action94,
		dylan-parser-action0,
		dylan-parser-action240,
		dylan-parser-action306,
		dylan-parser-action306,
		dylan-parser-action306,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action146,
		dylan-parser-action147,
		dylan-parser-action148,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action336,
		dylan-parser-action337,
		dylan-parser-action337,
		dylan-parser-action0,
		dylan-parser-action340,
		dylan-parser-action341,
		dylan-parser-action342,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action9,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action348,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action353,
		dylan-parser-action171,
		dylan-parser-action172,
		dylan-parser-action173),
  action-nargs-table: #[1, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 2, 0, 1, 1, 3, 1, 1, 1, 0, 1, 1, 3, 2, 1, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 2, 2, 2, 2, 2, 0, 1, 2, 0, 1, 2, 2, 0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1, 1, 1, 1, 1, 2, 2, 1, 1, 3, 3, 3, 3, 1, 5, 4, 6, 1, 0, 1, 1, 2, 1, 2, 3, 4, 4, 3, 1, 1, 1, 3, 2, 1, 1, 4, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 5, 3, 3, 1, 1, 0, 1, 1, 1, 3, 1, 1, 0, 1, 0, 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 1, 2, 3, 3, 3, 2, 2, 0, 1, 2, 1, 3, 4, 2, 3, 1, 2, 1, 1, 0, 2, 6, 2, 0, 1, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 2, 2, 2, 2, 2, 0, 1, 2, 0, 1, 2, 2, 0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 3, 0, 1, 1, 2, 1, 2, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 4, 2, 2, 1, 2, 3, 2, 0, 1, 2, 0, 1, 2, 1, 1, 1, 1, 3, 1, 2, 3],
  action-nt-table: #[70, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 72, 72, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 74, 74, 74, 75, 75, 76, 76, 76, 77, 77, 78, 78, 79, 79, 79, 80, 80, 81, 81, 81, 82, 82, 83, 83, 84, 84, 85, 85, 85, 86, 86, 86, 87, 88, 89, 89, 89, 90, 90, 91, 91, 91, 91, 92, 92, 92, 92, 92, 93, 94, 94, 95, 96, 96, 97, 97, 98, 98, 98, 99, 99, 100, 100, 101, 102, 103, 103, 104, 104, 105, 105, 106, 106, 107, 107, 108, 108, 109, 109, 110, 110, 110, 110, 110, 111, 111, 112, 113, 113, 114, 114, 115, 115, 116, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 119, 119, 119, 120, 120, 120, 120, 120, 121, 121, 121, 122, 122, 123, 123, 123, 124, 125, 125, 125, 126, 127, 127, 128, 128, 129, 129, 129, 130, 130, 130, 130, 131, 132, 132, 133, 133, 133, 134, 135, 135, 135, 135, 135, 135, 135, 136, 136, 136, 136, 136, 136, 137, 137, 138, 138, 138, 138, 138, 139, 139, 140, 141, 141, 142, 142, 143, 143, 144, 144, 145, 145, 146, 146, 147, 148, 148, 149, 150, 150, 151, 152, 152, 153, 153, 154, 154, 155, 155, 155, 156, 156, 157, 157, 158, 158, 159, 159, 160, 160, 160, 161, 162, 162, 162, 163, 163, 163, 164, 164, 165, 165, 166, 166, 167, 167, 168, 168, 169, 169, 170, 170, 171, 171, 172, 172, 172, 172, 172, 173, 173, 174, 175, 175, 176, 176, 177, 177, 178, 179, 179, 179, 179, 179, 179, 179, 179, 179, 179, 179, 180, 180, 180, 181, 182, 182, 183, 183, 183, 183, 184, 184, 184, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 186, 186, 187, 187, 187, 188, 188, 188, 188, 188, 188, 188, 189, 189, 190, 191, 191, 192, 193, 193, 193, 193, 193, 194, 194, 194]);
