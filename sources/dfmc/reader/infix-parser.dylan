Module: dfmc-reader
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

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

define function dylan-parser-action76 () => (value)
  empty-body-fragment()
end dylan-parser-action76;

define function dylan-parser-action78 (arg$1, arg$2) => (value)
  body-fragment(elements(arg$1))
end dylan-parser-action78;

define function dylan-parser-action79 () => (value)
  #()
end dylan-parser-action79;

define function dylan-parser-action81 (arg$1) => (value)
  append-sequence(arg$1)
end dylan-parser-action81;

define function dylan-parser-action82 (arg$1, arg$2, arg$3) => (value)
  append-element!(arg$1, arg$3)
end dylan-parser-action82;

define function dylan-parser-action89 (arg$1, arg$2, arg$3) => (value)
  concatenate(arg$1, pair(arg$2, arg$3))
end dylan-parser-action89;

define function dylan-parser-action90 (arg$1, arg$2) => (value)
  pair(arg$1, arg$2)
end dylan-parser-action90;

define function dylan-parser-action142 (arg$1, arg$2, arg$3) => (value)
  make(<parens-fragment>, 
              record:           fragment-record(arg$1),
	      source-position:  position-between(arg$1, arg$3),
	      left-delimiter:   arg$1,
	      nested-fragments: arg$2,
	      right-delimiter:  arg$3)
end dylan-parser-action142;

define function dylan-parser-action143 (arg$1, arg$2, arg$3) => (value)
  make(<brackets-fragment>, 
              record:           fragment-record(arg$1),
	      source-position:  position-between(arg$1, arg$3),
	      left-delimiter:   arg$1,
	      nested-fragments: arg$2,
	      right-delimiter:  arg$3)
end dylan-parser-action143;

define function dylan-parser-action144 (arg$1, arg$2, arg$3) => (value)
  make(<braces-fragment>, 
              record:           fragment-record(arg$1),
	      source-position:  position-between(arg$1, arg$3),
	      left-delimiter:   arg$1,
	      nested-fragments: arg$2,
	      right-delimiter:  arg$3)
end dylan-parser-action144;

define function dylan-parser-action150 (arg$1, arg$2) => (value)
  make(<local-declaration-fragment>,
              record:          fragment-record(arg$1),
	      source-position: position-between(arg$1, arg$2),
              macro: arg$1,
              list-fragment: arg$2)
end dylan-parser-action150;

define function dylan-parser-action153 (arg$1) => (value)
  list(arg$1)
end dylan-parser-action153;

define function dylan-parser-action154 (arg$1, arg$2, arg$3) => (value)
  pair(arg$1, pair(arg$2, arg$3))
end dylan-parser-action154;

define function dylan-parser-action155 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-definition-tail
	     (arg$1, arg$1, maybe-defined-name(arg$2), arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
  	        source-position: position-between(arg$1, arg$3),
	        macro: arg$1,
                body-fragment: arg$2);
         end
end dylan-parser-action155;

define function dylan-parser-action156 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-definition-tail
	     (arg$1, arg$1, maybe-defined-name(arg$2), arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
	        source-position: position-between(arg$1, arg$3),
	        macro: arg$1,
                body-fragment: arg$2);
         end
end dylan-parser-action156;

define function dylan-parser-action157 (arg$1, arg$2, arg$3) => (value)
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
end dylan-parser-action157;

define function dylan-parser-action159 (arg$1, arg$2, arg$3, arg$4, arg$5) => (value)
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
end dylan-parser-action159;

define function dylan-parser-action160 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<list-definition-fragment>,
              record:          fragment-record(arg$1),
	      source-position: position-between(arg$1, arg$4),
	      modifiers: arg$2,
	      define-word: arg$3,
	      list-fragment: arg$4)
end dylan-parser-action160;

define function dylan-parser-action161 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6) => (value)
  begin
           verify-definition-tail(arg$1, arg$3, arg$4, arg$6);
           make(<macro-body-definition-fragment>,
                record:          fragment-record(arg$1),
	        source-position: position-between(arg$1, arg$6),
	        modifiers: arg$2,
	        define-word: arg$3,
	        macro-body-fragment: pair(arg$4, arg$5));
         end
end dylan-parser-action161;

define function dylan-parser-action167 (arg$1) => (value)
  make(<definition-tail-fragment>, 
              record:          fragment-record(arg$1),
              source-position: fragment-source-position(arg$1),
              end: arg$1)
end dylan-parser-action167;

define function dylan-parser-action168 (arg$1, arg$2) => (value)
  make(<definition-tail-fragment>, 
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              end: arg$1, tail-name-1: arg$2)
end dylan-parser-action168;

define function dylan-parser-action169 (arg$1, arg$2, arg$3) => (value)
  make(<definition-tail-fragment>, 
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              end: arg$1, tail-name-1: arg$2, tail-name-2: arg$3)
end dylan-parser-action169;

define function dylan-parser-action170 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<prefix-call-fragment>, 
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$4),
              function: arg$1, arguments: arg$3)
end dylan-parser-action170;

define function dylan-parser-action171 (arg$1, arg$2, arg$3, arg$4) => (value)
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
end dylan-parser-action171;

define function dylan-parser-action172 (arg$1, arg$2, arg$3) => (value)
  make(<dot-call-fragment>, 
	      record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              function: arg$3, arguments: list(arg$1))
end dylan-parser-action172;

define function dylan-parser-action174 (arg$1) => (value)
  elements(arg$1)
end dylan-parser-action174;

define function dylan-parser-action177 (arg$1, arg$2) => (value)
  list(arg$1, arg$2)
end dylan-parser-action177;

define function dylan-parser-action180 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<function-macro-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$4),
              macro: arg$1,
              body-fragment: arg$3)
end dylan-parser-action180;

define function dylan-parser-action195 (arg$1, arg$2) => (value)
  make(<string-fragment>, 
	      record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              value: concatenate(fragment-value(arg$1), fragment-value(arg$2)))
end dylan-parser-action195;

define function dylan-parser-action196 (arg$1, arg$2, arg$3, arg$4, arg$5) => (value)
  make(<improper-list-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$5),
	      elements:        arg$2,
	      improper-tail:   arg$4)
end dylan-parser-action196;

define function dylan-parser-action197 (arg$1, arg$2, arg$3) => (value)
  make(<proper-list-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
	      elements:        arg$2)
end dylan-parser-action197;

define function dylan-parser-action198 (arg$1, arg$2, arg$3) => (value)
  make(<vector-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
	      elements:        arg$2)
end dylan-parser-action198;

define function dylan-parser-action213 (arg$1, arg$2, arg$3) => (value)
  list(arg$1, arg$2, arg$3)
end dylan-parser-action213;

define function dylan-parser-action215 (arg$1) => (value)
  make-variable-name-like
	   (arg$1, 
            record:          fragment-record(arg$1),
            source-position: fragment-source-position(arg$1),
	    name: #"...")
end dylan-parser-action215;

define function dylan-parser-action219 (arg$1) => (value)
  binop-fragment(arg$1)
end dylan-parser-action219;

define function dylan-parser-action220 (arg$1) => (value)
  (arg$1)
end dylan-parser-action220;

define function dylan-parser-action221 (arg$1, arg$2, arg$3) => (value)
  append-binop!(arg$1, arg$2, arg$3)
end dylan-parser-action221;

define function dylan-parser-action228 (arg$1, arg$2) => (value)
  make(<unary-operator-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              function: arg$1,
              arguments: list(arg$2))
end dylan-parser-action228;

define function dylan-parser-action229 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-statement-tail(arg$1, arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
  	        source-position: position-between(arg$1, arg$3),
	        macro: arg$1,
                body-fragment: arg$2);
         end
end dylan-parser-action229;

define function dylan-parser-action230 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-statement-tail(arg$1, arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
	        source-position: position-between(arg$1, arg$3),
	        macro: arg$1,
                body-fragment: arg$2);
         end
end dylan-parser-action230;

define function dylan-parser-action231 (arg$1, arg$2, arg$3) => (value)
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
end dylan-parser-action231;

define function dylan-parser-action232 (arg$1, arg$2) => (value)
  arg$2 | arg$1
end dylan-parser-action232;

define function dylan-parser-action236 (arg$1, arg$2) => (value)
  concatenate(arg$1, arg$2)
end dylan-parser-action236;

define function dylan-parser-action238 (arg$1, arg$2, arg$3) => (value)
  concatenate(arg$1, list(arg$2), arg$3)
end dylan-parser-action238;

define function dylan-parser-action239 (arg$1, arg$2, arg$3, arg$4) => (value)
  concatenate(arg$1, list(body-fragment(elements(arg$2))), list(arg$3), arg$4)
end dylan-parser-action239;

define function dylan-parser-action241 (arg$1, arg$2, arg$3) => (value)
  pair(body-fragment(elements(arg$1)), pair(arg$2, arg$3))
end dylan-parser-action241;

define function dylan-parser-action247 (arg$1, arg$2) => (value)
  concatenate(arg$1, list(arg$2))
end dylan-parser-action247;

define function dylan-parser-action248 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6) => (value)
  pair(make(<parens-fragment>, 
                   record:           fragment-record(arg$1),
	           source-position:  position-between(arg$1, arg$5),
	           left-delimiter:   arg$1,
	           nested-fragments: pair(arg$2, pair(arg$3, arg$4)),
	           right-delimiter:  arg$5),
              list(arg$6))
end dylan-parser-action248;

define function dylan-parser-action302 (arg$1, arg$2, arg$3) => (value)
  pair(arg$1, concatenate(arg$2, list(arg$3)))
end dylan-parser-action302;

define function dylan-parser-action332 (arg$1, arg$2, arg$3, arg$4) => (value)
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
end dylan-parser-action332;

define function dylan-parser-action333 (arg$1, arg$2) => (value)
  make(<sequence-pattern-variable-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              name:            arg$2,
              separator:       #f)
end dylan-parser-action333;

define function dylan-parser-action336 (arg$1, arg$2) => (value)
  make(<unhygienic-name-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              name:            arg$2)
end dylan-parser-action336;

define function dylan-parser-action337 (arg$1, arg$2, arg$3) => (value)
  make(<template-aux-rule-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              rule-name:       arg$2,
              template:        arg$3)
end dylan-parser-action337;

define function dylan-parser-action338 (arg$1, arg$2) => (value)
  make(<template-macro-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              template:        arg$2)
end dylan-parser-action338;

define function dylan-parser-action344 (arg$1, arg$2) => (value)
  arg$2
end dylan-parser-action344;

define function dylan-parser-action349 (arg$1, arg$2, arg$3) => (value)
  arg$1
end dylan-parser-action349;

define constant dylan-parser :: <parser>
  = make(<parser>,
  action-table:
      #[#[3, -36, 4, -47, 5, -40, 6, -24, 11, -8, 12, -43, 13, -66, 22, -32, 23, -50, 26, -30, 28, -63, 53, -17, 55, -14, 29, -13, 19, -58, 30, -28, 7, -15, 21, -16, 61, -7, 0, -10, 9, -38, 66, -48, 34, -51, 43, -18, 18, -34, 33, -29, 41, -27, 35, -53, 65, -33, 63, -41, 8, -31, 68, -55, 67, -37, 36, -64, 42, -61, 69, -56, 64, -23, 27, -46, 62, -22, 20, -5],
	#[#"eoi", #"accept"],
	#[65535, 214, 2, 214, 18, 214, 10, 214, 11, 214, 12, 214, 13, 214, 14, 214, 15, 214, 16, 214, 17, 214, 47, 214, 19, 214, 20, 214, 21, 214, 22, 214, 23, 214, 26, 214, 27, 214, 28, 214, 29, 214, 30, 214, 31, 214, 32, 214, 33, 214, 34, 214, 35, 214, 36, 214, 37, 214, 38, 214, 39, 214, 40, 214, 41, 214, 42, 214, 43, 214, 44, 214, 45, 214, 46, 214, 58, 214, 48, 214, 49, 214, 50, 214, 51, 214, 52, 214, 53, 214, 54, 214, 55, 214, 56, 214, 57, 214, 65, 214, 59, 214, 61, 214, 62, 214, 63, 214, 64, 214, 66, 214, 67, 214, 68, 214, 69, 214],
	#[55, -197],
	#[65535, 34, 2, 34, 10, 34, 11, 34, 12, 34, 13, 34, 14, 34, 15, 34, 16, 34, 17, 34, 18, 34, 19, 34, 20, 34, 21, 34, 22, 34, 23, 34, 26, 34, 27, 34, 28, 34, 29, 34, 30, 34, 31, 34, 32, 34, 33, 34, 34, 34, 35, 34, 36, 34, 37, 34, 38, 34, 39, 34, 40, 34, 41, 34, 42, 34, 43, 34, 44, 34, 45, 34, 46, 34, 47, 34, 48, 34, 49, 34, 50, 34, 51, 34, 52, 34, 53, 34, 54, 34, 55, 34, 56, 34, 57, 34, 58, 34, 59, 34, 60, 34, 61, 34, 62, 34, 63, 34, 64, 34, 65, 34, 66, 34, 67, 34, 68, 34, 69, 34],
	#[65535, 220, 2, 220, 10, 220, 11, 220, 36, 220, 37, 220, 38, 220, 39, 220, 40, 220, 48, 220, 56, 220, 58, 220],
	#[65535, 188, 2, 188, 10, 188, 11, 188, 36, 188, 37, 188, 38, 188, 39, 188, 40, 188, 46, 188, 48, 188, 55, 188, 56, 188, 57, 188, 58, 188],
	#[65535, 11, #"eoi", 11],
	#[65535, 158, 2, 158, 10, 158, 11, 158, 31, 158, 56, 158, 58, 158, 60, 158],
	#[1, -448],
	#[65535, 190, 39, 190, 10, 190, 2, 190, 38, 190, 55, 190, 37, 190, 46, 190, 36, 190, 40, 190, 11, 190, 58, 190, 57, 190, 56, 190, 48, 190],
	#[15, -142, 14, -118, 31, 92, 39, -205, 21, -16, 20, -5, 10, -238, 43, -211, 38, -199, 23, -50, 12, -157, 22, -32, 36, -239, 17, -107, 26, -30, 33, -29, 19, -58, 30, -28, 42, -236, 29, -13, 28, -63, 34, -51, 13, -108, 27, -46, 40, -215, 16, -137, 50, -223, 18, -34, 49, -212, 35, -229, 61, -203, 69, -56, 32, -237, 48, -201, 41, -218, 37, -208, 51, -230, 62, -213, 44, -222, 45, -214, 46, -224, 47, -233, 68, -55, 52, -198, 53, -210, 54, -221, 55, -207, 57, -232, 65, -33, 59, -219, 63, -225, 64, -23, 66, -48, 67, -37],
	#[65535, 54, 2, 54, 10, 54, 11, 54, 12, 54, 13, 54, 14, 54, 15, 54, 16, 54, 17, 54, 18, 54, 19, 54, 20, 54, 21, 54, 22, 54, 23, 54, 26, 54, 27, 54, 28, 54, 29, 54, 30, 54, 31, 54, 32, 54, 33, 54, 34, 54, 35, 54, 36, 54, 37, 54, 38, 54, 39, 54, 40, 54, 41, 54, 42, 54, 43, 54, 44, 54, 45, 54, 46, 54, 47, 54, 48, 54, 49, 54, 50, 54, 51, 54, 52, 54, 53, 54, 54, 54, 55, 54, 56, 54, 57, 54, 58, 54, 59, 54, 60, 54, 61, 54, 62, 54, 63, 54, 64, 54, 65, 54, 66, 54, 67, 54, 68, 54, 69, 54],
	#[21, -16, 23, -50, 13, -66, 35, -53, 62, -22, 28, -63, 12, -43, 53, -17, 55, -14, 33, -29, 22, -32, 68, -55, 34, -51, 36, -64, 20, -5, 43, -18, 63, -41, 19, -58, 42, -61, 29, -13, 66, -48, 69, -56, 41, -27, 67, -37, 65, -33, 18, -34, 61, -7],
	#[35, -53, 28, -63, 12, -43, 21, -16, 23, -50, 2, 234, 62, -22, 29, -13, 18, -34, 22, -32, 53, -17, 43, -18, 19, -58, 33, -29, 36, -64, 20, -5, 42, -61, 66, -48, 34, -51, 13, -66, 55, -413, 41, -27, 68, -55, 63, -41, 32, -420, 69, -56, 67, -37, 65, -33, 61, -7],
	#[65535, 35, 2, 35, 10, 35, 11, 35, 12, 35, 13, 35, 14, 35, 15, 35, 16, 35, 17, 35, 18, 35, 19, 35, 20, 35, 21, 35, 22, 35, 23, 35, 26, 35, 27, 35, 28, 35, 29, 35, 30, 35, 31, 35, 32, 35, 33, 35, 34, 35, 35, 35, 36, 35, 37, 35, 38, 35, 39, 35, 40, 35, 41, 35, 42, 35, 43, 35, 44, 35, 45, 35, 46, 35, 47, 35, 48, 35, 49, 35, 50, 35, 51, 35, 52, 35, 53, 35, 54, 35, 55, 35, 56, 35, 57, 35, 58, 35, 59, 35, 60, 35, 61, 35, 62, 35, 63, 35, 64, 35, 65, 35, 66, 35, 67, 35, 68, 35, 69, 35],
	#[65535, 215, 14, 215, 2, 215, 15, 215, 10, 215, 17, 215, 29, 215, 23, 215, 12, 215, 18, 215, 55, 215, 11, 215, 22, 215, 31, 215, 20, 215, 26, 215, 19, 215, 30, 215, 39, 215, 28, 215, 33, 215, 13, 215, 27, 215, 38, 215, 16, 215, 32, 215, 37, 215, 36, 215, 40, 215, 21, 215, 35, 215, 34, 215, 41, 215, 42, 215, 43, 215, 44, 215, 45, 215, 46, 215, 47, 215, 48, 215, 49, 215, 50, 215, 51, 215, 52, 215, 53, 215, 54, 215, 66, 215, 56, 215, 57, 215, 58, 215, 59, 215, 61, 215, 62, 215, 63, 215, 64, 215, 65, 215, 67, 215, 68, 215, 69, 215],
	#[65535, 187, 2, 187, 10, 187, 11, 187, 36, 187, 37, 187, 38, 187, 39, 187, 40, 187, 46, 187, 48, 187, 55, 187, 56, 187, 57, 187, 58, 187],
	#[10, -412, 11, -411],
	#[65535, 37, 14, 37, 20, 37, 13, 37, 15, 37, 2, 37, 11, 37, 21, 37, 23, 37, 10, 37, 19, 37, 17, 37, 38, 37, 16, 37, 29, 37, 31, 37, 27, 37, 22, 37, 46, 37, 12, 37, 37, 37, 39, 37, 26, 37, 35, 37, 30, 37, 18, 37, 49, 37, 32, 37, 45, 37, 47, 37, 40, 37, 33, 37, 41, 37, 62, 37, 28, 37, 50, 37, 53, 37, 55, 37, 42, 37, 51, 37, 68, 37, 34, 37, 36, 37, 58, 37, 43, 37, 63, 37, 56, 37, 59, 37, 54, 37, 60, 37, 44, 37, 66, 37, 69, 37, 48, 37, 64, 37, 57, 37, 65, 37, 52, 37, 67, 37, 61, 37],
	#[18, -34, 20, -5, 21, -16, 22, -32, 23, -50, 28, -63, 29, -13, 33, -29, 34, -51, 53, -17],
	#[65535, 189, 39, 189, 10, 189, 2, 189, 38, 189, 55, 189, 37, 189, 46, 189, 36, 189, 40, 189, 11, 189, 58, 189, 57, 189, 48, 189, 56, 189],
	#[65535, 152, 31, 152, 2, 152, 60, 152, 10, 152, 11, 152, 58, 152, 56, 152],
	#[2, 76, 12, -43, 13, -66, 18, -34, 19, -58, 20, -5, 21, -16, 22, -32, 23, -50, 26, -30, 27, -46, 28, -63, 29, -13, 30, -28, 33, -29, 34, -51, 35, -53, 36, -64, 41, -27, 42, -61, 43, -18, 53, -17, 55, -14, 61, -7, 62, -22, 63, -41, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 83, 2, 83, 10, 83, 11, 83],
	#[65535, 181, 2, 181, 10, 181, 11, 181, 36, 181, 37, 181, 38, 181, 39, 181, 40, 181, 46, 181, 48, 181, 56, 181, 55, 181, 57, 181, 58, 181],
	#[65535, 225, 2, 225, 10, 225, 11, 225, 36, 225, 37, 225, 38, 225, 39, 225, 40, 225, 48, 225, 56, 225, 58, 225],
	#[23, 163, 21, 163, 20, 163, 33, -300, 29, 163, 34, 163, 25, 163],
	#[65535, 36, 14, 36, 43, 36, 46, 36, 13, 36, 15, 36, 2, 36, 11, 36, 28, 36, 21, 36, 23, 36, 10, 36, 19, 36, 36, 36, 38, 36, 29, 36, 31, 36, 27, 36, 22, 36, 53, 36, 12, 36, 37, 36, 39, 36, 35, 36, 30, 36, 18, 36, 20, 36, 45, 36, 47, 36, 34, 36, 16, 36, 60, 36, 62, 36, 59, 36, 40, 36, 17, 36, 55, 36, 33, 36, 51, 36, 68, 36, 52, 36, 32, 36, 58, 36, 26, 36, 63, 36, 50, 36, 49, 36, 54, 36, 42, 36, 44, 36, 66, 36, 69, 36, 41, 36, 64, 36, 57, 36, 65, 36, 48, 36, 56, 36, 61, 36, 67, 36],
	#[65535, 61, 2, 61, 10, 61, 11, 61, 12, 61, 13, 61, 14, 61, 15, 61, 16, 61, 17, 61, 18, 61, 19, 61, 20, 61, 21, 61, 22, 61, 23, 61, 28, 61, 29, 61, 31, 61, 32, 61, 33, 61, 34, 61, 35, 61, 36, 61, 37, 61, 38, 61, 39, 61, 40, 61, 41, 61, 42, 61, 43, 61, 44, 61, 45, 61, 46, 61, 47, 61, 48, 61, 49, 61, 50, 61, 51, 61, 52, 61, 53, 61, 54, 61, 55, 61, 56, 61, 57, 61, 58, 61, 59, 61, 60, 61, 61, 61, 62, 61, 63, 61, 65, 61, 66, 61, 67, 61, 68, 61, 69, 61],
	#[2, 86, 41, -387],
	#[65535, 50, 2, 50, 10, 50, 11, 50, 12, 50, 13, 50, 14, 50, 15, 50, 16, 50, 17, 50, 18, 50, 19, 50, 20, 50, 21, 50, 22, 50, 23, 50, 26, 50, 27, 50, 28, 50, 29, 50, 30, 50, 31, 50, 32, 50, 33, 50, 34, 50, 35, 50, 36, 50, 37, 50, 38, 50, 39, 50, 40, 50, 41, 50, 42, 50, 43, 50, 44, 50, 45, 50, 46, 50, 47, 50, 48, 50, 49, 50, 50, 50, 51, 50, 52, 50, 53, 50, 54, 50, 55, 50, 56, 50, 57, 50, 58, 50, 59, 50, 60, 50, 61, 50, 62, 50, 63, 50, 64, 50, 65, 50, 66, 50, 67, 50, 68, 50, 69, 50],
	#[12, -43, 13, -66, 41, -80, 56, 201, 61, -7, 62, -22, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55],
	#[65535, 48, 14, 48, 20, 48, 13, 48, 15, 48, 2, 48, 11, 48, 21, 48, 23, 48, 10, 48, 19, 48, 17, 48, 38, 48, 16, 48, 29, 48, 31, 48, 27, 48, 22, 48, 46, 48, 12, 48, 37, 48, 39, 48, 26, 48, 35, 48, 30, 48, 18, 48, 49, 48, 32, 48, 45, 48, 47, 48, 40, 48, 33, 48, 41, 48, 62, 48, 28, 48, 50, 48, 53, 48, 55, 48, 42, 48, 51, 48, 68, 48, 34, 48, 36, 48, 58, 48, 43, 48, 63, 48, 56, 48, 59, 48, 54, 48, 60, 48, 44, 48, 66, 48, 69, 48, 48, 48, 64, 48, 57, 48, 65, 48, 52, 48, 67, 48, 61, 48],
	#[65535, 185, 2, 185, 10, 185, 11, 185, 36, 185, 37, 185, 38, 185, 39, 185, 40, 185, 46, 185, 48, 185, 55, 185, 56, 185, 57, 185, 58, 185],
	#[21, -16, 20, -5, 33, -29, 34, -51, 53, -17],
	#[65535, 199, 14, 199, 20, 199, 13, 199, 15, 199, 2, 199, 11, 199, 21, 199, 23, 199, 10, 199, 19, 199, 17, 199, 38, 199, 16, 199, 29, 199, 31, 199, 27, 199, 22, 199, 46, 199, 12, 199, 37, 199, 39, 199, 26, 199, 35, 199, 30, 199, 18, 199, 49, 199, 32, 199, 45, 199, 47, 199, 40, 199, 33, 199, 41, 199, 62, 199, 28, 199, 50, 199, 53, 199, 55, 199, 42, 199, 51, 199, 68, 199, 34, 199, 36, 199, 58, 199, 43, 199, 63, 199, 56, 199, 59, 199, 54, 199, 60, 199, 44, 199, 66, 199, 69, 199, 48, 199, 64, 199, 57, 199, 65, 199, 52, 199, 67, 199, 61, 199],
	#[2, 92, 10, -238, 12, -157, 13, -108, 14, -118, 15, -142, 16, -137, 17, -107, 18, -34, 19, -58, 20, -5, 21, -16, 22, -32, 23, -50, 26, -30, 27, -46, 28, -63, 29, -13, 30, -28, 32, -237, 33, -29, 34, -51, 35, -229, 36, -239, 37, -208, 38, -199, 39, -205, 40, -215, 41, -218, 42, -236, 43, -211, 44, -222, 45, -214, 46, -224, 47, -233, 48, -201, 49, -212, 50, -223, 51, -230, 52, -198, 53, -210, 54, -221, 55, -207, 57, -232, 59, -219, 61, -203, 62, -213, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[53, -210, 2, 96, 39, -205, 22, -32, 46, -224, 60, 96, 11, 96, 23, -50, 19, -58, 14, -118, 38, -199, 29, -13, 31, 96, 18, -34, 13, -108, 44, -222, 10, 96, 12, -157, 37, -208, 58, 96, 17, -107, 21, -16, 36, -239, 15, -142, 45, -214, 47, -233, 16, -137, 41, -218, 62, -213, 28, -63, 50, -223, 35, -229, 55, -207, 33, -29, 51, -230, 68, -55, 34, -51, 32, -237, 20, -5, 43, -211, 63, -225, 56, 96, 59, -219, 54, -221, 42, -236, 40, -254, 66, -48, 69, -56, 48, -201, 49, -212, 57, -232, 65, -33, 52, -198, 67, -37, 61, -203],
	#[21, -16, 20, -5, 33, -29, 34, -51, 53, -17],
	#[2, 194, 10, 194, 11, 194, 36, 194, 37, 194, 38, 194, 39, 194, 40, 194, 46, 194, 48, 194, 55, 194, 56, 194, 57, 194, 58, 194, 63, -41],
	#[14, -118, 22, -32, 17, -107, 13, -108, 15, -142, 28, -63, 30, -28, 54, -221, 21, -16, 23, -50, 10, -282, 19, -58, 36, -239, 31, 252, 18, -34, 27, -46, 44, -273, 46, -224, 37, -208, 39, -205, 26, -30, 35, -229, 52, -198, 61, -203, 16, -137, 45, -268, 47, -233, 29, -13, 38, -199, 62, -213, 59, -271, 12, -157, 53, -210, 55, -265, 33, -29, 51, -230, 68, -55, 34, -51, 32, -237, 20, -5, 43, -264, 63, -225, 50, -223, 49, -212, 57, -279, 42, -281, 40, -269, 66, -48, 69, -56, 41, -218, 64, -23, 67, -37, 65, -33, 48, -201],
	#[65535, 191, 36, 191, 55, 191, 38, 191, 2, 191, 39, 191, 37, 191, 48, 191, 10, 191, 58, 191, 11, 191, 46, 191, 40, 191, 56, 191, 57, 191],
	#[65535, 173, 2, 173, 10, 173, 11, 173, 36, 173, 37, 173, 38, 173, 39, 173, 40, 173, 46, 173, 48, 173, 55, 173, 56, 173, 57, 173, 58, 173],
	#[65535, 226, 10, 226, 2, 226, 38, 226, 37, 226, 36, 226, 39, 226, 11, 226, 58, 226, 40, 226, 56, 226, 48, 226],
	#[65535, 62, 18, 62, 20, 62, 21, 62, 22, 62, 23, 62, 28, 62, 29, 62, 33, 62, 34, 62, 53, 62],
	#[12, -43, 13, -66, 18, -34, 19, -58, 20, -5, 21, -16, 22, -32, 23, -50, 28, -63, 29, -13, 33, -29, 34, -51, 35, -53, 36, -64, 41, -27, 42, -61, 43, -18, 53, -17, 55, -14, 61, -7, 62, -22, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[62, -22, 13, -66, 68, -55, 61, -7, 63, -41, 66, -48, 12, -43, 41, -80, 58, 201, 67, -37, 65, -33],
	#[2, 219, 10, 219, 11, 219, 36, -94, 37, -97, 38, -98, 39, -96, 40, 219, 48, 219, 56, 219, 58, 219],
	#[65535, 49, 14, 49, 13, 49, 15, 49, 2, 49, 12, 49, 21, 49, 23, 49, 10, 49, 19, 49, 36, 49, 20, 49, 16, 49, 11, 49, 31, 49, 27, 49, 22, 49, 46, 49, 43, 49, 34, 49, 37, 49, 39, 49, 17, 49, 35, 49, 30, 49, 18, 49, 49, 49, 42, 49, 45, 49, 47, 49, 40, 49, 29, 49, 38, 49, 62, 49, 28, 49, 50, 49, 53, 49, 55, 49, 33, 49, 51, 49, 68, 49, 52, 49, 32, 49, 58, 49, 26, 49, 63, 49, 56, 49, 59, 49, 54, 49, 60, 49, 44, 49, 66, 49, 69, 49, 41, 49, 64, 49, 57, 49, 65, 49, 48, 49, 67, 49, 61, 49],
	#[65535, 38, 14, 38, 13, 38, 15, 38, 2, 38, 11, 38, 21, 38, 23, 38, 10, 38, 19, 38, 17, 38, 38, 38, 16, 38, 29, 38, 31, 38, 27, 38, 22, 38, 46, 38, 12, 38, 37, 38, 39, 38, 26, 38, 35, 38, 30, 38, 18, 38, 20, 38, 32, 38, 45, 38, 47, 38, 33, 38, 41, 38, 49, 38, 28, 38, 50, 38, 53, 38, 55, 38, 42, 38, 51, 38, 68, 38, 34, 38, 36, 38, 58, 38, 43, 38, 40, 38, 56, 38, 59, 38, 54, 38, 60, 38, 44, 38, 66, 38, 69, 38, 48, 38, 64, 38, 57, 38, 62, 38, 52, 38, 67, 38, 61, 38, 63, 38, 65, 38],
	#[65535, 184, 2, 184, 10, 184, 11, 184, 36, 184, 37, 184, 38, 184, 39, 184, 40, 184, 46, 184, 48, 184, 55, 184, 56, 184, 57, 184, 58, 184],
	#[65535, 64, 23, 64, 43, 64, 62, 64, 28, 64, 12, 64, 55, 64, 33, 64, 22, 64, 68, 64, 34, 64, 21, 64, 20, 64, 61, 64, 63, 64, 19, 64, 42, 64, 29, 64, 66, 64, 69, 64, 13, 64, 53, 64, 65, 64, 18, 64, 67, 64],
	#[23, -50, 33, -29, 43, -18, 62, -22, 28, -63, 12, -43, 53, -17, 55, -14, 42, -61, 22, -32, 68, -55, 34, -51, 21, -16, 20, -5, 61, -7, 63, -41, 19, -58, 18, -34, 29, -13, 66, -48, 69, -56, 13, -66, 67, -37, 65, -33],
	#[65535, 200, 2, 200, 10, 200, 11, 200, 12, 200, 13, 200, 14, 200, 15, 200, 16, 200, 17, 200, 18, 200, 19, 200, 20, 200, 21, 200, 22, 200, 23, 200, 26, 200, 27, 200, 28, 200, 29, 200, 30, 200, 31, 200, 32, 200, 33, 200, 34, 200, 35, 200, 36, 200, 37, 200, 38, 200, 39, 200, 40, 200, 41, 200, 42, 200, 43, 200, 44, 200, 45, 200, 46, 200, 47, 200, 48, 200, 49, 200, 50, 200, 51, 200, 52, 200, 53, 200, 54, 200, 55, 200, 56, 200, 57, 200, 58, 200, 59, 200, 60, 200, 61, 200, 62, 200, 63, 200, 64, 200, 65, 200, 66, 200, 67, 200, 68, 200, 69, 200],
	#[13, -108, 15, -142, 21, -115, 23, -150, 10, -159, 19, -152, 14, -118, 38, -98, 16, -137, 29, -111, 31, -141, 17, -107, 22, -128, 46, -135, 12, -157, 24, -102, 37, -97, 39, -96, 26, -127, 35, -146, 30, -125, 18, -131, 49, 339, 32, -158, 45, -119, 47, -153, 25, -113, 33, -104, 41, -124, 62, -117, 28, -161, 40, -121, 53, -114, 55, -112, 42, -101, 51, -148, 68, -149, 27, -143, 36, -94, 20, -106, 43, -130, 63, -136, 50, -132, 59, -134, 60, 296, 44, -122, 66, -145, 69, -147, 48, -103, 64, -120, 57, -151, 65, -129, 52, -100, 67, -133, 61, -105],
	#[65535, 182, 39, 182, 10, 182, 2, 182, 38, 182, 55, 182, 37, 182, 46, 182, 36, 182, 40, 182, 11, 182, 58, 182, 57, 182, 48, 182, 56, 182],
	#[65535, 63, 55, 63],
	#[65535, 84, 2, 84, 10, 84, 11, 84],
	#[65535, 193, 2, 193, 10, 193, 11, 193, 36, 193, 37, 193, 38, 193, 39, 193, 40, 193, 46, 193, 48, 193, 55, 193, 56, 193, 57, 193, 58, 193],
	#[65535, 186, 2, 186, 10, 186, 11, 186, 36, 186, 37, 186, 38, 186, 39, 186, 40, 186, 46, 186, 48, 186, 55, 186, 56, 186, 57, 186, 58, 186],
	#[39, 227, 10, 227, 2, 227, 38, 227, 55, -69, 37, 227, 46, -67, 36, 227, 40, 227, 11, 227, 58, 227, 57, -68, 48, 227, 56, 227],
	#[65535, 53, 2, 53, 10, 53, 11, 53, 12, 53, 13, 53, 14, 53, 15, 53, 16, 53, 17, 53, 18, 53, 19, 53, 20, 53, 21, 53, 22, 53, 23, 53, 26, 53, 27, 53, 28, 53, 29, 53, 30, 53, 31, 53, 32, 53, 33, 53, 34, 53, 35, 53, 36, 53, 37, 53, 38, 53, 39, 53, 40, 53, 41, 53, 42, 53, 43, 53, 44, 53, 45, 53, 46, 53, 47, 53, 48, 53, 49, 53, 50, 53, 51, 53, 52, 53, 53, 53, 54, 53, 55, 53, 56, 53, 57, 53, 58, 53, 59, 53, 60, 53, 61, 53, 62, 53, 63, 53, 64, 53, 65, 53, 66, 53, 67, 53, 68, 53, 69, 53],
	#[65535, 65, 23, 65, 28, 65, 12, 65, 55, 65, 22, 65, 68, 65, 21, 65, 20, 65, 43, 65, 33, 65, 19, 65, 42, 65, 29, 65, 66, 65, 34, 65, 13, 65, 53, 65, 18, 65, 62, 65, 61, 65, 63, 65, 65, 65, 67, 65, 69, 65],
	#[65535, 85, 2, 85, 10, 85, 11, 85],
	#[65535, 192, 2, 192, 10, 192, 11, 192, 36, 192, 37, 192, 38, 192, 39, 192, 40, 192, 46, 192, 48, 192, 55, 192, 56, 192, 57, 192, 58, 192],
	#[21, -16, 20, -5, 33, -29, 34, -51, 53, -17],
	#[23, -50, 21, -16, 62, -22, 28, -63, 12, -43, 35, -53, 55, -14, 22, -32, 68, -55, 34, -51, 36, -64, 20, -5, 43, -18, 33, -29, 19, -58, 42, -61, 29, -13, 66, -48, 69, -56, 13, -66, 53, -17, 65, -33, 18, -34, 41, -74, 61, -7, 63, -41, 67, -37],
	#[12, -43, 13, -66, 18, -34, 19, -58, 20, -5, 21, -16, 22, -32, 23, -50, 28, -63, 29, -13, 33, -29, 34, -51, 35, -53, 36, -64, 41, -74, 42, -61, 43, -18, 53, -17, 55, -14, 56, 208, 61, -7, 62, -22, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[58, 174, 40, -371, 56, 174],
	#[56, -374],
	#[65535, 223, 36, 223, 37, 223, 38, 223, 39, 223, 40, 223, 56, 223, 58, 223],
	#[65535, 175, 58, 175, 40, 175, 56, 175],
	#[21, -16, 23, -50, 29, -13, 13, -66, 19, -58, 35, -53, 33, -29, 43, -18, 62, -22, 28, -63, 12, -43, 53, -17, 55, -14, 42, -61, 22, -32, 68, -55, 34, -51, 36, -64, 20, -5, 61, -7, 63, -41, 56, 179, 18, -34, 40, 179, 66, -48, 69, -56, 41, -27, 58, 179, 67, -37, 65, -33],
	#[36, -94, 37, -97, 38, -98, 39, -96, 40, 222, 56, 222, 58, 222],
	#[65535, 209, 56, 209],
	#[65535, 178, 40, 178, 56, 178, 58, 178],
	#[65535, 206, 40, 206, 46, 206, 56, 206, 58, 206],
	#[65535, 204, 46, 204, 56, 204, 40, 204, 58, 204],
	#[65535, 207, 40, 207, 46, 207, 56, 207, 58, 207],
	#[46, 203, 58, 203, 40, -89, 56, 203],
	#[46, -91, 56, 202],
	#[56, -84],
	#[65535, 197, 62, 197, 59, 197, 65, 197, 68, 197, 56, 197, 58, 197, 49, 197, 57, 197, 60, 197, 40, 197, 66, 197, 69, 197, 48, 197, 64, 197, 52, 197, 61, 197, 47, 197, 11, 197, 30, 197, 14, 197, 38, 197, 26, 197, 45, 197, 36, 197, 22, 197, 46, 197, 13, 197, 15, 197, 2, 197, 44, 197, 28, 197, 17, 197, 27, 197, 21, 197, 23, 197, 10, 197, 19, 197, 55, 197, 20, 197, 33, 197, 16, 197, 29, 197, 31, 197, 18, 197, 63, 197, 53, 197, 12, 197, 37, 197, 39, 197, 32, 197, 35, 197, 50, 197, 54, 197, 51, 197, 42, 197, 67, 197, 34, 197, 43, 197, 41, 197],
	#[65535, 195, 2, 195, 10, 195, 11, 195, 36, 195, 37, 195, 38, 195, 39, 195, 40, 195, 46, 195, 48, 195, 55, 195, 56, 195, 57, 195, 58, 195],
	#[65535, 202, 58, 202],
	#[58, -88],
	#[65535, 198, 12, 198, 14, 198, 17, 198, 39, 198, 22, 198, 46, 198, 10, 198, 13, 198, 15, 198, 2, 198, 11, 198, 28, 198, 30, 198, 54, 198, 21, 198, 23, 198, 16, 198, 19, 198, 36, 198, 38, 198, 26, 198, 29, 198, 31, 198, 18, 198, 27, 198, 44, 198, 53, 198, 41, 198, 34, 198, 37, 198, 58, 198, 32, 198, 35, 198, 33, 198, 61, 198, 20, 198, 42, 198, 45, 198, 47, 198, 40, 198, 43, 198, 60, 198, 62, 198, 59, 198, 56, 198, 55, 198, 48, 198, 51, 198, 49, 198, 57, 198, 67, 198, 50, 198, 52, 198, 63, 198, 64, 198, 65, 198, 66, 198, 68, 198, 69, 198],
	#[12, -43, 13, -66, 41, -80, 61, -7, 62, -22, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55],
	#[65535, 205, 58, 205, 46, 205, 40, 205, 56, 205],
	#[12, -43, 13, -66, 41, -80, 61, -7, 62, -22, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55],
	#[56, -93],
	#[65535, 196, 14, 196, 22, 196, 46, 196, 13, 196, 15, 196, 2, 196, 11, 196, 21, 196, 23, 196, 10, 196, 19, 196, 17, 196, 20, 196, 62, 196, 16, 196, 29, 196, 31, 196, 27, 196, 44, 196, 28, 196, 12, 196, 37, 196, 39, 196, 26, 196, 35, 196, 30, 196, 18, 196, 49, 196, 45, 196, 47, 196, 34, 196, 33, 196, 38, 196, 69, 196, 50, 196, 53, 196, 55, 196, 42, 196, 51, 196, 68, 196, 52, 196, 36, 196, 58, 196, 43, 196, 63, 196, 56, 196, 59, 196, 54, 196, 60, 196, 40, 196, 66, 196, 32, 196, 41, 196, 64, 196, 57, 196, 65, 196, 48, 196, 67, 196, 61, 196],
	#[65535, 67, 10, 67, 12, 67, 13, 67, 14, 67, 15, 67, 16, 67, 17, 67, 18, 67, 19, 67, 20, 67, 21, 67, 22, 67, 23, 67, 24, 67, 25, 67, 26, 67, 27, 67, 28, 67, 29, 67, 30, 67, 31, 67, 32, 67, 33, 67, 34, 67, 35, 67, 36, 67, 37, 67, 38, 67, 39, 67, 40, 67, 41, 67, 42, 67, 43, 67, 44, 67, 45, 67, 46, 67, 47, 67, 48, 67, 49, 67, 50, 67, 51, 67, 52, 67, 53, 67, 55, 67, 56, 67, 57, 67, 58, 67, 59, 67, 60, 67, 61, 67, 62, 67, 63, 67, 64, 67, 65, 67, 66, 67, 67, 67, 68, 67, 69, 67],
	#[23, -50, 12, -43, 13, -66, 18, -34, 19, -58, 20, -5, 21, -16, 22, -32, 34, -51, 28, -63, 29, -13, 33, -29, 63, -41, 35, -53, 36, -64, 41, -27, 42, -61, 43, -18, 53, -17, 67, -37, 55, -14, 65, -33, 61, -7, 62, -22, 66, -48, 68, -55, 69, -56],
	#[65535, 69, 12, 69, 14, 69, 39, 69, 22, 69, 46, 69, 10, 69, 13, 69, 15, 69, 44, 69, 28, 69, 30, 69, 25, 69, 18, 69, 21, 69, 23, 69, 16, 69, 19, 69, 17, 69, 38, 69, 26, 69, 29, 69, 31, 69, 24, 69, 27, 69, 63, 69, 53, 69, 41, 69, 34, 69, 37, 69, 58, 69, 32, 69, 35, 69, 33, 69, 61, 69, 20, 69, 42, 69, 45, 69, 47, 69, 40, 69, 43, 69, 60, 69, 62, 69, 59, 69, 56, 69, 55, 69, 48, 69, 51, 69, 49, 69, 57, 69, 36, 69, 50, 69, 52, 69, 64, 69, 65, 69, 66, 69, 67, 69, 68, 69, 69, 69],
	#[65535, 66, 10, 66, 12, 66, 13, 66, 14, 66, 15, 66, 16, 66, 17, 66, 18, 66, 19, 66, 20, 66, 21, 66, 22, 66, 23, 66, 24, 66, 25, 66, 26, 66, 27, 66, 28, 66, 29, 66, 30, 66, 31, 66, 32, 66, 33, 66, 34, 66, 35, 66, 36, 66, 37, 66, 38, 66, 39, 66, 40, 66, 41, 66, 42, 66, 43, 66, 44, 66, 45, 66, 46, 66, 47, 66, 48, 66, 49, 66, 50, 66, 51, 66, 52, 66, 53, 66, 55, 66, 56, 66, 57, 66, 58, 66, 59, 66, 60, 66, 61, 66, 62, 66, 63, 66, 64, 66, 65, 66, 66, 66, 67, 66, 68, 66, 69, 66],
	#[65535, 68, 14, 68, 22, 68, 30, 68, 15, 68, 10, 68, 17, 68, 29, 68, 23, 68, 12, 68, 18, 68, 25, 68, 37, 68, 16, 68, 31, 68, 20, 68, 26, 68, 19, 68, 32, 68, 39, 68, 28, 68, 33, 68, 13, 68, 27, 68, 38, 68, 62, 68, 43, 68, 52, 68, 36, 68, 40, 68, 21, 68, 35, 68, 34, 68, 24, 68, 41, 68, 42, 68, 48, 68, 44, 68, 45, 68, 46, 68, 47, 68, 49, 68, 50, 68, 51, 68, 53, 68, 55, 68, 56, 68, 57, 68, 58, 68, 59, 68, 60, 68, 61, 68, 63, 68, 64, 68, 65, 68, 66, 68, 67, 68, 68, 68, 69, 68],
	#[65535, 224, 39, 224, 36, 224, 38, 224, 37, 224, 58, 224, 40, 224, 56, 224],
	#[18, -131, 19, -152, 20, -106, 21, -115, 22, -128, 23, -150, 24, -102, 25, -113, 26, -127, 27, -143, 28, -161, 29, -111, 30, -125, 31, -141, 32, -158, 33, -104, 59, -271],
	#[65535, 322, 31, 322, 28, 322, 15, 322, 20, 322, 37, 322, 42, 322, 12, 322, 14, 322, 26, 322, 39, 322, 22, 322, 46, 322, 13, 322, 35, 322, 47, 322, 30, 322, 25, 322, 33, 322, 21, 322, 23, 322, 10, 322, 17, 322, 38, 322, 62, 322, 16, 322, 29, 322, 50, 322, 18, 322, 27, 322, 63, 322, 53, 322, 41, 322, 19, 322, 58, 322, 68, 322, 52, 322, 61, 322, 49, 322, 32, 322, 45, 322, 66, 322, 40, 322, 43, 322, 60, 322, 24, 322, 56, 322, 55, 322, 51, 322, 36, 322, 59, 322, 57, 322, 65, 322, 44, 322, 69, 322, 48, 322, 64, 322, 67, 322],
	#[65535, 25, 12, 25, 14, 25, 20, 25, 22, 25, 13, 25, 15, 25, 30, 25, 21, 25, 23, 25, 10, 25, 19, 25, 17, 25, 38, 25, 16, 25, 29, 25, 31, 25, 18, 25, 27, 25, 25, 25, 46, 25, 41, 25, 24, 25, 37, 25, 39, 25, 26, 25, 35, 25, 33, 25, 61, 25, 49, 25, 32, 25, 45, 25, 47, 25, 40, 25, 43, 25, 58, 25, 62, 25, 28, 25, 50, 25, 53, 25, 55, 25, 42, 25, 51, 25, 68, 25, 34, 25, 36, 25, 63, 25, 56, 25, 59, 25, 54, 25, 60, 25, 44, 25, 66, 25, 69, 25, 48, 25, 64, 25, 57, 25, 65, 25, 52, 25, 67, 25],
	#[65535, 315, 10, 315, 12, 315, 13, 315, 14, 315, 15, 315, 16, 315, 17, 315, 18, 315, 19, 315, 20, 315, 21, 315, 22, 315, 23, 315, 24, 315, 25, 315, 26, 315, 27, 315, 28, 315, 29, 315, 30, 315, 31, 315, 32, 315, 33, 315, 35, 315, 36, 315, 37, 315, 38, 315, 39, 315, 40, 315, 41, 315, 42, 315, 43, 315, 44, 315, 45, 315, 46, 315, 47, 315, 48, 315, 49, 315, 50, 315, 51, 315, 52, 315, 53, 315, 55, 315, 56, 315, 57, 315, 58, 315, 59, 315, 60, 315, 61, 315, 62, 315, 63, 315, 64, 315, 65, 315, 66, 315, 67, 315, 68, 315, 69, 315],
	#[65535, 39, 15, 39, 10, 39, 23, 39, 12, 39, 14, 39, 38, 39, 20, 39, 22, 39, 25, 39, 13, 39, 35, 39, 47, 39, 30, 39, 54, 39, 21, 39, 42, 39, 16, 39, 19, 39, 17, 39, 45, 39, 62, 39, 41, 39, 29, 39, 31, 39, 18, 39, 27, 39, 44, 39, 46, 39, 48, 39, 24, 39, 37, 39, 39, 39, 26, 39, 68, 39, 33, 39, 61, 39, 49, 39, 32, 39, 66, 39, 40, 39, 43, 39, 60, 39, 28, 39, 50, 39, 53, 39, 55, 39, 51, 39, 34, 39, 36, 39, 58, 39, 64, 39, 63, 39, 56, 39, 59, 39, 57, 39, 65, 39, 69, 39, 52, 39, 67, 39],
	#[65535, 307, 10, 307, 12, 307, 13, 307, 14, 307, 15, 307, 16, 307, 17, 307, 18, 307, 19, 307, 20, 307, 21, 307, 22, 307, 23, 307, 24, 307, 25, 307, 26, 307, 27, 307, 28, 307, 29, 307, 30, 307, 31, 307, 32, 307, 33, 307, 35, 307, 36, 307, 37, 307, 38, 307, 39, 307, 40, 307, 41, 307, 42, 307, 43, 307, 44, 307, 45, 307, 46, 307, 47, 307, 48, 307, 49, 307, 50, 307, 51, 307, 52, 307, 53, 307, 55, 307, 56, 307, 57, 307, 58, 307, 59, 307, 60, 307, 61, 307, 62, 307, 63, 307, 64, 307, 65, 307, 66, 307, 67, 307, 68, 307, 69, 307],
	#[65535, 21, 10, 21, 12, 21, 13, 21, 14, 21, 15, 21, 16, 21, 17, 21, 18, 21, 19, 21, 20, 21, 21, 21, 22, 21, 23, 21, 24, 21, 25, 21, 26, 21, 27, 21, 28, 21, 29, 21, 30, 21, 31, 21, 32, 21, 33, 21, 34, 21, 35, 21, 36, 21, 37, 21, 38, 21, 39, 21, 40, 21, 41, 21, 42, 21, 43, 21, 44, 21, 45, 21, 46, 21, 47, 21, 48, 21, 49, 21, 50, 21, 51, 21, 52, 21, 53, 21, 54, 21, 55, 21, 56, 21, 57, 21, 58, 21, 59, 21, 60, 21, 61, 21, 62, 21, 63, 21, 64, 21, 65, 21, 66, 21, 67, 21, 68, 21, 69, 21],
	#[65535, 17, 27, 17, 31, 17, 69, 17, 63, 17, 38, 17, 2, 17, 21, 17, 12, 17, 46, 17, 68, 17, 10, 17, 29, 17, 23, 17, 64, 17, 17, 17, 16, 17, 37, 17, 28, 17, 35, 17, 14, 17, 11, 17, 51, 17, 26, 17, 45, 17, 36, 17, 39, 17, 22, 17, 19, 17, 25, 17, 13, 17, 15, 17, 53, 17, 44, 17, 47, 17, 30, 17, 50, 17, 18, 17, 24, 17, 42, 17, 61, 17, 52, 17, 48, 17, 20, 17, 62, 17, 41, 17, 49, 17, 65, 17, 60, 17, 67, 17, 33, 17, 43, 17, 40, 17, 58, 17, 54, 17, 56, 17, 57, 17, 66, 17, 34, 17, 59, 17, 55, 17, 32, 17],
	#[65535, 13, 2, 13, 10, 13, 11, 13, 12, 13, 13, 13, 14, 13, 15, 13, 16, 13, 17, 13, 18, 13, 19, 13, 20, 13, 21, 13, 22, 13, 23, 13, 24, 13, 25, 13, 26, 13, 27, 13, 28, 13, 29, 13, 30, 13, 31, 13, 32, 13, 33, 13, 34, 13, 35, 13, 36, 13, 37, 13, 38, 13, 39, 13, 40, 13, 41, 13, 42, 13, 43, 13, 44, 13, 45, 13, 46, 13, 47, 13, 48, 13, 49, 13, 50, 13, 51, 13, 52, 13, 53, 13, 54, 13, 55, 13, 56, 13, 57, 13, 58, 13, 59, 13, 60, 13, 61, 13, 62, 13, 63, 13, 64, 13, 65, 13, 66, 13, 67, 13, 68, 13, 69, 13],
	#[49, -181],
	#[65535, 40, 15, 40, 27, 40, 55, 40, 28, 40, 38, 40, 63, 40, 26, 40, 39, 40, 22, 40, 19, 40, 35, 40, 47, 40, 30, 40, 54, 40, 18, 40, 21, 40, 23, 40, 10, 40, 52, 40, 14, 40, 20, 40, 62, 40, 41, 40, 29, 40, 31, 40, 69, 40, 13, 40, 44, 40, 46, 40, 12, 40, 64, 40, 37, 40, 16, 40, 17, 40, 68, 40, 50, 40, 61, 40, 49, 40, 42, 40, 45, 40, 66, 40, 34, 40, 43, 40, 60, 40, 24, 40, 53, 40, 25, 40, 51, 40, 36, 40, 33, 40, 59, 40, 57, 40, 40, 40, 56, 40, 32, 40, 48, 40, 58, 40, 67, 40, 65, 40],
	#[65535, 30, 10, 30, 12, 30, 13, 30, 14, 30, 15, 30, 16, 30, 17, 30, 18, 30, 19, 30, 20, 30, 21, 30, 22, 30, 23, 30, 24, 30, 25, 30, 26, 30, 27, 30, 28, 30, 29, 30, 30, 30, 31, 30, 32, 30, 33, 30, 34, 30, 35, 30, 36, 30, 37, 30, 38, 30, 39, 30, 40, 30, 41, 30, 42, 30, 43, 30, 44, 30, 45, 30, 46, 30, 47, 30, 48, 30, 49, 30, 50, 30, 51, 30, 52, 30, 53, 30, 54, 30, 55, 30, 56, 30, 57, 30, 58, 30, 59, 30, 60, 30, 61, 30, 62, 30, 63, 30, 64, 30, 65, 30, 66, 30, 67, 30, 68, 30, 69, 30],
	#[10, -159, 12, -157, 13, -108, 14, -118, 15, -142, 16, -137, 17, -107, 18, -131, 19, -152, 20, -106, 21, -115, 22, -128, 23, -150, 24, -102, 25, -113, 26, -127, 27, -143, 28, -161, 29, -111, 30, -125, 31, -141, 32, -158, 33, -104, 35, -146, 36, -94, 37, -97, 38, -98, 39, -96, 40, -121, 41, -124, 42, -101, 43, -130, 44, -122, 45, -119, 46, -135, 47, -153, 48, -103, 49, 339, 50, -132, 51, -148, 52, -100, 53, -114, 55, -112, 56, 296, 57, -151, 65, -129, 59, -134, 61, -105, 62, -117, 63, -136, 64, -120, 66, -145, 67, -133, 68, -149, 69, -147],
	#[65535, 26, 12, 26, 14, 26, 61, 26, 39, 26, 22, 26, 13, 26, 15, 26, 44, 26, 30, 26, 25, 26, 18, 26, 21, 26, 23, 26, 10, 26, 19, 26, 17, 26, 38, 26, 33, 26, 16, 26, 29, 26, 31, 26, 24, 26, 27, 26, 63, 26, 46, 26, 41, 26, 34, 26, 37, 26, 58, 26, 26, 26, 35, 26, 50, 26, 54, 26, 20, 26, 42, 26, 45, 26, 47, 26, 40, 26, 43, 26, 60, 26, 62, 26, 28, 26, 53, 26, 55, 26, 48, 26, 51, 26, 49, 26, 56, 26, 32, 26, 52, 26, 36, 26, 59, 26, 65, 26, 57, 26, 64, 26, 66, 26, 67, 26, 68, 26, 69, 26],
	#[65535, 335, 10, 335, 12, 335, 13, 335, 14, 335, 15, 335, 16, 335, 17, 335, 18, 335, 19, 335, 20, 335, 21, 335, 22, 335, 23, 335, 24, 335, 25, 335, 26, 335, 27, 335, 28, 335, 29, 335, 30, 335, 31, 335, 32, 335, 33, 335, 35, 335, 36, 335, 37, 335, 38, 335, 39, 335, 40, 335, 41, 335, 42, 335, 43, 335, 44, 335, 45, 335, 46, 335, 47, 335, 48, 335, 49, 335, 50, 335, 51, 335, 52, 335, 53, 335, 55, 335, 56, 335, 57, 335, 58, 335, 59, 335, 60, 335, 61, 335, 62, 335, 63, 335, 64, 335, 65, 335, 66, 335, 67, 335, 68, 335, 69, 335],
	#[65535, 22, 13, 22, 21, 22, 30, 22, 12, 22, 39, 22, 29, 22, 15, 22, 38, 22, 19, 22, 37, 22, 17, 22, 23, 22, 46, 22, 27, 22, 10, 22, 45, 22, 25, 22, 31, 22, 36, 22, 20, 22, 18, 22, 53, 22, 16, 22, 14, 22, 48, 22, 44, 22, 28, 22, 26, 22, 61, 22, 24, 22, 22, 22, 47, 22, 42, 22, 52, 22, 51, 22, 34, 22, 41, 22, 32, 22, 55, 22, 40, 22, 60, 22, 59, 22, 35, 22, 63, 22, 58, 22, 68, 22, 50, 22, 43, 22, 65, 22, 64, 22, 69, 22, 33, 22, 54, 22, 67, 22, 49, 22, 62, 22, 56, 22, 57, 22, 66, 22],
	#[12, -157, 15, -142, 28, -161, 31, -141, 14, -118, 20, -106, 22, -128, 46, -135, 13, -108, 35, -146, 47, -153, 30, -125, 25, -113, 33, -104, 21, -115, 23, -150, 10, -159, 19, -152, 17, -107, 38, -98, 62, -117, 16, -137, 29, -111, 50, -132, 18, -131, 27, -143, 63, -136, 53, -114, 43, -130, 24, -102, 37, -97, 39, -96, 26, -127, 52, -100, 36, -94, 49, 339, 32, -158, 45, -119, 66, -145, 40, -121, 56, 298, 60, 298, 57, -151, 65, -129, 55, -112, 42, -101, 41, -124, 68, -149, 67, -133, 58, 298, 61, -105, 59, -134, 44, -122, 51, -148, 48, -103, 64, -120, 69, -147],
	#[65535, 308, 10, 308, 12, 308, 13, 308, 14, 308, 15, 308, 16, 308, 17, 308, 18, 308, 19, 308, 20, 308, 21, 308, 22, 308, 23, 308, 24, 308, 25, 308, 26, 308, 27, 308, 28, 308, 29, 308, 30, 308, 31, 308, 32, 308, 33, 308, 35, 308, 36, 308, 37, 308, 38, 308, 39, 308, 40, 308, 41, 308, 42, 308, 43, 308, 44, 308, 45, 308, 46, 308, 47, 308, 48, 308, 49, 308, 50, 308, 51, 308, 52, 308, 53, 308, 55, 308, 56, 308, 57, 308, 58, 308, 59, 308, 60, 308, 61, 308, 62, 308, 63, 308, 64, 308, 65, 308, 66, 308, 67, 308, 68, 308, 69, 308],
	#[65535, 14, 12, 14, 22, 14, 47, 14, 23, 14, 30, 14, 55, 14, 31, 14, 14, 14, 51, 14, 25, 14, 39, 14, 29, 14, 46, 14, 59, 14, 13, 14, 15, 14, 2, 14, 11, 14, 26, 14, 37, 14, 54, 14, 33, 14, 21, 14, 43, 14, 10, 14, 19, 14, 36, 14, 38, 14, 35, 14, 16, 14, 50, 14, 18, 14, 27, 14, 44, 14, 28, 14, 66, 14, 24, 14, 58, 14, 17, 14, 68, 14, 52, 14, 61, 14, 20, 14, 45, 14, 34, 14, 56, 14, 60, 14, 62, 14, 57, 14, 53, 14, 32, 14, 42, 14, 41, 14, 63, 14, 49, 14, 40, 14, 69, 14, 48, 14, 64, 14, 67, 14, 65, 14],
	#[65535, 321, 10, 321, 12, 321, 13, 321, 14, 321, 15, 321, 16, 321, 17, 321, 18, 321, 19, 321, 20, 321, 21, 321, 22, 321, 23, 321, 24, 321, 25, 321, 26, 321, 27, 321, 28, 321, 29, 321, 30, 321, 31, 321, 32, 321, 33, 321, 35, 321, 36, 321, 37, 321, 38, 321, 39, 321, 40, 321, 41, 321, 42, 321, 43, 321, 44, 321, 45, 321, 46, 321, 47, 321, 48, 321, 49, 321, 50, 321, 51, 321, 52, 321, 53, 321, 55, 321, 56, 321, 57, 321, 58, 321, 59, 321, 60, 321, 61, 321, 62, 321, 63, 321, 64, 321, 65, 321, 66, 321, 67, 321, 68, 321, 69, 321],
	#[65535, 324, 10, 324, 12, 324, 13, 324, 14, 324, 15, 324, 16, 324, 17, 324, 18, 324, 19, 324, 20, 324, 21, 324, 22, 324, 23, 324, 24, 324, 25, 324, 26, 324, 27, 324, 28, 324, 29, 324, 30, 324, 31, 324, 32, 324, 33, 324, 35, 324, 36, 324, 37, 324, 38, 324, 39, 324, 40, 324, 41, 324, 42, 324, 43, 324, 44, 324, 45, 324, 46, 324, 47, 324, 48, 324, 49, 324, 50, 324, 51, 324, 52, 324, 53, 324, 55, 324, 56, 324, 57, 324, 58, 324, 59, 324, 60, 324, 61, 324, 62, 324, 63, 324, 64, 324, 65, 324, 66, 324, 67, 324, 68, 324, 69, 324],
	#[65535, 330, 31, 330, 21, 330, 29, 330, 15, 330, 19, 330, 17, 330, 23, 330, 46, 330, 12, 330, 10, 330, 45, 330, 30, 330, 26, 330, 36, 330, 13, 330, 18, 330, 16, 330, 14, 330, 39, 330, 28, 330, 25, 330, 61, 330, 24, 330, 22, 330, 47, 330, 42, 330, 52, 330, 51, 330, 20, 330, 69, 330, 32, 330, 55, 330, 40, 330, 60, 330, 44, 330, 41, 330, 35, 330, 38, 330, 63, 330, 58, 330, 27, 330, 49, 330, 43, 330, 48, 330, 64, 330, 59, 330, 33, 330, 65, 330, 37, 330, 50, 330, 67, 330, 68, 330, 62, 330, 57, 330, 53, 330, 66, 330, 56, 330],
	#[65535, 320, 10, 320, 12, 320, 13, 320, 14, 320, 15, 320, 16, 320, 17, 320, 18, 320, 19, 320, 20, 320, 21, 320, 22, 320, 23, 320, 24, 320, 25, 320, 26, 320, 27, 320, 28, 320, 29, 320, 30, 320, 31, 320, 32, 320, 33, 320, 35, 320, 36, 320, 37, 320, 38, 320, 39, 320, 40, 320, 41, 320, 42, 320, 43, 320, 44, 320, 45, 320, 46, 320, 47, 320, 48, 320, 49, 320, 50, 320, 51, 320, 52, 320, 53, 320, 55, 320, 56, 320, 57, 320, 58, 320, 59, 320, 60, 320, 61, 320, 62, 320, 63, 320, 64, 320, 65, 320, 66, 320, 67, 320, 68, 320, 69, 320],
	#[65535, 319, 15, 319, 18, 319, 10, 319, 12, 319, 13, 319, 14, 319, 38, 319, 16, 319, 17, 319, 47, 319, 19, 319, 20, 319, 21, 319, 22, 319, 23, 319, 24, 319, 25, 319, 26, 319, 27, 319, 28, 319, 29, 319, 30, 319, 31, 319, 32, 319, 33, 319, 63, 319, 35, 319, 36, 319, 37, 319, 39, 319, 40, 319, 41, 319, 42, 319, 43, 319, 44, 319, 45, 319, 46, 319, 58, 319, 48, 319, 49, 319, 50, 319, 51, 319, 52, 319, 53, 319, 67, 319, 55, 319, 56, 319, 57, 319, 65, 319, 59, 319, 60, 319, 61, 319, 62, 319, 64, 319, 66, 319, 68, 319, 69, 319],
	#[65535, 306, 23, 306, 30, 306, 31, 306, 14, 306, 38, 306, 63, 306, 68, 306, 39, 306, 46, 306, 13, 306, 15, 306, 44, 306, 47, 306, 37, 306, 50, 306, 21, 306, 43, 306, 16, 306, 55, 306, 45, 306, 62, 306, 26, 306, 29, 306, 51, 306, 69, 306, 60, 306, 22, 306, 10, 306, 12, 306, 49, 306, 19, 306, 59, 306, 48, 306, 52, 306, 61, 306, 20, 306, 57, 306, 27, 306, 64, 306, 58, 306, 28, 306, 35, 306, 25, 306, 42, 306, 41, 306, 40, 306, 36, 306, 33, 306, 24, 306, 17, 306, 66, 306, 32, 306, 53, 306, 56, 306, 18, 306, 65, 306, 67, 306],
	#[65535, 31, 10, 31, 12, 31, 13, 31, 14, 31, 15, 31, 16, 31, 17, 31, 18, 31, 19, 31, 20, 31, 21, 31, 22, 31, 23, 31, 24, 31, 25, 31, 26, 31, 27, 31, 28, 31, 29, 31, 30, 31, 31, 31, 32, 31, 33, 31, 34, 31, 35, 31, 36, 31, 37, 31, 38, 31, 39, 31, 40, 31, 41, 31, 42, 31, 43, 31, 44, 31, 45, 31, 46, 31, 47, 31, 48, 31, 49, 31, 50, 31, 51, 31, 52, 31, 53, 31, 54, 31, 55, 31, 56, 31, 57, 31, 58, 31, 59, 31, 60, 31, 61, 31, 62, 31, 63, 31, 64, 31, 65, 31, 66, 31, 67, 31, 68, 31, 69, 31],
	#[61, -105, 51, -148, 31, -141, 63, -136, 36, -94, 20, -106, 15, -142, 28, -161, 37, -97, 27, -143, 10, -159, 14, -118, 38, -98, 26, -127, 29, -111, 50, -132, 69, -147, 13, -108, 22, -128, 46, -135, 19, -152, 39, -96, 17, -107, 35, -146, 30, -125, 18, -131, 49, 339, 45, -119, 47, -153, 25, -113, 16, -137, 60, 300, 62, -117, 23, -150, 12, -157, 53, -114, 55, -112, 42, -101, 24, -102, 68, -149, 52, -100, 21, -115, 58, 300, 43, -130, 40, -121, 41, -124, 59, -134, 57, -151, 44, -122, 66, -145, 33, -104, 48, -103, 64, -120, 67, -133, 65, -129, 56, 300, 32, -158],
	#[65535, 27, 31, 27, 51, 27, 15, 27, 28, 27, 14, 27, 29, 27, 50, 27, 69, 27, 63, 27, 46, 27, 12, 27, 39, 27, 17, 27, 30, 27, 36, 27, 20, 27, 10, 27, 47, 27, 43, 27, 26, 27, 23, 27, 18, 27, 42, 27, 22, 27, 68, 27, 32, 27, 33, 27, 19, 27, 45, 27, 54, 27, 44, 27, 25, 27, 34, 27, 13, 27, 27, 27, 38, 27, 16, 27, 41, 27, 21, 27, 35, 27, 48, 27, 24, 27, 40, 27, 55, 27, 49, 27, 58, 27, 37, 27, 52, 27, 53, 27, 67, 27, 66, 27, 56, 27, 57, 27, 65, 27, 59, 27, 60, 27, 61, 27, 62, 27, 64, 27],
	#[65535, 23, 10, 23, 12, 23, 13, 23, 14, 23, 15, 23, 16, 23, 17, 23, 18, 23, 19, 23, 20, 23, 21, 23, 22, 23, 23, 23, 24, 23, 25, 23, 26, 23, 27, 23, 28, 23, 29, 23, 30, 23, 31, 23, 32, 23, 33, 23, 34, 23, 35, 23, 36, 23, 37, 23, 38, 23, 39, 23, 40, 23, 41, 23, 42, 23, 43, 23, 44, 23, 45, 23, 46, 23, 47, 23, 48, 23, 49, 23, 50, 23, 51, 23, 52, 23, 53, 23, 54, 23, 55, 23, 56, 23, 57, 23, 58, 23, 59, 23, 60, 23, 61, 23, 62, 23, 63, 23, 64, 23, 65, 23, 66, 23, 67, 23, 68, 23, 69, 23],
	#[23, -150, 15, -142, 14, -118, 13, -108, 22, -128, 27, -143, 21, -115, 26, -127, 16, -137, 10, -159, 12, -157, 17, -107, 18, -131, 19, -152, 20, -106, 46, -135, 24, -102, 25, -113, 28, -161, 29, -111, 30, -125, 31, -141, 32, -158, 33, -104, 35, -146, 36, -94, 37, -97, 38, -98, 39, -96, 40, -121, 41, -124, 42, -101, 43, -130, 44, -122, 45, -119, 47, -153, 48, -103, 49, 339, 50, -132, 51, -148, 52, -100, 53, -114, 55, -112, 56, 296, 57, -151, 59, -134, 61, -105, 62, -117, 63, -136, 64, -120, 65, -129, 66, -145, 67, -133, 68, -149, 69, -147],
	#[65535, 323, 14, 323, 51, 323, 13, 323, 43, 323, 21, 323, 15, 323, 10, 323, 47, 323, 29, 323, 38, 323, 19, 323, 28, 323, 12, 323, 16, 323, 55, 323, 33, 323, 22, 323, 46, 323, 27, 323, 31, 323, 26, 323, 35, 323, 39, 323, 17, 323, 18, 323, 36, 323, 20, 323, 23, 323, 24, 323, 25, 323, 30, 323, 42, 323, 32, 323, 59, 323, 37, 323, 48, 323, 62, 323, 40, 323, 41, 323, 61, 323, 60, 323, 44, 323, 45, 323, 58, 323, 49, 323, 50, 323, 56, 323, 52, 323, 53, 323, 57, 323, 63, 323, 64, 323, 65, 323, 66, 323, 67, 323, 68, 323, 69, 323],
	#[65535, 19, 10, 19, 12, 19, 13, 19, 14, 19, 15, 19, 16, 19, 17, 19, 18, 19, 19, 19, 20, 19, 21, 19, 22, 19, 23, 19, 24, 19, 25, 19, 26, 19, 27, 19, 28, 19, 29, 19, 30, 19, 31, 19, 32, 19, 33, 19, 34, 19, 35, 19, 36, 19, 37, 19, 38, 19, 39, 19, 40, 19, 41, 19, 42, 19, 43, 19, 44, 19, 45, 19, 46, 19, 47, 19, 48, 19, 49, 19, 50, 19, 51, 19, 52, 19, 53, 19, 54, 19, 55, 19, 56, 19, 57, 19, 58, 19, 59, 19, 60, 19, 61, 19, 62, 19, 63, 19, 64, 19, 65, 19, 66, 19, 67, 19, 68, 19, 69, 19],
	#[31, -141, 30, -125, 29, -111, 27, -143, 26, -127, 18, -131, 19, -152, 20, -106, 21, -115, 22, -128, 23, -150, 24, -102, 25, -113, 33, -104, 32, -158, 28, -161, 45, -174],
	#[65535, 325, 10, 325, 12, 325, 13, 325, 14, 325, 15, 325, 16, 325, 17, 325, 18, 325, 19, 325, 20, 325, 21, 325, 22, 325, 23, 325, 24, 325, 25, 325, 26, 325, 27, 325, 28, 325, 29, 325, 30, 325, 31, 325, 32, 325, 33, 325, 35, 325, 36, 325, 37, 325, 38, 325, 39, 325, 40, 325, 41, 325, 42, 325, 43, 325, 44, 325, 45, 325, 46, 325, 47, 325, 48, 325, 49, 325, 50, 325, 51, 325, 52, 325, 53, 325, 55, 325, 56, 325, 57, 325, 58, 325, 59, 325, 60, 325, 61, 325, 62, 325, 63, 325, 64, 325, 65, 325, 66, 325, 67, 325, 68, 325, 69, 325],
	#[10, -159, 12, -157, 13, -108, 14, -118, 15, -142, 16, -137, 17, -107, 18, -131, 19, -152, 20, -106, 21, -115, 22, -128, 23, -150, 24, -102, 25, -113, 26, -127, 27, -143, 28, -161, 29, -111, 30, -125, 31, -141, 32, -158, 33, -104, 35, -146, 36, -94, 37, -97, 38, -98, 39, -96, 40, -121, 41, -124, 42, -101, 43, -130, 44, -122, 45, -119, 46, -135, 47, -153, 48, -103, 49, 339, 50, -132, 51, -148, 52, -100, 53, -114, 55, -112, 57, -151, 59, -134, 60, 296, 61, -105, 62, -117, 63, -136, 64, -120, 65, -129, 66, -145, 67, -133, 68, -149, 69, -147],
	#[65535, 313, 15, 313, 14, 313, 51, 313, 43, 313, 20, 313, 29, 313, 10, 313, 31, 313, 30, 313, 39, 313, 28, 313, 12, 313, 13, 313, 27, 313, 26, 313, 16, 313, 17, 313, 18, 313, 19, 313, 42, 313, 21, 313, 22, 313, 23, 313, 24, 313, 25, 313, 33, 313, 32, 313, 50, 313, 41, 313, 35, 313, 36, 313, 37, 313, 38, 313, 40, 313, 49, 313, 48, 313, 44, 313, 45, 313, 46, 313, 47, 313, 52, 313, 53, 313, 55, 313, 56, 313, 57, 313, 58, 313, 59, 313, 60, 313, 61, 313, 62, 313, 63, 313, 64, 313, 65, 313, 66, 313, 67, 313, 68, 313, 69, 313],
	#[10, 309, 12, 309, 13, 309, 14, 309, 15, 309, 16, 309, 17, 309, 18, 309, 19, 309, 20, 309, 21, 309, 22, 309, 23, 309, 24, 309, 25, 309, 26, 309, 27, 309, 28, 309, 29, 309, 30, 309, 31, 309, 32, 309, 33, 309, 35, 309, 36, 309, 37, 309, 38, 309, 39, 309, 40, 309, 41, 309, 42, 309, 43, 309, 44, 309, 45, 309, 46, 309, 47, 309, 48, 309, 49, 309, 50, 309, 51, 309, 52, 309, 53, 309, 54, -170, 55, 309, 56, 309, 57, 309, 58, 309, 59, 309, 60, 309, 61, 309, 62, 309, 63, 309, 64, 309, 65, 309, 66, 309, 67, 309, 68, 309, 69, 309],
	#[65535, 16, 2, 16, 10, 16, 11, 16, 12, 16, 13, 16, 14, 16, 15, 16, 16, 16, 17, 16, 18, 16, 19, 16, 20, 16, 21, 16, 22, 16, 23, 16, 24, 16, 25, 16, 26, 16, 27, 16, 28, 16, 29, 16, 30, 16, 31, 16, 32, 16, 33, 16, 34, 16, 35, 16, 36, 16, 37, 16, 38, 16, 39, 16, 40, 16, 41, 16, 42, 16, 43, 16, 44, 16, 45, 16, 46, 16, 47, 16, 48, 16, 49, 16, 50, 16, 51, 16, 52, 16, 53, 16, 54, 16, 55, 16, 56, 16, 57, 16, 58, 16, 59, 16, 60, 16, 61, 16, 62, 16, 63, 16, 64, 16, 65, 16, 66, 16, 67, 16, 68, 16, 69, 16],
	#[65535, 312, 12, 312, 14, 312, 39, 312, 22, 312, 46, 312, 10, 312, 13, 312, 15, 312, 28, 312, 30, 312, 25, 312, 21, 312, 23, 312, 16, 312, 19, 312, 17, 312, 38, 312, 26, 312, 29, 312, 31, 312, 18, 312, 27, 312, 44, 312, 53, 312, 41, 312, 24, 312, 37, 312, 58, 312, 32, 312, 35, 312, 33, 312, 61, 312, 20, 312, 42, 312, 45, 312, 47, 312, 40, 312, 43, 312, 60, 312, 62, 312, 59, 312, 56, 312, 55, 312, 48, 312, 51, 312, 49, 312, 57, 312, 36, 312, 50, 312, 52, 312, 63, 312, 64, 312, 65, 312, 66, 312, 67, 312, 68, 312, 69, 312],
	#[65535, 331, 10, 331, 12, 331, 13, 331, 14, 331, 15, 331, 16, 331, 17, 331, 18, 331, 19, 331, 20, 331, 21, 331, 22, 331, 23, 331, 24, 331, 25, 331, 26, 331, 27, 331, 28, 331, 29, 331, 30, 331, 31, 331, 32, 331, 33, 331, 35, 331, 36, 331, 37, 331, 38, 331, 39, 331, 40, 331, 41, 331, 42, 331, 43, 331, 44, 331, 45, 331, 46, 331, 47, 331, 48, 331, 49, 331, 50, 331, 51, 331, 52, 331, 53, 331, 55, 331, 56, 331, 57, 331, 58, 331, 59, 331, 60, 331, 61, 331, 62, 331, 63, 331, 64, 331, 65, 331, 66, 331, 67, 331, 68, 331, 69, 331],
	#[65535, 311, 10, 311, 12, 311, 13, 311, 14, 311, 15, 311, 16, 311, 17, 311, 18, 311, 19, 311, 20, 311, 21, 311, 22, 311, 23, 311, 24, 311, 25, 311, 26, 311, 27, 311, 28, 311, 29, 311, 30, 311, 31, 311, 32, 311, 33, 311, 35, 311, 36, 311, 37, 311, 38, 311, 39, 311, 40, 311, 41, 311, 42, 311, 43, 311, 44, 311, 45, 311, 46, 311, 47, 311, 48, 311, 49, 311, 50, 311, 51, 311, 52, 311, 53, 311, 55, 311, 56, 311, 57, 311, 58, 311, 59, 311, 60, 311, 61, 311, 62, 311, 63, 311, 64, 311, 65, 311, 66, 311, 67, 311, 68, 311, 69, 311],
	#[65535, 32, 43, 32, 10, 32, 12, 32, 13, 32, 14, 32, 15, 32, 16, 32, 17, 32, 18, 32, 19, 32, 20, 32, 21, 32, 22, 32, 23, 32, 24, 32, 25, 32, 26, 32, 27, 32, 28, 32, 29, 32, 30, 32, 31, 32, 32, 32, 33, 32, 34, 32, 35, 32, 36, 32, 37, 32, 38, 32, 39, 32, 40, 32, 41, 32, 42, 32, 48, 32, 44, 32, 45, 32, 46, 32, 47, 32, 49, 32, 50, 32, 51, 32, 52, 32, 53, 32, 54, 32, 55, 32, 56, 32, 57, 32, 58, 32, 59, 32, 60, 32, 61, 32, 62, 32, 63, 32, 64, 32, 65, 32, 66, 32, 67, 32, 68, 32, 69, 32],
	#[65535, 15, 2, 15, 10, 15, 11, 15, 12, 15, 13, 15, 14, 15, 15, 15, 16, 15, 17, 15, 18, 15, 19, 15, 20, 15, 21, 15, 22, 15, 23, 15, 24, 15, 25, 15, 26, 15, 27, 15, 28, 15, 29, 15, 30, 15, 31, 15, 32, 15, 33, 15, 34, 15, 35, 15, 36, 15, 37, 15, 38, 15, 39, 15, 40, 15, 41, 15, 42, 15, 43, 15, 44, 15, 45, 15, 46, 15, 47, 15, 48, 15, 49, 15, 50, 15, 51, 15, 52, 15, 53, 15, 54, 15, 55, 15, 56, 15, 57, 15, 58, 15, 59, 15, 60, 15, 61, 15, 62, 15, 63, 15, 64, 15, 65, 15, 66, 15, 67, 15, 68, 15, 69, 15],
	#[65535, 28, 15, 28, 14, 28, 43, 28, 20, 28, 29, 28, 26, 28, 23, 28, 10, 28, 31, 28, 30, 28, 39, 28, 28, 28, 12, 28, 13, 28, 27, 28, 38, 28, 16, 28, 17, 28, 18, 28, 19, 28, 42, 28, 21, 28, 22, 28, 34, 28, 24, 28, 25, 28, 33, 28, 32, 28, 50, 28, 41, 28, 35, 28, 36, 28, 37, 28, 51, 28, 40, 28, 49, 28, 48, 28, 44, 28, 45, 28, 46, 28, 47, 28, 52, 28, 53, 28, 54, 28, 55, 28, 56, 28, 57, 28, 58, 28, 59, 28, 60, 28, 61, 28, 62, 28, 63, 28, 64, 28, 65, 28, 66, 28, 67, 28, 68, 28, 69, 28],
	#[65535, 297, 56, 297, 58, 297, 60, 297],
	#[10, -159, 12, -157, 13, -108, 14, -118, 15, -142, 16, -137, 17, -107, 18, -131, 19, -152, 20, -106, 21, -115, 22, -128, 23, -150, 24, -102, 25, -113, 26, -127, 27, -143, 28, -161, 29, -111, 30, -125, 31, -141, 32, -158, 33, -104, 35, -146, 36, -94, 37, -97, 38, -98, 39, -96, 40, -121, 41, -124, 42, -101, 43, -130, 44, -122, 45, -119, 46, -135, 47, -153, 48, -103, 49, 339, 50, -132, 51, -148, 52, -100, 53, -114, 55, -112, 57, -151, 58, 296, 59, -134, 61, -105, 62, -117, 63, -136, 64, -120, 65, -129, 66, -145, 67, -133, 68, -149, 69, -147],
	#[65535, 310, 55, 310, 43, 310, 14, 310, 39, 310, 22, 310, 46, 310, 13, 310, 15, 310, 28, 310, 12, 310, 25, 310, 21, 310, 23, 310, 10, 310, 19, 310, 17, 310, 38, 310, 16, 310, 29, 310, 31, 310, 18, 310, 27, 310, 44, 310, 53, 310, 41, 310, 24, 310, 37, 310, 58, 310, 26, 310, 35, 310, 30, 310, 61, 310, 20, 310, 42, 310, 45, 310, 47, 310, 40, 310, 33, 310, 60, 310, 62, 310, 59, 310, 56, 310, 32, 310, 48, 310, 51, 310, 49, 310, 57, 310, 36, 310, 50, 310, 52, 310, 63, 310, 64, 310, 65, 310, 66, 310, 67, 310, 68, 310, 69, 310],
	#[62, -117, 32, -158, 60, 296, 10, -159, 12, -157, 13, -108, 14, -118, 15, -142, 16, -137, 17, -107, 18, -131, 19, -152, 20, -106, 21, -115, 22, -128, 23, -150, 24, -102, 25, -113, 26, -127, 27, -143, 28, -161, 29, -111, 30, -125, 31, -141, 33, -104, 35, -146, 36, -94, 37, -97, 38, -98, 39, -96, 40, -121, 41, -124, 42, -101, 43, -130, 44, -122, 45, -119, 46, -135, 47, -153, 48, -103, 49, 339, 50, -132, 51, -148, 52, -100, 53, -114, 55, -112, 57, -151, 59, -134, 61, -105, 63, -136, 64, -120, 65, -129, 66, -145, 67, -133, 68, -149, 69, -147],
	#[18, -131, 19, -152, 20, -106, 21, -115, 22, -128, 23, -150, 24, -102, 25, -113, 26, -127, 27, -143, 28, -161, 29, -111, 30, -125, 31, -141, 32, -158, 33, -104],
	#[65535, 326, 51, 326, 43, 326, 39, 326, 15, 326, 14, 326, 10, 326, 31, 326, 50, 326, 30, 326, 35, 326, 29, 326, 12, 326, 13, 326, 27, 326, 26, 326, 16, 326, 17, 326, 18, 326, 19, 326, 20, 326, 21, 326, 22, 326, 23, 326, 24, 326, 25, 326, 33, 326, 32, 326, 28, 326, 41, 326, 49, 326, 36, 326, 37, 326, 38, 326, 40, 326, 42, 326, 48, 326, 44, 326, 45, 326, 46, 326, 47, 326, 52, 326, 53, 326, 55, 326, 56, 326, 57, 326, 58, 326, 59, 326, 60, 326, 61, 326, 62, 326, 63, 326, 64, 326, 65, 326, 66, 326, 67, 326, 68, 326, 69, 326],
	#[65535, 24, 10, 24, 12, 24, 13, 24, 14, 24, 15, 24, 16, 24, 17, 24, 18, 24, 19, 24, 20, 24, 21, 24, 22, 24, 23, 24, 24, 24, 25, 24, 26, 24, 27, 24, 28, 24, 29, 24, 30, 24, 31, 24, 32, 24, 33, 24, 34, 24, 35, 24, 36, 24, 37, 24, 38, 24, 39, 24, 40, 24, 41, 24, 42, 24, 43, 24, 44, 24, 45, 24, 46, 24, 47, 24, 48, 24, 49, 24, 50, 24, 51, 24, 52, 24, 53, 24, 54, 24, 55, 24, 56, 24, 57, 24, 58, 24, 59, 24, 60, 24, 61, 24, 62, 24, 63, 24, 64, 24, 65, 24, 66, 24, 67, 24, 68, 24, 69, 24],
	#[10, -159, 12, -157, 13, -108, 14, -118, 15, -142, 16, -137, 17, -107, 18, -131, 19, -152, 20, -106, 21, -115, 22, -128, 23, -150, 24, -102, 25, -113, 26, -127, 27, -143, 28, -161, 29, -111, 30, -125, 31, -141, 32, -158, 33, -104, 35, -146, 36, -94, 37, -97, 38, -98, 39, -96, 40, -121, 41, -124, 42, -101, 43, -130, 44, -122, 45, -119, 46, -135, 47, -153, 48, -103, 49, 339, 50, -132, 51, -148, 52, -100, 53, -114, 55, -112, 57, -151, 58, 296, 59, -134, 61, -105, 62, -117, 63, -136, 64, -120, 65, -129, 66, -145, 67, -133, 68, -149, 69, -147],
	#[65535, 20, 15, 20, 23, 20, 14, 20, 31, 20, 13, 20, 10, 20, 43, 20, 59, 20, 18, 20, 51, 20, 27, 20, 29, 20, 38, 20, 19, 20, 28, 20, 12, 20, 16, 20, 55, 20, 22, 20, 34, 20, 21, 20, 26, 20, 49, 20, 54, 20, 35, 20, 39, 20, 17, 20, 47, 20, 36, 20, 20, 20, 46, 20, 24, 20, 25, 20, 30, 20, 42, 20, 32, 20, 33, 20, 37, 20, 48, 20, 62, 20, 40, 20, 41, 20, 61, 20, 60, 20, 44, 20, 45, 20, 58, 20, 50, 20, 56, 20, 52, 20, 53, 20, 57, 20, 63, 20, 64, 20, 65, 20, 66, 20, 67, 20, 68, 20, 69, 20],
	#[65535, 314, 10, 314, 12, 314, 13, 314, 14, 314, 15, 314, 16, 314, 17, 314, 18, 314, 19, 314, 20, 314, 21, 314, 22, 314, 23, 314, 24, 314, 25, 314, 26, 314, 27, 314, 28, 314, 29, 314, 30, 314, 31, 314, 32, 314, 33, 314, 35, 314, 36, 314, 37, 314, 38, 314, 39, 314, 40, 314, 41, 314, 42, 314, 43, 314, 44, 314, 45, 314, 46, 314, 47, 314, 48, 314, 49, 314, 50, 314, 51, 314, 52, 314, 53, 314, 55, 314, 56, 314, 57, 314, 58, 314, 59, 314, 60, 314, 61, 314, 62, 314, 63, 314, 64, 314, 65, 314, 66, 314, 67, 314, 68, 314, 69, 314],
	#[65535, 340, 49, 340],
	#[65535, 18, 15, 18, 14, 18, 10, 18, 31, 18, 20, 18, 26, 18, 30, 18, 29, 18, 12, 18, 13, 18, 27, 18, 38, 18, 16, 18, 17, 18, 18, 18, 19, 18, 42, 18, 21, 18, 22, 18, 23, 18, 24, 18, 25, 18, 33, 18, 32, 18, 28, 18, 34, 18, 35, 18, 36, 18, 37, 18, 51, 18, 39, 18, 40, 18, 41, 18, 49, 18, 43, 18, 44, 18, 45, 18, 46, 18, 47, 18, 48, 18, 50, 18, 52, 18, 53, 18, 54, 18, 55, 18, 56, 18, 57, 18, 58, 18, 59, 18, 60, 18, 61, 18, 62, 18, 63, 18, 64, 18, 65, 18, 66, 18, 67, 18, 68, 18, 69, 18],
	#[65535, 305, 10, 305, 12, 305, 13, 305, 14, 305, 15, 305, 16, 305, 17, 305, 18, 305, 19, 305, 20, 305, 21, 305, 22, 305, 23, 305, 24, 305, 25, 305, 26, 305, 27, 305, 28, 305, 29, 305, 30, 305, 31, 305, 32, 305, 33, 305, 35, 305, 36, 305, 37, 305, 38, 305, 39, 305, 40, 305, 41, 305, 42, 305, 43, 305, 44, 305, 45, 305, 46, 305, 47, 305, 48, 305, 49, 305, 50, 305, 51, 305, 52, 305, 53, 305, 55, 305, 56, 305, 57, 305, 58, 305, 59, 305, 60, 305, 61, 305, 62, 305, 63, 305, 64, 305, 65, 305, 66, 305, 67, 305, 68, 305, 69, 305],
	#[65535, 12, 2, 12, 10, 12, 11, 12, 12, 12, 13, 12, 14, 12, 15, 12, 16, 12, 17, 12, 18, 12, 19, 12, 20, 12, 21, 12, 22, 12, 23, 12, 24, 12, 25, 12, 26, 12, 27, 12, 28, 12, 29, 12, 30, 12, 31, 12, 32, 12, 33, 12, 34, 12, 35, 12, 36, 12, 37, 12, 38, 12, 39, 12, 40, 12, 41, 12, 42, 12, 43, 12, 44, 12, 45, 12, 46, 12, 47, 12, 48, 12, 49, 12, 50, 12, 51, 12, 52, 12, 53, 12, 54, 12, 55, 12, 56, 12, 57, 12, 58, 12, 59, 12, 60, 12, 61, 12, 62, 12, 63, 12, 64, 12, 65, 12, 66, 12, 67, 12, 68, 12, 69, 12],
	#[65535, 33, 51, 33, 15, 33, 14, 33, 10, 33, 31, 33, 30, 33, 42, 33, 29, 33, 12, 33, 13, 33, 27, 33, 26, 33, 16, 33, 17, 33, 18, 33, 19, 33, 20, 33, 21, 33, 22, 33, 23, 33, 24, 33, 25, 33, 33, 33, 32, 33, 28, 33, 34, 33, 35, 33, 36, 33, 37, 33, 38, 33, 39, 33, 40, 33, 41, 33, 49, 33, 43, 33, 44, 33, 45, 33, 46, 33, 47, 33, 48, 33, 50, 33, 52, 33, 53, 33, 54, 33, 55, 33, 56, 33, 57, 33, 58, 33, 59, 33, 60, 33, 61, 33, 62, 33, 63, 33, 64, 33, 65, 33, 66, 33, 67, 33, 68, 33, 69, 33],
	#[65535, 329, 10, 329, 12, 329, 13, 329, 14, 329, 15, 329, 16, 329, 17, 329, 18, 329, 19, 329, 20, 329, 21, 329, 22, 329, 23, 329, 24, 329, 25, 329, 26, 329, 27, 329, 28, 329, 29, 329, 30, 329, 31, 329, 32, 329, 33, 329, 35, 329, 36, 329, 37, 329, 38, 329, 39, 329, 40, 329, 41, 329, 42, 329, 43, 329, 44, 329, 45, 329, 46, 329, 47, 329, 48, 329, 49, 329, 50, 329, 51, 329, 52, 329, 53, 329, 55, 329, 56, 329, 57, 329, 58, 329, 59, 329, 60, 329, 61, 329, 62, 329, 63, 329, 64, 329, 65, 329, 66, 329, 67, 329, 68, 329, 69, 329],
	#[60, -162],
	#[65535, 29, 39, 29, 51, 29, 26, 29, 49, 29, 10, 29, 12, 29, 13, 29, 14, 29, 15, 29, 16, 29, 17, 29, 18, 29, 19, 29, 20, 29, 21, 29, 22, 29, 23, 29, 24, 29, 25, 29, 33, 29, 27, 29, 28, 29, 29, 29, 30, 29, 31, 29, 32, 29, 34, 29, 35, 29, 36, 29, 37, 29, 38, 29, 40, 29, 41, 29, 42, 29, 43, 29, 44, 29, 45, 29, 46, 29, 47, 29, 48, 29, 50, 29, 52, 29, 53, 29, 54, 29, 55, 29, 56, 29, 57, 29, 58, 29, 59, 29, 60, 29, 61, 29, 62, 29, 63, 29, 64, 29, 65, 29, 66, 29, 67, 29, 68, 29, 69, 29],
	#[65535, 231, 2, 231, 10, 231, 11, 231, 12, 231, 13, 231, 14, 231, 15, 231, 16, 231, 17, 231, 19, 231, 20, 231, 21, 231, 26, 231, 27, 231, 30, 231, 31, 231, 32, 231, 33, 231, 34, 231, 35, 231, 36, 231, 37, 231, 38, 231, 39, 231, 40, 231, 41, 231, 42, 231, 43, 231, 44, 231, 45, 231, 46, 231, 47, 231, 48, 231, 49, 231, 50, 231, 51, 231, 52, 231, 53, 231, 54, 231, 55, 231, 56, 231, 57, 231, 58, 231, 59, 231, 60, 231, 61, 231, 62, 231, 63, 231, 64, 231, 65, 231, 66, 231, 67, 231, 68, 231],
	#[58, -164],
	#[65535, 317, 10, 317, 12, 317, 13, 317, 14, 317, 15, 317, 16, 317, 17, 317, 18, 317, 19, 317, 20, 317, 21, 317, 22, 317, 23, 317, 24, 317, 25, 317, 26, 317, 27, 317, 28, 317, 29, 317, 30, 317, 31, 317, 32, 317, 33, 317, 35, 317, 36, 317, 37, 317, 38, 317, 39, 317, 40, 317, 41, 317, 42, 317, 43, 317, 44, 317, 45, 317, 46, 317, 47, 317, 48, 317, 49, 317, 50, 317, 51, 317, 52, 317, 53, 317, 55, 317, 56, 317, 57, 317, 58, 317, 59, 317, 60, 317, 61, 317, 62, 317, 63, 317, 64, 317, 65, 317, 66, 317, 67, 317, 68, 317, 69, 317],
	#[65535, 336, 10, 336, 12, 336, 13, 336, 14, 336, 15, 336, 16, 336, 17, 336, 18, 336, 19, 336, 20, 336, 21, 336, 22, 336, 23, 336, 24, 336, 25, 336, 26, 336, 27, 336, 28, 336, 29, 336, 30, 336, 31, 336, 32, 336, 33, 336, 35, 336, 36, 336, 37, 336, 38, 336, 39, 336, 40, 336, 41, 336, 42, 336, 43, 336, 44, 336, 45, 336, 46, 336, 47, 336, 48, 336, 49, 336, 50, 336, 51, 336, 52, 336, 53, 336, 55, 336, 56, 336, 57, 336, 58, 336, 59, 336, 60, 336, 61, 336, 62, 336, 63, 336, 64, 336, 65, 336, 66, 336, 67, 336, 68, 336, 69, 336],
	#[60, -167],
	#[65535, 304, 12, 304, 10, 304, 13, 304, 15, 304, 16, 304, 17, 304, 18, 304, 19, 304, 20, 304, 21, 304, 22, 304, 23, 304, 24, 304, 25, 304, 26, 304, 27, 304, 28, 304, 29, 304, 30, 304, 31, 304, 32, 304, 33, 304, 41, 304, 35, 304, 36, 304, 37, 304, 38, 304, 40, 304, 42, 304, 43, 304, 44, 304, 45, 304, 46, 304, 47, 304, 48, 304, 49, 304, 50, 304, 51, 304, 52, 304, 53, 304, 55, 304, 56, 304, 57, 304, 58, 304, 59, 304, 60, 304, 61, 304, 62, 304, 63, 304, 64, 304, 65, 304, 66, 304, 67, 304, 68, 304, 69, 304, 14, 304, 39, 304],
	#[58, -169],
	#[65535, 303, 55, 303, 47, 303, 43, 303, 37, 303, 14, 303, 51, 303, 45, 303, 20, 303, 13, 303, 15, 303, 53, 303, 44, 303, 28, 303, 12, 303, 50, 303, 21, 303, 23, 303, 52, 303, 36, 303, 38, 303, 35, 303, 31, 303, 18, 303, 27, 303, 22, 303, 10, 303, 41, 303, 49, 303, 19, 303, 39, 303, 26, 303, 48, 303, 30, 303, 16, 303, 42, 303, 17, 303, 33, 303, 32, 303, 24, 303, 40, 303, 29, 303, 25, 303, 46, 303, 56, 303, 57, 303, 58, 303, 59, 303, 60, 303, 61, 303, 62, 303, 63, 303, 64, 303, 65, 303, 66, 303, 67, 303, 68, 303, 69, 303],
	#[65535, 341, 49, 341],
	#[60, -172],
	#[65535, 318, 55, 318, 51, 318, 44, 318, 10, 318, 59, 318, 15, 318, 47, 318, 43, 318, 19, 318, 23, 318, 22, 318, 31, 318, 61, 318, 18, 318, 29, 318, 12, 318, 13, 318, 14, 318, 26, 318, 16, 318, 17, 318, 25, 318, 24, 318, 20, 318, 21, 318, 27, 318, 28, 318, 30, 318, 42, 318, 32, 318, 33, 318, 35, 318, 36, 318, 37, 318, 38, 318, 39, 318, 40, 318, 41, 318, 60, 318, 45, 318, 46, 318, 58, 318, 48, 318, 49, 318, 50, 318, 56, 318, 52, 318, 53, 318, 57, 318, 62, 318, 63, 318, 64, 318, 65, 318, 66, 318, 67, 318, 68, 318, 69, 318],
	#[65535, 334, 30, 334, 15, 334, 23, 334, 22, 334, 27, 334, 31, 334, 20, 334, 19, 334, 32, 334, 10, 334, 25, 334, 12, 334, 13, 334, 14, 334, 26, 334, 16, 334, 17, 334, 18, 334, 24, 334, 41, 334, 21, 334, 44, 334, 28, 334, 29, 334, 43, 334, 42, 334, 33, 334, 35, 334, 36, 334, 37, 334, 38, 334, 39, 334, 40, 334, 45, 334, 46, 334, 47, 334, 48, 334, 49, 334, 50, 334, 51, 334, 52, 334, 53, 334, 55, 334, 56, 334, 57, 334, 58, 334, 59, 334, 60, 334, 61, 334, 62, 334, 63, 334, 64, 334, 65, 334, 66, 334, 67, 334, 68, 334, 69, 334],
	#[65535, 333, 61, 333, 23, 333, 21, 333, 58, 333, 26, 333, 60, 333, 10, 333, 12, 333, 13, 333, 14, 333, 15, 333, 16, 333, 17, 333, 18, 333, 19, 333, 20, 333, 56, 333, 22, 333, 46, 333, 24, 333, 25, 333, 55, 333, 27, 333, 28, 333, 29, 333, 30, 333, 31, 333, 32, 333, 33, 333, 35, 333, 36, 333, 37, 333, 38, 333, 39, 333, 40, 333, 41, 333, 42, 333, 43, 333, 44, 333, 45, 333, 59, 333, 47, 333, 48, 333, 49, 333, 50, 333, 51, 333, 52, 333, 53, 333, 57, 333, 62, 333, 63, 333, 64, 333, 65, 333, 66, 333, 67, 333, 68, 333, 69, 333],
	#[56, -176],
	#[65535, 302, 10, 302, 12, 302, 13, 302, 14, 302, 15, 302, 16, 302, 17, 302, 18, 302, 19, 302, 20, 302, 21, 302, 22, 302, 23, 302, 24, 302, 25, 302, 26, 302, 27, 302, 28, 302, 29, 302, 30, 302, 31, 302, 32, 302, 33, 302, 35, 302, 36, 302, 37, 302, 38, 302, 39, 302, 40, 302, 41, 302, 42, 302, 43, 302, 44, 302, 45, 302, 46, 302, 47, 302, 48, 302, 49, 302, 50, 302, 51, 302, 52, 302, 53, 302, 55, 302, 56, 302, 57, 302, 58, 302, 59, 302, 60, 302, 61, 302, 62, 302, 63, 302, 64, 302, 65, 302, 66, 302, 67, 302, 68, 302, 69, 302],
	#[65535, 301, 56, 301, 60, 301, 58, 301],
	#[65535, 299, 56, 299, 58, 299, 60, 299],
	#[56, -180],
	#[65535, 316, 46, 316, 45, 316, 62, 316, 44, 316, 43, 316, 15, 316, 59, 316, 47, 316, 52, 316, 29, 316, 20, 316, 30, 316, 55, 316, 50, 316, 18, 316, 37, 316, 28, 316, 12, 316, 14, 316, 38, 316, 61, 316, 27, 316, 39, 316, 22, 316, 17, 316, 10, 316, 13, 316, 35, 316, 53, 316, 57, 316, 51, 316, 25, 316, 33, 316, 21, 316, 42, 316, 16, 316, 19, 316, 36, 316, 58, 316, 41, 316, 49, 316, 31, 316, 24, 316, 60, 316, 56, 316, 32, 316, 48, 316, 26, 316, 23, 316, 40, 316, 63, 316, 64, 316, 65, 316, 66, 316, 67, 316, 68, 316, 69, 316],
	#[18, -131, 19, -152, 20, -106, 21, -115, 22, -128, 23, -150, 24, -102, 25, -113, 26, -127, 27, -143, 28, -161, 29, -111, 30, -125, 31, -141, 32, -158, 33, -104, 41, -186, 45, -184, 55, -182, 63, -183],
	#[36, -64, 21, -16, 55, -14, 13, -66, 22, -32, 53, -17, 35, -53, 33, -29, 18, -34, 20, -5, 42, -61, 34, -51, 43, -18, 41, -27, 19, -58, 23, -50, 12, -43, 29, -13, 28, -63, 61, -7, 62, -22, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 346, 45, 346, 54, 346, 53, 346, 43, 346, 47, 346, 29, 346, 23, 346, 30, 346, 55, 346, 37, 346, 31, 346, 14, 346, 17, 346, 52, 346, 50, 346, 39, 346, 22, 346, 10, 346, 13, 346, 15, 346, 49, 346, 44, 346, 51, 346, 16, 346, 27, 346, 18, 346, 21, 346, 42, 346, 19, 346, 36, 346, 38, 346, 35, 346, 26, 346, 48, 346, 25, 346, 41, 346, 24, 346, 20, 346, 32, 346, 33, 346, 28, 346, 12, 346, 40, 346, 46, 346, 56, 346, 57, 346, 58, 346, 59, 346, 60, 346, 61, 346, 62, 346, 63, 346, 64, 346, 65, 346, 66, 346, 67, 346, 68, 346, 69, 346],
	#[65535, 348, 13, 348, 15, 348, 14, 348, 44, 348, 23, 348, 42, 348, 22, 348, 43, 348, 24, 348, 10, 348, 25, 348, 12, 348, 41, 348, 27, 348, 26, 348, 16, 348, 17, 348, 18, 348, 19, 348, 20, 348, 21, 348, 32, 348, 28, 348, 29, 348, 30, 348, 31, 348, 33, 348, 35, 348, 36, 348, 37, 348, 38, 348, 39, 348, 40, 348, 45, 348, 46, 348, 47, 348, 48, 348, 49, 348, 50, 348, 51, 348, 52, 348, 53, 348, 54, 348, 55, 348, 56, 348, 57, 348, 58, 348, 59, 348, 60, 348, 61, 348, 62, 348, 63, 348, 64, 348, 65, 348, 66, 348, 67, 348, 68, 348, 69, 348],
	#[10, 342, 12, 342, 13, 342, 14, 342, 15, 342, 16, 342, 17, 342, 18, 342, 19, 342, 20, 342, 21, 342, 22, 342, 23, 342, 24, 342, 25, 342, 26, 342, 27, 342, 28, 342, 29, 342, 30, 342, 31, 342, 32, 342, 33, 342, 35, 342, 36, 342, 37, 342, 38, 342, 39, 342, 40, 342, 41, 342, 42, 342, 43, 342, 44, 342, 45, 342, 46, 342, 47, 342, 48, 342, 49, 342, 50, 342, 51, 342, 52, 342, 53, 342, 54, -189, 55, 342, 56, 342, 57, 342, 58, 342, 59, 342, 60, 342, 61, 342, 62, 342, 63, 342, 64, 342, 65, 342, 66, 342, 67, 342, 68, 342, 69, 342],
	#[65535, 347, 47, 347, 22, 347, 31, 347, 28, 347, 54, 347, 39, 347, 51, 347, 29, 347, 15, 347, 62, 347, 49, 347, 59, 347, 37, 347, 35, 347, 23, 347, 46, 347, 61, 347, 55, 347, 45, 347, 43, 347, 30, 347, 52, 347, 36, 347, 13, 347, 53, 347, 16, 347, 14, 347, 27, 347, 57, 347, 44, 347, 21, 347, 12, 347, 19, 347, 50, 347, 18, 347, 42, 347, 10, 347, 25, 347, 20, 347, 41, 347, 58, 347, 24, 347, 17, 347, 60, 347, 38, 347, 32, 347, 48, 347, 33, 347, 56, 347, 26, 347, 40, 347, 63, 347, 64, 347, 65, 347, 66, 347, 67, 347, 68, 347, 69, 347],
	#[65535, 345, 39, 345, 15, 345, 14, 345, 19, 345, 20, 345, 10, 345, 12, 345, 13, 345, 16, 345, 17, 345, 18, 345, 21, 345, 22, 345, 23, 345, 24, 345, 25, 345, 26, 345, 27, 345, 28, 345, 29, 345, 30, 345, 31, 345, 32, 345, 33, 345, 35, 345, 36, 345, 37, 345, 38, 345, 40, 345, 41, 345, 42, 345, 43, 345, 44, 345, 45, 345, 46, 345, 47, 345, 48, 345, 49, 345, 50, 345, 51, 345, 52, 345, 53, 345, 54, 345, 55, 345, 56, 345, 57, 345, 58, 345, 59, 345, 60, 345, 61, 345, 62, 345, 63, 345, 64, 345, 65, 345, 66, 345, 67, 345, 68, 345, 69, 345],
	#[65535, 343, 35, 343, 46, 343, 51, 343, 20, 343, 23, 343, 30, 343, 55, 343, 60, 343, 37, 343, 31, 343, 38, 343, 61, 343, 39, 343, 22, 343, 19, 343, 56, 343, 15, 343, 53, 343, 58, 343, 28, 343, 12, 343, 25, 343, 18, 343, 21, 343, 43, 343, 10, 343, 14, 343, 45, 343, 62, 343, 26, 343, 29, 343, 48, 343, 27, 343, 44, 343, 57, 343, 59, 343, 32, 343, 52, 343, 42, 343, 17, 343, 40, 343, 16, 343, 41, 343, 49, 343, 13, 343, 50, 343, 33, 343, 24, 343, 36, 343, 47, 343, 63, 343, 64, 343, 65, 343, 66, 343, 67, 343, 68, 343, 69, 343],
	#[63, -191],
	#[65535, 332, 10, 332, 29, 332, 13, 332, 22, 332, 53, 332, 39, 332, 30, 332, 18, 332, 15, 332, 47, 332, 14, 332, 38, 332, 19, 332, 28, 332, 12, 332, 35, 332, 55, 332, 27, 332, 21, 332, 20, 332, 17, 332, 16, 332, 50, 332, 52, 332, 23, 332, 24, 332, 25, 332, 26, 332, 43, 332, 31, 332, 32, 332, 33, 332, 41, 332, 49, 332, 36, 332, 37, 332, 51, 332, 40, 332, 42, 332, 48, 332, 44, 332, 45, 332, 46, 332, 56, 332, 57, 332, 58, 332, 59, 332, 60, 332, 61, 332, 62, 332, 63, 332, 64, 332, 65, 332, 66, 332, 67, 332, 68, 332, 69, 332],
	#[65535, 344, 15, 344, 47, 344, 23, 344, 55, 344, 42, 344, 14, 344, 51, 344, 45, 344, 39, 344, 46, 344, 59, 344, 13, 344, 35, 344, 53, 344, 44, 344, 28, 344, 50, 344, 43, 344, 61, 344, 52, 344, 36, 344, 38, 344, 62, 344, 16, 344, 29, 344, 31, 344, 27, 344, 22, 344, 10, 344, 12, 344, 49, 344, 37, 344, 58, 344, 26, 344, 21, 344, 30, 344, 18, 344, 20, 344, 57, 344, 17, 344, 25, 344, 33, 344, 60, 344, 19, 344, 24, 344, 48, 344, 41, 344, 40, 344, 56, 344, 32, 344, 63, 344, 64, 344, 65, 344, 66, 344, 67, 344, 68, 344, 69, 344],
	#[56, -193],
	#[65535, 349, 15, 349, 14, 349, 44, 349, 23, 349, 22, 349, 31, 349, 10, 349, 25, 349, 12, 349, 13, 349, 24, 349, 26, 349, 16, 349, 17, 349, 18, 349, 19, 349, 20, 349, 21, 349, 27, 349, 28, 349, 29, 349, 30, 349, 42, 349, 32, 349, 33, 349, 35, 349, 36, 349, 37, 349, 38, 349, 39, 349, 40, 349, 41, 349, 43, 349, 45, 349, 46, 349, 47, 349, 48, 349, 49, 349, 50, 349, 51, 349, 52, 349, 53, 349, 54, 349, 55, 349, 56, 349, 57, 349, 58, 349, 59, 349, 60, 349, 61, 349, 62, 349, 63, 349, 64, 349, 65, 349, 66, 349, 67, 349, 68, 349, 69, 349],
	#[38, 228, 37, 228, 2, 228, 46, -67, 39, 228, 11, 228, 58, 228, 48, 228, 57, -68, 55, -69, 10, 228, 40, 228, 56, 228, 36, 228],
	#[58, -370],
	#[65535, 177, 40, 177, 56, 177, 58, 177],
	#[45, -214, 55, -207, 61, -203, 41, -218, 22, -32, 46, -224, 59, -219, 35, -229, 53, -210, 30, -28, 54, -221, 21, -16, 23, -50, 10, -238, 52, -198, 14, -118, 20, -5, 62, -213, 56, 92, 29, -13, 51, -230, 27, -46, 44, -222, 28, -63, 43, -211, 57, -232, 39, -205, 48, -201, 50, -223, 18, -34, 15, -142, 42, -236, 40, -215, 16, -137, 38, -199, 19, -58, 13, -108, 12, -157, 17, -107, 32, -237, 33, -29, 37, -208, 49, -212, 34, -51, 36, -239, 26, -30, 47, -233, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 138, 47, 138, 23, 138, 30, 138, 55, 138, 28, 138, 45, 138, 50, 138, 20, 138, 22, 138, 46, 138, 15, 138, 44, 138, 51, 138, 37, 138, 54, 138, 43, 138, 19, 138, 14, 138, 38, 138, 35, 138, 29, 138, 31, 138, 18, 138, 27, 138, 42, 138, 53, 138, 12, 138, 49, 138, 2, 138, 39, 138, 26, 138, 21, 138, 52, 138, 36, 138, 16, 138, 32, 138, 17, 138, 34, 138, 33, 138, 41, 138, 48, 138, 13, 138, 40, 138, 10, 138, 11, 138, 56, 138, 57, 138, 58, 138, 59, 138, 60, 138, 61, 138, 62, 138, 63, 138, 64, 138, 65, 138, 66, 138, 67, 138, 68, 138, 69, 138],
	#[65535, 73, 22, 73, 15, 73, 47, 73, 28, 73, 27, 73, 2, 73, 43, 73, 54, 73, 42, 73, 10, 73, 11, 73, 12, 73, 13, 73, 14, 73, 53, 73, 16, 73, 17, 73, 18, 73, 19, 73, 20, 73, 21, 73, 35, 73, 23, 73, 51, 73, 26, 73, 49, 73, 29, 73, 30, 73, 31, 73, 32, 73, 33, 73, 34, 73, 52, 73, 36, 73, 37, 73, 38, 73, 39, 73, 40, 73, 41, 73, 48, 73, 44, 73, 45, 73, 46, 73, 50, 73, 55, 73, 56, 73, 57, 73, 58, 73, 59, 73, 60, 73, 61, 73, 62, 73, 63, 73, 64, 73, 65, 73, 66, 73, 67, 73, 68, 73, 69, 73],
	#[65535, 121, 15, 121, 50, 121, 14, 121, 23, 121, 26, 121, 46, 121, 30, 121, 39, 121, 29, 121, 52, 121, 43, 121, 62, 121, 47, 121, 37, 121, 35, 121, 22, 121, 44, 121, 28, 121, 27, 121, 45, 121, 48, 121, 31, 121, 54, 121, 13, 121, 18, 121, 53, 121, 16, 121, 10, 121, 38, 121, 57, 121, 2, 121, 21, 121, 12, 121, 19, 121, 59, 121, 49, 121, 42, 121, 11, 121, 36, 121, 20, 121, 41, 121, 32, 121, 55, 121, 17, 121, 60, 121, 34, 121, 61, 121, 58, 121, 33, 121, 56, 121, 51, 121, 40, 121, 63, 121, 64, 121, 65, 121, 66, 121, 67, 121, 68, 121, 69, 121],
	#[65535, 134, 23, 134, 31, 134, 26, 134, 44, 134, 13, 134, 12, 134, 22, 134, 27, 134, 2, 134, 43, 134, 10, 134, 11, 134, 34, 134, 41, 134, 14, 134, 15, 134, 16, 134, 17, 134, 18, 134, 19, 134, 20, 134, 21, 134, 32, 134, 28, 134, 29, 134, 30, 134, 42, 134, 33, 134, 35, 134, 36, 134, 37, 134, 38, 134, 39, 134, 40, 134, 45, 134, 46, 134, 47, 134, 48, 134, 49, 134, 50, 134, 51, 134, 52, 134, 53, 134, 54, 134, 55, 134, 56, 134, 57, 134, 58, 134, 59, 134, 60, 134, 61, 134, 62, 134, 63, 134, 64, 134, 65, 134, 66, 134, 67, 134, 68, 134, 69, 134],
	#[65535, 127, 54, 127, 47, 127, 30, 127, 55, 127, 14, 127, 38, 127, 52, 127, 50, 127, 43, 127, 22, 127, 46, 127, 59, 127, 15, 127, 53, 127, 44, 127, 51, 127, 37, 127, 27, 127, 42, 127, 19, 127, 36, 127, 45, 127, 62, 127, 26, 127, 29, 127, 31, 127, 18, 127, 13, 127, 16, 127, 28, 127, 48, 127, 2, 127, 39, 127, 17, 127, 35, 127, 33, 127, 11, 127, 20, 127, 57, 127, 34, 127, 60, 127, 49, 127, 23, 127, 12, 127, 56, 127, 32, 127, 10, 127, 40, 127, 21, 127, 61, 127, 41, 127, 58, 127, 63, 127, 64, 127, 65, 127, 66, 127, 67, 127, 68, 127, 69, 127],
	#[65535, 145, 15, 145, 50, 145, 14, 145, 23, 145, 22, 145, 46, 145, 44, 145, 30, 145, 39, 145, 29, 145, 13, 145, 38, 145, 20, 145, 47, 145, 35, 145, 21, 145, 51, 145, 28, 145, 55, 145, 45, 145, 43, 145, 31, 145, 52, 145, 57, 145, 48, 145, 53, 145, 42, 145, 10, 145, 37, 145, 62, 145, 2, 145, 16, 145, 12, 145, 19, 145, 59, 145, 18, 145, 32, 145, 11, 145, 36, 145, 34, 145, 41, 145, 49, 145, 54, 145, 17, 145, 60, 145, 56, 145, 61, 145, 58, 145, 27, 145, 33, 145, 26, 145, 40, 145, 63, 145, 64, 145, 65, 145, 66, 145, 67, 145, 68, 145, 69, 145],
	#[65535, 122, 47, 122, 30, 122, 28, 122, 45, 122, 50, 122, 39, 122, 15, 122, 53, 122, 51, 122, 37, 122, 54, 122, 21, 122, 23, 122, 19, 122, 14, 122, 38, 122, 29, 122, 31, 122, 18, 122, 27, 122, 22, 122, 10, 122, 43, 122, 2, 122, 16, 122, 26, 122, 35, 122, 52, 122, 36, 122, 20, 122, 42, 122, 17, 122, 34, 122, 33, 122, 41, 122, 48, 122, 13, 122, 12, 122, 55, 122, 40, 122, 11, 122, 32, 122, 49, 122, 44, 122, 46, 122, 56, 122, 57, 122, 58, 122, 59, 122, 60, 122, 61, 122, 62, 122, 63, 122, 64, 122, 65, 122, 66, 122, 67, 122, 68, 122, 69, 122],
	#[65535, 74, 15, 74, 14, 74, 53, 74, 31, 74, 44, 74, 39, 74, 29, 74, 18, 74, 13, 74, 38, 74, 20, 74, 37, 74, 23, 74, 46, 74, 28, 74, 55, 74, 45, 74, 2, 74, 30, 74, 54, 74, 36, 74, 35, 74, 17, 74, 11, 74, 10, 74, 27, 74, 21, 74, 12, 74, 19, 74, 50, 74, 22, 74, 47, 74, 42, 74, 51, 74, 40, 74, 41, 74, 32, 74, 60, 74, 59, 74, 48, 74, 16, 74, 61, 74, 58, 74, 26, 74, 52, 74, 43, 74, 33, 74, 34, 74, 49, 74, 62, 74, 57, 74, 56, 74, 63, 74, 64, 74, 65, 74, 66, 74, 67, 74, 68, 74, 69, 74],
	#[65535, 94, 2, 94, 31, 94, 56, 94, 58, 94, 60, 94],
	#[27, -46, 30, -28, 15, -142, 23, -50, 22, -32, 49, -212, 34, -51, 41, -218, 32, -237, 10, -238, 12, -157, 13, -108, 14, -118, 26, -30, 16, -137, 17, -107, 18, -34, 19, -58, 20, -5, 21, -16, 44, -222, 28, -63, 29, -13, 43, -211, 42, -236, 33, -29, 35, -229, 36, -239, 37, -208, 38, -199, 39, -205, 40, -215, 45, -214, 46, -224, 47, -233, 48, -201, 50, -223, 51, -230, 52, -198, 53, -210, 54, -221, 55, -207, 56, 92, 57, -232, 59, -219, 61, -203, 62, -213, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 71, 50, 71, 23, 71, 55, 71, 22, 71, 46, 71, 54, 71, 39, 71, 29, 71, 15, 71, 38, 71, 47, 71, 35, 71, 21, 71, 51, 71, 28, 71, 27, 71, 45, 71, 43, 71, 31, 71, 52, 71, 13, 71, 17, 71, 53, 71, 42, 71, 14, 71, 37, 71, 62, 71, 44, 71, 16, 71, 12, 71, 61, 71, 59, 71, 18, 71, 32, 71, 11, 71, 36, 71, 20, 71, 2, 71, 58, 71, 30, 71, 19, 71, 49, 71, 57, 71, 26, 71, 10, 71, 33, 71, 34, 71, 60, 71, 41, 71, 40, 71, 48, 71, 56, 71, 63, 71, 64, 71, 65, 71, 66, 71, 67, 71, 68, 71, 69, 71],
	#[65535, 123, 2, 123, 10, 123, 11, 123, 12, 123, 13, 123, 14, 123, 15, 123, 16, 123, 17, 123, 18, 123, 19, 123, 20, 123, 21, 123, 22, 123, 23, 123, 26, 123, 27, 123, 28, 123, 29, 123, 30, 123, 31, 123, 32, 123, 33, 123, 34, 123, 35, 123, 36, 123, 37, 123, 38, 123, 39, 123, 40, 123, 41, 123, 42, 123, 43, 123, 44, 123, 45, 123, 46, 123, 47, 123, 48, 123, 49, 123, 50, 123, 51, 123, 52, 123, 53, 123, 54, 123, 55, 123, 56, 123, 57, 123, 58, 123, 59, 123, 60, 123, 61, 123, 62, 123, 63, 123, 64, 123, 65, 123, 66, 123, 67, 123, 68, 123, 69, 123],
	#[65535, 139, 23, 139, 45, 139, 28, 139, 22, 139, 46, 139, 52, 139, 29, 139, 27, 139, 30, 139, 42, 139, 12, 139, 14, 139, 26, 139, 49, 139, 43, 139, 17, 139, 13, 139, 15, 139, 53, 139, 44, 139, 47, 139, 50, 139, 18, 139, 55, 139, 32, 139, 51, 139, 16, 139, 19, 139, 48, 139, 54, 139, 57, 139, 2, 139, 10, 139, 11, 139, 34, 139, 20, 139, 21, 139, 41, 139, 31, 139, 33, 139, 35, 139, 36, 139, 37, 139, 38, 139, 39, 139, 40, 139, 56, 139, 58, 139, 59, 139, 60, 139, 61, 139, 62, 139, 63, 139, 64, 139, 65, 139, 66, 139, 67, 139, 68, 139, 69, 139],
	#[65535, 129, 23, 129, 45, 129, 47, 129, 27, 129, 30, 129, 55, 129, 18, 129, 28, 129, 12, 129, 14, 129, 26, 129, 52, 129, 50, 129, 43, 129, 22, 129, 46, 129, 13, 129, 15, 129, 53, 129, 44, 129, 51, 129, 37, 129, 54, 129, 48, 129, 42, 129, 61, 129, 19, 129, 36, 129, 38, 129, 35, 129, 41, 129, 29, 129, 31, 129, 60, 129, 16, 129, 49, 129, 2, 129, 39, 129, 32, 129, 21, 129, 33, 129, 20, 129, 57, 129, 17, 129, 34, 129, 56, 129, 58, 129, 62, 129, 59, 129, 40, 129, 11, 129, 10, 129, 63, 129, 64, 129, 65, 129, 66, 129, 67, 129, 68, 129, 69, 129],
	#[65535, 135, 45, 135, 22, 135, 47, 135, 23, 135, 55, 135, 28, 135, 14, 135, 27, 135, 52, 135, 50, 135, 43, 135, 29, 135, 59, 135, 13, 135, 15, 135, 44, 135, 51, 135, 30, 135, 54, 135, 48, 135, 42, 135, 57, 135, 19, 135, 36, 135, 38, 135, 26, 135, 49, 135, 31, 135, 18, 135, 60, 135, 16, 135, 46, 135, 12, 135, 37, 135, 39, 135, 32, 135, 21, 135, 61, 135, 20, 135, 10, 135, 17, 135, 33, 135, 62, 135, 35, 135, 11, 135, 40, 135, 34, 135, 2, 135, 41, 135, 58, 135, 56, 135, 53, 135, 63, 135, 64, 135, 65, 135, 66, 135, 67, 135, 68, 135, 69, 135],
	#[65535, 146, 15, 146, 23, 146, 46, 146, 28, 146, 45, 146, 54, 146, 44, 146, 43, 146, 61, 146, 22, 146, 47, 146, 29, 146, 27, 146, 13, 146, 30, 146, 55, 146, 50, 146, 37, 146, 42, 146, 12, 146, 14, 146, 38, 146, 62, 146, 26, 146, 52, 146, 36, 146, 39, 146, 48, 146, 17, 146, 59, 146, 16, 146, 34, 146, 53, 146, 57, 146, 51, 146, 32, 146, 33, 146, 49, 146, 19, 146, 20, 146, 58, 146, 11, 146, 31, 146, 18, 146, 60, 146, 10, 146, 41, 146, 2, 146, 21, 146, 35, 146, 40, 146, 56, 146, 63, 146, 64, 146, 65, 146, 66, 146, 67, 146, 68, 146, 69, 146],
	#[65535, 131, 45, 131, 43, 131, 22, 131, 47, 131, 23, 131, 30, 131, 55, 131, 50, 131, 28, 131, 12, 131, 14, 131, 38, 131, 27, 131, 52, 131, 36, 131, 20, 131, 29, 131, 46, 131, 13, 131, 15, 131, 44, 131, 51, 131, 37, 131, 54, 131, 48, 131, 42, 131, 19, 131, 34, 131, 2, 131, 35, 131, 26, 131, 49, 131, 31, 131, 18, 131, 60, 131, 16, 131, 53, 131, 41, 131, 39, 131, 32, 131, 21, 131, 33, 131, 11, 131, 57, 131, 10, 131, 17, 131, 58, 131, 62, 131, 59, 131, 40, 131, 56, 131, 61, 131, 63, 131, 64, 131, 65, 131, 66, 131, 67, 131, 68, 131, 69, 131],
	#[23, -50, 20, -5, 31, 92, 44, -222, 10, -238, 12, -157, 2, 92, 18, -34, 15, -142, 14, -118, 38, -199, 19, -58, 13, -108, 22, -32, 34, -51, 21, -16, 26, -30, 16, -137, 17, -107, 27, -46, 28, -63, 29, -13, 30, -28, 42, -236, 32, -237, 33, -29, 35, -229, 36, -239, 37, -208, 39, -205, 40, -215, 41, -218, 43, -211, 45, -214, 46, -224, 47, -233, 48, -201, 49, -212, 50, -223, 51, -230, 52, -198, 53, -210, 54, -221, 55, -207, 56, 92, 57, -232, 58, 92, 59, -219, 60, 92, 61, -203, 62, -213, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[31, 111, 10, -245, 2, 111, 60, 111, 58, 111, 56, 111],
	#[65535, 124, 47, 124, 23, 124, 52, 124, 22, 124, 43, 124, 30, 124, 29, 124, 27, 124, 15, 124, 38, 124, 19, 124, 37, 124, 35, 124, 50, 124, 46, 124, 28, 124, 42, 124, 45, 124, 48, 124, 31, 124, 54, 124, 20, 124, 18, 124, 53, 124, 16, 124, 14, 124, 39, 124, 62, 124, 44, 124, 21, 124, 12, 124, 61, 124, 59, 124, 49, 124, 32, 124, 10, 124, 36, 124, 34, 124, 41, 124, 13, 124, 57, 124, 55, 124, 17, 124, 60, 124, 11, 124, 2, 124, 58, 124, 26, 124, 33, 124, 51, 124, 40, 124, 56, 124, 63, 124, 64, 124, 65, 124, 66, 124, 67, 124, 68, 124, 69, 124],
	#[65535, 148, 23, 148, 45, 148, 22, 148, 47, 148, 52, 148, 27, 148, 30, 148, 55, 148, 50, 148, 28, 148, 14, 148, 61, 148, 26, 148, 36, 148, 43, 148, 29, 148, 46, 148, 13, 148, 15, 148, 44, 148, 51, 148, 54, 148, 48, 148, 42, 148, 19, 148, 34, 148, 38, 148, 35, 148, 49, 148, 31, 148, 18, 148, 60, 148, 16, 148, 53, 148, 12, 148, 37, 148, 39, 148, 32, 148, 21, 148, 41, 148, 20, 148, 57, 148, 17, 148, 40, 148, 33, 148, 62, 148, 59, 148, 11, 148, 10, 148, 56, 148, 2, 148, 58, 148, 63, 148, 64, 148, 65, 148, 66, 148, 67, 148, 68, 148, 69, 148],
	#[47, -233, 55, -207, 54, -221, 53, -210, 38, -199, 62, -213, 61, -203, 46, -224, 52, -198, 36, -239, 30, -28, 43, -211, 60, 92, 32, -237, 35, -229, 57, -232, 34, -51, 51, -230, 45, -214, 39, -205, 22, -32, 42, -236, 59, -219, 15, -142, 49, -212, 44, -222, 28, -63, 37, -208, 50, -223, 21, -16, 23, -50, 10, -238, 19, -58, 14, -118, 20, -5, 33, -29, 41, -218, 29, -13, 48, -201, 18, -34, 27, -46, 16, -137, 12, -157, 17, -107, 26, -30, 13, -108, 40, -215, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[47, -233, 55, -207, 31, 92, 54, -221, 53, -210, 38, -199, 62, -213, 61, -203, 46, -224, 52, -198, 36, -239, 2, 92, 30, -28, 43, -211, 50, -223, 60, 92, 32, -237, 41, -218, 35, -229, 57, -232, 34, -51, 51, -230, 58, 92, 45, -214, 39, -205, 22, -32, 42, -236, 59, -219, 56, 92, 15, -142, 49, -212, 44, -222, 28, -63, 37, -208, 27, -46, 33, -29, 21, -16, 23, -50, 10, -238, 19, -58, 14, -118, 20, -5, 26, -30, 29, -13, 48, -201, 18, -34, 17, -107, 16, -137, 12, -157, 13, -108, 40, -215, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 140, 14, 140, 29, 140, 23, 140, 52, 140, 22, 140, 18, 140, 43, 140, 30, 140, 50, 140, 27, 140, 15, 140, 38, 140, 47, 140, 26, 140, 21, 140, 46, 140, 28, 140, 42, 140, 45, 140, 48, 140, 19, 140, 54, 140, 57, 140, 13, 140, 17, 140, 53, 140, 16, 140, 10, 140, 39, 140, 62, 140, 44, 140, 58, 140, 12, 140, 61, 140, 59, 140, 49, 140, 35, 140, 32, 140, 11, 140, 51, 140, 34, 140, 2, 140, 55, 140, 40, 140, 60, 140, 37, 140, 41, 140, 31, 140, 36, 140, 20, 140, 56, 140, 33, 140, 63, 140, 64, 140, 65, 140, 66, 140, 67, 140, 68, 140, 69, 140],
	#[65535, 130, 52, 130, 22, 130, 43, 130, 30, 130, 29, 130, 27, 130, 15, 130, 38, 130, 47, 130, 37, 130, 26, 130, 23, 130, 46, 130, 28, 130, 42, 130, 45, 130, 48, 130, 19, 130, 54, 130, 20, 130, 18, 130, 53, 130, 51, 130, 14, 130, 39, 130, 62, 130, 44, 130, 16, 130, 12, 130, 61, 130, 50, 130, 49, 130, 35, 130, 32, 130, 10, 130, 36, 130, 34, 130, 41, 130, 13, 130, 57, 130, 55, 130, 17, 130, 60, 130, 31, 130, 21, 130, 11, 130, 2, 130, 58, 130, 33, 130, 56, 130, 40, 130, 59, 130, 63, 130, 64, 130, 65, 130, 66, 130, 67, 130, 68, 130, 69, 130],
	#[65535, 136, 23, 136, 46, 136, 45, 136, 47, 136, 27, 136, 30, 136, 55, 136, 18, 136, 28, 136, 14, 136, 38, 136, 26, 136, 52, 136, 50, 136, 43, 136, 22, 136, 19, 136, 15, 136, 53, 136, 44, 136, 51, 136, 12, 136, 54, 136, 48, 136, 21, 136, 42, 136, 61, 136, 36, 136, 20, 136, 35, 136, 41, 136, 29, 136, 31, 136, 13, 136, 16, 136, 49, 136, 37, 136, 39, 136, 32, 136, 33, 136, 57, 136, 17, 136, 34, 136, 56, 136, 60, 136, 62, 136, 59, 136, 40, 136, 11, 136, 10, 136, 2, 136, 58, 136, 63, 136, 64, 136, 65, 136, 66, 136, 67, 136, 68, 136, 69, 136],
	#[65535, 132, 47, 132, 23, 132, 55, 132, 28, 132, 14, 132, 27, 132, 45, 132, 50, 132, 22, 132, 46, 132, 13, 132, 15, 132, 44, 132, 51, 132, 30, 132, 54, 132, 42, 132, 19, 132, 36, 132, 38, 132, 26, 132, 29, 132, 31, 132, 18, 132, 60, 132, 16, 132, 53, 132, 43, 132, 49, 132, 37, 132, 39, 132, 32, 132, 21, 132, 52, 132, 41, 132, 20, 132, 57, 132, 48, 132, 17, 132, 34, 132, 33, 132, 58, 132, 62, 132, 59, 132, 12, 132, 35, 132, 11, 132, 10, 132, 40, 132, 56, 132, 2, 132, 61, 132, 63, 132, 64, 132, 65, 132, 66, 132, 67, 132, 68, 132, 69, 132],
	#[65535, 147, 47, 147, 23, 147, 55, 147, 28, 147, 14, 147, 27, 147, 45, 147, 50, 147, 43, 147, 22, 147, 46, 147, 13, 147, 15, 147, 44, 147, 51, 147, 30, 147, 54, 147, 48, 147, 42, 147, 61, 147, 19, 147, 36, 147, 38, 147, 35, 147, 26, 147, 29, 147, 31, 147, 18, 147, 60, 147, 16, 147, 53, 147, 12, 147, 49, 147, 37, 147, 39, 147, 32, 147, 21, 147, 52, 147, 41, 147, 20, 147, 57, 147, 17, 147, 34, 147, 33, 147, 58, 147, 62, 147, 59, 147, 11, 147, 10, 147, 40, 147, 56, 147, 2, 147, 63, 147, 64, 147, 65, 147, 66, 147, 67, 147, 68, 147, 69, 147],
	#[65535, 126, 23, 126, 45, 126, 43, 126, 22, 126, 47, 126, 52, 126, 27, 126, 30, 126, 55, 126, 50, 126, 44, 126, 28, 126, 14, 126, 38, 126, 26, 126, 36, 126, 39, 126, 29, 126, 46, 126, 13, 126, 15, 126, 57, 126, 51, 126, 54, 126, 48, 126, 42, 126, 61, 126, 19, 126, 34, 126, 20, 126, 35, 126, 49, 126, 31, 126, 18, 126, 60, 126, 16, 126, 53, 126, 12, 126, 37, 126, 58, 126, 32, 126, 21, 126, 41, 126, 17, 126, 40, 126, 33, 126, 62, 126, 59, 126, 11, 126, 10, 126, 56, 126, 2, 126, 63, 126, 64, 126, 65, 126, 66, 126, 67, 126, 68, 126, 69, 126],
	#[31, 104, 53, -210, 30, -28, 44, -222, 51, -230, 34, -51, 2, 104, 26, -30, 41, -218, 32, -237, 10, -238, 12, -157, 13, -108, 14, -118, 15, -142, 16, -137, 17, -107, 19, -58, 20, -5, 21, -16, 27, -46, 49, -212, 43, -211, 42, -236, 33, -29, 35, -229, 36, -239, 37, -208, 38, -199, 39, -205, 40, -215, 45, -214, 46, -224, 47, -233, 48, -201, 50, -223, 52, -198, 54, -221, 55, -207, 56, 104, 57, -232, 58, 104, 59, -219, 60, 104, 61, -203, 62, -213, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55],
	#[65535, 125, 23, 125, 46, 125, 45, 125, 47, 125, 27, 125, 30, 125, 55, 125, 18, 125, 28, 125, 14, 125, 38, 125, 26, 125, 52, 125, 50, 125, 43, 125, 22, 125, 19, 125, 15, 125, 44, 125, 51, 125, 12, 125, 54, 125, 48, 125, 21, 125, 42, 125, 61, 125, 36, 125, 20, 125, 35, 125, 41, 125, 29, 125, 31, 125, 13, 125, 16, 125, 37, 125, 39, 125, 32, 125, 33, 125, 57, 125, 17, 125, 34, 125, 56, 125, 60, 125, 62, 125, 59, 125, 40, 125, 11, 125, 10, 125, 2, 125, 58, 125, 49, 125, 53, 125, 63, 125, 64, 125, 65, 125, 66, 125, 67, 125, 68, 125, 69, 125],
	#[65535, 70, 14, 70, 52, 70, 22, 70, 18, 70, 43, 70, 30, 70, 45, 70, 29, 70, 27, 70, 15, 70, 38, 70, 47, 70, 26, 70, 23, 70, 46, 70, 28, 70, 42, 70, 32, 70, 48, 70, 19, 70, 54, 70, 57, 70, 50, 70, 17, 70, 53, 70, 16, 70, 10, 70, 39, 70, 62, 70, 44, 70, 21, 70, 12, 70, 61, 70, 59, 70, 49, 70, 35, 70, 11, 70, 51, 70, 34, 70, 2, 70, 13, 70, 55, 70, 40, 70, 60, 70, 37, 70, 41, 70, 31, 70, 58, 70, 36, 70, 20, 70, 56, 70, 33, 70, 63, 70, 64, 70, 65, 70, 66, 70, 67, 70, 68, 70, 69, 70],
	#[65535, 137, 23, 137, 28, 137, 45, 137, 54, 137, 39, 137, 43, 137, 15, 137, 22, 137, 47, 137, 29, 137, 27, 137, 30, 137, 55, 137, 50, 137, 42, 137, 49, 137, 14, 137, 38, 137, 26, 137, 52, 137, 20, 137, 48, 137, 46, 137, 13, 137, 35, 137, 53, 137, 44, 137, 51, 137, 37, 137, 32, 137, 21, 137, 40, 137, 61, 137, 19, 137, 36, 137, 2, 137, 58, 137, 16, 137, 11, 137, 31, 137, 18, 137, 60, 137, 56, 137, 10, 137, 12, 137, 34, 137, 17, 137, 33, 137, 57, 137, 62, 137, 59, 137, 41, 137, 63, 137, 64, 137, 65, 137, 66, 137, 67, 137, 68, 137, 69, 137],
	#[65535, 93, 2, 93, 31, 93, 56, 93, 58, 93, 60, 93],
	#[55, -207, 54, -221, 62, -213, 61, -203, 53, -210, 59, -219, 57, -232, 38, -199, 51, -230, 58, 92, 45, -214, 43, -211, 46, -224, 10, -238, 49, -212, 44, -222, 40, -215, 30, -28, 50, -223, 41, -218, 23, -50, 52, -198, 36, -239, 20, -5, 35, -229, 29, -13, 48, -201, 27, -46, 22, -32, 28, -63, 12, -157, 37, -208, 39, -205, 32, -237, 21, -16, 18, -34, 15, -142, 42, -236, 17, -107, 34, -51, 14, -118, 19, -58, 13, -108, 33, -29, 16, -137, 26, -30, 47, -233, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 133, 45, 133, 47, 133, 23, 133, 28, 133, 14, 133, 27, 133, 52, 133, 50, 133, 43, 133, 22, 133, 46, 133, 13, 133, 15, 133, 53, 133, 44, 133, 51, 133, 30, 133, 54, 133, 48, 133, 42, 133, 19, 133, 36, 133, 38, 133, 35, 133, 26, 133, 29, 133, 31, 133, 18, 133, 60, 133, 16, 133, 32, 133, 41, 133, 49, 133, 37, 133, 39, 133, 17, 133, 21, 133, 33, 133, 11, 133, 20, 133, 57, 133, 34, 133, 58, 133, 62, 133, 59, 133, 12, 133, 56, 133, 55, 133, 10, 133, 40, 133, 2, 133, 61, 133, 63, 133, 64, 133, 65, 133, 66, 133, 67, 133, 68, 133, 69, 133],
	#[2, 111, 58, 111, 31, 111, 60, 111, 56, 111, 10, -245],
	#[65535, 149, 23, 149, 28, 149, 45, 149, 54, 149, 53, 149, 14, 149, 44, 149, 43, 149, 12, 149, 15, 149, 22, 149, 47, 149, 52, 149, 29, 149, 27, 149, 30, 149, 55, 149, 50, 149, 18, 149, 37, 149, 42, 149, 35, 149, 57, 149, 38, 149, 61, 149, 26, 149, 36, 149, 39, 149, 48, 149, 46, 149, 59, 149, 13, 149, 34, 149, 58, 149, 51, 149, 32, 149, 33, 149, 41, 149, 49, 149, 19, 149, 20, 149, 11, 149, 31, 149, 60, 149, 16, 149, 10, 149, 40, 149, 17, 149, 21, 149, 2, 149, 62, 149, 56, 149, 63, 149, 64, 149, 65, 149, 66, 149, 67, 149, 68, 149, 69, 149],
	#[65535, 128, 47, 128, 23, 128, 28, 128, 14, 128, 27, 128, 45, 128, 50, 128, 43, 128, 22, 128, 46, 128, 15, 128, 44, 128, 51, 128, 30, 128, 54, 128, 42, 128, 19, 128, 36, 128, 38, 128, 35, 128, 26, 128, 29, 128, 31, 128, 18, 128, 13, 128, 16, 128, 53, 128, 12, 128, 49, 128, 37, 128, 39, 128, 32, 128, 21, 128, 52, 128, 41, 128, 20, 128, 57, 128, 48, 128, 17, 128, 34, 128, 33, 128, 60, 128, 62, 128, 59, 128, 40, 128, 56, 128, 55, 128, 11, 128, 10, 128, 2, 128, 61, 128, 58, 128, 63, 128, 64, 128, 65, 128, 66, 128, 67, 128, 68, 128, 69, 128],
	#[65535, 141, 2, 141, 18, 141, 42, 141, 10, 141, 11, 141, 12, 141, 13, 141, 14, 141, 15, 141, 16, 141, 17, 141, 37, 141, 19, 141, 20, 141, 21, 141, 22, 141, 23, 141, 26, 141, 27, 141, 28, 141, 29, 141, 30, 141, 31, 141, 32, 141, 33, 141, 34, 141, 35, 141, 36, 141, 38, 141, 39, 141, 40, 141, 41, 141, 43, 141, 44, 141, 45, 141, 46, 141, 47, 141, 48, 141, 49, 141, 50, 141, 51, 141, 52, 141, 53, 141, 54, 141, 55, 141, 56, 141, 57, 141, 58, 141, 59, 141, 60, 141, 61, 141, 62, 141, 63, 141, 64, 141, 65, 141, 66, 141, 67, 141, 68, 141, 69, 141],
	#[55, -207, 54, -221, 61, -203, 2, 92, 53, -210, 60, 92, 31, 92, 38, -199, 51, -230, 58, 92, 45, -214, 43, -211, 46, -224, 59, -219, 56, 92, 49, -212, 57, -232, 47, -233, 30, -28, 50, -223, 23, -50, 10, -238, 52, -198, 36, -239, 20, -5, 62, -213, 41, -218, 29, -13, 48, -201, 40, -215, 44, -222, 28, -63, 12, -157, 37, -208, 39, -205, 32, -237, 35, -229, 18, -34, 15, -142, 42, -236, 27, -46, 17, -107, 34, -51, 14, -118, 19, -58, 13, -108, 33, -29, 22, -32, 16, -137, 21, -16, 26, -30, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 72, 47, 72, 23, 72, 28, 72, 14, 72, 27, 72, 45, 72, 50, 72, 43, 72, 22, 72, 46, 72, 15, 72, 44, 72, 51, 72, 30, 72, 54, 72, 42, 72, 19, 72, 36, 72, 38, 72, 35, 72, 26, 72, 29, 72, 31, 72, 18, 72, 13, 72, 16, 72, 53, 72, 12, 72, 49, 72, 37, 72, 39, 72, 32, 72, 21, 72, 52, 72, 41, 72, 20, 72, 57, 72, 48, 72, 17, 72, 34, 72, 33, 72, 60, 72, 62, 72, 59, 72, 40, 72, 56, 72, 55, 72, 11, 72, 10, 72, 2, 72, 61, 72, 58, 72, 63, 72, 64, 72, 65, 72, 66, 72, 67, 72, 68, 72, 69, 72],
	#[56, -241],
	#[65535, 180, 60, 180, 31, 180, 62, 180, 61, 180, 2, 180, 19, 180, 12, 180, 21, 180, 51, 180, 45, 180, 20, 180, 17, 180, 59, 180, 56, 180, 15, 180, 53, 180, 58, 180, 28, 180, 30, 180, 54, 180, 33, 180, 41, 180, 40, 180, 52, 180, 14, 180, 38, 180, 35, 180, 16, 180, 50, 180, 13, 180, 22, 180, 46, 180, 43, 180, 37, 180, 39, 180, 32, 180, 48, 180, 36, 180, 49, 180, 42, 180, 27, 180, 47, 180, 34, 180, 29, 180, 44, 180, 23, 180, 18, 180, 55, 180, 11, 180, 10, 180, 57, 180, 26, 180, 63, 180, 64, 180, 65, 180, 66, 180, 67, 180, 68, 180, 69, 180],
	#[65535, 110, 2, 110, 60, 110, 56, 110, 31, 110, 58, 110],
	#[65535, 112, 2, 112, 31, 112, 56, 112, 58, 112, 60, 112],
	#[65535, 107, 60, 107, 58, 107, 2, 107, 56, 107, 31, 107],
	#[2, 92, 30, -28, 31, 92, 61, -203, 45, -214, 43, -211, 46, -224, 59, -219, 56, 92, 53, -210, 51, -230, 37, -208, 54, -221, 23, -50, 57, -232, 52, -198, 55, -207, 20, -5, 58, 92, 41, -218, 29, -13, 48, -201, 60, 92, 44, -222, 10, -238, 12, -157, 49, -212, 19, -58, 39, -205, 35, -229, 50, -223, 36, -239, 42, -236, 27, -46, 47, -233, 14, -118, 38, -199, 26, -30, 13, -108, 17, -107, 32, -237, 33, -29, 22, -32, 40, -215, 34, -51, 21, -16, 28, -63, 15, -142, 16, -137, 18, -34, 62, -213, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 113, 2, 113, 31, 113, 56, 113, 58, 113, 60, 113],
	#[58, -248],
	#[65535, 143, 47, 143, 23, 143, 28, 143, 14, 143, 38, 143, 27, 143, 45, 143, 50, 143, 22, 143, 46, 143, 15, 143, 44, 143, 51, 143, 30, 143, 54, 143, 42, 143, 19, 143, 36, 143, 20, 143, 26, 143, 29, 143, 31, 143, 18, 143, 13, 143, 16, 143, 53, 143, 43, 143, 49, 143, 37, 143, 39, 143, 32, 143, 21, 143, 52, 143, 41, 143, 57, 143, 48, 143, 17, 143, 34, 143, 33, 143, 60, 143, 62, 143, 59, 143, 12, 143, 35, 143, 55, 143, 11, 143, 10, 143, 40, 143, 56, 143, 2, 143, 61, 143, 58, 143, 63, 143, 64, 143, 65, 143, 66, 143, 67, 143, 68, 143, 69, 143],
	#[65535, 105, 2, 105, 31, 105, 56, 105, 58, 105, 60, 105],
	#[65535, 95, 60, 95, 2, 95, 58, 95, 31, 95, 56, 95],
	#[65535, 98, 2, 98, 10, 98, 11, 98, 31, 98, 56, 98, 58, 98, 60, 98],
	#[65535, 97, 31, 97, 60, 97, 10, 97, 2, 97, 58, 97, 56, 97, 11, 97],
	#[2, 96, 23, -50, 31, 96, 10, 96, 11, 96, 12, -157, 13, -108, 14, -118, 15, -142, 16, -137, 17, -107, 18, -34, 19, -58, 20, -5, 21, -16, 22, -32, 34, -51, 44, -222, 28, -63, 29, -13, 42, -236, 32, -237, 33, -29, 35, -229, 36, -239, 37, -208, 38, -199, 39, -205, 40, -254, 41, -218, 43, -211, 45, -214, 46, -224, 47, -233, 48, -201, 49, -212, 50, -223, 51, -230, 52, -198, 53, -210, 54, -221, 55, -207, 56, 96, 57, -232, 58, 96, 59, -219, 60, 96, 61, -203, 62, -213, 63, -225, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[23, -50, 11, 96, 22, -32, 46, -224, 31, 96, 54, -221, 39, -205, 32, -237, 38, -199, 41, -218, 47, -233, 48, -201, 21, -16, 44, -222, 28, -63, 55, -207, 10, 96, 45, -214, 43, -211, 2, 96, 19, -58, 52, -198, 36, -239, 35, -229, 18, -34, 53, -210, 51, -230, 14, -118, 37, -208, 34, -51, 40, -254, 17, -107, 12, -157, 15, -142, 50, -223, 42, -236, 29, -13, 20, -5, 16, -137, 49, -212, 33, -29, 13, -108, 56, 96, 57, -232, 58, 96, 59, -219, 60, 96, 61, -203, 62, -213, 63, -225, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 150, 58, 150, 60, 150, 11, 150, 10, 150, 2, 150, 56, 150, 31, 150],
	#[55, -207, 31, 114, 62, -213, 46, -224, 11, 114, 2, 114, 43, -211, 60, 114, 10, 114, 38, -199, 61, -203, 58, 114, 39, -205, 42, -236, 59, -219, 56, 114, 15, -142, 53, -210, 44, -222, 51, -230, 12, -157, 54, -221, 40, -254, 57, -232, 52, -198, 34, -51, 20, -5, 35, -229, 41, -218, 49, -212, 48, -201, 13, -108, 16, -137, 37, -208, 17, -107, 21, -16, 50, -223, 36, -239, 45, -214, 14, -118, 19, -58, 32, -237, 33, -29, 47, -233, 63, -225, 65, -33, 66, -48, 67, -37, 68, -55],
	#[65535, 115, 31, 115, 60, 115, 11, 115, 10, 115, 2, 115, 58, 115, 56, 115],
	#[65535, 99, 10, 99, 2, 99, 11, 99, 31, 99, 56, 99, 58, 99, 60, 99],
	#[60, -260],
	#[65535, 144, 23, 144, 46, 144, 28, 144, 45, 144, 43, 144, 15, 144, 22, 144, 47, 144, 27, 144, 30, 144, 55, 144, 50, 144, 42, 144, 12, 144, 14, 144, 51, 144, 26, 144, 52, 144, 36, 144, 39, 144, 29, 144, 19, 144, 35, 144, 53, 144, 44, 144, 37, 144, 54, 144, 48, 144, 49, 144, 16, 144, 34, 144, 38, 144, 11, 144, 31, 144, 18, 144, 13, 144, 33, 144, 40, 144, 17, 144, 21, 144, 20, 144, 32, 144, 10, 144, 2, 144, 41, 144, 56, 144, 57, 144, 58, 144, 59, 144, 60, 144, 61, 144, 62, 144, 63, 144, 64, 144, 65, 144, 66, 144, 67, 144, 68, 144, 69, 144],
	#[65535, 281, 47, 281, 23, 281, 46, 281, 28, 281, 27, 281, 45, 281, 29, 281, 54, 281, 39, 281, 43, 281, 12, 281, 15, 281, 22, 281, 35, 281, 51, 281, 30, 281, 55, 281, 18, 281, 42, 281, 31, 281, 14, 281, 26, 281, 52, 281, 48, 281, 19, 281, 13, 281, 41, 281, 53, 281, 44, 281, 37, 281, 33, 281, 21, 281, 36, 281, 38, 281, 49, 281, 17, 281, 16, 281, 10, 281, 34, 281, 32, 281, 20, 281, 40, 281, 50, 281, 57, 281, 59, 281, 61, 281, 62, 281, 63, 281, 64, 281, 65, 281, 66, 281, 67, 281, 68, 281, 69, 281],
	#[65535, 287, 52, 287, 22, 287, 46, 287, 18, 287, 43, 287, 42, 287, 28, 287, 26, 287, 29, 287, 27, 287, 15, 287, 38, 287, 47, 287, 37, 287, 35, 287, 23, 287, 51, 287, 49, 287, 55, 287, 45, 287, 48, 287, 30, 287, 54, 287, 20, 287, 17, 287, 53, 287, 16, 287, 14, 287, 39, 287, 34, 287, 44, 287, 21, 287, 12, 287, 19, 287, 50, 287, 32, 287, 10, 287, 36, 287, 40, 287, 41, 287, 13, 287, 31, 287, 33, 287, 57, 287, 59, 287, 61, 287, 62, 287, 63, 287, 64, 287, 65, 287, 66, 287, 67, 287, 68, 287, 69, 287],
	#[65535, 282, 23, 282, 22, 282, 30, 282, 54, 282, 47, 282, 45, 282, 28, 282, 14, 282, 29, 282, 62, 282, 13, 282, 55, 282, 52, 282, 36, 282, 20, 282, 46, 282, 18, 282, 43, 282, 42, 282, 35, 282, 12, 282, 26, 282, 49, 282, 50, 282, 27, 282, 15, 282, 38, 282, 32, 282, 48, 282, 37, 282, 16, 282, 21, 282, 51, 282, 61, 282, 57, 282, 10, 282, 17, 282, 34, 282, 33, 282, 31, 282, 59, 282, 53, 282, 39, 282, 44, 282, 41, 282, 40, 282, 19, 282, 63, 282, 64, 282, 65, 282, 66, 282, 67, 282, 68, 282, 69, 282],
	#[65535, 289, 22, 289, 30, 289, 29, 289, 27, 289, 42, 289, 38, 289, 47, 289, 26, 289, 23, 289, 46, 289, 28, 289, 55, 289, 45, 289, 31, 289, 54, 289, 18, 289, 51, 289, 14, 289, 39, 289, 44, 289, 43, 289, 12, 289, 15, 289, 50, 289, 49, 289, 35, 289, 32, 289, 10, 289, 36, 289, 13, 289, 48, 289, 16, 289, 33, 289, 21, 289, 34, 289, 17, 289, 52, 289, 20, 289, 53, 289, 40, 289, 37, 289, 19, 289, 41, 289, 57, 289, 59, 289, 61, 289, 62, 289, 63, 289, 64, 289, 65, 289, 66, 289, 67, 289, 68, 289, 69, 289],
	#[39, -96, 47, -153, 46, -135, 61, -105, 45, -119, 30, -125, 37, -97, 62, -117, 44, -122, 43, -130, 12, -157, 15, -142, 59, -134, 35, -146, 42, -101, 10, -159, 56, 296, 55, -112, 40, -121, 19, -152, 32, -158, 28, -161, 14, -118, 38, -98, 17, -107, 27, -143, 41, -124, 36, -94, 20, -106, 22, -128, 16, -137, 48, -103, 24, -102, 26, -127, 25, -113, 33, -104, 21, -115, 23, -150, 29, -111, 31, -141, 18, -131, 13, -108, 49, 339, 50, -132, 51, -148, 52, -100, 53, -114, 57, -151, 63, -136, 64, -120, 65, -129, 66, -145, 67, -133, 68, -149, 69, -147],
	#[65535, 294, 30, 294, 47, 294, 45, 294, 28, 294, 29, 294, 23, 294, 13, 294, 52, 294, 22, 294, 46, 294, 18, 294, 43, 294, 42, 294, 35, 294, 54, 294, 26, 294, 49, 294, 50, 294, 27, 294, 15, 294, 38, 294, 62, 294, 34, 294, 48, 294, 37, 294, 32, 294, 51, 294, 12, 294, 17, 294, 31, 294, 59, 294, 57, 294, 20, 294, 53, 294, 16, 294, 14, 294, 39, 294, 44, 294, 21, 294, 61, 294, 40, 294, 10, 294, 36, 294, 41, 294, 55, 294, 19, 294, 33, 294, 63, 294, 64, 294, 65, 294, 66, 294, 67, 294, 68, 294, 69, 294],
	#[65535, 283, 23, 283, 47, 283, 45, 283, 29, 283, 62, 283, 52, 283, 22, 283, 46, 283, 42, 283, 28, 283, 54, 283, 26, 283, 50, 283, 27, 283, 15, 283, 38, 283, 49, 283, 37, 283, 35, 283, 33, 283, 51, 283, 61, 283, 55, 283, 16, 283, 43, 283, 30, 283, 59, 283, 57, 283, 20, 283, 18, 283, 53, 283, 14, 283, 39, 283, 34, 283, 44, 283, 17, 283, 12, 283, 32, 283, 10, 283, 36, 283, 41, 283, 13, 283, 19, 283, 31, 283, 21, 283, 48, 283, 40, 283, 63, 283, 64, 283, 65, 283, 66, 283, 67, 283, 68, 283, 69, 283],
	#[65535, 291, 23, 291, 52, 291, 22, 291, 46, 291, 42, 291, 28, 291, 29, 291, 27, 291, 15, 291, 38, 291, 47, 291, 37, 291, 50, 291, 51, 291, 49, 291, 55, 291, 45, 291, 43, 291, 30, 291, 54, 291, 57, 291, 35, 291, 18, 291, 53, 291, 16, 291, 14, 291, 39, 291, 62, 291, 44, 291, 21, 291, 12, 291, 61, 291, 59, 291, 32, 291, 10, 291, 36, 291, 20, 291, 41, 291, 13, 291, 17, 291, 19, 291, 31, 291, 40, 291, 34, 291, 48, 291, 26, 291, 33, 291, 63, 291, 64, 291, 65, 291, 66, 291, 67, 291, 68, 291, 69, 291],
	#[31, 252, 42, -281, 43, -264, 45, -268, 29, -13, 57, -279, 53, -210, 54, -221, 52, -198, 36, -239, 55, -265, 35, -229, 38, -199, 62, -213, 10, -282, 44, -273, 28, -63, 41, -218, 40, -269, 22, -32, 46, -224, 19, -58, 59, -271, 39, -205, 32, -237, 48, -201, 30, -28, 15, -142, 27, -46, 33, -29, 47, -233, 50, -223, 18, -34, 14, -118, 23, -50, 34, -51, 49, -212, 51, -230, 26, -30, 20, -5, 61, -203, 16, -137, 12, -157, 13, -108, 37, -208, 21, -16, 17, -107, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[31, 271, 10, -289],
	#[52, -100, 62, -117, 49, 339, 53, -114, 46, -135, 31, -141, 43, -130, 42, -101, 10, -159, 60, 296, 30, -125, 39, -96, 51, -148, 50, -132, 15, -142, 38, -98, 20, -106, 47, -153, 57, -151, 37, -97, 35, -146, 22, -128, 41, -124, 61, -105, 55, -112, 45, -119, 48, -103, 19, -152, 59, -134, 36, -94, 13, -108, 24, -102, 40, -121, 33, -104, 14, -118, 27, -143, 44, -122, 21, -115, 18, -131, 17, -107, 23, -150, 26, -127, 16, -137, 32, -158, 28, -161, 12, -157, 25, -113, 29, -111, 63, -136, 64, -120, 65, -129, 66, -145, 67, -133, 68, -149, 69, -147],
	#[65535, 284, 39, 284, 22, 284, 47, 284, 23, 284, 43, 284, 28, 284, 14, 284, 45, 284, 50, 284, 29, 284, 46, 284, 15, 284, 53, 284, 44, 284, 51, 284, 30, 284, 54, 284, 48, 284, 42, 284, 19, 284, 36, 284, 38, 284, 35, 284, 26, 284, 49, 284, 31, 284, 18, 284, 27, 284, 16, 284, 10, 284, 12, 284, 34, 284, 37, 284, 17, 284, 21, 284, 52, 284, 40, 284, 20, 284, 32, 284, 33, 284, 41, 284, 13, 284, 55, 284, 57, 284, 59, 284, 61, 284, 62, 284, 63, 284, 64, 284, 65, 284, 66, 284, 67, 284, 68, 284, 69, 284],
	#[65535, 290, 23, 290, 45, 290, 29, 290, 22, 290, 46, 290, 52, 290, 42, 290, 54, 290, 27, 290, 47, 290, 50, 290, 28, 290, 14, 290, 38, 290, 62, 290, 13, 290, 55, 290, 53, 290, 48, 290, 20, 290, 37, 290, 18, 290, 43, 290, 49, 290, 57, 290, 61, 290, 44, 290, 30, 290, 12, 290, 26, 290, 51, 290, 32, 290, 17, 290, 15, 290, 10, 290, 19, 290, 35, 290, 16, 290, 21, 290, 41, 290, 40, 290, 31, 290, 59, 290, 33, 290, 39, 290, 34, 290, 36, 290, 63, 290, 64, 290, 65, 290, 66, 290, 67, 290, 68, 290, 69, 290],
	#[65535, 253, 31, 253],
	#[65535, 286, 22, 286, 15, 286, 23, 286, 18, 286, 37, 286, 27, 286, 36, 286, 17, 286, 19, 286, 30, 286, 42, 286, 10, 286, 16, 286, 12, 286, 13, 286, 14, 286, 20, 286, 21, 286, 35, 286, 34, 286, 26, 286, 28, 286, 29, 286, 31, 286, 32, 286, 33, 286, 38, 286, 39, 286, 40, 286, 41, 286, 49, 286, 43, 286, 44, 286, 45, 286, 46, 286, 47, 286, 48, 286, 50, 286, 51, 286, 52, 286, 53, 286, 54, 286, 55, 286, 57, 286, 59, 286, 61, 286, 62, 286, 63, 286, 64, 286, 65, 286, 66, 286, 67, 286, 68, 286, 69, 286],
	#[43, -264, 55, -265, 31, 264, 41, -218, 59, -271, 44, -273, 30, -28, 54, -221, 39, -205, 50, -223, 42, -281, 38, -199, 62, -213, 47, -233, 57, -279, 21, -16, 46, -224, 61, -203, 40, -269, 10, -282, 45, -268, 48, -201, 19, -58, 52, -198, 32, -237, 20, -5, 53, -210, 51, -230, 37, -208, 34, -51, 17, -107, 26, -30, 15, -142, 49, -212, 35, -229, 36, -239, 27, -46, 13, -108, 16, -137, 33, -29, 12, -157, 14, -118, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55],
	#[65535, 285, 23, 285, 46, 285, 28, 285, 55, 285, 45, 285, 54, 285, 43, 285, 12, 285, 15, 285, 22, 285, 47, 285, 27, 285, 30, 285, 18, 285, 42, 285, 31, 285, 14, 285, 38, 285, 26, 285, 52, 285, 50, 285, 39, 285, 29, 285, 13, 285, 35, 285, 53, 285, 44, 285, 51, 285, 37, 285, 48, 285, 49, 285, 19, 285, 36, 285, 20, 285, 41, 285, 32, 285, 17, 285, 16, 285, 33, 285, 34, 285, 40, 285, 21, 285, 10, 285, 57, 285, 59, 285, 61, 285, 62, 285, 63, 285, 64, 285, 65, 285, 66, 285, 67, 285, 68, 285, 69, 285],
	#[31, -340],
	#[52, -100, 50, -132, 31, -141, 43, -130, 49, 339, 30, -125, 39, -96, 51, -148, 29, -111, 42, -101, 38, -98, 53, -114, 47, -153, 37, -97, 22, -128, 46, -135, 28, -161, 55, -112, 10, -159, 45, -119, 48, -103, 19, -152, 26, -127, 36, -94, 35, -146, 40, -121, 33, -104, 14, -118, 27, -143, 44, -122, 21, -115, 25, -113, 15, -142, 41, -124, 32, -158, 24, -102, 20, -106, 23, -150, 13, -108, 17, -107, 18, -131, 12, -157, 16, -137, 57, -151, 58, 296, 59, -134, 61, -105, 62, -117, 63, -136, 64, -120, 65, -129, 66, -145, 67, -133, 68, -149, 69, -147],
	#[31, 271, 10, -289],
	#[65535, 288, 54, 288, 18, 288, 42, 288, 43, 288, 45, 288, 22, 288, 29, 288, 44, 288, 15, 288, 13, 288, 47, 288, 30, 288, 37, 288, 52, 288, 23, 288, 26, 288, 50, 288, 14, 288, 62, 288, 31, 288, 27, 288, 12, 288, 49, 288, 16, 288, 46, 288, 53, 288, 59, 288, 39, 288, 32, 288, 21, 288, 17, 288, 61, 288, 33, 288, 48, 288, 28, 288, 19, 288, 38, 288, 55, 288, 36, 288, 20, 288, 41, 288, 57, 288, 35, 288, 51, 288, 10, 288, 34, 288, 40, 288, 63, 288, 64, 288, 65, 288, 66, 288, 67, 288, 68, 288, 69, 288],
	#[59, -271, 55, -265, 31, 252, 43, -264, 57, -279, 61, -203, 44, -273, 30, -28, 54, -221, 39, -205, 51, -230, 50, -223, 52, -198, 38, -199, 62, -213, 47, -233, 48, -201, 37, -208, 22, -32, 46, -224, 28, -63, 42, -281, 10, -282, 45, -268, 19, -58, 49, -212, 36, -239, 35, -229, 18, -34, 53, -210, 33, -29, 41, -218, 27, -46, 34, -51, 40, -269, 21, -16, 26, -30, 15, -142, 32, -237, 17, -107, 29, -13, 20, -5, 23, -50, 13, -108, 16, -137, 12, -157, 14, -118, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 254, 31, 254],
	#[59, -271, 52, -198, 47, -233, 43, -264, 38, -199, 23, -50, 55, -265, 51, -230, 22, -32, 31, 252, 41, -218, 42, -281, 19, -58, 61, -203, 44, -273, 30, -28, 54, -221, 39, -205, 49, -212, 14, -118, 36, -239, 62, -213, 34, -51, 57, -279, 37, -208, 26, -30, 21, -16, 46, -224, 28, -63, 40, -269, 10, -282, 45, -268, 29, -13, 35, -229, 18, -34, 53, -210, 33, -29, 27, -46, 17, -107, 12, -157, 15, -142, 50, -223, 32, -237, 20, -5, 16, -137, 13, -108, 48, -201, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 268, 31, 268],
	#[65535, 270, 31, 270],
	#[65535, 267, 31, 267],
	#[65535, 272, 31, 272],
	#[55, -265, 43, -264, 47, -233, 29, -13, 30, -28, 54, -221, 31, 252, 51, -230, 45, -268, 36, -239, 39, -205, 22, -32, 46, -224, 40, -269, 53, -210, 44, -273, 28, -63, 37, -208, 50, -223, 41, -218, 23, -50, 10, -282, 52, -198, 14, -118, 20, -5, 35, -229, 26, -30, 48, -201, 13, -108, 16, -137, 33, -29, 12, -157, 49, -212, 19, -58, 21, -16, 18, -34, 15, -142, 42, -281, 27, -46, 17, -107, 34, -51, 38, -199, 32, -237, 57, -279, 59, -271, 61, -203, 62, -213, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 273, 31, 273],
	#[58, -292],
	#[65535, 293, 52, 293, 47, 293, 14, 293, 29, 293, 23, 293, 13, 293, 53, 293, 22, 293, 46, 293, 18, 293, 43, 293, 44, 293, 28, 293, 26, 293, 50, 293, 27, 293, 15, 293, 38, 293, 20, 293, 49, 293, 37, 293, 35, 293, 21, 293, 51, 293, 61, 293, 42, 293, 45, 293, 30, 293, 54, 293, 57, 293, 17, 293, 16, 293, 10, 293, 39, 293, 62, 293, 12, 293, 33, 293, 59, 293, 32, 293, 36, 293, 34, 293, 41, 293, 55, 293, 19, 293, 31, 293, 40, 293, 48, 293, 63, 293, 64, 293, 65, 293, 66, 293, 67, 293, 68, 293, 69, 293],
	#[59, -271],
	#[65535, 338, 31, 338, 20, 338, 30, 338, 38, 338, 36, 338, 22, 338, 46, 338, 18, 338, 32, 338, 21, 338, 19, 338, 60, 338, 35, 338, 58, 338, 26, 338, 29, 338, 27, 338, 62, 338, 47, 338, 37, 338, 56, 338, 23, 338, 51, 338, 28, 338, 55, 338, 10, 338, 45, 338, 25, 338, 33, 338, 41, 338, 59, 338, 57, 338, 13, 338, 24, 338, 53, 338, 42, 338, 14, 338, 39, 338, 44, 338, 17, 338, 12, 338, 61, 338, 49, 338, 52, 338, 40, 338, 43, 338, 50, 338, 16, 338, 48, 338, 15, 338, 63, 338, 64, 338, 65, 338, 66, 338, 67, 338, 68, 338, 69, 338],
	#[65535, 337, 14, 337, 23, 337, 55, 337, 52, 337, 50, 337, 43, 337, 42, 337, 59, 337, 30, 337, 12, 337, 39, 337, 51, 337, 29, 337, 15, 337, 38, 337, 62, 337, 47, 337, 57, 337, 37, 337, 60, 337, 46, 337, 49, 337, 27, 337, 45, 337, 48, 337, 31, 337, 16, 337, 36, 337, 13, 337, 17, 337, 53, 337, 33, 337, 41, 337, 44, 337, 28, 337, 26, 337, 19, 337, 22, 337, 35, 337, 10, 337, 25, 337, 20, 337, 58, 337, 24, 337, 40, 337, 56, 337, 61, 337, 32, 337, 18, 337, 21, 337, 63, 337, 64, 337, 65, 337, 66, 337, 67, 337, 68, 337, 69, 337],
	#[60, -297],
	#[65535, 295, 22, 295, 47, 295, 45, 295, 28, 295, 14, 295, 29, 295, 23, 295, 13, 295, 52, 295, 26, 295, 27, 295, 46, 295, 18, 295, 43, 295, 42, 295, 30, 295, 54, 295, 39, 295, 51, 295, 50, 295, 32, 295, 15, 295, 38, 295, 62, 295, 19, 295, 37, 295, 21, 295, 41, 295, 61, 295, 55, 295, 17, 295, 48, 295, 31, 295, 59, 295, 57, 295, 35, 295, 53, 295, 16, 295, 10, 295, 44, 295, 58, 295, 12, 295, 33, 295, 49, 295, 36, 295, 20, 295, 40, 295, 60, 295, 25, 295, 56, 295, 34, 295, 24, 295, 63, 295, 64, 295, 65, 295, 66, 295, 67, 295, 68, 295, 69, 295],
	#[65535, 255, 31, 255],
	#[65535, 265, 31, 265],
	#[65535, 162, 23, 162, 20, 162, 33, 162, 34, 162, 29, 162, 25, 162, 21, 162],
	#[65535, 164, 23, 164, 21, 164, 29, 164, 20, 164, 25, 164, 34, 164],
	#[34, -309, 21, -311, 29, -313, 20, -307, 23, -306, 25, -308],
	#[23, 165, 34, 165, 21, 165, 29, 165, 20, 165, 25, 165, 33, -300],
	#[65535, 166, 20, 166, 21, 166, 34, 166, 29, 166, 25, 166, 23, 166],
	#[55, -207, 62, -213, 61, -203, 29, -13, 57, -232, 53, -210, 30, -28, 54, -221, 52, -198, 59, -219, 50, -223, 38, -199, 45, -214, 31, 92, 28, -63, 40, -215, 22, -32, 46, -224, 19, -58, 18, -34, 39, -205, 41, -218, 10, -238, 48, -201, 34, -51, 15, -142, 27, -46, 32, -237, 47, -233, 44, -222, 14, -118, 23, -50, 35, -229, 49, -212, 51, -230, 36, -239, 20, -5, 33, -29, 43, -211, 42, -236, 17, -107, 26, -30, 16, -137, 13, -108, 37, -208, 21, -16, 12, -157, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 59, 10, 59, 38, 59, 62, 59, 35, 59, 55, 59, 36, 59, 20, 59, 41, 59, 21, 59, 19, 59, 40, 59, 34, 59, 30, 59, 39, 59, 51, 59, 29, 59, 14, 59, 47, 59, 59, 59, 23, 59, 46, 59, 61, 59, 12, 59, 16, 59, 45, 59, 43, 59, 31, 59, 54, 59, 57, 59, 13, 59, 17, 59, 53, 59, 42, 59, 32, 59, 37, 59, 44, 59, 28, 59, 26, 59, 15, 59, 22, 59, 52, 59, 33, 59, 49, 59, 50, 59, 18, 59, 48, 59, 27, 59, 63, 59, 64, 59, 65, 59, 66, 59, 67, 59, 68, 59, 69, 59],
	#[65535, 57, 38, 57, 55, 57, 20, 57, 21, 57, 19, 57, 30, 57, 39, 57, 51, 57, 36, 57, 62, 57, 47, 57, 59, 57, 37, 57, 23, 57, 46, 57, 61, 57, 12, 57, 10, 57, 34, 57, 31, 57, 54, 57, 57, 57, 35, 57, 17, 57, 53, 57, 42, 57, 41, 57, 27, 57, 40, 57, 28, 57, 26, 57, 15, 57, 22, 57, 45, 57, 52, 57, 29, 57, 33, 57, 43, 57, 50, 57, 16, 57, 14, 57, 48, 57, 49, 57, 13, 57, 32, 57, 44, 57, 18, 57, 63, 57, 64, 57, 65, 57, 66, 57, 67, 57, 68, 57, 69, 57],
	#[65535, 58, 38, 58, 10, 58, 55, 58, 20, 58, 41, 58, 21, 58, 19, 58, 30, 58, 39, 58, 51, 58, 36, 58, 62, 58, 47, 58, 59, 58, 37, 58, 40, 58, 23, 58, 46, 58, 61, 58, 12, 58, 16, 58, 34, 58, 31, 58, 54, 58, 57, 58, 35, 58, 17, 58, 53, 58, 42, 58, 14, 58, 27, 58, 44, 58, 28, 58, 26, 58, 15, 58, 22, 58, 45, 58, 52, 58, 29, 58, 33, 58, 13, 58, 43, 58, 50, 58, 18, 58, 48, 58, 49, 58, 32, 58, 63, 58, 64, 58, 65, 58, 66, 58, 67, 58, 68, 58, 69, 58],
	#[65535, 41, 23, 41, 30, 41, 29, 41, 21, 41, 28, 41, 27, 41, 31, 41, 20, 41, 33, 41, 26, 41, 19, 41, 22, 41, 24, 41, 18, 41, 25, 41, 32, 41],
	#[28, -161, 23, -150, 20, -106, 31, -141, 26, -127, 30, -125, 32, -158, 29, -111, 19, -152, 18, -131, 33, -104, 27, -143, 21, -115, 24, -102, 25, -113, 22, -128],
	#[65535, 60, 23, 60, 47, 60, 45, 60, 14, 60, 38, 60, 62, 60, 55, 60, 52, 60, 22, 60, 46, 60, 43, 60, 42, 60, 44, 60, 35, 60, 54, 60, 29, 60, 18, 60, 15, 60, 19, 60, 37, 60, 50, 60, 51, 60, 28, 60, 57, 60, 32, 60, 48, 60, 31, 60, 59, 60, 36, 60, 20, 60, 17, 60, 53, 60, 16, 60, 41, 60, 39, 60, 2, 60, 21, 60, 12, 60, 61, 60, 49, 60, 10, 60, 34, 60, 13, 60, 40, 60, 60, 60, 58, 60, 33, 60, 56, 60, 11, 60, 63, 60, 65, 60, 66, 60, 67, 60, 68, 60, 69, 60],
	#[55, -207, 31, 96, 62, -213, 2, 96, 29, -13, 57, -232, 28, -63, 11, 96, 53, -210, 10, 96, 54, -221, 52, -198, 56, 96, 50, -223, 14, -118, 38, -199, 45, -214, 60, 96, 32, -237, 34, -51, 41, -218, 58, 96, 22, -32, 46, -224, 19, -58, 59, -219, 40, -254, 35, -229, 20, -5, 48, -201, 15, -142, 61, -203, 33, -29, 47, -233, 44, -222, 12, -157, 23, -50, 49, -212, 51, -230, 36, -239, 17, -107, 43, -211, 42, -236, 16, -137, 39, -205, 18, -34, 13, -108, 37, -208, 21, -16, 63, -225, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 42, 28, 42, 30, 42, 26, 42, 29, 42, 23, 42, 27, 42, 33, 42, 31, 42, 32, 42, 21, 42, 19, 42, 22, 42, 24, 42, 20, 42, 18, 42, 25, 42],
	#[65535, 160, 11, 160, 10, 160, 2, 160, 31, 160, 56, 160, 58, 160, 60, 160],
	#[65535, 117, 60, 117, 10, 117, 2, 117, 58, 117, 31, 117, 11, 117, 56, 117],
	#[65535, 116, 31, 116, 11, 116, 2, 116, 56, 116, 10, 116, 60, 116, 58, 116],
	#[56, -318],
	#[65535, 142, 23, 142, 45, 142, 44, 142, 29, 142, 22, 142, 46, 142, 18, 142, 52, 142, 42, 142, 30, 142, 54, 142, 47, 142, 49, 142, 28, 142, 14, 142, 38, 142, 62, 142, 13, 142, 55, 142, 53, 142, 26, 142, 27, 142, 37, 142, 31, 142, 43, 142, 21, 142, 19, 142, 61, 142, 34, 142, 35, 142, 12, 142, 39, 142, 51, 142, 50, 142, 32, 142, 48, 142, 15, 142, 20, 142, 59, 142, 58, 142, 11, 142, 41, 142, 57, 142, 16, 142, 17, 142, 56, 142, 36, 142, 10, 142, 2, 142, 33, 142, 40, 142, 60, 142, 63, 142, 64, 142, 65, 142, 66, 142, 67, 142, 68, 142, 69, 142],
	#[65535, 108, 31, 108, 2, 108, 60, 108, 56, 108, 58, 108],
	#[65535, 106, 31, 106, 58, 106, 2, 106, 60, 106, 56, 106],
	#[65535, 109, 60, 109, 58, 109, 2, 109, 31, 109, 56, 109],
	#[59, -271, 55, -265, 31, 252, 43, -264, 61, -203, 30, -28, 54, -221, 39, -205, 38, -199, 62, -213, 47, -233, 57, -279, 37, -208, 23, -50, 46, -224, 28, -63, 42, -281, 10, -282, 45, -268, 34, -51, 41, -218, 26, -30, 36, -239, 50, -223, 51, -230, 14, -118, 27, -46, 40, -269, 21, -16, 12, -157, 19, -58, 22, -32, 35, -229, 32, -237, 52, -198, 29, -13, 20, -5, 49, -212, 53, -210, 17, -107, 18, -34, 48, -201, 33, -29, 16, -137, 13, -108, 15, -142, 44, -273, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[31, 153, 11, 153, 2, 153, 40, -346, 10, 153, 56, 153, 58, 153, 60, 153],
	#[65535, 151, 31, 151, 60, 151, 58, 151, 2, 151, 56, 151, 10, 151, 11, 151],
	#[55, -207, 54, -221, 47, -233, 30, -28, 43, -211, 31, 92, 51, -230, 45, -214, 39, -205, 46, -224, 40, -215, 53, -210, 44, -222, 28, -63, 37, -208, 50, -223, 23, -50, 10, -238, 52, -198, 36, -239, 20, -5, 35, -229, 41, -218, 29, -13, 48, -201, 18, -34, 13, -108, 22, -32, 33, -29, 12, -157, 49, -212, 19, -58, 16, -137, 21, -16, 15, -142, 42, -236, 27, -46, 17, -107, 34, -51, 14, -118, 38, -199, 26, -30, 32, -237, 57, -232, 59, -219, 61, -203, 62, -213, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[53, -210, 37, -208, 46, -224, 19, -58, 49, -212, 35, -229, 10, -238, 12, -157, 13, -108, 14, -118, 15, -142, 16, -137, 17, -107, 18, -34, 20, -5, 21, -16, 22, -32, 23, -50, 51, -230, 26, -30, 27, -46, 28, -63, 29, -13, 30, -28, 31, 92, 32, -237, 33, -29, 34, -51, 36, -239, 38, -199, 39, -205, 40, -215, 41, -218, 42, -236, 43, -211, 44, -222, 45, -214, 47, -233, 48, -201, 50, -223, 52, -198, 54, -221, 55, -207, 57, -232, 59, -219, 61, -203, 62, -213, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[31, -328],
	#[23, -331, 28, -63, 31, 167, 58, 167, 25, -308, 22, -32, 29, -13, 2, 167, 18, -34, 33, -29, 10, 167, 11, 167, 40, 167, 20, -332, 56, 167, 34, -51, 60, 167, 21, -16],
	#[65535, 157, 2, 157, 10, 157, 11, 157, 31, 157, 40, 157, 56, 157, 58, 157, 60, 157],
	#[23, -50, 29, -13, 22, -32, 28, -63, 21, -16, 18, -34, 20, -5, 34, -51, 33, -29],
	#[22, 59, 23, 59, 28, 59, 31, 49, 11, 49, 2, 49, 21, 59, 18, 59, 10, 49, 29, 59, 20, 59, 60, 49, 40, 49, 56, 49, 34, 59, 58, 49, 33, 59],
	#[2, 34, 10, 34, 11, 34, 18, 57, 20, 57, 21, 57, 22, 57, 23, 57, 28, 57, 29, 57, 31, 34, 33, 57, 34, 57, 40, 34, 56, 34, 58, 34, 60, 34],
	#[65535, 45, 56, 45, 58, 45, 60, 45, 31, 45, 10, 45, 11, 45, 2, 45, 40, 45],
	#[65535, 44, 2, 44, 10, 44, 11, 44, 31, 44, 40, 44, 56, 44, 58, 44, 60, 44],
	#[65535, 43, 31, 43, 11, 43, 10, 43, 40, 43, 2, 43, 56, 43, 58, 43, 60, 43],
	#[65535, 168, 31, 168, 60, 168, 58, 168, 11, 168, 10, 168, 40, 168, 56, 168, 2, 168],
	#[65535, 169, 31, 169, 60, 169, 11, 169, 58, 169, 40, 169, 10, 169, 56, 169, 2, 169],
	#[31, -340],
	#[65535, 229, 55, 229, 38, 229, 11, 229, 20, 229, 46, 229, 21, 229, 19, 229, 59, 229, 34, 229, 39, 229, 13, 229, 36, 229, 41, 229, 47, 229, 37, 229, 35, 229, 60, 229, 61, 229, 12, 229, 10, 229, 43, 229, 58, 229, 54, 229, 50, 229, 17, 229, 53, 229, 51, 229, 14, 229, 27, 229, 40, 229, 26, 229, 15, 229, 49, 229, 45, 229, 48, 229, 33, 229, 2, 229, 30, 229, 44, 229, 42, 229, 31, 229, 16, 229, 56, 229, 32, 229, 52, 229, 57, 229, 62, 229, 63, 229, 64, 229, 65, 229, 66, 229, 67, 229, 68, 229],
	#[55, 46, 39, 46, 21, 46, 46, 46, 10, 46, 36, 46, 20, 46, 54, 46, 37, 46, 28, -63, 12, 46, 14, 46, 38, 46, 51, 46, 45, 46, 16, 46, 43, 46, 29, -13, 19, 46, 40, 46, 13, 46, 15, 46, 44, 46, 47, 46, 30, 46, 50, 46, 33, 46, 41, 46, 23, -50, 52, 46, 2, 46, 35, 46, 11, 46, 31, 46, 18, -34, 27, 46, 22, -32, 53, 46, 34, 46, 17, 46, 48, 46, 49, 46, 42, 46, 26, 46, 32, 46, 56, 46, 57, 46, 58, 46, 59, 46, 60, 46, 61, 46, 62, 46, 63, 46, 64, 46, 65, 46, 66, 46, 67, 46, 68, 46],
	#[65535, 233, 30, 233, 2, 233, 53, 233, 27, 233, 31, 233, 49, 233, 10, 233, 11, 233, 12, 233, 13, 233, 14, 233, 15, 233, 16, 233, 17, 233, 19, 233, 20, 233, 21, 233, 35, 233, 51, 233, 26, 233, 32, 233, 42, 233, 33, 233, 34, 233, 36, 233, 37, 233, 38, 233, 39, 233, 40, 233, 41, 233, 43, 233, 44, 233, 45, 233, 46, 233, 47, 233, 48, 233, 50, 233, 52, 233, 54, 233, 55, 233, 56, 233, 57, 233, 58, 233, 59, 233, 60, 233, 61, 233, 62, 233, 63, 233, 64, 233, 65, 233, 66, 233, 67, 233, 68, 233],
	#[65535, 47, 13, 47, 52, 47, 31, 47, 43, 47, 45, 47, 26, 47, 27, 47, 15, 47, 38, 47, 47, 47, 35, 47, 50, 47, 51, 47, 42, 47, 32, 47, 34, 47, 2, 47, 30, 47, 54, 47, 36, 47, 20, 47, 17, 47, 11, 47, 16, 47, 14, 47, 39, 47, 44, 47, 21, 47, 12, 47, 33, 47, 41, 47, 49, 47, 46, 47, 55, 47, 40, 47, 19, 47, 37, 47, 48, 47, 53, 47, 10, 47, 56, 47, 57, 47, 58, 47, 59, 47, 60, 47, 61, 47, 62, 47, 63, 47, 64, 47, 65, 47, 66, 47, 67, 47, 68, 47],
	#[65535, 232, 52, 232, 30, 232, 58, 232, 47, 232, 45, 232, 14, 232, 38, 232, 62, 232, 13, 232, 53, 232, 26, 232, 31, 232, 60, 232, 35, 232, 54, 232, 39, 232, 51, 232, 50, 232, 27, 232, 15, 232, 37, 232, 56, 232, 32, 232, 46, 232, 42, 232, 16, 232, 17, 232, 43, 232, 2, 232, 41, 232, 59, 232, 57, 232, 20, 232, 11, 232, 10, 232, 44, 232, 21, 232, 12, 232, 61, 232, 49, 232, 36, 232, 40, 232, 55, 232, 19, 232, 48, 232, 33, 232, 34, 232, 63, 232, 64, 232, 65, 232, 66, 232, 67, 232, 68, 232],
	#[31, -328],
	#[65535, 155, 10, 155, 2, 155, 60, 155, 31, 155, 58, 155, 11, 155, 40, 155, 56, 155],
	#[29, -13, 23, -50, 21, -16, 28, -63, 22, -32, 18, -34, 33, -29, 53, -17, 20, -5, 34, -51],
	#[65535, 154, 60, 154, 2, 154, 31, 154, 58, 154, 11, 154, 10, 154, 56, 154],
	#[31, -328],
	#[65535, 156, 60, 156, 10, 156, 31, 156, 2, 156, 40, 156, 56, 156, 58, 156, 11, 156],
	#[65535, 266, 31, 266],
	#[65535, 269, 31, 269],
	#[56, -353],
	#[65535, 292, 18, 292, 47, 292, 28, 292, 29, 292, 23, 292, 13, 292, 52, 292, 26, 292, 22, 292, 46, 292, 31, 292, 43, 292, 42, 292, 44, 292, 30, 292, 45, 292, 39, 292, 51, 292, 50, 292, 27, 292, 15, 292, 38, 292, 32, 292, 37, 292, 35, 292, 49, 292, 55, 292, 34, 292, 54, 292, 36, 292, 20, 292, 17, 292, 53, 292, 16, 292, 14, 292, 21, 292, 12, 292, 19, 292, 41, 292, 40, 292, 10, 292, 48, 292, 33, 292, 57, 292, 59, 292, 61, 292, 62, 292, 63, 292, 64, 292, 65, 292, 66, 292, 67, 292, 68, 292, 69, 292],
	#[55, -265, 54, -221, 57, -279, 31, 252, 36, -239, 42, -281, 10, -282, 43, -264, 45, -268, 44, -273, 28, -63, 53, -210, 27, -46, 30, -28, 37, -208, 52, -198, 23, -50, 40, -269, 59, -271, 50, -223, 14, -118, 38, -199, 62, -213, 51, -230, 32, -237, 41, -218, 49, -212, 22, -32, 46, -224, 19, -58, 18, -34, 39, -205, 35, -229, 20, -5, 48, -201, 34, -51, 15, -142, 26, -30, 47, -233, 12, -157, 29, -13, 17, -107, 33, -29, 61, -203, 16, -137, 13, -108, 21, -16, 63, -225, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[31, -356],
	#[29, -360, 21, -16, 2, 350, 22, -32, 60, 350, 28, -63, 34, -358, 23, -50, 20, -5, 18, -34, 10, 350, 31, 350, 58, 350, 56, 350, 33, -29, 11, 350],
	#[65535, 161, 10, 161, 31, 161, 11, 161, 2, 161, 56, 161, 58, 161, 60, 161],
	#[21, 41, 23, 41, 29, 41, 31, 38, 60, 38, 22, 41, 28, 41, 58, 38, 33, 41, 18, 41, 20, 41, 10, 38, 2, 38, 11, 38, 34, 41, 56, 38],
	#[20, -5, 22, -32, 33, -29, 28, -63, 29, -13, 23, -50, 21, -16, 34, -51, 18, -34],
	#[31, 54, 60, 54, 28, 42, 11, 54, 23, 42, 29, 42, 20, 42, 2, 54, 21, 42, 22, 42, 10, 54, 33, 42, 18, 42, 34, 42, 58, 54, 56, 54],
	#[65535, 351, 31, 351, 2, 351, 10, 351, 11, 351, 56, 351, 58, 351, 60, 351],
	#[65535, 352, 10, 352, 2, 352, 58, 352, 31, 352, 60, 352, 56, 352, 11, 352],
	#[31, -328],
	#[65535, 159, 31, 159, 2, 159, 11, 159, 60, 159, 10, 159, 58, 159, 56, 159],
	#[65535, 230, 39, 230, 55, 230, 57, 230, 35, 230, 37, 230, 21, 230, 36, 230, 20, 230, 34, 230, 44, 230, 15, 230, 13, 230, 53, 230, 10, 230, 54, 230, 17, 230, 19, 230, 33, 230, 61, 230, 14, 230, 38, 230, 45, 230, 51, 230, 31, 230, 27, 230, 41, 230, 43, 230, 46, 230, 50, 230, 59, 230, 52, 230, 16, 230, 49, 230, 11, 230, 30, 230, 58, 230, 26, 230, 47, 230, 40, 230, 12, 230, 32, 230, 62, 230, 2, 230, 60, 230, 56, 230, 48, 230, 42, 230, 63, 230, 64, 230, 65, 230, 66, 230, 67, 230, 68, 230],
	#[13, -66, 23, -50, 21, -16, 42, -61, 34, -51, 22, -32, 53, -17, 35, -53, 41, -27, 43, -18, 12, -43, 29, -13, 55, -14, 36, -64, 20, -5, 19, -58, 28, -63, 18, -34, 33, -29, 61, -7, 62, -22, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 221, 37, 221, 36, 221, 38, 221, 2, 221, 58, 221, 39, 221, 48, 221, 10, 221, 40, 221, 56, 221, 11, 221],
	#[56, -369],
	#[65535, 183, 46, 183, 55, 183, 38, 183, 39, 183, 37, 183, 48, 183, 11, 183, 36, 183, 2, 183, 10, 183, 40, 183, 56, 183, 57, 183, 58, 183],
	#[65535, 171, 39, 171, 55, 171, 10, 171, 46, 171, 11, 171, 38, 171, 36, 171, 40, 171, 2, 171, 48, 171, 37, 171, 56, 171, 57, 171, 58, 171],
	#[13, -66, 36, -64, 29, -13, 22, -32, 53, -17, 33, -29, 35, -53, 41, -74, 43, -18, 12, -43, 23, -50, 20, -5, 19, -58, 34, -51, 28, -63, 18, -34, 42, -61, 21, -16, 55, -14, 61, -7, 62, -22, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 176, 58, 176, 40, 176, 56, 176],
	#[65535, 172, 39, 172, 46, 172, 55, 172, 48, 172, 11, 172, 10, 172, 40, 172, 36, 172, 37, 172, 38, 172, 2, 172, 56, 172, 57, 172, 58, 172],
	#[65535, 170, 55, 170, 46, 170, 39, 170, 37, 170, 38, 170, 11, 170, 36, 170, 40, 170, 2, 170, 10, 170, 48, 170, 56, 170, 57, 170, 58, 170],
	#[2, -376],
	#[65535, 3, #"eoi", 3],
	#[47, -380, 2, 212],
	#[2, -379],
	#[65535, 4, #"eoi", 4],
	#[61, -7, 13, -66, 23, -50, 55, -14, 62, -22, 28, -63, 12, -43, 22, -32, 19, -58, 18, -34, 33, -29, 43, -18, 53, -17, 20, -5, 34, -51, 29, -13, 42, -61, 21, -16, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 213, 2, 213],
	#[2, 216, 46, -67, 55, -69, 57, -68],
	#[2, -384],
	#[65535, 8, #"eoi", 8],
	#[2, -386],
	#[65535, 2, #"eoi", 2],
	#[52, -198, 46, -224, 55, -207, 54, -221, 53, -210, 38, -199, 36, -239, 45, -214, 29, -13, 15, -142, 51, -230, 47, -233, 37, -208, 48, -201, 23, -50, 19, -58, 50, -223, 14, -118, 42, -236, 17, -107, 32, -237, 34, -51, 41, -218, 49, -212, 22, -32, 39, -205, 35, -229, 20, -5, 21, -16, 16, -137, 44, -222, 28, -63, 12, -157, 33, -29, 13, -108, 43, -211, 18, -34, 57, -232, 59, -219, 61, -203, 62, -213, 63, -225, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[2, 88, 40, -392],
	#[65535, 87, 2, 87],
	#[2, -391],
	#[65535, 7, #"eoi", 7],
	#[41, -387],
	#[65535, 89, 2, 89],
	#[65535, 90, 2, 90, 40, 90],
	#[55, -207, 47, -233, 38, -199, 53, -210, 46, -224, 52, -198, 39, -205, 54, -221, 14, -118, 62, -213, 51, -230, 23, -50, 43, -211, 59, -219, 40, 100, 44, -222, 37, -208, 32, -237, 36, -239, 61, -203, 20, -5, 33, -29, 41, -218, 19, -58, 15, -142, 34, -51, 22, -32, 45, -214, 49, -212, 50, -223, 18, -34, 42, -236, 13, -108, 28, -63, 2, 100, 35, -229, 17, -107, 21, -16, 16, -137, 29, -13, 12, -157, 48, -201, 57, -232, 63, -225, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 102, 2, 102, 40, 102],
	#[53, -210, 12, -157, 2, 118, 17, -107, 13, -108, 14, -118, 15, -142, 16, -137, 19, -58, 20, -5, 21, -16, 46, -224, 51, -230, 44, -222, 49, -212, 42, -236, 32, -237, 33, -29, 34, -51, 35, -229, 36, -239, 37, -208, 38, -199, 39, -205, 40, 118, 41, -218, 43, -211, 45, -214, 47, -233, 48, -201, 50, -223, 52, -198, 54, -221, 55, -207, 57, -232, 59, -219, 61, -203, 62, -213, 63, -225, 65, -33, 66, -48, 67, -37, 68, -55],
	#[65535, 91, 2, 91, 40, 91],
	#[65535, 119, 2, 119, 40, 119],
	#[65535, 103, 40, 103, 2, 103],
	#[65535, 101, 2, 101, 40, 101],
	#[65535, 120, 2, 120, 40, 120],
	#[65535, 77, 2, 77],
	#[65535, 81, 2, 81, 10, 81],
	#[10, -409, 2, 210],
	#[2, -407],
	#[65535, 5, #"eoi", 5],
	#[65535, 78, 2, 78],
	#[62, -22, 13, -66, 61, -7, 2, 211, 35, -53, 41, -27, 27, -46, 22, -32, 28, -63, 43, -18, 19, -58, 21, -16, 30, -28, 36, -64, 20, -5, 42, -61, 29, -13, 23, -50, 12, -43, 18, -34, 55, -14, 33, -29, 34, -51, 26, -30, 53, -17, 63, -41, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 82, 10, 82, 2, 82],
	#[65535, 10, #"eoi", 10],
	#[65535, 9, #"eoi", 9],
	#[29, -13, 23, -50, 43, -18, 20, -5, 22, -32, 42, -61, 35, -53, 28, -63, 12, -43, 21, -16, 36, -64, 41, -27, 13, -66, 33, -29, 19, -58, 18, -34, 34, -51, 53, -17, 55, -14, 61, -7, 62, -22, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[48, -434],
	#[13, -66, 2, 246, 26, -30, 28, -63, 53, -17, 22, -32, 27, -46, 21, -16, 19, -58, 10, -441, 12, -43, 18, -34, 33, -29, 20, -5, 23, -50, 29, -13, 30, -28, 42, -61, 34, -51, 35, -53, 36, -64, 41, -27, 43, -18, 55, -14, 61, -7, 62, -22, 63, -41, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[67, -37, 55, -14, 61, -7, 62, -22, 63, -41, 64, -23, 65, -33, 66, -48, 68, -55, 69, -56, 22, -32, 13, -66, 53, -17, 30, -28, 23, -50, 42, -61, 12, -43, 19, -58, 35, -53, 34, -51, 27, -46, 41, -27, 43, -18, 18, -34, 29, -13, 10, -427, 2, 246, 36, -64, 20, -5, 33, -29, 21, -16, 28, -63, 26, -30],
	#[65535, 235, 2, 235],
	#[65535, 237, 2, 237],
	#[2, -426],
	#[36, 250, 20, 250, 19, 250, 22, 250, 26, 250, 35, 250, 27, 250, 30, 250, 43, 250, 23, 250, 18, 250, 42, 250, 10, 250, 28, 250, 41, 250, 13, 250, 53, 250, 33, 250, 21, 250, 48, -424, 12, 250, 29, 250, 55, 250, 34, 250, 2, 250, 61, 250, 62, 250, 63, 250, 64, 250, 65, 250, 66, 250, 67, 250, 68, 250, 69, 250],
	#[48, 217, 40, -422, 56, 217],
	#[29, -13, 43, -18, 23, -50, 55, -14, 28, -63, 22, -32, 53, -17, 36, -64, 20, -5, 41, -27, 18, -34, 12, -43, 13, -66, 33, -29, 19, -58, 61, -7, 34, -51, 42, -61, 35, -53, 21, -16, 62, -22, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 218, 48, 218, 56, 218],
	#[65535, 251, 10, 251, 63, 251, 22, 251, 69, 251, 13, 251, 53, 251, 30, 251, 23, 251, 61, 251, 55, 251, 2, 251, 33, 251, 29, 251, 18, 251, 12, 251, 19, 251, 21, 251, 20, 251, 34, 251, 43, 251, 41, 251, 26, 251, 42, 251, 64, 251, 27, 251, 36, 251, 28, 251, 62, 251, 35, 251, 65, 251, 66, 251, 67, 251, 68, 251],
	#[65535, 249, 35, 249, 28, 249, 13, 249, 18, 249, 41, 249, 20, 249, 30, 249, 2, 249, 61, 249, 33, 249, 43, 249, 12, 249, 23, 249, 55, 249, 53, 249, 36, 249, 22, 249, 21, 249, 10, 249, 34, 249, 26, 249, 29, 249, 27, 249, 42, 249, 19, 249, 62, 249, 63, 249, 64, 249, 65, 249, 66, 249, 67, 249, 68, 249, 69, 249],
	#[65535, 6, #"eoi", 6],
	#[12, -43, 53, -17, 22, -32, 43, -18, 34, -51, 28, -63, 13, -66, 36, -64, 2, 245, 35, -53, 23, -50, 41, -27, 55, -413, 19, -58, 20, -5, 18, -34, 21, -16, 33, -29, 29, -13, 32, -420, 42, -61, 61, -7, 62, -22, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 244, 2, 244],
	#[10, -431, 2, 210],
	#[65535, 243, 2, 243],
	#[13, -66, 22, -32, 30, -28, 41, -27, 23, -50, 55, -413, 32, -420, 20, -5, 43, -18, 19, -58, 28, -63, 12, -43, 18, -34, 42, -61, 36, -64, 34, -51, 2, 211, 35, -53, 21, -16, 26, -30, 53, -17, 33, -29, 27, -46, 29, -13, 61, -7, 62, -22, 63, -41, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 239, 36, 239, 27, 239, 23, 239, 42, 239, 55, 239, 2, 239, 20, 239, 19, 239, 22, 239, 26, 239, 13, 239, 53, 239, 41, 239, 30, 239, 34, 239, 43, 239, 35, 239, 33, 239, 10, 239, 21, 239, 18, 239, 12, 239, 29, 239, 28, 239, 61, 239, 62, 239, 63, 239, 64, 239, 65, 239, 66, 239, 67, 239, 68, 239, 69, 239],
	#[48, 217, 40, -422, 10, 85, 2, 85],
	#[65535, 247, 28, 247, 23, 247, 10, 247, 29, 247, 2, 247, 43, 247, 53, 247, 33, 247, 35, 247, 20, 247, 21, 247, 26, 247, 18, 247, 12, 247, 42, 247, 13, 247, 55, 247, 22, 247, 41, 247, 19, 247, 34, 247, 30, 247, 27, 247, 36, 247, 61, 247, 62, 247, 63, 247, 64, 247, 65, 247, 66, 247, 67, 247, 68, 247, 69, 247],
	#[40, -436, 56, -369],
	#[61, -7, 13, -66, 23, -50, 55, -14, 35, -53, 62, -22, 28, -63, 12, -43, 22, -32, 19, -58, 18, -34, 20, -5, 33, -29, 43, -18, 36, -64, 34, -51, 41, -27, 29, -13, 42, -61, 21, -16, 53, -17, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[56, -438],
	#[48, -439],
	#[65535, 248, 36, 248, 13, 248, 33, 248, 23, 248, 43, 248, 30, 248, 53, 248, 2, 248, 35, 248, 20, 248, 34, 248, 19, 248, 22, 248, 26, 248, 41, 248, 42, 248, 27, 248, 61, 248, 62, 248, 63, 248, 64, 248, 65, 248, 66, 248, 67, 248, 68, 248, 69, 248, 21, 248, 29, 248, 55, 248, 28, 248, 12, 248, 18, 248, 10, 248],
	#[65535, 238, 23, 238, 21, 238, 30, 238, 12, 238, 29, 238, 13, 238, 43, 238, 20, 238, 34, 238, 35, 238, 28, 238, 10, 238, 36, 238, 18, 238, 53, 238, 27, 238, 2, 238, 26, 238, 19, 238, 42, 238, 33, 238, 41, 238, 22, 238, 55, 238, 61, 238, 62, 238, 63, 238, 64, 238, 65, 238, 66, 238, 67, 238, 68, 238, 69, 238],
	#[55, -413, 53, -17, 2, 245, 23, -50, 43, -18, 35, -53, 32, -420, 36, -64, 18, -34, 22, -32, 19, -58, 13, -66, 28, -63, 12, -43, 33, -29, 41, -27, 21, -16, 20, -5, 34, -51, 29, -13, 42, -61, 61, -7, 62, -22, 63, -41, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 236, 2, 236],
	#[2, 210, 10, -445],
	#[65535, 242, 2, 242],
	#[36, -64, 13, -66, 53, -17, 41, -27, 30, -28, 43, -18, 23, -50, 55, -413, 12, -43, 22, -32, 18, -34, 33, -29, 35, -53, 32, -420, 21, -16, 2, 211, 19, -58, 29, -13, 20, -5, 34, -51, 28, -63, 26, -30, 27, -46, 42, -61, 61, -7, 62, -22, 63, -41, 64, -23, 65, -33, 66, -48, 67, -37, 68, -55, 69, -56],
	#[65535, 241, 2, 241],
	#[65535, 240, 2, 240],
	#[2, -449],
	#[65535, 1, #"eoi", 1]],
  goto-table:
      #[#[149, 64, 65, 32, 43, 17, 124, 24, 42, 60, 71, 1, 8, 30, 26, 29, 0, 9, 41, 26, 75, 19, 64, 22, 87, 38, 33, 28, 67, 36, 90, 53, 22, 31, 35, 52, 18, 33, 62, 21, 4, 46, 29, 12, 36, 63, 66, 47, 98, 18, 9, 37, 30, 27, 61, 6, 21, 15, 63, 40, 5, 39, 28, 62, 27, 45, 13, 65, 55, 13, 34, 50, 11, 7, 20, 4, 68, 54, 6, 23, 19, 57, 7, 14, 155, 34, 154, 44, 153, 5, 136, 25, 146, 56, 134, 51, 125, 8, 130, 61, 137, 10, 138, 59, 150, 48, 89, 3, 88, 20, 83, 41, 135, 43, 76, 2, 81, 11, 121, 58, 69, 55, 53, 16, 23, 49, 12, 42, 3, 35],
	#[],
	#[],
	#[55, 196],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[1, 447],
	#[],
	#[155, 226, 120, 203, 119, 216, 118, 201, 117, 219, 138, 234, 121, 233, 134, 227, 110, 205, 124, 215, 125, 8, 104, 230, 103, 337, 92, 208, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 199, 75, 19, 72, 225, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 214, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 237],
	#[],
	#[155, 34, 154, 44, 153, 5, 150, 48, 149, 367, 146, 56, 138, 59, 137, 10, 136, 25, 135, 43, 134, 51, 130, 61, 90, 53, 89, 3, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 55, 13, 53, 16, 43, 17, 42, 60, 41, 26, 36, 63, 35, 52, 34, 50, 33, 28, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 13, 65, 12, 42],
	#[29, 12, 13, 65, 12, 42, 18, 33, 21, 15, 34, 50, 36, 63, 19, 57, 20, 4, 22, 31, 23, 49, 163, 415, 161, 417, 159, 414, 158, 416, 157, 418, 155, 34, 154, 44, 153, 5, 150, 48, 149, 420, 148, 413, 146, 56, 138, 59, 137, 10, 136, 25, 135, 43, 134, 51, 130, 61, 89, 3, 83, 41, 81, 11, 90, 53, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 55, 412, 53, 16, 42, 60, 43, 17, 32, 419, 35, 52, 33, 28, 41, 26, 28, 62],
	#[],
	#[],
	#[],
	#[10, 411, 11, 410],
	#[],
	#[146, 325, 123, 322, 122, 323, 83, 321, 81, 324, 76, 2, 75, 19, 53, 16, 34, 50, 33, 28, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 20, 4, 18, 33],
	#[],
	#[],
	#[153, 5, 154, 44, 138, 59, 135, 43, 134, 51, 137, 10, 155, 34, 150, 48, 130, 61, 136, 25, 121, 58, 146, 56, 149, 64, 124, 24, 125, 8, 98, 403, 97, 404, 94, 405, 90, 53, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 95, 402, 65, 32, 64, 22, 63, 40, 62, 21, 61, 6, 55, 13, 53, 16, 66, 47, 43, 17, 42, 60, 41, 26, 36, 63, 35, 52, 34, 50, 33, 28, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 13, 65, 12, 42],
	#[],
	#[],
	#[],
	#[128, 300, 127, 301, 126, 302, 33, 299],
	#[],
	#[],
	#[101, 387, 100, 388, 99, 389, 41, 386],
	#[],
	#[142, 78, 141, 80, 140, 81, 139, 82, 138, 59, 137, 10, 136, 77, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 41, 79, 13, 65, 12, 42],
	#[],
	#[],
	#[146, 384, 76, 2, 75, 19, 33, 28, 34, 50, 21, 15, 53, 16, 20, 4],
	#[],
	#[155, 226, 138, 234, 125, 8, 124, 215, 121, 233, 120, 203, 119, 216, 118, 201, 117, 219, 134, 227, 110, 205, 104, 230, 103, 382, 92, 208, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 199, 75, 19, 72, 225, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 50, 222, 48, 200, 46, 223, 45, 213, 49, 211, 41, 217, 40, 214, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 42, 235, 43, 210, 29, 12, 28, 62, 27, 45, 26, 29, 51, 229, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 237, 30, 27, 44, 221, 47, 232],
	#[138, 234, 134, 227, 120, 203, 119, 216, 118, 201, 155, 255, 114, 250, 106, 251, 105, 254, 117, 252, 92, 208, 89, 3, 81, 11, 76, 199, 75, 19, 72, 225, 83, 41, 68, 54, 67, 36, 63, 224, 62, 212, 52, 197, 50, 222, 48, 200, 59, 218, 41, 217, 45, 213, 17, 106, 16, 136, 49, 211, 37, 207, 42, 235, 32, 236, 53, 209, 65, 32, 28, 62, 44, 221, 29, 12, 40, 253, 66, 47, 57, 231, 51, 229, 61, 202, 33, 28, 21, 15, 39, 204, 19, 57, 34, 50, 43, 210, 46, 223, 22, 31, 13, 107, 69, 55, 35, 228, 38, 198, 14, 117, 23, 49, 18, 33, 54, 220, 12, 156, 47, 232, 15, 141, 20, 4, 36, 238, 55, 206],
	#[146, 376, 145, 377, 76, 2, 75, 19, 53, 16, 34, 50, 33, 28, 21, 15, 20, 4],
	#[137, 84, 63, 40],
	#[172, 282, 166, 273, 180, 271, 138, 234, 121, 279, 155, 275, 125, 8, 181, 265, 120, 262, 88, 20, 89, 3, 81, 11, 179, 283, 124, 269, 118, 261, 165, 277, 69, 55, 68, 54, 72, 274, 66, 47, 134, 276, 64, 22, 63, 224, 75, 19, 61, 202, 76, 260, 92, 266, 83, 41, 55, 264, 67, 36, 53, 209, 52, 197, 65, 32, 57, 278, 49, 211, 48, 200, 47, 232, 59, 270, 45, 267, 44, 272, 43, 263, 42, 280, 41, 217, 40, 268, 62, 212, 51, 229, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 54, 220, 30, 27, 87, 38, 50, 222, 27, 45, 26, 29, 46, 223, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 38, 198, 14, 117, 13, 107, 12, 156, 28, 62, 39, 204, 10, 281, 23, 49, 15, 141, 29, 12],
	#[],
	#[],
	#[],
	#[],
	#[150, 48, 154, 44, 137, 10, 138, 59, 136, 25, 153, 5, 146, 56, 130, 61, 155, 34, 134, 51, 135, 43, 149, 374, 90, 53, 89, 3, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 55, 13, 53, 16, 43, 17, 42, 60, 41, 26, 36, 63, 35, 52, 34, 50, 33, 28, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 13, 65, 12, 42],
	#[138, 59, 136, 77, 137, 10, 140, 85, 141, 80, 139, 86, 142, 78, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 41, 79, 13, 65, 12, 42],
	#[91, 365, 39, 95, 38, 97, 37, 96, 36, 93],
	#[],
	#[],
	#[],
	#[],
	#[146, 56, 155, 34, 138, 59, 137, 10, 136, 25, 135, 43, 134, 51, 130, 193, 89, 3, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 55, 13, 53, 16, 42, 60, 34, 50, 33, 28, 43, 17, 29, 12, 28, 62, 23, 49, 22, 31, 20, 4, 19, 57, 18, 33, 13, 65, 12, 42, 21, 15],
	#[],
	#[190, 153, 189, 108, 188, 122, 185, 115, 184, 125, 182, 159, 187, 139, 183, 143, 91, 138, 77, 155, 74, 154, 73, 109, 72, 137, 68, 148, 67, 132, 63, 135, 62, 116, 52, 99, 69, 146, 48, 102, 59, 133, 41, 123, 55, 111, 20, 105, 57, 150, 66, 144, 64, 119, 42, 100, 32, 157, 53, 113, 65, 128, 28, 160, 44, 121, 38, 97, 43, 129, 40, 120, 47, 152, 45, 118, 51, 147, 61, 104, 50, 131, 35, 145, 26, 126, 39, 95, 37, 96, 24, 101, 12, 156, 46, 134, 25, 112, 27, 142, 18, 130, 31, 140, 29, 110, 16, 136, 33, 103, 36, 93, 19, 151, 10, 158, 23, 149, 21, 114, 30, 124, 15, 141, 13, 107, 17, 106, 22, 127, 14, 117],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[55, 68, 57, 67, 46, 66],
	#[],
	#[],
	#[],
	#[],
	#[146, 372, 76, 2, 75, 19, 53, 16, 34, 50, 33, 28, 21, 15, 20, 4],
	#[155, 34, 154, 71, 152, 74, 151, 76, 146, 56, 138, 59, 137, 10, 136, 25, 135, 43, 134, 51, 133, 72, 132, 69, 131, 194, 130, 61, 90, 53, 89, 3, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 55, 13, 53, 16, 43, 17, 42, 60, 41, 73, 36, 63, 35, 52, 34, 50, 33, 28, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 13, 65, 12, 42],
	#[155, 34, 154, 71, 152, 74, 151, 76, 146, 56, 143, 70, 138, 59, 137, 10, 135, 43, 133, 72, 130, 61, 136, 25, 132, 69, 134, 51, 131, 75, 90, 53, 89, 3, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 63, 40, 62, 21, 61, 6, 65, 32, 55, 13, 43, 17, 35, 52, 53, 16, 28, 62, 33, 28, 42, 60, 36, 63, 41, 73, 19, 57, 29, 12, 34, 50, 20, 4, 18, 33, 12, 42, 22, 31, 13, 65, 23, 49, 21, 15],
	#[40, 370],
	#[56, 373],
	#[],
	#[],
	#[18, 33, 34, 50, 12, 42, 19, 57, 43, 17, 20, 4, 61, 6, 33, 28, 21, 15, 28, 62, 13, 65, 29, 12, 36, 63, 23, 49, 155, 34, 154, 44, 153, 5, 150, 48, 149, 195, 138, 59, 146, 56, 134, 51, 137, 10, 130, 61, 136, 25, 135, 43, 90, 53, 89, 3, 83, 41, 81, 11, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 63, 40, 62, 21, 65, 32, 55, 13, 53, 16, 76, 2, 41, 26, 35, 52, 22, 31, 42, 60],
	#[91, 94, 39, 95, 38, 97, 37, 96, 36, 93],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[40, 88],
	#[46, 90],
	#[56, 83],
	#[],
	#[],
	#[],
	#[58, 87],
	#[],
	#[142, 89, 138, 59, 137, 10, 136, 77, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 41, 79, 13, 65, 12, 42],
	#[],
	#[142, 91, 138, 59, 137, 10, 136, 77, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 41, 79, 13, 65, 12, 42],
	#[56, 92],
	#[],
	#[],
	#[155, 34, 154, 44, 153, 98, 146, 56, 138, 59, 137, 10, 136, 25, 135, 43, 134, 51, 130, 61, 90, 53, 89, 3, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 55, 13, 53, 16, 43, 17, 42, 60, 41, 26, 36, 63, 35, 52, 34, 50, 33, 28, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 13, 65, 12, 42],
	#[],
	#[],
	#[],
	#[],
	#[181, 293, 77, 292, 74, 154, 73, 109, 59, 270, 33, 103, 32, 157, 31, 140, 30, 124, 29, 110, 28, 160, 27, 142, 26, 126, 25, 112, 24, 101, 23, 149, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[49, 180],
	#[],
	#[],
	#[189, 108, 184, 125, 190, 153, 185, 115, 188, 122, 182, 178, 187, 139, 183, 143, 91, 138, 77, 155, 74, 154, 73, 109, 72, 137, 69, 146, 68, 148, 67, 132, 66, 144, 65, 128, 64, 119, 63, 135, 62, 116, 61, 104, 59, 133, 57, 150, 55, 111, 53, 113, 52, 99, 51, 147, 50, 131, 48, 102, 47, 152, 46, 134, 45, 118, 44, 121, 43, 129, 42, 100, 41, 123, 40, 120, 39, 95, 38, 97, 37, 96, 36, 93, 35, 145, 33, 103, 32, 157, 31, 140, 30, 124, 29, 110, 28, 160, 27, 142, 26, 126, 25, 112, 24, 101, 23, 149, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 158],
	#[],
	#[],
	#[],
	#[190, 153, 189, 108, 188, 122, 183, 177, 184, 125, 187, 139, 185, 115, 91, 138, 77, 155, 74, 154, 73, 109, 72, 137, 69, 146, 68, 148, 67, 132, 66, 144, 65, 128, 64, 119, 63, 135, 62, 116, 61, 104, 59, 133, 57, 150, 55, 111, 53, 113, 52, 99, 51, 147, 50, 131, 48, 102, 47, 152, 46, 134, 45, 118, 44, 121, 43, 129, 42, 100, 41, 123, 40, 120, 39, 95, 38, 97, 37, 96, 36, 93, 35, 145, 33, 103, 32, 157, 31, 140, 30, 124, 29, 110, 28, 160, 27, 142, 26, 126, 25, 112, 24, 101, 23, 149, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 158],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[190, 153, 189, 108, 188, 122, 187, 139, 185, 115, 184, 125, 183, 176, 91, 138, 77, 155, 74, 154, 73, 109, 72, 137, 69, 146, 68, 148, 67, 132, 66, 144, 65, 128, 64, 119, 63, 135, 62, 116, 61, 104, 59, 133, 57, 150, 55, 111, 53, 113, 52, 99, 51, 147, 50, 131, 48, 102, 47, 152, 46, 134, 45, 118, 44, 121, 43, 129, 42, 100, 41, 123, 40, 120, 39, 95, 38, 97, 37, 96, 36, 93, 35, 145, 33, 103, 32, 157, 31, 140, 30, 124, 29, 110, 28, 160, 27, 142, 26, 126, 25, 112, 24, 101, 23, 149, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 158],
	#[],
	#[],
	#[190, 153, 189, 108, 184, 125, 185, 115, 183, 143, 187, 139, 188, 122, 182, 174, 91, 138, 77, 155, 74, 154, 73, 109, 72, 137, 69, 146, 68, 148, 67, 132, 66, 144, 65, 128, 64, 119, 63, 135, 62, 116, 61, 104, 59, 133, 57, 150, 55, 111, 53, 113, 52, 99, 51, 147, 50, 131, 48, 102, 47, 152, 46, 134, 45, 118, 44, 121, 43, 129, 42, 100, 41, 123, 40, 120, 39, 95, 38, 97, 37, 96, 36, 93, 35, 145, 33, 103, 32, 157, 31, 140, 30, 124, 29, 110, 28, 160, 27, 142, 26, 126, 25, 112, 24, 101, 23, 149, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 158],
	#[],
	#[],
	#[77, 172, 74, 154, 73, 109, 45, 173, 33, 103, 32, 157, 31, 140, 30, 124, 29, 110, 28, 160, 27, 142, 26, 126, 25, 112, 24, 101, 23, 149, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130],
	#[],
	#[190, 153, 189, 108, 188, 122, 187, 139, 185, 115, 184, 125, 183, 143, 182, 170, 91, 138, 77, 155, 74, 154, 72, 137, 69, 146, 73, 109, 65, 128, 63, 135, 62, 116, 52, 99, 50, 131, 48, 102, 68, 148, 57, 150, 53, 113, 40, 120, 64, 119, 44, 121, 43, 129, 25, 112, 45, 118, 42, 100, 51, 147, 41, 123, 21, 114, 32, 157, 59, 133, 19, 151, 66, 144, 33, 103, 67, 132, 27, 142, 24, 101, 31, 140, 29, 110, 16, 136, 35, 145, 38, 97, 55, 111, 61, 104, 47, 152, 15, 141, 10, 158, 22, 127, 39, 95, 36, 93, 17, 106, 14, 117, 12, 156, 28, 160, 13, 107, 46, 134, 18, 130, 26, 126, 37, 96, 20, 105, 30, 124, 23, 149],
	#[],
	#[54, 169],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[190, 153, 189, 108, 184, 125, 185, 115, 183, 143, 187, 139, 188, 122, 91, 138, 182, 167, 77, 155, 74, 154, 73, 109, 72, 137, 69, 146, 68, 148, 67, 132, 66, 144, 65, 128, 64, 119, 63, 135, 62, 116, 61, 104, 59, 133, 57, 150, 55, 111, 53, 113, 52, 99, 51, 147, 50, 131, 48, 102, 47, 152, 46, 134, 45, 118, 44, 121, 43, 129, 41, 123, 40, 120, 39, 95, 38, 97, 37, 96, 36, 93, 35, 145, 33, 103, 32, 157, 42, 100, 30, 124, 29, 110, 28, 160, 27, 142, 26, 126, 25, 112, 24, 101, 23, 149, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 158, 31, 140],
	#[],
	#[40, 120, 64, 119, 50, 131, 41, 123, 68, 148, 25, 112, 66, 144, 74, 154, 57, 150, 33, 103, 51, 147, 65, 128, 47, 152, 22, 127, 59, 133, 61, 104, 77, 155, 67, 132, 53, 113, 73, 109, 28, 160, 32, 157, 35, 145, 36, 93, 69, 146, 17, 106, 62, 116, 38, 97, 18, 130, 37, 96, 30, 124, 42, 100, 13, 107, 16, 136, 44, 121, 19, 151, 43, 129, 20, 105, 27, 142, 63, 135, 14, 117, 24, 101, 31, 140, 12, 156, 55, 111, 29, 110, 45, 118, 46, 134, 52, 99, 21, 114, 39, 95, 26, 126, 10, 158, 15, 141, 23, 149, 190, 153, 189, 108, 188, 122, 185, 115, 184, 125, 183, 143, 182, 165, 187, 139, 91, 138, 72, 137, 48, 102],
	#[77, 164, 74, 154, 73, 109, 33, 103, 32, 157, 31, 140, 30, 124, 29, 110, 28, 160, 27, 142, 26, 126, 25, 112, 24, 101, 23, 149, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130],
	#[],
	#[],
	#[187, 139, 185, 115, 183, 143, 189, 108, 182, 162, 184, 125, 190, 153, 188, 122, 91, 138, 77, 155, 74, 154, 73, 109, 72, 137, 69, 146, 68, 148, 67, 132, 66, 144, 65, 128, 64, 119, 63, 135, 62, 116, 61, 104, 59, 133, 57, 150, 55, 111, 53, 113, 52, 99, 51, 147, 50, 131, 48, 102, 47, 152, 46, 134, 45, 118, 44, 121, 43, 129, 42, 100, 41, 123, 40, 120, 39, 95, 38, 97, 37, 96, 36, 93, 35, 145, 33, 103, 32, 157, 31, 140, 30, 124, 29, 110, 28, 160, 27, 142, 26, 126, 25, 112, 24, 101, 23, 149, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 158],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[60, 161],
	#[],
	#[],
	#[58, 163],
	#[],
	#[],
	#[60, 166],
	#[],
	#[58, 168],
	#[],
	#[],
	#[60, 171],
	#[],
	#[],
	#[],
	#[56, 175],
	#[],
	#[],
	#[],
	#[56, 179],
	#[],
	#[193, 184, 77, 186, 74, 154, 73, 109, 63, 182, 23, 149, 32, 157, 33, 103, 25, 112, 31, 140, 27, 142, 41, 185, 55, 181, 18, 130, 28, 160, 26, 126, 24, 101, 45, 183, 30, 124, 21, 114, 19, 151, 22, 127, 29, 110, 20, 105],
	#[138, 59, 136, 25, 146, 56, 134, 51, 154, 44, 149, 191, 155, 34, 150, 48, 153, 5, 137, 10, 130, 61, 135, 43, 90, 53, 89, 3, 83, 41, 81, 11, 76, 2, 75, 19, 68, 54, 66, 47, 62, 21, 61, 6, 65, 32, 55, 13, 67, 36, 53, 16, 69, 55, 43, 17, 42, 60, 41, 26, 36, 63, 35, 52, 63, 40, 33, 28, 29, 12, 34, 50, 22, 31, 21, 15, 20, 4, 18, 33, 28, 62, 19, 57, 13, 65, 23, 49, 12, 42],
	#[],
	#[],
	#[192, 187, 191, 189, 54, 188],
	#[],
	#[],
	#[],
	#[63, 190],
	#[],
	#[],
	#[56, 192],
	#[],
	#[57, 67, 55, 68, 46, 66],
	#[58, 369],
	#[],
	#[155, 226, 138, 234, 134, 227, 125, 8, 124, 215, 121, 233, 120, 203, 119, 216, 118, 201, 117, 219, 110, 205, 104, 230, 103, 239, 92, 208, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 199, 75, 19, 72, 225, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 214, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 237],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[155, 226, 138, 234, 134, 227, 125, 8, 124, 215, 121, 233, 120, 203, 119, 216, 118, 201, 117, 219, 110, 205, 104, 230, 103, 316, 92, 208, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 199, 75, 19, 72, 225, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 214, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 237],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[155, 226, 138, 234, 134, 227, 125, 8, 124, 215, 120, 203, 119, 216, 118, 201, 117, 219, 121, 233, 110, 205, 104, 230, 103, 320, 89, 3, 88, 20, 87, 38, 81, 11, 76, 199, 75, 19, 72, 225, 83, 41, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 92, 208, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 49, 211, 48, 200, 47, 232, 59, 218, 45, 213, 43, 210, 42, 235, 41, 217, 40, 214, 50, 222, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 44, 221, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 17, 106, 16, 136, 27, 45, 13, 107, 12, 156, 28, 62, 39, 204, 46, 223, 51, 229, 18, 33, 10, 237, 15, 141, 14, 117],
	#[111, 319, 112, 242, 10, 244],
	#[],
	#[],
	#[155, 226, 138, 234, 134, 227, 125, 8, 124, 215, 121, 233, 120, 203, 119, 216, 118, 201, 117, 219, 110, 205, 104, 230, 103, 258, 92, 208, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 199, 75, 19, 72, 225, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 214, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 12, 156, 10, 237, 13, 107],
	#[22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 16, 136, 26, 29, 27, 45, 13, 107, 51, 229, 39, 204, 57, 231, 17, 106, 35, 228, 23, 49, 38, 198, 34, 50, 47, 232, 15, 141, 36, 238, 12, 156, 46, 223, 44, 221, 69, 55, 14, 117, 10, 237, 155, 226, 134, 227, 125, 8, 124, 215, 121, 233, 120, 203, 119, 216, 118, 201, 117, 219, 138, 234, 110, 205, 104, 230, 103, 318, 92, 208, 89, 3, 88, 20, 87, 38, 81, 11, 76, 199, 75, 19, 72, 225, 83, 41, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 218, 55, 206, 54, 220, 53, 209, 52, 197, 50, 222, 49, 211, 48, 200, 43, 210, 37, 207, 40, 214, 41, 217, 33, 28, 32, 236, 42, 235, 30, 27, 29, 12, 28, 62, 45, 213],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[138, 234, 125, 8, 124, 215, 121, 233, 120, 203, 119, 216, 118, 201, 117, 219, 134, 227, 110, 248, 109, 249, 92, 208, 89, 3, 88, 20, 87, 38, 76, 199, 75, 19, 72, 225, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 214, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 27, 45, 26, 29, 21, 15, 20, 4, 19, 57, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 237],
	#[],
	#[],
	#[],
	#[],
	#[155, 226, 138, 234, 134, 227, 125, 8, 124, 215, 121, 233, 120, 203, 119, 216, 118, 201, 104, 230, 103, 246, 117, 219, 92, 208, 89, 3, 88, 20, 110, 205, 83, 41, 76, 199, 81, 11, 72, 225, 69, 55, 66, 47, 65, 32, 57, 231, 48, 200, 68, 54, 62, 212, 17, 106, 52, 197, 87, 38, 64, 22, 75, 19, 53, 209, 59, 218, 38, 198, 43, 210, 40, 214, 67, 36, 10, 237, 32, 236, 36, 238, 50, 222, 21, 15, 26, 29, 39, 204, 37, 207, 49, 211, 41, 217, 33, 28, 44, 221, 13, 107, 18, 33, 51, 229, 29, 12, 16, 136, 35, 228, 45, 213, 55, 206, 19, 57, 61, 202, 42, 235, 54, 220, 30, 27, 28, 62, 34, 50, 46, 223, 22, 31, 20, 4, 27, 45, 12, 156, 47, 232, 14, 117, 23, 49, 15, 141, 63, 224],
	#[],
	#[112, 242, 111, 243, 10, 244],
	#[],
	#[],
	#[],
	#[155, 226, 138, 234, 134, 227, 125, 8, 124, 215, 121, 233, 120, 203, 119, 216, 118, 201, 117, 219, 110, 205, 104, 230, 103, 241, 92, 208, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 199, 75, 19, 72, 225, 69, 55, 68, 54, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 214, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 67, 36, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 28, 62, 10, 237],
	#[],
	#[56, 240],
	#[],
	#[],
	#[],
	#[],
	#[155, 226, 138, 234, 134, 227, 125, 8, 124, 215, 121, 233, 120, 203, 119, 216, 118, 201, 117, 219, 110, 205, 104, 230, 103, 245, 92, 208, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 199, 75, 19, 72, 225, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 214, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 237],
	#[],
	#[58, 247],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[155, 255, 138, 234, 134, 227, 120, 203, 119, 216, 118, 201, 114, 250, 106, 251, 105, 315, 117, 252, 92, 208, 89, 3, 83, 41, 76, 199, 81, 11, 72, 225, 69, 55, 66, 47, 65, 32, 63, 224, 62, 212, 52, 197, 57, 231, 48, 200, 61, 202, 68, 54, 16, 136, 20, 4, 34, 50, 49, 211, 42, 235, 55, 206, 53, 209, 59, 218, 44, 221, 41, 217, 40, 253, 67, 36, 45, 213, 32, 236, 51, 229, 36, 238, 50, 222, 21, 15, 17, 106, 39, 204, 37, 207, 43, 210, 29, 12, 33, 28, 38, 198, 14, 117, 19, 57, 23, 49, 12, 156, 47, 232, 75, 19, 15, 141, 13, 107, 46, 223, 22, 31, 18, 33, 35, 228, 28, 62, 54, 220],
	#[155, 255, 138, 234, 134, 227, 120, 203, 119, 216, 118, 201, 117, 252, 114, 250, 106, 251, 105, 314, 92, 208, 89, 3, 83, 41, 81, 11, 76, 199, 75, 19, 72, 225, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 253, 39, 204, 38, 198, 37, 207, 36, 238, 34, 50, 33, 28, 32, 236, 29, 12, 28, 62, 35, 228, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 22, 31, 12, 156, 23, 49],
	#[],
	#[138, 234, 134, 227, 120, 203, 119, 216, 118, 201, 117, 252, 114, 256, 113, 257, 92, 208, 89, 3, 76, 199, 75, 19, 72, 225, 68, 54, 67, 36, 66, 47, 65, 32, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 253, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 21, 15, 20, 4, 19, 57, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156],
	#[],
	#[],
	#[60, 259],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[189, 108, 184, 125, 190, 153, 185, 115, 188, 122, 182, 351, 187, 139, 183, 143, 91, 138, 77, 155, 74, 154, 73, 109, 72, 137, 69, 146, 68, 148, 67, 132, 66, 144, 65, 128, 64, 119, 63, 135, 62, 116, 61, 104, 59, 133, 57, 150, 55, 111, 53, 113, 52, 99, 51, 147, 50, 131, 48, 102, 47, 152, 46, 134, 45, 118, 44, 121, 43, 129, 42, 100, 41, 123, 40, 120, 39, 95, 38, 97, 37, 96, 36, 93, 35, 145, 33, 103, 32, 157, 31, 140, 30, 124, 29, 110, 28, 160, 27, 142, 26, 126, 25, 112, 24, 101, 23, 149, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 158],
	#[],
	#[],
	#[],
	#[181, 265, 180, 271, 179, 283, 172, 282, 166, 273, 165, 350, 155, 275, 138, 234, 134, 276, 125, 8, 124, 269, 121, 279, 120, 262, 118, 261, 92, 266, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 260, 75, 19, 72, 274, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 270, 57, 278, 55, 264, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 267, 44, 272, 43, 263, 42, 280, 41, 217, 40, 268, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 281],
	#[173, 349, 174, 287, 10, 288],
	#[190, 153, 189, 108, 188, 122, 187, 139, 185, 115, 184, 125, 183, 143, 182, 295, 91, 138, 77, 155, 74, 154, 73, 109, 72, 137, 69, 146, 68, 148, 67, 132, 66, 144, 65, 128, 64, 119, 63, 135, 62, 116, 61, 104, 59, 133, 57, 150, 55, 111, 53, 113, 52, 99, 51, 147, 50, 131, 48, 102, 47, 152, 46, 134, 45, 118, 43, 129, 42, 100, 41, 123, 40, 120, 39, 95, 38, 97, 37, 96, 36, 93, 35, 145, 33, 103, 32, 157, 31, 140, 30, 124, 29, 110, 28, 160, 26, 126, 25, 112, 24, 101, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130, 17, 106, 16, 136, 15, 141, 12, 156, 27, 142, 23, 149, 14, 117, 10, 158, 13, 107, 44, 121],
	#[],
	#[],
	#[],
	#[],
	#[181, 265, 180, 271, 179, 283, 172, 298, 171, 297, 138, 234, 134, 276, 125, 8, 124, 269, 121, 279, 120, 262, 118, 261, 92, 266, 89, 3, 88, 20, 87, 38, 76, 260, 75, 19, 72, 274, 68, 54, 67, 36, 66, 47, 65, 32, 63, 224, 62, 212, 61, 202, 59, 270, 57, 278, 64, 22, 53, 209, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 267, 44, 272, 43, 263, 42, 280, 41, 217, 40, 268, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 54, 220, 30, 27, 27, 45, 26, 29, 21, 15, 20, 4, 19, 57, 52, 197, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 281, 55, 264],
	#[],
	#[156, 364, 31, 339],
	#[91, 138, 77, 155, 74, 154, 73, 109, 72, 137, 69, 146, 68, 148, 67, 132, 66, 144, 65, 128, 64, 119, 63, 135, 62, 116, 61, 104, 59, 133, 57, 150, 55, 111, 53, 113, 52, 99, 51, 147, 50, 131, 48, 102, 47, 152, 46, 134, 45, 118, 44, 121, 43, 129, 42, 100, 41, 123, 40, 120, 39, 95, 38, 97, 37, 96, 36, 93, 35, 145, 33, 103, 32, 157, 31, 140, 30, 124, 29, 110, 28, 160, 27, 142, 26, 126, 25, 112, 24, 101, 23, 149, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 158, 189, 108, 188, 122, 187, 139, 185, 115, 184, 125, 183, 143, 182, 290, 190, 153],
	#[173, 286, 174, 287, 10, 288],
	#[],
	#[181, 265, 180, 271, 179, 283, 172, 282, 166, 273, 165, 285, 155, 275, 138, 234, 134, 276, 125, 8, 124, 269, 121, 279, 120, 262, 118, 261, 92, 266, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 260, 75, 19, 72, 274, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 270, 57, 278, 55, 264, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 267, 44, 272, 43, 263, 42, 280, 41, 217, 40, 268, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 281],
	#[],
	#[181, 265, 179, 283, 172, 282, 166, 273, 165, 284, 155, 275, 138, 234, 134, 276, 180, 271, 125, 8, 124, 269, 121, 279, 120, 262, 118, 261, 92, 266, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 260, 75, 19, 72, 274, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 270, 57, 278, 55, 264, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 267, 44, 272, 43, 263, 42, 280, 41, 217, 40, 268, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 281, 28, 62],
	#[],
	#[],
	#[],
	#[],
	#[181, 265, 180, 271, 179, 283, 172, 282, 166, 273, 165, 289, 155, 275, 138, 234, 134, 276, 125, 8, 124, 269, 121, 279, 120, 262, 118, 261, 92, 266, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 260, 75, 19, 72, 274, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 270, 57, 278, 55, 264, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 267, 44, 272, 43, 263, 42, 280, 41, 217, 40, 268, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 281],
	#[],
	#[58, 291],
	#[],
	#[59, 270, 181, 294],
	#[],
	#[],
	#[60, 296],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[86, 311, 85, 304, 78, 309, 34, 308, 29, 312, 25, 307, 23, 305, 21, 310, 20, 306],
	#[128, 303, 126, 302, 33, 299],
	#[],
	#[155, 226, 138, 234, 134, 227, 125, 8, 124, 215, 121, 233, 120, 203, 119, 216, 118, 201, 117, 219, 110, 205, 104, 230, 103, 362, 92, 208, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 199, 75, 19, 72, 225, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 214, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 237],
	#[],
	#[],
	#[],
	#[],
	#[77, 353, 74, 154, 73, 109, 33, 103, 32, 157, 31, 140, 30, 124, 29, 110, 28, 160, 27, 142, 26, 126, 25, 112, 24, 101, 23, 149, 22, 127, 21, 114, 20, 105, 19, 151, 18, 130],
	#[],
	#[155, 255, 138, 234, 134, 227, 120, 203, 119, 216, 118, 201, 117, 252, 114, 250, 106, 251, 105, 313, 92, 208, 89, 3, 83, 41, 81, 11, 76, 199, 75, 19, 72, 225, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 224, 62, 212, 61, 202, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 253, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 59, 218, 12, 156],
	#[],
	#[],
	#[],
	#[],
	#[56, 317],
	#[],
	#[],
	#[],
	#[],
	#[181, 265, 180, 271, 179, 283, 172, 282, 166, 273, 165, 347, 155, 275, 138, 234, 134, 276, 125, 8, 124, 269, 121, 279, 120, 262, 118, 261, 92, 266, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 260, 75, 19, 72, 274, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 270, 57, 278, 55, 264, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 267, 44, 272, 43, 263, 42, 280, 41, 217, 40, 268, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 26, 29, 27, 45, 13, 107, 12, 156, 10, 281, 14, 117, 15, 141],
	#[40, 345],
	#[],
	#[155, 226, 138, 234, 134, 227, 125, 8, 124, 215, 121, 233, 120, 203, 119, 216, 118, 201, 117, 219, 110, 205, 104, 230, 103, 343, 92, 208, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 199, 75, 19, 72, 225, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 214, 39, 204, 38, 198, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 37, 207, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 237, 18, 33, 28, 62],
	#[155, 226, 138, 234, 134, 227, 125, 8, 124, 215, 121, 233, 120, 203, 119, 216, 118, 201, 117, 219, 110, 205, 104, 230, 103, 326, 92, 208, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 199, 75, 19, 72, 225, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 40, 214, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 237],
	#[129, 328, 31, 327],
	#[85, 329, 83, 333, 81, 334, 79, 335, 76, 332, 75, 19, 34, 50, 33, 28, 29, 12, 28, 62, 22, 31, 21, 15, 20, 331, 18, 33, 25, 307, 23, 330],
	#[],
	#[83, 333, 81, 334, 79, 336, 76, 332, 75, 19, 34, 50, 33, 28, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 20, 4, 18, 33],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[156, 338, 31, 339],
	#[],
	#[83, 340, 81, 341, 80, 342, 29, 12, 28, 62, 23, 49, 22, 31, 18, 33],
	#[],
	#[],
	#[],
	#[129, 344, 31, 327],
	#[],
	#[146, 325, 123, 322, 122, 346, 83, 321, 81, 324, 76, 2, 75, 19, 53, 16, 34, 50, 33, 28, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 20, 4, 18, 33],
	#[],
	#[129, 348, 31, 327],
	#[],
	#[],
	#[],
	#[56, 352],
	#[],
	#[181, 265, 180, 271, 179, 283, 172, 282, 166, 273, 165, 354, 155, 275, 138, 234, 134, 276, 125, 8, 124, 269, 121, 279, 120, 262, 118, 261, 92, 266, 89, 3, 88, 20, 83, 41, 81, 11, 76, 260, 75, 19, 72, 274, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 224, 62, 212, 61, 202, 59, 270, 87, 38, 57, 278, 55, 264, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 267, 44, 272, 43, 263, 42, 280, 41, 217, 40, 268, 39, 204, 38, 198, 37, 207, 36, 238, 35, 228, 34, 50, 33, 28, 32, 236, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 10, 281],
	#[194, 356, 31, 355],
	#[83, 333, 81, 334, 79, 360, 78, 358, 76, 332, 75, 19, 34, 357, 33, 28, 29, 359, 23, 49, 22, 31, 21, 15, 20, 4, 18, 33, 28, 62],
	#[],
	#[],
	#[83, 333, 81, 334, 75, 19, 76, 332, 79, 361, 34, 50, 33, 28, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 20, 4, 18, 33],
	#[],
	#[],
	#[],
	#[129, 363, 31, 327],
	#[],
	#[],
	#[35, 52, 21, 15, 20, 4, 90, 53, 81, 11, 41, 26, 42, 60, 18, 33, 12, 42, 29, 12, 67, 36, 65, 32, 130, 61, 61, 6, 68, 54, 19, 57, 83, 41, 43, 17, 53, 16, 22, 31, 13, 65, 69, 55, 75, 19, 62, 21, 55, 13, 66, 47, 23, 49, 33, 28, 28, 62, 34, 50, 36, 63, 63, 40, 146, 56, 155, 34, 153, 366, 136, 25, 137, 10, 138, 59, 134, 51, 154, 44, 135, 43, 89, 3, 76, 2],
	#[],
	#[56, 368],
	#[],
	#[],
	#[155, 34, 154, 71, 152, 74, 151, 76, 146, 56, 138, 59, 137, 10, 136, 25, 135, 43, 134, 51, 133, 371, 130, 61, 90, 53, 89, 3, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 55, 13, 53, 16, 43, 17, 42, 60, 41, 73, 36, 63, 35, 52, 34, 50, 33, 28, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 13, 65, 12, 42],
	#[],
	#[],
	#[],
	#[2, 375],
	#[],
	#[47, 379],
	#[2, 378],
	#[],
	#[155, 34, 147, 380, 146, 56, 138, 59, 137, 10, 136, 25, 135, 43, 134, 51, 130, 381, 89, 3, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 55, 13, 53, 16, 43, 17, 42, 60, 34, 50, 33, 28, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 13, 65, 12, 42],
	#[],
	#[57, 67, 55, 68, 46, 66],
	#[2, 383],
	#[],
	#[2, 385],
	#[],
	#[155, 396, 138, 234, 120, 203, 119, 216, 118, 201, 117, 394, 116, 395, 134, 227, 102, 393, 92, 208, 108, 397, 89, 3, 83, 41, 81, 11, 76, 199, 72, 225, 69, 55, 63, 224, 75, 19, 59, 218, 65, 32, 57, 231, 49, 211, 48, 200, 47, 232, 46, 223, 43, 210, 61, 202, 41, 217, 39, 204, 38, 198, 37, 207, 36, 238, 52, 197, 53, 209, 33, 28, 32, 236, 54, 220, 29, 12, 67, 36, 44, 221, 45, 213, 51, 229, 34, 50, 22, 31, 21, 15, 42, 235, 18, 33, 50, 222, 62, 212, 15, 141, 14, 117, 13, 107, 12, 156, 16, 136, 17, 106, 35, 228, 20, 4, 55, 206, 23, 49, 19, 57, 66, 47, 68, 54, 28, 62],
	#[40, 391],
	#[],
	#[2, 390],
	#[],
	#[101, 387, 100, 392, 41, 386],
	#[],
	#[],
	#[155, 396, 138, 234, 134, 227, 120, 203, 119, 216, 118, 201, 117, 394, 116, 395, 108, 400, 107, 401, 92, 208, 89, 3, 81, 11, 68, 54, 66, 47, 72, 225, 33, 28, 43, 210, 62, 212, 38, 198, 49, 211, 13, 107, 12, 156, 16, 136, 39, 204, 65, 32, 76, 199, 45, 213, 41, 217, 63, 224, 61, 202, 48, 200, 21, 15, 52, 197, 46, 223, 42, 235, 55, 206, 53, 209, 50, 222, 44, 221, 83, 41, 29, 12, 34, 50, 67, 36, 57, 231, 15, 141, 54, 220, 75, 19, 35, 228, 32, 236, 59, 218, 37, 207, 28, 62, 22, 31, 17, 106, 18, 33, 51, 229, 20, 4, 14, 117, 19, 57, 69, 55, 36, 238, 23, 49, 47, 232],
	#[],
	#[138, 234, 120, 203, 119, 216, 118, 201, 117, 394, 116, 398, 115, 399, 134, 227, 92, 208, 89, 3, 76, 199, 75, 19, 72, 225, 68, 54, 67, 36, 66, 47, 65, 32, 63, 224, 62, 212, 61, 202, 59, 218, 57, 231, 55, 206, 54, 220, 53, 209, 52, 197, 51, 229, 50, 222, 49, 211, 48, 200, 47, 232, 46, 223, 45, 213, 44, 221, 43, 210, 42, 235, 41, 217, 38, 198, 37, 207, 36, 238, 32, 236, 33, 28, 34, 50, 35, 228, 21, 15, 20, 4, 19, 57, 17, 106, 16, 136, 15, 141, 14, 117, 13, 107, 12, 156, 39, 204],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[144, 407, 10, 408],
	#[2, 406],
	#[],
	#[],
	#[155, 34, 154, 44, 153, 5, 150, 48, 149, 64, 146, 56, 138, 59, 137, 10, 136, 25, 135, 43, 134, 51, 130, 61, 125, 8, 124, 24, 121, 58, 98, 409, 90, 53, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 40, 62, 21, 61, 6, 55, 13, 43, 17, 42, 60, 41, 26, 36, 63, 35, 52, 53, 16, 33, 28, 30, 27, 29, 12, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 19, 57, 18, 33, 13, 65, 12, 42, 34, 50, 20, 4, 28, 62],
	#[],
	#[],
	#[],
	#[154, 44, 150, 48, 155, 34, 153, 5, 138, 59, 137, 10, 146, 56, 134, 51, 136, 25, 149, 434, 130, 61, 90, 53, 83, 41, 89, 3, 135, 43, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 81, 11, 55, 13, 43, 17, 42, 60, 41, 26, 35, 52, 53, 16, 33, 28, 29, 12, 22, 31, 21, 15, 20, 4, 18, 33, 13, 65, 12, 42, 19, 57, 36, 63, 34, 50, 23, 49, 28, 62],
	#[48, 433],
	#[162, 443, 160, 441, 155, 34, 153, 5, 149, 64, 146, 56, 154, 44, 138, 59, 135, 43, 136, 25, 137, 10, 150, 48, 125, 8, 124, 24, 121, 58, 130, 61, 134, 51, 98, 403, 95, 427, 97, 442, 89, 3, 87, 38, 76, 2, 81, 11, 41, 26, 22, 31, 20, 4, 43, 17, 53, 16, 88, 20, 66, 47, 10, 440, 64, 22, 30, 27, 19, 57, 33, 28, 61, 6, 21, 15, 27, 45, 68, 54, 42, 60, 55, 13, 18, 33, 23, 49, 62, 21, 83, 41, 29, 12, 34, 50, 67, 36, 65, 32, 36, 63, 75, 19, 35, 52, 26, 29, 28, 62, 63, 40, 13, 65, 69, 55, 90, 53, 12, 42],
	#[155, 34, 154, 44, 153, 5, 150, 48, 146, 56, 162, 429, 138, 59, 137, 10, 136, 25, 135, 43, 130, 61, 125, 8, 124, 24, 121, 58, 98, 403, 97, 428, 95, 427, 89, 3, 55, 13, 75, 19, 88, 20, 35, 52, 33, 28, 81, 11, 83, 41, 43, 17, 29, 12, 67, 36, 41, 26, 76, 2, 64, 22, 61, 6, 22, 31, 21, 15, 20, 4, 18, 33, 68, 54, 62, 21, 27, 45, 90, 53, 12, 42, 66, 47, 10, 426, 65, 32, 30, 27, 19, 57, 63, 40, 26, 29, 36, 63, 149, 64, 53, 16, 69, 55, 34, 50, 42, 60, 28, 62, 13, 65, 87, 38, 23, 49, 134, 51],
	#[],
	#[],
	#[2, 425],
	#[164, 424, 48, 423],
	#[40, 421],
	#[155, 34, 153, 5, 149, 420, 148, 422, 146, 56, 154, 44, 138, 59, 135, 43, 136, 25, 137, 10, 150, 48, 130, 61, 134, 51, 90, 53, 89, 3, 81, 11, 83, 41, 67, 36, 62, 21, 41, 26, 76, 2, 23, 49, 35, 52, 21, 15, 20, 4, 36, 63, 18, 33, 68, 54, 65, 32, 53, 16, 12, 42, 28, 62, 29, 12, 42, 60, 19, 57, 63, 40, 61, 6, 34, 50, 66, 47, 22, 31, 33, 28, 55, 13, 13, 65, 69, 55, 43, 17, 75, 19],
	#[],
	#[],
	#[],
	#[],
	#[155, 34, 154, 44, 153, 5, 150, 48, 149, 420, 148, 413, 138, 59, 137, 10, 136, 25, 146, 56, 134, 51, 130, 61, 163, 439, 135, 43, 89, 3, 81, 11, 90, 53, 68, 54, 62, 21, 66, 47, 32, 419, 41, 26, 12, 42, 29, 12, 35, 52, 19, 57, 42, 60, 55, 412, 28, 62, 69, 55, 83, 41, 76, 2, 34, 50, 67, 36, 65, 32, 20, 4, 18, 33, 21, 15, 43, 17, 53, 16, 22, 31, 13, 65, 23, 49, 33, 28, 75, 19, 36, 63, 61, 6, 63, 40],
	#[],
	#[144, 407, 10, 430],
	#[],
	#[163, 431, 155, 34, 154, 44, 153, 5, 150, 48, 149, 432, 148, 413, 146, 56, 138, 59, 137, 10, 136, 25, 135, 43, 134, 51, 130, 61, 125, 8, 124, 24, 121, 58, 98, 409, 90, 53, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 2, 75, 19, 67, 36, 62, 21, 61, 6, 64, 22, 65, 32, 55, 412, 53, 16, 68, 54, 69, 55, 66, 47, 43, 17, 42, 60, 41, 26, 36, 63, 35, 52, 63, 40, 33, 28, 32, 419, 30, 27, 29, 12, 28, 62, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 20, 4, 19, 57, 18, 33, 13, 65, 12, 42, 34, 50],
	#[],
	#[40, 421],
	#[],
	#[40, 435, 56, 368],
	#[155, 34, 154, 44, 153, 5, 150, 48, 149, 420, 148, 436, 146, 56, 138, 59, 136, 25, 135, 43, 134, 51, 137, 10, 130, 61, 90, 53, 89, 3, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 55, 13, 43, 17, 42, 60, 41, 26, 36, 63, 35, 52, 53, 16, 33, 28, 29, 12, 28, 62, 23, 49, 22, 31, 21, 15, 62, 21, 12, 42, 18, 33, 61, 6, 13, 65, 19, 57, 34, 50, 20, 4],
	#[56, 437],
	#[48, 438],
	#[],
	#[],
	#[163, 415, 161, 417, 159, 414, 158, 446, 155, 34, 154, 44, 153, 5, 150, 48, 149, 420, 148, 413, 146, 56, 138, 59, 137, 10, 136, 25, 135, 43, 134, 51, 130, 61, 90, 53, 89, 3, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 63, 40, 62, 21, 61, 6, 55, 412, 43, 17, 42, 60, 41, 26, 36, 63, 35, 52, 53, 16, 33, 28, 32, 419, 29, 12, 23, 49, 22, 31, 21, 15, 12, 42, 19, 57, 18, 33, 34, 50, 20, 4, 28, 62, 13, 65],
	#[],
	#[144, 407, 10, 444],
	#[],
	#[163, 415, 161, 417, 159, 414, 158, 445, 155, 34, 154, 44, 153, 5, 150, 48, 149, 432, 148, 413, 146, 56, 138, 59, 137, 10, 136, 25, 135, 43, 134, 51, 130, 61, 125, 8, 124, 24, 121, 58, 98, 409, 90, 53, 89, 3, 88, 20, 87, 38, 83, 41, 81, 11, 76, 2, 75, 19, 69, 55, 68, 54, 67, 36, 66, 47, 65, 32, 64, 22, 63, 40, 62, 21, 61, 6, 55, 412, 53, 16, 43, 17, 42, 60, 41, 26, 36, 63, 35, 52, 34, 50, 33, 28, 32, 419, 30, 27, 29, 12, 27, 45, 26, 29, 23, 49, 22, 31, 21, 15, 12, 42, 18, 33, 19, 57, 28, 62, 13, 65, 20, 4],
	#[],
	#[],
	#[2, 448],
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
		dylan-parser-action76,
		dylan-parser-action0,
		dylan-parser-action78,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action81,
		dylan-parser-action82,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action89,
		dylan-parser-action90,
		dylan-parser-action0,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action90,
		dylan-parser-action90,
		dylan-parser-action90,
		dylan-parser-action90,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action90,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action90,
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
		dylan-parser-action142,
		dylan-parser-action143,
		dylan-parser-action144,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action150,
		dylan-parser-action150,
		dylan-parser-action0,
		dylan-parser-action153,
		dylan-parser-action154,
		dylan-parser-action155,
		dylan-parser-action156,
		dylan-parser-action157,
		dylan-parser-action0,
		dylan-parser-action159,
		dylan-parser-action160,
		dylan-parser-action161,
		dylan-parser-action0,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action153,
		dylan-parser-action90,
		dylan-parser-action167,
		dylan-parser-action168,
		dylan-parser-action169,
		dylan-parser-action170,
		dylan-parser-action171,
		dylan-parser-action172,
		dylan-parser-action0,
		dylan-parser-action174,
		dylan-parser-action81,
		dylan-parser-action82,
		dylan-parser-action177,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action180,
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
		dylan-parser-action195,
		dylan-parser-action196,
		dylan-parser-action197,
		dylan-parser-action198,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action174,
		dylan-parser-action81,
		dylan-parser-action82,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action153,
		dylan-parser-action213,
		dylan-parser-action0,
		dylan-parser-action215,
		dylan-parser-action0,
		dylan-parser-action153,
		dylan-parser-action154,
		dylan-parser-action219,
		dylan-parser-action220,
		dylan-parser-action221,
		dylan-parser-action219,
		dylan-parser-action0,
		dylan-parser-action221,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action228,
		dylan-parser-action229,
		dylan-parser-action230,
		dylan-parser-action231,
		dylan-parser-action232,
		dylan-parser-action9,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action236,
		dylan-parser-action0,
		dylan-parser-action238,
		dylan-parser-action239,
		dylan-parser-action90,
		dylan-parser-action241,
		dylan-parser-action0,
		dylan-parser-action236,
		dylan-parser-action153,
		dylan-parser-action153,
		dylan-parser-action79,
		dylan-parser-action247,
		dylan-parser-action248,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action153,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action90,
		dylan-parser-action90,
		dylan-parser-action90,
		dylan-parser-action90,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action90,
		dylan-parser-action90,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action90,
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
		dylan-parser-action142,
		dylan-parser-action143,
		dylan-parser-action0,
		dylan-parser-action144,
		dylan-parser-action79,
		dylan-parser-action0,
		dylan-parser-action153,
		dylan-parser-action90,
		dylan-parser-action0,
		dylan-parser-action236,
		dylan-parser-action302,
		dylan-parser-action302,
		dylan-parser-action302,
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
		dylan-parser-action142,
		dylan-parser-action143,
		dylan-parser-action144,
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
		dylan-parser-action332,
		dylan-parser-action333,
		dylan-parser-action333,
		dylan-parser-action0,
		dylan-parser-action336,
		dylan-parser-action337,
		dylan-parser-action338,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action9,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action344,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action349,
		dylan-parser-action167,
		dylan-parser-action168,
		dylan-parser-action169),
  action-nargs-table: #[1, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 2, 0, 1, 1, 3, 1, 1, 1, 0, 1, 1, 3, 2, 1, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 2, 2, 2, 2, 2, 0, 1, 2, 0, 1, 2, 2, 0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1, 1, 1, 1, 1, 2, 2, 1, 1, 3, 3, 3, 3, 1, 5, 4, 6, 1, 0, 1, 1, 2, 1, 2, 3, 4, 4, 3, 1, 1, 1, 3, 2, 1, 1, 4, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 5, 3, 3, 1, 1, 0, 1, 1, 1, 3, 1, 1, 0, 1, 0, 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 1, 2, 3, 3, 3, 2, 2, 0, 1, 2, 1, 3, 4, 2, 3, 1, 2, 1, 1, 0, 2, 6, 2, 0, 1, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 2, 2, 2, 2, 2, 0, 1, 2, 0, 1, 2, 2, 0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 3, 0, 1, 1, 2, 1, 2, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 4, 2, 2, 1, 2, 3, 2, 0, 1, 2, 0, 1, 2, 1, 1, 1, 1, 3, 1, 2, 3],
  action-nt-table: #[70, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 72, 72, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 74, 74, 74, 75, 75, 76, 76, 76, 77, 77, 78, 78, 79, 79, 79, 80, 80, 81, 81, 81, 82, 82, 83, 83, 84, 84, 85, 85, 85, 86, 87, 88, 89, 90, 90, 91, 91, 91, 91, 92, 92, 92, 92, 92, 93, 94, 94, 95, 96, 96, 97, 97, 98, 98, 98, 99, 99, 100, 100, 101, 102, 103, 103, 104, 104, 105, 105, 106, 106, 107, 107, 108, 108, 109, 109, 110, 110, 110, 110, 110, 111, 111, 112, 113, 113, 114, 114, 115, 115, 116, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 119, 119, 119, 120, 120, 120, 120, 120, 121, 121, 121, 122, 122, 123, 123, 123, 124, 125, 125, 125, 126, 127, 127, 128, 128, 129, 129, 129, 130, 130, 130, 130, 131, 132, 132, 133, 133, 133, 134, 135, 135, 135, 135, 135, 135, 135, 136, 136, 136, 136, 136, 136, 137, 137, 138, 138, 138, 138, 138, 139, 139, 140, 141, 141, 142, 142, 143, 143, 144, 144, 145, 145, 146, 146, 147, 148, 148, 149, 150, 150, 151, 152, 152, 153, 153, 154, 154, 155, 155, 155, 156, 156, 157, 157, 158, 158, 159, 159, 160, 160, 160, 161, 162, 162, 162, 163, 163, 163, 164, 164, 165, 165, 166, 166, 167, 167, 168, 168, 169, 169, 170, 170, 171, 171, 172, 172, 172, 172, 172, 173, 173, 174, 175, 175, 176, 176, 177, 177, 178, 179, 179, 179, 179, 179, 179, 179, 179, 179, 179, 179, 180, 180, 180, 181, 182, 182, 183, 183, 183, 183, 184, 184, 184, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 186, 186, 187, 187, 187, 188, 188, 188, 188, 188, 188, 188, 189, 189, 190, 191, 191, 192, 193, 193, 193, 193, 193, 194, 194, 194]);

// eof
