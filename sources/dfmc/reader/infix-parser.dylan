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

define function dylan-parser-action176 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6) => (value)
  begin
           let getter-name
             = if (size(arg$3) = 1) #"element" else #"aref" end;
           let default-symbol-fragment
             = make(<keyword-syntax-symbol-fragment>,
                    record: fragment-record(arg$5),
                    source-position: fragment-source-position(arg$5),
                    value: #"default");
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
                arguments: concatenate(pair(arg$1, arg$3),
                                       list(default-symbol-fragment, arg$6)));
         end
end dylan-parser-action176;

define function dylan-parser-action177 (arg$1, arg$2, arg$3) => (value)
  make(<dot-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              function: arg$3, arguments: list(arg$1))
end dylan-parser-action177;

define function dylan-parser-action179 (arg$1) => (value)
  elements(arg$1)
end dylan-parser-action179;

define function dylan-parser-action182 (arg$1, arg$2) => (value)
  list(arg$1, arg$2)
end dylan-parser-action182;

define function dylan-parser-action185 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<function-macro-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$4),
              macro: arg$1,
              body-fragment: arg$3)
end dylan-parser-action185;

define function dylan-parser-action200 (arg$1, arg$2) => (value)
  make(<string-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              value: concatenate(fragment-value(arg$1), fragment-value(arg$2)))
end dylan-parser-action200;

define function dylan-parser-action201 (arg$1, arg$2, arg$3, arg$4, arg$5) => (value)
  make(<improper-list-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$5),
              elements:        arg$2,
              improper-tail:   arg$4)
end dylan-parser-action201;

define function dylan-parser-action202 (arg$1, arg$2, arg$3) => (value)
  make(<proper-list-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              elements:        arg$2)
end dylan-parser-action202;

define function dylan-parser-action203 (arg$1, arg$2, arg$3) => (value)
  make(<vector-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              elements:        arg$2)
end dylan-parser-action203;

define function dylan-parser-action220 (arg$1, arg$2, arg$3) => (value)
  list(arg$1, arg$2, arg$3)
end dylan-parser-action220;

define function dylan-parser-action222 (arg$1) => (value)
  make-variable-name-like
           (arg$1,
            record:          fragment-record(arg$1),
            source-position: fragment-source-position(arg$1),
            name: #"...")
end dylan-parser-action222;

define function dylan-parser-action226 (arg$1) => (value)
  binop-fragment(arg$1)
end dylan-parser-action226;

define function dylan-parser-action227 (arg$1) => (value)
  (arg$1)
end dylan-parser-action227;

define function dylan-parser-action228 (arg$1, arg$2, arg$3) => (value)
  append-binop!(arg$1, arg$2, arg$3)
end dylan-parser-action228;

define function dylan-parser-action235 (arg$1, arg$2) => (value)
  make(<unary-operator-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              function: arg$1,
              arguments: list(arg$2))
end dylan-parser-action235;

define function dylan-parser-action236 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-statement-tail(arg$1, arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
                source-position: position-between(arg$1, arg$3),
                macro: arg$1,
                body-fragment: arg$2);
         end
end dylan-parser-action236;

define function dylan-parser-action238 (arg$1, arg$2, arg$3) => (value)
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
end dylan-parser-action238;

define function dylan-parser-action239 (arg$1, arg$2) => (value)
  arg$2 | arg$1
end dylan-parser-action239;

define function dylan-parser-action243 (arg$1, arg$2) => (value)
  concatenate(arg$1, arg$2)
end dylan-parser-action243;

define function dylan-parser-action245 (arg$1, arg$2, arg$3) => (value)
  concatenate(arg$1, list(arg$2), arg$3)
end dylan-parser-action245;

define function dylan-parser-action246 (arg$1, arg$2, arg$3, arg$4) => (value)
  concatenate(arg$1, list(body-fragment(elements(arg$2))), list(arg$3), arg$4)
end dylan-parser-action246;

define function dylan-parser-action248 (arg$1, arg$2, arg$3) => (value)
  pair(body-fragment(elements(arg$1)), pair(arg$2, arg$3))
end dylan-parser-action248;

define function dylan-parser-action254 (arg$1, arg$2) => (value)
  concatenate(arg$1, list(arg$2))
end dylan-parser-action254;

define function dylan-parser-action255 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6) => (value)
  pair(make(<parens-fragment>,
                   record:           fragment-record(arg$1),
                   source-position:  position-between(arg$1, arg$5),
                   left-delimiter:   arg$1,
                   nested-fragments: pair(arg$2, pair(arg$3, arg$4)),
                   right-delimiter:  arg$5),
              list(arg$6))
end dylan-parser-action255;

define function dylan-parser-action309 (arg$1, arg$2, arg$3) => (value)
  pair(arg$1, concatenate(arg$2, list(arg$3)))
end dylan-parser-action309;

define function dylan-parser-action339 (arg$1, arg$2, arg$3, arg$4) => (value)
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
end dylan-parser-action339;

define function dylan-parser-action340 (arg$1, arg$2) => (value)
  make(<sequence-pattern-variable-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              name:            arg$2,
              separator:       #f)
end dylan-parser-action340;

define function dylan-parser-action343 (arg$1, arg$2) => (value)
  make(<unhygienic-name-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              name:            arg$2)
end dylan-parser-action343;

define function dylan-parser-action344 (arg$1, arg$2, arg$3) => (value)
  make(<template-aux-rule-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              rule-name:       arg$2,
              template:        arg$3)
end dylan-parser-action344;

define function dylan-parser-action345 (arg$1, arg$2) => (value)
  make(<template-macro-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              template:        arg$2)
end dylan-parser-action345;

define function dylan-parser-action351 (arg$1, arg$2) => (value)
  arg$2
end dylan-parser-action351;

define function dylan-parser-action356 (arg$1, arg$2, arg$3) => (value)
  arg$1
end dylan-parser-action356;

define constant dylan-parser :: <parser>
  = make(<parser>,
  action-table:
      #[#[3, -42, 4, -44, 5, -49, 6, -50, 7, -57, 8, -59, 9, -64, 11, -68, 12, -51, 13, -55, 18, -54, 19, -56, 20, -62, 21, -63, 22, -66, 24, -14, 25, -18, 26, -19, 28, -28, 29, -32, 30, -37, 33, -3, 34, -58, 35, -43, 36, -52, 41, -65, 42, -22, 43, -26, 53, -20, 55, -8, 61, -21, 62, -25, 63, -30, 64, -27, 27, -24, 67, -35, 0, -40, 66, -31, 23, -10, 65, -47, 68, -41, 69, -34],
	#[10, -114, 12, -106, 13, -107, 14, -109, 15, -113, 16, -117, 17, -118, 18, -54, 19, -56, 20, -62, 21, -63, 22, -66, 23, -10, 24, -14, 25, -18, 26, -19, 27, -24, 28, -28, 29, -32, 30, -37, 31, 96, 32, -99, 33, -3, 34, -58, 35, -101, 36, -105, 37, -104, 38, -91, 39, -94, 40, -112, 41, -115, 42, -82, 43, -87, 44, -89, 45, -92, 46, -96, 47, -98, 48, -72, 49, -74, 50, -76, 51, -77, 52, -79, 53, -80, 54, -84, 55, -75, 57, -95, 59, -119, 61, -81, 62, -85, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 36, 15, 36, 2, 36, 23, 36, 10, 36, 31, 36, 18, 36, 17, 36, 11, 36, 12, 36, 13, 36, 14, 36, 38, 36, 16, 36, 47, 36, 19, 36, 20, 36, 21, 36, 22, 36, 34, 36, 24, 36, 25, 36, 26, 36, 27, 36, 28, 36, 29, 36, 30, 36, 54, 36, 32, 36, 33, 36, 63, 36, 35, 36, 36, 36, 37, 36, 39, 36, 40, 36, 41, 36, 42, 36, 43, 36, 44, 36, 45, 36, 46, 36, 58, 36, 48, 36, 49, 36, 50, 36, 51, 36, 52, 36, 53, 36, 67, 36, 55, 36, 56, 36, 57, 36, 65, 36, 59, 36, 60, 36, 61, 36, 62, 36, 64, 36, 66, 36, 68, 36, 69, 36],
	#[65535, 178, 2, 178, 10, 178, 11, 178, 36, 178, 37, 178, 38, 178, 39, 178, 40, 178, 46, 178, 48, 178, 55, 178, 56, 178, 57, 178, 58, 178],
	#[65535, 187, 2, 187, 10, 187, 11, 187, 36, 187, 37, 187, 38, 187, 39, 187, 40, 187, 46, 187, 48, 187, 55, 187, 56, 187, 57, 187, 58, 187],
	#[#"eoi", #"accept"],
	#[18, -54, 20, -62, 21, -63, 22, -66, 23, -10, 28, -28, 29, -32, 33, -3, 34, -58, 53, -20],
	#[21, -63, 23, -10, 13, -55, 35, -43, 61, -21, 42, -22, 25, -18, 62, -25, 28, -28, 12, -51, 53, -20, 55, -8, 33, -3, 22, -66, 68, -41, 34, -58, 36, -52, 20, -62, 43, -26, 63, -30, 19, -56, 18, -54, 29, -32, 66, -31, 69, -34, 41, -65, 24, -14, 67, -35, 65, -47],
	#[65535, 221, 31, 221, 14, 221, 38, 221, 17, 221, 20, 221, 22, 221, 46, 221, 13, 221, 15, 221, 28, 221, 30, 221, 54, 221, 23, 221, 10, 221, 19, 221, 36, 221, 2, 221, 33, 221, 11, 221, 50, 221, 18, 221, 27, 221, 44, 221, 53, 221, 24, 221, 37, 221, 39, 221, 26, 221, 21, 221, 52, 221, 61, 221, 16, 221, 45, 221, 47, 221, 25, 221, 29, 221, 58, 221, 62, 221, 59, 221, 12, 221, 35, 221, 55, 221, 42, 221, 51, 221, 68, 221, 34, 221, 32, 221, 48, 221, 43, 221, 63, 221, 56, 221, 49, 221, 57, 221, 40, 221, 66, 221, 69, 221, 41, 221, 64, 221, 67, 221, 65, 221],
	#[65535, 49, 2, 49, 10, 49, 11, 49, 12, 49, 13, 49, 14, 49, 15, 49, 16, 49, 17, 49, 18, 49, 19, 49, 20, 49, 21, 49, 22, 49, 23, 49, 24, 49, 25, 49, 26, 49, 27, 49, 28, 49, 29, 49, 30, 49, 31, 49, 32, 49, 33, 49, 34, 49, 35, 49, 36, 49, 37, 49, 38, 49, 39, 49, 40, 49, 41, 49, 42, 49, 43, 49, 44, 49, 45, 49, 46, 49, 47, 49, 48, 49, 49, 49, 50, 49, 51, 49, 52, 49, 53, 49, 54, 49, 55, 49, 56, 49, 57, 49, 58, 49, 59, 49, 60, 49, 61, 49, 62, 49, 63, 49, 64, 49, 65, 49, 66, 49, 67, 49, 68, 49, 69, 49],
	#[39, 234, 10, 234, 2, 234, 38, 234, 55, -337, 37, 234, 46, -336, 36, 234, 40, 234, 11, 234, 48, 234, 58, 234, 57, -338, 56, 234],
	#[65535, 227, 38, 227, 39, 227, 11, 227, 36, 227, 37, 227, 48, 227, 2, 227, 58, 227, 10, 227, 40, 227, 56, 227],
	#[10, -262, 12, -106, 13, -107, 14, -109, 15, -113, 16, -117, 17, -118, 18, -54, 19, -56, 20, -62, 21, -63, 22, -66, 23, -10, 24, -14, 25, -18, 26, -19, 27, -24, 28, -28, 29, -32, 30, -37, 31, 259, 32, -99, 33, -3, 34, -58, 35, -101, 36, -105, 37, -104, 38, -91, 39, -94, 40, -261, 41, -115, 42, -246, 43, -249, 44, -250, 45, -252, 46, -96, 47, -98, 48, -72, 49, -74, 50, -76, 51, -77, 52, -79, 53, -80, 54, -84, 55, -242, 57, -254, 59, -263, 61, -81, 62, -85, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 67, 55, 67],
	#[65535, 162, 31, 162, 11, 162, 60, 162, 2, 162, 58, 162, 10, 162, 56, 162],
	#[65535, 195, 2, 195, 10, 195, 11, 195, 36, 195, 37, 195, 38, 195, 39, 195, 40, 195, 46, 195, 48, 195, 55, 195, 56, 195, 57, 195, 58, 195],
	#[23, -10, 33, -3, 25, -18, 43, -26, 62, -25, 28, -28, 12, -51, 53, -20, 55, -8, 42, -22, 22, -66, 68, -41, 34, -58, 21, -63, 20, -62, 61, -21, 63, -30, 19, -56, 18, -54, 29, -32, 66, -31, 69, -34, 13, -55, 24, -14, 67, -35, 65, -47],
	#[65535, 66, 55, 66],
	#[65535, 63, 2, 63, 10, 63, 11, 63, 12, 63, 13, 63, 14, 63, 15, 63, 16, 63, 17, 63, 18, 63, 19, 63, 20, 63, 21, 63, 22, 63, 23, 63, 24, 63, 25, 63, 28, 63, 29, 63, 31, 63, 32, 63, 33, 63, 34, 63, 35, 63, 36, 63, 37, 63, 38, 63, 39, 63, 40, 63, 41, 63, 42, 63, 43, 63, 44, 63, 45, 63, 46, 63, 47, 63, 48, 63, 49, 63, 50, 63, 51, 63, 52, 63, 53, 63, 54, 63, 55, 63, 56, 63, 57, 63, 58, 63, 59, 63, 60, 63, 61, 63, 62, 63, 63, 63, 65, 63, 66, 63, 67, 63, 68, 63, 69, 63],
	#[65535, 222, 14, 222, 20, 222, 22, 222, 13, 222, 15, 222, 2, 222, 11, 222, 30, 222, 18, 222, 21, 222, 23, 222, 10, 222, 19, 222, 17, 222, 38, 222, 16, 222, 29, 222, 31, 222, 24, 222, 27, 222, 25, 222, 46, 222, 12, 222, 34, 222, 37, 222, 39, 222, 26, 222, 35, 222, 33, 222, 54, 222, 49, 222, 32, 222, 45, 222, 47, 222, 40, 222, 43, 222, 41, 222, 62, 222, 28, 222, 50, 222, 53, 222, 55, 222, 42, 222, 51, 222, 68, 222, 52, 222, 36, 222, 58, 222, 61, 222, 63, 222, 56, 222, 59, 222, 57, 222, 44, 222, 66, 222, 69, 222, 48, 222, 64, 222, 67, 222, 65, 222],
	#[65535, 193, 2, 193, 10, 193, 11, 193, 36, 193, 37, 193, 38, 193, 39, 193, 40, 193, 46, 193, 48, 193, 55, 193, 56, 193, 57, 193, 58, 193],
	#[65535, 191, 39, 191, 36, 191, 10, 191, 38, 191, 55, 191, 37, 191, 46, 191, 2, 191, 40, 191, 11, 191, 48, 191, 58, 191, 57, 191, 56, 191],
	#[65535, 190, 39, 190, 36, 190, 10, 190, 38, 190, 55, 190, 37, 190, 46, 190, 2, 190, 40, 190, 11, 190, 48, 190, 58, 190, 57, 190, 56, 190],
	#[65535, 64, 18, 64, 20, 64, 21, 64, 22, 64, 23, 64, 28, 64, 29, 64, 33, 64, 34, 64, 53, 64],
	#[65535, 194, 39, 194, 36, 194, 10, 194, 38, 194, 55, 194, 37, 194, 46, 194, 2, 194, 40, 194, 11, 194, 48, 194, 58, 194, 57, 194, 56, 194],
	#[65535, 192, 39, 192, 36, 192, 10, 192, 38, 192, 55, 192, 37, 192, 46, 192, 2, 192, 40, 192, 11, 192, 48, 192, 58, 192, 57, 192, 56, 192],
	#[65535, 156, 2, 156, 10, 156, 11, 156, 31, 156, 56, 156, 58, 156, 60, 156],
	#[65535, 53, 2, 53, 10, 53, 11, 53, 12, 53, 13, 53, 14, 53, 15, 53, 16, 53, 17, 53, 18, 53, 19, 53, 20, 53, 21, 53, 22, 53, 23, 53, 24, 53, 25, 53, 26, 53, 27, 53, 28, 53, 29, 53, 30, 53, 31, 53, 32, 53, 33, 53, 34, 53, 35, 53, 36, 53, 37, 53, 38, 53, 39, 53, 40, 53, 41, 53, 42, 53, 43, 53, 44, 53, 45, 53, 46, 53, 47, 53, 48, 53, 49, 53, 50, 53, 51, 53, 52, 53, 53, 53, 54, 53, 55, 53, 56, 53, 57, 53, 58, 53, 59, 53, 60, 53, 61, 53, 62, 53, 63, 53, 64, 53, 65, 53, 66, 53, 67, 53, 68, 53, 69, 53],
	#[65535, 88, 10, 88, 2, 88, 11, 88],
	#[2, 199, 10, 199, 11, 199, 36, 199, 37, 199, 38, 199, 39, 199, 40, 199, 46, 199, 48, 199, 55, 199, 56, 199, 57, 199, 58, 199, 63, -30],
	#[62, -25, 13, -55, 68, -41, 61, -21, 63, -30, 66, -31, 12, -51, 41, -127, 58, 206, 67, -35, 65, -47],
	#[65535, 54, 2, 54, 10, 54, 11, 54, 12, 54, 13, 54, 14, 54, 15, 54, 16, 54, 17, 54, 18, 54, 19, 54, 20, 54, 21, 54, 22, 54, 23, 54, 24, 54, 25, 54, 26, 54, 27, 54, 28, 54, 29, 54, 30, 54, 31, 54, 32, 54, 33, 54, 34, 54, 35, 54, 36, 54, 37, 54, 38, 54, 39, 54, 40, 54, 41, 54, 42, 54, 43, 54, 44, 54, 45, 54, 46, 54, 47, 54, 48, 54, 49, 54, 50, 54, 51, 54, 52, 54, 53, 54, 54, 54, 55, 54, 56, 54, 57, 54, 58, 54, 59, 54, 60, 54, 61, 54, 62, 54, 63, 54, 64, 54, 65, 54, 66, 54, 67, 54, 68, 54, 69, 54],
	#[2, 226, 10, 226, 11, 226, 36, -213, 37, -214, 38, -197, 39, -202, 40, 226, 48, 226, 56, 226, 58, 226],
	#[14, -109, 13, -107, 15, -113, 30, -201, 21, -224, 23, -174, 10, -226, 19, -219, 17, -118, 38, -197, 16, -117, 29, -196, 31, -207, 18, -216, 27, -190, 22, -228, 46, -205, 12, -106, 24, -176, 37, -214, 39, -202, 26, -182, 35, -210, 33, -172, 41, -227, 49, 346, 32, -209, 45, -199, 47, -208, 25, -179, 43, -189, 60, 303, 62, -188, 28, -191, 40, -223, 53, -183, 55, -173, 42, -185, 51, -177, 68, -206, 52, -180, 36, -213, 20, -221, 61, -184, 63, -193, 50, -175, 59, -229, 57, -203, 44, -194, 66, -195, 69, -198, 48, -171, 64, -181, 67, -204, 65, -215],
	#[65535, 204, 2, 204, 10, 204, 11, 204, 12, 204, 13, 204, 14, 204, 15, 204, 16, 204, 17, 204, 18, 204, 19, 204, 20, 204, 21, 204, 22, 204, 23, 204, 24, 204, 25, 204, 26, 204, 27, 204, 28, 204, 29, 204, 30, 204, 31, 204, 32, 204, 33, 204, 34, 204, 35, 204, 36, 204, 37, 204, 38, 204, 39, 204, 40, 204, 41, 204, 42, 204, 43, 204, 44, 204, 45, 204, 46, 204, 47, 204, 48, 204, 49, 204, 50, 204, 51, 204, 52, 204, 53, 204, 54, 204, 55, 204, 56, 204, 57, 204, 58, 204, 59, 204, 60, 204, 61, 204, 62, 204, 63, 204, 64, 204, 65, 204, 66, 204, 67, 204, 68, 204, 69, 204],
	#[65535, 189, 39, 189, 2, 189, 38, 189, 55, 189, 10, 189, 46, 189, 36, 189, 40, 189, 11, 189, 58, 189, 57, 189, 56, 189, 37, 189, 48, 189],
	#[20, 167, 21, 167, 22, 167, 23, 167, 24, 167, 25, 167, 29, 167, 33, -157, 34, 167],
	#[65535, 37, 2, 37, 10, 37, 11, 37, 12, 37, 13, 37, 14, 37, 15, 37, 16, 37, 17, 37, 18, 37, 19, 37, 20, 37, 21, 37, 22, 37, 23, 37, 24, 37, 25, 37, 26, 37, 27, 37, 28, 37, 29, 37, 30, 37, 31, 37, 32, 37, 33, 37, 34, 37, 35, 37, 36, 37, 37, 37, 38, 37, 39, 37, 40, 37, 41, 37, 42, 37, 43, 37, 44, 37, 45, 37, 46, 37, 47, 37, 48, 37, 49, 37, 50, 37, 51, 37, 52, 37, 53, 37, 54, 37, 55, 37, 56, 37, 57, 37, 58, 37, 59, 37, 60, 37, 61, 37, 62, 37, 63, 37, 64, 37, 65, 37, 66, 37, 67, 37, 68, 37, 69, 37],
	#[15, -113, 2, 100, 23, -10, 31, 100, 18, -54, 10, 100, 11, 100, 12, -106, 13, -107, 14, -109, 38, -91, 16, -117, 17, -118, 47, -98, 19, -56, 20, -62, 21, -63, 22, -66, 34, -58, 24, -14, 25, -18, 28, -28, 29, -32, 54, -84, 32, -99, 33, -3, 63, -88, 35, -101, 36, -105, 37, -104, 39, -94, 40, -151, 41, -115, 42, -82, 43, -87, 44, -89, 45, -92, 46, -96, 58, 100, 48, -72, 49, -74, 50, -76, 51, -77, 52, -79, 53, -80, 67, -35, 55, -75, 56, 100, 57, -95, 65, -47, 59, -119, 60, 100, 61, -81, 62, -85, 66, -31, 68, -41, 69, -34],
	#[1, -456],
	#[65535, 205, 2, 205, 10, 205, 11, 205, 12, 205, 13, 205, 14, 205, 15, 205, 16, 205, 17, 205, 18, 205, 19, 205, 20, 205, 21, 205, 22, 205, 23, 205, 24, 205, 25, 205, 26, 205, 27, 205, 28, 205, 29, 205, 30, 205, 31, 205, 32, 205, 33, 205, 34, 205, 35, 205, 36, 205, 37, 205, 38, 205, 39, 205, 40, 205, 41, 205, 42, 205, 43, 205, 44, 205, 45, 205, 46, 205, 47, 205, 48, 205, 49, 205, 50, 205, 51, 205, 52, 205, 53, 205, 54, 205, 55, 205, 56, 205, 57, 205, 58, 205, 59, 205, 60, 205, 61, 205, 62, 205, 63, 205, 64, 205, 65, 205, 66, 205, 67, 205, 68, 205, 69, 205],
	#[20, -62, 21, -63, 34, -58, 33, -3, 53, -20],
	#[65535, 68, 23, 68, 12, 68, 13, 68, 53, 68, 18, 68, 19, 68, 20, 68, 21, 68, 22, 68, 34, 68, 24, 68, 25, 68, 28, 68, 29, 68, 33, 68, 63, 68, 42, 68, 43, 68, 55, 68, 65, 68, 61, 68, 62, 68, 66, 68, 67, 68, 68, 68, 69, 68],
	#[12, -51, 13, -55, 18, -54, 19, -56, 20, -62, 21, -63, 22, -66, 23, -10, 24, -14, 25, -18, 28, -28, 29, -32, 33, -3, 34, -58, 35, -43, 36, -52, 41, -65, 42, -22, 43, -26, 53, -20, 55, -8, 61, -21, 62, -25, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 87, 2, 87, 10, 87, 11, 87],
	#[65535, 186, 2, 186, 10, 186, 11, 186, 36, 186, 37, 186, 38, 186, 39, 186, 40, 186, 46, 186, 48, 186, 55, 186, 56, 186, 57, 186, 58, 186],
	#[12, -51, 13, -55, 41, -127, 56, 206, 61, -21, 62, -25, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41],
	#[55, -139],
	#[20, -62, 21, -63, 33, -3, 34, -58, 53, -20],
	#[2, 80, 12, -51, 13, -55, 18, -54, 19, -56, 20, -62, 21, -63, 22, -66, 23, -10, 24, -14, 25, -18, 26, -19, 27, -24, 28, -28, 29, -32, 30, -37, 33, -3, 34, -58, 35, -43, 36, -52, 41, -65, 42, -22, 43, -26, 53, -20, 55, -8, 65, -47, 61, -21, 62, -25, 63, -30, 64, -27, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 196, 39, 196, 10, 196, 2, 196, 38, 196, 55, 196, 37, 196, 46, 196, 36, 196, 40, 196, 11, 196, 48, 196, 58, 196, 57, 196, 56, 196],
	#[65535, 69, 12, 69, 13, 69, 18, 69, 19, 69, 20, 69, 21, 69, 22, 69, 23, 69, 24, 69, 25, 69, 28, 69, 29, 69, 33, 69, 34, 69, 42, 69, 43, 69, 53, 69, 55, 69, 61, 69, 62, 69, 63, 69, 65, 69, 66, 69, 67, 69, 68, 69, 69, 69],
	#[65535, 233, 2, 233, 10, 233, 11, 233, 36, 233, 37, 233, 38, 233, 39, 233, 40, 233, 48, 233, 56, 233, 58, 233],
	#[65535, 48, 12, 48, 14, 48, 20, 48, 22, 48, 13, 48, 15, 48, 2, 48, 11, 48, 30, 48, 18, 48, 21, 48, 23, 48, 10, 48, 19, 48, 17, 48, 38, 48, 16, 48, 29, 48, 31, 48, 24, 48, 27, 48, 25, 48, 46, 48, 41, 48, 34, 48, 37, 48, 39, 48, 26, 48, 35, 48, 33, 48, 54, 48, 49, 48, 32, 48, 45, 48, 47, 48, 40, 48, 43, 48, 60, 48, 62, 48, 28, 48, 50, 48, 53, 48, 55, 48, 42, 48, 51, 48, 68, 48, 52, 48, 36, 48, 58, 48, 61, 48, 63, 48, 56, 48, 59, 48, 57, 48, 44, 48, 66, 48, 69, 48, 48, 48, 64, 48, 67, 48, 65, 48],
	#[65535, 197, 2, 197, 10, 197, 11, 197, 36, 197, 37, 197, 38, 197, 39, 197, 40, 197, 46, 197, 48, 197, 55, 197, 56, 197, 57, 197, 58, 197],
	#[65535, 65, 55, 65],
	#[23, -10, 2, 241, 12, -51, 13, -55, 18, -54, 19, -56, 20, -62, 21, -63, 22, -66, 34, -58, 24, -14, 25, -18, 28, -28, 29, -32, 32, -408, 33, -3, 63, -30, 35, -43, 36, -52, 41, -65, 42, -22, 43, -26, 53, -20, 55, -405, 61, -21, 62, -25, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 38, 2, 38, 10, 38, 11, 38, 12, 38, 13, 38, 14, 38, 15, 38, 16, 38, 17, 38, 18, 38, 19, 38, 20, 38, 21, 38, 22, 38, 23, 38, 24, 38, 25, 38, 26, 38, 27, 38, 28, 38, 29, 38, 30, 38, 31, 38, 32, 38, 33, 38, 34, 38, 35, 38, 36, 38, 37, 38, 38, 38, 39, 38, 40, 38, 41, 38, 42, 38, 43, 38, 44, 38, 45, 38, 46, 38, 47, 38, 48, 38, 49, 38, 50, 38, 51, 38, 52, 38, 53, 38, 54, 38, 55, 38, 56, 38, 57, 38, 58, 38, 59, 38, 60, 38, 61, 38, 62, 38, 63, 38, 64, 38, 65, 38, 66, 38, 67, 38, 68, 38, 69, 38],
	#[2, 90, 41, -389],
	#[65535, 198, 2, 198, 10, 198, 11, 198, 36, 198, 37, 198, 38, 198, 39, 198, 40, 198, 46, 198, 48, 198, 55, 198, 56, 198, 57, 198, 58, 198],
	#[65535, 89, 2, 89, 10, 89, 11, 89],
	#[65535, 34, 15, 34, 2, 34, 23, 34, 31, 34, 18, 34, 10, 34, 11, 34, 12, 34, 13, 34, 14, 34, 38, 34, 16, 34, 17, 34, 47, 34, 19, 34, 20, 34, 21, 34, 22, 34, 34, 34, 24, 34, 25, 34, 26, 34, 27, 34, 28, 34, 29, 34, 30, 34, 54, 34, 32, 34, 33, 34, 63, 34, 35, 34, 36, 34, 37, 34, 39, 34, 40, 34, 41, 34, 42, 34, 43, 34, 44, 34, 45, 34, 46, 34, 58, 34, 48, 34, 49, 34, 50, 34, 51, 34, 52, 34, 53, 34, 67, 34, 55, 34, 56, 34, 57, 34, 65, 34, 59, 34, 60, 34, 61, 34, 62, 34, 64, 34, 66, 34, 68, 34, 69, 34],
	#[65535, 35, 2, 35, 10, 35, 11, 35, 12, 35, 13, 35, 14, 35, 15, 35, 16, 35, 17, 35, 18, 35, 19, 35, 20, 35, 21, 35, 22, 35, 23, 35, 24, 35, 25, 35, 26, 35, 27, 35, 28, 35, 29, 35, 30, 35, 31, 35, 32, 35, 33, 35, 34, 35, 35, 35, 36, 35, 37, 35, 38, 35, 39, 35, 40, 35, 41, 35, 42, 35, 43, 35, 44, 35, 45, 35, 46, 35, 47, 35, 48, 35, 49, 35, 50, 35, 51, 35, 52, 35, 53, 35, 54, 35, 55, 35, 56, 35, 57, 35, 58, 35, 59, 35, 60, 35, 61, 35, 62, 35, 63, 35, 64, 35, 65, 35, 66, 35, 67, 35, 68, 35, 69, 35],
	#[22, -66, 13, -107, 15, -113, 2, 96, 30, -37, 21, -63, 23, -10, 10, -114, 19, -56, 14, -109, 38, -91, 29, -32, 18, -54, 27, -24, 25, -18, 46, -96, 12, -106, 37, -104, 39, -94, 17, -118, 35, -101, 33, -3, 54, -84, 16, -117, 32, -99, 45, -92, 47, -98, 34, -58, 43, -87, 41, -115, 26, -19, 24, -14, 40, -112, 53, -80, 55, -75, 42, -82, 51, -77, 49, -74, 52, -79, 36, -105, 20, -62, 61, -81, 63, -88, 50, -76, 59, -119, 57, -95, 44, -89, 28, -28, 69, -34, 48, -72, 64, -27, 67, -35, 62, -85, 66, -31, 68, -41, 65, -47],
	#[65535, 232, 2, 232, 10, 232, 11, 232, 38, 232, 36, 232, 37, 232, 39, 232, 40, 232, 48, 232, 56, 232, 58, 232],
	#[65535, 50, 2, 50, 10, 50, 11, 50, 12, 50, 13, 50, 14, 50, 15, 50, 16, 50, 17, 50, 18, 50, 19, 50, 20, 50, 21, 50, 22, 50, 23, 50, 24, 50, 25, 50, 26, 50, 27, 50, 28, 50, 29, 50, 30, 50, 31, 50, 32, 50, 33, 50, 34, 50, 35, 50, 36, 50, 37, 50, 38, 50, 39, 50, 40, 50, 41, 50, 42, 50, 43, 50, 44, 50, 45, 50, 46, 50, 47, 50, 48, 50, 49, 50, 50, 50, 51, 50, 52, 50, 53, 50, 54, 50, 55, 50, 56, 50, 57, 50, 58, 50, 59, 50, 60, 50, 61, 50, 62, 50, 63, 50, 64, 50, 65, 50, 66, 50, 67, 50, 68, 50, 69, 50],
	#[10, -69, 11, -70],
	#[65535, 11, #"eoi", 11],
	#[65535, 9, #"eoi", 9],
	#[65535, 10, #"eoi", 10],
	#[12, -106, 14, -109, 20, -62, 22, -66, 13, -107, 15, -113, 2, 96, 30, -37, 18, -54, 21, -63, 23, -10, 10, -114, 19, -56, 17, -118, 38, -91, 16, -117, 29, -32, 31, 96, 24, -14, 27, -24, 25, -18, 46, -96, 41, -115, 34, -58, 37, -104, 39, -94, 26, -19, 35, -101, 33, -3, 54, -84, 49, -74, 32, -99, 45, -92, 47, -98, 40, -112, 43, -87, 60, 96, 62, -85, 28, -28, 50, -76, 53, -80, 55, -75, 42, -82, 51, -77, 68, -41, 52, -79, 36, -105, 58, 96, 61, -81, 63, -88, 56, 96, 59, -119, 57, -95, 44, -89, 66, -31, 69, -34, 48, -72, 64, -27, 67, -35, 65, -47],
	#[65535, 138, 2, 138, 10, 138, 11, 138, 12, 138, 13, 138, 14, 138, 15, 138, 16, 138, 17, 138, 18, 138, 19, 138, 20, 138, 21, 138, 22, 138, 23, 138, 24, 138, 25, 138, 26, 138, 27, 138, 28, 138, 29, 138, 30, 138, 31, 138, 32, 138, 33, 138, 34, 138, 35, 138, 36, 138, 37, 138, 38, 138, 39, 138, 40, 138, 41, 138, 42, 138, 43, 138, 44, 138, 45, 138, 46, 138, 47, 138, 48, 138, 49, 138, 50, 138, 51, 138, 52, 138, 53, 138, 54, 138, 55, 138, 56, 138, 57, 138, 58, 138, 59, 138, 60, 138, 61, 138, 62, 138, 63, 138, 64, 138, 65, 138, 66, 138, 67, 138, 68, 138, 69, 138],
	#[65535, 125, 2, 125, 10, 125, 14, 125, 23, 125, 11, 125, 22, 125, 31, 125, 19, 125, 30, 125, 29, 125, 28, 125, 12, 125, 13, 125, 27, 125, 15, 125, 16, 125, 17, 125, 18, 125, 36, 125, 20, 125, 21, 125, 32, 125, 34, 125, 24, 125, 25, 125, 26, 125, 33, 125, 41, 125, 35, 125, 37, 125, 38, 125, 39, 125, 40, 125, 42, 125, 43, 125, 44, 125, 45, 125, 46, 125, 47, 125, 48, 125, 49, 125, 50, 125, 51, 125, 52, 125, 53, 125, 54, 125, 55, 125, 56, 125, 57, 125, 58, 125, 59, 125, 60, 125, 61, 125, 62, 125, 63, 125, 64, 125, 65, 125, 66, 125, 67, 125, 68, 125, 69, 125],
	#[65535, 139, 15, 139, 10, 139, 14, 139, 23, 139, 11, 139, 22, 139, 2, 139, 17, 139, 19, 139, 30, 139, 29, 139, 28, 139, 12, 139, 13, 139, 27, 139, 26, 139, 16, 139, 32, 139, 18, 139, 36, 139, 20, 139, 21, 139, 35, 139, 34, 139, 24, 139, 25, 139, 31, 139, 33, 139, 41, 139, 40, 139, 37, 139, 38, 139, 39, 139, 42, 139, 43, 139, 44, 139, 45, 139, 46, 139, 47, 139, 48, 139, 49, 139, 50, 139, 51, 139, 52, 139, 53, 139, 54, 139, 55, 139, 56, 139, 57, 139, 58, 139, 59, 139, 60, 139, 61, 139, 62, 139, 63, 139, 64, 139, 65, 139, 66, 139, 67, 139, 68, 139, 69, 139],
	#[10, -114, 12, -106, 13, -107, 14, -109, 15, -113, 16, -117, 17, -118, 18, -54, 19, -56, 20, -62, 21, -63, 22, -66, 23, -10, 24, -14, 25, -18, 26, -19, 27, -24, 28, -28, 29, -32, 30, -37, 32, -99, 33, -3, 34, -58, 35, -101, 36, -105, 37, -104, 38, -91, 39, -94, 40, -112, 41, -115, 42, -82, 43, -87, 44, -89, 45, -92, 46, -96, 47, -98, 48, -72, 49, -74, 50, -76, 51, -77, 52, -79, 53, -80, 54, -84, 55, -75, 56, 96, 57, -95, 59, -119, 61, -81, 62, -85, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 140, 14, 140, 22, 140, 13, 140, 15, 140, 2, 140, 11, 140, 30, 140, 25, 140, 21, 140, 23, 140, 10, 140, 19, 140, 17, 140, 38, 140, 62, 140, 16, 140, 29, 140, 31, 140, 27, 140, 44, 140, 46, 140, 37, 140, 39, 140, 26, 140, 35, 140, 24, 140, 18, 140, 20, 140, 45, 140, 47, 140, 34, 140, 43, 140, 32, 140, 49, 140, 28, 140, 12, 140, 53, 140, 55, 140, 42, 140, 51, 140, 68, 140, 52, 140, 36, 140, 58, 140, 61, 140, 33, 140, 50, 140, 59, 140, 54, 140, 60, 140, 40, 140, 66, 140, 69, 140, 41, 140, 64, 140, 57, 140, 56, 140, 48, 140, 63, 140, 65, 140, 67, 140],
	#[65535, 141, 2, 141, 10, 141, 11, 141, 12, 141, 13, 141, 14, 141, 15, 141, 16, 141, 17, 141, 18, 141, 19, 141, 20, 141, 21, 141, 22, 141, 23, 141, 24, 141, 25, 141, 26, 141, 27, 141, 28, 141, 29, 141, 30, 141, 31, 141, 32, 141, 33, 141, 34, 141, 35, 141, 36, 141, 37, 141, 38, 141, 39, 141, 40, 141, 41, 141, 42, 141, 43, 141, 44, 141, 45, 141, 46, 141, 47, 141, 48, 141, 49, 141, 50, 141, 51, 141, 52, 141, 53, 141, 54, 141, 55, 141, 56, 141, 57, 141, 58, 141, 59, 141, 60, 141, 61, 141, 62, 141, 63, 141, 64, 141, 65, 141, 66, 141, 67, 141, 68, 141, 69, 141],
	#[65535, 128, 21, 128, 2, 128, 22, 128, 31, 128, 30, 128, 10, 128, 11, 128, 12, 128, 13, 128, 14, 128, 15, 128, 16, 128, 17, 128, 18, 128, 19, 128, 20, 128, 35, 128, 23, 128, 24, 128, 25, 128, 26, 128, 27, 128, 28, 128, 29, 128, 54, 128, 32, 128, 33, 128, 34, 128, 36, 128, 37, 128, 38, 128, 39, 128, 40, 128, 41, 128, 42, 128, 43, 128, 44, 128, 45, 128, 46, 128, 47, 128, 48, 128, 49, 128, 50, 128, 51, 128, 52, 128, 53, 128, 55, 128, 56, 128, 57, 128, 58, 128, 59, 128, 60, 128, 61, 128, 62, 128, 63, 128, 64, 128, 65, 128, 66, 128, 67, 128, 68, 128, 69, 128],
	#[65535, 142, 14, 142, 2, 142, 15, 142, 10, 142, 17, 142, 29, 142, 23, 142, 12, 142, 18, 142, 25, 142, 11, 142, 22, 142, 31, 142, 20, 142, 26, 142, 19, 142, 30, 142, 39, 142, 28, 142, 33, 142, 13, 142, 27, 142, 38, 142, 16, 142, 32, 142, 37, 142, 36, 142, 40, 142, 21, 142, 35, 142, 34, 142, 24, 142, 41, 142, 42, 142, 43, 142, 44, 142, 45, 142, 46, 142, 47, 142, 48, 142, 49, 142, 50, 142, 51, 142, 52, 142, 53, 142, 54, 142, 55, 142, 56, 142, 57, 142, 58, 142, 59, 142, 60, 142, 61, 142, 62, 142, 63, 142, 64, 142, 65, 142, 66, 142, 67, 142, 68, 142, 69, 142],
	#[65535, 143, 2, 143, 10, 143, 11, 143, 12, 143, 13, 143, 14, 143, 15, 143, 16, 143, 17, 143, 18, 143, 19, 143, 20, 143, 21, 143, 22, 143, 23, 143, 24, 143, 25, 143, 26, 143, 27, 143, 28, 143, 29, 143, 30, 143, 31, 143, 32, 143, 33, 143, 34, 143, 35, 143, 36, 143, 37, 143, 38, 143, 39, 143, 40, 143, 41, 143, 42, 143, 43, 143, 44, 143, 45, 143, 46, 143, 47, 143, 48, 143, 49, 143, 50, 143, 51, 143, 52, 143, 53, 143, 54, 143, 55, 143, 56, 143, 57, 143, 58, 143, 59, 143, 60, 143, 61, 143, 62, 143, 63, 143, 64, 143, 65, 143, 66, 143, 67, 143, 68, 143, 69, 143],
	#[65535, 149, 12, 149, 22, 149, 13, 149, 15, 149, 2, 149, 11, 149, 30, 149, 21, 149, 23, 149, 10, 149, 19, 149, 14, 149, 38, 149, 16, 149, 29, 149, 31, 149, 18, 149, 27, 149, 25, 149, 46, 149, 41, 149, 24, 149, 37, 149, 39, 149, 17, 149, 35, 149, 33, 149, 54, 149, 20, 149, 32, 149, 45, 149, 47, 149, 34, 149, 43, 149, 60, 149, 26, 149, 28, 149, 40, 149, 53, 149, 55, 149, 42, 149, 51, 149, 49, 149, 52, 149, 36, 149, 58, 149, 61, 149, 63, 149, 50, 149, 59, 149, 57, 149, 44, 149, 56, 149, 69, 149, 48, 149, 64, 149, 67, 149, 62, 149, 66, 149, 68, 149, 65, 149],
	#[65535, 132, 12, 132, 22, 132, 13, 132, 15, 132, 2, 132, 11, 132, 30, 132, 21, 132, 23, 132, 10, 132, 19, 132, 14, 132, 38, 132, 16, 132, 29, 132, 31, 132, 18, 132, 27, 132, 25, 132, 46, 132, 41, 132, 24, 132, 37, 132, 39, 132, 17, 132, 35, 132, 33, 132, 54, 132, 20, 132, 32, 132, 45, 132, 47, 132, 34, 132, 43, 132, 60, 132, 26, 132, 28, 132, 40, 132, 53, 132, 55, 132, 42, 132, 51, 132, 49, 132, 52, 132, 36, 132, 58, 132, 61, 132, 63, 132, 50, 132, 59, 132, 57, 132, 44, 132, 56, 132, 69, 132, 48, 132, 64, 132, 67, 132, 62, 132, 66, 132, 68, 132, 65, 132],
	#[2, 108, 10, -114, 12, -106, 13, -107, 14, -109, 15, -113, 16, -117, 17, -118, 19, -56, 20, -62, 21, -63, 24, -14, 25, -18, 26, -19, 27, -24, 30, -37, 31, 108, 32, -99, 33, -3, 34, -58, 35, -101, 36, -105, 37, -104, 38, -91, 39, -94, 40, -112, 41, -115, 42, -82, 43, -87, 44, -89, 45, -92, 46, -96, 47, -98, 48, -72, 49, -74, 50, -76, 51, -77, 52, -79, 53, -80, 54, -84, 55, -75, 56, 108, 57, -95, 58, 108, 59, -119, 60, 108, 61, -81, 62, -85, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41],
	#[65535, 144, 64, 144, 49, 144, 62, 144, 63, 144, 65, 144, 67, 144, 31, 144, 47, 144, 30, 144, 14, 144, 15, 144, 18, 144, 45, 144, 11, 144, 34, 144, 23, 144, 13, 144, 26, 144, 55, 144, 17, 144, 19, 144, 28, 144, 12, 144, 21, 144, 38, 144, 61, 144, 27, 144, 52, 144, 36, 144, 20, 144, 22, 144, 46, 144, 10, 144, 16, 144, 35, 144, 2, 144, 44, 144, 51, 144, 37, 144, 54, 144, 33, 144, 24, 144, 43, 144, 68, 144, 59, 144, 25, 144, 58, 144, 41, 144, 29, 144, 50, 144, 69, 144, 42, 144, 53, 144, 48, 144, 57, 144, 39, 144, 32, 144, 66, 144, 40, 144, 56, 144, 60, 144],
	#[65535, 150, 23, 150, 13, 150, 22, 150, 21, 150, 29, 150, 15, 150, 38, 150, 19, 150, 37, 150, 12, 150, 10, 150, 45, 150, 25, 150, 2, 150, 31, 150, 20, 150, 18, 150, 53, 150, 16, 150, 14, 150, 39, 150, 44, 150, 28, 150, 26, 150, 61, 150, 24, 150, 47, 150, 11, 150, 36, 150, 34, 150, 27, 150, 32, 150, 30, 150, 55, 150, 17, 150, 60, 150, 42, 150, 35, 150, 40, 150, 62, 150, 58, 150, 52, 150, 50, 150, 57, 150, 46, 150, 69, 150, 33, 150, 41, 150, 51, 150, 56, 150, 54, 150, 48, 150, 43, 150, 68, 150, 66, 150, 49, 150, 59, 150, 64, 150, 63, 150, 65, 150, 67, 150],
	#[2, 115, 10, -143, 31, 115, 56, 115, 58, 115, 60, 115],
	#[65535, 133, 15, 133, 14, 133, 29, 133, 2, 133, 18, 133, 20, 133, 47, 133, 43, 133, 26, 133, 23, 133, 17, 133, 25, 133, 11, 133, 10, 133, 31, 133, 63, 133, 30, 133, 54, 133, 39, 133, 28, 133, 12, 133, 13, 133, 27, 133, 38, 133, 16, 133, 19, 133, 42, 133, 21, 133, 22, 133, 34, 133, 24, 133, 51, 133, 33, 133, 32, 133, 50, 133, 41, 133, 35, 133, 36, 133, 37, 133, 40, 133, 49, 133, 48, 133, 44, 133, 45, 133, 46, 133, 58, 133, 52, 133, 53, 133, 67, 133, 55, 133, 56, 133, 57, 133, 65, 133, 59, 133, 60, 133, 61, 133, 62, 133, 64, 133, 66, 133, 68, 133, 69, 133],
	#[65535, 151, 18, 151, 12, 151, 22, 151, 20, 151, 19, 151, 31, 151, 14, 151, 38, 151, 39, 151, 29, 151, 46, 151, 10, 151, 13, 151, 15, 151, 2, 151, 11, 151, 47, 151, 30, 151, 33, 151, 21, 151, 23, 151, 16, 151, 52, 151, 17, 151, 45, 151, 62, 151, 26, 151, 32, 151, 50, 151, 24, 151, 27, 151, 25, 151, 53, 151, 41, 151, 34, 151, 37, 151, 58, 151, 35, 151, 54, 151, 49, 151, 42, 151, 48, 151, 66, 151, 40, 151, 43, 151, 60, 151, 28, 151, 56, 151, 55, 151, 51, 151, 68, 151, 36, 151, 63, 151, 59, 151, 57, 151, 65, 151, 44, 151, 69, 151, 64, 151, 67, 151, 61, 151],
	#[65535, 134, 2, 134, 10, 134, 11, 134, 12, 134, 13, 134, 14, 134, 15, 134, 16, 134, 17, 134, 18, 134, 19, 134, 20, 134, 21, 134, 22, 134, 23, 134, 24, 134, 25, 134, 26, 134, 27, 134, 28, 134, 29, 134, 30, 134, 31, 134, 32, 134, 33, 134, 34, 134, 35, 134, 36, 134, 37, 134, 38, 134, 39, 134, 40, 134, 41, 134, 42, 134, 43, 134, 44, 134, 45, 134, 46, 134, 47, 134, 48, 134, 49, 134, 50, 134, 51, 134, 52, 134, 53, 134, 54, 134, 55, 134, 56, 134, 57, 134, 58, 134, 59, 134, 60, 134, 61, 134, 62, 134, 63, 134, 64, 134, 65, 134, 66, 134, 67, 134, 68, 134, 69, 134],
	#[65535, 127, 15, 127, 29, 127, 28, 127, 14, 127, 38, 127, 63, 127, 20, 127, 22, 127, 13, 127, 35, 127, 2, 127, 47, 127, 12, 127, 27, 127, 18, 127, 21, 127, 10, 127, 19, 127, 36, 127, 45, 127, 62, 127, 26, 127, 11, 127, 31, 127, 69, 127, 60, 127, 44, 127, 46, 127, 41, 127, 34, 127, 37, 127, 39, 127, 17, 127, 68, 127, 30, 127, 54, 127, 49, 127, 42, 127, 48, 127, 66, 127, 25, 127, 33, 127, 23, 127, 50, 127, 53, 127, 55, 127, 24, 127, 16, 127, 32, 127, 58, 127, 43, 127, 40, 127, 59, 127, 57, 127, 64, 127, 51, 127, 52, 127, 56, 127, 61, 127, 65, 127, 67, 127],
	#[65535, 77, 2, 77, 10, 77, 11, 77, 12, 77, 13, 77, 14, 77, 15, 77, 16, 77, 17, 77, 18, 77, 19, 77, 20, 77, 21, 77, 22, 77, 23, 77, 24, 77, 25, 77, 26, 77, 27, 77, 28, 77, 29, 77, 30, 77, 31, 77, 32, 77, 33, 77, 34, 77, 35, 77, 36, 77, 37, 77, 38, 77, 39, 77, 40, 77, 41, 77, 42, 77, 43, 77, 44, 77, 45, 77, 46, 77, 47, 77, 48, 77, 49, 77, 50, 77, 51, 77, 52, 77, 53, 77, 54, 77, 55, 77, 56, 77, 57, 77, 58, 77, 59, 77, 60, 77, 61, 77, 62, 77, 63, 77, 64, 77, 65, 77, 66, 77, 67, 77, 68, 77, 69, 77],
	#[65535, 135, 2, 135, 10, 135, 11, 135, 12, 135, 13, 135, 14, 135, 15, 135, 16, 135, 17, 135, 18, 135, 19, 135, 20, 135, 21, 135, 22, 135, 23, 135, 24, 135, 25, 135, 26, 135, 27, 135, 28, 135, 29, 135, 30, 135, 31, 135, 32, 135, 33, 135, 34, 135, 35, 135, 36, 135, 37, 135, 38, 135, 39, 135, 40, 135, 41, 135, 42, 135, 43, 135, 44, 135, 45, 135, 46, 135, 47, 135, 48, 135, 49, 135, 50, 135, 51, 135, 52, 135, 53, 135, 54, 135, 55, 135, 56, 135, 57, 135, 58, 135, 59, 135, 60, 135, 61, 135, 62, 135, 63, 135, 64, 135, 65, 135, 66, 135, 67, 135, 68, 135, 69, 135],
	#[65535, 129, 28, 129, 13, 129, 21, 129, 30, 129, 54, 129, 29, 129, 15, 129, 38, 129, 20, 129, 19, 129, 2, 129, 37, 129, 16, 129, 11, 129, 46, 129, 24, 129, 27, 129, 10, 129, 45, 129, 33, 129, 31, 129, 52, 129, 35, 129, 17, 129, 53, 129, 51, 129, 14, 129, 39, 129, 62, 129, 44, 129, 43, 129, 12, 129, 61, 129, 41, 129, 22, 129, 47, 129, 32, 129, 36, 129, 34, 129, 23, 129, 49, 129, 26, 129, 55, 129, 40, 129, 18, 129, 59, 129, 42, 129, 64, 129, 57, 129, 63, 129, 25, 129, 68, 129, 67, 129, 56, 129, 65, 129, 69, 129, 66, 129, 60, 129, 50, 129, 58, 129, 48, 129],
	#[65535, 78, 2, 78, 10, 78, 11, 78, 12, 78, 13, 78, 14, 78, 15, 78, 16, 78, 17, 78, 18, 78, 19, 78, 20, 78, 21, 78, 22, 78, 23, 78, 24, 78, 25, 78, 26, 78, 27, 78, 28, 78, 29, 78, 30, 78, 31, 78, 32, 78, 33, 78, 34, 78, 35, 78, 36, 78, 37, 78, 38, 78, 39, 78, 40, 78, 41, 78, 42, 78, 43, 78, 44, 78, 45, 78, 46, 78, 47, 78, 48, 78, 49, 78, 50, 78, 51, 78, 52, 78, 53, 78, 54, 78, 55, 78, 56, 78, 57, 78, 58, 78, 59, 78, 60, 78, 61, 78, 62, 78, 63, 78, 64, 78, 65, 78, 66, 78, 67, 78, 68, 78, 69, 78],
	#[14, -109, 20, -62, 13, -107, 15, -113, 21, -63, 23, -10, 10, -114, 19, -56, 17, -118, 38, -91, 16, -117, 27, -24, 22, -66, 46, -96, 12, -106, 24, -14, 37, -104, 39, -94, 26, -19, 35, -101, 30, -37, 18, -54, 49, -74, 32, -99, 45, -92, 47, -98, 25, -18, 29, -32, 41, -115, 62, -85, 28, -28, 40, -112, 53, -80, 55, -75, 33, -3, 51, -77, 68, -41, 34, -58, 36, -105, 58, 96, 43, -87, 63, -88, 50, -76, 59, -119, 54, -84, 42, -82, 44, -89, 66, -31, 69, -34, 48, -72, 64, -27, 57, -95, 65, -47, 52, -79, 67, -35, 61, -81],
	#[65535, 136, 13, 136, 21, 136, 12, 136, 11, 136, 29, 136, 15, 136, 38, 136, 20, 136, 19, 136, 17, 136, 23, 136, 46, 136, 28, 136, 27, 136, 10, 136, 45, 136, 31, 136, 16, 136, 36, 136, 35, 136, 18, 136, 53, 136, 33, 136, 14, 136, 39, 136, 2, 136, 43, 136, 26, 136, 61, 136, 24, 136, 22, 136, 47, 136, 52, 136, 51, 136, 34, 136, 41, 136, 32, 136, 30, 136, 55, 136, 40, 136, 25, 136, 44, 136, 42, 136, 49, 136, 57, 136, 63, 136, 58, 136, 64, 136, 67, 136, 50, 136, 48, 136, 69, 136, 66, 136, 60, 136, 65, 136, 37, 136, 54, 136, 68, 136, 59, 136, 62, 136, 56, 136],
	#[65535, 97, 2, 97, 31, 97, 56, 97, 58, 97, 60, 97],
	#[65535, 137, 14, 137, 20, 137, 22, 137, 13, 137, 15, 137, 2, 137, 11, 137, 21, 137, 23, 137, 10, 137, 19, 137, 17, 137, 38, 137, 16, 137, 29, 137, 31, 137, 27, 137, 25, 137, 46, 137, 12, 137, 24, 137, 37, 137, 39, 137, 26, 137, 35, 137, 30, 137, 18, 137, 49, 137, 32, 137, 45, 137, 47, 137, 40, 137, 33, 137, 41, 137, 62, 137, 28, 137, 50, 137, 53, 137, 55, 137, 42, 137, 51, 137, 68, 137, 34, 137, 36, 137, 58, 137, 43, 137, 63, 137, 56, 137, 59, 137, 54, 137, 60, 137, 44, 137, 66, 137, 69, 137, 48, 137, 64, 137, 57, 137, 65, 137, 52, 137, 67, 137, 61, 137],
	#[65535, 145, 18, 145, 12, 145, 22, 145, 20, 145, 19, 145, 28, 145, 31, 145, 14, 145, 17, 145, 39, 145, 29, 145, 46, 145, 10, 145, 13, 145, 15, 145, 2, 145, 11, 145, 47, 145, 30, 145, 54, 145, 33, 145, 21, 145, 23, 145, 16, 145, 52, 145, 36, 145, 38, 145, 62, 145, 26, 145, 32, 145, 50, 145, 24, 145, 27, 145, 25, 145, 53, 145, 41, 145, 34, 145, 37, 145, 58, 145, 35, 145, 61, 145, 49, 145, 42, 145, 45, 145, 66, 145, 40, 145, 43, 145, 60, 145, 57, 145, 56, 145, 55, 145, 51, 145, 68, 145, 63, 145, 59, 145, 65, 145, 44, 145, 69, 145, 48, 145, 64, 145, 67, 145],
	#[65535, 131, 2, 131, 10, 131, 11, 131, 12, 131, 13, 131, 14, 131, 15, 131, 16, 131, 17, 131, 18, 131, 19, 131, 20, 131, 21, 131, 22, 131, 23, 131, 24, 131, 25, 131, 26, 131, 27, 131, 28, 131, 29, 131, 30, 131, 31, 131, 32, 131, 33, 131, 34, 131, 35, 131, 36, 131, 37, 131, 38, 131, 39, 131, 40, 131, 41, 131, 42, 131, 43, 131, 44, 131, 45, 131, 46, 131, 47, 131, 48, 131, 49, 131, 50, 131, 51, 131, 52, 131, 53, 131, 54, 131, 55, 131, 56, 131, 57, 131, 58, 131, 59, 131, 60, 131, 61, 131, 62, 131, 63, 131, 64, 131, 65, 131, 66, 131, 67, 131, 68, 131, 69, 131],
	#[65535, 74, 22, 74, 27, 74, 20, 74, 29, 74, 2, 74, 28, 74, 30, 74, 25, 74, 21, 74, 10, 74, 19, 74, 14, 74, 38, 74, 62, 74, 26, 74, 11, 74, 31, 74, 18, 74, 13, 74, 44, 74, 46, 74, 12, 74, 24, 74, 37, 74, 39, 74, 17, 74, 35, 74, 33, 74, 54, 74, 15, 74, 45, 74, 47, 74, 34, 74, 16, 74, 32, 74, 49, 74, 23, 74, 50, 74, 53, 74, 55, 74, 42, 74, 51, 74, 68, 74, 52, 74, 36, 74, 58, 74, 43, 74, 63, 74, 56, 74, 59, 74, 57, 74, 60, 74, 40, 74, 66, 74, 69, 74, 41, 74, 64, 74, 67, 74, 65, 74, 48, 74, 61, 74],
	#[31, 115, 10, -143, 60, 115, 2, 115, 58, 115, 56, 115],
	#[65535, 130, 2, 130, 10, 130, 11, 130, 12, 130, 13, 130, 14, 130, 15, 130, 16, 130, 17, 130, 18, 130, 19, 130, 20, 130, 21, 130, 22, 130, 23, 130, 24, 130, 25, 130, 26, 130, 27, 130, 28, 130, 29, 130, 30, 130, 31, 130, 32, 130, 33, 130, 34, 130, 35, 130, 36, 130, 37, 130, 38, 130, 39, 130, 40, 130, 41, 130, 42, 130, 43, 130, 44, 130, 45, 130, 46, 130, 47, 130, 48, 130, 49, 130, 50, 130, 51, 130, 52, 130, 53, 130, 54, 130, 55, 130, 56, 130, 57, 130, 58, 130, 59, 130, 60, 130, 61, 130, 62, 130, 63, 130, 64, 130, 65, 130, 66, 130, 67, 130, 68, 130, 69, 130],
	#[65535, 75, 15, 75, 14, 75, 28, 75, 2, 75, 20, 75, 10, 75, 17, 75, 29, 75, 26, 75, 23, 75, 18, 75, 22, 75, 31, 75, 30, 75, 54, 75, 39, 75, 11, 75, 12, 75, 13, 75, 27, 75, 38, 75, 16, 75, 43, 75, 47, 75, 19, 75, 42, 75, 21, 75, 32, 75, 34, 75, 24, 75, 25, 75, 33, 75, 41, 75, 50, 75, 63, 75, 35, 75, 36, 75, 37, 75, 51, 75, 40, 75, 49, 75, 48, 75, 44, 75, 45, 75, 46, 75, 58, 75, 52, 75, 53, 75, 67, 75, 55, 75, 56, 75, 57, 75, 65, 75, 59, 75, 60, 75, 61, 75, 62, 75, 64, 75, 66, 75, 68, 75, 69, 75],
	#[65535, 76, 2, 76, 10, 76, 11, 76, 12, 76, 13, 76, 14, 76, 15, 76, 16, 76, 17, 76, 18, 76, 19, 76, 20, 76, 21, 76, 22, 76, 23, 76, 24, 76, 25, 76, 26, 76, 27, 76, 28, 76, 29, 76, 30, 76, 31, 76, 32, 76, 33, 76, 34, 76, 35, 76, 36, 76, 37, 76, 38, 76, 39, 76, 40, 76, 41, 76, 42, 76, 43, 76, 44, 76, 45, 76, 46, 76, 47, 76, 48, 76, 49, 76, 50, 76, 51, 76, 52, 76, 53, 76, 54, 76, 55, 76, 56, 76, 57, 76, 58, 76, 59, 76, 60, 76, 61, 76, 62, 76, 63, 76, 64, 76, 65, 76, 66, 76, 67, 76, 68, 76, 69, 76],
	#[65535, 12, 2, 12, 10, 12, 11, 12, 12, 12, 13, 12, 14, 12, 15, 12, 16, 12, 17, 12, 18, 12, 19, 12, 20, 12, 21, 12, 22, 12, 23, 12, 24, 12, 25, 12, 26, 12, 27, 12, 28, 12, 29, 12, 30, 12, 31, 12, 32, 12, 33, 12, 34, 12, 35, 12, 36, 12, 37, 12, 38, 12, 39, 12, 40, 12, 41, 12, 42, 12, 43, 12, 44, 12, 45, 12, 46, 12, 47, 12, 48, 12, 49, 12, 50, 12, 51, 12, 52, 12, 53, 12, 54, 12, 55, 12, 56, 12, 57, 12, 58, 12, 59, 12, 60, 12, 61, 12, 62, 12, 63, 12, 64, 12, 65, 12, 66, 12, 67, 12, 68, 12, 69, 12],
	#[65535, 13, 15, 13, 47, 13, 23, 13, 30, 13, 18, 13, 28, 13, 12, 13, 27, 13, 45, 13, 50, 13, 43, 13, 22, 13, 46, 13, 35, 13, 53, 13, 44, 13, 51, 13, 37, 13, 54, 13, 67, 13, 21, 13, 42, 13, 36, 13, 38, 13, 62, 13, 26, 13, 29, 13, 31, 13, 69, 13, 13, 13, 63, 13, 10, 13, 48, 13, 49, 13, 19, 13, 39, 13, 17, 13, 68, 13, 52, 13, 11, 13, 20, 13, 32, 13, 66, 13, 14, 13, 60, 13, 24, 13, 65, 13, 16, 13, 55, 13, 41, 13, 34, 13, 2, 13, 58, 13, 61, 13, 40, 13, 56, 13, 59, 13, 57, 13, 64, 13, 33, 13, 25, 13],
	#[65535, 126, 2, 126, 10, 126, 11, 126, 12, 126, 13, 126, 14, 126, 15, 126, 16, 126, 17, 126, 18, 126, 19, 126, 20, 126, 21, 126, 22, 126, 23, 126, 24, 126, 25, 126, 26, 126, 27, 126, 28, 126, 29, 126, 30, 126, 31, 126, 32, 126, 33, 126, 34, 126, 35, 126, 36, 126, 37, 126, 38, 126, 39, 126, 40, 126, 41, 126, 42, 126, 43, 126, 44, 126, 45, 126, 46, 126, 47, 126, 48, 126, 49, 126, 50, 126, 51, 126, 52, 126, 53, 126, 54, 126, 55, 126, 56, 126, 57, 126, 58, 126, 59, 126, 60, 126, 61, 126, 62, 126, 63, 126, 64, 126, 65, 126, 66, 126, 67, 126, 68, 126, 69, 126],
	#[65535, 14, 51, 14, 2, 14, 10, 14, 11, 14, 12, 14, 13, 14, 14, 14, 15, 14, 16, 14, 17, 14, 18, 14, 19, 14, 20, 14, 21, 14, 22, 14, 23, 14, 24, 14, 25, 14, 26, 14, 27, 14, 28, 14, 29, 14, 30, 14, 31, 14, 32, 14, 33, 14, 34, 14, 35, 14, 36, 14, 37, 14, 38, 14, 39, 14, 40, 14, 41, 14, 42, 14, 43, 14, 44, 14, 45, 14, 46, 14, 47, 14, 48, 14, 49, 14, 50, 14, 56, 14, 52, 14, 53, 14, 54, 14, 55, 14, 57, 14, 58, 14, 59, 14, 60, 14, 61, 14, 62, 14, 63, 14, 64, 14, 65, 14, 66, 14, 67, 14, 68, 14, 69, 14],
	#[65535, 153, 28, 153, 29, 153, 20, 153, 30, 153, 42, 153, 12, 153, 39, 153, 22, 153, 46, 153, 15, 153, 2, 153, 26, 153, 37, 153, 27, 153, 21, 153, 23, 153, 10, 153, 14, 153, 38, 153, 33, 153, 16, 153, 11, 153, 31, 153, 18, 153, 13, 153, 25, 153, 53, 153, 41, 153, 34, 153, 19, 153, 58, 153, 17, 153, 35, 153, 50, 153, 61, 153, 49, 153, 32, 153, 45, 153, 47, 153, 40, 153, 43, 153, 60, 153, 62, 153, 24, 153, 56, 153, 55, 153, 51, 153, 68, 153, 36, 153, 63, 153, 59, 153, 54, 153, 44, 153, 66, 153, 69, 153, 48, 153, 64, 153, 57, 153, 65, 153, 52, 153, 67, 153],
	#[2, -386],
	#[69, -34, 31, 96, 59, -119, 66, -31, 2, 96, 58, 96, 65, -47, 30, -37, 54, -84, 52, -79, 55, -75, 45, -92, 62, -85, 56, 96, 51, -77, 64, -27, 60, 96, 44, -89, 46, -96, 43, -87, 49, -74, 37, -104, 39, -94, 68, -41, 50, -76, 36, -105, 15, -113, 10, -114, 47, -98, 29, -32, 32, -99, 26, -19, 23, -10, 12, -106, 53, -80, 25, -18, 42, -82, 22, -66, 40, -112, 27, -24, 21, -63, 20, -62, 24, -14, 33, -3, 19, -56, 18, -54, 57, -95, 35, -101, 17, -118, 28, -28, 34, -58, 13, -107, 14, -109, 38, -91, 16, -117, 48, -72, 41, -115, 61, -81, 63, -88, 67, -35],
	#[65535, 15, 20, 15, 15, 15, 22, 15, 10, 15, 34, 15, 18, 15, 12, 15, 14, 15, 38, 15, 17, 15, 39, 15, 29, 15, 46, 15, 25, 15, 13, 15, 35, 15, 2, 15, 11, 15, 47, 15, 30, 15, 54, 15, 33, 15, 21, 15, 23, 15, 16, 15, 19, 15, 36, 15, 45, 15, 62, 15, 26, 15, 31, 15, 24, 15, 27, 15, 63, 15, 53, 15, 41, 15, 49, 15, 37, 15, 58, 15, 32, 15, 68, 15, 50, 15, 61, 15, 51, 15, 42, 15, 48, 15, 66, 15, 40, 15, 43, 15, 60, 15, 28, 15, 56, 15, 55, 15, 64, 15, 59, 15, 57, 15, 65, 15, 44, 15, 69, 15, 67, 15, 52, 15],
	#[2, 96, 10, -114, 12, -106, 13, -107, 14, -109, 15, -113, 16, -117, 17, -118, 18, -54, 19, -56, 20, -62, 21, -63, 22, -66, 23, -10, 24, -14, 25, -18, 26, -19, 27, -24, 28, -28, 29, -32, 30, -37, 31, 96, 32, -99, 33, -3, 34, -58, 35, -101, 36, -105, 37, -104, 38, -91, 39, -94, 40, -112, 41, -115, 42, -82, 43, -87, 44, -89, 45, -92, 46, -96, 47, -98, 48, -72, 49, -74, 50, -76, 51, -77, 52, -79, 53, -80, 54, -84, 55, -75, 56, 96, 57, -95, 58, 96, 59, -119, 60, 96, 61, -81, 62, -85, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 152, 23, 152, 42, 152, 11, 152, 29, 152, 27, 152, 15, 152, 20, 152, 19, 152, 37, 152, 17, 152, 21, 152, 46, 152, 28, 152, 12, 152, 10, 152, 45, 152, 25, 152, 31, 152, 54, 152, 35, 152, 18, 152, 53, 152, 16, 152, 14, 152, 39, 152, 2, 152, 43, 152, 26, 152, 61, 152, 24, 152, 22, 152, 47, 152, 68, 152, 52, 152, 36, 152, 34, 152, 41, 152, 13, 152, 30, 152, 55, 152, 50, 152, 60, 152, 44, 152, 48, 152, 64, 152, 57, 152, 38, 152, 32, 152, 58, 152, 67, 152, 40, 152, 33, 152, 51, 152, 49, 152, 62, 152, 56, 152, 69, 152, 66, 152, 59, 152, 63, 152, 65, 152],
	#[65535, 98, 31, 98, 58, 98, 60, 98, 2, 98, 56, 98],
	#[65535, 16, 2, 16, 10, 16, 11, 16, 12, 16, 13, 16, 14, 16, 15, 16, 16, 16, 17, 16, 18, 16, 19, 16, 20, 16, 21, 16, 22, 16, 23, 16, 24, 16, 25, 16, 26, 16, 27, 16, 28, 16, 29, 16, 30, 16, 31, 16, 32, 16, 33, 16, 34, 16, 35, 16, 36, 16, 37, 16, 38, 16, 39, 16, 40, 16, 41, 16, 42, 16, 43, 16, 44, 16, 45, 16, 46, 16, 47, 16, 48, 16, 49, 16, 50, 16, 51, 16, 52, 16, 53, 16, 54, 16, 55, 16, 56, 16, 57, 16, 58, 16, 59, 16, 60, 16, 61, 16, 62, 16, 63, 16, 64, 16, 65, 16, 66, 16, 67, 16, 68, 16, 69, 16],
	#[65535, 17, 13, 17, 18, 17, 21, 17, 10, 17, 12, 17, 11, 17, 29, 17, 27, 17, 15, 17, 38, 17, 20, 17, 19, 17, 37, 17, 23, 17, 46, 17, 28, 17, 55, 17, 16, 17, 45, 17, 31, 17, 52, 17, 36, 17, 35, 17, 17, 17, 53, 17, 33, 17, 14, 17, 39, 17, 62, 17, 2, 17, 43, 17, 26, 17, 61, 17, 24, 17, 22, 17, 47, 17, 42, 17, 51, 17, 34, 17, 41, 17, 32, 17, 30, 17, 65, 17, 40, 17, 25, 17, 44, 17, 48, 17, 49, 17, 57, 17, 63, 17, 58, 17, 68, 17, 67, 17, 50, 17, 64, 17, 69, 17, 66, 17, 60, 17, 56, 17, 54, 17, 59, 17],
	#[10, -114, 12, -106, 13, -107, 14, -109, 15, -113, 16, -117, 17, -118, 18, -54, 19, -56, 20, -62, 21, -63, 22, -66, 23, -10, 24, -14, 25, -18, 26, -19, 27, -24, 28, -28, 29, -32, 30, -37, 32, -99, 33, -3, 34, -58, 35, -101, 36, -105, 37, -104, 38, -91, 39, -94, 40, -112, 41, -115, 42, -82, 43, -87, 44, -89, 45, -92, 46, -96, 47, -98, 48, -72, 49, -74, 50, -76, 51, -77, 52, -79, 53, -80, 54, -84, 55, -75, 57, -95, 59, -119, 60, 96, 61, -81, 62, -85, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[60, -385],
	#[65535, 114, 60, 114, 31, 114, 2, 114, 56, 114, 58, 114],
	#[65535, 113, 2, 113, 31, 113, 56, 113, 58, 113, 60, 113],
	#[65535, 211, 40, 211, 46, 211, 56, 211, 58, 211],
	#[56, -138],
	#[46, -135, 56, 207],
	#[65535, 209, 40, 209, 46, 209, 56, 209, 58, 209],
	#[65535, 212, 46, 212, 40, 212, 58, 212, 56, 212],
	#[40, -129, 46, 208, 56, 208, 58, 208],
	#[12, -51, 13, -55, 41, -127, 61, -21, 62, -25, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41],
	#[65535, 210, 40, 210, 46, 210, 58, 210, 56, 210],
	#[65535, 200, 2, 200, 10, 200, 11, 200, 36, 200, 37, 200, 38, 200, 39, 200, 40, 200, 46, 200, 48, 200, 55, 200, 56, 200, 57, 200, 58, 200],
	#[65535, 207, 58, 207],
	#[58, -134],
	#[65535, 203, 2, 203, 10, 203, 11, 203, 12, 203, 13, 203, 14, 203, 15, 203, 16, 203, 17, 203, 18, 203, 19, 203, 20, 203, 21, 203, 22, 203, 23, 203, 24, 203, 25, 203, 26, 203, 27, 203, 28, 203, 29, 203, 30, 203, 31, 203, 32, 203, 33, 203, 34, 203, 35, 203, 36, 203, 37, 203, 38, 203, 39, 203, 40, 203, 41, 203, 42, 203, 43, 203, 44, 203, 45, 203, 46, 203, 47, 203, 48, 203, 49, 203, 50, 203, 51, 203, 52, 203, 53, 203, 54, 203, 55, 203, 56, 203, 57, 203, 58, 203, 59, 203, 60, 203, 61, 203, 62, 203, 63, 203, 64, 203, 65, 203, 66, 203, 67, 203, 68, 203, 69, 203],
	#[13, -55, 12, -51, 62, -25, 41, -127, 61, -21, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41],
	#[56, -137],
	#[65535, 201, 2, 201, 10, 201, 11, 201, 12, 201, 13, 201, 14, 201, 15, 201, 16, 201, 17, 201, 18, 201, 19, 201, 20, 201, 21, 201, 22, 201, 23, 201, 24, 201, 25, 201, 26, 201, 27, 201, 28, 201, 29, 201, 30, 201, 31, 201, 32, 201, 33, 201, 34, 201, 35, 201, 36, 201, 37, 201, 38, 201, 39, 201, 40, 201, 41, 201, 42, 201, 43, 201, 44, 201, 45, 201, 46, 201, 47, 201, 48, 201, 49, 201, 50, 201, 51, 201, 52, 201, 53, 201, 54, 201, 55, 201, 56, 201, 57, 201, 58, 201, 59, 201, 60, 201, 61, 201, 62, 201, 63, 201, 64, 201, 65, 201, 66, 201, 67, 201, 68, 201, 69, 201],
	#[65535, 202, 39, 202, 20, 202, 2, 202, 38, 202, 26, 202, 23, 202, 17, 202, 11, 202, 10, 202, 27, 202, 31, 202, 30, 202, 29, 202, 28, 202, 12, 202, 13, 202, 14, 202, 15, 202, 16, 202, 43, 202, 18, 202, 19, 202, 42, 202, 21, 202, 22, 202, 34, 202, 24, 202, 25, 202, 33, 202, 32, 202, 50, 202, 41, 202, 35, 202, 36, 202, 37, 202, 51, 202, 40, 202, 49, 202, 48, 202, 44, 202, 45, 202, 46, 202, 47, 202, 52, 202, 53, 202, 54, 202, 55, 202, 56, 202, 57, 202, 58, 202, 59, 202, 60, 202, 61, 202, 62, 202, 63, 202, 64, 202, 65, 202, 66, 202, 67, 202, 68, 202, 69, 202],
	#[10, -114, 12, -106, 13, -107, 14, -109, 15, -113, 16, -117, 17, -118, 18, -54, 19, -56, 20, -62, 21, -63, 22, -66, 23, -10, 24, -14, 25, -18, 26, -19, 27, -24, 28, -28, 29, -32, 30, -37, 32, -99, 33, -3, 34, -58, 35, -101, 36, -105, 37, -104, 38, -91, 39, -94, 40, -112, 41, -115, 42, -82, 43, -87, 44, -89, 45, -92, 46, -96, 47, -98, 48, -72, 49, -74, 50, -76, 51, -77, 52, -79, 53, -80, 54, -84, 55, -75, 56, 96, 57, -95, 59, -119, 61, -81, 62, -85, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[56, -141],
	#[65535, 185, 12, 185, 10, 185, 31, 185, 14, 185, 39, 185, 22, 185, 46, 185, 25, 185, 13, 185, 15, 185, 2, 185, 11, 185, 28, 185, 30, 185, 54, 185, 18, 185, 21, 185, 23, 185, 16, 185, 19, 185, 17, 185, 38, 185, 33, 185, 26, 185, 29, 185, 50, 185, 24, 185, 27, 185, 44, 185, 53, 185, 41, 185, 34, 185, 37, 185, 58, 185, 32, 185, 35, 185, 52, 185, 61, 185, 20, 185, 42, 185, 45, 185, 47, 185, 40, 185, 43, 185, 60, 185, 62, 185, 59, 185, 56, 185, 55, 185, 48, 185, 51, 185, 49, 185, 57, 185, 36, 185, 63, 185, 64, 185, 65, 185, 66, 185, 67, 185, 68, 185, 69, 185],
	#[65535, 116, 2, 116, 31, 116, 56, 116, 58, 116, 60, 116],
	#[15, -113, 14, -109, 2, 96, 20, -62, 29, -32, 26, -19, 23, -10, 17, -118, 10, -114, 31, 96, 30, -37, 39, -94, 28, -28, 12, -106, 13, -107, 27, -24, 38, -91, 16, -117, 43, -87, 18, -54, 19, -56, 42, -82, 21, -63, 22, -66, 34, -58, 24, -14, 25, -18, 33, -3, 32, -99, 50, -76, 41, -115, 35, -101, 36, -105, 37, -104, 51, -77, 40, -112, 49, -74, 48, -72, 44, -89, 45, -92, 46, -96, 47, -98, 52, -79, 53, -80, 54, -84, 55, -75, 56, 96, 57, -95, 58, 96, 59, -119, 60, 96, 61, -81, 62, -85, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 110, 2, 110, 31, 110, 56, 110, 58, 110, 60, 110],
	#[65535, 117, 2, 117, 31, 117, 56, 117, 58, 117, 60, 117],
	#[65535, 154, 60, 154, 2, 154, 58, 154, 11, 154, 10, 154, 56, 154, 31, 154],
	#[2, 100, 10, 100, 11, 100, 12, -106, 13, -107, 14, -109, 15, -113, 16, -117, 17, -118, 18, -54, 19, -56, 20, -62, 21, -63, 22, -66, 23, -10, 24, -14, 25, -18, 28, -28, 29, -32, 31, 100, 32, -99, 33, -3, 34, -58, 35, -101, 36, -105, 37, -104, 38, -91, 39, -94, 40, -151, 41, -115, 42, -82, 43, -87, 44, -89, 45, -92, 46, -96, 47, -98, 48, -72, 49, -74, 50, -76, 51, -77, 52, -79, 53, -80, 54, -84, 55, -75, 56, 100, 57, -95, 58, 100, 59, -119, 60, 100, 61, -81, 62, -85, 63, -88, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 102, 2, 102, 10, 102, 11, 102, 31, 102, 56, 102, 58, 102, 60, 102],
	#[55, -75, 31, 118, 38, -91, 2, 118, 52, -79, 39, -94, 53, -80, 37, -104, 54, -84, 41, -115, 43, -87, 10, 118, 19, -56, 36, -105, 20, -62, 62, -85, 56, 118, 11, 118, 51, -77, 60, 118, 44, -89, 46, -96, 12, -106, 49, -74, 57, -95, 59, -119, 32, -99, 35, -101, 50, -76, 61, -81, 15, -113, 42, -82, 45, -92, 47, -98, 34, -58, 14, -109, 58, 118, 48, -72, 13, -107, 17, -118, 25, -18, 33, -3, 24, -14, 40, -151, 16, -117, 21, -63, 63, -88, 65, -47, 66, -31, 67, -35, 68, -41],
	#[65535, 101, 2, 101, 10, 101, 11, 101, 31, 101, 56, 101, 58, 101, 60, 101],
	#[2, 100, 10, 100, 11, 100, 12, -106, 13, -107, 14, -109, 15, -113, 16, -117, 17, -118, 18, -54, 19, -56, 20, -62, 21, -63, 22, -66, 23, -10, 24, -14, 25, -18, 28, -28, 29, -32, 31, 100, 32, -99, 33, -3, 34, -58, 35, -101, 36, -105, 37, -104, 38, -91, 39, -94, 40, -151, 41, -115, 42, -82, 43, -87, 44, -89, 45, -92, 46, -96, 47, -98, 48, -72, 49, -74, 50, -76, 51, -77, 52, -79, 53, -80, 54, -84, 55, -75, 56, 100, 57, -95, 58, 100, 59, -119, 60, 100, 61, -81, 62, -85, 63, -88, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 121, 2, 121, 10, 121, 11, 121, 31, 121, 56, 121, 58, 121, 60, 121],
	#[58, -154],
	#[65535, 147, 2, 147, 10, 147, 11, 147, 12, 147, 13, 147, 14, 147, 15, 147, 16, 147, 17, 147, 18, 147, 19, 147, 20, 147, 21, 147, 22, 147, 23, 147, 24, 147, 25, 147, 26, 147, 27, 147, 28, 147, 29, 147, 30, 147, 31, 147, 32, 147, 33, 147, 34, 147, 35, 147, 36, 147, 37, 147, 38, 147, 39, 147, 40, 147, 41, 147, 42, 147, 43, 147, 44, 147, 45, 147, 46, 147, 47, 147, 48, 147, 49, 147, 50, 147, 51, 147, 52, 147, 53, 147, 54, 147, 55, 147, 56, 147, 57, 147, 58, 147, 59, 147, 60, 147, 61, 147, 62, 147, 63, 147, 64, 147, 65, 147, 66, 147, 67, 147, 68, 147, 69, 147],
	#[22, 169, 20, 169, 29, 169, 23, 169, 25, 169, 33, -157, 24, 169, 34, 169, 21, 169],
	#[65535, 168, 20, 168, 21, 168, 22, 168, 23, 168, 24, 168, 25, 168, 29, 168, 34, 168],
	#[65535, 166, 24, 166, 20, 166, 21, 166, 22, 166, 23, 166, 25, 166, 29, 166, 33, 166, 34, 166],
	#[20, -168, 29, -166, 23, -167, 22, -161, 34, -160, 21, -165, 25, -159, 24, -164],
	#[65535, 58, 10, 58, 12, 58, 13, 58, 14, 58, 15, 58, 16, 58, 17, 58, 18, 58, 19, 58, 20, 58, 21, 58, 22, 58, 23, 58, 24, 58, 25, 58, 26, 58, 27, 58, 28, 58, 29, 58, 30, 58, 31, 58, 32, 58, 33, 58, 34, 58, 35, 58, 36, 58, 37, 58, 38, 58, 39, 58, 40, 58, 41, 58, 42, 58, 43, 58, 44, 58, 45, 58, 46, 58, 47, 58, 48, 58, 49, 58, 50, 58, 51, 58, 52, 58, 53, 58, 54, 58, 55, 58, 57, 58, 59, 58, 61, 58, 62, 58, 63, 58, 64, 58, 65, 58, 66, 58, 67, 58, 68, 58, 69, 58],
	#[65535, 41, 31, 41, 22, 41, 28, 41, 26, 41, 21, 41, 30, 41, 20, 41, 29, 41, 19, 41, 23, 41, 18, 41, 25, 41, 33, 41, 24, 41, 27, 41, 32, 41],
	#[65535, 61, 39, 61, 43, 61, 55, 61, 50, 61, 45, 61, 36, 61, 20, 61, 22, 61, 46, 61, 13, 61, 15, 61, 44, 61, 51, 61, 54, 61, 21, 61, 42, 61, 61, 61, 52, 61, 59, 61, 38, 61, 62, 61, 29, 61, 31, 61, 18, 61, 60, 61, 53, 61, 12, 61, 49, 61, 37, 61, 58, 61, 17, 61, 48, 61, 40, 61, 16, 61, 32, 61, 10, 61, 24, 61, 34, 61, 33, 61, 19, 61, 57, 61, 35, 61, 25, 61, 11, 61, 41, 61, 56, 61, 2, 61, 14, 61, 23, 61, 28, 61, 47, 61, 63, 61, 65, 61, 66, 61, 67, 61, 68, 61, 69, 61],
	#[18, -216, 19, -219, 20, -221, 21, -224, 22, -228, 23, -174, 24, -176, 25, -179, 26, -182, 27, -190, 28, -191, 29, -196, 30, -201, 31, -207, 32, -209, 33, -172],
	#[23, -10, 55, -75, 31, 96, 45, -92, 36, -105, 22, -66, 46, -96, 59, -119, 53, -80, 51, -77, 30, -37, 54, -84, 21, -63, 43, -87, 61, -81, 52, -79, 14, -109, 20, -62, 62, -85, 41, -115, 29, -32, 57, -95, 13, -107, 44, -89, 10, -114, 12, -106, 49, -74, 37, -104, 39, -94, 26, -19, 35, -101, 50, -76, 18, -54, 15, -113, 42, -82, 27, -24, 47, -98, 34, -58, 33, -3, 38, -91, 19, -56, 28, -28, 17, -118, 25, -18, 48, -72, 24, -14, 40, -112, 16, -117, 32, -99, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 62, 2, 62, 10, 62, 11, 62, 12, 62, 13, 62, 14, 62, 15, 62, 16, 62, 17, 62, 18, 62, 19, 62, 20, 62, 21, 62, 22, 62, 23, 62, 24, 62, 25, 62, 28, 62, 29, 62, 31, 62, 32, 62, 33, 62, 34, 62, 35, 62, 36, 62, 37, 62, 38, 62, 39, 62, 40, 62, 41, 62, 42, 62, 43, 62, 44, 62, 45, 62, 46, 62, 47, 62, 48, 62, 49, 62, 50, 62, 51, 62, 52, 62, 53, 62, 54, 62, 55, 62, 56, 62, 57, 62, 58, 62, 59, 62, 60, 62, 61, 62, 62, 62, 63, 62, 65, 62, 66, 62, 67, 62, 68, 62, 69, 62],
	#[65535, 60, 2, 60, 10, 60, 11, 60, 12, 60, 13, 60, 14, 60, 15, 60, 16, 60, 17, 60, 18, 60, 19, 60, 20, 60, 21, 60, 22, 60, 23, 60, 24, 60, 25, 60, 28, 60, 29, 60, 31, 60, 32, 60, 33, 60, 34, 60, 35, 60, 36, 60, 37, 60, 38, 60, 39, 60, 40, 60, 41, 60, 42, 60, 43, 60, 44, 60, 45, 60, 46, 60, 47, 60, 48, 60, 49, 60, 50, 60, 51, 60, 52, 60, 53, 60, 54, 60, 55, 60, 56, 60, 57, 60, 58, 60, 59, 60, 60, 60, 61, 60, 62, 60, 63, 60, 65, 60, 66, 60, 67, 60, 68, 60, 69, 60],
	#[65535, 42, 28, 42, 30, 42, 20, 42, 29, 42, 19, 42, 23, 42, 18, 42, 33, 42, 22, 42, 27, 42, 31, 42, 24, 42, 25, 42, 26, 42, 21, 42, 32, 42],
	#[65535, 59, 15, 59, 10, 59, 16, 59, 27, 59, 21, 59, 20, 59, 13, 59, 17, 59, 18, 59, 23, 59, 24, 59, 25, 59, 26, 59, 30, 59, 31, 59, 32, 59, 33, 59, 41, 59, 35, 59, 36, 59, 37, 59, 38, 59, 40, 59, 42, 59, 43, 59, 44, 59, 45, 59, 46, 59, 48, 59, 49, 59, 50, 59, 51, 59, 52, 59, 53, 59, 54, 59, 55, 59, 57, 59, 59, 59, 61, 59, 62, 59, 63, 59, 64, 59, 65, 59, 66, 59, 67, 59, 68, 59, 69, 59, 47, 59, 14, 59, 29, 59, 22, 59, 28, 59, 12, 59, 34, 59, 19, 59, 39, 59],
	#[65535, 57, 38, 57, 39, 57, 20, 57, 55, 57, 37, 57, 34, 57, 36, 57, 43, 57, 15, 57, 51, 57, 17, 57, 54, 57, 33, 57, 21, 57, 23, 57, 10, 57, 52, 57, 14, 57, 45, 57, 40, 57, 31, 57, 13, 57, 22, 57, 53, 57, 12, 57, 24, 57, 19, 57, 16, 57, 26, 57, 35, 57, 30, 57, 18, 57, 49, 57, 27, 57, 47, 57, 25, 57, 29, 57, 41, 57, 48, 57, 28, 57, 50, 57, 32, 57, 42, 57, 44, 57, 46, 57, 57, 57, 59, 57, 61, 57, 62, 57, 63, 57, 64, 57, 65, 57, 66, 57, 67, 57, 68, 57, 69, 57],
	#[2, 100, 45, -92, 20, -62, 23, -10, 55, -75, 28, -28, 31, 100, 11, 100, 51, -77, 52, -79, 36, -105, 43, -87, 29, -32, 46, -96, 53, -80, 44, -89, 47, -98, 37, -104, 54, -84, 48, -72, 49, -74, 10, 100, 19, -56, 14, -109, 25, -18, 35, -101, 41, -115, 50, -76, 18, -54, 13, -107, 22, -66, 12, -106, 34, -58, 39, -94, 32, -99, 21, -63, 33, -3, 15, -113, 42, -82, 17, -118, 40, -151, 16, -117, 38, -91, 24, -14, 56, 100, 57, -95, 58, 100, 59, -119, 60, 100, 61, -81, 62, -85, 63, -88, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 164, 2, 164, 10, 164, 11, 164, 31, 164, 56, 164, 58, 164, 60, 164],
	#[65535, 322, 13, 322, 10, 322, 15, 322, 14, 322, 23, 322, 22, 322, 43, 322, 19, 322, 42, 322, 44, 322, 25, 322, 12, 322, 41, 322, 27, 322, 26, 322, 16, 322, 17, 322, 18, 322, 24, 322, 20, 322, 21, 322, 32, 322, 28, 322, 29, 322, 30, 322, 31, 322, 33, 322, 35, 322, 36, 322, 37, 322, 38, 322, 39, 322, 40, 322, 45, 322, 46, 322, 47, 322, 48, 322, 49, 322, 50, 322, 51, 322, 52, 322, 53, 322, 55, 322, 56, 322, 57, 322, 58, 322, 59, 322, 60, 322, 61, 322, 62, 322, 63, 322, 64, 322, 65, 322, 66, 322, 67, 322, 68, 322, 69, 322],
	#[65535, 39, 15, 39, 46, 39, 55, 39, 59, 39, 12, 39, 14, 39, 51, 39, 45, 39, 50, 39, 39, 39, 17, 39, 10, 39, 35, 39, 53, 39, 44, 39, 47, 39, 37, 39, 54, 39, 42, 39, 61, 39, 52, 39, 36, 39, 38, 39, 62, 39, 31, 39, 13, 39, 16, 39, 28, 39, 43, 39, 49, 39, 19, 39, 58, 39, 26, 39, 21, 39, 30, 39, 41, 39, 20, 39, 57, 39, 27, 39, 24, 39, 34, 39, 29, 39, 60, 39, 48, 39, 23, 39, 18, 39, 25, 39, 33, 39, 22, 39, 40, 39, 56, 39, 32, 39, 63, 39, 64, 39, 65, 39, 66, 39, 67, 39, 68, 39, 69, 39],
	#[15, -113, 14, -109, 19, -219, 10, -226, 20, -221, 39, -202, 12, -106, 13, -107, 16, -117, 17, -118, 18, -216, 21, -224, 22, -228, 23, -174, 24, -176, 25, -179, 26, -182, 27, -190, 28, -191, 29, -196, 30, -201, 31, -207, 32, -209, 33, -172, 35, -210, 36, -213, 37, -214, 38, -197, 40, -223, 41, -227, 42, -185, 43, -189, 44, -194, 45, -199, 46, -205, 47, -208, 48, -171, 49, 346, 50, -175, 51, -177, 52, -180, 53, -183, 55, -173, 56, 303, 57, -203, 59, -229, 61, -184, 62, -188, 63, -193, 64, -181, 65, -215, 66, -195, 67, -204, 68, -206, 69, -198],
	#[65535, 24, 47, 24, 46, 24, 12, 24, 45, 24, 52, 24, 57, 24, 13, 24, 37, 24, 62, 24, 43, 24, 26, 24, 15, 24, 22, 24, 35, 24, 10, 24, 51, 24, 20, 24, 23, 24, 49, 24, 30, 24, 55, 24, 50, 24, 59, 24, 42, 24, 31, 24, 14, 24, 38, 24, 61, 24, 27, 24, 39, 24, 29, 24, 17, 24, 25, 24, 16, 24, 34, 24, 53, 24, 44, 24, 28, 24, 54, 24, 48, 24, 21, 24, 40, 24, 56, 24, 19, 24, 36, 24, 58, 24, 32, 24, 18, 24, 60, 24, 33, 24, 41, 24, 24, 24, 63, 24, 64, 24, 65, 24, 66, 24, 67, 24, 68, 24, 69, 24],
	#[22, -228, 30, -201, 18, -216, 27, -190, 28, -191, 26, -182, 19, -219, 29, -196, 20, -221, 23, -174, 25, -179, 21, -224, 33, -172, 45, -283, 32, -209, 24, -176, 31, -207],
	#[65535, 25, 43, 25, 55, 25, 14, 25, 51, 25, 45, 25, 39, 25, 22, 25, 46, 25, 13, 25, 15, 25, 53, 25, 44, 25, 47, 25, 12, 25, 50, 25, 21, 25, 23, 25, 10, 25, 52, 25, 36, 25, 38, 25, 35, 25, 29, 25, 31, 25, 18, 25, 27, 25, 25, 25, 28, 25, 41, 25, 49, 25, 37, 25, 16, 25, 26, 25, 48, 25, 30, 25, 54, 25, 20, 25, 42, 25, 17, 25, 34, 25, 33, 25, 32, 25, 19, 25, 24, 25, 40, 25, 56, 25, 57, 25, 58, 25, 59, 25, 60, 25, 61, 25, 62, 25, 63, 25, 64, 25, 65, 25, 66, 25, 67, 25, 68, 25, 69, 25],
	#[28, -191, 18, -216, 21, -224, 23, -174, 20, -221, 31, -207, 27, -190, 22, -228, 19, -219, 30, -201, 25, -179, 29, -196, 32, -209, 26, -182, 24, -176, 33, -172],
	#[65535, 326, 10, 326, 12, 326, 13, 326, 14, 326, 15, 326, 16, 326, 17, 326, 18, 326, 19, 326, 20, 326, 21, 326, 22, 326, 23, 326, 24, 326, 25, 326, 26, 326, 27, 326, 28, 326, 29, 326, 30, 326, 31, 326, 32, 326, 33, 326, 35, 326, 36, 326, 37, 326, 38, 326, 39, 326, 40, 326, 41, 326, 42, 326, 43, 326, 44, 326, 45, 326, 46, 326, 47, 326, 48, 326, 49, 326, 50, 326, 51, 326, 52, 326, 53, 326, 55, 326, 56, 326, 57, 326, 58, 326, 59, 326, 60, 326, 61, 326, 62, 326, 63, 326, 64, 326, 65, 326, 66, 326, 67, 326, 68, 326, 69, 326],
	#[65535, 26, 55, 26, 14, 26, 51, 26, 45, 26, 43, 26, 13, 26, 15, 26, 53, 26, 47, 26, 50, 26, 21, 26, 23, 26, 10, 26, 52, 26, 36, 26, 38, 26, 35, 26, 16, 26, 29, 26, 31, 26, 18, 26, 27, 26, 22, 26, 28, 26, 12, 26, 37, 26, 39, 26, 26, 26, 25, 26, 30, 26, 54, 26, 20, 26, 42, 26, 48, 26, 17, 26, 34, 26, 33, 26, 41, 26, 19, 26, 24, 26, 40, 26, 32, 26, 49, 26, 44, 26, 46, 26, 56, 26, 57, 26, 58, 26, 59, 26, 60, 26, 61, 26, 62, 26, 63, 26, 64, 26, 65, 26, 66, 26, 67, 26, 68, 26, 69, 26],
	#[24, -176, 18, -216, 19, -219, 20, -221, 21, -224, 22, -228, 23, -174, 25, -179, 26, -182, 27, -190, 28, -191, 29, -196, 30, -201, 31, -207, 32, -209, 33, -172, 59, -263],
	#[65535, 331, 10, 331, 12, 331, 13, 331, 14, 331, 15, 331, 16, 331, 17, 331, 18, 331, 19, 331, 20, 331, 21, 331, 22, 331, 23, 331, 24, 331, 25, 331, 26, 331, 27, 331, 28, 331, 29, 331, 30, 331, 31, 331, 32, 331, 33, 331, 35, 331, 36, 331, 37, 331, 38, 331, 39, 331, 40, 331, 41, 331, 42, 331, 43, 331, 44, 331, 45, 331, 46, 331, 47, 331, 48, 331, 49, 331, 50, 331, 51, 331, 52, 331, 53, 331, 55, 331, 56, 331, 57, 331, 58, 331, 59, 331, 60, 331, 61, 331, 62, 331, 63, 331, 64, 331, 65, 331, 66, 331, 67, 331, 68, 331, 69, 331],
	#[65535, 27, 13, 27, 10, 27, 15, 27, 14, 27, 23, 27, 12, 27, 22, 27, 27, 27, 43, 27, 42, 27, 44, 27, 25, 27, 34, 27, 41, 27, 24, 27, 26, 27, 16, 27, 17, 27, 18, 27, 19, 27, 20, 27, 21, 27, 32, 27, 28, 27, 29, 27, 30, 27, 31, 27, 33, 27, 35, 27, 36, 27, 37, 27, 38, 27, 39, 27, 40, 27, 45, 27, 46, 27, 47, 27, 48, 27, 49, 27, 50, 27, 51, 27, 52, 27, 53, 27, 54, 27, 55, 27, 56, 27, 57, 27, 58, 27, 59, 27, 60, 27, 61, 27, 62, 27, 63, 27, 64, 27, 65, 27, 66, 27, 67, 27, 68, 27, 69, 27],
	#[65535, 342, 46, 342, 55, 342, 45, 342, 31, 342, 53, 342, 51, 342, 14, 342, 39, 342, 62, 342, 44, 342, 12, 342, 15, 342, 22, 342, 47, 342, 36, 342, 30, 342, 43, 342, 50, 342, 37, 342, 28, 342, 49, 342, 21, 342, 38, 342, 61, 342, 52, 342, 57, 342, 29, 342, 42, 342, 59, 342, 13, 342, 35, 342, 60, 342, 58, 342, 26, 342, 16, 342, 25, 342, 33, 342, 24, 342, 40, 342, 10, 342, 19, 342, 17, 342, 20, 342, 56, 342, 32, 342, 48, 342, 18, 342, 27, 342, 41, 342, 23, 342, 63, 342, 64, 342, 65, 342, 66, 342, 67, 342, 68, 342, 69, 342],
	#[65535, 314, 10, 314, 12, 314, 13, 314, 14, 314, 15, 314, 16, 314, 17, 314, 18, 314, 19, 314, 20, 314, 21, 314, 22, 314, 23, 314, 24, 314, 25, 314, 26, 314, 27, 314, 28, 314, 29, 314, 30, 314, 31, 314, 32, 314, 33, 314, 41, 314, 35, 314, 36, 314, 37, 314, 38, 314, 39, 314, 40, 314, 42, 314, 43, 314, 44, 314, 45, 314, 46, 314, 47, 314, 48, 314, 49, 314, 50, 314, 51, 314, 52, 314, 53, 314, 55, 314, 56, 314, 57, 314, 58, 314, 59, 314, 60, 314, 61, 314, 62, 314, 63, 314, 64, 314, 65, 314, 66, 314, 67, 314, 68, 314, 69, 314],
	#[65535, 329, 55, 329, 51, 329, 45, 329, 20, 329, 46, 329, 13, 329, 15, 329, 53, 329, 44, 329, 47, 329, 30, 329, 21, 329, 43, 329, 10, 329, 17, 329, 38, 329, 35, 329, 29, 329, 31, 329, 18, 329, 27, 329, 22, 329, 28, 329, 12, 329, 37, 329, 39, 329, 26, 329, 48, 329, 52, 329, 36, 329, 49, 329, 42, 329, 24, 329, 25, 329, 14, 329, 32, 329, 19, 329, 23, 329, 40, 329, 33, 329, 41, 329, 16, 329, 50, 329, 56, 329, 57, 329, 58, 329, 59, 329, 60, 329, 61, 329, 62, 329, 63, 329, 64, 329, 65, 329, 66, 329, 67, 329, 68, 329, 69, 329],
	#[65535, 40, 53, 40, 15, 40, 47, 40, 28, 40, 14, 40, 55, 40, 52, 40, 31, 40, 21, 40, 30, 40, 39, 40, 51, 40, 13, 40, 38, 40, 62, 40, 49, 40, 59, 40, 37, 40, 35, 40, 23, 40, 46, 40, 61, 40, 12, 40, 45, 40, 43, 40, 29, 40, 54, 40, 36, 40, 20, 40, 18, 40, 25, 40, 16, 40, 10, 40, 27, 40, 57, 40, 44, 40, 17, 40, 26, 40, 19, 40, 50, 40, 22, 40, 42, 40, 48, 40, 24, 40, 33, 40, 41, 40, 32, 40, 40, 40, 60, 40, 34, 40, 58, 40, 56, 40, 63, 40, 64, 40, 65, 40, 66, 40, 67, 40, 68, 40, 69, 40],
	#[65535, 304, 56, 304, 58, 304, 60, 304],
	#[65535, 315, 55, 315, 45, 315, 31, 315, 53, 315, 14, 315, 15, 315, 47, 315, 43, 315, 37, 315, 12, 315, 21, 315, 38, 315, 51, 315, 52, 315, 50, 315, 39, 315, 22, 315, 46, 315, 59, 315, 13, 315, 35, 315, 60, 315, 44, 315, 28, 315, 30, 315, 25, 315, 18, 315, 41, 315, 23, 315, 61, 315, 36, 315, 20, 315, 62, 315, 29, 315, 57, 315, 27, 315, 42, 315, 10, 315, 49, 315, 19, 315, 16, 315, 33, 315, 32, 315, 17, 315, 40, 315, 56, 315, 58, 315, 26, 315, 24, 315, 48, 315, 63, 315, 64, 315, 65, 315, 66, 315, 67, 315, 68, 315, 69, 315],
	#[65535, 330, 47, 330, 45, 330, 31, 330, 15, 330, 35, 330, 10, 330, 23, 330, 13, 330, 55, 330, 37, 330, 42, 330, 12, 330, 14, 330, 38, 330, 40, 330, 36, 330, 39, 330, 46, 330, 59, 330, 16, 330, 41, 330, 44, 330, 28, 330, 30, 330, 50, 330, 18, 330, 21, 330, 43, 330, 61, 330, 17, 330, 20, 330, 33, 330, 29, 330, 51, 330, 27, 330, 22, 330, 53, 330, 49, 330, 19, 330, 58, 330, 26, 330, 52, 330, 32, 330, 24, 330, 25, 330, 56, 330, 60, 330, 62, 330, 57, 330, 48, 330, 63, 330, 64, 330, 65, 330, 66, 330, 67, 330, 68, 330, 69, 330],
	#[65535, 28, 39, 28, 15, 28, 14, 28, 19, 28, 20, 28, 10, 28, 12, 28, 13, 28, 16, 28, 17, 28, 18, 28, 21, 28, 22, 28, 23, 28, 24, 28, 25, 28, 26, 28, 27, 28, 28, 28, 29, 28, 30, 28, 31, 28, 32, 28, 33, 28, 34, 28, 35, 28, 36, 28, 37, 28, 38, 28, 40, 28, 41, 28, 42, 28, 43, 28, 44, 28, 45, 28, 46, 28, 47, 28, 48, 28, 49, 28, 50, 28, 51, 28, 52, 28, 53, 28, 54, 28, 55, 28, 56, 28, 57, 28, 58, 28, 59, 28, 60, 28, 61, 28, 62, 28, 63, 28, 64, 28, 65, 28, 66, 28, 67, 28, 68, 28, 69, 28],
	#[65535, 29, 46, 29, 55, 29, 14, 29, 39, 29, 15, 29, 47, 29, 52, 29, 13, 29, 43, 29, 37, 29, 31, 29, 21, 29, 38, 29, 51, 29, 27, 29, 45, 29, 50, 29, 20, 29, 42, 29, 59, 29, 16, 29, 35, 29, 53, 29, 44, 29, 28, 29, 12, 29, 54, 29, 18, 29, 41, 29, 23, 29, 61, 29, 36, 29, 25, 29, 62, 29, 29, 29, 48, 29, 60, 29, 22, 29, 10, 29, 49, 29, 40, 29, 58, 29, 26, 29, 30, 29, 57, 29, 17, 29, 34, 29, 56, 29, 32, 29, 19, 29, 24, 29, 33, 29, 63, 29, 64, 29, 65, 29, 66, 29, 67, 29, 68, 29, 69, 29],
	#[65535, 347, 49, 347],
	#[14, 316, 23, 316, 10, 316, 31, 316, 17, 316, 12, 316, 13, 316, 27, 316, 15, 316, 16, 316, 18, 316, 19, 316, 20, 316, 21, 316, 22, 316, 46, 316, 24, 316, 25, 316, 26, 316, 44, 316, 28, 316, 29, 316, 30, 316, 42, 316, 32, 316, 33, 316, 35, 316, 36, 316, 37, 316, 38, 316, 39, 316, 40, 316, 41, 316, 43, 316, 45, 316, 47, 316, 48, 316, 49, 316, 50, 316, 51, 316, 52, 316, 53, 316, 54, -277, 55, 316, 56, 316, 57, 316, 58, 316, 59, 316, 60, 316, 61, 316, 62, 316, 63, 316, 64, 316, 65, 316, 66, 316, 67, 316, 68, 316, 69, 316],
	#[65535, 327, 39, 327, 55, 327, 31, 327, 14, 327, 38, 327, 20, 327, 22, 327, 46, 327, 13, 327, 15, 327, 30, 327, 25, 327, 33, 327, 21, 327, 23, 327, 10, 327, 19, 327, 17, 327, 45, 327, 35, 327, 26, 327, 29, 327, 50, 327, 18, 327, 27, 327, 44, 327, 53, 327, 12, 327, 24, 327, 37, 327, 16, 327, 32, 327, 48, 327, 52, 327, 36, 327, 51, 327, 42, 327, 47, 327, 40, 327, 43, 327, 41, 327, 49, 327, 28, 327, 56, 327, 57, 327, 58, 327, 59, 327, 60, 327, 61, 327, 62, 327, 63, 327, 64, 327, 65, 327, 66, 327, 67, 327, 68, 327, 69, 327],
	#[10, -226, 12, -106, 13, -107, 14, -109, 15, -113, 16, -117, 17, -118, 18, -216, 19, -219, 20, -221, 21, -224, 22, -228, 23, -174, 24, -176, 25, -179, 26, -182, 27, -190, 28, -191, 29, -196, 30, -201, 31, -207, 32, -209, 33, -172, 35, -210, 36, -213, 37, -214, 38, -197, 39, -202, 40, -223, 41, -227, 42, -185, 43, -189, 44, -194, 45, -199, 46, -205, 47, -208, 48, -171, 49, 346, 50, -175, 51, -177, 52, -180, 53, -183, 55, -173, 57, -203, 58, 303, 59, -229, 61, -184, 62, -188, 63, -193, 64, -181, 65, -215, 66, -195, 67, -204, 68, -206, 69, -198],
	#[65535, 30, 23, 30, 15, 30, 14, 30, 44, 30, 13, 30, 12, 30, 42, 30, 22, 30, 43, 30, 19, 30, 18, 30, 35, 30, 10, 30, 34, 30, 41, 30, 27, 30, 26, 30, 16, 30, 17, 30, 25, 30, 24, 30, 20, 30, 21, 30, 32, 30, 28, 30, 29, 30, 30, 30, 31, 30, 33, 30, 49, 30, 36, 30, 37, 30, 38, 30, 39, 30, 40, 30, 45, 30, 46, 30, 47, 30, 48, 30, 50, 30, 51, 30, 52, 30, 53, 30, 54, 30, 55, 30, 56, 30, 57, 30, 58, 30, 59, 30, 60, 30, 61, 30, 62, 30, 63, 30, 64, 30, 65, 30, 66, 30, 67, 30, 68, 30, 69, 30],
	#[65535, 72, 62, 72, 15, 72, 35, 72, 20, 72, 30, 72, 55, 72, 19, 72, 12, 72, 14, 72, 38, 72, 51, 72, 25, 72, 26, 72, 39, 72, 22, 72, 17, 72, 59, 72, 13, 72, 34, 72, 53, 72, 28, 72, 37, 72, 27, 72, 18, 72, 23, 72, 10, 72, 52, 72, 36, 72, 45, 72, 33, 72, 16, 72, 29, 72, 31, 72, 24, 72, 40, 72, 44, 72, 46, 72, 43, 72, 58, 72, 32, 72, 21, 72, 50, 72, 41, 72, 42, 72, 48, 72, 47, 72, 60, 72, 49, 72, 57, 72, 56, 72, 61, 72, 63, 72, 64, 72, 65, 72, 66, 72, 67, 72, 68, 72, 69, 72],
	#[37, -214, 35, -210, 14, -109, 38, -197, 17, -118, 45, -199, 36, -213, 39, -202, 46, -205, 40, -223, 13, -107, 15, -113, 44, -194, 28, -191, 30, -201, 50, -175, 23, -174, 10, -226, 55, -173, 20, -221, 33, -172, 29, -196, 31, -207, 27, -190, 22, -228, 53, -183, 41, -227, 49, 346, 19, -219, 16, -117, 26, -182, 21, -224, 52, -180, 18, -216, 51, -177, 42, -185, 47, -208, 25, -179, 43, -189, 32, -209, 48, -171, 24, -176, 12, -106, 57, -203, 59, -229, 60, 303, 61, -184, 62, -188, 63, -193, 64, -181, 65, -215, 66, -195, 67, -204, 68, -206, 69, -198],
	#[65535, 328, 15, 328, 44, 328, 23, 328, 22, 328, 31, 328, 19, 328, 18, 328, 10, 328, 12, 328, 13, 328, 14, 328, 26, 328, 16, 328, 17, 328, 25, 328, 24, 328, 20, 328, 21, 328, 27, 328, 28, 328, 29, 328, 30, 328, 42, 328, 32, 328, 33, 328, 35, 328, 36, 328, 37, 328, 38, 328, 39, 328, 40, 328, 41, 328, 43, 328, 45, 328, 46, 328, 47, 328, 48, 328, 49, 328, 50, 328, 51, 328, 52, 328, 53, 328, 55, 328, 56, 328, 57, 328, 58, 328, 59, 328, 60, 328, 61, 328, 62, 328, 63, 328, 64, 328, 65, 328, 66, 328, 67, 328, 68, 328, 69, 328],
	#[23, -174, 36, -213, 46, -205, 31, -207, 61, -184, 30, -201, 39, -202, 49, 346, 38, -197, 62, -188, 47, -208, 59, -229, 37, -214, 22, -228, 44, -194, 28, -191, 55, -173, 45, -199, 43, -189, 29, -196, 26, -182, 57, -203, 35, -210, 53, -183, 51, -177, 10, -226, 27, -190, 60, 305, 40, -223, 21, -224, 32, -209, 15, -113, 50, -175, 18, -216, 42, -185, 52, -180, 25, -179, 20, -221, 41, -227, 13, -107, 48, -171, 24, -176, 17, -118, 19, -219, 12, -106, 14, -109, 16, -117, 58, 305, 33, -172, 56, 305, 63, -193, 64, -181, 65, -215, 66, -195, 67, -204, 68, -206, 69, -198],
	#[65535, 31, 43, 31, 54, 31, 14, 31, 38, 31, 52, 31, 39, 31, 22, 31, 13, 31, 15, 31, 53, 31, 44, 31, 51, 31, 27, 31, 18, 31, 21, 31, 23, 31, 10, 31, 19, 31, 55, 31, 45, 31, 35, 31, 26, 31, 29, 31, 31, 31, 24, 31, 17, 31, 25, 31, 46, 31, 12, 31, 34, 31, 37, 31, 16, 31, 32, 31, 48, 31, 30, 31, 36, 31, 20, 31, 42, 31, 47, 31, 40, 31, 33, 31, 41, 31, 49, 31, 28, 31, 50, 31, 56, 31, 57, 31, 58, 31, 59, 31, 60, 31, 61, 31, 62, 31, 63, 31, 64, 31, 65, 31, 66, 31, 67, 31, 68, 31, 69, 31],
	#[65535, 73, 35, 73, 20, 73, 13, 73, 19, 73, 42, 73, 12, 73, 14, 73, 38, 73, 17, 73, 26, 73, 36, 73, 39, 73, 22, 73, 46, 73, 10, 73, 16, 73, 15, 73, 53, 73, 28, 73, 30, 73, 25, 73, 18, 73, 21, 73, 23, 73, 61, 73, 55, 73, 45, 73, 33, 73, 41, 73, 29, 73, 31, 73, 24, 73, 27, 73, 44, 73, 34, 73, 37, 73, 58, 73, 32, 73, 52, 73, 57, 73, 47, 73, 40, 73, 43, 73, 60, 73, 62, 73, 59, 73, 50, 73, 48, 73, 51, 73, 49, 73, 56, 73, 63, 73, 64, 73, 65, 73, 66, 73, 67, 73, 68, 73, 69, 73],
	#[15, -113, 19, -219, 10, -226, 20, -221, 12, -106, 13, -107, 14, -109, 16, -117, 17, -118, 18, -216, 21, -224, 22, -228, 23, -174, 24, -176, 25, -179, 26, -182, 27, -190, 28, -191, 29, -196, 30, -201, 31, -207, 32, -209, 33, -172, 53, -183, 35, -210, 36, -213, 37, -214, 38, -197, 39, -202, 40, -223, 41, -227, 42, -185, 43, -189, 44, -194, 45, -199, 46, -205, 47, -208, 48, -171, 49, 346, 50, -175, 51, -177, 52, -180, 55, -173, 57, -203, 58, 303, 59, -229, 61, -184, 62, -188, 63, -193, 64, -181, 65, -215, 66, -195, 67, -204, 68, -206, 69, -198],
	#[65535, 332, 43, 332, 12, 332, 14, 332, 51, 332, 45, 332, 36, 332, 39, 332, 46, 332, 13, 332, 15, 332, 53, 332, 44, 332, 47, 332, 37, 332, 50, 332, 48, 332, 23, 332, 10, 332, 52, 332, 55, 332, 38, 332, 35, 332, 29, 332, 31, 332, 27, 332, 22, 332, 28, 332, 41, 332, 49, 332, 19, 332, 16, 332, 21, 332, 30, 332, 18, 332, 20, 332, 42, 332, 17, 332, 25, 332, 33, 332, 32, 332, 26, 332, 24, 332, 40, 332, 56, 332, 57, 332, 58, 332, 59, 332, 60, 332, 61, 332, 62, 332, 63, 332, 64, 332, 65, 332, 66, 332, 67, 332, 68, 332, 69, 332],
	#[65535, 320, 30, 320, 55, 320, 42, 320, 14, 320, 45, 320, 49, 320, 43, 320, 22, 320, 46, 320, 13, 320, 15, 320, 53, 320, 44, 320, 51, 320, 37, 320, 21, 320, 23, 320, 10, 320, 19, 320, 17, 320, 38, 320, 31, 320, 18, 320, 27, 320, 25, 320, 12, 320, 24, 320, 39, 320, 26, 320, 35, 320, 52, 320, 36, 320, 20, 320, 32, 320, 47, 320, 40, 320, 29, 320, 41, 320, 28, 320, 33, 320, 48, 320, 16, 320, 50, 320, 56, 320, 57, 320, 58, 320, 59, 320, 60, 320, 61, 320, 62, 320, 63, 320, 64, 320, 65, 320, 66, 320, 67, 320, 68, 320, 69, 320],
	#[65535, 333, 51, 333, 12, 333, 10, 333, 42, 333, 39, 333, 33, 333, 13, 333, 14, 333, 15, 333, 16, 333, 17, 333, 18, 333, 19, 333, 20, 333, 21, 333, 22, 333, 23, 333, 24, 333, 25, 333, 26, 333, 27, 333, 28, 333, 29, 333, 30, 333, 31, 333, 32, 333, 35, 333, 36, 333, 37, 333, 38, 333, 40, 333, 41, 333, 49, 333, 43, 333, 44, 333, 45, 333, 46, 333, 47, 333, 48, 333, 50, 333, 52, 333, 53, 333, 55, 333, 56, 333, 57, 333, 58, 333, 59, 333, 60, 333, 61, 333, 62, 333, 63, 333, 64, 333, 65, 333, 66, 333, 67, 333, 68, 333, 69, 333],
	#[65535, 32, 15, 32, 55, 32, 12, 32, 14, 32, 38, 32, 51, 32, 45, 32, 43, 32, 46, 32, 13, 32, 35, 32, 53, 32, 44, 32, 47, 32, 17, 32, 50, 32, 48, 32, 23, 32, 10, 32, 52, 32, 36, 32, 25, 32, 33, 32, 16, 32, 31, 32, 18, 32, 27, 32, 22, 32, 28, 32, 41, 32, 49, 32, 37, 32, 39, 32, 26, 32, 21, 32, 30, 32, 54, 32, 20, 32, 42, 32, 24, 32, 34, 32, 29, 32, 32, 32, 19, 32, 40, 32, 56, 32, 57, 32, 58, 32, 59, 32, 60, 32, 61, 32, 62, 32, 63, 32, 64, 32, 65, 32, 66, 32, 67, 32, 68, 32, 69, 32],
	#[65535, 321, 19, 321, 42, 321, 10, 321, 20, 321, 39, 321, 12, 321, 13, 321, 14, 321, 15, 321, 16, 321, 17, 321, 18, 321, 21, 321, 22, 321, 23, 321, 24, 321, 25, 321, 26, 321, 27, 321, 28, 321, 29, 321, 30, 321, 31, 321, 32, 321, 33, 321, 35, 321, 36, 321, 37, 321, 38, 321, 40, 321, 41, 321, 49, 321, 43, 321, 44, 321, 45, 321, 46, 321, 47, 321, 48, 321, 50, 321, 51, 321, 52, 321, 53, 321, 55, 321, 56, 321, 57, 321, 58, 321, 59, 321, 60, 321, 61, 321, 62, 321, 63, 321, 64, 321, 65, 321, 66, 321, 67, 321, 68, 321, 69, 321],
	#[65535, 33, 14, 33, 51, 33, 13, 33, 44, 33, 43, 33, 39, 33, 52, 33, 15, 33, 42, 33, 10, 33, 47, 33, 29, 33, 19, 33, 28, 33, 12, 33, 53, 33, 55, 33, 31, 33, 20, 33, 33, 33, 41, 33, 16, 33, 17, 33, 18, 33, 21, 33, 22, 33, 23, 33, 24, 33, 25, 33, 26, 33, 27, 33, 30, 33, 54, 33, 32, 33, 34, 33, 35, 33, 36, 33, 37, 33, 38, 33, 40, 33, 49, 33, 48, 33, 45, 33, 46, 33, 50, 33, 56, 33, 57, 33, 58, 33, 59, 33, 60, 33, 61, 33, 62, 33, 63, 33, 64, 33, 65, 33, 66, 33, 67, 33, 68, 33, 69, 33],
	#[65535, 317, 44, 317, 14, 317, 51, 317, 22, 317, 53, 317, 43, 317, 39, 317, 52, 317, 15, 317, 42, 317, 10, 317, 47, 317, 29, 317, 19, 317, 28, 317, 12, 317, 16, 317, 55, 317, 40, 317, 27, 317, 21, 317, 20, 317, 33, 317, 13, 317, 17, 317, 18, 317, 23, 317, 24, 317, 25, 317, 26, 317, 30, 317, 31, 317, 32, 317, 41, 317, 35, 317, 36, 317, 37, 317, 38, 317, 49, 317, 48, 317, 45, 317, 46, 317, 50, 317, 56, 317, 57, 317, 58, 317, 59, 317, 60, 317, 61, 317, 62, 317, 63, 317, 64, 317, 65, 317, 66, 317, 67, 317, 68, 317, 69, 317],
	#[65535, 318, 51, 318, 12, 318, 10, 318, 42, 318, 39, 318, 33, 318, 13, 318, 14, 318, 15, 318, 16, 318, 17, 318, 18, 318, 19, 318, 20, 318, 21, 318, 22, 318, 23, 318, 24, 318, 25, 318, 26, 318, 27, 318, 28, 318, 29, 318, 30, 318, 31, 318, 32, 318, 35, 318, 36, 318, 37, 318, 38, 318, 40, 318, 41, 318, 49, 318, 43, 318, 44, 318, 45, 318, 46, 318, 47, 318, 48, 318, 50, 318, 52, 318, 53, 318, 55, 318, 56, 318, 57, 318, 58, 318, 59, 318, 60, 318, 61, 318, 62, 318, 63, 318, 64, 318, 65, 318, 66, 318, 67, 318, 68, 318, 69, 318],
	#[65535, 319, 55, 319, 51, 319, 13, 319, 43, 319, 15, 319, 47, 319, 14, 319, 44, 319, 23, 319, 42, 319, 22, 319, 27, 319, 17, 319, 61, 319, 59, 319, 60, 319, 10, 319, 25, 319, 12, 319, 41, 319, 24, 319, 26, 319, 16, 319, 32, 319, 18, 319, 19, 319, 20, 319, 21, 319, 28, 319, 29, 319, 30, 319, 31, 319, 33, 319, 35, 319, 36, 319, 37, 319, 38, 319, 39, 319, 40, 319, 45, 319, 46, 319, 58, 319, 48, 319, 49, 319, 50, 319, 56, 319, 52, 319, 53, 319, 57, 319, 62, 319, 63, 319, 64, 319, 65, 319, 66, 319, 67, 319, 68, 319, 69, 319],
	#[65535, 71, 23, 71, 10, 71, 14, 71, 44, 71, 28, 71, 35, 71, 52, 71, 43, 71, 19, 71, 18, 71, 42, 71, 39, 71, 25, 71, 12, 71, 13, 71, 27, 71, 15, 71, 16, 71, 17, 71, 47, 71, 33, 71, 20, 71, 21, 71, 22, 71, 34, 71, 24, 71, 51, 71, 26, 71, 49, 71, 29, 71, 30, 71, 31, 71, 32, 71, 53, 71, 40, 71, 36, 71, 37, 71, 38, 71, 41, 71, 48, 71, 45, 71, 46, 71, 50, 71, 55, 71, 56, 71, 57, 71, 58, 71, 59, 71, 60, 71, 61, 71, 62, 71, 63, 71, 64, 71, 65, 71, 66, 71, 67, 71, 68, 71, 69, 71],
	#[65535, 70, 43, 70, 29, 70, 12, 70, 14, 70, 41, 70, 18, 70, 10, 70, 31, 70, 19, 70, 30, 70, 42, 70, 39, 70, 28, 70, 13, 70, 27, 70, 15, 70, 16, 70, 17, 70, 25, 70, 33, 70, 20, 70, 21, 70, 22, 70, 23, 70, 24, 70, 51, 70, 26, 70, 32, 70, 50, 70, 34, 70, 35, 70, 36, 70, 37, 70, 38, 70, 40, 70, 49, 70, 48, 70, 44, 70, 45, 70, 46, 70, 47, 70, 52, 70, 53, 70, 55, 70, 56, 70, 57, 70, 58, 70, 59, 70, 60, 70, 61, 70, 62, 70, 63, 70, 64, 70, 65, 70, 66, 70, 67, 70, 68, 70, 69, 70],
	#[51, -177, 19, -219, 10, -226, 20, -221, 39, -202, 12, -106, 13, -107, 14, -109, 15, -113, 16, -117, 17, -118, 18, -216, 21, -224, 22, -228, 23, -174, 24, -176, 25, -179, 26, -182, 27, -190, 28, -191, 29, -196, 30, -201, 31, -207, 32, -209, 33, -172, 35, -210, 36, -213, 37, -214, 38, -197, 40, -223, 41, -227, 42, -185, 43, -189, 44, -194, 45, -199, 46, -205, 47, -208, 48, -171, 49, 346, 50, -175, 52, -180, 53, -183, 55, -173, 56, 303, 57, -203, 59, -229, 61, -184, 62, -188, 63, -193, 64, -181, 65, -215, 66, -195, 67, -204, 68, -206, 69, -198],
	#[65535, 19, 53, 19, 44, 19, 50, 19, 14, 19, 51, 19, 13, 19, 22, 19, 10, 19, 43, 19, 39, 19, 21, 19, 52, 19, 15, 19, 42, 19, 47, 19, 29, 19, 41, 19, 19, 19, 28, 19, 12, 19, 16, 19, 55, 19, 27, 19, 31, 19, 20, 19, 17, 19, 18, 19, 23, 19, 24, 19, 25, 19, 26, 19, 30, 19, 54, 19, 32, 19, 33, 19, 34, 19, 35, 19, 36, 19, 37, 19, 38, 19, 40, 19, 49, 19, 48, 19, 45, 19, 46, 19, 56, 19, 57, 19, 58, 19, 59, 19, 60, 19, 61, 19, 62, 19, 63, 19, 64, 19, 65, 19, 66, 19, 67, 19, 68, 19, 69, 19],
	#[60, -369],
	#[65535, 312, 59, 312, 53, 312, 14, 312, 51, 312, 13, 312, 42, 312, 10, 312, 43, 312, 49, 312, 39, 312, 48, 312, 52, 312, 15, 312, 47, 312, 29, 312, 19, 312, 28, 312, 35, 312, 55, 312, 46, 312, 27, 312, 36, 312, 20, 312, 12, 312, 41, 312, 16, 312, 17, 312, 18, 312, 21, 312, 22, 312, 23, 312, 24, 312, 25, 312, 26, 312, 32, 312, 30, 312, 31, 312, 33, 312, 58, 312, 37, 312, 38, 312, 40, 312, 61, 312, 60, 312, 44, 312, 45, 312, 56, 312, 50, 312, 57, 312, 62, 312, 63, 312, 64, 312, 65, 312, 66, 312, 67, 312, 68, 312, 69, 312],
	#[65535, 20, 15, 20, 14, 20, 44, 20, 23, 20, 12, 20, 22, 20, 49, 20, 42, 20, 10, 20, 25, 20, 34, 20, 13, 20, 24, 20, 26, 20, 16, 20, 17, 20, 18, 20, 19, 20, 20, 20, 21, 20, 27, 20, 28, 20, 29, 20, 30, 20, 31, 20, 32, 20, 33, 20, 35, 20, 36, 20, 37, 20, 38, 20, 39, 20, 40, 20, 41, 20, 43, 20, 45, 20, 46, 20, 47, 20, 48, 20, 50, 20, 51, 20, 52, 20, 53, 20, 54, 20, 55, 20, 56, 20, 57, 20, 58, 20, 59, 20, 60, 20, 61, 20, 62, 20, 63, 20, 64, 20, 65, 20, 66, 20, 67, 20, 68, 20, 69, 20],
	#[49, -233],
	#[65535, 21, 47, 21, 53, 21, 44, 21, 43, 21, 15, 21, 46, 21, 49, 21, 55, 21, 59, 21, 42, 21, 12, 21, 14, 21, 38, 21, 51, 21, 27, 21, 45, 21, 50, 21, 39, 21, 17, 21, 10, 21, 13, 21, 35, 21, 48, 21, 57, 21, 40, 21, 30, 21, 54, 21, 18, 21, 24, 21, 23, 21, 61, 21, 52, 21, 36, 21, 20, 21, 62, 21, 16, 21, 29, 21, 31, 21, 60, 21, 25, 21, 28, 21, 41, 21, 37, 21, 58, 21, 26, 21, 21, 21, 34, 21, 19, 21, 33, 21, 22, 21, 56, 21, 32, 21, 63, 21, 64, 21, 65, 21, 66, 21, 67, 21, 68, 21, 69, 21],
	#[65535, 338, 15, 338, 14, 338, 51, 338, 45, 338, 43, 338, 46, 338, 59, 338, 13, 338, 35, 338, 44, 338, 47, 338, 50, 338, 21, 338, 42, 338, 61, 338, 52, 338, 36, 338, 38, 338, 62, 338, 31, 338, 60, 338, 16, 338, 28, 338, 48, 338, 49, 338, 37, 338, 39, 338, 26, 338, 30, 338, 41, 338, 20, 338, 57, 338, 10, 338, 17, 338, 29, 338, 58, 338, 19, 338, 23, 338, 12, 338, 18, 338, 55, 338, 33, 338, 22, 338, 40, 338, 27, 338, 32, 338, 25, 338, 24, 338, 56, 338, 53, 338, 63, 338, 64, 338, 65, 338, 66, 338, 67, 338, 68, 338, 69, 338],
	#[65535, 337, 53, 337, 14, 337, 51, 337, 22, 337, 10, 337, 43, 337, 39, 337, 52, 337, 15, 337, 42, 337, 47, 337, 29, 337, 19, 337, 28, 337, 18, 337, 55, 337, 27, 337, 21, 337, 20, 337, 12, 337, 13, 337, 16, 337, 17, 337, 23, 337, 24, 337, 25, 337, 26, 337, 30, 337, 31, 337, 32, 337, 33, 337, 41, 337, 35, 337, 36, 337, 37, 337, 38, 337, 40, 337, 49, 337, 48, 337, 44, 337, 45, 337, 46, 337, 50, 337, 56, 337, 57, 337, 58, 337, 59, 337, 60, 337, 61, 337, 62, 337, 63, 337, 64, 337, 65, 337, 66, 337, 67, 337, 68, 337, 69, 337],
	#[65535, 22, 53, 22, 13, 22, 55, 22, 52, 22, 22, 22, 50, 22, 43, 22, 49, 22, 59, 22, 30, 22, 39, 22, 51, 22, 29, 22, 15, 22, 38, 22, 62, 22, 47, 22, 57, 22, 37, 22, 21, 22, 46, 22, 61, 22, 12, 22, 16, 22, 45, 22, 48, 22, 58, 22, 26, 22, 36, 22, 20, 22, 17, 22, 33, 22, 14, 22, 27, 22, 60, 22, 44, 22, 25, 22, 19, 22, 41, 22, 18, 22, 35, 22, 42, 22, 10, 22, 24, 22, 34, 22, 23, 22, 54, 22, 40, 22, 56, 22, 32, 22, 28, 22, 31, 22, 63, 22, 64, 22, 65, 22, 66, 22, 67, 22, 68, 22, 69, 22],
	#[51, -177, 27, -190, 49, 346, 24, -176, 10, -226, 12, -106, 13, -107, 14, -109, 15, -113, 16, -117, 17, -118, 18, -216, 19, -219, 20, -221, 21, -224, 22, -228, 23, -174, 25, -179, 26, -182, 28, -191, 29, -196, 30, -201, 31, -207, 32, -209, 33, -172, 53, -183, 35, -210, 36, -213, 37, -214, 38, -197, 39, -202, 40, -223, 41, -227, 42, -185, 43, -189, 44, -194, 45, -199, 46, -205, 47, -208, 48, -171, 50, -175, 52, -180, 55, -173, 56, 307, 57, -203, 58, 307, 59, -229, 60, 307, 61, -184, 62, -188, 63, -193, 64, -181, 65, -215, 66, -195, 67, -204, 68, -206, 69, -198],
	#[65535, 336, 45, 336, 44, 336, 43, 336, 12, 336, 15, 336, 46, 336, 30, 336, 55, 336, 59, 336, 42, 336, 31, 336, 14, 336, 38, 336, 51, 336, 50, 336, 39, 336, 17, 336, 10, 336, 13, 336, 35, 336, 53, 336, 57, 336, 47, 336, 27, 336, 48, 336, 40, 336, 61, 336, 52, 336, 36, 336, 20, 336, 62, 336, 49, 336, 24, 336, 60, 336, 16, 336, 28, 336, 41, 336, 37, 336, 58, 336, 26, 336, 21, 336, 18, 336, 25, 336, 29, 336, 19, 336, 23, 336, 33, 336, 22, 336, 56, 336, 32, 336, 63, 336, 64, 336, 65, 336, 66, 336, 67, 336, 68, 336, 69, 336],
	#[65535, 313, 14, 313, 51, 313, 42, 313, 10, 313, 12, 313, 13, 313, 24, 313, 15, 313, 16, 313, 17, 313, 18, 313, 19, 313, 20, 313, 21, 313, 22, 313, 23, 313, 25, 313, 26, 313, 27, 313, 28, 313, 29, 313, 30, 313, 31, 313, 32, 313, 33, 313, 53, 313, 35, 313, 36, 313, 37, 313, 38, 313, 39, 313, 40, 313, 41, 313, 49, 313, 43, 313, 44, 313, 45, 313, 46, 313, 47, 313, 48, 313, 50, 313, 52, 313, 55, 313, 56, 313, 57, 313, 58, 313, 59, 313, 60, 313, 61, 313, 62, 313, 63, 313, 64, 313, 65, 313, 66, 313, 67, 313, 68, 313, 69, 313],
	#[65535, 23, 47, 23, 45, 23, 15, 23, 46, 23, 55, 23, 12, 23, 14, 23, 38, 23, 51, 23, 52, 23, 43, 23, 17, 23, 13, 23, 35, 23, 53, 23, 44, 23, 28, 23, 37, 23, 50, 23, 48, 23, 40, 23, 10, 23, 19, 23, 36, 23, 25, 23, 33, 23, 16, 23, 31, 23, 18, 23, 27, 23, 22, 23, 41, 23, 49, 23, 39, 23, 26, 23, 21, 23, 30, 23, 54, 23, 20, 23, 42, 23, 24, 23, 34, 23, 29, 23, 32, 23, 23, 23, 56, 23, 57, 23, 58, 23, 59, 23, 60, 23, 61, 23, 62, 23, 63, 23, 64, 23, 65, 23, 66, 23, 67, 23, 68, 23, 69, 23],
	#[43, -189, 55, -173, 50, -175, 42, -185, 49, 346, 14, -109, 38, -197, 51, -177, 45, -199, 36, -213, 39, -202, 46, -205, 40, -223, 35, -210, 53, -183, 44, -194, 47, -208, 30, -201, 27, -190, 48, -171, 23, -174, 10, -226, 52, -180, 20, -221, 33, -172, 41, -227, 29, -196, 31, -207, 18, -216, 13, -107, 22, -228, 28, -191, 12, -106, 24, -176, 37, -214, 16, -117, 26, -182, 21, -224, 15, -113, 32, -209, 17, -118, 25, -179, 19, -219, 57, -203, 59, -229, 60, 303, 61, -184, 62, -188, 63, -193, 64, -181, 65, -215, 66, -195, 67, -204, 68, -206, 69, -198],
	#[65535, 18, 55, 18, 52, 18, 49, 18, 39, 18, 51, 18, 50, 18, 15, 18, 43, 18, 47, 18, 59, 18, 22, 18, 46, 18, 61, 18, 12, 18, 16, 18, 45, 18, 48, 18, 31, 18, 54, 18, 36, 18, 13, 18, 17, 18, 53, 18, 33, 18, 14, 18, 37, 18, 62, 18, 44, 18, 28, 18, 26, 18, 19, 18, 41, 18, 18, 18, 35, 18, 42, 18, 10, 18, 29, 18, 20, 18, 27, 18, 32, 18, 30, 18, 40, 18, 60, 18, 21, 18, 38, 18, 25, 18, 24, 18, 56, 18, 34, 18, 58, 18, 23, 18, 57, 18, 63, 18, 64, 18, 65, 18, 66, 18, 67, 18, 68, 18, 69, 18],
	#[60, -368],
	#[65535, 308, 60, 308, 58, 308, 56, 308],
	#[24, -176, 18, -216, 19, -219, 20, -221, 21, -224, 22, -228, 23, -174, 25, -179, 26, -182, 27, -190, 28, -191, 29, -196, 30, -201, 31, -207, 32, -209, 33, -172, 41, -239, 45, -238, 55, -237, 63, -234],
	#[65535, 353, 54, 353, 15, 353, 55, 353, 59, 353, 14, 353, 51, 353, 45, 353, 46, 353, 10, 353, 13, 353, 35, 353, 53, 353, 47, 353, 50, 353, 48, 353, 43, 353, 61, 353, 52, 353, 36, 353, 38, 353, 62, 353, 31, 353, 60, 353, 44, 353, 28, 353, 12, 353, 49, 353, 37, 353, 39, 353, 21, 353, 30, 353, 41, 353, 16, 353, 42, 353, 27, 353, 17, 353, 29, 353, 58, 353, 26, 353, 24, 353, 18, 353, 25, 353, 33, 353, 22, 353, 40, 353, 56, 353, 32, 353, 19, 353, 20, 353, 23, 353, 57, 353, 63, 353, 64, 353, 65, 353, 66, 353, 67, 353, 68, 353, 69, 353],
	#[65535, 352, 14, 352, 51, 352, 42, 352, 10, 352, 12, 352, 13, 352, 24, 352, 15, 352, 16, 352, 17, 352, 18, 352, 19, 352, 20, 352, 21, 352, 22, 352, 23, 352, 25, 352, 26, 352, 27, 352, 28, 352, 29, 352, 30, 352, 31, 352, 32, 352, 33, 352, 53, 352, 35, 352, 36, 352, 37, 352, 38, 352, 39, 352, 40, 352, 41, 352, 49, 352, 43, 352, 44, 352, 45, 352, 46, 352, 47, 352, 48, 352, 50, 352, 52, 352, 54, 352, 55, 352, 56, 352, 57, 352, 58, 352, 59, 352, 60, 352, 61, 352, 62, 352, 63, 352, 64, 352, 65, 352, 66, 352, 67, 352, 68, 352, 69, 352],
	#[51, 349, 10, 349, 12, 349, 13, 349, 14, 349, 15, 349, 16, 349, 17, 349, 18, 349, 19, 349, 20, 349, 21, 349, 22, 349, 23, 349, 24, 349, 25, 349, 26, 349, 27, 349, 28, 349, 29, 349, 30, 349, 31, 349, 32, 349, 33, 349, 35, 349, 36, 349, 37, 349, 38, 349, 39, 349, 40, 349, 41, 349, 42, 349, 43, 349, 44, 349, 45, 349, 46, 349, 47, 349, 48, 349, 49, 349, 50, 349, 52, 349, 53, 349, 54, -365, 55, 349, 56, 349, 57, 349, 58, 349, 59, 349, 60, 349, 61, 349, 62, 349, 63, 349, 64, 349, 65, 349, 66, 349, 67, 349, 68, 349, 69, 349],
	#[22, -66, 61, -21, 55, -8, 43, -26, 19, -56, 36, -52, 35, -43, 53, -20, 41, -65, 62, -25, 28, -28, 12, -51, 33, -3, 24, -14, 42, -22, 29, -32, 20, -62, 23, -10, 13, -55, 18, -54, 21, -63, 34, -58, 25, -18, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 355, 47, 355, 54, 355, 15, 355, 49, 355, 46, 355, 13, 355, 55, 355, 42, 355, 12, 355, 14, 355, 38, 355, 51, 355, 45, 355, 50, 355, 43, 355, 17, 355, 16, 355, 35, 355, 53, 355, 44, 355, 40, 355, 30, 355, 25, 355, 48, 355, 23, 355, 10, 355, 52, 355, 36, 355, 33, 355, 26, 355, 29, 355, 31, 355, 27, 355, 22, 355, 28, 355, 41, 355, 37, 355, 39, 355, 32, 355, 21, 355, 18, 355, 20, 355, 24, 355, 19, 355, 56, 355, 57, 355, 58, 355, 59, 355, 60, 355, 61, 355, 62, 355, 63, 355, 64, 355, 65, 355, 66, 355, 67, 355, 68, 355, 69, 355],
	#[65535, 354, 47, 354, 45, 354, 54, 354, 53, 354, 15, 354, 46, 354, 55, 354, 37, 354, 12, 354, 14, 354, 51, 354, 52, 354, 43, 354, 17, 354, 10, 354, 13, 354, 35, 354, 48, 354, 44, 354, 28, 354, 30, 354, 50, 354, 18, 354, 42, 354, 19, 354, 36, 354, 38, 354, 33, 354, 16, 354, 31, 354, 27, 354, 22, 354, 41, 354, 49, 354, 39, 354, 26, 354, 21, 354, 20, 354, 32, 354, 24, 354, 25, 354, 29, 354, 23, 354, 40, 354, 56, 354, 57, 354, 58, 354, 59, 354, 60, 354, 61, 354, 62, 354, 63, 354, 64, 354, 65, 354, 66, 354, 67, 354, 68, 354, 69, 354],
	#[56, -363],
	#[65535, 288, 23, 288, 22, 288, 47, 288, 27, 288, 55, 288, 50, 288, 28, 288, 14, 288, 26, 288, 45, 288, 36, 288, 43, 288, 29, 288, 15, 288, 53, 288, 51, 288, 30, 288, 54, 288, 48, 288, 42, 288, 52, 288, 34, 288, 38, 288, 35, 288, 16, 288, 49, 288, 31, 288, 18, 288, 13, 288, 25, 288, 32, 288, 12, 288, 37, 288, 39, 288, 17, 288, 21, 288, 41, 288, 20, 288, 10, 288, 24, 288, 40, 288, 33, 288, 19, 288, 44, 288, 46, 288, 57, 288, 59, 288, 61, 288, 62, 288, 63, 288, 64, 288, 65, 288, 66, 288, 67, 288, 68, 288, 69, 288],
	#[52, -180, 49, 346, 39, -202, 51, -177, 29, -196, 42, -185, 38, -197, 53, -183, 47, -208, 57, -203, 56, 303, 50, -175, 46, -205, 61, -184, 55, -173, 10, -226, 45, -199, 43, -189, 19, -219, 59, -229, 36, -213, 35, -210, 40, -223, 33, -172, 14, -109, 37, -214, 62, -188, 44, -194, 28, -191, 12, -106, 15, -113, 41, -227, 22, -228, 32, -209, 48, -171, 25, -179, 20, -221, 27, -190, 13, -107, 30, -201, 17, -118, 18, -216, 16, -117, 24, -176, 26, -182, 21, -224, 23, -174, 31, -207, 63, -193, 64, -181, 65, -215, 66, -195, 67, -204, 68, -206, 69, -198],
	#[65535, 301, 23, 301, 28, 301, 54, 301, 43, 301, 15, 301, 22, 301, 47, 301, 29, 301, 27, 301, 30, 301, 55, 301, 50, 301, 18, 301, 42, 301, 14, 301, 38, 301, 26, 301, 45, 301, 36, 301, 20, 301, 48, 301, 46, 301, 10, 301, 13, 301, 35, 301, 53, 301, 44, 301, 51, 301, 12, 301, 25, 301, 33, 301, 49, 301, 52, 301, 34, 301, 16, 301, 32, 301, 31, 301, 24, 301, 41, 301, 37, 301, 39, 301, 17, 301, 21, 301, 40, 301, 19, 301, 57, 301, 59, 301, 61, 301, 62, 301, 63, 301, 64, 301, 65, 301, 66, 301, 67, 301, 68, 301, 69, 301],
	#[31, -325],
	#[65535, 261, 31, 261],
	#[65535, 295, 23, 295, 52, 295, 22, 295, 43, 295, 30, 295, 54, 295, 29, 295, 27, 295, 15, 295, 38, 295, 47, 295, 26, 295, 50, 295, 46, 295, 28, 295, 42, 295, 45, 295, 48, 295, 31, 295, 59, 295, 32, 295, 13, 295, 18, 295, 53, 295, 51, 295, 14, 295, 39, 295, 62, 295, 44, 295, 16, 295, 12, 295, 61, 295, 49, 295, 35, 295, 10, 295, 36, 295, 34, 295, 41, 295, 57, 295, 55, 295, 17, 295, 25, 295, 37, 295, 21, 295, 24, 295, 40, 295, 20, 295, 33, 295, 19, 295, 63, 295, 64, 295, 65, 295, 66, 295, 67, 295, 68, 295, 69, 295],
	#[55, -242, 31, 271, 44, -250, 30, -37, 54, -84, 39, -94, 38, -91, 62, -85, 47, -98, 59, -263, 46, -96, 61, -81, 42, -246, 10, -262, 45, -252, 43, -249, 41, -115, 52, -79, 36, -105, 53, -80, 51, -77, 14, -109, 37, -104, 34, -58, 40, -261, 21, -63, 12, -106, 15, -113, 50, -76, 35, -101, 32, -99, 48, -72, 24, -14, 20, -62, 27, -24, 49, -74, 26, -19, 17, -118, 19, -56, 16, -117, 25, -18, 33, -3, 13, -107, 57, -254, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41],
	#[31, 278, 10, -289],
	#[65535, 296, 23, 296, 28, 296, 22, 296, 47, 296, 29, 296, 27, 296, 50, 296, 18, 296, 42, 296, 14, 296, 38, 296, 26, 296, 45, 296, 36, 296, 43, 296, 48, 296, 46, 296, 15, 296, 53, 296, 44, 296, 51, 296, 30, 296, 54, 296, 33, 296, 49, 296, 10, 296, 52, 296, 34, 296, 20, 296, 16, 296, 32, 296, 31, 296, 24, 296, 13, 296, 25, 296, 12, 296, 37, 296, 39, 296, 17, 296, 21, 296, 41, 296, 40, 296, 19, 296, 35, 296, 55, 296, 57, 296, 59, 296, 61, 296, 62, 296, 63, 296, 64, 296, 65, 296, 66, 296, 67, 296, 68, 296, 69, 296],
	#[65535, 297, 63, 297, 64, 297, 65, 297, 66, 297, 67, 297, 68, 297, 69, 297, 22, 297, 47, 297, 54, 297, 52, 297, 23, 297, 26, 297, 18, 297, 45, 297, 44, 297, 29, 297, 13, 297, 46, 297, 59, 297, 39, 297, 42, 297, 30, 297, 15, 297, 27, 297, 48, 297, 49, 297, 28, 297, 14, 297, 38, 297, 62, 297, 55, 297, 53, 297, 36, 297, 20, 297, 50, 297, 31, 297, 43, 297, 19, 297, 61, 297, 34, 297, 32, 297, 12, 297, 41, 297, 51, 297, 16, 297, 24, 297, 33, 297, 37, 297, 35, 297, 25, 297, 21, 297, 57, 297, 10, 297, 17, 297, 40, 297],
	#[65535, 290, 28, 290, 54, 290, 43, 290, 45, 290, 22, 290, 29, 290, 44, 290, 15, 290, 18, 290, 47, 290, 30, 290, 37, 290, 52, 290, 23, 290, 25, 290, 50, 290, 14, 290, 42, 290, 31, 290, 27, 290, 12, 290, 16, 290, 46, 290, 53, 290, 39, 290, 49, 290, 21, 290, 61, 290, 26, 290, 48, 290, 57, 290, 19, 290, 38, 290, 62, 290, 33, 290, 13, 290, 55, 290, 36, 290, 24, 290, 32, 290, 10, 290, 17, 290, 35, 290, 41, 290, 51, 290, 34, 290, 59, 290, 40, 290, 20, 290, 63, 290, 64, 290, 65, 290, 66, 290, 67, 290, 68, 290, 69, 290],
	#[65535, 298, 45, 298, 28, 298, 14, 298, 23, 298, 13, 298, 52, 298, 22, 298, 46, 298, 18, 298, 42, 298, 30, 298, 54, 298, 29, 298, 27, 298, 15, 298, 38, 298, 62, 298, 47, 298, 26, 298, 50, 298, 51, 298, 61, 298, 55, 298, 17, 298, 43, 298, 31, 298, 59, 298, 32, 298, 24, 298, 53, 298, 16, 298, 41, 298, 39, 298, 44, 298, 21, 298, 12, 298, 49, 298, 35, 298, 10, 298, 36, 298, 20, 298, 48, 298, 19, 298, 37, 298, 25, 298, 33, 298, 40, 298, 34, 298, 57, 298, 63, 298, 64, 298, 65, 298, 66, 298, 67, 298, 68, 298, 69, 298],
	#[65535, 292, 23, 292, 46, 292, 28, 292, 45, 292, 53, 292, 43, 292, 15, 292, 22, 292, 47, 292, 42, 292, 29, 292, 27, 292, 13, 292, 30, 292, 55, 292, 50, 292, 18, 292, 14, 292, 17, 292, 26, 292, 52, 292, 36, 292, 39, 292, 48, 292, 10, 292, 16, 292, 35, 292, 44, 292, 51, 292, 37, 292, 54, 292, 33, 292, 21, 292, 49, 292, 19, 292, 34, 292, 38, 292, 40, 292, 31, 292, 24, 292, 25, 292, 12, 292, 20, 292, 32, 292, 41, 292, 57, 292, 59, 292, 61, 292, 62, 292, 63, 292, 64, 292, 65, 292, 66, 292, 67, 292, 68, 292, 69, 292],
	#[59, -229, 39, -202, 38, -197, 23, -174, 55, -173, 52, -180, 50, -175, 31, -207, 43, -189, 49, 346, 57, -203, 61, -184, 30, -201, 58, 303, 26, -182, 51, -177, 29, -196, 42, -185, 36, -213, 53, -183, 47, -208, 48, -171, 37, -214, 22, -228, 46, -205, 28, -191, 12, -106, 10, -226, 45, -199, 33, -172, 19, -219, 32, -209, 35, -210, 18, -216, 40, -223, 14, -109, 27, -190, 62, -188, 44, -194, 21, -224, 15, -113, 41, -227, 17, -118, 24, -176, 20, -221, 16, -117, 13, -107, 25, -179, 63, -193, 64, -181, 65, -215, 66, -195, 67, -204, 68, -206, 69, -198],
	#[65535, 291, 45, 291, 23, 291, 13, 291, 52, 291, 22, 291, 46, 291, 30, 291, 54, 291, 29, 291, 27, 291, 15, 291, 43, 291, 62, 291, 47, 291, 26, 291, 50, 291, 51, 291, 28, 291, 42, 291, 32, 291, 48, 291, 31, 291, 16, 291, 57, 291, 20, 291, 18, 291, 53, 291, 14, 291, 39, 291, 44, 291, 17, 291, 12, 291, 61, 291, 59, 291, 49, 291, 35, 291, 10, 291, 36, 291, 41, 291, 55, 291, 25, 291, 37, 291, 38, 291, 24, 291, 33, 291, 34, 291, 21, 291, 40, 291, 19, 291, 63, 291, 64, 291, 65, 291, 66, 291, 67, 291, 68, 291, 69, 291],
	#[65535, 294, 45, 294, 28, 294, 14, 294, 23, 294, 52, 294, 22, 294, 46, 294, 18, 294, 43, 294, 42, 294, 30, 294, 54, 294, 26, 294, 29, 294, 27, 294, 15, 294, 38, 294, 53, 294, 47, 294, 37, 294, 35, 294, 50, 294, 51, 294, 61, 294, 55, 294, 32, 294, 48, 294, 31, 294, 59, 294, 57, 294, 20, 294, 24, 294, 16, 294, 10, 294, 39, 294, 62, 294, 44, 294, 21, 294, 12, 294, 33, 294, 49, 294, 25, 294, 34, 294, 41, 294, 13, 294, 17, 294, 19, 294, 40, 294, 36, 294, 63, 294, 64, 294, 65, 294, 66, 294, 67, 294, 68, 294, 69, 294],
	#[31, 278, 10, -289],
	#[65535, 293, 29, 293, 22, 293, 52, 293, 42, 293, 54, 293, 47, 293, 45, 293, 28, 293, 14, 293, 38, 293, 23, 293, 53, 293, 26, 293, 27, 293, 46, 293, 18, 293, 43, 293, 49, 293, 44, 293, 30, 293, 12, 293, 39, 293, 51, 293, 50, 293, 32, 293, 17, 293, 15, 293, 41, 293, 19, 293, 48, 293, 37, 293, 35, 293, 25, 293, 21, 293, 24, 293, 55, 293, 16, 293, 34, 293, 33, 293, 31, 293, 36, 293, 20, 293, 10, 293, 40, 293, 13, 293, 57, 293, 59, 293, 61, 293, 62, 293, 63, 293, 64, 293, 65, 293, 66, 293, 67, 293, 68, 293, 69, 293],
	#[65535, 289, 28, 289, 29, 289, 54, 289, 14, 289, 18, 289, 42, 289, 43, 289, 45, 289, 22, 289, 46, 289, 44, 289, 15, 289, 13, 289, 51, 289, 47, 289, 30, 289, 37, 289, 52, 289, 23, 289, 26, 289, 50, 289, 35, 289, 62, 289, 16, 289, 31, 289, 27, 289, 12, 289, 49, 289, 32, 289, 53, 289, 24, 289, 39, 289, 10, 289, 48, 289, 21, 289, 17, 289, 61, 289, 33, 289, 41, 289, 57, 289, 19, 289, 38, 289, 40, 289, 55, 289, 36, 289, 20, 289, 34, 289, 25, 289, 59, 289, 63, 289, 64, 289, 65, 289, 66, 289, 67, 289, 68, 289, 69, 289],
	#[65535, 260, 31, 260],
	#[47, -98, 59, -263, 62, -85, 55, -242, 31, 259, 42, -246, 61, -81, 44, -250, 30, -37, 54, -84, 39, -94, 51, -77, 52, -79, 38, -91, 53, -80, 57, -254, 37, -104, 22, -66, 46, -96, 28, -28, 40, -261, 10, -262, 45, -252, 43, -249, 19, -56, 49, -74, 36, -105, 35, -101, 25, -18, 33, -3, 41, -115, 27, -24, 34, -58, 21, -63, 26, -19, 15, -113, 50, -76, 32, -99, 17, -118, 29, -32, 20, -62, 23, -10, 13, -107, 24, -14, 18, -54, 12, -106, 14, -109, 16, -117, 48, -72, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[55, -242, 54, -84, 31, 259, 42, -246, 43, -249, 45, -252, 57, -254, 53, -80, 10, -262, 30, -37, 37, -104, 52, -79, 36, -105, 40, -261, 59, -263, 41, -115, 35, -101, 38, -91, 62, -85, 51, -77, 44, -250, 34, -58, 24, -14, 49, -74, 22, -66, 46, -96, 50, -76, 25, -18, 39, -94, 20, -62, 48, -72, 21, -63, 15, -113, 27, -24, 32, -99, 47, -98, 18, -54, 29, -32, 23, -10, 16, -117, 26, -19, 17, -118, 19, -56, 61, -81, 28, -28, 14, -109, 33, -3, 12, -106, 13, -107, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[23, -174, 44, -194, 10, -226, 26, -182, 15, -113, 14, -109, 19, -219, 13, -107, 12, -106, 18, -216, 42, -185, 22, -228, 46, -205, 52, -180, 21, -224, 20, -221, 25, -179, 16, -117, 24, -176, 17, -118, 27, -190, 28, -191, 29, -196, 30, -201, 31, -207, 32, -209, 33, -172, 35, -210, 36, -213, 37, -214, 38, -197, 39, -202, 40, -223, 41, -227, 43, -189, 45, -199, 47, -208, 48, -171, 49, 346, 50, -175, 51, -177, 53, -183, 55, -173, 57, -203, 59, -229, 60, 303, 61, -184, 62, -188, 63, -193, 64, -181, 65, -215, 66, -195, 67, -204, 68, -206, 69, -198],
	#[59, -263, 62, -85, 55, -242, 51, -77, 40, -261, 31, 259, 41, -115, 42, -246, 61, -81, 44, -250, 30, -37, 54, -84, 39, -94, 49, -74, 52, -79, 38, -91, 53, -80, 47, -98, 57, -254, 37, -104, 35, -101, 46, -96, 28, -28, 27, -24, 10, -262, 45, -252, 43, -249, 19, -56, 26, -19, 36, -105, 13, -107, 25, -18, 33, -3, 14, -109, 34, -58, 21, -63, 12, -106, 15, -113, 24, -14, 22, -66, 32, -99, 48, -72, 29, -32, 20, -62, 23, -10, 50, -76, 18, -54, 16, -117, 17, -118, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 275, 31, 275],
	#[60, -267],
	#[65535, 302, 54, 302, 23, 302, 13, 302, 52, 302, 22, 302, 18, 302, 43, 302, 42, 302, 30, 302, 12, 302, 29, 302, 27, 302, 15, 302, 38, 302, 62, 302, 47, 302, 26, 302, 50, 302, 46, 302, 28, 302, 55, 302, 45, 302, 48, 302, 19, 302, 57, 302, 20, 302, 24, 302, 53, 302, 51, 302, 14, 302, 39, 302, 34, 302, 44, 302, 21, 302, 25, 302, 61, 302, 59, 302, 49, 302, 35, 302, 32, 302, 10, 302, 36, 302, 16, 302, 58, 302, 17, 302, 60, 302, 37, 302, 41, 302, 40, 302, 33, 302, 56, 302, 31, 302, 63, 302, 64, 302, 65, 302, 66, 302, 67, 302, 68, 302, 69, 302],
	#[56, -269],
	#[65535, 309, 15, 309, 50, 309, 13, 309, 14, 309, 52, 309, 42, 309, 43, 309, 45, 309, 57, 309, 28, 309, 17, 309, 51, 309, 53, 309, 30, 309, 37, 309, 48, 309, 36, 309, 21, 309, 59, 309, 55, 309, 35, 309, 38, 309, 62, 309, 44, 309, 27, 309, 12, 309, 49, 309, 22, 309, 46, 309, 18, 309, 39, 309, 10, 309, 40, 309, 25, 309, 33, 309, 47, 309, 16, 309, 29, 309, 23, 309, 41, 309, 32, 309, 26, 309, 20, 309, 31, 309, 19, 309, 61, 309, 56, 309, 24, 309, 58, 309, 60, 309, 63, 309, 64, 309, 65, 309, 66, 309, 67, 309, 68, 309, 69, 309],
	#[58, -271],
	#[65535, 324, 42, 324, 50, 324, 59, 324, 13, 324, 55, 324, 52, 324, 22, 324, 46, 324, 31, 324, 43, 324, 49, 324, 57, 324, 61, 324, 44, 324, 30, 324, 39, 324, 51, 324, 29, 324, 48, 324, 15, 324, 38, 324, 62, 324, 47, 324, 37, 324, 21, 324, 28, 324, 12, 324, 16, 324, 45, 324, 33, 324, 26, 324, 36, 324, 20, 324, 17, 324, 53, 324, 14, 324, 27, 324, 40, 324, 58, 324, 25, 324, 19, 324, 41, 324, 18, 324, 35, 324, 10, 324, 24, 324, 23, 324, 56, 324, 32, 324, 60, 324, 63, 324, 64, 324, 65, 324, 66, 324, 67, 324, 68, 324, 69, 324],
	#[65535, 306, 58, 306, 60, 306, 56, 306],
	#[60, -274],
	#[65535, 311, 53, 311, 42, 311, 50, 311, 59, 311, 62, 311, 13, 311, 55, 311, 52, 311, 22, 311, 37, 311, 31, 311, 43, 311, 49, 311, 57, 311, 61, 311, 44, 311, 30, 311, 45, 311, 39, 311, 51, 311, 29, 311, 15, 311, 38, 311, 47, 311, 58, 311, 26, 311, 21, 311, 46, 311, 28, 311, 12, 311, 16, 311, 17, 311, 48, 311, 33, 311, 36, 311, 20, 311, 18, 311, 25, 311, 14, 311, 27, 311, 40, 311, 32, 311, 19, 311, 41, 311, 35, 311, 10, 311, 24, 311, 23, 311, 60, 311, 56, 311, 63, 311, 64, 311, 65, 311, 66, 311, 67, 311, 68, 311, 69, 311],
	#[58, -276],
	#[65535, 310, 53, 310, 14, 310, 55, 310, 52, 310, 50, 310, 43, 310, 49, 310, 59, 310, 44, 310, 30, 310, 45, 310, 39, 310, 51, 310, 29, 310, 15, 310, 38, 310, 62, 310, 47, 310, 57, 310, 37, 310, 17, 310, 60, 310, 46, 310, 61, 310, 12, 310, 48, 310, 33, 310, 31, 310, 16, 310, 36, 310, 13, 310, 18, 310, 41, 310, 27, 310, 40, 310, 28, 310, 26, 310, 19, 310, 35, 310, 42, 310, 10, 310, 25, 310, 20, 310, 23, 310, 58, 310, 56, 310, 32, 310, 21, 310, 24, 310, 22, 310, 63, 310, 64, 310, 65, 310, 66, 310, 67, 310, 68, 310, 69, 310],
	#[65535, 348, 49, 348],
	#[65535, 345, 31, 345, 22, 345, 18, 345, 21, 345, 60, 345, 30, 345, 58, 345, 27, 345, 14, 345, 38, 345, 20, 345, 19, 345, 37, 345, 35, 345, 56, 345, 23, 345, 46, 345, 61, 345, 55, 345, 10, 345, 29, 345, 26, 345, 36, 345, 13, 345, 24, 345, 25, 345, 51, 345, 39, 345, 62, 345, 44, 345, 28, 345, 12, 345, 15, 345, 41, 345, 47, 345, 32, 345, 52, 345, 40, 345, 16, 345, 57, 345, 53, 345, 17, 345, 48, 345, 33, 345, 45, 345, 43, 345, 49, 345, 59, 345, 50, 345, 42, 345, 63, 345, 64, 345, 65, 345, 66, 345, 67, 345, 68, 345, 69, 345],
	#[59, -263],
	#[65535, 344, 42, 344, 47, 344, 49, 344, 14, 344, 38, 344, 23, 344, 13, 344, 55, 344, 52, 344, 50, 344, 43, 344, 21, 344, 59, 344, 44, 344, 39, 344, 51, 344, 29, 344, 15, 344, 36, 344, 62, 344, 19, 344, 57, 344, 37, 344, 60, 344, 46, 344, 61, 344, 12, 344, 16, 344, 45, 344, 48, 344, 33, 344, 30, 344, 26, 344, 35, 344, 17, 344, 53, 344, 41, 344, 27, 344, 40, 344, 58, 344, 25, 344, 24, 344, 22, 344, 10, 344, 20, 344, 56, 344, 28, 344, 31, 344, 18, 344, 32, 344, 63, 344, 64, 344, 65, 344, 66, 344, 67, 344, 68, 344, 69, 344],
	#[65535, 343, 32, 343, 31, 343, 21, 343, 62, 343, 20, 343, 39, 343, 19, 343, 22, 343, 28, 343, 18, 343, 53, 343, 27, 343, 30, 343, 37, 343, 23, 343, 25, 343, 56, 343, 55, 343, 14, 343, 24, 343, 45, 343, 60, 343, 44, 343, 29, 343, 41, 343, 58, 343, 13, 343, 46, 343, 50, 343, 52, 343, 36, 343, 10, 343, 51, 343, 15, 343, 61, 343, 26, 343, 47, 343, 40, 343, 12, 343, 38, 343, 48, 343, 33, 343, 49, 343, 17, 343, 16, 343, 57, 343, 35, 343, 43, 343, 59, 343, 42, 343, 63, 343, 64, 343, 65, 343, 66, 343, 67, 343, 68, 343, 69, 343],
	#[65535, 341, 61, 341, 22, 341, 20, 341, 32, 341, 30, 341, 18, 341, 37, 341, 31, 341, 38, 341, 62, 341, 58, 341, 26, 341, 39, 341, 29, 341, 19, 341, 25, 341, 56, 341, 15, 341, 60, 341, 28, 341, 12, 341, 27, 341, 33, 341, 21, 341, 23, 341, 10, 341, 36, 341, 45, 341, 35, 341, 41, 341, 49, 341, 51, 341, 24, 341, 13, 341, 16, 341, 46, 341, 57, 341, 17, 341, 48, 341, 50, 341, 40, 341, 14, 341, 44, 341, 59, 341, 53, 341, 55, 341, 42, 341, 52, 341, 43, 341, 47, 341, 63, 341, 64, 341, 65, 341, 66, 341, 67, 341, 68, 341, 69, 341],
	#[65535, 340, 20, 340, 27, 340, 61, 340, 39, 340, 22, 340, 28, 340, 18, 340, 41, 340, 30, 340, 32, 340, 23, 340, 21, 340, 56, 340, 55, 340, 14, 340, 24, 340, 62, 340, 60, 340, 31, 340, 29, 340, 12, 340, 58, 340, 26, 340, 46, 340, 19, 340, 25, 340, 52, 340, 36, 340, 10, 340, 51, 340, 15, 340, 33, 340, 47, 340, 43, 340, 40, 340, 38, 340, 13, 340, 49, 340, 53, 340, 17, 340, 16, 340, 57, 340, 59, 340, 44, 340, 35, 340, 48, 340, 42, 340, 37, 340, 50, 340, 45, 340, 63, 340, 64, 340, 65, 340, 66, 340, 67, 340, 68, 340, 69, 340],
	#[56, -285],
	#[65535, 323, 47, 323, 49, 323, 14, 323, 23, 323, 55, 323, 52, 323, 22, 323, 50, 323, 43, 323, 21, 323, 59, 323, 44, 323, 30, 323, 45, 323, 39, 323, 51, 323, 29, 323, 15, 323, 38, 323, 62, 323, 19, 323, 57, 323, 37, 323, 60, 323, 46, 323, 61, 323, 12, 323, 16, 323, 17, 323, 48, 323, 31, 323, 26, 323, 36, 323, 13, 323, 18, 323, 53, 323, 33, 323, 41, 323, 27, 323, 40, 323, 58, 323, 25, 323, 24, 323, 35, 323, 42, 323, 10, 323, 20, 323, 56, 323, 28, 323, 32, 323, 63, 323, 64, 323, 65, 323, 66, 323, 67, 323, 68, 323, 69, 323],
	#[65535, 277, 31, 277],
	#[65535, 276, 31, 276],
	#[65535, 273, 31, 273],
	#[55, -242, 30, -37, 53, -80, 59, -263, 31, 259, 57, -254, 38, -91, 62, -85, 45, -252, 43, -249, 10, -262, 49, -74, 44, -250, 37, -104, 54, -84, 23, -10, 61, -81, 52, -79, 36, -105, 20, -62, 35, -101, 41, -115, 29, -32, 51, -77, 40, -261, 42, -246, 28, -28, 48, -72, 39, -94, 21, -63, 50, -76, 18, -54, 15, -113, 32, -99, 24, -14, 34, -58, 14, -109, 19, -56, 13, -107, 12, -106, 17, -118, 25, -18, 33, -3, 22, -66, 46, -96, 16, -117, 26, -19, 27, -24, 47, -98, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 279, 31, 279],
	#[65535, 280, 31, 280],
	#[58, -293],
	#[65535, 300, 15, 300, 23, 300, 28, 300, 45, 300, 39, 300, 44, 300, 43, 300, 61, 300, 59, 300, 22, 300, 47, 300, 29, 300, 20, 300, 27, 300, 13, 300, 30, 300, 55, 300, 50, 300, 18, 300, 37, 300, 42, 300, 12, 300, 14, 300, 38, 300, 62, 300, 26, 300, 52, 300, 36, 300, 48, 300, 46, 300, 10, 300, 16, 300, 35, 300, 53, 300, 57, 300, 51, 300, 54, 300, 33, 300, 24, 300, 49, 300, 19, 300, 34, 300, 32, 300, 31, 300, 17, 300, 25, 300, 41, 300, 21, 300, 40, 300, 63, 300, 64, 300, 65, 300, 66, 300, 67, 300, 68, 300, 69, 300],
	#[65535, 274, 31, 274],
	#[65535, 272, 31, 272],
	#[65535, 262, 31, 262],
	#[56, -298],
	#[65535, 299, 22, 299, 23, 299, 38, 299, 27, 299, 45, 299, 29, 299, 46, 299, 15, 299, 44, 299, 28, 299, 30, 299, 54, 299, 48, 299, 43, 299, 19, 299, 14, 299, 20, 299, 35, 299, 26, 299, 49, 299, 50, 299, 18, 299, 13, 299, 42, 299, 32, 299, 12, 299, 37, 299, 39, 299, 17, 299, 21, 299, 52, 299, 36, 299, 57, 299, 10, 299, 24, 299, 25, 299, 16, 299, 62, 299, 59, 299, 40, 299, 55, 299, 33, 299, 34, 299, 31, 299, 61, 299, 41, 299, 47, 299, 51, 299, 53, 299, 63, 299, 64, 299, 65, 299, 66, 299, 67, 299, 68, 299, 69, 299],
	#[31, 96, 54, -84, 39, -94, 47, -98, 46, -96, 55, -75, 30, -37, 52, -79, 53, -80, 38, -91, 62, -85, 61, -81, 59, -119, 45, -92, 10, -114, 36, -105, 20, -62, 41, -115, 32, -99, 57, -95, 43, -87, 50, -76, 18, -54, 37, -104, 42, -82, 35, -101, 14, -109, 34, -58, 51, -77, 25, -18, 40, -112, 22, -66, 17, -118, 16, -117, 15, -113, 49, -74, 44, -89, 28, -28, 27, -24, 33, -3, 24, -14, 23, -10, 19, -56, 29, -32, 48, -72, 13, -107, 12, -106, 21, -63, 26, -19, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[55, -242, 45, -252, 31, 259, 54, -84, 43, -249, 47, -98, 42, -246, 30, -37, 53, -80, 50, -76, 37, -104, 41, -115, 10, -262, 40, -261, 38, -91, 51, -77, 26, -19, 52, -79, 36, -105, 39, -94, 22, -66, 46, -96, 25, -18, 35, -101, 49, -74, 44, -250, 12, -106, 27, -24, 24, -14, 23, -10, 16, -117, 34, -58, 20, -62, 29, -32, 48, -72, 18, -54, 13, -107, 28, -28, 19, -56, 17, -118, 21, -63, 33, -3, 15, -113, 32, -99, 14, -109, 57, -254, 59, -263, 61, -81, 62, -85, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[31, 96, 54, -84, 39, -94, 47, -98, 55, -75, 30, -37, 52, -79, 36, -105, 53, -80, 38, -91, 62, -85, 61, -81, 59, -119, 46, -96, 10, -114, 29, -32, 41, -115, 32, -99, 57, -95, 43, -87, 50, -76, 37, -104, 42, -82, 35, -101, 14, -109, 34, -58, 51, -77, 26, -19, 45, -92, 20, -62, 22, -66, 17, -118, 40, -112, 16, -117, 15, -113, 49, -74, 44, -89, 28, -28, 25, -18, 33, -3, 24, -14, 23, -10, 19, -56, 48, -72, 13, -107, 12, -106, 21, -63, 18, -54, 27, -24, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[58, 157, 31, 157, 60, 157, 2, 157, 40, -304, 11, 157, 10, 157, 56, 157],
	#[65535, 155, 31, 155, 10, 155, 2, 155, 11, 155, 56, 155, 58, 155, 60, 155],
	#[22, -66, 20, -62, 23, -10, 28, -28, 53, -20, 21, -63, 33, -3, 29, -32, 18, -54, 34, -58],
	#[65535, 158, 31, 158, 2, 158, 58, 158, 60, 158, 10, 158, 11, 158, 56, 158],
	#[31, -308],
	#[65535, 161, 40, 161, 10, 161, 2, 161, 58, 161, 31, 161, 60, 161, 56, 161, 11, 161],
	#[23, -313, 29, -32, 31, 171, 60, 171, 22, -66, 28, -28, 21, -63, 33, -3, 20, -315, 34, -58, 2, 171, 40, 171, 18, -54, 11, 171, 10, 171, 56, 171, 25, -159, 58, 171],
	#[65535, 43, 11, 43, 31, 43, 2, 43, 58, 43, 40, 43, 10, 43, 60, 43, 56, 43],
	#[65535, 45, 31, 45, 60, 45, 2, 45, 11, 45, 10, 45, 40, 45, 56, 45, 58, 45],
	#[65535, 44, 11, 44, 40, 44, 10, 44, 2, 44, 60, 44, 31, 44, 56, 44, 58, 44],
	#[23, -10, 28, -28, 21, -63, 22, -66, 29, -32, 20, -62, 18, -54, 33, -3, 34, -58],
	#[23, 59, 28, 59, 22, 59, 21, 59, 2, 49, 29, 59, 10, 49, 33, 59, 34, 59, 58, 49, 20, 59, 31, 49, 40, 49, 56, 49, 11, 49, 18, 59, 60, 49],
	#[65535, 172, 2, 172, 10, 172, 11, 172, 31, 172, 40, 172, 56, 172, 58, 172, 60, 172],
	#[22, 57, 29, 57, 23, 57, 2, 34, 31, 34, 18, 57, 10, 34, 34, 57, 20, 57, 33, 57, 28, 57, 21, 57, 11, 34, 40, 34, 56, 34, 58, 34, 60, 34],
	#[65535, 173, 11, 173, 2, 173, 10, 173, 31, 173, 40, 173, 56, 173, 58, 173, 60, 173],
	#[65535, 111, 2, 111, 58, 111, 31, 111, 56, 111, 60, 111],
	#[65535, 99, 31, 99, 60, 99, 58, 99, 2, 99, 56, 99],
	#[65535, 109, 60, 109, 58, 109, 2, 109, 56, 109, 31, 109],
	#[56, -321],
	#[65535, 146, 54, 146, 47, 146, 45, 146, 28, 146, 29, 146, 23, 146, 13, 146, 52, 146, 26, 146, 22, 146, 46, 146, 18, 146, 43, 146, 42, 146, 44, 146, 30, 146, 12, 146, 39, 146, 50, 146, 27, 146, 15, 146, 38, 146, 62, 146, 49, 146, 37, 146, 35, 146, 32, 146, 51, 146, 55, 146, 16, 146, 17, 146, 48, 146, 31, 146, 59, 146, 57, 146, 20, 146, 24, 146, 53, 146, 14, 146, 2, 146, 21, 146, 25, 146, 61, 146, 41, 146, 56, 146, 10, 146, 36, 146, 40, 146, 60, 146, 33, 146, 11, 146, 58, 146, 34, 146, 19, 146, 63, 146, 64, 146, 65, 146, 66, 146, 67, 146, 68, 146, 69, 146],
	#[65535, 112, 58, 112, 2, 112, 60, 112, 31, 112, 56, 112],
	#[31, -325],
	#[65535, 236, 55, 236, 38, 236, 11, 236, 20, 236, 46, 236, 19, 236, 30, 236, 39, 236, 57, 236, 62, 236, 47, 236, 59, 236, 37, 236, 21, 236, 41, 236, 61, 236, 12, 236, 10, 236, 34, 236, 24, 236, 31, 236, 52, 236, 36, 236, 35, 236, 17, 236, 25, 236, 42, 236, 14, 236, 44, 236, 16, 236, 26, 236, 15, 236, 40, 236, 45, 236, 51, 236, 33, 236, 13, 236, 54, 236, 50, 236, 60, 236, 2, 236, 48, 236, 27, 236, 43, 236, 32, 236, 58, 236, 49, 236, 56, 236, 53, 236, 63, 236, 64, 236, 65, 236, 66, 236, 67, 236, 68, 236],
	#[39, 46, 55, 46, 28, -28, 57, 46, 37, 46, 21, 46, 36, 46, 20, 46, 34, 46, 29, -32, 35, 46, 15, 46, 10, 46, 54, 46, 17, 46, 19, 46, 61, 46, 14, 46, 38, 46, 62, 46, 51, 46, 33, 46, 41, 46, 40, 46, 43, 46, 46, 46, 53, 46, 59, 46, 52, 46, 16, 46, 49, 46, 11, 46, 30, 46, 2, 46, 27, 46, 32, 46, 47, 46, 44, 46, 12, 46, 42, 46, 25, 46, 24, 46, 13, 46, 58, 46, 26, 46, 22, -66, 50, 46, 31, 46, 60, 46, 56, 46, 18, -54, 48, 46, 23, -10, 45, 46, 63, 46, 64, 46, 65, 46, 66, 46, 67, 46, 68, 46],
	#[65535, 47, 60, 47, 54, 47, 58, 47, 47, 47, 45, 47, 14, 47, 38, 47, 2, 47, 52, 47, 36, 47, 31, 47, 43, 47, 42, 47, 61, 47, 30, 47, 12, 47, 39, 47, 50, 47, 27, 47, 15, 47, 62, 47, 48, 47, 37, 47, 26, 47, 56, 47, 32, 47, 46, 47, 55, 47, 16, 47, 17, 47, 25, 47, 24, 47, 41, 47, 59, 47, 57, 47, 35, 47, 11, 47, 10, 47, 44, 47, 21, 47, 33, 47, 20, 47, 13, 47, 40, 47, 19, 47, 34, 47, 49, 47, 51, 47, 53, 47, 63, 47, 64, 47, 65, 47, 66, 47, 67, 47, 68, 47],
	#[65535, 240, 45, 240, 52, 240, 31, 240, 43, 240, 42, 240, 44, 240, 12, 240, 26, 240, 51, 240, 27, 240, 13, 240, 38, 240, 47, 240, 37, 240, 35, 240, 21, 240, 55, 240, 48, 240, 2, 240, 30, 240, 54, 240, 20, 240, 53, 240, 16, 240, 14, 240, 39, 240, 24, 240, 25, 240, 15, 240, 41, 240, 49, 240, 46, 240, 32, 240, 11, 240, 36, 240, 40, 240, 19, 240, 10, 240, 17, 240, 33, 240, 34, 240, 50, 240, 56, 240, 57, 240, 58, 240, 59, 240, 60, 240, 61, 240, 62, 240, 63, 240, 64, 240, 65, 240, 66, 240, 67, 240, 68, 240],
	#[65535, 239, 31, 239, 30, 239, 15, 239, 47, 239, 20, 239, 27, 239, 13, 239, 26, 239, 43, 239, 19, 239, 44, 239, 42, 239, 12, 239, 14, 239, 24, 239, 45, 239, 36, 239, 39, 239, 46, 239, 16, 239, 35, 239, 2, 239, 40, 239, 17, 239, 25, 239, 48, 239, 21, 239, 38, 239, 41, 239, 11, 239, 34, 239, 37, 239, 33, 239, 32, 239, 10, 239, 49, 239, 50, 239, 51, 239, 52, 239, 53, 239, 54, 239, 55, 239, 56, 239, 57, 239, 58, 239, 59, 239, 60, 239, 61, 239, 62, 239, 63, 239, 64, 239, 65, 239, 66, 239, 67, 239, 68, 239],
	#[31, -308],
	#[65535, 160, 31, 160, 58, 160, 60, 160, 56, 160, 2, 160, 40, 160, 11, 160, 10, 160],
	#[31, -308],
	#[65535, 159, 31, 159, 11, 159, 10, 159, 2, 159, 40, 159, 60, 159, 58, 159, 56, 159],
	#[65535, 237, 63, 237, 64, 237, 65, 237, 66, 237, 67, 237, 68, 237, 46, 237, 44, 237, 55, 237, 36, 237, 14, 237, 21, 237, 39, 237, 10, 237, 11, 237, 34, 237, 13, 237, 20, 237, 37, 237, 38, 237, 12, 237, 62, 237, 15, 237, 52, 237, 47, 237, 33, 237, 41, 237, 57, 237, 40, 237, 16, 237, 30, 237, 19, 237, 53, 237, 2, 237, 35, 237, 31, 237, 25, 237, 51, 237, 32, 237, 61, 237, 24, 237, 43, 237, 45, 237, 59, 237, 49, 237, 42, 237, 27, 237, 54, 237, 17, 237, 50, 237, 48, 237, 60, 237, 58, 237, 56, 237, 26, 237],
	#[13, -55, 62, -25, 53, -20, 43, -26, 19, -56, 21, -63, 61, -21, 36, -52, 20, -62, 24, -14, 29, -32, 42, -22, 55, -8, 28, -28, 12, -51, 35, -43, 25, -18, 41, -65, 33, -3, 34, -58, 23, -10, 18, -54, 22, -66, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 228, 38, 228, 10, 228, 39, 228, 37, 228, 2, 228, 40, 228, 36, 228, 48, 228, 56, 228, 58, 228, 11, 228],
	#[34, -58, 21, -63, 33, -3, 20, -62, 53, -20],
	#[62, -25, 13, -55, 36, -52, 28, -28, 12, -51, 43, -26, 23, -10, 55, -8, 42, -22, 56, 213, 24, -14, 22, -66, 53, -20, 18, -54, 35, -43, 21, -63, 33, -3, 41, -340, 19, -56, 29, -32, 20, -62, 61, -21, 34, -58, 25, -18, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[23, -10, 61, -21, 62, -25, 28, -28, 12, -51, 22, -66, 35, -43, 43, -26, 19, -56, 33, -3, 13, -55, 55, -8, 53, -20, 36, -52, 20, -62, 41, -340, 42, -22, 34, -58, 29, -32, 18, -54, 25, -18, 24, -14, 21, -63, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 180, 40, 180, 56, 180, 58, 180],
	#[41, -65, 42, -22, 23, -10, 13, -55, 55, -8, 36, -52, 22, -66, 43, -26, 19, -56, 34, -58, 28, -28, 12, -51, 40, 184, 18, -54, 24, -14, 33, -3, 53, -20, 35, -43, 21, -63, 20, -62, 25, -18, 29, -32, 56, 184, 58, 184, 61, -21, 62, -25, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 230, 38, 230, 39, 230, 37, 230, 40, 230, 58, 230, 36, 230, 56, 230],
	#[58, -358],
	#[65535, 183, 40, 183, 56, 183, 58, 183],
	#[58, 179, 40, -353, 56, 179],
	#[39, -202, 37, -214, 58, 229, 38, -197, 36, -213, 40, 229, 56, 229],
	#[53, -20, 55, -8, 62, -25, 20, -62, 42, -22, 13, -55, 35, -43, 12, -51, 24, -14, 23, -10, 61, -21, 36, -52, 29, -32, 18, -54, 22, -66, 28, -28, 43, -26, 19, -56, 21, -63, 41, -65, 25, -18, 33, -3, 34, -58, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 231, 37, 231, 39, 231, 36, 231, 38, 231, 58, 231, 56, 231, 40, 231],
	#[56, -349],
	#[65535, 188, 55, 188, 39, 188, 38, 188, 11, 188, 46, 188, 36, 188, 2, 188, 48, 188, 10, 188, 37, 188, 40, 188, 56, 188, 57, 188, 58, 188],
	#[55, -337, 38, 235, 40, 235, 46, -336, 39, 235, 2, 235, 10, 235, 58, 235, 36, 235, 48, 235, 56, 235, 57, -338, 37, 235, 11, 235],
	#[56, -356],
	#[65535, 214, 56, 214],
	#[23, -10, 13, -55, 42, -22, 36, -52, 43, -26, 29, -32, 24, -14, 35, -43, 12, -51, 25, -18, 18, -54, 21, -63, 20, -62, 22, -66, 28, -28, 34, -58, 19, -56, 33, -3, 41, -340, 53, -20, 55, -8, 61, -21, 62, -25, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 181, 58, 181, 56, 181, 40, 181],
	#[65535, 182, 40, 182, 56, 182, 58, 182],
	#[65535, 174, 55, 174, 39, 174, 36, 174, 37, 174, 46, 174, 10, 174, 48, 174, 2, 174, 40, 174, 11, 174, 38, 174, 56, 174, 57, 174, 58, 174],
	#[65535, 177, 10, 177, 2, 177, 36, 177, 55, 177, 38, 177, 11, 177, 58, 177, 57, 177, 46, 177, 39, 177, 37, 177, 48, 177, 40, 177, 56, 177],
	#[38, 175, 39, 175, 46, 175, 2, 175, 11, 175, 32, -359, 36, 175, 48, 175, 40, 175, 10, 175, 37, 175, 55, 175, 56, 175, 57, 175, 58, 175],
	#[13, -55, 43, -26, 23, -10, 21, -63, 55, -8, 62, -25, 28, -28, 12, -51, 22, -66, 18, -54, 42, -22, 20, -62, 41, -361, 19, -56, 29, -32, 33, -3, 53, -20, 61, -21, 34, -58, 25, -18, 24, -14, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 176, 38, 176, 37, 176, 46, 176, 55, 176, 39, 176, 48, 176, 10, 176, 36, 176, 2, 176, 40, 176, 11, 176, 56, 176, 57, 176, 58, 176],
	#[65535, 216, 37, 216, 55, 216, 38, 216, 46, 216, 39, 216, 36, 216, 2, 216, 58, 216, 48, 216, 10, 216, 57, 216, 40, 216, 56, 216, 11, 216],
	#[65535, 215, 39, 215, 11, 215, 38, 215, 2, 215, 37, 215, 46, 215, 55, 215, 10, 215, 36, 215, 48, 215, 40, 215, 56, 215, 57, 215, 58, 215],
	#[65535, 356, 15, 356, 49, 356, 50, 356, 42, 356, 55, 356, 54, 356, 12, 356, 14, 356, 21, 356, 36, 356, 61, 356, 43, 356, 45, 356, 59, 356, 29, 356, 57, 356, 28, 356, 13, 356, 51, 356, 47, 356, 30, 356, 37, 356, 52, 356, 23, 356, 20, 356, 16, 356, 41, 356, 35, 356, 38, 356, 62, 356, 10, 356, 44, 356, 27, 356, 40, 356, 22, 356, 46, 356, 53, 356, 18, 356, 39, 356, 48, 356, 26, 356, 19, 356, 32, 356, 33, 356, 58, 356, 25, 356, 31, 356, 60, 356, 56, 356, 17, 356, 24, 356, 63, 356, 64, 356, 65, 356, 66, 356, 67, 356, 68, 356, 69, 356],
	#[65535, 339, 14, 339, 20, 339, 60, 339, 30, 339, 32, 339, 39, 339, 53, 339, 15, 339, 21, 339, 58, 339, 28, 339, 12, 339, 23, 339, 18, 339, 19, 339, 56, 339, 29, 339, 36, 339, 13, 339, 31, 339, 27, 339, 38, 339, 62, 339, 10, 339, 33, 339, 41, 339, 22, 339, 17, 339, 26, 339, 55, 339, 61, 339, 24, 339, 37, 339, 40, 339, 16, 339, 59, 339, 52, 339, 47, 339, 46, 339, 51, 339, 57, 339, 25, 339, 44, 339, 35, 339, 48, 339, 49, 339, 42, 339, 45, 339, 50, 339, 43, 339, 63, 339, 64, 339, 65, 339, 66, 339, 67, 339, 68, 339, 69, 339],
	#[63, -367],
	#[65535, 350, 31, 350, 15, 350, 18, 350, 55, 350, 20, 350, 58, 350, 30, 350, 29, 350, 28, 350, 13, 350, 39, 350, 38, 350, 62, 350, 21, 350, 22, 350, 27, 350, 23, 350, 37, 350, 61, 350, 56, 350, 32, 350, 36, 350, 24, 350, 17, 350, 10, 350, 60, 350, 53, 350, 14, 350, 12, 350, 19, 350, 59, 350, 52, 350, 47, 350, 46, 350, 26, 350, 57, 350, 40, 350, 16, 350, 25, 350, 44, 350, 41, 350, 35, 350, 48, 350, 51, 350, 49, 350, 42, 350, 45, 350, 33, 350, 50, 350, 43, 350, 63, 350, 64, 350, 65, 350, 66, 350, 67, 350, 68, 350, 69, 350],
	#[65535, 351, 53, 351, 50, 351, 13, 351, 12, 351, 14, 351, 21, 351, 36, 351, 42, 351, 43, 351, 39, 351, 26, 351, 44, 351, 15, 351, 17, 351, 51, 351, 47, 351, 37, 351, 48, 351, 23, 351, 20, 351, 16, 351, 55, 351, 35, 351, 38, 351, 45, 351, 10, 351, 31, 351, 28, 351, 49, 351, 22, 351, 46, 351, 19, 351, 18, 351, 41, 351, 30, 351, 40, 351, 27, 351, 24, 351, 29, 351, 52, 351, 25, 351, 32, 351, 33, 351, 56, 351, 57, 351, 58, 351, 59, 351, 60, 351, 61, 351, 62, 351, 63, 351, 64, 351, 65, 351, 66, 351, 67, 351, 68, 351, 69, 351],
	#[65535, 325, 15, 325, 51, 325, 50, 325, 29, 325, 44, 325, 14, 325, 21, 325, 36, 325, 42, 325, 43, 325, 45, 325, 59, 325, 26, 325, 57, 325, 28, 325, 13, 325, 25, 325, 47, 325, 30, 325, 37, 325, 52, 325, 23, 325, 20, 325, 16, 325, 55, 325, 35, 325, 38, 325, 62, 325, 10, 325, 31, 325, 27, 325, 12, 325, 49, 325, 22, 325, 46, 325, 53, 325, 18, 325, 39, 325, 48, 325, 40, 325, 32, 325, 41, 325, 19, 325, 33, 325, 58, 325, 61, 325, 56, 325, 17, 325, 24, 325, 60, 325, 63, 325, 64, 325, 65, 325, 66, 325, 67, 325, 68, 325, 69, 325],
	#[65535, 238, 39, 238, 55, 238, 38, 238, 54, 238, 37, 238, 34, 238, 51, 238, 36, 238, 20, 238, 17, 238, 10, 238, 15, 238, 11, 238, 47, 238, 12, 238, 50, 238, 33, 238, 21, 238, 52, 238, 14, 238, 45, 238, 35, 238, 41, 238, 31, 238, 24, 238, 13, 238, 44, 238, 53, 238, 43, 238, 19, 238, 16, 238, 26, 238, 48, 238, 30, 238, 49, 238, 32, 238, 27, 238, 25, 238, 2, 238, 40, 238, 42, 238, 46, 238, 56, 238, 57, 238, 58, 238, 59, 238, 60, 238, 61, 238, 62, 238, 63, 238, 64, 238, 65, 238, 66, 238, 67, 238, 68, 238],
	#[65535, 119, 11, 119, 2, 119, 31, 119, 10, 119, 56, 119, 58, 119, 60, 119],
	#[65535, 103, 2, 103, 31, 103, 11, 103, 10, 103, 56, 103, 58, 103, 60, 103],
	#[65535, 120, 58, 120, 10, 120, 31, 120, 11, 120, 2, 120, 56, 120, 60, 120],
	#[31, -308],
	#[65535, 163, 11, 163, 31, 163, 10, 163, 2, 163, 56, 163, 58, 163, 60, 163],
	#[55, -242, 47, -98, 46, -96, 42, -246, 45, -252, 31, 259, 54, -84, 51, -77, 39, -94, 44, -250, 43, -249, 35, -101, 32, -99, 10, -262, 29, -32, 40, -261, 27, -24, 49, -74, 30, -37, 53, -80, 50, -76, 19, -56, 37, -104, 41, -115, 24, -14, 21, -63, 38, -91, 17, -118, 33, -3, 52, -79, 36, -105, 18, -54, 22, -66, 25, -18, 13, -107, 34, -58, 48, -72, 28, -28, 23, -10, 14, -109, 20, -62, 26, -19, 16, -117, 12, -106, 15, -113, 57, -254, 59, -263, 61, -81, 62, -85, 63, -88, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[31, -378],
	#[65535, 165, 11, 165, 31, 165, 10, 165, 2, 165, 56, 165, 58, 165, 60, 165],
	#[21, -63, 11, 357, 29, -381, 28, -28, 18, -54, 34, -379, 2, 357, 33, -3, 31, 357, 22, -66, 20, -62, 10, 357, 23, -10, 56, 357, 58, 357, 60, 357],
	#[29, 41, 18, 41, 23, 41, 34, 41, 33, 41, 11, 38, 31, 38, 22, 41, 21, 41, 20, 41, 10, 38, 2, 38, 28, 41, 56, 38, 58, 38, 60, 38],
	#[28, -28, 23, -10, 29, -32, 18, -54, 22, -66, 34, -58, 21, -63, 20, -62, 33, -3],
	#[11, 54, 60, 54, 2, 54, 28, 42, 22, 42, 10, 54, 29, 42, 20, 42, 23, 42, 18, 42, 31, 54, 21, 42, 33, 42, 56, 54, 34, 42, 58, 54],
	#[65535, 358, 31, 358, 10, 358, 60, 358, 56, 358, 58, 358, 11, 358, 2, 358],
	#[65535, 359, 2, 359, 31, 359, 11, 359, 10, 359, 58, 359, 60, 359, 56, 359],
	#[65535, 170, 22, 170, 21, 170, 34, 170, 23, 170, 29, 170, 20, 170, 24, 170, 25, 170],
	#[65535, 148, 23, 148, 46, 148, 28, 148, 45, 148, 43, 148, 12, 148, 15, 148, 22, 148, 47, 148, 29, 148, 27, 148, 30, 148, 55, 148, 50, 148, 18, 148, 44, 148, 42, 148, 35, 148, 14, 148, 38, 148, 26, 148, 52, 148, 36, 148, 39, 148, 48, 148, 19, 148, 10, 148, 13, 148, 34, 148, 53, 148, 11, 148, 51, 148, 37, 148, 54, 148, 21, 148, 49, 148, 2, 148, 33, 148, 16, 148, 31, 148, 17, 148, 25, 148, 41, 148, 20, 148, 32, 148, 24, 148, 40, 148, 56, 148, 57, 148, 58, 148, 59, 148, 60, 148, 61, 148, 62, 148, 63, 148, 64, 148, 65, 148, 66, 148, 67, 148, 68, 148, 69, 148],
	#[65535, 8, #"eoi", 8],
	#[65535, 91, 2, 91],
	#[2, -402],
	#[47, -98, 46, -96, 55, -75, 54, -84, 53, -80, 39, -94, 44, -89, 43, -87, 49, -74, 35, -101, 32, -99, 52, -79, 24, -14, 50, -76, 19, -56, 37, -104, 42, -82, 21, -63, 38, -91, 51, -77, 33, -3, 45, -92, 36, -105, 18, -54, 29, -32, 25, -18, 13, -107, 34, -58, 48, -72, 28, -28, 17, -118, 23, -10, 14, -109, 20, -62, 41, -115, 22, -66, 12, -106, 16, -117, 15, -113, 57, -95, 59, -119, 61, -81, 62, -85, 63, -88, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[40, -391, 2, 92],
	#[41, -389],
	#[65535, 93, 2, 93],
	#[47, -98, 55, -75, 67, -35, 54, -84, 36, -105, 63, -88, 53, -80, 66, -31, 62, -85, 2, 104, 21, -63, 32, -99, 61, -81, 46, -96, 42, -82, 29, -32, 40, 104, 41, -115, 49, -74, 43, -87, 50, -76, 19, -56, 37, -104, 28, -28, 35, -101, 14, -109, 38, -91, 51, -77, 68, -41, 52, -79, 39, -94, 22, -66, 59, -119, 13, -107, 15, -113, 48, -72, 44, -89, 12, -106, 18, -54, 23, -10, 20, -62, 33, -3, 16, -117, 24, -14, 45, -92, 17, -118, 34, -58, 25, -18, 57, -95, 65, -47, 69, -34],
	#[65535, 94, 2, 94, 40, 94],
	#[2, 122, 54, -84, 47, -98, 38, -91, 55, -75, 52, -79, 41, -115, 40, 122, 35, -101, 45, -92, 39, -94, 51, -77, 50, -76, 32, -99, 15, -113, 53, -80, 48, -72, 37, -104, 17, -118, 21, -63, 46, -96, 12, -106, 43, -87, 33, -3, 19, -56, 49, -74, 36, -105, 13, -107, 25, -18, 16, -117, 14, -109, 34, -58, 20, -62, 24, -14, 42, -82, 44, -89, 57, -95, 59, -119, 61, -81, 62, -85, 63, -88, 65, -47, 66, -31, 67, -35, 68, -41],
	#[65535, 106, 40, 106, 2, 106],
	#[65535, 95, 2, 95, 40, 95],
	#[65535, 123, 2, 123, 40, 123],
	#[65535, 107, 2, 107, 40, 107],
	#[65535, 124, 2, 124, 40, 124],
	#[65535, 105, 40, 105, 2, 105],
	#[65535, 7, #"eoi", 7],
	#[22, -66, 30, -37, 2, 253, 27, -24, 41, -65, 12, -51, 29, -32, 23, -10, 35, -43, 13, -55, 55, -8, 53, -20, 20, -62, 43, -26, 42, -22, 19, -56, 25, -18, 24, -14, 18, -54, 36, -52, 34, -58, 26, -19, 33, -3, 28, -28, 10, -431, 21, -63, 61, -21, 62, -25, 63, -30, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 242, 2, 242],
	#[63, -30, 68, -41, 22, -66, 19, -56, 28, -28, 12, -51, 13, -55, 36, -52, 24, -14, 23, -10, 18, -54, 55, -8, 43, -26, 29, -32, 35, -43, 33, -3, 34, -58, 21, -63, 25, -18, 42, -22, 20, -62, 41, -65, 53, -20, 61, -21, 62, -25, 65, -47, 66, -31, 67, -35, 69, -34],
	#[48, -423],
	#[2, -440],
	#[22, 257, 12, 257, 30, 257, 27, 257, 26, 257, 36, 257, 20, 257, 41, 257, 13, 257, 23, 257, 55, 257, 42, 257, 61, 257, 19, 257, 29, 257, 25, 257, 10, 257, 35, 257, 48, -422, 33, 257, 18, 257, 43, 257, 34, 257, 2, 257, 24, 257, 28, 257, 53, 257, 21, 257, 62, 257, 63, 257, 64, 257, 65, 257, 66, 257, 67, 257, 68, 257, 69, 257],
	#[13, -55, 2, 253, 36, -52, 61, -21, 19, -56, 35, -43, 12, -51, 30, -37, 43, -26, 26, -19, 55, -8, 18, -54, 42, -22, 10, -418, 28, -28, 24, -14, 22, -66, 25, -18, 33, -3, 41, -65, 29, -32, 23, -10, 53, -20, 20, -62, 21, -63, 34, -58, 27, -24, 62, -25, 63, -30, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[48, 224, 56, 224, 40, -412],
	#[65535, 244, 2, 244],
	#[23, -10, 36, -52, 13, -55, 42, -22, 41, -65, 63, -30, 22, -66, 53, -20, 35, -43, 21, -63, 25, -18, 24, -14, 12, -51, 29, -32, 34, -58, 55, -8, 20, -62, 43, -26, 19, -56, 28, -28, 18, -54, 33, -3, 61, -21, 62, -25, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 225, 48, 225, 56, 225],
	#[65535, 251, 2, 251],
	#[65535, 243, 2, 243],
	#[10, -438, 2, 217],
	#[65535, 249, 2, 249],
	#[55, -405, 23, -10, 13, -55, 36, -52, 2, 252, 22, -66, 12, -51, 53, -20, 43, -26, 19, -56, 41, -65, 35, -43, 24, -14, 42, -22, 25, -18, 33, -3, 32, -408, 21, -63, 18, -54, 29, -32, 34, -58, 20, -62, 28, -28, 61, -21, 62, -25, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 85, 2, 85, 10, 85],
	#[65535, 247, 2, 247],
	#[65535, 256, 13, 256, 12, 256, 21, 256, 22, 256, 23, 256, 55, 256, 53, 256, 18, 256, 62, 256, 61, 256, 43, 256, 2, 256, 41, 256, 29, 256, 28, 256, 24, 256, 30, 256, 34, 256, 36, 256, 20, 256, 35, 256, 42, 256, 10, 256, 25, 256, 27, 256, 26, 256, 19, 256, 33, 256, 63, 256, 64, 256, 65, 256, 66, 256, 67, 256, 68, 256, 69, 256],
	#[65535, 258, 23, 258, 21, 258, 2, 258, 22, 258, 28, 258, 13, 258, 53, 258, 34, 258, 43, 258, 36, 258, 26, 258, 35, 258, 10, 258, 25, 258, 29, 258, 12, 258, 33, 258, 41, 258, 20, 258, 24, 258, 18, 258, 19, 258, 42, 258, 55, 258, 27, 258, 30, 258, 61, 258, 62, 258, 63, 258, 64, 258, 65, 258, 66, 258, 67, 258, 68, 258, 69, 258],
	#[65535, 254, 13, 254, 61, 254, 22, 254, 29, 254, 28, 254, 12, 254, 10, 254, 34, 254, 2, 254, 36, 254, 21, 254, 55, 254, 62, 254, 27, 254, 41, 254, 43, 254, 65, 254, 19, 254, 25, 254, 20, 254, 66, 254, 30, 254, 33, 254, 18, 254, 23, 254, 35, 254, 26, 254, 42, 254, 24, 254, 69, 254, 53, 254, 63, 254, 64, 254, 67, 254, 68, 254],
	#[40, -425, 56, -349],
	#[20, -62, 29, -32, 53, -20, 36, -52, 43, -26, 12, -51, 23, -10, 42, -22, 41, -65, 13, -55, 28, -28, 25, -18, 24, -14, 19, -56, 21, -63, 34, -58, 22, -66, 18, -54, 55, -8, 33, -3, 35, -43, 61, -21, 62, -25, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[56, -427],
	#[48, -428],
	#[65535, 255, 53, 255, 33, 255, 13, 255, 34, 255, 23, 255, 55, 255, 12, 255, 66, 255, 2, 255, 22, 255, 18, 255, 28, 255, 21, 255, 19, 255, 25, 255, 29, 255, 67, 255, 10, 255, 36, 255, 43, 255, 42, 255, 24, 255, 41, 255, 64, 255, 27, 255, 20, 255, 26, 255, 30, 255, 35, 255, 61, 255, 62, 255, 63, 255, 65, 255, 68, 255, 69, 255],
	#[2, 217, 10, -434],
	#[65535, 250, 2, 250],
	#[13, -55, 23, -10, 34, -58, 22, -66, 43, -26, 21, -63, 19, -56, 12, -51, 41, -65, 29, -32, 36, -52, 2, 252, 35, -43, 18, -54, 28, -28, 42, -22, 24, -14, 20, -62, 25, -18, 33, -3, 32, -408, 53, -20, 55, -405, 61, -21, 62, -25, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 245, 13, 245, 23, 245, 21, 245, 10, 245, 12, 245, 43, 245, 20, 245, 24, 245, 33, 245, 28, 245, 19, 245, 29, 245, 35, 245, 34, 245, 36, 245, 22, 245, 18, 245, 42, 245, 30, 245, 27, 245, 26, 245, 41, 245, 2, 245, 25, 245, 53, 245, 55, 245, 61, 245, 62, 245, 63, 245, 64, 245, 65, 245, 66, 245, 67, 245, 68, 245, 69, 245],
	#[65535, 82, 2, 82],
	#[30, -37, 23, -10, 22, -66, 35, -43, 13, -55, 20, -62, 26, -19, 21, -63, 28, -28, 12, -51, 43, -26, 19, -56, 36, -52, 18, -54, 25, -18, 42, -22, 32, -408, 27, -24, 24, -14, 2, 218, 33, -3, 29, -32, 34, -58, 41, -65, 53, -20, 55, -405, 61, -21, 62, -25, 63, -30, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 246, 20, 246, 23, 246, 30, 246, 43, 246, 2, 246, 27, 246, 28, 246, 55, 246, 35, 246, 36, 246, 26, 246, 25, 246, 19, 246, 24, 246, 22, 246, 18, 246, 42, 246, 33, 246, 34, 246, 13, 246, 53, 246, 41, 246, 29, 246, 10, 246, 12, 246, 21, 246, 61, 246, 62, 246, 63, 246, 64, 246, 65, 246, 66, 246, 67, 246, 68, 246, 69, 246],
	#[2, 89, 10, 89, 40, -412, 48, 224],
	#[65535, 86, 10, 86, 2, 86],
	#[22, -66, 30, -37, 23, -10, 20, -62, 32, -408, 35, -43, 12, -51, 29, -32, 27, -24, 13, -55, 43, -26, 19, -56, 2, 218, 24, -14, 26, -19, 41, -65, 28, -28, 34, -58, 33, -3, 36, -52, 18, -54, 25, -18, 21, -63, 42, -22, 53, -20, 55, -405, 61, -21, 62, -25, 63, -30, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 248, 2, 248],
	#[65535, 6, #"eoi", 6],
	#[65535, 81, 2, 81],
	#[2, 217, 10, -445],
	#[2, -444],
	#[65535, 5, #"eoi", 5],
	#[22, -66, 42, -22, 30, -37, 27, -24, 43, -26, 12, -51, 29, -32, 23, -10, 13, -55, 2, 218, 36, -52, 20, -62, 21, -63, 19, -56, 34, -58, 35, -43, 26, -19, 24, -14, 18, -54, 28, -28, 33, -3, 25, -18, 41, -65, 53, -20, 55, -8, 61, -21, 62, -25, 63, -30, 64, -27, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[47, -449, 2, 219],
	#[2, -448],
	#[65535, 4, #"eoi", 4],
	#[28, -28, 62, -25, 55, -8, 13, -55, 43, -26, 22, -66, 61, -21, 20, -62, 18, -54, 42, -22, 12, -51, 24, -14, 23, -10, 21, -63, 25, -18, 33, -3, 29, -32, 34, -58, 53, -20, 19, -56, 63, -30, 65, -47, 66, -31, 67, -35, 68, -41, 69, -34],
	#[65535, 220, 2, 220],
	#[55, -337, 46, -336, 2, 223, 57, -338],
	#[2, -453],
	#[65535, 3, #"eoi", 3],
	#[2, -455],
	#[65535, 2, #"eoi", 2],
	#[2, -457],
	#[65535, 1, #"eoi", 1]],
  goto-table:
      #[#[12, 50, 28, 27, 41, 64, 43, 25, 24, 13, 36, 51, 30, 36, 9, 63, 34, 57, 42, 21, 13, 54, 18, 53, 35, 42, 25, 17, 23, 9, 21, 62, 33, 2, 0, 39, 53, 19, 22, 65, 20, 61, 156, 22, 155, 52, 154, 11, 151, 32, 150, 60, 147, 4, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 130, 10, 125, 14, 121, 28, 124, 44, 88, 6, 98, 66, 83, 12, 89, 47, 81, 1, 90, 16, 75, 37, 71, 5, 69, 33, 68, 40, 66, 30, 64, 26, 63, 29, 62, 24, 61, 20, 87, 38, 55, 7, 67, 34, 65, 46, 76, 8, 26, 18, 19, 55, 27, 23, 11, 67, 29, 31, 8, 58, 7, 56, 6, 49, 5, 48, 4, 43, 3, 41],
	#[156, 82, 134, 92, 138, 109, 120, 107, 119, 77, 118, 99, 121, 85, 124, 101, 125, 14, 104, 96, 103, 322, 117, 70, 92, 89, 89, 47, 88, 6, 110, 115, 83, 12, 76, 72, 81, 1, 72, 102, 69, 33, 66, 30, 65, 46, 63, 87, 62, 84, 52, 78, 57, 94, 48, 71, 61, 80, 17, 117, 27, 23, 87, 38, 64, 26, 42, 81, 55, 74, 53, 79, 50, 75, 24, 13, 49, 73, 32, 98, 29, 31, 25, 17, 67, 34, 45, 91, 51, 76, 54, 83, 33, 2, 68, 40, 26, 18, 34, 57, 44, 88, 13, 106, 18, 53, 75, 37, 35, 100, 38, 90, 36, 104, 19, 55, 10, 113, 43, 86, 21, 62, 41, 114, 16, 116, 46, 95, 22, 65, 39, 93, 40, 111, 14, 108, 12, 105, 28, 27, 37, 103, 30, 36, 23, 9, 20, 61, 47, 97, 59, 118, 15, 112],
	#[],
	#[],
	#[],
	#[],
	#[147, 300, 123, 301, 122, 302, 83, 299, 81, 298, 76, 8, 75, 37, 53, 19, 28, 27, 33, 2, 22, 65, 34, 57, 18, 53, 29, 31, 20, 61, 23, 9, 21, 62],
	#[138, 59, 136, 45, 154, 11, 155, 52, 137, 15, 147, 4, 130, 10, 134, 35, 156, 22, 150, 347, 90, 16, 83, 12, 81, 1, 135, 3, 76, 8, 89, 47, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 151, 32, 63, 29, 75, 37, 61, 20, 53, 19, 42, 21, 41, 64, 62, 24, 28, 27, 55, 7, 24, 13, 22, 65, 34, 57, 33, 2, 25, 17, 13, 54, 18, 53, 35, 42, 36, 51, 19, 55, 43, 25, 21, 62, 29, 31, 20, 61, 12, 50, 23, 9],
	#[],
	#[],
	#[57, 337, 55, 336, 46, 335],
	#[],
	#[173, 244, 138, 109, 125, 14, 124, 256, 156, 246, 121, 247, 120, 258, 118, 255, 182, 242, 180, 263, 134, 252, 167, 259, 181, 254, 166, 243, 92, 250, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 240, 75, 37, 72, 257, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 262, 57, 253, 55, 241, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 251, 44, 249, 43, 248, 42, 245, 41, 114, 40, 260, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 261],
	#[],
	#[],
	#[],
	#[156, 22, 136, 45, 135, 3, 138, 59, 147, 4, 130, 349, 137, 15, 134, 35, 89, 47, 83, 12, 81, 1, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 63, 29, 62, 24, 61, 20, 65, 46, 55, 7, 53, 19, 43, 25, 42, 21, 34, 57, 28, 27, 33, 2, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 13, 54, 12, 50, 29, 31, 76, 8],
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
	#[137, 130, 63, 29],
	#[142, 125, 141, 127, 140, 131, 137, 15, 139, 132, 136, 122, 138, 59, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 62, 24, 61, 20, 41, 126, 12, 50, 13, 54],
	#[],
	#[91, 333, 39, 201, 38, 196, 37, 213, 36, 212],
	#[191, 191, 189, 177, 188, 210, 185, 224, 183, 216, 186, 199, 184, 186, 190, 219, 91, 221, 77, 217, 74, 229, 73, 185, 72, 211, 69, 197, 68, 205, 67, 203, 66, 194, 65, 214, 64, 180, 63, 192, 62, 187, 59, 228, 55, 172, 53, 182, 52, 179, 51, 176, 57, 202, 48, 170, 47, 207, 46, 204, 45, 198, 44, 193, 61, 183, 41, 226, 40, 222, 50, 174, 38, 196, 37, 213, 36, 212, 35, 209, 33, 171, 32, 208, 42, 184, 43, 188, 29, 195, 28, 190, 27, 189, 26, 181, 25, 178, 24, 175, 23, 173, 22, 227, 21, 223, 20, 220, 19, 218, 18, 215, 39, 201, 30, 200, 16, 116, 17, 117, 12, 105, 13, 106, 14, 108, 10, 225, 15, 112, 31, 206],
	#[],
	#[],
	#[128, 155, 127, 157, 126, 154, 33, 156],
	#[],
	#[156, 148, 138, 109, 134, 92, 120, 107, 119, 77, 118, 99, 114, 147, 106, 149, 105, 145, 117, 146, 92, 89, 89, 47, 83, 12, 76, 72, 81, 1, 72, 102, 69, 33, 66, 30, 65, 46, 63, 87, 75, 37, 52, 78, 57, 94, 48, 71, 61, 80, 17, 117, 34, 57, 68, 40, 55, 74, 53, 79, 50, 75, 28, 27, 62, 84, 41, 114, 43, 86, 40, 150, 67, 34, 45, 91, 42, 81, 51, 76, 54, 83, 33, 2, 25, 17, 32, 98, 59, 118, 37, 103, 49, 73, 12, 105, 46, 95, 44, 88, 13, 106, 24, 13, 29, 31, 16, 116, 35, 100, 20, 61, 36, 104, 19, 55, 23, 9, 21, 62, 18, 53, 47, 97, 15, 112, 22, 65, 39, 93, 38, 90, 14, 108],
	#[1, 455],
	#[],
	#[147, 453, 76, 8, 75, 37, 53, 19, 34, 57, 33, 2, 21, 62, 20, 61],
	#[],
	#[156, 22, 154, 11, 151, 32, 150, 451, 147, 4, 155, 52, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 130, 10, 89, 47, 83, 12, 90, 16, 76, 8, 75, 37, 81, 1, 69, 33, 68, 40, 66, 30, 65, 46, 63, 29, 62, 24, 61, 20, 67, 34, 55, 7, 22, 65, 41, 64, 24, 13, 42, 21, 25, 17, 34, 57, 33, 2, 53, 19, 18, 53, 20, 61, 19, 55, 12, 50, 28, 27, 21, 62, 35, 42, 13, 54, 29, 31, 36, 51, 43, 25, 23, 9],
	#[],
	#[],
	#[142, 125, 141, 127, 140, 124, 137, 15, 139, 123, 136, 122, 138, 59, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 62, 24, 61, 20, 41, 126, 13, 54, 12, 50],
	#[55, 138],
	#[147, 445, 146, 446, 76, 8, 75, 37, 33, 2, 34, 57, 53, 19, 20, 61, 21, 62],
	#[154, 11, 155, 52, 156, 22, 138, 59, 137, 15, 136, 45, 135, 3, 147, 4, 130, 10, 150, 60, 125, 14, 124, 44, 151, 32, 121, 28, 134, 35, 98, 418, 97, 441, 95, 440, 94, 442, 90, 16, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 8, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 29, 62, 24, 61, 20, 55, 7, 53, 19, 43, 25, 42, 21, 41, 64, 36, 51, 35, 42, 34, 57, 33, 2, 30, 36, 29, 31, 28, 27, 27, 23, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 26, 18, 13, 54, 12, 50],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[155, 52, 160, 408, 156, 22, 136, 45, 147, 4, 154, 11, 164, 402, 162, 410, 138, 59, 137, 15, 151, 32, 130, 10, 158, 406, 149, 405, 135, 3, 150, 409, 159, 403, 90, 16, 89, 47, 134, 35, 83, 12, 81, 1, 76, 8, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 62, 24, 55, 404, 61, 20, 33, 2, 25, 17, 12, 50, 19, 55, 32, 407, 24, 13, 42, 21, 20, 61, 36, 51, 75, 37, 53, 19, 22, 65, 18, 53, 29, 31, 41, 64, 34, 57, 21, 62, 28, 27, 35, 42, 13, 54, 43, 25, 23, 9],
	#[],
	#[101, 389, 100, 386, 99, 387, 41, 388],
	#[],
	#[],
	#[],
	#[],
	#[156, 82, 138, 109, 134, 92, 125, 14, 124, 101, 121, 85, 120, 107, 119, 77, 118, 99, 117, 70, 110, 115, 104, 96, 103, 110, 92, 89, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 42, 81, 41, 114, 40, 111, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 113],
	#[],
	#[],
	#[10, 68, 11, 69],
	#[],
	#[],
	#[],
	#[156, 82, 138, 109, 134, 92, 125, 14, 124, 101, 121, 85, 120, 107, 119, 77, 118, 99, 117, 70, 110, 115, 104, 96, 103, 321, 92, 89, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 42, 81, 41, 114, 40, 111, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 113],
	#[],
	#[],
	#[],
	#[156, 82, 138, 109, 134, 92, 125, 14, 124, 101, 121, 85, 120, 107, 119, 77, 118, 99, 117, 70, 110, 115, 104, 96, 103, 319, 92, 89, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 42, 81, 41, 114, 40, 111, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 113],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[138, 109, 125, 14, 124, 101, 121, 85, 120, 107, 119, 77, 118, 99, 117, 70, 134, 92, 110, 318, 109, 317, 92, 89, 89, 47, 88, 6, 87, 38, 76, 72, 72, 102, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 59, 118, 55, 74, 54, 83, 44, 88, 48, 71, 49, 73, 40, 111, 50, 75, 51, 76, 33, 2, 25, 17, 26, 18, 24, 13, 12, 105, 19, 55, 41, 114, 34, 57, 47, 97, 27, 23, 36, 104, 30, 36, 32, 98, 57, 94, 43, 86, 53, 79, 42, 81, 13, 106, 75, 37, 35, 100, 45, 91, 17, 117, 52, 78, 61, 80, 21, 62, 16, 116, 10, 113, 46, 95, 20, 61, 38, 90, 37, 103, 15, 112, 39, 93, 14, 108],
	#[],
	#[],
	#[112, 141, 111, 316, 10, 142],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[156, 82, 138, 109, 134, 92, 125, 14, 124, 101, 121, 85, 120, 107, 119, 77, 118, 99, 117, 70, 110, 115, 104, 96, 103, 152, 92, 89, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 42, 81, 41, 114, 40, 111, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 113],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[111, 143, 112, 141, 10, 142],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[2, 385],
	#[156, 82, 138, 109, 134, 92, 125, 14, 124, 101, 121, 85, 120, 107, 119, 77, 118, 99, 117, 70, 110, 115, 104, 96, 103, 121, 92, 89, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 42, 81, 41, 114, 40, 111, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 113],
	#[],
	#[156, 82, 138, 109, 134, 92, 125, 14, 124, 101, 121, 85, 120, 107, 119, 77, 118, 99, 117, 70, 110, 115, 104, 96, 103, 120, 92, 89, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 44, 88, 39, 93, 48, 71, 37, 103, 36, 104, 40, 111, 41, 114, 33, 2, 32, 98, 42, 81, 43, 86, 29, 31, 28, 27, 27, 23, 45, 91, 25, 17, 24, 13, 46, 95, 35, 100, 21, 62, 20, 61, 19, 55, 47, 97, 17, 117, 16, 116, 38, 90, 14, 108, 13, 106, 34, 57, 10, 113, 30, 36, 26, 18, 22, 65, 18, 53, 12, 105, 23, 9, 15, 112],
	#[],
	#[],
	#[],
	#[],
	#[156, 82, 138, 109, 134, 92, 125, 14, 124, 101, 121, 85, 120, 107, 119, 77, 118, 99, 117, 70, 110, 115, 104, 96, 103, 119, 92, 89, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 42, 81, 41, 114, 40, 111, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 113],
	#[60, 384],
	#[],
	#[],
	#[],
	#[56, 137],
	#[46, 134],
	#[],
	#[],
	#[40, 128],
	#[136, 122, 138, 59, 142, 129, 68, 40, 137, 15, 66, 30, 65, 46, 63, 29, 67, 34, 41, 126, 62, 24, 61, 20, 13, 54, 12, 50],
	#[],
	#[],
	#[],
	#[58, 133],
	#[],
	#[142, 135, 138, 59, 137, 15, 136, 122, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 62, 24, 61, 20, 41, 126, 13, 54, 12, 50],
	#[56, 136],
	#[],
	#[],
	#[138, 109, 124, 101, 156, 82, 121, 85, 119, 77, 118, 99, 117, 70, 134, 92, 120, 107, 125, 14, 104, 96, 103, 139, 92, 89, 89, 47, 88, 6, 110, 115, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 118, 87, 38, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 42, 81, 41, 114, 40, 111, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 113],
	#[56, 140],
	#[],
	#[],
	#[156, 82, 138, 109, 134, 92, 125, 14, 124, 101, 121, 85, 120, 107, 119, 77, 117, 70, 110, 115, 103, 144, 104, 96, 118, 99, 92, 89, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 75, 37, 69, 33, 68, 40, 72, 102, 66, 30, 64, 26, 63, 87, 62, 84, 61, 80, 65, 46, 57, 94, 55, 74, 67, 34, 53, 79, 52, 78, 51, 76, 59, 118, 45, 91, 44, 88, 48, 71, 76, 72, 40, 111, 50, 75, 38, 90, 49, 73, 54, 83, 43, 86, 29, 31, 28, 27, 41, 114, 25, 17, 24, 13, 46, 95, 35, 100, 21, 62, 42, 81, 36, 104, 47, 97, 16, 116, 26, 18, 27, 23, 33, 2, 17, 117, 32, 98, 19, 55, 20, 61, 34, 57, 37, 103, 18, 53, 12, 105, 13, 106, 14, 108, 30, 36, 39, 93, 22, 65, 10, 113, 23, 9, 15, 112],
	#[],
	#[],
	#[],
	#[15, 112, 14, 108, 13, 106, 12, 105, 42, 81, 24, 13, 20, 61, 21, 62, 22, 65, 35, 100, 23, 9, 19, 55, 156, 148, 138, 109, 120, 107, 119, 77, 118, 99, 117, 146, 114, 147, 134, 92, 106, 149, 105, 371, 92, 89, 89, 47, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 41, 114, 40, 150, 39, 93, 38, 90, 37, 103, 36, 104, 34, 57, 33, 2, 32, 98, 29, 31, 28, 27, 25, 17, 18, 53, 17, 117, 16, 116],
	#[],
	#[138, 109, 134, 92, 120, 107, 119, 77, 118, 99, 117, 146, 114, 369, 113, 370, 92, 89, 89, 47, 76, 72, 75, 37, 72, 102, 68, 40, 67, 34, 66, 30, 65, 46, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 42, 81, 41, 114, 40, 150, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 25, 17, 24, 13, 21, 62, 20, 61, 19, 55, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105],
	#[],
	#[156, 148, 138, 109, 134, 92, 120, 107, 119, 77, 118, 99, 117, 146, 114, 147, 106, 149, 105, 151, 92, 89, 89, 47, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 42, 81, 41, 114, 39, 93, 38, 90, 37, 103, 36, 104, 40, 150, 34, 57, 33, 2, 32, 98, 28, 27, 25, 17, 24, 13, 23, 9, 35, 100, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 29, 31, 22, 65],
	#[],
	#[58, 153],
	#[],
	#[128, 383, 126, 154, 33, 156],
	#[],
	#[],
	#[86, 168, 85, 162, 78, 161, 34, 159, 25, 158, 24, 163, 23, 166, 22, 160, 21, 164, 20, 167, 29, 165],
	#[],
	#[],
	#[],
	#[77, 374, 74, 229, 73, 185, 33, 171, 32, 208, 31, 206, 30, 200, 28, 190, 27, 189, 26, 181, 25, 178, 24, 175, 23, 173, 22, 227, 21, 223, 20, 220, 19, 218, 29, 195, 18, 215],
	#[156, 82, 138, 109, 134, 92, 125, 14, 124, 101, 121, 85, 120, 107, 119, 77, 118, 99, 117, 70, 110, 115, 104, 96, 103, 372, 92, 89, 89, 47, 88, 6, 87, 38, 81, 1, 76, 72, 75, 37, 72, 102, 83, 12, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 43, 86, 33, 2, 32, 98, 42, 81, 30, 36, 27, 23, 25, 17, 23, 9, 35, 100, 21, 62, 40, 111, 24, 13, 37, 103, 50, 75, 16, 116, 26, 18, 14, 108, 41, 114, 34, 57, 28, 27, 39, 93, 18, 53, 19, 55, 36, 104, 17, 117, 12, 105, 59, 118, 69, 33, 38, 90, 29, 31, 10, 113, 20, 61, 44, 88, 13, 106, 22, 65, 15, 112],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[156, 148, 138, 109, 134, 92, 120, 107, 119, 77, 118, 99, 117, 146, 114, 147, 106, 149, 105, 169, 92, 89, 89, 47, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 52, 78, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 44, 88, 42, 81, 41, 114, 45, 91, 17, 117, 34, 57, 40, 150, 51, 76, 33, 2, 32, 98, 53, 79, 12, 105, 23, 9, 19, 55, 38, 90, 43, 86, 25, 17, 24, 13, 16, 116, 36, 104, 35, 100, 39, 93, 37, 103, 28, 27, 22, 65, 13, 106, 18, 53, 29, 31, 20, 61, 14, 108, 21, 62, 15, 112],
	#[],
	#[],
	#[],
	#[191, 191, 190, 219, 189, 177, 188, 210, 186, 199, 185, 224, 184, 186, 183, 283, 91, 221, 77, 217, 74, 229, 73, 185, 72, 211, 69, 197, 68, 205, 67, 203, 66, 194, 65, 214, 64, 180, 63, 192, 62, 187, 61, 183, 59, 228, 57, 202, 55, 172, 53, 182, 52, 179, 51, 176, 50, 174, 48, 170, 47, 207, 46, 204, 45, 198, 44, 193, 43, 188, 42, 184, 41, 226, 40, 222, 39, 201, 38, 196, 37, 213, 36, 212, 35, 209, 33, 171, 32, 208, 31, 206, 30, 200, 29, 195, 28, 190, 27, 189, 26, 181, 25, 178, 24, 175, 23, 173, 22, 227, 21, 223, 20, 220, 19, 218, 18, 215, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 225],
	#[],
	#[77, 281, 74, 229, 73, 185, 45, 282, 33, 171, 32, 208, 31, 206, 30, 200, 29, 195, 28, 190, 27, 189, 26, 181, 25, 178, 24, 175, 23, 173, 22, 227, 21, 223, 20, 220, 19, 218, 18, 215],
	#[],
	#[77, 280, 74, 229, 73, 185, 33, 171, 32, 208, 31, 206, 30, 200, 29, 195, 28, 190, 27, 189, 26, 181, 25, 178, 24, 175, 23, 173, 22, 227, 21, 223, 20, 220, 19, 218, 18, 215],
	#[],
	#[],
	#[182, 277, 77, 278, 74, 229, 73, 185, 59, 262, 33, 171, 32, 208, 31, 206, 30, 200, 29, 195, 28, 190, 27, 189, 25, 178, 24, 175, 21, 223, 20, 220, 19, 218, 18, 215, 26, 181, 22, 227, 23, 173],
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
	#[54, 276],
	#[],
	#[191, 191, 190, 219, 189, 177, 188, 210, 186, 199, 185, 224, 184, 186, 183, 274, 91, 221, 77, 217, 74, 229, 73, 185, 72, 211, 69, 197, 68, 205, 67, 203, 66, 194, 65, 214, 64, 180, 63, 192, 62, 187, 61, 183, 59, 228, 57, 202, 55, 172, 53, 182, 52, 179, 51, 176, 50, 174, 44, 193, 39, 201, 48, 170, 37, 213, 36, 212, 40, 222, 41, 226, 33, 171, 32, 208, 42, 184, 43, 188, 29, 195, 28, 190, 27, 189, 45, 198, 25, 178, 24, 175, 46, 204, 35, 209, 21, 223, 20, 220, 47, 207, 17, 117, 16, 116, 38, 196, 14, 108, 13, 106, 12, 105, 10, 225, 30, 200, 19, 218, 26, 181, 31, 206, 22, 227, 18, 215, 23, 173, 15, 112],
	#[],
	#[],
	#[184, 186, 188, 210, 185, 224, 190, 219, 186, 199, 191, 191, 189, 177, 183, 272, 91, 221, 77, 217, 74, 229, 73, 185, 72, 211, 69, 197, 68, 205, 67, 203, 66, 194, 65, 214, 64, 180, 63, 192, 62, 187, 61, 183, 59, 228, 57, 202, 55, 172, 53, 182, 52, 179, 51, 176, 50, 174, 48, 170, 47, 207, 46, 204, 45, 198, 44, 193, 43, 188, 42, 184, 41, 226, 40, 222, 39, 201, 38, 196, 37, 213, 36, 212, 35, 209, 33, 171, 30, 200, 29, 195, 28, 190, 32, 208, 26, 181, 25, 178, 24, 175, 23, 173, 22, 227, 21, 223, 20, 220, 19, 218, 18, 215, 17, 117, 16, 116, 15, 112, 27, 189, 13, 106, 12, 105, 10, 225, 31, 206, 14, 108],
	#[],
	#[191, 191, 190, 219, 189, 177, 188, 210, 186, 199, 185, 224, 184, 271, 91, 221, 77, 217, 72, 211, 69, 197, 68, 205, 73, 185, 64, 180, 74, 229, 62, 187, 61, 183, 65, 214, 57, 202, 66, 194, 67, 203, 48, 170, 47, 207, 44, 193, 43, 188, 42, 184, 41, 226, 40, 222, 50, 174, 51, 176, 36, 212, 52, 179, 63, 192, 59, 228, 32, 208, 31, 206, 30, 200, 29, 195, 28, 190, 24, 175, 46, 204, 35, 209, 21, 223, 25, 178, 16, 116, 53, 182, 27, 189, 39, 201, 45, 198, 19, 218, 33, 171, 20, 220, 10, 225, 55, 172, 18, 215, 12, 105, 23, 173, 26, 181, 38, 196, 14, 108, 17, 117, 15, 112, 37, 213, 22, 227, 13, 106],
	#[],
	#[],
	#[191, 191, 184, 186, 185, 224, 183, 269, 186, 199, 189, 177, 188, 210, 190, 219, 91, 221, 77, 217, 74, 229, 73, 185, 72, 211, 69, 197, 68, 205, 67, 203, 66, 194, 65, 214, 64, 180, 63, 192, 62, 187, 61, 183, 59, 228, 57, 202, 55, 172, 53, 182, 52, 179, 51, 176, 50, 174, 48, 170, 47, 207, 46, 204, 45, 198, 44, 193, 43, 188, 42, 184, 41, 226, 40, 222, 36, 212, 35, 209, 33, 171, 32, 208, 31, 206, 29, 195, 28, 190, 27, 189, 26, 181, 25, 178, 22, 227, 21, 223, 20, 220, 24, 175, 37, 213, 17, 117, 16, 116, 38, 196, 14, 108, 12, 105, 39, 201, 30, 200, 19, 218, 10, 225, 18, 215, 23, 173, 15, 112, 13, 106],
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
	#[91, 221, 183, 267, 72, 211, 53, 182, 68, 205, 48, 170, 184, 186, 186, 199, 74, 229, 40, 222, 77, 217, 185, 224, 57, 202, 188, 210, 64, 180, 66, 194, 32, 208, 73, 185, 65, 214, 190, 219, 67, 203, 55, 172, 51, 176, 24, 175, 61, 183, 35, 209, 59, 228, 36, 212, 191, 191, 62, 187, 38, 196, 27, 189, 41, 226, 69, 197, 28, 190, 44, 193, 42, 184, 45, 198, 50, 174, 63, 192, 43, 188, 20, 220, 31, 206, 52, 179, 46, 204, 189, 177, 33, 171, 25, 178, 18, 215, 12, 105, 23, 173, 26, 181, 29, 195, 47, 207, 10, 225, 16, 116, 30, 200, 21, 223, 17, 117, 39, 201, 37, 213, 22, 227, 13, 106, 14, 108, 19, 218, 15, 112],
	#[],
	#[60, 368],
	#[],
	#[],
	#[49, 232],
	#[],
	#[],
	#[],
	#[],
	#[191, 191, 190, 219, 189, 177, 188, 210, 186, 199, 185, 224, 184, 231, 91, 221, 77, 217, 74, 229, 73, 185, 72, 211, 69, 197, 68, 205, 67, 203, 66, 194, 65, 214, 64, 180, 63, 192, 62, 187, 61, 183, 59, 228, 57, 202, 55, 172, 53, 182, 52, 179, 51, 176, 50, 174, 48, 170, 47, 207, 46, 204, 45, 198, 44, 193, 43, 188, 42, 184, 41, 226, 40, 222, 39, 201, 38, 196, 37, 213, 36, 212, 35, 209, 33, 171, 32, 208, 31, 206, 30, 200, 29, 195, 28, 190, 27, 189, 26, 181, 25, 178, 24, 175, 23, 173, 22, 227, 21, 223, 20, 220, 19, 218, 18, 215, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 225],
	#[],
	#[],
	#[],
	#[191, 191, 190, 219, 189, 177, 188, 210, 186, 199, 184, 186, 183, 230, 185, 224, 91, 221, 77, 217, 74, 229, 73, 185, 72, 211, 69, 197, 68, 205, 67, 203, 66, 194, 65, 214, 64, 180, 63, 192, 62, 187, 61, 183, 57, 202, 53, 182, 52, 179, 51, 176, 50, 174, 48, 170, 47, 207, 46, 204, 45, 198, 44, 193, 43, 188, 42, 184, 41, 226, 40, 222, 39, 201, 38, 196, 37, 213, 36, 212, 35, 209, 33, 171, 29, 195, 28, 190, 27, 189, 26, 181, 25, 178, 24, 175, 23, 173, 32, 208, 21, 223, 59, 228, 19, 218, 18, 215, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 225, 30, 200, 20, 220, 31, 206, 22, 227, 55, 172],
	#[],
	#[60, 367],
	#[],
	#[194, 235, 77, 234, 74, 229, 73, 185, 63, 233, 55, 236, 45, 237, 41, 238, 33, 171, 32, 208, 31, 206, 30, 200, 29, 195, 28, 190, 27, 189, 26, 181, 25, 178, 24, 175, 22, 227, 21, 223, 20, 220, 19, 218, 18, 215, 23, 173],
	#[],
	#[],
	#[192, 363, 193, 365, 54, 364],
	#[156, 22, 155, 52, 154, 11, 151, 32, 150, 239, 147, 4, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 130, 10, 90, 16, 89, 47, 83, 12, 81, 1, 76, 8, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 62, 24, 61, 20, 55, 7, 53, 19, 43, 25, 42, 21, 41, 64, 36, 51, 35, 42, 34, 57, 33, 2, 29, 31, 28, 27, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 13, 54, 12, 50],
	#[],
	#[],
	#[56, 362],
	#[],
	#[191, 191, 190, 219, 189, 177, 188, 210, 186, 199, 185, 224, 184, 186, 183, 296, 91, 221, 72, 211, 68, 205, 66, 194, 64, 180, 61, 183, 59, 228, 53, 182, 51, 176, 48, 170, 47, 207, 46, 204, 45, 198, 44, 193, 43, 188, 42, 184, 41, 226, 40, 222, 39, 201, 38, 196, 37, 213, 36, 212, 35, 209, 33, 171, 32, 208, 31, 206, 30, 200, 29, 195, 28, 190, 27, 189, 26, 181, 25, 178, 24, 175, 23, 173, 22, 227, 21, 223, 20, 220, 19, 218, 18, 215, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 225, 57, 202, 50, 174, 62, 187, 74, 229, 69, 197, 73, 185, 67, 203, 65, 214, 77, 217, 52, 179, 63, 192, 55, 172],
	#[],
	#[157, 332, 31, 324],
	#[],
	#[],
	#[180, 263, 181, 254, 172, 295, 182, 242, 173, 294, 134, 252, 138, 109, 125, 14, 124, 256, 121, 247, 120, 258, 118, 255, 92, 250, 89, 47, 88, 6, 87, 38, 76, 240, 75, 37, 72, 257, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 262, 57, 253, 55, 241, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 47, 97, 46, 95, 45, 251, 44, 249, 48, 71, 42, 245, 41, 114, 40, 260, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 43, 248, 27, 23, 26, 18, 25, 17, 24, 13, 21, 62, 20, 61, 19, 55, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 261, 30, 36],
	#[175, 289, 174, 293, 10, 288],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[190, 219, 189, 177, 188, 210, 186, 199, 185, 224, 184, 186, 183, 291, 191, 191, 91, 221, 77, 217, 74, 229, 73, 185, 72, 211, 69, 197, 68, 205, 67, 203, 66, 194, 65, 214, 64, 180, 63, 192, 62, 187, 61, 183, 59, 228, 57, 202, 55, 172, 53, 182, 52, 179, 51, 176, 50, 174, 48, 170, 47, 207, 46, 204, 45, 198, 44, 193, 43, 188, 42, 184, 41, 226, 40, 222, 39, 201, 38, 196, 37, 213, 36, 212, 35, 209, 33, 171, 32, 208, 31, 206, 30, 200, 29, 195, 28, 190, 27, 189, 26, 181, 25, 178, 24, 175, 23, 173, 22, 227, 21, 223, 20, 220, 19, 218, 18, 215, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 225],
	#[],
	#[],
	#[175, 289, 174, 287, 10, 288],
	#[],
	#[],
	#[],
	#[182, 242, 181, 254, 180, 263, 173, 244, 167, 259, 166, 286, 156, 246, 138, 109, 134, 252, 125, 14, 124, 256, 121, 247, 120, 258, 118, 255, 92, 250, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 240, 75, 37, 72, 257, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 262, 57, 253, 55, 241, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 251, 44, 249, 43, 248, 42, 245, 41, 114, 40, 260, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 261],
	#[182, 242, 181, 254, 180, 263, 173, 244, 167, 259, 166, 285, 156, 246, 138, 109, 134, 252, 125, 14, 124, 256, 121, 247, 120, 258, 118, 255, 92, 250, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 240, 75, 37, 72, 257, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 262, 57, 253, 55, 241, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 251, 44, 249, 43, 248, 42, 245, 41, 114, 40, 260, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 261],
	#[191, 191, 190, 219, 189, 177, 188, 210, 186, 199, 185, 224, 183, 265, 184, 186, 91, 221, 77, 217, 74, 229, 73, 185, 72, 211, 69, 197, 66, 194, 65, 214, 64, 180, 63, 192, 62, 187, 61, 183, 59, 228, 57, 202, 55, 172, 67, 203, 53, 182, 52, 179, 68, 205, 50, 174, 48, 170, 47, 207, 46, 204, 44, 193, 43, 188, 42, 184, 41, 226, 40, 222, 39, 201, 51, 176, 37, 213, 36, 212, 35, 209, 33, 171, 32, 208, 31, 206, 30, 200, 29, 195, 28, 190, 27, 189, 24, 175, 23, 173, 22, 227, 21, 223, 20, 220, 25, 178, 17, 117, 16, 116, 38, 196, 14, 108, 10, 225, 45, 198, 19, 218, 26, 181, 18, 215, 12, 105, 13, 106, 15, 112],
	#[182, 242, 181, 254, 180, 263, 173, 244, 167, 259, 166, 264, 156, 246, 138, 109, 134, 252, 121, 247, 120, 258, 118, 255, 124, 256, 125, 14, 92, 250, 89, 47, 88, 6, 87, 38, 83, 12, 76, 240, 81, 1, 72, 257, 69, 33, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 52, 78, 51, 76, 57, 253, 49, 73, 48, 71, 46, 95, 44, 249, 43, 248, 61, 80, 41, 114, 40, 260, 50, 75, 38, 90, 37, 103, 75, 37, 32, 98, 24, 13, 23, 9, 35, 100, 47, 97, 42, 245, 54, 83, 45, 251, 19, 55, 20, 61, 27, 23, 10, 261, 33, 2, 25, 17, 53, 79, 26, 18, 29, 31, 34, 57, 17, 117, 16, 116, 18, 53, 30, 36, 68, 40, 59, 262, 12, 105, 28, 27, 22, 65, 13, 106, 14, 108, 21, 62, 67, 34, 55, 241, 36, 104, 15, 112, 39, 93],
	#[],
	#[60, 266],
	#[],
	#[56, 268],
	#[],
	#[58, 270],
	#[],
	#[],
	#[60, 273],
	#[],
	#[58, 275],
	#[],
	#[],
	#[],
	#[59, 262, 182, 279],
	#[],
	#[],
	#[],
	#[],
	#[56, 284],
	#[],
	#[],
	#[],
	#[],
	#[182, 242, 181, 254, 180, 263, 173, 244, 167, 259, 166, 290, 156, 246, 138, 109, 134, 252, 125, 14, 124, 256, 121, 247, 120, 258, 118, 255, 92, 250, 89, 47, 88, 6, 87, 38, 81, 1, 76, 240, 75, 37, 72, 257, 66, 30, 65, 46, 64, 26, 63, 87, 83, 12, 53, 79, 52, 78, 50, 75, 49, 73, 48, 71, 42, 245, 33, 2, 29, 31, 27, 23, 51, 76, 24, 13, 21, 62, 40, 260, 25, 17, 62, 84, 14, 108, 13, 106, 35, 100, 57, 253, 41, 114, 43, 248, 17, 117, 32, 98, 16, 116, 22, 65, 18, 53, 12, 105, 23, 9, 26, 18, 34, 57, 67, 34, 45, 251, 20, 61, 36, 104, 30, 36, 68, 40, 19, 55, 46, 95, 44, 249, 69, 33, 38, 90, 55, 241, 61, 80, 54, 83, 37, 103, 47, 97, 15, 112, 10, 261, 39, 93, 28, 27, 59, 262],
	#[],
	#[],
	#[58, 292],
	#[],
	#[],
	#[],
	#[],
	#[56, 297],
	#[],
	#[156, 82, 138, 109, 134, 92, 125, 14, 124, 101, 121, 85, 120, 107, 119, 77, 118, 99, 117, 70, 110, 115, 104, 96, 103, 330, 92, 89, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 42, 81, 41, 114, 40, 111, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 113],
	#[167, 259, 166, 328, 180, 263, 181, 254, 182, 242, 156, 246, 173, 244, 138, 109, 134, 252, 125, 14, 124, 256, 120, 258, 118, 255, 121, 247, 92, 250, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 240, 75, 37, 72, 257, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 262, 57, 253, 55, 241, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 251, 44, 249, 43, 248, 42, 245, 41, 114, 40, 260, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 12, 105, 10, 261, 13, 106, 23, 9],
	#[156, 82, 138, 109, 124, 101, 121, 85, 118, 99, 117, 70, 134, 92, 120, 107, 125, 14, 104, 96, 103, 305, 92, 89, 119, 77, 89, 47, 88, 6, 110, 115, 83, 12, 81, 1, 76, 72, 75, 37, 72, 102, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 87, 62, 84, 61, 80, 59, 118, 87, 38, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 42, 81, 41, 114, 40, 111, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105, 10, 113],
	#[40, 303],
	#[],
	#[147, 300, 123, 301, 122, 304, 83, 299, 81, 298, 76, 8, 75, 37, 53, 19, 34, 57, 33, 2, 29, 31, 28, 27, 22, 65, 21, 62, 20, 61, 18, 53, 23, 9],
	#[],
	#[129, 306, 31, 307],
	#[],
	#[85, 311, 83, 310, 81, 308, 79, 313, 76, 309, 75, 37, 34, 57, 33, 2, 29, 31, 28, 27, 25, 158, 22, 65, 21, 62, 20, 314, 18, 53, 23, 312],
	#[],
	#[],
	#[],
	#[83, 310, 81, 308, 79, 315, 76, 309, 75, 37, 34, 57, 33, 2, 29, 31, 28, 27, 23, 9, 22, 65, 21, 62, 20, 61, 18, 53],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[56, 320],
	#[],
	#[],
	#[157, 323, 31, 324],
	#[],
	#[83, 326, 81, 325, 80, 327, 29, 31, 28, 27, 23, 9, 22, 65, 18, 53],
	#[],
	#[],
	#[],
	#[129, 329, 31, 307],
	#[],
	#[129, 331, 31, 307],
	#[],
	#[],
	#[156, 22, 155, 52, 154, 334, 147, 4, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 130, 10, 90, 16, 89, 47, 81, 1, 68, 40, 66, 30, 62, 24, 43, 25, 67, 34, 24, 13, 41, 64, 69, 33, 75, 37, 19, 55, 83, 12, 61, 20, 20, 61, 65, 46, 34, 57, 22, 65, 53, 19, 12, 50, 25, 17, 42, 21, 36, 51, 33, 2, 35, 42, 28, 27, 63, 29, 13, 54, 76, 8, 29, 31, 55, 7, 21, 62, 18, 53, 23, 9],
	#[],
	#[147, 356, 76, 8, 75, 37, 53, 19, 34, 57, 33, 2, 21, 62, 20, 61],
	#[156, 22, 155, 340, 153, 344, 152, 342, 147, 4, 143, 350, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 133, 338, 132, 343, 131, 351, 130, 10, 90, 16, 89, 47, 83, 12, 81, 1, 76, 8, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 62, 24, 61, 20, 55, 7, 53, 19, 43, 25, 42, 21, 41, 339, 36, 51, 35, 42, 34, 57, 33, 2, 29, 31, 28, 27, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 13, 54, 12, 50],
	#[156, 22, 155, 340, 153, 344, 152, 342, 147, 4, 138, 59, 137, 15, 136, 45, 135, 3, 133, 338, 132, 343, 131, 341, 130, 10, 89, 47, 55, 7, 90, 16, 33, 2, 81, 1, 42, 21, 43, 25, 29, 31, 28, 27, 41, 339, 25, 17, 24, 13, 75, 37, 35, 42, 21, 62, 20, 61, 36, 51, 18, 53, 68, 40, 65, 46, 67, 34, 13, 54, 34, 57, 66, 30, 76, 8, 19, 55, 63, 29, 61, 20, 22, 65, 53, 19, 12, 50, 23, 9, 69, 33, 83, 12, 134, 35, 62, 24],
	#[],
	#[156, 22, 155, 52, 154, 11, 151, 32, 150, 354, 147, 4, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 130, 10, 90, 16, 89, 47, 83, 12, 81, 1, 76, 8, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 62, 24, 61, 20, 55, 7, 53, 19, 43, 25, 42, 21, 41, 64, 36, 51, 35, 42, 34, 57, 33, 2, 29, 31, 28, 27, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 13, 54, 12, 50],
	#[],
	#[58, 357],
	#[],
	#[40, 352],
	#[91, 345, 39, 201, 38, 196, 37, 213, 36, 212],
	#[156, 22, 155, 52, 154, 346, 147, 4, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 130, 10, 90, 16, 89, 47, 83, 12, 81, 1, 76, 8, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 62, 24, 61, 20, 53, 19, 43, 25, 42, 21, 41, 64, 36, 51, 35, 42, 33, 2, 29, 31, 28, 27, 25, 17, 24, 13, 20, 61, 18, 53, 19, 55, 34, 57, 22, 65, 55, 7, 12, 50, 13, 54, 21, 62, 63, 29, 23, 9],
	#[],
	#[56, 348],
	#[],
	#[57, 337, 55, 336, 46, 335],
	#[56, 355],
	#[],
	#[156, 22, 155, 340, 153, 344, 152, 342, 147, 4, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 133, 353, 130, 10, 90, 16, 89, 47, 83, 12, 81, 1, 76, 8, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 62, 24, 61, 20, 55, 7, 53, 19, 43, 25, 42, 21, 41, 339, 36, 51, 35, 42, 34, 57, 33, 2, 29, 31, 28, 27, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 13, 54, 12, 50],
	#[],
	#[],
	#[],
	#[],
	#[32, 358],
	#[156, 22, 147, 4, 144, 359, 138, 59, 137, 15, 136, 45, 135, 361, 134, 35, 89, 47, 83, 12, 81, 1, 76, 8, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 62, 24, 61, 20, 55, 7, 53, 19, 43, 25, 42, 21, 41, 360, 34, 57, 33, 2, 29, 31, 28, 27, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 13, 54, 12, 50],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[63, 366],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[129, 373, 31, 307],
	#[],
	#[182, 242, 181, 254, 180, 263, 173, 244, 167, 259, 166, 375, 156, 246, 138, 109, 134, 252, 125, 14, 118, 255, 120, 258, 121, 247, 92, 250, 28, 27, 50, 75, 33, 2, 81, 1, 25, 17, 65, 46, 24, 13, 83, 12, 76, 240, 34, 57, 67, 34, 48, 71, 61, 80, 88, 6, 26, 18, 59, 262, 37, 103, 64, 26, 89, 47, 44, 249, 27, 23, 51, 76, 49, 73, 75, 37, 62, 84, 45, 251, 14, 108, 66, 30, 57, 253, 23, 9, 41, 114, 18, 53, 54, 83, 30, 36, 53, 79, 32, 98, 16, 116, 19, 55, 29, 31, 43, 248, 36, 104, 52, 78, 68, 40, 72, 257, 38, 90, 21, 62, 35, 100, 42, 245, 87, 38, 40, 260, 55, 241, 124, 256, 69, 33, 20, 61, 10, 261, 22, 65, 15, 112, 12, 105, 17, 117, 39, 93, 63, 87, 46, 95, 47, 97, 13, 106],
	#[195, 376, 31, 377],
	#[],
	#[75, 37, 81, 308, 83, 310, 76, 309, 34, 378, 33, 2, 78, 379, 29, 380, 28, 27, 23, 9, 22, 65, 79, 381, 20, 61, 18, 53, 21, 62],
	#[],
	#[83, 310, 81, 308, 79, 382, 76, 309, 75, 37, 34, 57, 33, 2, 29, 31, 28, 27, 23, 9, 22, 65, 21, 62, 20, 61, 18, 53],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[2, 401],
	#[156, 394, 138, 109, 134, 92, 120, 107, 119, 77, 118, 99, 117, 392, 116, 395, 108, 396, 102, 393, 92, 89, 89, 47, 83, 12, 81, 1, 76, 72, 75, 37, 68, 40, 66, 30, 63, 87, 62, 84, 61, 80, 65, 46, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 44, 88, 43, 86, 42, 81, 67, 34, 39, 93, 72, 102, 36, 104, 34, 57, 33, 2, 29, 31, 28, 27, 32, 98, 45, 91, 25, 17, 24, 13, 23, 9, 20, 61, 19, 55, 13, 106, 69, 33, 16, 116, 17, 117, 35, 100, 21, 62, 46, 95, 18, 53, 12, 105, 59, 118, 38, 90, 14, 108, 41, 114, 22, 65, 15, 112, 37, 103, 47, 97],
	#[40, 390],
	#[101, 389, 100, 391, 41, 388],
	#[],
	#[156, 394, 116, 395, 120, 107, 117, 392, 118, 99, 107, 399, 119, 77, 89, 47, 76, 72, 81, 1, 72, 102, 47, 97, 45, 91, 83, 12, 50, 75, 48, 71, 53, 79, 44, 88, 51, 76, 24, 13, 66, 30, 33, 2, 25, 17, 43, 86, 62, 84, 67, 34, 49, 73, 13, 106, 69, 33, 28, 27, 29, 31, 42, 81, 54, 83, 92, 89, 19, 55, 63, 87, 61, 80, 20, 61, 52, 78, 46, 95, 37, 103, 55, 74, 134, 92, 65, 46, 23, 9, 38, 90, 14, 108, 34, 57, 108, 400, 57, 94, 15, 112, 36, 104, 75, 37, 68, 40, 32, 98, 39, 93, 12, 105, 22, 65, 17, 117, 18, 53, 138, 109, 41, 114, 35, 100, 59, 118, 16, 116, 21, 62],
	#[],
	#[138, 109, 134, 92, 120, 107, 119, 77, 118, 99, 117, 392, 116, 397, 115, 398, 92, 89, 89, 47, 76, 72, 75, 37, 72, 102, 68, 40, 67, 34, 66, 30, 65, 46, 63, 87, 62, 84, 61, 80, 59, 118, 57, 94, 55, 74, 54, 83, 53, 79, 52, 78, 51, 76, 50, 75, 49, 73, 48, 71, 47, 97, 46, 95, 45, 91, 44, 88, 43, 86, 42, 81, 41, 114, 39, 93, 38, 90, 37, 103, 36, 104, 35, 100, 34, 57, 33, 2, 32, 98, 25, 17, 24, 13, 21, 62, 20, 61, 19, 55, 17, 117, 16, 116, 15, 112, 14, 108, 13, 106, 12, 105],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[163, 429, 156, 22, 155, 52, 154, 11, 151, 32, 147, 4, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 130, 10, 150, 60, 125, 14, 124, 44, 121, 28, 98, 418, 97, 428, 95, 413, 90, 16, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 8, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 29, 62, 24, 61, 20, 55, 7, 53, 19, 43, 25, 42, 21, 41, 64, 36, 51, 35, 42, 33, 2, 30, 36, 29, 31, 28, 27, 27, 23, 25, 17, 24, 13, 23, 9, 22, 65, 20, 61, 19, 55, 18, 53, 26, 18, 13, 54, 12, 50, 10, 430, 21, 62, 34, 57],
	#[],
	#[147, 4, 154, 11, 136, 45, 137, 15, 155, 52, 138, 59, 156, 22, 134, 35, 135, 3, 150, 423, 130, 10, 151, 32, 90, 16, 89, 47, 83, 12, 81, 1, 76, 8, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 62, 24, 61, 20, 55, 7, 53, 19, 43, 25, 42, 21, 41, 64, 36, 51, 35, 42, 34, 57, 33, 2, 29, 31, 28, 27, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 13, 54, 12, 50],
	#[48, 422],
	#[2, 439],
	#[165, 420, 48, 421],
	#[163, 416, 161, 414, 156, 22, 154, 11, 151, 32, 150, 60, 147, 4, 155, 52, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 130, 10, 125, 14, 124, 44, 121, 28, 98, 418, 97, 415, 95, 413, 87, 38, 76, 8, 81, 1, 65, 46, 69, 33, 27, 23, 25, 17, 24, 13, 21, 62, 33, 2, 88, 6, 43, 25, 62, 24, 26, 18, 64, 26, 90, 16, 42, 21, 30, 36, 19, 55, 83, 12, 61, 20, 34, 57, 66, 30, 89, 47, 55, 7, 53, 19, 12, 50, 23, 9, 41, 64, 29, 31, 67, 34, 10, 417, 20, 61, 28, 27, 63, 29, 13, 54, 18, 53, 75, 37, 35, 42, 68, 40, 22, 65, 36, 51],
	#[40, 411],
	#[],
	#[154, 11, 150, 409, 149, 412, 147, 4, 155, 52, 156, 22, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 130, 10, 151, 32, 90, 16, 89, 47, 83, 12, 75, 37, 81, 1, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 62, 24, 61, 20, 76, 8, 55, 7, 43, 25, 42, 21, 41, 64, 36, 51, 35, 42, 53, 19, 33, 2, 29, 31, 28, 27, 25, 17, 24, 13, 34, 57, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 13, 54, 12, 50, 23, 9],
	#[],
	#[],
	#[],
	#[145, 432, 10, 437],
	#[],
	#[160, 408, 159, 419, 154, 11, 151, 32, 150, 409, 149, 405, 164, 402, 155, 52, 162, 410, 156, 22, 138, 59, 137, 15, 136, 45, 135, 3, 147, 4, 130, 10, 134, 35, 90, 16, 89, 47, 83, 12, 81, 1, 76, 8, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 62, 24, 55, 404, 61, 20, 41, 64, 36, 51, 53, 19, 33, 2, 32, 407, 43, 25, 29, 31, 28, 27, 42, 21, 13, 54, 12, 50, 24, 13, 19, 55, 25, 17, 22, 65, 35, 42, 34, 57, 21, 62, 23, 9, 20, 61, 18, 53],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[56, 348, 40, 424],
	#[156, 22, 155, 52, 154, 11, 151, 32, 150, 409, 149, 425, 138, 59, 137, 15, 134, 35, 136, 45, 130, 10, 147, 4, 135, 3, 89, 47, 83, 12, 81, 1, 90, 16, 76, 8, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 63, 29, 75, 37, 61, 20, 55, 7, 43, 25, 42, 21, 41, 64, 62, 24, 36, 51, 53, 19, 33, 2, 24, 13, 34, 57, 19, 55, 13, 54, 12, 50, 21, 62, 22, 65, 25, 17, 18, 53, 28, 27, 29, 31, 35, 42, 20, 61, 23, 9],
	#[56, 426],
	#[48, 427],
	#[],
	#[145, 432, 10, 433],
	#[],
	#[156, 22, 137, 15, 134, 35, 154, 11, 136, 45, 164, 431, 155, 52, 138, 59, 151, 32, 149, 405, 135, 3, 41, 64, 65, 46, 75, 37, 90, 16, 63, 29, 81, 1, 43, 25, 29, 31, 67, 34, 32, 407, 55, 404, 25, 17, 24, 13, 61, 20, 20, 61, 33, 2, 18, 53, 68, 40, 62, 24, 53, 19, 13, 54, 150, 409, 66, 30, 42, 21, 76, 8, 19, 55, 83, 12, 147, 4, 21, 62, 34, 57, 22, 65, 130, 10, 35, 42, 12, 50, 28, 27, 69, 33, 89, 47, 36, 51, 23, 9],
	#[],
	#[],
	#[149, 405, 24, 13, 130, 10, 88, 6, 90, 16, 27, 23, 33, 2, 81, 1, 53, 19, 121, 28, 164, 434, 26, 18, 135, 3, 154, 11, 65, 46, 42, 21, 156, 22, 61, 20, 64, 26, 137, 15, 32, 407, 19, 55, 83, 12, 147, 4, 25, 17, 13, 54, 76, 8, 125, 14, 29, 31, 75, 37, 62, 24, 20, 61, 55, 404, 66, 30, 68, 40, 23, 9, 21, 62, 67, 34, 138, 59, 30, 36, 28, 27, 35, 42, 136, 45, 69, 33, 124, 44, 22, 65, 43, 25, 36, 51, 134, 35, 89, 47, 151, 32, 63, 29, 34, 57, 12, 50, 150, 435, 87, 38, 18, 53, 41, 64, 155, 52, 98, 436],
	#[],
	#[40, 411],
	#[],
	#[164, 402, 162, 410, 160, 408, 159, 438, 156, 22, 155, 52, 154, 11, 151, 32, 150, 435, 149, 405, 147, 4, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 130, 10, 125, 14, 124, 44, 121, 28, 98, 436, 90, 16, 89, 47, 88, 6, 87, 38, 83, 12, 81, 1, 76, 8, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 64, 26, 63, 29, 62, 24, 61, 20, 55, 404, 53, 19, 43, 25, 42, 21, 41, 64, 36, 51, 35, 42, 34, 57, 33, 2, 32, 407, 30, 36, 29, 31, 28, 27, 27, 23, 26, 18, 25, 17, 24, 13, 23, 9, 22, 65, 21, 62, 20, 61, 19, 55, 18, 53, 13, 54, 12, 50],
	#[],
	#[],
	#[],
	#[145, 432, 10, 444],
	#[2, 443],
	#[],
	#[156, 22, 155, 52, 151, 32, 147, 4, 154, 11, 138, 59, 130, 10, 150, 60, 125, 14, 121, 28, 137, 15, 134, 35, 124, 44, 135, 3, 98, 436, 136, 45, 89, 47, 87, 38, 81, 1, 88, 6, 25, 17, 29, 31, 24, 13, 19, 55, 33, 2, 42, 21, 26, 18, 76, 8, 34, 57, 27, 23, 68, 40, 65, 46, 83, 12, 53, 19, 13, 54, 64, 26, 62, 24, 55, 7, 66, 30, 35, 42, 22, 65, 90, 16, 28, 27, 67, 34, 75, 37, 18, 53, 61, 20, 23, 9, 43, 25, 30, 36, 12, 50, 69, 33, 41, 64, 20, 61, 36, 51, 21, 62, 63, 29],
	#[47, 448],
	#[2, 447],
	#[],
	#[156, 22, 148, 449, 147, 4, 138, 59, 137, 15, 136, 45, 135, 3, 134, 35, 130, 450, 89, 47, 83, 12, 81, 1, 76, 8, 75, 37, 69, 33, 68, 40, 67, 34, 66, 30, 65, 46, 55, 7, 43, 25, 42, 21, 63, 29, 33, 2, 29, 31, 28, 27, 25, 17, 24, 13, 61, 20, 20, 61, 13, 54, 12, 50, 23, 9, 62, 24, 34, 57, 21, 62, 53, 19, 22, 65, 18, 53, 19, 55],
	#[],
	#[55, 336, 46, 335, 57, 337],
	#[2, 452],
	#[],
	#[2, 454],
	#[],
	#[2, 456],
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
		dylan-parser-action177,
		dylan-parser-action0,
		dylan-parser-action179,
		dylan-parser-action85,
		dylan-parser-action86,
		dylan-parser-action182,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action185,
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
		dylan-parser-action200,
		dylan-parser-action201,
		dylan-parser-action202,
		dylan-parser-action203,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action179,
		dylan-parser-action85,
		dylan-parser-action86,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action157,
		dylan-parser-action220,
		dylan-parser-action0,
		dylan-parser-action222,
		dylan-parser-action0,
		dylan-parser-action157,
		dylan-parser-action158,
		dylan-parser-action226,
		dylan-parser-action227,
		dylan-parser-action228,
		dylan-parser-action226,
		dylan-parser-action0,
		dylan-parser-action228,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action235,
		dylan-parser-action236,
		dylan-parser-action236,
		dylan-parser-action238,
		dylan-parser-action239,
		dylan-parser-action9,
		dylan-parser-action83,
		dylan-parser-action0,
		dylan-parser-action243,
		dylan-parser-action0,
		dylan-parser-action245,
		dylan-parser-action246,
		dylan-parser-action94,
		dylan-parser-action248,
		dylan-parser-action0,
		dylan-parser-action243,
		dylan-parser-action157,
		dylan-parser-action157,
		dylan-parser-action83,
		dylan-parser-action254,
		dylan-parser-action255,
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
		dylan-parser-action243,
		dylan-parser-action309,
		dylan-parser-action309,
		dylan-parser-action309,
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
		dylan-parser-action339,
		dylan-parser-action340,
		dylan-parser-action340,
		dylan-parser-action0,
		dylan-parser-action343,
		dylan-parser-action344,
		dylan-parser-action345,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action9,
		dylan-parser-action46,
		dylan-parser-action0,
		dylan-parser-action351,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action0,
		dylan-parser-action356,
		dylan-parser-action171,
		dylan-parser-action172,
		dylan-parser-action173),
  action-nargs-table: #[1, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 2, 0, 1, 1, 3, 1, 1, 1, 0, 1, 1, 3, 2, 1, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 2, 2, 2, 2, 2, 0, 1, 2, 0, 1, 2, 2, 0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1, 1, 1, 1, 1, 2, 2, 1, 1, 3, 3, 3, 3, 1, 5, 4, 6, 1, 0, 1, 1, 2, 1, 2, 3, 4, 4, 6, 3, 1, 1, 1, 3, 2, 1, 1, 4, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 5, 3, 3, 1, 1, 0, 1, 1, 1, 3, 1, 1, 0, 1, 1, 1, 0, 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 1, 2, 3, 3, 3, 2, 2, 0, 1, 2, 1, 3, 4, 2, 3, 1, 2, 1, 1, 0, 2, 6, 2, 0, 1, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 2, 2, 2, 2, 2, 0, 1, 2, 0, 1, 2, 2, 0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 3, 0, 1, 1, 2, 1, 2, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 4, 2, 2, 1, 2, 3, 2, 0, 1, 2, 0, 1, 2, 1, 1, 1, 1, 3, 1, 2, 3],
  action-nt-table: #[70, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 72, 72, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 74, 74, 74, 75, 75, 76, 76, 76, 77, 77, 78, 78, 79, 79, 79, 80, 80, 81, 81, 81, 82, 82, 83, 83, 84, 84, 85, 85, 85, 86, 86, 86, 87, 88, 89, 89, 89, 90, 90, 91, 91, 91, 91, 92, 92, 92, 92, 92, 93, 94, 94, 95, 96, 96, 97, 97, 98, 98, 98, 99, 99, 100, 100, 101, 102, 103, 103, 104, 104, 105, 105, 106, 106, 107, 107, 108, 108, 109, 109, 110, 110, 110, 110, 110, 111, 111, 112, 113, 113, 114, 114, 115, 115, 116, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 119, 119, 119, 120, 120, 120, 120, 120, 121, 121, 121, 122, 122, 123, 123, 123, 124, 125, 125, 125, 126, 127, 127, 128, 128, 129, 129, 129, 130, 130, 130, 130, 130, 131, 132, 132, 133, 133, 133, 134, 135, 135, 135, 135, 135, 135, 135, 136, 136, 136, 136, 136, 136, 137, 137, 138, 138, 138, 138, 138, 139, 139, 140, 141, 141, 142, 142, 143, 143, 144, 144, 145, 145, 146, 146, 147, 147, 148, 149, 149, 150, 151, 151, 152, 153, 153, 154, 154, 155, 155, 156, 156, 156, 157, 157, 158, 158, 159, 159, 160, 160, 161, 161, 161, 162, 163, 163, 163, 164, 164, 164, 165, 165, 166, 166, 167, 167, 168, 168, 169, 169, 170, 170, 171, 171, 172, 172, 173, 173, 173, 173, 173, 174, 174, 175, 176, 176, 177, 177, 178, 178, 179, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 182, 183, 183, 184, 184, 184, 184, 185, 185, 185, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 187, 187, 188, 188, 188, 189, 189, 189, 189, 189, 189, 189, 190, 190, 191, 192, 192, 193, 194, 194, 194, 194, 194, 195, 195, 195]);

// eof
