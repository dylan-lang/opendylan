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

define function infix-dylan-parser-action0 (arg$1) => (value)
  arg$1
end infix-dylan-parser-action0;

define function infix-dylan-parser-action1 (arg$1, arg$2, arg$3) => (value)
  arg$2
end infix-dylan-parser-action1;

define function infix-dylan-parser-action9 (arg$1, arg$2) => (value)
  arg$1
end infix-dylan-parser-action9;

define function infix-dylan-parser-action11 (arg$1) => (value)
  #f
end infix-dylan-parser-action11;

define function infix-dylan-parser-action46 () => (value)
  #f
end infix-dylan-parser-action46;

define function infix-dylan-parser-action80 () => (value)
  empty-body-fragment()
end infix-dylan-parser-action80;

define function infix-dylan-parser-action82 (arg$1, arg$2) => (value)
  body-fragment(elements(arg$1))
end infix-dylan-parser-action82;

define function infix-dylan-parser-action83 () => (value)
  #()
end infix-dylan-parser-action83;

define function infix-dylan-parser-action85 (arg$1) => (value)
  append-sequence(arg$1)
end infix-dylan-parser-action85;

define function infix-dylan-parser-action86 (arg$1, arg$2, arg$3) => (value)
  append-element!(arg$1, arg$3)
end infix-dylan-parser-action86;

define function infix-dylan-parser-action93 (arg$1, arg$2, arg$3) => (value)
  concatenate(arg$1, pair(arg$2, arg$3))
end infix-dylan-parser-action93;

define function infix-dylan-parser-action94 (arg$1, arg$2) => (value)
  pair(arg$1, arg$2)
end infix-dylan-parser-action94;

define function infix-dylan-parser-action146 (arg$1, arg$2, arg$3) => (value)
  make(<parens-fragment>,
              record:           fragment-record(arg$1),
              source-position:  position-between(arg$1, arg$3),
              left-delimiter:   arg$1,
              nested-fragments: arg$2,
              right-delimiter:  arg$3)
end infix-dylan-parser-action146;

define function infix-dylan-parser-action147 (arg$1, arg$2, arg$3) => (value)
  make(<brackets-fragment>,
              record:           fragment-record(arg$1),
              source-position:  position-between(arg$1, arg$3),
              left-delimiter:   arg$1,
              nested-fragments: arg$2,
              right-delimiter:  arg$3)
end infix-dylan-parser-action147;

define function infix-dylan-parser-action148 (arg$1, arg$2, arg$3) => (value)
  make(<braces-fragment>,
              record:           fragment-record(arg$1),
              source-position:  position-between(arg$1, arg$3),
              left-delimiter:   arg$1,
              nested-fragments: arg$2,
              right-delimiter:  arg$3)
end infix-dylan-parser-action148;

define function infix-dylan-parser-action154 (arg$1, arg$2) => (value)
  make(<local-declaration-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              macro: arg$1,
              list-fragment: arg$2)
end infix-dylan-parser-action154;

define function infix-dylan-parser-action157 (arg$1) => (value)
  list(arg$1)
end infix-dylan-parser-action157;

define function infix-dylan-parser-action158 (arg$1, arg$2, arg$3) => (value)
  pair(arg$1, pair(arg$2, arg$3))
end infix-dylan-parser-action158;

define function infix-dylan-parser-action159 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-definition-tail
             (arg$1, arg$1, maybe-defined-name(arg$2), arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
                  source-position: position-between(arg$1, arg$3),
                macro: arg$1,
                body-fragment: arg$2);
         end
end infix-dylan-parser-action159;

define function infix-dylan-parser-action160 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-definition-tail
             (arg$1, arg$1, maybe-defined-name(arg$2), arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
                source-position: position-between(arg$1, arg$3),
                macro: arg$1,
                body-fragment: arg$2);
         end
end infix-dylan-parser-action160;

define function infix-dylan-parser-action161 (arg$1, arg$2, arg$3) => (value)
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
end infix-dylan-parser-action161;

define function infix-dylan-parser-action163 (arg$1, arg$2, arg$3, arg$4, arg$5) => (value)
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
end infix-dylan-parser-action163;

define function infix-dylan-parser-action164 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<list-definition-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$4),
              modifiers: arg$2,
              define-word: arg$3,
              list-fragment: arg$4)
end infix-dylan-parser-action164;

define function infix-dylan-parser-action165 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6) => (value)
  begin
           verify-definition-tail(arg$1, arg$3, arg$4, arg$6);
           make(<macro-body-definition-fragment>,
                record:          fragment-record(arg$1),
                source-position: position-between(arg$1, arg$6),
                modifiers: arg$2,
                define-word: arg$3,
                macro-body-fragment: pair(arg$4, arg$5));
         end
end infix-dylan-parser-action165;

define function infix-dylan-parser-action171 (arg$1) => (value)
  make(<definition-tail-fragment>,
              record:          fragment-record(arg$1),
              source-position: fragment-source-position(arg$1),
              end: arg$1)
end infix-dylan-parser-action171;

define function infix-dylan-parser-action172 (arg$1, arg$2) => (value)
  make(<definition-tail-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              end: arg$1, tail-name-1: arg$2)
end infix-dylan-parser-action172;

define function infix-dylan-parser-action173 (arg$1, arg$2, arg$3) => (value)
  make(<definition-tail-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              end: arg$1, tail-name-1: arg$2, tail-name-2: arg$3)
end infix-dylan-parser-action173;

define function infix-dylan-parser-action174 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<prefix-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$4),
              function: arg$1, arguments: arg$3)
end infix-dylan-parser-action174;

define function infix-dylan-parser-action175 (arg$1, arg$2, arg$3, arg$4) => (value)
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
end infix-dylan-parser-action175;

define function infix-dylan-parser-action176 (arg$1, arg$2, arg$3) => (value)
  make(<dot-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              function: arg$3, arguments: list(arg$1))
end infix-dylan-parser-action176;

define function infix-dylan-parser-action178 (arg$1) => (value)
  elements(arg$1)
end infix-dylan-parser-action178;

define function infix-dylan-parser-action181 (arg$1, arg$2) => (value)
  list(arg$1, arg$2)
end infix-dylan-parser-action181;

define function infix-dylan-parser-action184 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<function-macro-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$4),
              macro: arg$1,
              body-fragment: arg$3)
end infix-dylan-parser-action184;

define function infix-dylan-parser-action199 (arg$1, arg$2) => (value)
  make(<string-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              value: concatenate(fragment-value(arg$1), fragment-value(arg$2)))
end infix-dylan-parser-action199;

define function infix-dylan-parser-action200 (arg$1, arg$2, arg$3, arg$4, arg$5) => (value)
  make(<improper-list-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$5),
              elements:        arg$2,
              improper-tail:   arg$4)
end infix-dylan-parser-action200;

define function infix-dylan-parser-action201 (arg$1, arg$2, arg$3) => (value)
  make(<proper-list-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              elements:        arg$2)
end infix-dylan-parser-action201;

define function infix-dylan-parser-action202 (arg$1, arg$2, arg$3) => (value)
  make(<vector-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              elements:        arg$2)
end infix-dylan-parser-action202;

define function infix-dylan-parser-action217 (arg$1, arg$2, arg$3) => (value)
  list(arg$1, arg$2, arg$3)
end infix-dylan-parser-action217;

define function infix-dylan-parser-action219 (arg$1) => (value)
  make-variable-name-like
           (arg$1,
            record:          fragment-record(arg$1),
            source-position: fragment-source-position(arg$1),
            name: #"...")
end infix-dylan-parser-action219;

define function infix-dylan-parser-action223 (arg$1) => (value)
  binop-fragment(arg$1)
end infix-dylan-parser-action223;

define function infix-dylan-parser-action224 (arg$1) => (value)
  (arg$1)
end infix-dylan-parser-action224;

define function infix-dylan-parser-action225 (arg$1, arg$2, arg$3) => (value)
  append-binop!(arg$1, arg$2, arg$3)
end infix-dylan-parser-action225;

define function infix-dylan-parser-action232 (arg$1, arg$2) => (value)
  make(<unary-operator-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              function: arg$1,
              arguments: list(arg$2))
end infix-dylan-parser-action232;

define function infix-dylan-parser-action233 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-statement-tail(arg$1, arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
                  source-position: position-between(arg$1, arg$3),
                macro: arg$1,
                body-fragment: arg$2);
         end
end infix-dylan-parser-action233;

define function infix-dylan-parser-action234 (arg$1, arg$2, arg$3) => (value)
  begin
           verify-statement-tail(arg$1, arg$3);
           make(<statement-fragment>,
                record:          fragment-record(arg$1),
                source-position: position-between(arg$1, arg$3),
                macro: arg$1,
                body-fragment: arg$2);
         end
end infix-dylan-parser-action234;

define function infix-dylan-parser-action235 (arg$1, arg$2, arg$3) => (value)
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
end infix-dylan-parser-action235;

define function infix-dylan-parser-action236 (arg$1, arg$2) => (value)
  arg$2 | arg$1
end infix-dylan-parser-action236;

define function infix-dylan-parser-action240 (arg$1, arg$2) => (value)
  concatenate(arg$1, arg$2)
end infix-dylan-parser-action240;

define function infix-dylan-parser-action242 (arg$1, arg$2, arg$3) => (value)
  concatenate(arg$1, list(arg$2), arg$3)
end infix-dylan-parser-action242;

define function infix-dylan-parser-action243 (arg$1, arg$2, arg$3, arg$4) => (value)
  concatenate(arg$1, list(body-fragment(elements(arg$2))), list(arg$3), arg$4)
end infix-dylan-parser-action243;

define function infix-dylan-parser-action245 (arg$1, arg$2, arg$3) => (value)
  pair(body-fragment(elements(arg$1)), pair(arg$2, arg$3))
end infix-dylan-parser-action245;

define function infix-dylan-parser-action251 (arg$1, arg$2) => (value)
  concatenate(arg$1, list(arg$2))
end infix-dylan-parser-action251;

define function infix-dylan-parser-action252 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6) => (value)
  pair(make(<parens-fragment>,
                   record:           fragment-record(arg$1),
                   source-position:  position-between(arg$1, arg$5),
                   left-delimiter:   arg$1,
                   nested-fragments: pair(arg$2, pair(arg$3, arg$4)),
                   right-delimiter:  arg$5),
              list(arg$6))
end infix-dylan-parser-action252;

define function infix-dylan-parser-action306 (arg$1, arg$2, arg$3) => (value)
  pair(arg$1, concatenate(arg$2, list(arg$3)))
end infix-dylan-parser-action306;

define function infix-dylan-parser-action336 (arg$1, arg$2, arg$3, arg$4) => (value)
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
end infix-dylan-parser-action336;

define function infix-dylan-parser-action337 (arg$1, arg$2) => (value)
  make(<sequence-pattern-variable-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              name:            arg$2,
              separator:       #f)
end infix-dylan-parser-action337;

define function infix-dylan-parser-action340 (arg$1, arg$2) => (value)
  make(<unhygienic-name-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              name:            arg$2)
end infix-dylan-parser-action340;

define function infix-dylan-parser-action341 (arg$1, arg$2, arg$3) => (value)
  make(<template-aux-rule-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$3),
              rule-name:       arg$2,
              template:        arg$3)
end infix-dylan-parser-action341;

define function infix-dylan-parser-action342 (arg$1, arg$2) => (value)
  make(<template-macro-call-fragment>,
              record:          fragment-record(arg$1),
              source-position: position-between(arg$1, arg$2),
              template:        arg$2)
end infix-dylan-parser-action342;

define function infix-dylan-parser-action348 (arg$1, arg$2) => (value)
  arg$2
end infix-dylan-parser-action348;

define function infix-dylan-parser-action353 (arg$1, arg$2, arg$3) => (value)
  arg$1
end infix-dylan-parser-action353;

define constant infix-dylan-parser :: <parser>
  = make(<parser>,
  action-table:
      #[#[34, -51, 3, -6, 4, -35, 5, -10, 6, -5, 7, -11, 8, -45, 9, -15, 11, -24, 12, -9, 13, -42, 64, -47, 18, -40, 19, -12, 21, -27, 22, -62, 23, -21, 24, -54, 25, -26, 26, -57, 27, -4, 28, -34, 29, -7, 30, -37, 33, -59, 35, -31, 36, -66, 41, -43, 42, -30, 43, -63, 53, -14, 55, -36, 61, -49, 68, -56, 63, -50, 20, -55, 62, -16, 67, -23, 66, -52, 0, -3, 65, -61, 69, -58],
	#[10, -453, 11, -452],
	#[1, -450],
	#[65535, 64, 18, 64, 20, 64, 21, 64, 22, 64, 23, 64, 28, 64, 29, 64, 33, 64, 34, 64, 53, 64],
	#[2, 80, 12, -9, 13, -42, 18, -40, 19, -12, 20, -55, 21, -27, 22, -62, 23, -21, 24, -54, 25, -26, 26, -57, 27, -4, 28, -34, 29, -7, 30, -37, 33, -59, 34, -51, 35, -31, 36, -66, 41, -43, 42, -30, 43, -63, 53, -14, 55, -36, 61, -49, 62, -16, 63, -50, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[20, -55, 21, -27, 33, -59, 34, -51, 53, -14],
	#[65535, 54, 2, 54, 10, 54, 11, 54, 12, 54, 13, 54, 14, 54, 15, 54, 16, 54, 17, 54, 18, 54, 19, 54, 20, 54, 21, 54, 22, 54, 23, 54, 24, 54, 25, 54, 26, 54, 27, 54, 28, 54, 29, 54, 30, 54, 31, 54, 32, 54, 33, 54, 34, 54, 35, 54, 36, 54, 37, 54, 38, 54, 39, 54, 40, 54, 41, 54, 42, 54, 43, 54, 44, 54, 45, 54, 46, 54, 47, 54, 48, 54, 49, 54, 50, 54, 51, 54, 52, 54, 53, 54, 54, 54, 55, 54, 56, 54, 57, 54, 58, 54, 59, 54, 60, 54, 61, 54, 62, 54, 63, 54, 64, 54, 65, 54, 66, 54, 67, 54, 68, 54, 69, 54],
	#[65535, 188, 39, 188, 36, 188, 10, 188, 38, 188, 55, 188, 37, 188, 46, 188, 2, 188, 40, 188, 11, 188, 48, 188, 58, 188, 57, 188, 56, 188],
	#[65535, 195, 11, 195, 39, 195, 36, 195, 10, 195, 38, 195, 55, 195, 37, 195, 46, 195, 2, 195, 58, 195, 48, 195, 40, 195, 56, 195, 57, 195],
	#[20, -55, 21, -27, 33, -59, 34, -51, 53, -14],
	#[21, -27, 23, -21, 13, -42, 2, 238, 35, -31, 33, -59, 18, -40, 25, -26, 43, -63, 62, -16, 28, -34, 12, -9, 53, -14, 55, -404, 42, -30, 22, -62, 68, -56, 34, -51, 36, -66, 20, -55, 61, -49, 63, -50, 19, -12, 32, -405, 29, -7, 66, -52, 69, -58, 41, -43, 24, -54, 67, -23, 65, -61],
	#[65535, 65, 55, 65],
	#[65535, 218, 2, 218, 10, 218, 11, 218, 12, 218, 13, 218, 14, 218, 15, 218, 16, 218, 17, 218, 18, 218, 19, 218, 20, 218, 21, 218, 22, 218, 23, 218, 24, 218, 25, 218, 26, 218, 27, 218, 28, 218, 29, 218, 30, 218, 31, 218, 32, 218, 33, 218, 34, 218, 35, 218, 36, 218, 37, 218, 38, 218, 39, 218, 40, 218, 41, 218, 42, 218, 43, 218, 44, 218, 45, 218, 46, 218, 47, 218, 48, 218, 49, 218, 50, 218, 51, 218, 52, 218, 53, 218, 54, 218, 55, 218, 56, 218, 57, 218, 58, 218, 59, 218, 61, 218, 62, 218, 63, 218, 64, 218, 65, 218, 66, 218, 67, 218, 68, 218, 69, 218],
	#[65535, 219, 14, 219, 20, 219, 22, 219, 13, 219, 15, 219, 2, 219, 11, 219, 30, 219, 18, 219, 21, 219, 23, 219, 10, 219, 19, 219, 17, 219, 38, 219, 16, 219, 29, 219, 31, 219, 24, 219, 27, 219, 25, 219, 46, 219, 12, 219, 34, 219, 37, 219, 39, 219, 26, 219, 35, 219, 33, 219, 54, 219, 49, 219, 32, 219, 45, 219, 47, 219, 40, 219, 43, 219, 41, 219, 62, 219, 28, 219, 50, 219, 53, 219, 55, 219, 42, 219, 51, 219, 68, 219, 52, 219, 36, 219, 58, 219, 61, 219, 63, 219, 56, 219, 59, 219, 57, 219, 44, 219, 66, 219, 69, 219, 48, 219, 64, 219, 67, 219, 65, 219],
	#[30, -37, 14, -86, 22, -62, 17, -112, 13, -105, 15, -111, 2, 96, 28, -34, 12, -82, 21, -27, 23, -21, 10, -109, 19, -12, 36, -116, 25, -26, 29, -7, 18, -40, 27, -4, 44, -95, 46, -98, 43, -115, 37, -97, 39, -114, 26, -57, 35, -96, 24, -54, 54, -107, 16, -90, 42, -92, 45, -117, 47, -72, 34, -51, 33, -59, 38, -91, 62, -83, 59, -118, 50, -101, 53, -81, 55, -100, 51, -78, 68, -56, 52, -103, 32, -102, 20, -55, 61, -104, 63, -110, 41, -106, 49, -75, 57, -88, 40, -74, 66, -52, 69, -58, 48, -99, 64, -47, 67, -23, 65, -61],
	#[65535, 193, 2, 193, 10, 193, 11, 193, 36, 193, 37, 193, 38, 193, 39, 193, 40, 193, 46, 193, 48, 193, 55, 193, 56, 193, 57, 193, 58, 193],
	#[65535, 87, 2, 87, 10, 87, 11, 87],
	#[65535, 189, 2, 189, 10, 189, 11, 189, 36, 189, 37, 189, 38, 189, 39, 189, 40, 189, 46, 189, 48, 189, 55, 189, 56, 189, 57, 189, 58, 189],
	#[10, -249, 12, -82, 13, -105, 14, -86, 15, -111, 16, -90, 17, -112, 18, -40, 19, -12, 20, -55, 21, -27, 22, -62, 23, -21, 24, -54, 25, -26, 26, -57, 27, -4, 28, -34, 29, -7, 30, -37, 31, 256, 32, -102, 33, -59, 34, -51, 35, -96, 36, -116, 37, -97, 38, -91, 39, -114, 40, -234, 41, -106, 42, -243, 43, -253, 44, -247, 45, -254, 46, -98, 47, -72, 48, -99, 49, -75, 50, -101, 51, -78, 52, -103, 53, -81, 54, -107, 55, -248, 57, -241, 59, -255, 61, -104, 62, -83, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[23, -21, 33, -59, 25, -26, 43, -63, 62, -16, 28, -34, 12, -9, 53, -14, 55, -36, 42, -30, 22, -62, 68, -56, 34, -51, 21, -27, 20, -55, 61, -49, 63, -50, 19, -12, 18, -40, 29, -7, 66, -52, 69, -58, 13, -42, 24, -54, 67, -23, 65, -61],
	#[65535, 49, 2, 49, 10, 49, 11, 49, 12, 49, 13, 49, 14, 49, 15, 49, 16, 49, 17, 49, 18, 49, 19, 49, 20, 49, 21, 49, 22, 49, 23, 49, 24, 49, 25, 49, 26, 49, 27, 49, 28, 49, 29, 49, 30, 49, 31, 49, 32, 49, 33, 49, 34, 49, 35, 49, 36, 49, 37, 49, 38, 49, 39, 49, 40, 49, 41, 49, 42, 49, 43, 49, 44, 49, 45, 49, 46, 49, 47, 49, 48, 49, 49, 49, 50, 49, 51, 49, 52, 49, 53, 49, 54, 49, 55, 49, 56, 49, 57, 49, 58, 49, 59, 49, 60, 49, 61, 49, 62, 49, 63, 49, 64, 49, 65, 49, 66, 49, 67, 49, 68, 49, 69, 49],
	#[65535, 197, 39, 197, 36, 197, 10, 197, 38, 197, 55, 197, 37, 197, 46, 197, 2, 197, 40, 197, 11, 197, 48, 197, 58, 197, 57, 197, 56, 197],
	#[65535, 203, 54, 203, 28, 203, 31, 203, 14, 203, 38, 203, 61, 203, 39, 203, 46, 203, 13, 203, 15, 203, 44, 203, 47, 203, 37, 203, 27, 203, 23, 203, 10, 203, 52, 203, 36, 203, 2, 203, 11, 203, 50, 203, 60, 203, 22, 203, 53, 203, 43, 203, 19, 203, 59, 203, 26, 203, 21, 203, 30, 203, 18, 203, 20, 203, 42, 203, 45, 203, 17, 203, 34, 203, 29, 203, 41, 203, 62, 203, 24, 203, 12, 203, 35, 203, 55, 203, 33, 203, 51, 203, 68, 203, 16, 203, 32, 203, 58, 203, 25, 203, 63, 203, 56, 203, 49, 203, 57, 203, 40, 203, 66, 203, 69, 203, 48, 203, 64, 203, 67, 203, 65, 203],
	#[65535, 11, #"eoi", 11],
	#[65535, 88, 2, 88, 10, 88, 11, 88],
	#[65535, 66, 55, 66],
	#[65535, 35, 2, 35, 10, 35, 11, 35, 12, 35, 13, 35, 14, 35, 15, 35, 16, 35, 17, 35, 18, 35, 19, 35, 20, 35, 21, 35, 22, 35, 23, 35, 24, 35, 25, 35, 26, 35, 27, 35, 28, 35, 29, 35, 30, 35, 31, 35, 32, 35, 33, 35, 34, 35, 35, 35, 36, 35, 37, 35, 38, 35, 39, 35, 40, 35, 41, 35, 42, 35, 43, 35, 44, 35, 45, 35, 46, 35, 47, 35, 48, 35, 49, 35, 50, 35, 51, 35, 52, 35, 53, 35, 54, 35, 55, 35, 56, 35, 57, 35, 58, 35, 59, 35, 60, 35, 61, 35, 62, 35, 63, 35, 64, 35, 65, 35, 66, 35, 67, 35, 68, 35, 69, 35],
	#[22, -62, 15, -111, 2, 100, 11, 100, 12, -82, 18, -40, 21, -27, 23, -21, 10, 100, 19, -12, 14, -86, 38, -91, 16, -90, 29, -7, 31, 100, 24, -54, 13, -105, 25, -26, 46, -98, 41, -106, 34, -51, 37, -97, 39, -114, 17, -112, 35, -96, 33, -59, 54, -107, 49, -75, 32, -102, 45, -117, 47, -72, 40, -304, 43, -115, 60, 100, 62, -83, 28, -34, 50, -101, 53, -81, 55, -100, 42, -92, 51, -78, 68, -56, 52, -103, 36, -116, 20, -55, 61, -104, 63, -110, 56, 100, 59, -118, 57, -88, 44, -95, 66, -52, 69, -58, 48, -99, 58, 100, 67, -23, 65, -61],
	#[65535, 177, 39, 177, 36, 177, 10, 177, 38, 177, 55, 177, 37, 177, 46, 177, 2, 177, 40, 177, 11, 177, 48, 177, 58, 177, 57, 177, 56, 177],
	#[65535, 190, 2, 190, 10, 190, 11, 190, 36, 190, 37, 190, 38, 190, 39, 190, 40, 190, 46, 190, 48, 190, 55, 190, 56, 190, 57, 190, 58, 190],
	#[65535, 68, 23, 68, 33, 68, 25, 68, 43, 68, 62, 68, 28, 68, 12, 68, 53, 68, 55, 68, 42, 68, 22, 68, 68, 68, 34, 68, 21, 68, 20, 68, 61, 68, 63, 68, 19, 68, 18, 68, 29, 68, 66, 68, 69, 68, 13, 68, 24, 68, 67, 68, 65, 68],
	#[65535, 162, 2, 162, 10, 162, 11, 162, 31, 162, 56, 162, 58, 162, 60, 162],
	#[65535, 89, 2, 89, 10, 89, 11, 89],
	#[65535, 53, 12, 53, 14, 53, 20, 53, 22, 53, 13, 53, 15, 53, 2, 53, 11, 53, 30, 53, 18, 53, 21, 53, 23, 53, 10, 53, 19, 53, 17, 53, 38, 53, 16, 53, 29, 53, 31, 53, 24, 53, 27, 53, 25, 53, 46, 53, 41, 53, 34, 53, 37, 53, 39, 53, 26, 53, 35, 53, 33, 53, 54, 53, 49, 53, 32, 53, 45, 53, 47, 53, 40, 53, 43, 53, 60, 53, 62, 53, 28, 53, 50, 53, 53, 53, 55, 53, 42, 53, 51, 53, 68, 53, 52, 53, 36, 53, 58, 53, 61, 53, 63, 53, 56, 53, 59, 53, 57, 53, 44, 53, 66, 53, 69, 53, 48, 53, 64, 53, 67, 53, 65, 53],
	#[12, -9, 13, -42, 18, -40, 19, -12, 20, -55, 21, -27, 22, -62, 23, -21, 24, -54, 25, -26, 28, -34, 29, -7, 33, -59, 34, -51, 35, -31, 36, -66, 41, -43, 42, -30, 43, -63, 53, -14, 55, -36, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[12, -9, 13, -42, 18, -40, 19, -12, 20, -55, 21, -27, 22, -62, 23, -21, 24, -54, 25, -26, 28, -34, 29, -7, 33, -59, 34, -51, 35, -31, 36, -66, 41, -43, 42, -30, 43, -63, 53, -14, 67, -23, 55, -36, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 68, -56, 69, -58],
	#[20, 167, 21, 167, 22, 167, 23, 167, 24, 167, 25, 167, 29, 167, 33, -288, 34, 167],
	#[65535, 186, 2, 186, 10, 186, 11, 186, 36, 186, 37, 186, 38, 186, 39, 186, 40, 186, 46, 186, 48, 186, 55, 186, 56, 186, 57, 186, 58, 186],
	#[23, -21, 10, -109, 12, -82, 13, -105, 14, -86, 15, -111, 16, -90, 17, -112, 18, -40, 19, -12, 20, -55, 21, -27, 22, -62, 34, -51, 24, -54, 25, -26, 26, -57, 27, -4, 28, -34, 29, -7, 30, -37, 31, 96, 32, -102, 33, -59, 63, -110, 35, -96, 36, -116, 37, -97, 38, -91, 39, -114, 40, -74, 41, -106, 42, -92, 43, -115, 44, -95, 45, -117, 46, -98, 47, -72, 48, -99, 49, -75, 50, -101, 51, -78, 52, -103, 53, -81, 54, -107, 55, -100, 57, -88, 59, -118, 61, -104, 62, -83, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 48, 15, 48, 23, 48, 2, 48, 19, 48, 18, 48, 10, 48, 11, 48, 12, 48, 13, 48, 14, 48, 53, 48, 16, 48, 17, 48, 47, 48, 36, 48, 20, 48, 21, 48, 22, 48, 34, 48, 24, 48, 25, 48, 26, 48, 27, 48, 28, 48, 29, 48, 30, 48, 31, 48, 32, 48, 33, 48, 63, 48, 35, 48, 58, 48, 37, 48, 38, 48, 39, 48, 40, 48, 41, 48, 42, 48, 43, 48, 44, 48, 45, 48, 46, 48, 48, 48, 49, 48, 50, 48, 51, 48, 52, 48, 54, 48, 55, 48, 56, 48, 57, 48, 65, 48, 59, 48, 60, 48, 61, 48, 62, 48, 64, 48, 66, 48, 67, 48, 68, 48, 69, 48],
	#[65535, 224, 2, 224, 10, 224, 11, 224, 36, 224, 37, 224, 38, 224, 39, 224, 40, 224, 48, 224, 56, 224, 58, 224],
	#[65535, 196, 2, 196, 10, 196, 11, 196, 36, 196, 37, 196, 38, 196, 39, 196, 40, 196, 46, 196, 48, 196, 55, 196, 56, 196, 57, 196, 58, 196],
	#[65535, 229, 2, 229, 10, 229, 11, 229, 36, 229, 37, 229, 38, 229, 39, 229, 40, 229, 48, 229, 56, 229, 58, 229],
	#[18, -40, 20, -55, 21, -27, 22, -62, 23, -21, 28, -34, 29, -7, 33, -59, 34, -51, 53, -14],
	#[2, 90, 41, -382],
	#[65535, 185, 2, 185, 10, 185, 11, 185, 36, 185, 37, 185, 38, 185, 39, 185, 40, 185, 46, 185, 48, 185, 55, 185, 56, 185, 57, 185, 58, 185],
	#[65535, 156, 2, 156, 10, 156, 11, 156, 31, 156, 56, 156, 58, 156, 60, 156],
	#[#"eoi", #"accept"],
	#[65535, 192, 2, 192, 10, 192, 11, 192, 36, 192, 37, 192, 38, 192, 39, 192, 40, 192, 46, 192, 48, 192, 55, 192, 56, 192, 57, 192, 58, 192],
	#[2, 198, 10, 198, 11, 198, 63, -50, 36, 198, 37, 198, 38, 198, 39, 198, 40, 198, 46, 198, 48, 198, 55, 198, 56, 198, 57, 198, 58, 198],
	#[65535, 38, 12, 38, 14, 38, 20, 38, 22, 38, 13, 38, 15, 38, 2, 38, 11, 38, 30, 38, 18, 38, 21, 38, 23, 38, 10, 38, 19, 38, 17, 38, 38, 38, 16, 38, 29, 38, 31, 38, 24, 38, 27, 38, 25, 38, 46, 38, 41, 38, 34, 38, 37, 38, 39, 38, 26, 38, 35, 38, 33, 38, 54, 38, 49, 38, 32, 38, 45, 38, 47, 38, 40, 38, 43, 38, 60, 38, 62, 38, 28, 38, 50, 38, 53, 38, 55, 38, 42, 38, 51, 38, 68, 38, 52, 38, 36, 38, 58, 38, 61, 38, 63, 38, 56, 38, 59, 38, 57, 38, 44, 38, 66, 38, 69, 38, 48, 38, 64, 38, 67, 38, 65, 38],
	#[12, -9, 13, -42, 41, -125, 58, 205, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56],
	#[10, 223, 2, 223, 38, -156, 37, -161, 36, -193, 39, -190, 11, 223, 58, 223, 40, 223, 48, 223, 56, 223],
	#[65535, 67, 55, 67],
	#[65535, 34, 2, 34, 10, 34, 11, 34, 12, 34, 13, 34, 14, 34, 15, 34, 16, 34, 17, 34, 18, 34, 19, 34, 20, 34, 21, 34, 22, 34, 23, 34, 24, 34, 25, 34, 26, 34, 27, 34, 28, 34, 29, 34, 30, 34, 31, 34, 32, 34, 33, 34, 34, 34, 35, 34, 36, 34, 37, 34, 38, 34, 39, 34, 40, 34, 41, 34, 42, 34, 43, 34, 44, 34, 45, 34, 46, 34, 47, 34, 48, 34, 49, 34, 50, 34, 51, 34, 52, 34, 53, 34, 54, 34, 55, 34, 56, 34, 57, 34, 58, 34, 59, 34, 60, 34, 61, 34, 62, 34, 63, 34, 64, 34, 65, 34, 66, 34, 67, 34, 68, 34, 69, 34],
	#[65535, 204, 2, 204, 23, 204, 31, 204, 18, 204, 10, 204, 11, 204, 12, 204, 13, 204, 14, 204, 15, 204, 16, 204, 17, 204, 47, 204, 19, 204, 20, 204, 21, 204, 22, 204, 34, 204, 24, 204, 25, 204, 26, 204, 27, 204, 28, 204, 29, 204, 30, 204, 54, 204, 32, 204, 33, 204, 63, 204, 35, 204, 36, 204, 37, 204, 38, 204, 39, 204, 40, 204, 41, 204, 42, 204, 43, 204, 44, 204, 45, 204, 46, 204, 58, 204, 48, 204, 49, 204, 50, 204, 51, 204, 52, 204, 53, 204, 67, 204, 55, 204, 56, 204, 57, 204, 65, 204, 59, 204, 60, 204, 61, 204, 62, 204, 64, 204, 66, 204, 68, 204, 69, 204],
	#[65535, 63, 22, 63, 15, 63, 2, 63, 11, 63, 21, 63, 23, 63, 10, 63, 19, 63, 14, 63, 38, 63, 16, 63, 29, 63, 31, 63, 18, 63, 13, 63, 25, 63, 46, 63, 12, 63, 24, 63, 37, 63, 39, 63, 17, 63, 35, 63, 33, 63, 54, 63, 49, 63, 32, 63, 45, 63, 47, 63, 40, 63, 43, 63, 41, 63, 62, 63, 28, 63, 50, 63, 53, 63, 55, 63, 42, 63, 51, 63, 68, 63, 34, 63, 36, 63, 20, 63, 61, 63, 63, 63, 56, 63, 59, 63, 57, 63, 60, 63, 44, 63, 66, 63, 69, 63, 48, 63, 58, 63, 67, 63, 65, 63, 52, 63],
	#[10, -179, 12, -82, 13, -105, 14, -86, 15, -111, 16, -90, 17, -112, 18, -172, 19, -145, 20, -178, 21, -149, 22, -183, 23, -150, 24, -186, 25, -153, 26, -187, 27, -137, 28, -164, 29, -140, 30, -168, 31, -143, 32, -173, 33, -189, 35, -158, 36, -193, 37, -161, 38, -156, 39, -190, 40, -142, 41, -171, 42, -157, 43, -192, 44, -159, 45, -194, 46, -163, 47, -138, 48, -166, 49, 343, 50, -169, 51, -144, 52, -174, 53, -146, 55, -167, 57, -151, 59, -195, 60, 300, 61, -176, 62, -148, 63, -180, 64, -175, 65, -191, 66, -182, 67, -154, 68, -185, 69, -188],
	#[65535, 36, 14, 36, 20, 36, 22, 36, 13, 36, 15, 36, 2, 36, 11, 36, 21, 36, 23, 36, 10, 36, 19, 36, 17, 36, 38, 36, 16, 36, 29, 36, 31, 36, 27, 36, 25, 36, 46, 36, 12, 36, 24, 36, 37, 36, 39, 36, 26, 36, 35, 36, 30, 36, 18, 36, 49, 36, 32, 36, 45, 36, 47, 36, 40, 36, 33, 36, 41, 36, 62, 36, 28, 36, 50, 36, 53, 36, 55, 36, 42, 36, 51, 36, 68, 36, 34, 36, 36, 36, 58, 36, 43, 36, 63, 36, 56, 36, 59, 36, 54, 36, 60, 36, 44, 36, 66, 36, 69, 36, 48, 36, 64, 36, 57, 36, 65, 36, 52, 36, 67, 36, 61, 36],
	#[65535, 37, 2, 37, 10, 37, 11, 37, 12, 37, 13, 37, 14, 37, 15, 37, 16, 37, 17, 37, 18, 37, 19, 37, 20, 37, 21, 37, 22, 37, 23, 37, 24, 37, 25, 37, 26, 37, 27, 37, 28, 37, 29, 37, 30, 37, 31, 37, 32, 37, 33, 37, 34, 37, 35, 37, 36, 37, 37, 37, 38, 37, 39, 37, 40, 37, 41, 37, 42, 37, 43, 37, 44, 37, 45, 37, 46, 37, 47, 37, 48, 37, 49, 37, 50, 37, 51, 37, 52, 37, 53, 37, 54, 37, 55, 37, 56, 37, 57, 37, 58, 37, 59, 37, 60, 37, 61, 37, 62, 37, 63, 37, 64, 37, 65, 37, 66, 37, 67, 37, 68, 37, 69, 37],
	#[12, -9, 13, -42, 41, -125, 56, 205, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56],
	#[65535, 50, 14, 50, 20, 50, 22, 50, 13, 50, 15, 50, 2, 50, 11, 50, 30, 50, 21, 50, 23, 50, 10, 50, 19, 50, 17, 50, 38, 50, 16, 50, 29, 50, 31, 50, 18, 50, 27, 50, 25, 50, 46, 50, 12, 50, 24, 50, 37, 50, 39, 50, 26, 50, 35, 50, 33, 50, 54, 50, 49, 50, 32, 50, 45, 50, 47, 50, 40, 50, 43, 50, 41, 50, 62, 50, 28, 50, 50, 50, 53, 50, 55, 50, 42, 50, 51, 50, 68, 50, 34, 50, 36, 50, 58, 50, 61, 50, 63, 50, 56, 50, 59, 50, 57, 50, 60, 50, 44, 50, 66, 50, 69, 50, 48, 50, 64, 50, 67, 50, 65, 50, 52, 50],
	#[65535, 191, 2, 191, 10, 191, 11, 191, 36, 191, 37, 191, 38, 191, 39, 191, 40, 191, 46, 191, 48, 191, 55, 191, 56, 191, 57, 191, 58, 191],
	#[39, 231, 10, 231, 2, 231, 38, 231, 55, -214, 37, 231, 46, -213, 36, 231, 40, 231, 11, 231, 58, 231, 57, -215, 48, 231, 56, 231],
	#[65535, 230, 10, 230, 2, 230, 38, 230, 37, 230, 36, 230, 39, 230, 11, 230, 58, 230, 40, 230, 48, 230, 56, 230],
	#[65535, 69, 12, 69, 13, 69, 18, 69, 19, 69, 20, 69, 21, 69, 22, 69, 23, 69, 24, 69, 25, 69, 28, 69, 29, 69, 33, 69, 34, 69, 42, 69, 43, 69, 53, 69, 55, 69, 61, 69, 62, 69, 63, 69, 65, 69, 66, 69, 67, 69, 68, 69, 69, 69],
	#[55, -69],
	#[65535, 194, 39, 194, 10, 194, 2, 194, 38, 194, 55, 194, 37, 194, 46, 194, 36, 194, 40, 194, 11, 194, 58, 194, 57, 194, 48, 194, 56, 194],
	#[10, -109, 12, -82, 13, -105, 14, -86, 15, -111, 16, -90, 17, -112, 18, -40, 19, -12, 20, -55, 21, -27, 22, -62, 23, -21, 24, -54, 25, -26, 26, -57, 27, -4, 28, -34, 29, -7, 30, -37, 32, -102, 33, -59, 34, -51, 35, -96, 36, -116, 37, -97, 38, -91, 39, -114, 40, -74, 41, -106, 42, -92, 43, -115, 44, -95, 45, -117, 46, -98, 47, -72, 48, -99, 49, -75, 50, -101, 51, -78, 52, -103, 53, -81, 54, -107, 55, -100, 56, 96, 57, -88, 59, -118, 61, -104, 62, -83, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 130, 14, 130, 20, 130, 22, 130, 13, 130, 15, 130, 2, 130, 11, 130, 30, 130, 21, 130, 23, 130, 10, 130, 19, 130, 17, 130, 38, 130, 16, 130, 29, 130, 31, 130, 18, 130, 27, 130, 25, 130, 46, 130, 12, 130, 24, 130, 37, 130, 39, 130, 26, 130, 35, 130, 33, 130, 54, 130, 49, 130, 32, 130, 45, 130, 47, 130, 40, 130, 43, 130, 41, 130, 62, 130, 28, 130, 50, 130, 53, 130, 55, 130, 42, 130, 51, 130, 68, 130, 34, 130, 36, 130, 58, 130, 61, 130, 63, 130, 56, 130, 59, 130, 57, 130, 60, 130, 44, 130, 66, 130, 69, 130, 48, 130, 64, 130, 67, 130, 65, 130, 52, 130],
	#[65535, 126, 14, 126, 20, 126, 22, 126, 13, 126, 15, 126, 2, 126, 11, 126, 21, 126, 23, 126, 10, 126, 19, 126, 17, 126, 38, 126, 16, 126, 29, 126, 31, 126, 27, 126, 25, 126, 46, 126, 12, 126, 24, 126, 37, 126, 39, 126, 26, 126, 35, 126, 30, 126, 18, 126, 49, 126, 32, 126, 45, 126, 47, 126, 40, 126, 33, 126, 41, 126, 62, 126, 28, 126, 50, 126, 53, 126, 55, 126, 42, 126, 51, 126, 68, 126, 34, 126, 36, 126, 58, 126, 43, 126, 63, 126, 56, 126, 59, 126, 54, 126, 60, 126, 44, 126, 66, 126, 69, 126, 48, 126, 64, 126, 57, 126, 65, 126, 52, 126, 67, 126, 61, 126],
	#[65535, 137, 2, 137, 10, 137, 11, 137, 12, 137, 13, 137, 14, 137, 15, 137, 16, 137, 17, 137, 18, 137, 19, 137, 20, 137, 21, 137, 22, 137, 23, 137, 24, 137, 25, 137, 26, 137, 27, 137, 28, 137, 29, 137, 30, 137, 31, 137, 32, 137, 33, 137, 34, 137, 35, 137, 36, 137, 37, 137, 38, 137, 39, 137, 40, 137, 41, 137, 42, 137, 43, 137, 44, 137, 45, 137, 46, 137, 47, 137, 48, 137, 49, 137, 50, 137, 51, 137, 52, 137, 53, 137, 54, 137, 55, 137, 56, 137, 57, 137, 58, 137, 59, 137, 60, 137, 61, 137, 62, 137, 63, 137, 64, 137, 65, 137, 66, 137, 67, 137, 68, 137, 69, 137],
	#[56, -378],
	#[14, -86, 20, -55, 22, -62, 13, -105, 15, -111, 21, -27, 23, -21, 10, -109, 19, -12, 17, -112, 38, -91, 16, -90, 29, -7, 31, 96, 18, -40, 27, -4, 25, -26, 46, -98, 12, -82, 24, -54, 37, -97, 39, -114, 26, -57, 35, -96, 30, -37, 36, -116, 49, -75, 32, -102, 45, -117, 47, -72, 40, -74, 33, -59, 41, -106, 62, -83, 28, -34, 50, -101, 53, -81, 55, -100, 42, -92, 51, -78, 68, -56, 34, -51, 2, 96, 58, 96, 43, -115, 63, -110, 56, 96, 59, -118, 54, -107, 60, 96, 44, -95, 66, -52, 69, -58, 48, -99, 64, -47, 57, -88, 65, -61, 52, -103, 67, -23, 61, -104],
	#[65535, 139, 2, 139, 10, 139, 11, 139, 12, 139, 13, 139, 14, 139, 15, 139, 16, 139, 17, 139, 18, 139, 19, 139, 20, 139, 21, 139, 22, 139, 23, 139, 24, 139, 25, 139, 26, 139, 27, 139, 28, 139, 29, 139, 30, 139, 31, 139, 32, 139, 33, 139, 34, 139, 35, 139, 36, 139, 37, 139, 38, 139, 39, 139, 40, 139, 41, 139, 42, 139, 43, 139, 44, 139, 45, 139, 46, 139, 47, 139, 48, 139, 49, 139, 50, 139, 51, 139, 52, 139, 53, 139, 54, 139, 55, 139, 56, 139, 57, 139, 58, 139, 59, 139, 60, 139, 61, 139, 62, 139, 63, 139, 64, 139, 65, 139, 66, 139, 67, 139, 68, 139, 69, 139],
	#[65535, 98, 2, 98, 31, 98, 60, 98, 58, 98, 56, 98],
	#[65535, 129, 2, 129, 10, 129, 11, 129, 12, 129, 13, 129, 14, 129, 15, 129, 16, 129, 17, 129, 18, 129, 19, 129, 20, 129, 21, 129, 22, 129, 23, 129, 24, 129, 25, 129, 26, 129, 27, 129, 28, 129, 29, 129, 30, 129, 31, 129, 32, 129, 33, 129, 34, 129, 35, 129, 36, 129, 37, 129, 38, 129, 39, 129, 40, 129, 41, 129, 42, 129, 43, 129, 44, 129, 45, 129, 46, 129, 47, 129, 48, 129, 49, 129, 50, 129, 51, 129, 52, 129, 53, 129, 54, 129, 55, 129, 56, 129, 57, 129, 58, 129, 59, 129, 60, 129, 61, 129, 62, 129, 63, 129, 64, 129, 65, 129, 66, 129, 67, 129, 68, 129, 69, 129],
	#[65535, 141, 14, 141, 2, 141, 31, 141, 13, 141, 22, 141, 10, 141, 37, 141, 39, 141, 17, 141, 21, 141, 30, 141, 15, 141, 27, 141, 47, 141, 25, 141, 29, 141, 38, 141, 26, 141, 23, 141, 12, 141, 18, 141, 32, 141, 11, 141, 51, 141, 34, 141, 36, 141, 20, 141, 43, 141, 33, 141, 19, 141, 45, 141, 42, 141, 44, 141, 28, 141, 69, 141, 41, 141, 49, 141, 40, 141, 16, 141, 50, 141, 52, 141, 35, 141, 46, 141, 24, 141, 48, 141, 68, 141, 62, 141, 59, 141, 58, 141, 53, 141, 54, 141, 55, 141, 56, 141, 57, 141, 64, 141, 60, 141, 61, 141, 63, 141, 65, 141, 66, 141, 67, 141],
	#[14, -86, 20, -55, 22, -62, 13, -105, 15, -111, 2, 96, 21, -27, 23, -21, 10, -109, 19, -12, 17, -112, 38, -91, 16, -90, 29, -7, 31, 96, 27, -4, 25, -26, 46, -98, 12, -82, 24, -54, 37, -97, 39, -114, 26, -57, 35, -96, 30, -37, 18, -40, 49, -75, 32, -102, 45, -117, 47, -72, 40, -74, 33, -59, 41, -106, 62, -83, 28, -34, 50, -101, 53, -81, 55, -100, 42, -92, 51, -78, 68, -56, 34, -51, 36, -116, 58, 96, 43, -115, 63, -110, 56, 96, 59, -118, 54, -107, 60, 96, 44, -95, 66, -52, 69, -58, 48, -99, 64, -47, 57, -88, 65, -61, 52, -103, 67, -23, 61, -104],
	#[65535, 125, 2, 125, 10, 125, 11, 125, 12, 125, 13, 125, 14, 125, 15, 125, 16, 125, 17, 125, 18, 125, 19, 125, 20, 125, 21, 125, 22, 125, 23, 125, 24, 125, 25, 125, 26, 125, 27, 125, 28, 125, 29, 125, 30, 125, 31, 125, 32, 125, 33, 125, 34, 125, 35, 125, 36, 125, 37, 125, 38, 125, 39, 125, 40, 125, 41, 125, 42, 125, 43, 125, 44, 125, 45, 125, 46, 125, 47, 125, 48, 125, 49, 125, 50, 125, 51, 125, 52, 125, 53, 125, 54, 125, 55, 125, 56, 125, 57, 125, 58, 125, 59, 125, 60, 125, 61, 125, 62, 125, 63, 125, 64, 125, 65, 125, 66, 125, 67, 125, 68, 125, 69, 125],
	#[65535, 143, 14, 143, 20, 143, 22, 143, 13, 143, 15, 143, 2, 143, 11, 143, 30, 143, 21, 143, 23, 143, 10, 143, 19, 143, 17, 143, 38, 143, 16, 143, 29, 143, 31, 143, 18, 143, 27, 143, 25, 143, 46, 143, 12, 143, 24, 143, 37, 143, 39, 143, 26, 143, 35, 143, 33, 143, 54, 143, 49, 143, 32, 143, 45, 143, 47, 143, 40, 143, 43, 143, 41, 143, 62, 143, 28, 143, 50, 143, 53, 143, 55, 143, 42, 143, 51, 143, 68, 143, 34, 143, 36, 143, 58, 143, 61, 143, 63, 143, 56, 143, 59, 143, 57, 143, 60, 143, 44, 143, 66, 143, 69, 143, 48, 143, 64, 143, 67, 143, 65, 143, 52, 143],
	#[65535, 12, 14, 12, 20, 12, 22, 12, 13, 12, 15, 12, 2, 12, 11, 12, 21, 12, 23, 12, 10, 12, 19, 12, 17, 12, 38, 12, 16, 12, 29, 12, 31, 12, 27, 12, 25, 12, 46, 12, 12, 12, 24, 12, 37, 12, 39, 12, 26, 12, 35, 12, 30, 12, 18, 12, 49, 12, 32, 12, 45, 12, 47, 12, 40, 12, 33, 12, 41, 12, 62, 12, 28, 12, 50, 12, 53, 12, 55, 12, 42, 12, 51, 12, 68, 12, 34, 12, 36, 12, 58, 12, 43, 12, 63, 12, 56, 12, 59, 12, 54, 12, 60, 12, 44, 12, 66, 12, 69, 12, 48, 12, 64, 12, 57, 12, 65, 12, 52, 12, 67, 12, 61, 12],
	#[65535, 150, 2, 150, 10, 150, 11, 150, 12, 150, 13, 150, 14, 150, 15, 150, 16, 150, 17, 150, 18, 150, 19, 150, 20, 150, 21, 150, 22, 150, 23, 150, 24, 150, 25, 150, 26, 150, 27, 150, 28, 150, 29, 150, 30, 150, 31, 150, 32, 150, 33, 150, 34, 150, 35, 150, 36, 150, 37, 150, 38, 150, 39, 150, 40, 150, 41, 150, 42, 150, 43, 150, 44, 150, 45, 150, 46, 150, 47, 150, 48, 150, 49, 150, 50, 150, 51, 150, 52, 150, 53, 150, 54, 150, 55, 150, 56, 150, 57, 150, 58, 150, 59, 150, 60, 150, 61, 150, 62, 150, 63, 150, 64, 150, 65, 150, 66, 150, 67, 150, 68, 150, 69, 150],
	#[58, 115, 10, -311, 31, 115, 2, 115, 56, 115, 60, 115],
	#[14, -86, 20, -55, 13, -105, 15, -111, 2, 108, 30, -37, 54, -107, 21, -27, 10, -109, 19, -12, 17, -112, 38, -91, 16, -90, 31, 108, 27, -4, 25, -26, 46, -98, 12, -82, 37, -97, 39, -114, 26, -57, 35, -96, 33, -59, 61, -104, 49, -75, 32, -102, 45, -117, 47, -72, 40, -74, 43, -115, 58, 108, 62, -83, 24, -54, 50, -101, 53, -81, 55, -100, 42, -92, 41, -106, 68, -56, 34, -51, 36, -116, 63, -110, 56, 108, 59, -118, 57, -88, 60, 108, 44, -95, 66, -52, 51, -78, 48, -99, 64, -47, 67, -23, 65, -61, 52, -103],
	#[65535, 14, 2, 14, 10, 14, 11, 14, 12, 14, 13, 14, 14, 14, 15, 14, 16, 14, 17, 14, 18, 14, 19, 14, 20, 14, 21, 14, 22, 14, 23, 14, 24, 14, 25, 14, 26, 14, 27, 14, 28, 14, 29, 14, 30, 14, 31, 14, 32, 14, 33, 14, 34, 14, 35, 14, 36, 14, 37, 14, 38, 14, 39, 14, 40, 14, 41, 14, 42, 14, 43, 14, 44, 14, 45, 14, 46, 14, 47, 14, 48, 14, 49, 14, 50, 14, 51, 14, 52, 14, 53, 14, 54, 14, 55, 14, 56, 14, 57, 14, 58, 14, 59, 14, 60, 14, 61, 14, 62, 14, 63, 14, 64, 14, 65, 14, 66, 14, 67, 14, 68, 14, 69, 14],
	#[65535, 153, 15, 153, 11, 153, 26, 153, 28, 153, 38, 153, 27, 153, 22, 153, 25, 153, 13, 153, 35, 153, 2, 153, 24, 153, 47, 153, 30, 153, 54, 153, 33, 153, 21, 153, 10, 153, 19, 153, 14, 153, 20, 153, 62, 153, 16, 153, 29, 153, 31, 153, 18, 153, 17, 153, 44, 153, 46, 153, 12, 153, 34, 153, 37, 153, 39, 153, 32, 153, 68, 153, 52, 153, 41, 153, 49, 153, 42, 153, 45, 153, 66, 153, 40, 153, 43, 153, 60, 153, 23, 153, 50, 153, 53, 153, 55, 153, 51, 153, 56, 153, 36, 153, 58, 153, 64, 153, 63, 153, 59, 153, 57, 153, 65, 153, 69, 153, 48, 153, 67, 153, 61, 153],
	#[12, -82, 20, -55, 18, -40, 14, -86, 39, -114, 26, -57, 15, -111, 22, -62, 10, -109, 34, -51, 23, -21, 13, -105, 30, -37, 17, -112, 19, -12, 28, -34, 24, -54, 21, -27, 38, -91, 61, -104, 25, -26, 27, -4, 36, -116, 43, -115, 29, -7, 46, -98, 40, -74, 16, -90, 35, -96, 44, -95, 47, -72, 37, -97, 54, -107, 33, -59, 68, -56, 52, -103, 48, -99, 45, -117, 62, -83, 41, -106, 32, -102, 50, -101, 69, -58, 63, -110, 53, -81, 66, -52, 49, -75, 65, -61, 51, -78, 42, -92, 67, -23, 58, 96, 57, -88, 55, -100, 64, -47, 59, -118],
	#[2, 115, 10, -311, 31, 115, 56, 115, 58, 115, 60, 115],
	#[65535, 16, 13, 16, 21, 16, 12, 16, 11, 16, 29, 16, 18, 16, 15, 16, 38, 16, 20, 16, 19, 16, 37, 16, 23, 16, 46, 16, 28, 16, 27, 16, 10, 16, 45, 16, 25, 16, 2, 16, 31, 16, 16, 16, 36, 16, 35, 16, 17, 16, 53, 16, 33, 16, 14, 16, 39, 16, 62, 16, 44, 16, 43, 16, 26, 16, 61, 16, 24, 16, 22, 16, 47, 16, 32, 16, 51, 16, 34, 16, 41, 16, 49, 16, 30, 16, 55, 16, 40, 16, 60, 16, 59, 16, 42, 16, 64, 16, 57, 16, 63, 16, 58, 16, 68, 16, 52, 16, 50, 16, 48, 16, 69, 16, 66, 16, 54, 16, 56, 16, 65, 16, 67, 16],
	#[65535, 77, 2, 77, 10, 77, 11, 77, 12, 77, 13, 77, 14, 77, 15, 77, 16, 77, 17, 77, 18, 77, 19, 77, 20, 77, 21, 77, 22, 77, 23, 77, 24, 77, 25, 77, 26, 77, 27, 77, 28, 77, 29, 77, 30, 77, 31, 77, 32, 77, 33, 77, 34, 77, 35, 77, 36, 77, 37, 77, 38, 77, 39, 77, 40, 77, 41, 77, 42, 77, 43, 77, 44, 77, 45, 77, 46, 77, 47, 77, 48, 77, 49, 77, 50, 77, 51, 77, 52, 77, 53, 77, 54, 77, 55, 77, 56, 77, 57, 77, 58, 77, 59, 77, 60, 77, 61, 77, 62, 77, 63, 77, 64, 77, 65, 77, 66, 77, 67, 77, 68, 77, 69, 77],
	#[65535, 132, 15, 132, 10, 132, 2, 132, 23, 132, 22, 132, 21, 132, 19, 132, 18, 132, 35, 132, 39, 132, 11, 132, 12, 132, 13, 132, 14, 132, 26, 132, 16, 132, 17, 132, 47, 132, 20, 132, 34, 132, 24, 132, 25, 132, 55, 132, 27, 132, 28, 132, 29, 132, 30, 132, 31, 132, 32, 132, 33, 132, 53, 132, 52, 132, 36, 132, 37, 132, 38, 132, 62, 132, 40, 132, 41, 132, 42, 132, 43, 132, 44, 132, 45, 132, 46, 132, 58, 132, 48, 132, 49, 132, 50, 132, 51, 132, 54, 132, 66, 132, 56, 132, 57, 132, 59, 132, 60, 132, 61, 132, 63, 132, 64, 132, 65, 132, 67, 132, 68, 132, 69, 132],
	#[65535, 97, 2, 97, 31, 97, 58, 97, 60, 97, 56, 97],
	#[65535, 131, 2, 131, 10, 131, 11, 131, 12, 131, 13, 131, 14, 131, 15, 131, 16, 131, 17, 131, 18, 131, 19, 131, 20, 131, 21, 131, 22, 131, 23, 131, 24, 131, 25, 131, 26, 131, 27, 131, 28, 131, 29, 131, 30, 131, 31, 131, 32, 131, 33, 131, 34, 131, 35, 131, 36, 131, 37, 131, 38, 131, 39, 131, 40, 131, 41, 131, 42, 131, 43, 131, 44, 131, 45, 131, 46, 131, 47, 131, 48, 131, 49, 131, 50, 131, 51, 131, 52, 131, 53, 131, 54, 131, 55, 131, 56, 131, 57, 131, 58, 131, 59, 131, 60, 131, 61, 131, 62, 131, 63, 131, 64, 131, 65, 131, 66, 131, 67, 131, 68, 131, 69, 131],
	#[65535, 134, 14, 134, 20, 134, 22, 134, 13, 134, 15, 134, 2, 134, 11, 134, 30, 134, 21, 134, 23, 134, 10, 134, 19, 134, 17, 134, 38, 134, 16, 134, 29, 134, 31, 134, 18, 134, 27, 134, 25, 134, 46, 134, 12, 134, 24, 134, 37, 134, 39, 134, 26, 134, 35, 134, 33, 134, 54, 134, 49, 134, 32, 134, 45, 134, 47, 134, 40, 134, 43, 134, 41, 134, 62, 134, 28, 134, 50, 134, 53, 134, 55, 134, 42, 134, 51, 134, 68, 134, 34, 134, 36, 134, 58, 134, 61, 134, 63, 134, 56, 134, 59, 134, 57, 134, 60, 134, 44, 134, 66, 134, 69, 134, 48, 134, 64, 134, 67, 134, 65, 134, 52, 134],
	#[65535, 74, 13, 74, 21, 74, 30, 74, 12, 74, 11, 74, 29, 74, 15, 74, 38, 74, 20, 74, 19, 74, 23, 74, 46, 74, 28, 74, 27, 74, 10, 74, 45, 74, 2, 74, 31, 74, 16, 74, 36, 74, 35, 74, 18, 74, 53, 74, 33, 74, 14, 74, 39, 74, 44, 74, 43, 74, 26, 74, 61, 74, 24, 74, 22, 74, 47, 74, 32, 74, 52, 74, 51, 74, 34, 74, 41, 74, 49, 74, 55, 74, 17, 74, 25, 74, 59, 74, 42, 74, 64, 74, 40, 74, 63, 74, 58, 74, 68, 74, 67, 74, 50, 74, 48, 74, 69, 74, 66, 74, 60, 74, 65, 74, 37, 74, 54, 74, 62, 74, 56, 74, 57, 74],
	#[65535, 75, 2, 75, 10, 75, 11, 75, 12, 75, 13, 75, 14, 75, 15, 75, 16, 75, 17, 75, 18, 75, 19, 75, 20, 75, 21, 75, 22, 75, 23, 75, 24, 75, 25, 75, 26, 75, 27, 75, 28, 75, 29, 75, 30, 75, 31, 75, 32, 75, 33, 75, 34, 75, 35, 75, 36, 75, 37, 75, 38, 75, 39, 75, 40, 75, 41, 75, 42, 75, 43, 75, 44, 75, 45, 75, 46, 75, 47, 75, 48, 75, 49, 75, 50, 75, 51, 75, 52, 75, 53, 75, 54, 75, 55, 75, 56, 75, 57, 75, 58, 75, 59, 75, 60, 75, 61, 75, 62, 75, 63, 75, 64, 75, 65, 75, 66, 75, 67, 75, 68, 75, 69, 75],
	#[65535, 136, 2, 136, 31, 136, 10, 136, 11, 136, 12, 136, 13, 136, 14, 136, 15, 136, 16, 136, 17, 136, 18, 136, 19, 136, 20, 136, 21, 136, 22, 136, 23, 136, 24, 136, 25, 136, 26, 136, 27, 136, 28, 136, 29, 136, 30, 136, 54, 136, 32, 136, 33, 136, 34, 136, 35, 136, 36, 136, 37, 136, 38, 136, 39, 136, 40, 136, 41, 136, 42, 136, 43, 136, 44, 136, 45, 136, 46, 136, 47, 136, 48, 136, 49, 136, 50, 136, 51, 136, 52, 136, 53, 136, 67, 136, 55, 136, 56, 136, 57, 136, 58, 136, 59, 136, 60, 136, 61, 136, 62, 136, 63, 136, 64, 136, 65, 136, 66, 136, 68, 136, 69, 136],
	#[65535, 138, 20, 138, 18, 138, 12, 138, 14, 138, 38, 138, 39, 138, 22, 138, 46, 138, 10, 138, 13, 138, 15, 138, 2, 138, 11, 138, 47, 138, 30, 138, 54, 138, 33, 138, 21, 138, 23, 138, 16, 138, 19, 138, 17, 138, 45, 138, 35, 138, 26, 138, 29, 138, 31, 138, 24, 138, 27, 138, 25, 138, 53, 138, 41, 138, 49, 138, 37, 138, 58, 138, 32, 138, 68, 138, 50, 138, 51, 138, 42, 138, 48, 138, 66, 138, 34, 138, 43, 138, 60, 138, 62, 138, 28, 138, 56, 138, 55, 138, 64, 138, 36, 138, 40, 138, 59, 138, 65, 138, 44, 138, 69, 138, 57, 138, 52, 138, 61, 138, 63, 138, 67, 138],
	#[10, -109, 12, -82, 13, -105, 14, -86, 15, -111, 16, -90, 17, -112, 18, -40, 19, -12, 20, -55, 21, -27, 22, -62, 23, -21, 24, -54, 25, -26, 26, -57, 27, -4, 28, -34, 29, -7, 30, -37, 32, -102, 33, -59, 34, -51, 35, -96, 36, -116, 37, -97, 38, -91, 39, -114, 40, -74, 41, -106, 42, -92, 43, -115, 44, -95, 45, -117, 46, -98, 47, -72, 48, -99, 49, -75, 50, -101, 51, -78, 52, -103, 53, -81, 54, -107, 55, -100, 56, 96, 57, -88, 59, -118, 61, -104, 62, -83, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 140, 23, 140, 29, 140, 22, 140, 2, 140, 39, 140, 15, 140, 43, 140, 28, 140, 51, 140, 27, 140, 31, 140, 30, 140, 42, 140, 10, 140, 11, 140, 12, 140, 13, 140, 14, 140, 26, 140, 16, 140, 17, 140, 18, 140, 19, 140, 20, 140, 21, 140, 32, 140, 46, 140, 24, 140, 25, 140, 33, 140, 41, 140, 50, 140, 34, 140, 35, 140, 36, 140, 37, 140, 38, 140, 40, 140, 49, 140, 48, 140, 44, 140, 45, 140, 56, 140, 47, 140, 52, 140, 53, 140, 54, 140, 55, 140, 57, 140, 58, 140, 59, 140, 60, 140, 61, 140, 62, 140, 63, 140, 64, 140, 65, 140, 66, 140, 67, 140, 68, 140, 69, 140],
	#[65535, 145, 13, 145, 21, 145, 12, 145, 11, 145, 29, 145, 15, 145, 38, 145, 19, 145, 23, 145, 28, 145, 27, 145, 10, 145, 45, 145, 2, 145, 31, 145, 36, 145, 20, 145, 18, 145, 53, 145, 16, 145, 14, 145, 39, 145, 34, 145, 44, 145, 43, 145, 26, 145, 61, 145, 24, 145, 22, 145, 47, 145, 17, 145, 40, 145, 41, 145, 32, 145, 30, 145, 55, 145, 50, 145, 60, 145, 59, 145, 42, 145, 35, 145, 63, 145, 25, 145, 33, 145, 52, 145, 57, 145, 48, 145, 46, 145, 69, 145, 66, 145, 58, 145, 51, 145, 37, 145, 54, 145, 68, 145, 62, 145, 56, 145, 49, 145, 65, 145, 67, 145, 64, 145],
	#[65535, 142, 2, 142, 10, 142, 11, 142, 12, 142, 13, 142, 14, 142, 15, 142, 16, 142, 17, 142, 18, 142, 19, 142, 20, 142, 21, 142, 22, 142, 23, 142, 24, 142, 25, 142, 26, 142, 27, 142, 28, 142, 29, 142, 30, 142, 31, 142, 32, 142, 33, 142, 34, 142, 35, 142, 36, 142, 37, 142, 38, 142, 39, 142, 40, 142, 41, 142, 42, 142, 43, 142, 44, 142, 45, 142, 46, 142, 47, 142, 48, 142, 49, 142, 50, 142, 51, 142, 52, 142, 53, 142, 54, 142, 55, 142, 56, 142, 57, 142, 58, 142, 59, 142, 60, 142, 61, 142, 62, 142, 63, 142, 64, 142, 65, 142, 66, 142, 67, 142, 68, 142, 69, 142],
	#[65535, 149, 14, 149, 23, 149, 13, 149, 22, 149, 31, 149, 21, 149, 63, 149, 12, 149, 39, 149, 29, 149, 69, 149, 15, 149, 38, 149, 20, 149, 11, 149, 46, 149, 28, 149, 27, 149, 10, 149, 45, 149, 19, 149, 36, 149, 35, 149, 17, 149, 53, 149, 42, 149, 41, 149, 37, 149, 34, 149, 2, 149, 43, 149, 26, 149, 61, 149, 24, 149, 18, 149, 47, 149, 32, 149, 52, 149, 25, 149, 33, 149, 16, 149, 49, 149, 30, 149, 55, 149, 50, 149, 60, 149, 44, 149, 48, 149, 64, 149, 57, 149, 51, 149, 58, 149, 68, 149, 67, 149, 56, 149, 65, 149, 40, 149, 66, 149, 54, 149, 59, 149, 62, 149],
	#[65535, 13, 2, 13, 10, 13, 11, 13, 12, 13, 13, 13, 14, 13, 15, 13, 16, 13, 17, 13, 18, 13, 19, 13, 20, 13, 21, 13, 22, 13, 23, 13, 24, 13, 25, 13, 26, 13, 27, 13, 28, 13, 29, 13, 30, 13, 31, 13, 32, 13, 33, 13, 34, 13, 35, 13, 36, 13, 37, 13, 38, 13, 39, 13, 40, 13, 41, 13, 42, 13, 43, 13, 44, 13, 45, 13, 46, 13, 47, 13, 48, 13, 49, 13, 50, 13, 51, 13, 52, 13, 53, 13, 54, 13, 55, 13, 56, 13, 57, 13, 58, 13, 59, 13, 60, 13, 61, 13, 62, 13, 63, 13, 64, 13, 65, 13, 66, 13, 67, 13, 68, 13, 69, 13],
	#[65535, 152, 2, 152, 10, 152, 11, 152, 12, 152, 13, 152, 14, 152, 15, 152, 16, 152, 17, 152, 18, 152, 19, 152, 20, 152, 21, 152, 22, 152, 23, 152, 24, 152, 25, 152, 26, 152, 27, 152, 28, 152, 29, 152, 30, 152, 31, 152, 32, 152, 33, 152, 34, 152, 35, 152, 36, 152, 37, 152, 38, 152, 39, 152, 40, 152, 41, 152, 42, 152, 43, 152, 44, 152, 45, 152, 46, 152, 47, 152, 48, 152, 49, 152, 50, 152, 51, 152, 52, 152, 53, 152, 54, 152, 55, 152, 56, 152, 57, 152, 58, 152, 59, 152, 60, 152, 61, 152, 62, 152, 63, 152, 64, 152, 65, 152, 66, 152, 67, 152, 68, 152, 69, 152],
	#[65535, 144, 2, 144, 10, 144, 11, 144, 12, 144, 13, 144, 14, 144, 15, 144, 16, 144, 17, 144, 18, 144, 19, 144, 20, 144, 21, 144, 22, 144, 23, 144, 24, 144, 25, 144, 26, 144, 27, 144, 28, 144, 29, 144, 30, 144, 31, 144, 32, 144, 33, 144, 34, 144, 35, 144, 36, 144, 37, 144, 38, 144, 39, 144, 40, 144, 41, 144, 42, 144, 43, 144, 44, 144, 45, 144, 46, 144, 47, 144, 48, 144, 49, 144, 50, 144, 51, 144, 52, 144, 53, 144, 54, 144, 55, 144, 56, 144, 57, 144, 58, 144, 59, 144, 60, 144, 61, 144, 62, 144, 63, 144, 64, 144, 65, 144, 66, 144, 67, 144, 68, 144, 69, 144],
	#[65535, 128, 2, 128, 10, 128, 11, 128, 12, 128, 13, 128, 14, 128, 15, 128, 16, 128, 17, 128, 18, 128, 19, 128, 20, 128, 21, 128, 22, 128, 23, 128, 24, 128, 25, 128, 26, 128, 27, 128, 28, 128, 29, 128, 30, 128, 31, 128, 32, 128, 33, 128, 34, 128, 35, 128, 36, 128, 37, 128, 38, 128, 39, 128, 40, 128, 41, 128, 42, 128, 43, 128, 44, 128, 45, 128, 46, 128, 47, 128, 48, 128, 49, 128, 50, 128, 51, 128, 52, 128, 53, 128, 54, 128, 55, 128, 56, 128, 57, 128, 58, 128, 59, 128, 60, 128, 61, 128, 62, 128, 63, 128, 64, 128, 65, 128, 66, 128, 67, 128, 68, 128, 69, 128],
	#[2, 96, 10, -109, 12, -82, 13, -105, 14, -86, 15, -111, 16, -90, 17, -112, 18, -40, 19, -12, 20, -55, 21, -27, 22, -62, 23, -21, 24, -54, 25, -26, 26, -57, 27, -4, 28, -34, 29, -7, 30, -37, 31, 96, 32, -102, 33, -59, 34, -51, 35, -96, 36, -116, 37, -97, 38, -91, 39, -114, 40, -74, 41, -106, 42, -92, 43, -115, 44, -95, 45, -117, 46, -98, 47, -72, 48, -99, 49, -75, 50, -101, 51, -78, 52, -103, 53, -81, 54, -107, 55, -100, 56, 96, 57, -88, 58, 96, 59, -118, 60, 96, 61, -104, 62, -83, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 151, 31, 151, 28, 151, 29, 151, 30, 151, 42, 151, 12, 151, 27, 151, 39, 151, 22, 151, 46, 151, 25, 151, 15, 151, 2, 151, 26, 151, 37, 151, 54, 151, 18, 151, 21, 151, 23, 151, 10, 151, 14, 151, 38, 151, 33, 151, 16, 151, 11, 151, 50, 151, 24, 151, 13, 151, 44, 151, 53, 151, 41, 151, 34, 151, 19, 151, 58, 151, 17, 151, 35, 151, 52, 151, 61, 151, 20, 151, 32, 151, 45, 151, 47, 151, 40, 151, 43, 151, 60, 151, 62, 151, 59, 151, 56, 151, 55, 151, 48, 151, 51, 151, 49, 151, 57, 151, 36, 151, 63, 151, 64, 151, 65, 151, 66, 151, 67, 151, 68, 151, 69, 151],
	#[65535, 15, 2, 15, 10, 15, 11, 15, 12, 15, 13, 15, 14, 15, 15, 15, 16, 15, 17, 15, 18, 15, 19, 15, 20, 15, 21, 15, 22, 15, 23, 15, 24, 15, 25, 15, 26, 15, 27, 15, 28, 15, 29, 15, 30, 15, 31, 15, 32, 15, 33, 15, 34, 15, 35, 15, 36, 15, 37, 15, 38, 15, 39, 15, 40, 15, 41, 15, 42, 15, 43, 15, 44, 15, 45, 15, 46, 15, 47, 15, 48, 15, 49, 15, 50, 15, 51, 15, 52, 15, 53, 15, 54, 15, 55, 15, 56, 15, 57, 15, 58, 15, 59, 15, 60, 15, 61, 15, 62, 15, 63, 15, 64, 15, 65, 15, 66, 15, 67, 15, 68, 15, 69, 15],
	#[65535, 17, 2, 17, 10, 17, 11, 17, 12, 17, 13, 17, 14, 17, 15, 17, 16, 17, 17, 17, 18, 17, 19, 17, 20, 17, 21, 17, 22, 17, 23, 17, 24, 17, 25, 17, 26, 17, 27, 17, 28, 17, 29, 17, 30, 17, 31, 17, 32, 17, 33, 17, 34, 17, 35, 17, 36, 17, 37, 17, 38, 17, 39, 17, 40, 17, 41, 17, 42, 17, 43, 17, 44, 17, 45, 17, 46, 17, 47, 17, 48, 17, 49, 17, 50, 17, 51, 17, 52, 17, 53, 17, 54, 17, 55, 17, 56, 17, 57, 17, 58, 17, 59, 17, 60, 17, 61, 17, 62, 17, 63, 17, 64, 17, 65, 17, 66, 17, 67, 17, 68, 17, 69, 17],
	#[65535, 127, 23, 127, 29, 127, 43, 127, 39, 127, 2, 127, 26, 127, 28, 127, 51, 127, 27, 127, 31, 127, 30, 127, 42, 127, 10, 127, 11, 127, 12, 127, 13, 127, 14, 127, 15, 127, 16, 127, 17, 127, 18, 127, 19, 127, 20, 127, 21, 127, 22, 127, 34, 127, 24, 127, 25, 127, 33, 127, 32, 127, 50, 127, 41, 127, 35, 127, 36, 127, 37, 127, 38, 127, 40, 127, 49, 127, 48, 127, 44, 127, 45, 127, 46, 127, 47, 127, 52, 127, 53, 127, 54, 127, 55, 127, 56, 127, 57, 127, 58, 127, 59, 127, 60, 127, 61, 127, 62, 127, 63, 127, 64, 127, 65, 127, 66, 127, 67, 127, 68, 127, 69, 127],
	#[65535, 78, 2, 78, 10, 78, 11, 78, 12, 78, 13, 78, 14, 78, 15, 78, 16, 78, 17, 78, 18, 78, 19, 78, 20, 78, 21, 78, 22, 78, 23, 78, 24, 78, 25, 78, 26, 78, 27, 78, 28, 78, 29, 78, 30, 78, 31, 78, 32, 78, 33, 78, 34, 78, 35, 78, 36, 78, 37, 78, 38, 78, 39, 78, 40, 78, 41, 78, 42, 78, 43, 78, 44, 78, 45, 78, 46, 78, 47, 78, 48, 78, 49, 78, 50, 78, 51, 78, 52, 78, 53, 78, 54, 78, 55, 78, 56, 78, 57, 78, 58, 78, 59, 78, 60, 78, 61, 78, 62, 78, 63, 78, 64, 78, 65, 78, 66, 78, 67, 78, 68, 78, 69, 78],
	#[65535, 133, 2, 133, 23, 133, 51, 133, 31, 133, 19, 133, 10, 133, 11, 133, 12, 133, 13, 133, 14, 133, 15, 133, 16, 133, 17, 133, 18, 133, 24, 133, 20, 133, 21, 133, 22, 133, 34, 133, 25, 133, 26, 133, 27, 133, 28, 133, 29, 133, 30, 133, 42, 133, 32, 133, 33, 133, 53, 133, 35, 133, 36, 133, 37, 133, 38, 133, 39, 133, 40, 133, 41, 133, 49, 133, 43, 133, 44, 133, 45, 133, 46, 133, 47, 133, 48, 133, 50, 133, 52, 133, 54, 133, 55, 133, 56, 133, 57, 133, 58, 133, 59, 133, 60, 133, 61, 133, 62, 133, 63, 133, 64, 133, 65, 133, 66, 133, 67, 133, 68, 133, 69, 133],
	#[65535, 76, 2, 76, 10, 76, 11, 76, 12, 76, 13, 76, 14, 76, 15, 76, 16, 76, 17, 76, 18, 76, 19, 76, 20, 76, 21, 76, 22, 76, 23, 76, 24, 76, 25, 76, 26, 76, 27, 76, 28, 76, 29, 76, 30, 76, 31, 76, 32, 76, 33, 76, 34, 76, 35, 76, 36, 76, 37, 76, 38, 76, 39, 76, 40, 76, 41, 76, 42, 76, 43, 76, 44, 76, 45, 76, 46, 76, 47, 76, 48, 76, 49, 76, 50, 76, 51, 76, 52, 76, 53, 76, 54, 76, 55, 76, 56, 76, 57, 76, 58, 76, 59, 76, 60, 76, 61, 76, 62, 76, 63, 76, 64, 76, 65, 76, 66, 76, 67, 76, 68, 76, 69, 76],
	#[65535, 135, 2, 135, 10, 135, 11, 135, 12, 135, 13, 135, 14, 135, 15, 135, 16, 135, 17, 135, 18, 135, 19, 135, 20, 135, 21, 135, 22, 135, 23, 135, 24, 135, 25, 135, 26, 135, 27, 135, 28, 135, 29, 135, 30, 135, 31, 135, 32, 135, 33, 135, 34, 135, 35, 135, 36, 135, 37, 135, 38, 135, 39, 135, 40, 135, 41, 135, 42, 135, 43, 135, 44, 135, 45, 135, 46, 135, 47, 135, 48, 135, 49, 135, 50, 135, 51, 135, 52, 135, 53, 135, 54, 135, 55, 135, 56, 135, 57, 135, 58, 135, 59, 135, 60, 135, 61, 135, 62, 135, 63, 135, 64, 135, 65, 135, 66, 135, 67, 135, 68, 135, 69, 135],
	#[15, -111, 23, -21, 14, -86, 13, -105, 10, -109, 16, -90, 27, -4, 17, -112, 29, -7, 38, -91, 19, -12, 28, -34, 12, -82, 18, -40, 55, -100, 33, -59, 22, -62, 34, -51, 21, -27, 20, -55, 26, -57, 30, -37, 35, -96, 39, -114, 36, -116, 42, -92, 46, -98, 24, -54, 25, -26, 43, -115, 54, -107, 32, -102, 59, -118, 37, -97, 48, -99, 62, -83, 40, -74, 41, -106, 61, -104, 60, 96, 44, -95, 45, -117, 47, -72, 49, -75, 50, -101, 51, -78, 52, -103, 53, -81, 57, -88, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[60, -377],
	#[40, -130, 46, 207, 56, 207, 58, 207],
	#[65535, 208, 40, 208, 46, 208, 56, 208, 58, 208],
	#[56, -135],
	#[65535, 210, 40, 210, 46, 210, 56, 210, 58, 210],
	#[46, -132, 56, 206],
	#[65535, 211, 40, 211, 46, 211, 56, 211, 58, 211],
	#[65535, 206, 58, 206],
	#[58, -129],
	#[65535, 199, 2, 199, 10, 199, 11, 199, 36, 199, 37, 199, 38, 199, 39, 199, 40, 199, 46, 199, 48, 199, 55, 199, 56, 199, 57, 199, 58, 199],
	#[65535, 202, 23, 202, 29, 202, 43, 202, 2, 202, 38, 202, 26, 202, 28, 202, 10, 202, 27, 202, 31, 202, 30, 202, 42, 202, 39, 202, 11, 202, 12, 202, 13, 202, 14, 202, 15, 202, 16, 202, 17, 202, 18, 202, 19, 202, 20, 202, 21, 202, 22, 202, 34, 202, 24, 202, 25, 202, 33, 202, 32, 202, 50, 202, 41, 202, 35, 202, 36, 202, 37, 202, 51, 202, 40, 202, 49, 202, 48, 202, 44, 202, 45, 202, 46, 202, 47, 202, 52, 202, 53, 202, 54, 202, 55, 202, 56, 202, 57, 202, 58, 202, 59, 202, 60, 202, 61, 202, 62, 202, 63, 202, 64, 202, 65, 202, 66, 202, 67, 202, 68, 202, 69, 202],
	#[12, -9, 13, -42, 41, -125, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56],
	#[65535, 209, 40, 209, 46, 209, 56, 209, 58, 209],
	#[12, -9, 13, -42, 41, -125, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56],
	#[56, -134],
	#[65535, 200, 2, 200, 10, 200, 11, 200, 12, 200, 13, 200, 14, 200, 15, 200, 16, 200, 17, 200, 18, 200, 19, 200, 20, 200, 21, 200, 22, 200, 23, 200, 24, 200, 25, 200, 26, 200, 27, 200, 28, 200, 29, 200, 30, 200, 31, 200, 32, 200, 33, 200, 34, 200, 35, 200, 36, 200, 37, 200, 38, 200, 39, 200, 40, 200, 41, 200, 42, 200, 43, 200, 44, 200, 45, 200, 46, 200, 47, 200, 48, 200, 49, 200, 50, 200, 51, 200, 52, 200, 53, 200, 54, 200, 55, 200, 56, 200, 57, 200, 58, 200, 59, 200, 60, 200, 61, 200, 62, 200, 63, 200, 64, 200, 65, 200, 66, 200, 67, 200, 68, 200, 69, 200],
	#[65535, 201, 47, 201, 2, 201, 38, 201, 23, 201, 55, 201, 46, 201, 31, 201, 26, 201, 30, 201, 42, 201, 10, 201, 11, 201, 12, 201, 13, 201, 14, 201, 15, 201, 16, 201, 17, 201, 18, 201, 19, 201, 20, 201, 21, 201, 22, 201, 34, 201, 24, 201, 25, 201, 33, 201, 27, 201, 28, 201, 29, 201, 43, 201, 54, 201, 32, 201, 59, 201, 35, 201, 36, 201, 37, 201, 51, 201, 39, 201, 40, 201, 41, 201, 61, 201, 60, 201, 44, 201, 45, 201, 56, 201, 58, 201, 48, 201, 49, 201, 50, 201, 52, 201, 53, 201, 57, 201, 62, 201, 63, 201, 64, 201, 65, 201, 66, 201, 67, 201, 68, 201, 69, 201],
	#[65535, 316, 10, 316, 12, 316, 13, 316, 14, 316, 15, 316, 16, 316, 17, 316, 18, 316, 19, 316, 20, 316, 21, 316, 22, 316, 23, 316, 24, 316, 25, 316, 26, 316, 27, 316, 28, 316, 29, 316, 30, 316, 31, 316, 32, 316, 33, 316, 35, 316, 36, 316, 37, 316, 38, 316, 39, 316, 40, 316, 41, 316, 42, 316, 43, 316, 44, 316, 45, 316, 46, 316, 47, 316, 48, 316, 49, 316, 50, 316, 51, 316, 52, 316, 53, 316, 55, 316, 56, 316, 57, 316, 58, 316, 59, 316, 60, 316, 61, 316, 62, 316, 63, 316, 64, 316, 65, 316, 66, 316, 67, 316, 68, 316, 69, 316],
	#[65535, 28, 10, 28, 12, 28, 13, 28, 14, 28, 15, 28, 16, 28, 17, 28, 18, 28, 19, 28, 20, 28, 21, 28, 22, 28, 23, 28, 24, 28, 25, 28, 26, 28, 27, 28, 28, 28, 29, 28, 30, 28, 31, 28, 32, 28, 33, 28, 34, 28, 35, 28, 36, 28, 37, 28, 38, 28, 39, 28, 40, 28, 41, 28, 42, 28, 43, 28, 44, 28, 45, 28, 46, 28, 47, 28, 48, 28, 49, 28, 50, 28, 51, 28, 52, 28, 53, 28, 54, 28, 55, 28, 56, 28, 57, 28, 58, 28, 59, 28, 60, 28, 61, 28, 62, 28, 63, 28, 64, 28, 65, 28, 66, 28, 67, 28, 68, 28, 69, 28],
	#[65535, 318, 55, 318, 47, 318, 43, 318, 31, 318, 51, 318, 45, 318, 39, 318, 22, 318, 46, 318, 13, 318, 15, 318, 44, 318, 28, 318, 30, 318, 21, 318, 23, 318, 10, 318, 19, 318, 14, 318, 38, 318, 16, 318, 29, 318, 50, 318, 18, 318, 17, 318, 25, 318, 33, 318, 12, 318, 37, 318, 58, 318, 26, 318, 35, 318, 52, 318, 61, 318, 20, 318, 42, 318, 27, 318, 24, 318, 40, 318, 56, 318, 60, 318, 62, 318, 59, 318, 53, 318, 32, 318, 48, 318, 41, 318, 49, 318, 57, 318, 36, 318, 63, 318, 64, 318, 65, 318, 66, 318, 67, 318, 68, 318, 69, 318],
	#[65535, 301, 56, 301, 58, 301, 60, 301],
	#[65535, 30, 10, 30, 12, 30, 13, 30, 14, 30, 15, 30, 16, 30, 17, 30, 18, 30, 19, 30, 20, 30, 21, 30, 22, 30, 23, 30, 24, 30, 25, 30, 26, 30, 27, 30, 28, 30, 29, 30, 30, 30, 31, 30, 32, 30, 33, 30, 34, 30, 35, 30, 36, 30, 37, 30, 38, 30, 39, 30, 40, 30, 41, 30, 42, 30, 43, 30, 44, 30, 45, 30, 46, 30, 47, 30, 48, 30, 49, 30, 50, 30, 51, 30, 52, 30, 53, 30, 54, 30, 55, 30, 56, 30, 57, 30, 58, 30, 59, 30, 60, 30, 61, 30, 62, 30, 63, 30, 64, 30, 65, 30, 66, 30, 67, 30, 68, 30, 69, 30],
	#[65535, 344, 49, 344],
	#[65535, 334, 10, 334, 12, 334, 13, 334, 14, 334, 15, 334, 16, 334, 17, 334, 18, 334, 19, 334, 20, 334, 21, 334, 22, 334, 23, 334, 24, 334, 25, 334, 26, 334, 27, 334, 28, 334, 29, 334, 30, 334, 31, 334, 32, 334, 33, 334, 35, 334, 36, 334, 37, 334, 38, 334, 39, 334, 40, 334, 41, 334, 42, 334, 43, 334, 44, 334, 45, 334, 46, 334, 47, 334, 48, 334, 49, 334, 50, 334, 51, 334, 52, 334, 53, 334, 55, 334, 56, 334, 57, 334, 58, 334, 59, 334, 60, 334, 61, 334, 62, 334, 63, 334, 64, 334, 65, 334, 66, 334, 67, 334, 68, 334, 69, 334],
	#[65535, 32, 46, 32, 55, 32, 15, 32, 47, 32, 43, 32, 14, 32, 51, 32, 45, 32, 39, 32, 42, 32, 59, 32, 13, 32, 35, 32, 53, 32, 44, 32, 28, 32, 37, 32, 50, 32, 21, 32, 23, 32, 61, 32, 52, 32, 36, 32, 38, 32, 62, 32, 31, 32, 60, 32, 22, 32, 10, 32, 12, 32, 49, 32, 19, 32, 16, 32, 48, 32, 30, 32, 54, 32, 20, 32, 57, 32, 27, 32, 17, 32, 34, 32, 29, 32, 58, 32, 26, 32, 24, 32, 18, 32, 25, 32, 33, 32, 41, 32, 40, 32, 56, 32, 32, 32, 63, 32, 64, 32, 65, 32, 66, 32, 67, 32, 68, 32, 69, 32],
	#[18, -172, 19, -145, 20, -178, 21, -149, 22, -183, 23, -150, 24, -186, 25, -153, 26, -187, 27, -137, 28, -164, 29, -140, 30, -168, 31, -143, 32, -173, 33, -189],
	#[65535, 20, 10, 20, 12, 20, 13, 20, 14, 20, 15, 20, 16, 20, 17, 20, 18, 20, 19, 20, 20, 20, 21, 20, 22, 20, 23, 20, 24, 20, 25, 20, 26, 20, 27, 20, 28, 20, 29, 20, 30, 20, 31, 20, 32, 20, 33, 20, 34, 20, 35, 20, 36, 20, 37, 20, 38, 20, 39, 20, 40, 20, 41, 20, 42, 20, 43, 20, 44, 20, 45, 20, 46, 20, 47, 20, 48, 20, 49, 20, 50, 20, 51, 20, 52, 20, 53, 20, 54, 20, 55, 20, 56, 20, 57, 20, 58, 20, 59, 20, 60, 20, 61, 20, 62, 20, 63, 20, 64, 20, 65, 20, 66, 20, 67, 20, 68, 20, 69, 20],
	#[65535, 339, 15, 339, 14, 339, 20, 339, 29, 339, 26, 339, 17, 339, 10, 339, 31, 339, 30, 339, 39, 339, 28, 339, 12, 339, 13, 339, 27, 339, 38, 339, 16, 339, 43, 339, 18, 339, 19, 339, 42, 339, 21, 339, 22, 339, 23, 339, 24, 339, 25, 339, 33, 339, 32, 339, 50, 339, 41, 339, 35, 339, 36, 339, 37, 339, 51, 339, 40, 339, 49, 339, 48, 339, 44, 339, 45, 339, 46, 339, 47, 339, 52, 339, 53, 339, 55, 339, 56, 339, 57, 339, 58, 339, 59, 339, 60, 339, 61, 339, 62, 339, 63, 339, 64, 339, 65, 339, 66, 339, 67, 339, 68, 339, 69, 339],
	#[65535, 315, 10, 315, 12, 315, 13, 315, 14, 315, 15, 315, 16, 315, 17, 315, 18, 315, 19, 315, 20, 315, 21, 315, 22, 315, 23, 315, 24, 315, 25, 315, 26, 315, 27, 315, 28, 315, 29, 315, 30, 315, 31, 315, 32, 315, 33, 315, 35, 315, 36, 315, 37, 315, 38, 315, 39, 315, 40, 315, 41, 315, 42, 315, 43, 315, 44, 315, 45, 315, 46, 315, 47, 315, 48, 315, 49, 315, 50, 315, 51, 315, 52, 315, 53, 315, 55, 315, 56, 315, 57, 315, 58, 315, 59, 315, 60, 315, 61, 315, 62, 315, 63, 315, 64, 315, 65, 315, 66, 315, 67, 315, 68, 315, 69, 315],
	#[65535, 312, 10, 312, 12, 312, 13, 312, 14, 312, 15, 312, 16, 312, 17, 312, 18, 312, 19, 312, 20, 312, 21, 312, 22, 312, 23, 312, 24, 312, 25, 312, 26, 312, 27, 312, 28, 312, 29, 312, 30, 312, 31, 312, 32, 312, 33, 312, 35, 312, 36, 312, 37, 312, 38, 312, 39, 312, 40, 312, 41, 312, 42, 312, 43, 312, 44, 312, 45, 312, 46, 312, 47, 312, 48, 312, 49, 312, 50, 312, 51, 312, 52, 312, 53, 312, 55, 312, 56, 312, 57, 312, 58, 312, 59, 312, 60, 312, 61, 312, 62, 312, 63, 312, 64, 312, 65, 312, 66, 312, 67, 312, 68, 312, 69, 312],
	#[65535, 22, 23, 22, 14, 22, 13, 22, 39, 22, 21, 22, 15, 22, 10, 22, 47, 22, 16, 22, 38, 22, 19, 22, 28, 22, 12, 22, 18, 22, 55, 22, 33, 22, 22, 22, 27, 22, 31, 22, 20, 22, 26, 22, 30, 22, 35, 22, 17, 22, 36, 22, 42, 22, 46, 22, 24, 22, 25, 22, 48, 22, 29, 22, 43, 22, 54, 22, 32, 22, 59, 22, 34, 22, 37, 22, 51, 22, 62, 22, 40, 22, 41, 22, 61, 22, 60, 22, 44, 22, 45, 22, 58, 22, 49, 22, 50, 22, 56, 22, 52, 22, 53, 22, 57, 22, 63, 22, 64, 22, 65, 22, 66, 22, 67, 22, 68, 22, 69, 22],
	#[65535, 24, 10, 24, 12, 24, 13, 24, 14, 24, 15, 24, 16, 24, 17, 24, 18, 24, 19, 24, 20, 24, 21, 24, 22, 24, 23, 24, 24, 24, 25, 24, 26, 24, 27, 24, 28, 24, 29, 24, 30, 24, 31, 24, 32, 24, 33, 24, 34, 24, 35, 24, 36, 24, 37, 24, 38, 24, 39, 24, 40, 24, 41, 24, 42, 24, 43, 24, 44, 24, 45, 24, 46, 24, 47, 24, 48, 24, 49, 24, 50, 24, 51, 24, 52, 24, 53, 24, 54, 24, 55, 24, 56, 24, 57, 24, 58, 24, 59, 24, 60, 24, 61, 24, 62, 24, 63, 24, 64, 24, 65, 24, 66, 24, 67, 24, 68, 24, 69, 24],
	#[10, -179, 12, -82, 13, -105, 14, -86, 15, -111, 16, -90, 17, -112, 18, -172, 19, -145, 20, -178, 21, -149, 22, -183, 23, -150, 24, -186, 25, -153, 26, -187, 27, -137, 28, -164, 29, -140, 30, -168, 31, -143, 32, -173, 33, -189, 35, -158, 36, -193, 37, -161, 38, -156, 39, -190, 40, -142, 41, -171, 42, -157, 43, -192, 44, -159, 45, -194, 46, -163, 47, -138, 48, -166, 49, 343, 50, -169, 51, -144, 52, -174, 53, -146, 55, -167, 57, -151, 58, 300, 59, -195, 61, -176, 62, -148, 63, -180, 64, -175, 65, -191, 66, -182, 67, -154, 68, -185, 69, -188],
	#[65535, 40, 15, 40, 55, 40, 59, 40, 14, 40, 51, 40, 45, 40, 39, 40, 46, 40, 10, 40, 13, 40, 35, 40, 44, 40, 47, 40, 50, 40, 42, 40, 61, 40, 52, 40, 36, 40, 38, 40, 62, 40, 16, 40, 31, 40, 27, 40, 22, 40, 28, 40, 43, 40, 49, 40, 37, 40, 58, 40, 21, 40, 30, 40, 54, 40, 20, 40, 57, 40, 48, 40, 17, 40, 29, 40, 60, 40, 19, 40, 23, 40, 12, 40, 18, 40, 25, 40, 33, 40, 41, 40, 40, 40, 34, 40, 32, 40, 26, 40, 24, 40, 56, 40, 53, 40, 63, 40, 64, 40, 65, 40, 66, 40, 67, 40, 68, 40, 69, 40],
	#[65535, 26, 10, 26, 12, 26, 13, 26, 14, 26, 15, 26, 16, 26, 17, 26, 18, 26, 19, 26, 20, 26, 21, 26, 22, 26, 23, 26, 24, 26, 25, 26, 26, 26, 27, 26, 28, 26, 29, 26, 30, 26, 31, 26, 32, 26, 33, 26, 34, 26, 35, 26, 36, 26, 37, 26, 38, 26, 39, 26, 40, 26, 41, 26, 42, 26, 43, 26, 44, 26, 45, 26, 46, 26, 47, 26, 48, 26, 49, 26, 50, 26, 51, 26, 52, 26, 53, 26, 54, 26, 55, 26, 56, 26, 57, 26, 58, 26, 59, 26, 60, 26, 61, 26, 62, 26, 63, 26, 64, 26, 65, 26, 66, 26, 67, 26, 68, 26, 69, 26],
	#[65535, 329, 10, 329, 12, 329, 13, 329, 14, 329, 15, 329, 16, 329, 17, 329, 18, 329, 19, 329, 20, 329, 21, 329, 22, 329, 23, 329, 24, 329, 25, 329, 26, 329, 27, 329, 28, 329, 29, 329, 30, 329, 31, 329, 32, 329, 33, 329, 35, 329, 36, 329, 37, 329, 38, 329, 39, 329, 40, 329, 41, 329, 42, 329, 43, 329, 44, 329, 45, 329, 46, 329, 47, 329, 48, 329, 49, 329, 50, 329, 51, 329, 52, 329, 53, 329, 55, 329, 56, 329, 57, 329, 58, 329, 59, 329, 60, 329, 61, 329, 62, 329, 63, 329, 64, 329, 65, 329, 66, 329, 67, 329, 68, 329, 69, 329],
	#[55, -167, 59, -195, 43, -192, 37, -161, 38, -156, 51, -144, 45, -194, 39, -190, 42, -157, 10, -179, 53, -146, 47, -138, 30, -168, 50, -169, 23, -150, 61, -176, 52, -174, 36, -193, 20, -178, 62, -148, 29, -140, 31, -143, 18, -172, 40, -142, 44, -159, 28, -164, 12, -82, 49, 343, 19, -145, 58, 304, 26, -187, 35, -158, 24, -186, 41, -171, 15, -111, 57, -151, 27, -137, 17, -112, 25, -153, 14, -86, 60, 304, 48, -166, 13, -105, 16, -90, 32, -173, 33, -189, 22, -183, 46, -163, 56, 304, 21, -149, 63, -180, 64, -175, 65, -191, 66, -182, 67, -154, 68, -185, 69, -188],
	#[65535, 72, 10, 72, 12, 72, 13, 72, 14, 72, 15, 72, 16, 72, 17, 72, 18, 72, 19, 72, 20, 72, 21, 72, 22, 72, 23, 72, 24, 72, 25, 72, 26, 72, 27, 72, 28, 72, 29, 72, 30, 72, 31, 72, 32, 72, 33, 72, 34, 72, 35, 72, 36, 72, 37, 72, 38, 72, 39, 72, 40, 72, 41, 72, 42, 72, 43, 72, 44, 72, 45, 72, 46, 72, 47, 72, 48, 72, 49, 72, 50, 72, 51, 72, 52, 72, 53, 72, 55, 72, 56, 72, 57, 72, 58, 72, 59, 72, 60, 72, 61, 72, 62, 72, 63, 72, 64, 72, 65, 72, 66, 72, 67, 72, 68, 72, 69, 72],
	#[65535, 326, 10, 326, 12, 326, 13, 326, 14, 326, 15, 326, 16, 326, 17, 326, 18, 326, 19, 326, 20, 326, 21, 326, 22, 326, 23, 326, 24, 326, 25, 326, 26, 326, 27, 326, 28, 326, 29, 326, 30, 326, 31, 326, 32, 326, 33, 326, 35, 326, 36, 326, 37, 326, 38, 326, 39, 326, 40, 326, 41, 326, 42, 326, 43, 326, 44, 326, 45, 326, 46, 326, 47, 326, 48, 326, 49, 326, 50, 326, 51, 326, 52, 326, 53, 326, 55, 326, 56, 326, 57, 326, 58, 326, 59, 326, 60, 326, 61, 326, 62, 326, 63, 326, 64, 326, 65, 326, 66, 326, 67, 326, 68, 326, 69, 326],
	#[65535, 314, 47, 314, 51, 314, 14, 314, 61, 314, 45, 314, 36, 314, 39, 314, 59, 314, 13, 314, 15, 314, 53, 314, 44, 314, 28, 314, 37, 314, 50, 314, 21, 314, 43, 314, 10, 314, 52, 314, 55, 314, 38, 314, 62, 314, 16, 314, 31, 314, 27, 314, 22, 314, 46, 314, 12, 314, 49, 314, 57, 314, 58, 314, 26, 314, 35, 314, 30, 314, 41, 314, 20, 314, 42, 314, 48, 314, 17, 314, 29, 314, 60, 314, 19, 314, 23, 314, 18, 314, 25, 314, 33, 314, 24, 314, 40, 314, 56, 314, 32, 314, 63, 314, 64, 314, 65, 314, 66, 314, 67, 314, 68, 314, 69, 314],
	#[65535, 324, 10, 324, 12, 324, 13, 324, 14, 324, 15, 324, 16, 324, 17, 324, 18, 324, 19, 324, 20, 324, 21, 324, 22, 324, 23, 324, 24, 324, 25, 324, 26, 324, 27, 324, 28, 324, 29, 324, 30, 324, 31, 324, 32, 324, 33, 324, 35, 324, 36, 324, 37, 324, 38, 324, 39, 324, 40, 324, 41, 324, 42, 324, 43, 324, 44, 324, 45, 324, 46, 324, 47, 324, 48, 324, 49, 324, 50, 324, 51, 324, 52, 324, 53, 324, 55, 324, 56, 324, 57, 324, 58, 324, 59, 324, 60, 324, 61, 324, 62, 324, 63, 324, 64, 324, 65, 324, 66, 324, 67, 324, 68, 324, 69, 324],
	#[65535, 309, 62, 309, 42, 309, 14, 309, 51, 309, 45, 309, 36, 309, 43, 309, 46, 309, 59, 309, 13, 309, 15, 309, 53, 309, 44, 309, 47, 309, 12, 309, 23, 309, 10, 309, 52, 309, 55, 309, 38, 309, 35, 309, 29, 309, 50, 309, 60, 309, 22, 309, 28, 309, 48, 309, 49, 309, 37, 309, 39, 309, 26, 309, 21, 309, 30, 309, 61, 309, 20, 309, 57, 309, 27, 309, 17, 309, 25, 309, 16, 309, 58, 309, 19, 309, 24, 309, 18, 309, 32, 309, 33, 309, 41, 309, 40, 309, 56, 309, 31, 309, 63, 309, 64, 309, 65, 309, 66, 309, 67, 309, 68, 309, 69, 309],
	#[65535, 70, 46, 70, 53, 70, 59, 70, 27, 70, 50, 70, 39, 70, 17, 70, 10, 70, 16, 70, 15, 70, 60, 70, 58, 70, 47, 70, 52, 70, 26, 70, 32, 70, 48, 70, 40, 70, 49, 70, 56, 70, 12, 70, 13, 70, 14, 70, 18, 70, 19, 70, 20, 70, 21, 70, 22, 70, 23, 70, 24, 70, 25, 70, 28, 70, 29, 70, 30, 70, 31, 70, 33, 70, 34, 70, 35, 70, 36, 70, 37, 70, 38, 70, 41, 70, 42, 70, 43, 70, 44, 70, 45, 70, 51, 70, 55, 70, 57, 70, 61, 70, 62, 70, 63, 70, 64, 70, 65, 70, 66, 70, 67, 70, 68, 70, 69, 70],
	#[65535, 323, 10, 323, 12, 323, 13, 323, 14, 323, 15, 323, 16, 323, 17, 323, 18, 323, 19, 323, 20, 323, 21, 323, 22, 323, 23, 323, 24, 323, 25, 323, 26, 323, 27, 323, 28, 323, 29, 323, 30, 323, 31, 323, 32, 323, 33, 323, 35, 323, 36, 323, 37, 323, 38, 323, 39, 323, 40, 323, 41, 323, 42, 323, 43, 323, 44, 323, 45, 323, 46, 323, 47, 323, 48, 323, 49, 323, 50, 323, 51, 323, 52, 323, 53, 323, 55, 323, 56, 323, 57, 323, 58, 323, 59, 323, 60, 323, 61, 323, 62, 323, 63, 323, 64, 323, 65, 323, 66, 323, 67, 323, 68, 323, 69, 323],
	#[65535, 317, 55, 317, 15, 317, 47, 317, 43, 317, 14, 317, 51, 317, 45, 317, 39, 317, 46, 317, 59, 317, 35, 317, 53, 317, 44, 317, 28, 317, 37, 317, 50, 317, 23, 317, 52, 317, 36, 317, 38, 317, 62, 317, 16, 317, 29, 317, 31, 317, 13, 317, 22, 317, 10, 317, 12, 317, 49, 317, 57, 317, 58, 317, 26, 317, 21, 317, 30, 317, 61, 317, 20, 317, 42, 317, 27, 317, 17, 317, 33, 317, 60, 317, 19, 317, 24, 317, 18, 317, 25, 317, 48, 317, 41, 317, 40, 317, 56, 317, 32, 317, 63, 317, 64, 317, 65, 317, 66, 317, 67, 317, 68, 317, 69, 317],
	#[65535, 29, 10, 29, 12, 29, 13, 29, 14, 29, 15, 29, 16, 29, 17, 29, 18, 29, 19, 29, 20, 29, 21, 29, 22, 29, 23, 29, 24, 29, 25, 29, 26, 29, 27, 29, 28, 29, 29, 29, 30, 29, 31, 29, 32, 29, 33, 29, 34, 29, 35, 29, 36, 29, 37, 29, 38, 29, 39, 29, 40, 29, 41, 29, 42, 29, 43, 29, 44, 29, 45, 29, 46, 29, 47, 29, 48, 29, 49, 29, 50, 29, 51, 29, 52, 29, 53, 29, 54, 29, 55, 29, 56, 29, 57, 29, 58, 29, 59, 29, 60, 29, 61, 29, 62, 29, 63, 29, 64, 29, 65, 29, 66, 29, 67, 29, 68, 29, 69, 29],
	#[65535, 335, 10, 335, 12, 335, 13, 335, 14, 335, 15, 335, 16, 335, 17, 335, 18, 335, 19, 335, 20, 335, 21, 335, 22, 335, 23, 335, 24, 335, 25, 335, 26, 335, 27, 335, 28, 335, 29, 335, 30, 335, 31, 335, 32, 335, 33, 335, 35, 335, 36, 335, 37, 335, 38, 335, 39, 335, 40, 335, 41, 335, 42, 335, 43, 335, 44, 335, 45, 335, 46, 335, 47, 335, 48, 335, 49, 335, 50, 335, 51, 335, 52, 335, 53, 335, 55, 335, 56, 335, 57, 335, 58, 335, 59, 335, 60, 335, 61, 335, 62, 335, 63, 335, 64, 335, 65, 335, 66, 335, 67, 335, 68, 335, 69, 335],
	#[65535, 319, 31, 319, 47, 319, 55, 319, 37, 319, 12, 319, 38, 319, 45, 319, 36, 319, 39, 319, 59, 319, 15, 319, 44, 319, 28, 319, 30, 319, 21, 319, 23, 319, 10, 319, 14, 319, 20, 319, 33, 319, 16, 319, 29, 319, 50, 319, 18, 319, 13, 319, 22, 319, 53, 319, 41, 319, 19, 319, 58, 319, 26, 319, 35, 319, 52, 319, 61, 319, 49, 319, 42, 319, 27, 319, 17, 319, 40, 319, 43, 319, 60, 319, 62, 319, 24, 319, 56, 319, 25, 319, 48, 319, 51, 319, 46, 319, 57, 319, 32, 319, 63, 319, 64, 319, 65, 319, 66, 319, 67, 319, 68, 319, 69, 319],
	#[15, -111, 10, -179, 27, -137, 21, -149, 16, -90, 17, -112, 18, -172, 20, -178, 23, -150, 24, -186, 25, -153, 26, -187, 30, -168, 31, -143, 32, -173, 33, -189, 41, -171, 35, -158, 36, -193, 37, -161, 38, -156, 40, -142, 42, -157, 43, -192, 44, -159, 45, -194, 46, -163, 48, -166, 49, 343, 50, -169, 51, -144, 52, -174, 53, -146, 55, -167, 56, 300, 57, -151, 59, -195, 61, -176, 62, -148, 63, -180, 64, -175, 65, -191, 66, -182, 67, -154, 68, -185, 69, -188, 47, -138, 14, -86, 29, -140, 13, -105, 22, -183, 28, -164, 12, -82, 19, -145, 39, -190],
	#[65535, 31, 10, 31, 12, 31, 13, 31, 14, 31, 15, 31, 16, 31, 17, 31, 18, 31, 19, 31, 20, 31, 21, 31, 22, 31, 23, 31, 24, 31, 25, 31, 26, 31, 27, 31, 28, 31, 29, 31, 30, 31, 31, 31, 32, 31, 33, 31, 34, 31, 35, 31, 36, 31, 37, 31, 38, 31, 39, 31, 40, 31, 41, 31, 42, 31, 43, 31, 44, 31, 45, 31, 46, 31, 47, 31, 48, 31, 49, 31, 50, 31, 51, 31, 52, 31, 53, 31, 54, 31, 55, 31, 56, 31, 57, 31, 58, 31, 59, 31, 60, 31, 61, 31, 62, 31, 63, 31, 64, 31, 65, 31, 66, 31, 67, 31, 68, 31, 69, 31],
	#[31, -143, 22, -183, 28, -164, 26, -187, 30, -168, 20, -178, 45, -265, 25, -153, 29, -140, 19, -145, 23, -150, 18, -172, 32, -173, 33, -189, 24, -186, 27, -137, 21, -149],
	#[15, -111, 14, -86, 19, -145, 10, -179, 20, -178, 39, -190, 12, -82, 13, -105, 16, -90, 17, -112, 18, -172, 21, -149, 22, -183, 23, -150, 24, -186, 25, -153, 26, -187, 27, -137, 28, -164, 29, -140, 30, -168, 31, -143, 32, -173, 33, -189, 35, -158, 36, -193, 37, -161, 38, -156, 40, -142, 41, -171, 42, -157, 43, -192, 44, -159, 45, -194, 46, -163, 47, -138, 48, -166, 49, 343, 50, -169, 51, -144, 52, -174, 53, -146, 55, -167, 56, 302, 57, -151, 58, 302, 59, -195, 60, 302, 61, -176, 62, -148, 63, -180, 64, -175, 65, -191, 66, -182, 67, -154, 68, -185, 69, -188],
	#[65535, 310, 15, 310, 53, 310, 14, 310, 50, 310, 13, 310, 22, 310, 10, 310, 43, 310, 39, 310, 35, 310, 52, 310, 51, 310, 47, 310, 29, 310, 19, 310, 28, 310, 12, 310, 16, 310, 55, 310, 27, 310, 21, 310, 20, 310, 17, 310, 18, 310, 23, 310, 24, 310, 25, 310, 26, 310, 30, 310, 31, 310, 32, 310, 33, 310, 41, 310, 49, 310, 36, 310, 37, 310, 38, 310, 40, 310, 42, 310, 48, 310, 44, 310, 45, 310, 46, 310, 56, 310, 57, 310, 58, 310, 59, 310, 60, 310, 61, 310, 62, 310, 63, 310, 64, 310, 65, 310, 66, 310, 67, 310, 68, 310, 69, 310],
	#[65535, 19, 15, 19, 39, 19, 20, 19, 14, 19, 19, 19, 51, 19, 42, 19, 10, 19, 12, 19, 13, 19, 24, 19, 53, 19, 16, 19, 17, 19, 18, 19, 21, 19, 22, 19, 23, 19, 25, 19, 26, 19, 27, 19, 28, 19, 29, 19, 30, 19, 31, 19, 32, 19, 33, 19, 34, 19, 35, 19, 36, 19, 37, 19, 38, 19, 40, 19, 41, 19, 49, 19, 43, 19, 44, 19, 45, 19, 46, 19, 47, 19, 48, 19, 50, 19, 52, 19, 54, 19, 55, 19, 56, 19, 57, 19, 58, 19, 59, 19, 60, 19, 61, 19, 62, 19, 63, 19, 64, 19, 65, 19, 66, 19, 67, 19, 68, 19, 69, 19],
	#[65535, 33, 43, 33, 55, 33, 14, 33, 51, 33, 45, 33, 50, 33, 39, 33, 22, 33, 46, 33, 13, 33, 15, 33, 53, 33, 44, 33, 47, 33, 37, 33, 54, 33, 48, 33, 23, 33, 10, 33, 52, 33, 36, 33, 38, 33, 35, 33, 41, 33, 29, 33, 31, 33, 27, 33, 25, 33, 28, 33, 12, 33, 49, 33, 19, 33, 16, 33, 21, 33, 30, 33, 18, 33, 20, 33, 42, 33, 17, 33, 34, 33, 33, 33, 32, 33, 26, 33, 24, 33, 40, 33, 56, 33, 57, 33, 58, 33, 59, 33, 60, 33, 61, 33, 62, 33, 63, 33, 64, 33, 65, 33, 66, 33, 67, 33, 68, 33, 69, 33],
	#[18, -172, 21, -149, 23, -150, 20, -178, 31, -143, 27, -137, 22, -183, 19, -145, 59, -255, 30, -168, 24, -186, 25, -153, 29, -140, 32, -173, 26, -187, 28, -164, 33, -189],
	#[65535, 328, 39, 328, 43, 328, 61, 328, 47, 328, 17, 328, 53, 328, 50, 328, 37, 328, 42, 328, 35, 328, 21, 328, 38, 328, 62, 328, 52, 328, 49, 328, 20, 328, 19, 328, 10, 328, 13, 328, 41, 328, 44, 328, 26, 328, 30, 328, 27, 328, 18, 328, 24, 328, 23, 328, 16, 328, 36, 328, 25, 328, 58, 328, 29, 328, 31, 328, 60, 328, 22, 328, 28, 328, 57, 328, 32, 328, 40, 328, 56, 328, 48, 328, 33, 328, 12, 328, 14, 328, 15, 328, 45, 328, 46, 328, 51, 328, 55, 328, 59, 328, 63, 328, 64, 328, 65, 328, 66, 328, 67, 328, 68, 328, 69, 328],
	#[65535, 311, 39, 311, 19, 311, 20, 311, 10, 311, 12, 311, 13, 311, 14, 311, 15, 311, 16, 311, 17, 311, 18, 311, 21, 311, 22, 311, 23, 311, 24, 311, 25, 311, 26, 311, 27, 311, 28, 311, 29, 311, 30, 311, 31, 311, 32, 311, 33, 311, 35, 311, 36, 311, 37, 311, 38, 311, 40, 311, 41, 311, 42, 311, 43, 311, 44, 311, 45, 311, 46, 311, 47, 311, 48, 311, 49, 311, 50, 311, 51, 311, 52, 311, 53, 311, 55, 311, 56, 311, 57, 311, 58, 311, 59, 311, 60, 311, 61, 311, 62, 311, 63, 311, 64, 311, 65, 311, 66, 311, 67, 311, 68, 311, 69, 311],
	#[65535, 18, 15, 18, 14, 18, 38, 18, 47, 18, 37, 18, 46, 18, 55, 18, 51, 18, 10, 18, 39, 18, 43, 18, 12, 18, 61, 18, 59, 18, 22, 18, 45, 18, 42, 18, 52, 18, 36, 18, 40, 18, 23, 18, 13, 18, 30, 18, 53, 18, 50, 18, 44, 18, 28, 18, 31, 18, 21, 18, 16, 18, 62, 18, 20, 18, 29, 18, 17, 18, 25, 18, 56, 18, 35, 18, 60, 18, 58, 18, 26, 18, 54, 18, 33, 18, 41, 18, 49, 18, 19, 18, 34, 18, 32, 18, 57, 18, 18, 18, 27, 18, 48, 18, 24, 18, 63, 18, 64, 18, 65, 18, 66, 18, 67, 18, 68, 18, 69, 18],
	#[65535, 21, 10, 21, 12, 21, 13, 21, 14, 21, 15, 21, 16, 21, 17, 21, 18, 21, 19, 21, 20, 21, 21, 21, 22, 21, 23, 21, 24, 21, 25, 21, 26, 21, 27, 21, 28, 21, 29, 21, 30, 21, 31, 21, 32, 21, 33, 21, 34, 21, 35, 21, 36, 21, 37, 21, 38, 21, 39, 21, 40, 21, 41, 21, 42, 21, 43, 21, 44, 21, 45, 21, 46, 21, 47, 21, 48, 21, 49, 21, 50, 21, 51, 21, 52, 21, 53, 21, 54, 21, 55, 21, 56, 21, 57, 21, 58, 21, 59, 21, 60, 21, 61, 21, 62, 21, 63, 21, 64, 21, 65, 21, 66, 21, 67, 21, 68, 21, 69, 21],
	#[65535, 333, 47, 333, 55, 333, 14, 333, 51, 333, 45, 333, 43, 333, 13, 333, 15, 333, 53, 333, 44, 333, 28, 333, 37, 333, 50, 333, 23, 333, 52, 333, 36, 333, 38, 333, 35, 333, 16, 333, 29, 333, 31, 333, 18, 333, 27, 333, 22, 333, 10, 333, 12, 333, 49, 333, 19, 333, 39, 333, 26, 333, 21, 333, 30, 333, 41, 333, 20, 333, 42, 333, 17, 333, 25, 333, 33, 333, 32, 333, 48, 333, 24, 333, 40, 333, 46, 333, 56, 333, 57, 333, 58, 333, 59, 333, 60, 333, 61, 333, 62, 333, 63, 333, 64, 333, 65, 333, 66, 333, 67, 333, 68, 333, 69, 333],
	#[15, 313, 54, -259, 14, 313, 55, 313, 52, 313, 31, 313, 28, 313, 12, 313, 39, 313, 49, 313, 13, 313, 10, 313, 62, 313, 47, 313, 59, 313, 58, 313, 35, 313, 50, 313, 46, 313, 61, 313, 27, 313, 16, 313, 45, 313, 43, 313, 30, 313, 26, 313, 36, 313, 20, 313, 18, 313, 53, 313, 51, 313, 41, 313, 37, 313, 57, 313, 44, 313, 21, 313, 25, 313, 19, 313, 24, 313, 22, 313, 42, 313, 17, 313, 29, 313, 33, 313, 23, 313, 32, 313, 40, 313, 60, 313, 38, 313, 48, 313, 56, 313, 63, 313, 64, 313, 65, 313, 66, 313, 67, 313, 68, 313, 69, 313],
	#[60, -376],
	#[43, -192, 14, -86, 44, -159, 23, -150, 22, -183, 27, -137, 26, -187, 41, -171, 18, -172, 42, -157, 10, -179, 12, -82, 13, -105, 24, -186, 15, -111, 16, -90, 17, -112, 25, -153, 19, -145, 20, -178, 21, -149, 32, -173, 28, -164, 29, -140, 30, -168, 31, -143, 33, -189, 35, -158, 36, -193, 37, -161, 38, -156, 39, -190, 40, -142, 45, -194, 46, -163, 47, -138, 48, -166, 49, 343, 50, -169, 51, -144, 52, -174, 53, -146, 55, -167, 57, -151, 58, 300, 59, -195, 61, -176, 62, -148, 63, -180, 64, -175, 65, -191, 66, -182, 67, -154, 68, -185, 69, -188],
	#[65535, 23, 15, 23, 55, 23, 59, 23, 14, 23, 51, 23, 45, 23, 50, 23, 39, 23, 46, 23, 10, 23, 13, 23, 35, 23, 44, 23, 47, 23, 27, 23, 21, 23, 42, 23, 61, 23, 52, 23, 36, 23, 38, 23, 62, 23, 31, 23, 18, 23, 60, 23, 22, 23, 28, 23, 43, 23, 49, 23, 37, 23, 16, 23, 26, 23, 48, 23, 30, 23, 54, 23, 20, 23, 57, 23, 17, 23, 29, 23, 58, 23, 19, 23, 23, 23, 12, 23, 33, 23, 41, 23, 40, 23, 34, 23, 32, 23, 25, 23, 24, 23, 56, 23, 53, 23, 63, 23, 64, 23, 65, 23, 66, 23, 67, 23, 68, 23, 69, 23],
	#[49, -199],
	#[65535, 330, 12, 330, 30, 330, 55, 330, 31, 330, 45, 330, 36, 330, 13, 330, 15, 330, 53, 330, 47, 330, 37, 330, 50, 330, 23, 330, 10, 330, 52, 330, 14, 330, 38, 330, 35, 330, 16, 330, 51, 330, 27, 330, 22, 330, 28, 330, 43, 330, 49, 330, 19, 330, 39, 330, 21, 330, 33, 330, 18, 330, 20, 330, 48, 330, 17, 330, 25, 330, 29, 330, 41, 330, 26, 330, 24, 330, 40, 330, 32, 330, 42, 330, 44, 330, 46, 330, 56, 330, 57, 330, 58, 330, 59, 330, 60, 330, 61, 330, 62, 330, 63, 330, 64, 330, 65, 330, 66, 330, 67, 330, 68, 330, 69, 330],
	#[65535, 25, 30, 25, 47, 25, 31, 25, 52, 25, 53, 25, 51, 25, 14, 25, 39, 25, 44, 25, 43, 25, 12, 25, 15, 25, 59, 25, 22, 25, 46, 25, 10, 25, 36, 25, 26, 25, 55, 25, 37, 25, 42, 25, 35, 25, 57, 25, 38, 25, 62, 25, 27, 25, 45, 25, 20, 25, 29, 25, 17, 25, 25, 25, 13, 25, 41, 25, 49, 25, 58, 25, 28, 25, 16, 25, 54, 25, 33, 25, 21, 25, 40, 25, 61, 25, 19, 25, 34, 25, 24, 25, 56, 25, 32, 25, 50, 25, 18, 25, 60, 25, 48, 25, 23, 25, 63, 25, 64, 25, 65, 25, 66, 25, 67, 25, 68, 25, 69, 25],
	#[65535, 27, 55, 27, 14, 27, 51, 27, 45, 27, 50, 27, 43, 27, 22, 27, 46, 27, 13, 27, 15, 27, 53, 27, 44, 27, 47, 27, 54, 27, 21, 27, 23, 27, 10, 27, 52, 27, 36, 27, 38, 27, 35, 27, 16, 27, 29, 27, 31, 27, 18, 27, 27, 27, 25, 27, 28, 27, 12, 27, 49, 27, 37, 27, 39, 27, 26, 27, 48, 27, 30, 27, 41, 27, 20, 27, 42, 27, 17, 27, 34, 27, 33, 27, 32, 27, 19, 27, 24, 27, 40, 27, 56, 27, 57, 27, 58, 27, 59, 27, 60, 27, 61, 27, 62, 27, 63, 27, 64, 27, 65, 27, 66, 27, 67, 27, 68, 27, 69, 27],
	#[10, -179, 59, -195, 45, -194, 52, -174, 30, -168, 55, -167, 60, 300, 44, -159, 35, -158, 38, -156, 51, -144, 41, -171, 36, -193, 43, -192, 46, -163, 40, -142, 13, -105, 15, -111, 53, -146, 57, -151, 47, -138, 12, -82, 50, -169, 18, -172, 21, -149, 23, -150, 61, -176, 14, -86, 20, -178, 62, -148, 29, -140, 31, -143, 27, -137, 22, -183, 28, -164, 49, 343, 37, -161, 39, -190, 26, -187, 33, -189, 42, -157, 17, -112, 25, -153, 16, -90, 32, -173, 19, -145, 24, -186, 48, -166, 63, -180, 64, -175, 65, -191, 66, -182, 67, -154, 68, -185, 69, -188],
	#[65535, 39, 42, 39, 14, 39, 59, 39, 13, 39, 55, 39, 52, 39, 50, 39, 43, 39, 49, 39, 61, 39, 44, 39, 45, 39, 39, 39, 51, 39, 29, 39, 15, 39, 38, 39, 53, 39, 47, 39, 57, 39, 22, 39, 46, 39, 28, 39, 12, 39, 17, 39, 48, 39, 30, 39, 16, 39, 36, 39, 35, 39, 18, 39, 40, 39, 33, 39, 10, 39, 37, 39, 62, 39, 21, 39, 32, 39, 19, 39, 25, 39, 20, 39, 27, 39, 58, 39, 54, 39, 60, 39, 31, 39, 24, 39, 41, 39, 56, 39, 34, 39, 26, 39, 23, 39, 63, 39, 64, 39, 65, 39, 66, 39, 67, 39, 68, 39, 69, 39],
	#[65535, 73, 43, 73, 18, 73, 28, 73, 25, 73, 45, 73, 36, 73, 20, 73, 22, 73, 46, 73, 59, 73, 13, 73, 35, 73, 53, 73, 51, 73, 50, 73, 33, 73, 23, 73, 52, 73, 14, 73, 38, 73, 62, 73, 41, 73, 29, 73, 31, 73, 24, 73, 60, 73, 44, 73, 10, 73, 12, 73, 49, 73, 19, 73, 39, 73, 21, 73, 30, 73, 61, 73, 15, 73, 42, 73, 27, 73, 47, 73, 34, 73, 16, 73, 58, 73, 26, 73, 57, 73, 17, 73, 55, 73, 48, 73, 37, 73, 40, 73, 56, 73, 32, 73, 63, 73, 64, 73, 65, 73, 66, 73, 67, 73, 68, 73, 69, 73],
	#[55, -167, 10, -179, 40, -142, 38, -156, 51, -144, 45, -194, 43, -192, 42, -157, 59, -195, 56, 300, 53, -146, 47, -138, 30, -168, 50, -169, 48, -166, 23, -150, 61, -176, 52, -174, 36, -193, 20, -178, 62, -148, 41, -171, 29, -140, 31, -143, 18, -172, 27, -137, 44, -159, 28, -164, 12, -82, 49, 343, 37, -161, 39, -190, 26, -187, 35, -158, 24, -186, 15, -111, 57, -151, 17, -112, 25, -153, 14, -86, 32, -173, 19, -145, 13, -105, 33, -189, 22, -183, 46, -163, 16, -90, 21, -149, 63, -180, 64, -175, 65, -191, 66, -182, 67, -154, 68, -185, 69, -188],
	#[65535, 327, 15, 327, 55, 327, 59, 327, 14, 327, 51, 327, 45, 327, 43, 327, 46, 327, 10, 327, 13, 327, 35, 327, 53, 327, 44, 327, 47, 327, 50, 327, 48, 327, 21, 327, 42, 327, 61, 327, 52, 327, 36, 327, 38, 327, 62, 327, 31, 327, 60, 327, 16, 327, 28, 327, 12, 327, 49, 327, 37, 327, 39, 327, 26, 327, 30, 327, 41, 327, 20, 327, 57, 327, 27, 327, 17, 327, 29, 327, 58, 327, 19, 327, 23, 327, 18, 327, 25, 327, 33, 327, 22, 327, 40, 327, 56, 327, 32, 327, 24, 327, 63, 327, 64, 327, 65, 327, 66, 327, 67, 327, 68, 327, 69, 327],
	#[65535, 71, 22, 71, 23, 71, 41, 71, 55, 71, 36, 71, 20, 71, 46, 71, 43, 71, 28, 71, 45, 71, 39, 71, 29, 71, 18, 71, 13, 71, 38, 71, 62, 71, 47, 71, 35, 71, 21, 71, 61, 71, 12, 71, 17, 71, 34, 71, 24, 71, 19, 71, 52, 71, 57, 71, 50, 71, 25, 71, 16, 71, 14, 71, 37, 71, 60, 71, 44, 71, 58, 71, 15, 71, 59, 71, 49, 71, 42, 71, 10, 71, 51, 71, 33, 71, 30, 71, 53, 71, 40, 71, 56, 71, 48, 71, 31, 71, 27, 71, 32, 71, 26, 71, 63, 71, 64, 71, 65, 71, 66, 71, 67, 71, 68, 71, 69, 71],
	#[65535, 325, 59, 325, 55, 325, 52, 325, 50, 325, 49, 325, 61, 325, 39, 325, 51, 325, 29, 325, 15, 325, 47, 325, 57, 325, 37, 325, 22, 325, 46, 325, 28, 325, 12, 325, 16, 325, 45, 325, 43, 325, 31, 325, 26, 325, 36, 325, 48, 325, 17, 325, 53, 325, 33, 325, 14, 325, 27, 325, 62, 325, 44, 325, 21, 325, 25, 325, 19, 325, 41, 325, 18, 325, 35, 325, 42, 325, 10, 325, 24, 325, 20, 325, 23, 325, 13, 325, 30, 325, 40, 325, 60, 325, 32, 325, 38, 325, 58, 325, 56, 325, 63, 325, 64, 325, 65, 325, 66, 325, 67, 325, 68, 325, 69, 325],
	#[52, -174, 53, -146, 43, -192, 45, -194, 10, -179, 30, -168, 55, -167, 60, 300, 42, -157, 35, -158, 40, -142, 38, -156, 51, -144, 41, -171, 39, -190, 46, -163, 59, -195, 15, -111, 48, -166, 44, -159, 47, -138, 12, -82, 50, -169, 18, -172, 21, -149, 23, -150, 61, -176, 36, -193, 20, -178, 62, -148, 29, -140, 31, -143, 27, -137, 22, -183, 28, -164, 49, 343, 37, -161, 16, -90, 26, -187, 24, -186, 57, -151, 17, -112, 25, -153, 14, -86, 32, -173, 19, -145, 13, -105, 33, -189, 63, -180, 64, -175, 65, -191, 66, -182, 67, -154, 68, -185, 69, -188],
	#[60, -375],
	#[56, -374],
	#[60, -373],
	#[55, -201, 45, -203, 31, -143, 22, -183, 28, -164, 26, -187, 30, -168, 18, -172, 20, -178, 24, -186, 25, -153, 29, -140, 32, -173, 19, -145, 23, -150, 33, -189, 41, -200, 27, -137, 21, -149, 63, -202],
	#[65535, 351, 54, 351, 59, 351, 13, 351, 55, 351, 52, 351, 50, 351, 49, 351, 61, 351, 30, 351, 45, 351, 39, 351, 51, 351, 29, 351, 15, 351, 43, 351, 47, 351, 57, 351, 37, 351, 22, 351, 46, 351, 28, 351, 12, 351, 16, 351, 17, 351, 48, 351, 31, 351, 26, 351, 36, 351, 35, 351, 18, 351, 53, 351, 33, 351, 14, 351, 27, 351, 62, 351, 44, 351, 21, 351, 25, 351, 19, 351, 41, 351, 42, 351, 10, 351, 24, 351, 20, 351, 23, 351, 32, 351, 40, 351, 60, 351, 38, 351, 58, 351, 56, 351, 63, 351, 64, 351, 65, 351, 66, 351, 67, 351, 68, 351, 69, 351],
	#[53, -14, 55, -36, 12, -9, 62, -16, 36, -66, 42, -30, 35, -31, 28, -34, 25, -26, 24, -54, 23, -21, 61, -49, 20, -55, 41, -43, 29, -7, 18, -40, 13, -42, 22, -62, 33, -59, 43, -63, 21, -27, 19, -12, 34, -51, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 350, 14, 350, 51, 350, 22, 350, 43, 350, 39, 350, 52, 350, 54, 350, 15, 350, 42, 350, 10, 350, 47, 350, 29, 350, 19, 350, 28, 350, 53, 350, 55, 350, 40, 350, 27, 350, 21, 350, 20, 350, 12, 350, 13, 350, 16, 350, 17, 350, 18, 350, 23, 350, 24, 350, 25, 350, 26, 350, 30, 350, 31, 350, 32, 350, 33, 350, 41, 350, 35, 350, 36, 350, 37, 350, 38, 350, 49, 350, 48, 350, 44, 350, 45, 350, 46, 350, 50, 350, 56, 350, 57, 350, 58, 350, 59, 350, 60, 350, 61, 350, 62, 350, 63, 350, 64, 350, 65, 350, 66, 350, 67, 350, 68, 350, 69, 350],
	#[65535, 352, 51, 352, 39, 352, 15, 352, 19, 352, 12, 352, 42, 352, 10, 352, 20, 352, 33, 352, 13, 352, 14, 352, 16, 352, 17, 352, 18, 352, 21, 352, 22, 352, 23, 352, 24, 352, 25, 352, 26, 352, 27, 352, 28, 352, 29, 352, 30, 352, 31, 352, 32, 352, 35, 352, 36, 352, 37, 352, 38, 352, 40, 352, 41, 352, 49, 352, 43, 352, 44, 352, 45, 352, 46, 352, 47, 352, 48, 352, 50, 352, 52, 352, 53, 352, 54, 352, 55, 352, 56, 352, 57, 352, 58, 352, 59, 352, 60, 352, 61, 352, 62, 352, 63, 352, 64, 352, 65, 352, 66, 352, 67, 352, 68, 352, 69, 352],
	#[65535, 349, 51, 349, 13, 349, 10, 349, 15, 349, 14, 349, 44, 349, 23, 349, 12, 349, 22, 349, 27, 349, 43, 349, 49, 349, 42, 349, 39, 349, 25, 349, 33, 349, 41, 349, 24, 349, 26, 349, 16, 349, 17, 349, 18, 349, 19, 349, 20, 349, 21, 349, 32, 349, 28, 349, 29, 349, 30, 349, 31, 349, 35, 349, 36, 349, 37, 349, 38, 349, 40, 349, 45, 349, 46, 349, 47, 349, 48, 349, 50, 349, 52, 349, 53, 349, 54, 349, 55, 349, 56, 349, 57, 349, 58, 349, 59, 349, 60, 349, 61, 349, 62, 349, 63, 349, 64, 349, 65, 349, 66, 349, 67, 349, 68, 349, 69, 349],
	#[15, 346, 55, 346, 14, 346, 51, 346, 45, 346, 13, 346, 35, 346, 53, 346, 47, 346, 37, 346, 50, 346, 48, 346, 43, 346, 10, 346, 52, 346, 36, 346, 38, 346, 33, 346, 16, 346, 31, 346, 18, 346, 17, 346, 22, 346, 28, 346, 12, 346, 39, 346, 26, 346, 21, 346, 30, 346, 54, -208, 20, 346, 42, 346, 27, 346, 24, 346, 25, 346, 29, 346, 41, 346, 19, 346, 23, 346, 40, 346, 32, 346, 49, 346, 44, 346, 46, 346, 56, 346, 57, 346, 58, 346, 59, 346, 60, 346, 61, 346, 62, 346, 63, 346, 64, 346, 65, 346, 66, 346, 67, 346, 68, 346, 69, 346],
	#[65535, 336, 22, 336, 20, 336, 30, 336, 31, 336, 14, 336, 38, 336, 32, 336, 26, 336, 39, 336, 29, 336, 46, 336, 15, 336, 28, 336, 37, 336, 25, 336, 33, 336, 21, 336, 23, 336, 10, 336, 19, 336, 55, 336, 45, 336, 35, 336, 41, 336, 51, 336, 18, 336, 27, 336, 44, 336, 53, 336, 12, 336, 24, 336, 16, 336, 17, 336, 48, 336, 52, 336, 36, 336, 49, 336, 42, 336, 47, 336, 40, 336, 43, 336, 13, 336, 50, 336, 56, 336, 57, 336, 58, 336, 59, 336, 60, 336, 61, 336, 62, 336, 63, 336, 64, 336, 65, 336, 66, 336, 67, 336, 68, 336, 69, 336],
	#[65535, 347, 39, 347, 23, 347, 22, 347, 27, 347, 31, 347, 26, 347, 19, 347, 18, 347, 35, 347, 10, 347, 12, 347, 13, 347, 14, 347, 15, 347, 16, 347, 17, 347, 25, 347, 24, 347, 20, 347, 21, 347, 44, 347, 28, 347, 29, 347, 30, 347, 42, 347, 32, 347, 33, 347, 49, 347, 36, 347, 37, 347, 38, 347, 40, 347, 41, 347, 43, 347, 45, 347, 46, 347, 47, 347, 48, 347, 50, 347, 51, 347, 52, 347, 53, 347, 55, 347, 56, 347, 57, 347, 58, 347, 59, 347, 60, 347, 61, 347, 62, 347, 63, 347, 64, 347, 65, 347, 66, 347, 67, 347, 68, 347, 69, 347],
	#[63, -209],
	#[65535, 348, 51, 348, 15, 348, 14, 348, 44, 348, 23, 348, 22, 348, 42, 348, 10, 348, 25, 348, 12, 348, 13, 348, 24, 348, 26, 348, 16, 348, 17, 348, 18, 348, 19, 348, 20, 348, 21, 348, 27, 348, 28, 348, 29, 348, 30, 348, 31, 348, 32, 348, 33, 348, 35, 348, 36, 348, 37, 348, 38, 348, 39, 348, 40, 348, 41, 348, 43, 348, 45, 348, 46, 348, 47, 348, 48, 348, 49, 348, 50, 348, 52, 348, 53, 348, 55, 348, 56, 348, 57, 348, 58, 348, 59, 348, 60, 348, 61, 348, 62, 348, 63, 348, 64, 348, 65, 348, 66, 348, 67, 348, 68, 348, 69, 348],
	#[56, -211],
	#[65535, 353, 43, 353, 10, 353, 12, 353, 39, 353, 51, 353, 14, 353, 22, 353, 31, 353, 50, 353, 30, 353, 42, 353, 29, 353, 33, 353, 13, 353, 27, 353, 15, 353, 16, 353, 17, 353, 18, 353, 19, 353, 20, 353, 21, 353, 32, 353, 23, 353, 24, 353, 25, 353, 26, 353, 28, 353, 41, 353, 35, 353, 36, 353, 37, 353, 38, 353, 40, 353, 49, 353, 48, 353, 44, 353, 45, 353, 46, 353, 47, 353, 52, 353, 53, 353, 54, 353, 55, 353, 56, 353, 57, 353, 58, 353, 59, 353, 60, 353, 61, 353, 62, 353, 63, 353, 64, 353, 65, 353, 66, 353, 67, 353, 68, 353, 69, 353],
	#[36, 232, 55, -214, 38, 232, 58, 232, 56, 232, 46, -213, 37, 232, 39, 232, 40, 232, 2, 232, 48, 232, 11, 232, 10, 232, 57, -215],
	#[20, -55, 21, -27, 34, -51, 33, -59, 53, -14],
	#[12, -9, 13, -42, 18, -40, 19, -12, 20, -55, 21, -27, 22, -62, 23, -21, 24, -54, 25, -26, 33, -59, 28, -34, 29, -7, 34, -51, 35, -31, 36, -66, 41, -221, 42, -30, 43, -63, 53, -14, 55, -36, 56, 212, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[55, -36, 13, -42, 23, -21, 12, -9, 22, -62, 61, -49, 25, -26, 34, -51, 41, -221, 24, -54, 18, -40, 19, -12, 20, -55, 21, -27, 28, -34, 29, -7, 43, -63, 42, -30, 33, -59, 35, -31, 36, -66, 53, -14, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[58, -372],
	#[65535, 182, 40, 182, 58, 182, 56, 182],
	#[65535, 179, 40, 179, 58, 179, 56, 179],
	#[39, -190, 56, 226, 38, -156, 40, 226, 37, -161, 36, -193, 58, 226],
	#[58, 178, 40, -228, 56, 178],
	#[61, -49, 43, -63, 62, -16, 28, -34, 34, -51, 56, 183, 53, -14, 42, -30, 40, 183, 21, -27, 12, -9, 19, -12, 41, -43, 22, -62, 35, -31, 20, -55, 23, -21, 55, -36, 18, -40, 24, -54, 58, 183, 36, -66, 25, -26, 13, -42, 33, -59, 29, -7, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 227, 37, 227, 39, 227, 36, 227, 38, 227, 40, 227, 56, 227, 58, 227],
	#[65535, 181, 40, 181, 58, 181, 56, 181],
	#[55, -36, 53, -14, 36, -66, 35, -31, 29, -7, 13, -42, 22, -62, 28, -34, 43, -63, 21, -27, 18, -40, 20, -55, 42, -30, 24, -54, 34, -51, 33, -59, 19, -12, 23, -21, 12, -9, 25, -26, 41, -43, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 225, 39, 225, 11, 225, 10, 225, 37, 225, 48, 225, 36, 225, 38, 225, 2, 225, 58, 225, 56, 225, 40, 225],
	#[56, -370],
	#[65535, 213, 56, 213],
	#[61, -49, 23, -21, 28, -34, 55, -36, 43, -63, 36, -66, 35, -31, 53, -14, 62, -16, 21, -27, 12, -9, 19, -12, 22, -62, 42, -30, 29, -7, 20, -55, 41, -221, 13, -42, 18, -40, 34, -51, 25, -26, 24, -54, 33, -59, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 180, 40, 180, 56, 180, 58, 180],
	#[55, -36, 62, -16, 53, -14, 28, -34, 25, -26, 18, -40, 43, -63, 61, -49, 36, -66, 20, -55, 35, -31, 13, -42, 22, -62, 42, -30, 24, -54, 34, -51, 29, -7, 19, -12, 23, -21, 12, -9, 33, -59, 41, -43, 21, -27, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 228, 39, 228, 56, 228, 38, 228, 40, 228, 37, 228, 36, 228, 58, 228],
	#[65535, 290, 23, 290, 28, 290, 45, 290, 54, 290, 53, 290, 44, 290, 43, 290, 15, 290, 22, 290, 47, 290, 27, 290, 30, 290, 55, 290, 50, 290, 18, 290, 37, 290, 42, 290, 12, 290, 14, 290, 38, 290, 26, 290, 52, 290, 36, 290, 39, 290, 29, 290, 46, 290, 59, 290, 13, 290, 35, 290, 57, 290, 51, 290, 25, 290, 48, 290, 24, 290, 49, 290, 61, 290, 19, 290, 20, 290, 33, 290, 16, 290, 32, 290, 31, 290, 17, 290, 10, 290, 41, 290, 40, 290, 21, 290, 62, 290, 34, 290, 63, 290, 64, 290, 65, 290, 66, 290, 67, 290, 68, 290, 69, 290],
	#[65535, 286, 28, 286, 27, 286, 45, 286, 50, 286, 22, 286, 46, 286, 15, 286, 53, 286, 44, 286, 47, 286, 30, 286, 54, 286, 21, 286, 23, 286, 19, 286, 14, 286, 38, 286, 26, 286, 29, 286, 51, 286, 18, 286, 13, 286, 42, 286, 32, 286, 43, 286, 49, 286, 37, 286, 39, 286, 17, 286, 52, 286, 36, 286, 20, 286, 57, 286, 48, 286, 24, 286, 25, 286, 16, 286, 41, 286, 62, 286, 59, 286, 12, 286, 35, 286, 55, 286, 33, 286, 10, 286, 40, 286, 34, 286, 31, 286, 61, 286, 63, 286, 64, 286, 65, 286, 66, 286, 67, 286, 68, 286, 69, 286],
	#[55, -248, 31, 256, 62, -83, 43, -253, 45, -254, 30, -37, 54, -107, 59, -255, 42, -243, 10, -249, 57, -241, 51, -78, 52, -103, 39, -114, 46, -98, 40, -234, 15, -111, 44, -247, 28, -34, 37, -97, 50, -101, 18, -40, 23, -21, 61, -104, 36, -116, 20, -55, 35, -96, 41, -106, 29, -7, 48, -99, 27, -4, 25, -26, 19, -12, 16, -90, 24, -54, 32, -102, 17, -112, 34, -51, 14, -86, 38, -91, 26, -57, 13, -105, 12, -82, 33, -59, 22, -62, 21, -27, 47, -72, 49, -75, 53, -81, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 289, 45, 289, 23, 289, 52, 289, 22, 289, 54, 289, 29, 289, 27, 289, 15, 289, 43, 289, 47, 289, 26, 289, 50, 289, 46, 289, 28, 289, 42, 289, 32, 289, 48, 289, 30, 289, 16, 289, 57, 289, 20, 289, 53, 289, 14, 289, 39, 289, 62, 289, 44, 289, 17, 289, 12, 289, 61, 289, 59, 289, 18, 289, 35, 289, 10, 289, 51, 289, 41, 289, 13, 289, 55, 289, 25, 289, 37, 289, 31, 289, 38, 289, 36, 289, 24, 289, 33, 289, 34, 289, 49, 289, 19, 289, 40, 289, 21, 289, 63, 289, 64, 289, 65, 289, 66, 289, 67, 289, 68, 289, 69, 289],
	#[31, -357],
	#[65535, 288, 23, 288, 52, 288, 22, 288, 46, 288, 43, 288, 42, 288, 30, 288, 29, 288, 27, 288, 15, 288, 38, 288, 47, 288, 37, 288, 26, 288, 50, 288, 51, 288, 28, 288, 55, 288, 45, 288, 48, 288, 31, 288, 54, 288, 35, 288, 18, 288, 53, 288, 16, 288, 14, 288, 39, 288, 57, 288, 44, 288, 21, 288, 12, 288, 61, 288, 59, 288, 49, 288, 32, 288, 10, 288, 36, 288, 20, 288, 41, 288, 13, 288, 17, 288, 19, 288, 34, 288, 62, 288, 25, 288, 24, 288, 33, 288, 40, 288, 63, 288, 64, 288, 65, 288, 66, 288, 67, 288, 68, 288, 69, 288],
	#[65535, 285, 14, 285, 52, 285, 22, 285, 46, 285, 42, 285, 12, 285, 29, 285, 27, 285, 15, 285, 43, 285, 47, 285, 26, 285, 23, 285, 51, 285, 28, 285, 55, 285, 45, 285, 48, 285, 30, 285, 54, 285, 32, 285, 50, 285, 18, 285, 53, 285, 16, 285, 39, 285, 62, 285, 44, 285, 17, 285, 25, 285, 61, 285, 59, 285, 49, 285, 35, 285, 10, 285, 36, 285, 41, 285, 13, 285, 57, 285, 19, 285, 37, 285, 31, 285, 38, 285, 24, 285, 40, 285, 20, 285, 33, 285, 34, 285, 21, 285, 63, 285, 64, 285, 65, 285, 66, 285, 67, 285, 68, 285, 69, 285],
	#[31, 275, 10, -340],
	#[55, -248, 31, 268, 59, -255, 54, -107, 39, -114, 38, -91, 62, -83, 47, -72, 57, -241, 46, -98, 61, -104, 42, -243, 45, -254, 43, -253, 30, -37, 52, -103, 36, -116, 35, -96, 53, -81, 51, -78, 10, -249, 37, -97, 34, -51, 44, -247, 21, -27, 26, -57, 15, -111, 41, -106, 40, -234, 32, -102, 48, -99, 24, -54, 20, -55, 27, -4, 49, -75, 50, -101, 19, -12, 12, -82, 14, -86, 16, -90, 17, -112, 25, -26, 33, -59, 13, -105, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56],
	#[55, -167, 53, -146, 31, -143, 59, -195, 30, -168, 39, -190, 51, -144, 52, -174, 38, -156, 62, -148, 47, -138, 57, -151, 58, 300, 46, -163, 61, -176, 12, -82, 10, -179, 45, -194, 43, -192, 29, -140, 26, -187, 36, -193, 50, -169, 40, -142, 33, -189, 14, -86, 37, -161, 44, -159, 28, -164, 15, -111, 41, -171, 22, -183, 35, -158, 42, -157, 48, -166, 25, -153, 20, -178, 27, -137, 49, 343, 24, -186, 18, -172, 32, -173, 16, -90, 17, -112, 13, -105, 23, -150, 19, -145, 21, -149, 63, -180, 64, -175, 65, -191, 66, -182, 67, -154, 68, -185, 69, -188],
	#[10, -340, 31, 275],
	#[65535, 292, 23, 292, 52, 292, 22, 292, 18, 292, 42, 292, 30, 292, 54, 292, 29, 292, 27, 292, 15, 292, 43, 292, 47, 292, 26, 292, 50, 292, 46, 292, 28, 292, 57, 292, 45, 292, 48, 292, 31, 292, 59, 292, 20, 292, 24, 292, 53, 292, 51, 292, 14, 292, 39, 292, 62, 292, 44, 292, 16, 292, 12, 292, 61, 292, 41, 292, 49, 292, 35, 292, 32, 292, 17, 292, 36, 292, 40, 292, 13, 292, 55, 292, 25, 292, 37, 292, 38, 292, 10, 292, 33, 292, 34, 292, 21, 292, 19, 292, 63, 292, 64, 292, 65, 292, 66, 292, 67, 292, 68, 292, 69, 292],
	#[65535, 291, 23, 291, 52, 291, 22, 291, 18, 291, 42, 291, 30, 291, 45, 291, 29, 291, 27, 291, 15, 291, 43, 291, 62, 291, 47, 291, 37, 291, 26, 291, 50, 291, 46, 291, 28, 291, 12, 291, 17, 291, 48, 291, 31, 291, 54, 291, 32, 291, 20, 291, 24, 291, 51, 291, 14, 291, 39, 291, 44, 291, 21, 291, 25, 291, 61, 291, 59, 291, 49, 291, 35, 291, 10, 291, 36, 291, 16, 291, 13, 291, 57, 291, 55, 291, 40, 291, 19, 291, 41, 291, 38, 291, 33, 291, 34, 291, 53, 291, 63, 291, 64, 291, 65, 291, 66, 291, 67, 291, 68, 291, 69, 291],
	#[65535, 257, 31, 257],
	#[65535, 298, 23, 298, 52, 298, 22, 298, 46, 298, 54, 298, 29, 298, 27, 298, 15, 298, 43, 298, 62, 298, 47, 298, 26, 298, 50, 298, 51, 298, 28, 298, 42, 298, 45, 298, 48, 298, 30, 298, 16, 298, 32, 298, 20, 298, 18, 298, 53, 298, 14, 298, 39, 298, 44, 298, 17, 298, 12, 298, 61, 298, 59, 298, 49, 298, 35, 298, 10, 298, 36, 298, 40, 298, 41, 298, 13, 298, 55, 298, 25, 298, 37, 298, 31, 298, 38, 298, 24, 298, 33, 298, 34, 298, 57, 298, 19, 298, 21, 298, 63, 298, 64, 298, 65, 298, 66, 298, 67, 298, 68, 298, 69, 298],
	#[65535, 294, 23, 294, 52, 294, 22, 294, 46, 294, 18, 294, 43, 294, 42, 294, 30, 294, 29, 294, 27, 294, 15, 294, 38, 294, 47, 294, 37, 294, 26, 294, 50, 294, 51, 294, 28, 294, 55, 294, 45, 294, 48, 294, 31, 294, 54, 294, 57, 294, 35, 294, 17, 294, 53, 294, 16, 294, 14, 294, 39, 294, 62, 294, 44, 294, 21, 294, 12, 294, 61, 294, 59, 294, 49, 294, 32, 294, 10, 294, 36, 294, 20, 294, 41, 294, 13, 294, 40, 294, 19, 294, 34, 294, 25, 294, 24, 294, 33, 294, 63, 294, 64, 294, 65, 294, 66, 294, 67, 294, 68, 294, 69, 294],
	#[31, -143, 49, 343, 61, -176, 30, -168, 39, -190, 51, -144, 52, -174, 38, -156, 47, -138, 57, -151, 37, -161, 56, 300, 50, -169, 46, -163, 28, -164, 55, -167, 10, -179, 45, -194, 43, -192, 19, -145, 59, -195, 36, -193, 35, -158, 53, -146, 33, -189, 14, -86, 27, -137, 62, -148, 44, -159, 21, -149, 12, -82, 15, -111, 41, -171, 22, -183, 42, -157, 48, -166, 29, -140, 20, -178, 23, -150, 32, -173, 40, -142, 18, -172, 16, -90, 17, -112, 25, -153, 24, -186, 13, -105, 26, -187, 63, -180, 64, -175, 65, -191, 66, -182, 67, -154, 68, -185, 69, -188],
	#[45, -254, 55, -248, 46, -98, 31, 256, 59, -255, 44, -247, 30, -37, 54, -107, 39, -114, 52, -103, 38, -91, 62, -83, 47, -72, 57, -241, 61, -104, 42, -243, 10, -249, 32, -102, 43, -253, 19, -12, 49, -75, 36, -116, 20, -55, 53, -81, 51, -78, 14, -86, 27, -4, 40, -234, 21, -27, 12, -82, 15, -111, 41, -106, 22, -62, 35, -96, 48, -99, 24, -54, 23, -21, 26, -57, 50, -101, 18, -40, 37, -97, 34, -51, 17, -112, 33, -59, 16, -90, 25, -26, 13, -105, 28, -34, 29, -7, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 258, 31, 258],
	#[65535, 287, 23, 287, 52, 287, 22, 287, 43, 287, 30, 287, 29, 287, 15, 287, 38, 287, 47, 287, 37, 287, 26, 287, 50, 287, 46, 287, 28, 287, 27, 287, 45, 287, 48, 287, 31, 287, 54, 287, 57, 287, 35, 287, 18, 287, 42, 287, 14, 287, 39, 287, 62, 287, 44, 287, 16, 287, 12, 287, 61, 287, 59, 287, 49, 287, 32, 287, 10, 287, 25, 287, 20, 287, 41, 287, 13, 287, 55, 287, 17, 287, 19, 287, 21, 287, 34, 287, 24, 287, 36, 287, 33, 287, 53, 287, 51, 287, 40, 287, 63, 287, 64, 287, 65, 287, 66, 287, 67, 287, 68, 287, 69, 287],
	#[39, -114, 55, -248, 45, -254, 31, 256, 54, -107, 38, -91, 43, -253, 47, -72, 10, -249, 30, -37, 53, -81, 50, -101, 32, -102, 42, -243, 35, -96, 40, -234, 34, -51, 51, -78, 52, -103, 36, -116, 20, -55, 22, -62, 46, -98, 25, -26, 13, -105, 15, -111, 49, -75, 44, -247, 28, -34, 37, -97, 27, -4, 33, -59, 21, -27, 23, -21, 19, -12, 14, -86, 41, -106, 29, -7, 48, -99, 24, -54, 16, -90, 12, -82, 17, -112, 18, -40, 26, -57, 57, -241, 59, -255, 61, -104, 62, -83, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 293, 15, 293, 47, 293, 45, 293, 28, 293, 23, 293, 52, 293, 22, 293, 46, 293, 18, 293, 43, 293, 42, 293, 30, 293, 54, 293, 29, 293, 27, 293, 24, 293, 38, 293, 62, 293, 19, 293, 48, 293, 37, 293, 26, 293, 50, 293, 51, 293, 61, 293, 55, 293, 17, 293, 34, 293, 31, 293, 59, 293, 32, 293, 20, 293, 53, 293, 16, 293, 14, 293, 39, 293, 44, 293, 21, 293, 12, 293, 33, 293, 49, 293, 35, 293, 10, 293, 25, 293, 41, 293, 13, 293, 57, 293, 40, 293, 36, 293, 63, 293, 64, 293, 65, 293, 66, 293, 67, 293, 68, 293, 69, 293],
	#[65535, 295, 54, 295, 45, 295, 29, 295, 22, 295, 46, 295, 52, 295, 30, 295, 15, 295, 47, 295, 50, 295, 28, 295, 14, 295, 42, 295, 23, 295, 13, 295, 55, 295, 53, 295, 26, 295, 27, 295, 37, 295, 18, 295, 43, 295, 49, 295, 61, 295, 44, 295, 35, 295, 12, 295, 39, 295, 51, 295, 32, 295, 48, 295, 24, 295, 38, 295, 62, 295, 34, 295, 59, 295, 16, 295, 21, 295, 57, 295, 10, 295, 17, 295, 19, 295, 36, 295, 20, 295, 25, 295, 40, 295, 33, 295, 41, 295, 31, 295, 63, 295, 64, 295, 65, 295, 66, 295, 67, 295, 68, 295, 69, 295],
	#[45, -194, 10, -179, 55, -167, 60, 300, 44, -159, 42, -157, 35, -158, 38, -156, 51, -144, 49, 343, 43, -192, 46, -163, 59, -195, 15, -111, 53, -146, 57, -151, 47, -138, 30, -168, 50, -169, 48, -166, 23, -150, 61, -176, 52, -174, 36, -193, 20, -178, 62, -148, 29, -140, 31, -143, 27, -137, 22, -183, 28, -164, 41, -171, 37, -161, 39, -190, 26, -187, 21, -149, 18, -172, 32, -173, 17, -112, 40, -142, 14, -86, 19, -145, 13, -105, 12, -82, 25, -153, 33, -189, 16, -90, 24, -186, 63, -180, 64, -175, 65, -191, 66, -182, 67, -154, 68, -185, 69, -188],
	#[60, -272],
	#[58, -258],
	#[65535, 307, 42, 307, 14, 307, 59, 307, 23, 307, 13, 307, 55, 307, 52, 307, 22, 307, 50, 307, 31, 307, 43, 307, 49, 307, 61, 307, 44, 307, 30, 307, 45, 307, 39, 307, 51, 307, 29, 307, 15, 307, 38, 307, 62, 307, 47, 307, 57, 307, 37, 307, 26, 307, 21, 307, 46, 307, 28, 307, 12, 307, 16, 307, 17, 307, 48, 307, 33, 307, 58, 307, 36, 307, 20, 307, 53, 307, 10, 307, 27, 307, 40, 307, 25, 307, 19, 307, 41, 307, 18, 307, 35, 307, 24, 307, 60, 307, 32, 307, 56, 307, 63, 307, 64, 307, 65, 307, 66, 307, 67, 307, 68, 307, 69, 307],
	#[65535, 345, 49, 345],
	#[59, -255],
	#[65535, 342, 31, 342, 30, 342, 20, 342, 18, 342, 32, 342, 21, 342, 19, 342, 60, 342, 28, 342, 58, 342, 27, 342, 38, 342, 62, 342, 47, 342, 37, 342, 35, 342, 23, 342, 46, 342, 61, 342, 55, 342, 10, 342, 45, 342, 24, 342, 29, 342, 26, 342, 36, 342, 17, 342, 53, 342, 56, 342, 14, 342, 39, 342, 57, 342, 44, 342, 43, 342, 12, 342, 15, 342, 59, 342, 22, 342, 42, 342, 52, 342, 51, 342, 40, 342, 41, 342, 49, 342, 50, 342, 25, 342, 16, 342, 48, 342, 33, 342, 13, 342, 63, 342, 64, 342, 65, 342, 66, 342, 67, 342, 68, 342, 69, 342],
	#[65535, 341, 44, 341, 55, 341, 53, 341, 50, 341, 36, 341, 42, 341, 43, 341, 45, 341, 57, 341, 15, 341, 13, 341, 47, 341, 37, 341, 52, 341, 23, 341, 21, 341, 59, 341, 61, 341, 35, 341, 38, 341, 62, 341, 51, 341, 31, 341, 28, 341, 12, 341, 49, 341, 22, 341, 46, 341, 17, 341, 25, 341, 39, 341, 10, 341, 48, 341, 30, 341, 40, 341, 58, 341, 33, 341, 41, 341, 18, 341, 14, 341, 29, 341, 16, 341, 26, 341, 20, 341, 19, 341, 60, 341, 56, 341, 32, 341, 24, 341, 27, 341, 63, 341, 64, 341, 65, 341, 66, 341, 67, 341, 68, 341, 69, 341],
	#[65535, 303, 58, 303, 56, 303, 60, 303],
	#[65535, 338, 31, 338, 36, 338, 22, 338, 18, 338, 21, 338, 30, 338, 26, 338, 29, 338, 27, 338, 38, 338, 20, 338, 19, 338, 37, 338, 35, 338, 23, 338, 46, 338, 28, 338, 55, 338, 10, 338, 32, 338, 25, 338, 41, 338, 52, 338, 13, 338, 24, 338, 53, 338, 42, 338, 14, 338, 39, 338, 17, 338, 12, 338, 15, 338, 50, 338, 40, 338, 47, 338, 51, 338, 48, 338, 16, 338, 44, 338, 33, 338, 45, 338, 43, 338, 49, 338, 56, 338, 57, 338, 58, 338, 59, 338, 60, 338, 61, 338, 62, 338, 63, 338, 64, 338, 65, 338, 66, 338, 67, 338, 68, 338, 69, 338],
	#[65535, 337, 31, 337, 20, 337, 30, 337, 19, 337, 55, 337, 58, 337, 22, 337, 18, 337, 32, 337, 21, 337, 24, 337, 60, 337, 28, 337, 12, 337, 26, 337, 29, 337, 27, 337, 15, 337, 38, 337, 62, 337, 47, 337, 37, 337, 35, 337, 56, 337, 23, 337, 46, 337, 61, 337, 42, 337, 10, 337, 45, 337, 25, 337, 33, 337, 41, 337, 59, 337, 36, 337, 13, 337, 17, 337, 53, 337, 14, 337, 39, 337, 57, 337, 44, 337, 43, 337, 48, 337, 50, 337, 52, 337, 51, 337, 40, 337, 16, 337, 49, 337, 63, 337, 64, 337, 65, 337, 66, 337, 67, 337, 68, 337, 69, 337],
	#[56, -267],
	#[65535, 320, 47, 320, 45, 320, 53, 320, 43, 320, 15, 320, 46, 320, 51, 320, 49, 320, 55, 320, 50, 320, 42, 320, 12, 320, 14, 320, 38, 320, 17, 320, 52, 320, 39, 320, 19, 320, 10, 320, 13, 320, 35, 320, 44, 320, 28, 320, 37, 320, 48, 320, 40, 320, 36, 320, 25, 320, 33, 320, 16, 320, 31, 320, 18, 320, 27, 320, 22, 320, 41, 320, 24, 320, 26, 320, 21, 320, 30, 320, 20, 320, 29, 320, 32, 320, 23, 320, 56, 320, 57, 320, 58, 320, 59, 320, 60, 320, 61, 320, 62, 320, 63, 320, 64, 320, 65, 320, 66, 320, 67, 320, 68, 320, 69, 320],
	#[65535, 305, 58, 305, 60, 305, 56, 305],
	#[58, -270],
	#[65535, 321, 15, 321, 44, 321, 42, 321, 55, 321, 53, 321, 50, 321, 57, 321, 43, 321, 45, 321, 46, 321, 35, 321, 28, 321, 13, 321, 47, 321, 49, 321, 37, 321, 52, 321, 36, 321, 25, 321, 59, 321, 61, 321, 18, 321, 38, 321, 62, 321, 10, 321, 31, 321, 27, 321, 12, 321, 40, 321, 22, 321, 17, 321, 39, 321, 20, 321, 48, 321, 51, 321, 58, 321, 41, 321, 14, 321, 29, 321, 23, 321, 16, 321, 26, 321, 21, 321, 19, 321, 60, 321, 56, 321, 30, 321, 24, 321, 33, 321, 32, 321, 63, 321, 64, 321, 65, 321, 66, 321, 67, 321, 68, 321, 69, 321],
	#[65535, 340, 31, 340, 20, 340, 30, 340, 15, 340, 19, 340, 22, 340, 46, 340, 18, 340, 32, 340, 21, 340, 24, 340, 60, 340, 28, 340, 58, 340, 26, 340, 29, 340, 27, 340, 14, 340, 38, 340, 62, 340, 47, 340, 37, 340, 35, 340, 56, 340, 23, 340, 51, 340, 61, 340, 55, 340, 10, 340, 45, 340, 43, 340, 33, 340, 59, 340, 36, 340, 13, 340, 17, 340, 53, 340, 42, 340, 39, 340, 40, 340, 12, 340, 48, 340, 50, 340, 49, 340, 52, 340, 41, 340, 57, 340, 25, 340, 44, 340, 16, 340, 63, 340, 64, 340, 65, 340, 66, 340, 67, 340, 68, 340, 69, 340],
	#[65535, 299, 23, 299, 50, 299, 18, 299, 28, 299, 12, 299, 14, 299, 21, 299, 42, 299, 43, 299, 45, 299, 22, 299, 29, 299, 44, 299, 15, 299, 13, 299, 47, 299, 27, 299, 30, 299, 54, 299, 52, 299, 36, 299, 20, 299, 55, 299, 35, 299, 24, 299, 62, 299, 51, 299, 31, 299, 34, 299, 49, 299, 16, 299, 46, 299, 53, 299, 59, 299, 39, 299, 41, 299, 10, 299, 48, 299, 61, 299, 26, 299, 19, 299, 38, 299, 33, 299, 57, 299, 37, 299, 40, 299, 56, 299, 32, 299, 17, 299, 25, 299, 58, 299, 60, 299, 63, 299, 64, 299, 65, 299, 66, 299, 67, 299, 68, 299, 69, 299],
	#[65535, 272, 31, 272],
	#[65535, 274, 31, 274],
	#[45, -254, 55, -248, 31, 256, 59, -255, 44, -247, 30, -37, 54, -107, 39, -114, 51, -78, 52, -103, 38, -91, 62, -83, 47, -72, 57, -241, 46, -98, 61, -104, 42, -243, 10, -249, 32, -102, 43, -253, 41, -106, 49, -75, 36, -116, 20, -55, 25, -26, 14, -86, 37, -97, 34, -51, 40, -234, 28, -34, 26, -57, 15, -111, 50, -101, 22, -62, 35, -96, 48, -99, 29, -7, 27, -4, 13, -105, 53, -81, 18, -40, 33, -59, 12, -82, 16, -90, 17, -112, 24, -54, 23, -21, 19, -12, 21, -27, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[62, -83, 52, -103, 55, -100, 54, -107, 53, -81, 31, 96, 38, -91, 36, -116, 61, -104, 45, -117, 41, -106, 57, -88, 47, -72, 27, -4, 30, -37, 37, -97, 43, -115, 23, -21, 59, -118, 50, -101, 14, -86, 42, -92, 44, -95, 29, -7, 12, -82, 40, -74, 22, -62, 46, -98, 19, -12, 25, -26, 39, -114, 35, -96, 10, -109, 48, -99, 34, -51, 15, -111, 32, -102, 28, -34, 33, -59, 49, -75, 51, -78, 26, -57, 20, -55, 21, -27, 17, -112, 16, -90, 18, -40, 13, -105, 24, -54, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 155, 2, 155, 10, 155, 31, 155, 60, 155, 56, 155, 58, 155, 11, 155],
	#[62, -83, 30, -37, 61, -104, 47, -72, 23, -21, 41, -106, 57, -88, 46, -98, 31, 96, 43, -115, 19, -12, 59, -118, 28, -34, 54, -107, 39, -114, 51, -78, 50, -101, 32, -102, 52, -103, 38, -91, 53, -81, 34, -51, 48, -99, 35, -96, 21, -27, 44, -95, 55, -100, 10, -109, 45, -117, 29, -7, 49, -75, 36, -116, 13, -105, 25, -26, 42, -92, 14, -86, 27, -4, 40, -74, 26, -57, 15, -111, 24, -54, 22, -62, 17, -112, 20, -55, 18, -40, 37, -97, 12, -82, 16, -90, 33, -59, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[10, 157, 11, 157, 31, 157, 40, -280, 2, 157, 56, 157, 58, 157, 60, 157],
	#[29, -7, 18, -40, 21, -27, 20, -55, 22, -62, 33, -59, 53, -14, 28, -34, 23, -21, 34, -51],
	#[65535, 158, 10, 158, 11, 158, 31, 158, 2, 158, 56, 158, 58, 158, 60, 158],
	#[31, -324],
	#[65535, 114, 31, 114, 2, 114, 58, 114, 56, 114, 60, 114],
	#[31, -357],
	#[65535, 168, 20, 168, 25, 168, 24, 168, 23, 168, 34, 168, 29, 168, 22, 168, 21, 168],
	#[23, 169, 20, 169, 29, 169, 22, 169, 21, 169, 34, 169, 24, 169, 25, 169, 33, -288],
	#[21, -293, 23, -290, 20, -291, 24, -292, 25, -299, 29, -294, 22, -297, 34, -298],
	#[65535, 166, 22, 166, 33, 166, 24, 166, 21, 166, 25, 166, 20, 166, 34, 166, 23, 166, 29, 166],
	#[58, 100, 60, 100, 11, 100, 52, -103, 46, -98, 31, 100, 41, -106, 10, 100, 59, -118, 54, -107, 39, -114, 15, -111, 38, -91, 62, -83, 47, -72, 48, -99, 37, -97, 35, -96, 50, -101, 44, -95, 61, -104, 55, -100, 56, 100, 45, -117, 43, -115, 2, 100, 19, -12, 49, -75, 36, -116, 13, -105, 53, -81, 51, -78, 14, -86, 57, -88, 40, -304, 21, -27, 32, -102, 24, -54, 22, -62, 42, -92, 17, -112, 29, -7, 34, -51, 23, -21, 18, -40, 28, -34, 12, -82, 25, -26, 33, -59, 16, -90, 20, -55, 63, -110, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 59, 34, 59, 62, 59, 61, 59, 20, 59, 55, 59, 37, 59, 10, 59, 21, 59, 38, 59, 17, 59, 40, 59, 36, 59, 39, 59, 46, 59, 59, 59, 15, 59, 53, 59, 51, 59, 12, 59, 50, 59, 33, 59, 41, 59, 23, 59, 57, 59, 52, 59, 14, 59, 25, 59, 35, 59, 16, 59, 31, 59, 13, 59, 22, 59, 28, 59, 43, 59, 24, 59, 19, 59, 32, 59, 48, 59, 30, 59, 54, 59, 49, 59, 45, 59, 29, 59, 44, 59, 18, 59, 42, 59, 27, 59, 26, 59, 47, 59, 63, 59, 64, 59, 65, 59, 66, 59, 67, 59, 68, 59, 69, 59],
	#[65535, 57, 20, 57, 15, 57, 10, 57, 14, 57, 12, 57, 25, 57, 37, 57, 34, 57, 36, 57, 17, 57, 26, 57, 19, 57, 18, 57, 35, 57, 44, 57, 16, 57, 32, 57, 13, 57, 27, 57, 42, 57, 21, 57, 22, 57, 23, 57, 24, 57, 28, 57, 29, 57, 30, 57, 31, 57, 33, 57, 38, 57, 39, 57, 40, 57, 41, 57, 43, 57, 45, 57, 46, 57, 47, 57, 48, 57, 49, 57, 50, 57, 51, 57, 52, 57, 53, 57, 54, 57, 55, 57, 57, 57, 59, 57, 61, 57, 62, 57, 63, 57, 64, 57, 65, 57, 66, 57, 67, 57, 68, 57, 69, 57],
	#[65535, 62, 45, 62, 28, 62, 22, 62, 47, 62, 23, 62, 50, 62, 44, 62, 42, 62, 14, 62, 38, 62, 61, 62, 52, 62, 36, 62, 39, 62, 29, 62, 46, 62, 13, 62, 15, 62, 53, 62, 57, 62, 51, 62, 54, 62, 18, 62, 49, 62, 19, 62, 34, 62, 20, 62, 16, 62, 32, 62, 31, 62, 24, 62, 60, 62, 25, 62, 33, 62, 43, 62, 37, 62, 17, 62, 21, 62, 41, 62, 48, 62, 2, 62, 62, 62, 59, 62, 12, 62, 35, 62, 55, 62, 11, 62, 10, 62, 40, 62, 56, 62, 58, 62, 63, 62, 65, 62, 66, 62, 67, 62, 68, 62, 69, 62],
	#[65535, 60, 2, 60, 44, 60, 13, 60, 12, 60, 42, 60, 10, 60, 11, 60, 14, 60, 15, 60, 16, 60, 17, 60, 18, 60, 19, 60, 20, 60, 21, 60, 22, 60, 23, 60, 24, 60, 25, 60, 32, 60, 28, 60, 29, 60, 31, 60, 33, 60, 34, 60, 35, 60, 36, 60, 37, 60, 38, 60, 39, 60, 40, 60, 41, 60, 49, 60, 43, 60, 45, 60, 46, 60, 47, 60, 48, 60, 50, 60, 51, 60, 52, 60, 53, 60, 54, 60, 55, 60, 56, 60, 57, 60, 58, 60, 59, 60, 60, 60, 61, 60, 62, 60, 63, 60, 65, 60, 66, 60, 67, 60, 68, 60, 69, 60],
	#[65535, 42, 30, 42, 31, 42, 22, 42, 28, 42, 26, 42, 21, 42, 18, 42, 20, 42, 32, 42, 25, 42, 29, 42, 19, 42, 23, 42, 33, 42, 27, 42, 24, 42],
	#[31, -143, 20, -178, 30, -168, 21, -149, 18, -172, 25, -153, 33, -189, 32, -173, 24, -186, 28, -164, 23, -150, 19, -145, 29, -140, 27, -137, 22, -183, 26, -187],
	#[39, -114, 47, -72, 37, -97, 52, -103, 51, -78, 10, -109, 14, -86, 13, -105, 53, -81, 46, -98, 27, -4, 31, 96, 26, -57, 49, -75, 48, -99, 35, -96, 16, -90, 12, -82, 15, -111, 17, -112, 18, -40, 19, -12, 20, -55, 21, -27, 22, -62, 23, -21, 24, -54, 25, -26, 28, -34, 29, -7, 30, -37, 54, -107, 32, -102, 33, -59, 34, -51, 36, -116, 38, -91, 40, -74, 41, -106, 42, -92, 43, -115, 44, -95, 45, -117, 50, -101, 55, -100, 57, -88, 59, -118, 61, -104, 62, -83, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 61, 22, 61, 28, 61, 23, 61, 29, 61, 44, 61, 46, 61, 39, 61, 21, 61, 36, 61, 15, 61, 42, 61, 48, 61, 47, 61, 14, 61, 32, 61, 19, 61, 13, 61, 12, 61, 18, 61, 55, 61, 11, 61, 51, 61, 49, 61, 16, 61, 2, 61, 17, 61, 61, 61, 59, 61, 54, 61, 35, 61, 10, 61, 20, 61, 34, 61, 24, 61, 25, 61, 43, 61, 31, 61, 33, 61, 37, 61, 38, 61, 62, 61, 40, 61, 41, 61, 60, 61, 45, 61, 58, 61, 50, 61, 56, 61, 52, 61, 53, 61, 57, 61, 63, 61, 65, 61, 66, 61, 67, 61, 68, 61, 69, 61],
	#[65535, 41, 22, 41, 28, 41, 26, 41, 25, 41, 19, 41, 23, 41, 18, 41, 27, 41, 21, 41, 20, 41, 24, 41, 29, 41, 30, 41, 31, 41, 32, 41, 33, 41],
	#[65535, 58, 15, 58, 10, 58, 14, 58, 38, 58, 13, 58, 37, 58, 34, 58, 36, 58, 20, 58, 25, 58, 19, 58, 18, 58, 35, 58, 17, 58, 16, 58, 12, 58, 27, 58, 42, 58, 21, 58, 22, 58, 23, 58, 24, 58, 26, 58, 28, 58, 29, 58, 30, 58, 31, 58, 32, 58, 33, 58, 51, 58, 39, 58, 40, 58, 41, 58, 43, 58, 44, 58, 45, 58, 46, 58, 47, 58, 48, 58, 49, 58, 50, 58, 52, 58, 53, 58, 54, 58, 55, 58, 57, 58, 59, 58, 61, 58, 62, 58, 63, 58, 64, 58, 65, 58, 66, 58, 67, 58, 68, 58, 69, 58],
	#[31, -324],
	#[56, -323],
	#[65535, 101, 11, 101, 31, 101, 10, 101, 2, 101, 56, 101, 58, 101, 60, 101],
	#[55, -100, 54, -107, 47, -72, 2, 100, 53, -81, 31, 100, 11, 100, 51, -78, 39, -114, 22, -62, 46, -98, 49, -75, 44, -95, 40, -304, 37, -97, 50, -101, 23, -21, 10, 100, 52, -103, 36, -116, 20, -55, 35, -96, 41, -106, 29, -7, 48, -99, 18, -40, 13, -105, 28, -34, 43, -115, 24, -54, 19, -12, 16, -90, 32, -102, 21, -27, 33, -59, 15, -111, 42, -92, 45, -117, 17, -112, 34, -51, 14, -86, 38, -91, 12, -82, 25, -26, 56, 100, 57, -88, 58, 100, 59, -118, 60, 100, 61, -104, 62, -83, 63, -110, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[54, -107, 36, -116, 62, -83, 60, 100, 2, 100, 58, 100, 56, 100, 46, -98, 50, -101, 39, -114, 37, -97, 11, 100, 15, -111, 61, -104, 47, -72, 44, -95, 14, -86, 38, -91, 23, -21, 41, -106, 55, -100, 52, -103, 22, -62, 25, -26, 31, 100, 43, -115, 42, -92, 19, -12, 59, -118, 34, -51, 28, -34, 51, -78, 32, -102, 13, -105, 33, -59, 53, -81, 48, -99, 24, -54, 35, -96, 21, -27, 18, -40, 40, -304, 10, 100, 45, -117, 29, -7, 49, -75, 17, -112, 12, -82, 16, -90, 20, -55, 57, -88, 63, -110, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[31, 118, 11, 118, 43, -115, 37, -97, 35, -96, 38, -91, 51, -78, 45, -117, 36, -116, 39, -114, 15, -111, 2, 118, 44, -95, 47, -72, 12, -82, 25, -26, 48, -99, 24, -54, 40, -304, 10, 118, 32, -102, 14, -86, 20, -55, 41, -106, 49, -75, 13, -105, 16, -90, 46, -98, 34, -51, 19, -12, 17, -112, 21, -27, 33, -59, 42, -92, 50, -101, 52, -103, 53, -81, 54, -107, 55, -100, 56, 118, 57, -88, 58, 118, 59, -118, 60, 118, 61, -104, 62, -83, 63, -110, 65, -61, 66, -52, 67, -23, 68, -56],
	#[65535, 102, 31, 102, 11, 102, 10, 102, 2, 102, 56, 102, 58, 102, 60, 102],
	#[65535, 154, 2, 154, 31, 154, 60, 154, 10, 154, 58, 154, 11, 154, 56, 154],
	#[58, -318],
	#[65535, 111, 31, 111, 2, 111, 56, 111, 58, 111, 60, 111],
	#[65535, 116, 2, 116, 31, 116, 56, 116, 58, 116, 60, 116],
	#[55, -100, 62, -83, 60, 96, 2, 96, 58, 96, 46, -98, 53, -81, 52, -103, 36, -116, 10, -109, 30, -37, 54, -107, 61, -104, 32, -102, 47, -72, 44, -95, 56, 96, 38, -91, 23, -21, 41, -106, 49, -75, 51, -78, 22, -62, 37, -97, 31, 96, 43, -115, 42, -92, 19, -12, 59, -118, 34, -51, 25, -26, 12, -82, 39, -114, 40, -74, 50, -101, 27, -4, 48, -99, 15, -111, 20, -55, 24, -54, 35, -96, 21, -27, 18, -40, 28, -34, 45, -117, 33, -59, 29, -7, 26, -57, 13, -105, 14, -86, 16, -90, 17, -112, 57, -88, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 117, 31, 117, 2, 117, 56, 117, 58, 117, 60, 117],
	#[65535, 109, 2, 109, 58, 109, 56, 109, 31, 109, 60, 109],
	#[65535, 99, 31, 99, 2, 99, 56, 99, 58, 99, 60, 99],
	#[65535, 110, 60, 110, 31, 110, 2, 110, 58, 110, 56, 110],
	#[65535, 112, 31, 112, 2, 112, 60, 112, 56, 112, 58, 112],
	#[65535, 113, 2, 113, 58, 113, 31, 113, 56, 113, 60, 113],
	#[65535, 147, 55, 147, 28, 147, 14, 147, 42, 147, 45, 147, 22, 147, 29, 147, 44, 147, 50, 147, 18, 147, 47, 147, 30, 147, 54, 147, 52, 147, 23, 147, 61, 147, 35, 147, 38, 147, 62, 147, 31, 147, 27, 147, 12, 147, 49, 147, 16, 147, 46, 147, 53, 147, 59, 147, 39, 147, 37, 147, 48, 147, 51, 147, 15, 147, 26, 147, 60, 147, 24, 147, 19, 147, 10, 147, 33, 147, 13, 147, 2, 147, 57, 147, 36, 147, 20, 147, 40, 147, 43, 147, 21, 147, 34, 147, 41, 147, 56, 147, 32, 147, 25, 147, 58, 147, 11, 147, 17, 147, 63, 147, 64, 147, 65, 147, 66, 147, 67, 147, 68, 147, 69, 147],
	#[65535, 119, 31, 119, 11, 119, 2, 119, 10, 119, 56, 119, 58, 119, 60, 119],
	#[65535, 103, 10, 103, 2, 103, 31, 103, 11, 103, 56, 103, 58, 103, 60, 103],
	#[65535, 120, 31, 120, 10, 120, 2, 120, 56, 120, 11, 120, 58, 120, 60, 120],
	#[65535, 121, 11, 121, 31, 121, 10, 121, 2, 121, 56, 121, 58, 121, 60, 121],
	#[65535, 146, 54, 146, 14, 146, 18, 146, 43, 146, 45, 146, 22, 146, 29, 146, 44, 146, 13, 146, 47, 146, 30, 146, 37, 146, 52, 146, 26, 146, 35, 146, 42, 146, 51, 146, 31, 146, 27, 146, 12, 146, 16, 146, 46, 146, 50, 146, 59, 146, 39, 146, 36, 146, 49, 146, 24, 146, 15, 146, 61, 146, 33, 146, 41, 146, 28, 146, 19, 146, 38, 146, 23, 146, 34, 146, 55, 146, 53, 146, 20, 146, 40, 146, 21, 146, 57, 146, 60, 146, 56, 146, 2, 146, 32, 146, 48, 146, 25, 146, 10, 146, 58, 146, 11, 146, 17, 146, 62, 146, 63, 146, 64, 146, 65, 146, 66, 146, 67, 146, 68, 146, 69, 146],
	#[28, -34, 34, -51, 22, -62, 18, -40, 21, -27, 33, -59, 29, -7, 23, -326, 40, 171, 2, 171, 58, 171, 20, -327, 31, 171, 60, 171, 56, 171, 25, -299, 10, 171, 11, 171],
	#[65535, 163, 31, 163, 10, 163, 11, 163, 2, 163, 56, 163, 58, 163, 60, 163],
	#[29, 59, 23, 59, 31, 49, 28, 59, 22, 59, 2, 49, 33, 59, 10, 49, 40, 49, 20, 59, 21, 59, 34, 59, 11, 49, 56, 49, 18, 59, 58, 49, 60, 49],
	#[29, 57, 23, 57, 31, 34, 28, 57, 22, 57, 18, 57, 11, 34, 2, 34, 33, 57, 10, 34, 40, 34, 20, 57, 21, 57, 56, 34, 58, 34, 60, 34, 34, 57],
	#[65535, 44, 31, 44, 11, 44, 2, 44, 10, 44, 40, 44, 56, 44, 58, 44, 60, 44],
	#[65535, 43, 31, 43, 10, 43, 11, 43, 2, 43, 40, 43, 56, 43, 58, 43, 60, 43],
	#[23, -21, 28, -34, 22, -62, 18, -40, 20, -55, 33, -59, 34, -51, 29, -7, 21, -27],
	#[65535, 45, 2, 45, 58, 45, 31, 45, 10, 45, 40, 45, 56, 45, 11, 45, 60, 45],
	#[65535, 172, 31, 172, 11, 172, 2, 172, 58, 172, 40, 172, 56, 172, 10, 172, 60, 172],
	#[65535, 173, 56, 173, 58, 173, 60, 173, 11, 173, 31, 173, 40, 173, 10, 173, 2, 173],
	#[31, 256, 42, -243, 43, -253, 45, -254, 29, -7, 44, -247, 53, -81, 27, -4, 54, -107, 52, -103, 59, -255, 55, -248, 14, -86, 38, -91, 62, -83, 25, -26, 28, -34, 41, -106, 40, -234, 22, -62, 46, -98, 19, -12, 18, -40, 39, -114, 37, -97, 10, -249, 57, -241, 30, -37, 15, -111, 47, -72, 50, -101, 12, -82, 23, -21, 35, -96, 49, -75, 51, -78, 36, -116, 20, -55, 33, -59, 32, -102, 48, -99, 61, -104, 17, -112, 26, -57, 16, -90, 13, -105, 24, -54, 21, -27, 34, -51, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[31, -349],
	#[56, -337],
	#[65535, 296, 53, 296, 44, 296, 45, 296, 22, 296, 13, 296, 26, 296, 23, 296, 28, 296, 38, 296, 18, 296, 52, 296, 47, 296, 51, 296, 50, 296, 29, 296, 55, 296, 54, 296, 12, 296, 32, 296, 37, 296, 14, 296, 21, 296, 62, 296, 42, 296, 43, 296, 39, 296, 19, 296, 48, 296, 46, 296, 15, 296, 17, 296, 25, 296, 24, 296, 27, 296, 30, 296, 34, 296, 36, 296, 20, 296, 61, 296, 35, 296, 10, 296, 31, 296, 41, 296, 49, 296, 16, 296, 59, 296, 40, 296, 33, 296, 57, 296, 63, 296, 64, 296, 65, 296, 66, 296, 67, 296, 68, 296, 69, 296],
	#[65535, 271, 31, 271],
	#[65535, 276, 31, 276],
	#[31, 256, 30, -37, 39, -114, 47, -72, 46, -98, 45, -254, 19, -12, 36, -116, 38, -91, 44, -247, 43, -253, 15, -111, 41, -106, 22, -62, 35, -96, 42, -243, 10, -249, 29, -7, 34, -51, 27, -4, 32, -102, 26, -57, 24, -54, 40, -234, 18, -40, 37, -97, 28, -34, 12, -82, 21, -27, 16, -90, 51, -78, 25, -26, 20, -55, 17, -112, 13, -105, 33, -59, 23, -21, 14, -86, 48, -99, 49, -75, 50, -101, 52, -103, 53, -81, 54, -107, 55, -248, 57, -241, 59, -255, 61, -104, 62, -83, 63, -110, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 277, 31, 277],
	#[58, -343],
	#[65535, 297, 22, 297, 54, 297, 52, 297, 45, 297, 31, 297, 29, 297, 43, 297, 46, 297, 18, 297, 39, 297, 42, 297, 49, 297, 21, 297, 15, 297, 47, 297, 50, 297, 28, 297, 14, 297, 38, 297, 23, 297, 13, 297, 55, 297, 53, 297, 26, 297, 27, 297, 41, 297, 19, 297, 44, 297, 30, 297, 12, 297, 51, 297, 32, 297, 17, 297, 24, 297, 33, 297, 20, 297, 34, 297, 48, 297, 37, 297, 35, 297, 16, 297, 40, 297, 10, 297, 25, 297, 36, 297, 57, 297, 59, 297, 61, 297, 62, 297, 63, 297, 64, 297, 65, 297, 66, 297, 67, 297, 68, 297, 69, 297],
	#[65535, 269, 31, 269],
	#[65535, 259, 31, 259],
	#[65535, 270, 31, 270],
	#[65535, 273, 31, 273],
	#[65535, 165, 31, 165, 11, 165, 2, 165, 58, 165, 60, 165, 10, 165, 56, 165],
	#[23, -21, 28, -34, 31, 354, 11, 354, 33, -59, 21, -27, 18, -40, 29, -350, 20, -55, 10, 354, 34, -352, 2, 354, 22, -62, 56, 354, 58, 354, 60, 354],
	#[22, 42, 31, 54, 60, 54, 23, 42, 28, 42, 58, 54, 11, 54, 10, 54, 2, 54, 21, 42, 29, 42, 20, 42, 18, 42, 33, 42, 56, 54, 34, 42],
	#[23, -21, 20, -55, 18, -40, 22, -62, 33, -59, 21, -27, 29, -7, 34, -51, 28, -34],
	#[23, 41, 18, 41, 22, 41, 31, 38, 34, 41, 28, 41, 11, 38, 2, 38, 20, 41, 33, 41, 10, 38, 21, 41, 29, 41, 56, 38, 58, 38, 60, 38],
	#[65535, 355, 31, 355, 11, 355, 2, 355, 10, 355, 60, 355, 56, 355, 58, 355],
	#[65535, 356, 31, 356, 10, 356, 11, 356, 2, 356, 58, 356, 60, 356, 56, 356],
	#[65535, 164, 31, 164, 10, 164, 2, 164, 11, 164, 60, 164, 58, 164, 56, 164],
	#[65535, 170, 23, 170, 29, 170, 25, 170, 24, 170, 22, 170, 21, 170, 20, 170, 34, 170],
	#[55, 46, 20, 46, 38, 46, 11, 46, 22, -62, 46, 46, 21, 46, 19, 46, 34, 46, 30, 46, 54, 46, 39, 46, 29, -7, 52, 46, 40, 46, 41, 46, 47, 46, 48, 46, 37, 46, 35, 46, 23, -21, 44, 46, 28, -34, 12, 46, 10, 46, 17, 46, 25, 46, 2, 46, 31, 46, 49, 46, 36, 46, 13, 46, 18, -40, 53, 46, 32, 46, 27, 46, 24, 46, 16, 46, 26, 46, 15, 46, 50, 46, 51, 46, 33, 46, 43, 46, 42, 46, 14, 46, 45, 46, 56, 46, 57, 46, 58, 46, 59, 46, 60, 46, 61, 46, 62, 46, 63, 46, 64, 46, 65, 46, 66, 46, 67, 46, 68, 46],
	#[65535, 233, 55, 233, 37, 233, 21, 233, 20, 233, 39, 233, 44, 233, 15, 233, 11, 233, 53, 233, 10, 233, 34, 233, 2, 233, 36, 233, 19, 233, 61, 233, 14, 233, 38, 233, 62, 233, 51, 233, 31, 233, 33, 233, 41, 233, 57, 233, 46, 233, 50, 233, 52, 233, 35, 233, 48, 233, 30, 233, 54, 233, 16, 233, 26, 233, 47, 233, 40, 233, 12, 233, 32, 233, 25, 233, 13, 233, 49, 233, 58, 233, 24, 233, 59, 233, 56, 233, 27, 233, 17, 233, 42, 233, 43, 233, 60, 233, 45, 233, 63, 233, 64, 233, 65, 233, 66, 233, 67, 233, 68, 233],
	#[65535, 237, 31, 237, 26, 237, 15, 237, 47, 237, 60, 237, 58, 237, 46, 237, 43, 237, 30, 237, 54, 237, 51, 237, 53, 237, 27, 237, 50, 237, 14, 237, 38, 237, 42, 237, 12, 237, 61, 237, 56, 237, 32, 237, 45, 237, 52, 237, 17, 237, 16, 237, 44, 237, 20, 237, 59, 237, 13, 237, 11, 237, 49, 237, 24, 237, 41, 237, 19, 237, 21, 237, 34, 237, 2, 237, 57, 237, 25, 237, 55, 237, 35, 237, 37, 237, 33, 237, 62, 237, 10, 237, 40, 237, 39, 237, 48, 237, 36, 237, 63, 237, 64, 237, 65, 237, 66, 237, 67, 237, 68, 237],
	#[65535, 236, 47, 236, 30, 236, 50, 236, 55, 236, 54, 236, 12, 236, 31, 236, 14, 236, 42, 236, 43, 236, 45, 236, 19, 236, 26, 236, 44, 236, 15, 236, 13, 236, 53, 236, 49, 236, 37, 236, 52, 236, 20, 236, 35, 236, 11, 236, 51, 236, 32, 236, 27, 236, 24, 236, 16, 236, 46, 236, 25, 236, 39, 236, 36, 236, 21, 236, 17, 236, 33, 236, 48, 236, 40, 236, 38, 236, 10, 236, 41, 236, 34, 236, 2, 236, 56, 236, 57, 236, 58, 236, 59, 236, 60, 236, 61, 236, 62, 236, 63, 236, 64, 236, 65, 236, 66, 236, 67, 236, 68, 236],
	#[65535, 47, 47, 47, 50, 47, 60, 47, 30, 47, 54, 47, 12, 47, 56, 47, 31, 47, 14, 47, 52, 47, 42, 47, 43, 47, 45, 47, 58, 47, 46, 47, 15, 47, 13, 47, 51, 47, 53, 47, 49, 47, 37, 47, 36, 47, 26, 47, 59, 47, 61, 47, 35, 47, 62, 47, 17, 47, 44, 47, 27, 47, 41, 47, 40, 47, 16, 47, 48, 47, 39, 47, 10, 47, 57, 47, 21, 47, 2, 47, 33, 47, 24, 47, 19, 47, 38, 47, 55, 47, 20, 47, 34, 47, 32, 47, 25, 47, 11, 47, 63, 47, 64, 47, 65, 47, 66, 47, 67, 47, 68, 47],
	#[65535, 159, 31, 159, 2, 159, 40, 159, 10, 159, 11, 159, 56, 159, 58, 159, 60, 159],
	#[31, -324],
	#[65535, 161, 31, 161, 11, 161, 40, 161, 2, 161, 10, 161, 56, 161, 58, 161, 60, 161],
	#[31, -324],
	#[65535, 160, 10, 160, 11, 160, 31, 160, 40, 160, 2, 160, 56, 160, 58, 160, 60, 160],
	#[65535, 234, 55, 234, 38, 234, 11, 234, 19, 234, 54, 234, 39, 234, 40, 234, 20, 234, 47, 234, 37, 234, 35, 234, 21, 234, 46, 234, 12, 234, 10, 234, 25, 234, 2, 234, 31, 234, 52, 234, 36, 234, 13, 234, 53, 234, 41, 234, 49, 234, 34, 234, 17, 234, 26, 234, 15, 234, 24, 234, 45, 234, 42, 234, 51, 234, 33, 234, 30, 234, 43, 234, 16, 234, 32, 234, 14, 234, 27, 234, 48, 234, 44, 234, 50, 234, 56, 234, 57, 234, 58, 234, 59, 234, 60, 234, 61, 234, 62, 234, 63, 234, 64, 234, 65, 234, 66, 234, 67, 234, 68, 234],
	#[56, -369],
	#[65535, 187, 38, 187, 55, 187, 11, 187, 39, 187, 37, 187, 36, 187, 46, 187, 48, 187, 2, 187, 10, 187, 40, 187, 56, 187, 57, 187, 58, 187],
	#[65535, 174, 55, 174, 37, 174, 11, 174, 39, 174, 46, 174, 36, 174, 38, 174, 40, 174, 48, 174, 10, 174, 2, 174, 56, 174, 57, 174, 58, 174],
	#[65535, 176, 39, 176, 38, 176, 2, 176, 37, 176, 46, 176, 11, 176, 10, 176, 48, 176, 36, 176, 55, 176, 40, 176, 56, 176, 57, 176, 58, 176],
	#[65535, 175, 36, 175, 55, 175, 46, 175, 39, 175, 2, 175, 58, 175, 38, 175, 11, 175, 37, 175, 48, 175, 10, 175, 40, 175, 56, 175, 57, 175],
	#[65535, 308, 59, 308, 42, 308, 47, 308, 38, 308, 13, 308, 55, 308, 52, 308, 22, 308, 50, 308, 43, 308, 49, 308, 61, 308, 44, 308, 30, 308, 39, 308, 51, 308, 29, 308, 15, 308, 36, 308, 19, 308, 57, 308, 21, 308, 46, 308, 28, 308, 12, 308, 45, 308, 48, 308, 31, 308, 16, 308, 20, 308, 17, 308, 53, 308, 14, 308, 37, 308, 62, 308, 40, 308, 58, 308, 26, 308, 41, 308, 18, 308, 35, 308, 10, 308, 25, 308, 27, 308, 60, 308, 33, 308, 56, 308, 24, 308, 32, 308, 23, 308, 63, 308, 64, 308, 65, 308, 66, 308, 67, 308, 68, 308, 69, 308],
	#[65535, 306, 47, 306, 44, 306, 43, 306, 15, 306, 46, 306, 51, 306, 13, 306, 55, 306, 59, 306, 42, 306, 12, 306, 14, 306, 38, 306, 62, 306, 45, 306, 50, 306, 39, 306, 17, 306, 10, 306, 16, 306, 35, 306, 57, 306, 40, 306, 30, 306, 48, 306, 21, 306, 23, 306, 61, 306, 52, 306, 36, 306, 25, 306, 58, 306, 26, 306, 49, 306, 31, 306, 60, 306, 22, 306, 28, 306, 41, 306, 37, 306, 18, 306, 20, 306, 32, 306, 24, 306, 29, 306, 19, 306, 33, 306, 27, 306, 56, 306, 53, 306, 63, 306, 64, 306, 65, 306, 66, 306, 67, 306, 68, 306, 69, 306],
	#[65535, 322, 45, 322, 14, 322, 23, 322, 55, 322, 52, 322, 50, 322, 43, 322, 49, 322, 44, 322, 30, 322, 12, 322, 39, 322, 51, 322, 29, 322, 15, 322, 38, 322, 62, 322, 47, 322, 59, 322, 37, 322, 60, 322, 46, 322, 28, 322, 27, 322, 10, 322, 17, 322, 48, 322, 31, 322, 26, 322, 36, 322, 20, 322, 53, 322, 16, 322, 41, 322, 40, 322, 21, 322, 25, 322, 61, 322, 22, 322, 35, 322, 42, 322, 24, 322, 13, 322, 56, 322, 33, 322, 58, 322, 19, 322, 32, 322, 18, 322, 57, 322, 63, 322, 64, 322, 65, 322, 66, 322, 67, 322, 68, 322, 69, 322],
	#[65535, 235, 39, 235, 38, 235, 46, 235, 36, 235, 2, 235, 55, 235, 37, 235, 21, 235, 34, 235, 62, 235, 50, 235, 20, 235, 17, 235, 59, 235, 13, 235, 15, 235, 48, 235, 30, 235, 27, 235, 33, 235, 41, 235, 40, 235, 10, 235, 52, 235, 14, 235, 45, 235, 35, 235, 16, 235, 11, 235, 31, 235, 60, 235, 44, 235, 53, 235, 12, 235, 49, 235, 19, 235, 58, 235, 26, 235, 25, 235, 54, 235, 47, 235, 43, 235, 56, 235, 32, 235, 42, 235, 51, 235, 24, 235, 61, 235, 57, 235, 63, 235, 64, 235, 65, 235, 66, 235, 67, 235, 68, 235],
	#[65535, 148, 22, 148, 29, 148, 23, 148, 46, 148, 28, 148, 27, 148, 45, 148, 30, 148, 54, 148, 18, 148, 53, 148, 14, 148, 39, 148, 43, 148, 15, 148, 49, 148, 47, 148, 36, 148, 16, 148, 13, 148, 26, 148, 55, 148, 50, 148, 19, 148, 44, 148, 42, 148, 12, 148, 21, 148, 38, 148, 32, 148, 24, 148, 52, 148, 20, 148, 48, 148, 10, 148, 33, 148, 35, 148, 2, 148, 11, 148, 51, 148, 37, 148, 25, 148, 40, 148, 34, 148, 41, 148, 31, 148, 17, 148, 56, 148, 57, 148, 58, 148, 59, 148, 60, 148, 61, 148, 62, 148, 63, 148, 64, 148, 65, 148, 66, 148, 67, 148, 68, 148, 69, 148],
	#[65535, 184, 15, 184, 46, 184, 31, 184, 19, 184, 30, 184, 12, 184, 14, 184, 20, 184, 47, 184, 35, 184, 21, 184, 27, 184, 45, 184, 29, 184, 13, 184, 17, 184, 25, 184, 16, 184, 32, 184, 39, 184, 34, 184, 28, 184, 26, 184, 41, 184, 22, 184, 42, 184, 10, 184, 36, 184, 33, 184, 23, 184, 43, 184, 40, 184, 18, 184, 44, 184, 48, 184, 38, 184, 2, 184, 11, 184, 37, 184, 24, 184, 49, 184, 50, 184, 51, 184, 52, 184, 53, 184, 54, 184, 55, 184, 56, 184, 57, 184, 58, 184, 59, 184, 60, 184, 61, 184, 62, 184, 63, 184, 64, 184, 65, 184, 66, 184, 67, 184, 68, 184, 69, 184],
	#[65535, 91, 2, 91],
	#[2, 92, 40, -393],
	#[2, -392],
	#[55, -100, 54, -107, 62, -83, 46, -98, 43, -115, 44, -95, 38, -91, 61, -104, 50, -101, 39, -114, 42, -92, 59, -118, 32, -102, 24, -54, 47, -72, 25, -26, 18, -40, 21, -27, 23, -21, 57, -88, 52, -103, 36, -116, 20, -55, 35, -96, 41, -106, 29, -7, 48, -99, 13, -105, 22, -62, 28, -34, 12, -82, 37, -97, 15, -111, 45, -117, 17, -112, 34, -51, 14, -86, 19, -12, 33, -59, 16, -90, 49, -75, 51, -78, 53, -81, 63, -110, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[47, -72, 38, -91, 23, -21, 41, -106, 2, 104, 22, -62, 46, -98, 43, -115, 44, -95, 32, -102, 39, -114, 18, -40, 15, -111, 36, -116, 20, -55, 19, -12, 48, -99, 24, -54, 35, -96, 21, -27, 28, -34, 40, 104, 45, -117, 34, -51, 33, -59, 29, -7, 13, -105, 17, -112, 25, -26, 16, -90, 14, -86, 37, -97, 12, -82, 42, -92, 49, -75, 50, -101, 51, -78, 52, -103, 53, -81, 54, -107, 55, -100, 57, -88, 59, -118, 61, -104, 62, -83, 63, -110, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[38, -91, 43, -115, 37, -97, 41, -106, 35, -96, 14, -86, 34, -51, 45, -117, 36, -116, 39, -114, 46, -98, 40, 122, 16, -90, 15, -111, 2, 122, 44, -95, 47, -72, 12, -82, 25, -26, 48, -99, 21, -27, 42, -92, 32, -102, 20, -55, 49, -75, 24, -54, 13, -105, 33, -59, 19, -12, 17, -112, 50, -101, 51, -78, 52, -103, 53, -81, 54, -107, 55, -100, 57, -88, 59, -118, 61, -104, 62, -83, 63, -110, 65, -61, 66, -52, 67, -23, 68, -56],
	#[65535, 95, 2, 95, 40, 95],
	#[65535, 94, 2, 94, 40, 94],
	#[65535, 106, 2, 106, 40, 106],
	#[65535, 107, 40, 107, 2, 107],
	#[65535, 123, 2, 123, 40, 123],
	#[65535, 124, 2, 124, 40, 124],
	#[65535, 105, 2, 105, 40, 105],
	#[65535, 7, #"eoi", 7],
	#[41, -382],
	#[65535, 93, 2, 93],
	#[2, -396],
	#[65535, 3, #"eoi", 3],
	#[2, -398],
	#[65535, 8, #"eoi", 8],
	#[65535, 239, 2, 239],
	#[48, -424],
	#[30, -37, 53, -14, 55, -36, 10, -433, 43, -63, 41, -43, 61, -49, 29, -7, 2, 250, 13, -42, 19, -12, 24, -54, 21, -27, 36, -66, 20, -55, 62, -16, 27, -4, 18, -40, 42, -30, 28, -34, 12, -9, 35, -31, 26, -57, 34, -51, 22, -62, 25, -26, 33, -59, 23, -21, 63, -50, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[48, 221, 40, -414, 56, 221],
	#[13, -42, 36, -66, 12, -9, 53, -14, 30, -37, 43, -63, 23, -21, 26, -57, 41, -43, 42, -30, 25, -26, 27, -4, 24, -54, 22, -62, 35, -31, 10, -422, 21, -27, 33, -59, 18, -40, 19, -12, 29, -7, 34, -51, 55, -36, 20, -55, 28, -34, 2, 250, 61, -49, 62, -16, 63, -50, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[13, -42, 12, -9, 53, -14, 43, -63, 36, -66, 42, -30, 24, -54, 22, -62, 25, -26, 35, -31, 20, -55, 21, -27, 33, -59, 41, -43, 18, -40, 19, -12, 29, -7, 23, -21, 34, -51, 55, -36, 28, -34, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[23, 254, 30, 254, 43, 254, 22, 254, 35, 254, 53, 254, 28, 254, 12, 254, 27, 254, 19, 254, 36, 254, 25, 254, 33, 254, 41, 254, 29, 254, 18, 254, 13, 254, 63, 254, 10, 254, 24, 254, 2, 254, 26, 254, 21, 254, 20, 254, 42, 254, 34, 254, 48, -410, 55, 254, 65, 254, 61, 254, 62, 254, 64, 254, 66, 254, 67, 254, 68, 254, 69, 254],
	#[2, -408],
	#[65535, 241, 2, 241],
	#[65535, 6, #"eoi", 6],
	#[65535, 253, 53, 253, 23, 253, 13, 253, 21, 253, 2, 253, 22, 253, 29, 253, 28, 253, 33, 253, 34, 253, 43, 253, 36, 253, 55, 253, 42, 253, 10, 253, 25, 253, 27, 253, 12, 253, 26, 253, 35, 253, 20, 253, 24, 253, 18, 253, 19, 253, 41, 253, 30, 253, 61, 253, 62, 253, 63, 253, 64, 253, 65, 253, 66, 253, 67, 253, 68, 253, 69, 253],
	#[65535, 255, 23, 255, 13, 255, 53, 255, 21, 255, 2, 255, 22, 255, 26, 255, 28, 255, 25, 255, 33, 255, 34, 255, 43, 255, 36, 255, 55, 255, 42, 255, 10, 255, 29, 255, 12, 255, 35, 255, 20, 255, 24, 255, 18, 255, 19, 255, 41, 255, 27, 255, 30, 255, 61, 255, 62, 255, 63, 255, 64, 255, 65, 255, 66, 255, 67, 255, 68, 255, 69, 255],
	#[40, -412, 56, -369],
	#[13, -42, 36, -66, 22, -62, 12, -9, 43, -63, 23, -21, 42, -30, 25, -26, 24, -54, 53, -14, 35, -31, 20, -55, 21, -27, 33, -59, 41, -43, 18, -40, 19, -12, 29, -7, 34, -51, 55, -36, 28, -34, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[56, -416],
	#[13, -42, 12, -9, 53, -14, 43, -63, 23, -21, 19, -12, 55, -36, 42, -30, 28, -34, 24, -54, 22, -62, 18, -40, 33, -59, 36, -66, 20, -55, 21, -27, 25, -26, 41, -43, 29, -7, 34, -51, 35, -31, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 222, 48, 222, 56, 222],
	#[48, -417],
	#[65535, 252, 55, 252, 2, 252, 12, 252, 29, 252, 10, 252, 13, 252, 62, 252, 19, 252, 21, 252, 61, 252, 22, 252, 20, 252, 27, 252, 23, 252, 42, 252, 30, 252, 28, 252, 53, 252, 35, 252, 41, 252, 24, 252, 43, 252, 34, 252, 26, 252, 18, 252, 25, 252, 36, 252, 33, 252, 63, 252, 64, 252, 65, 252, 66, 252, 67, 252, 68, 252, 69, 252],
	#[65535, 85, 2, 85, 10, 85],
	#[65535, 247, 2, 247],
	#[10, -425, 2, 214],
	#[65535, 248, 2, 248],
	#[13, -42, 21, -27, 29, -7, 28, -34, 12, -9, 41, -43, 34, -51, 43, -63, 36, -66, 35, -31, 24, -54, 42, -30, 33, -59, 22, -62, 19, -12, 25, -26, 20, -55, 2, 249, 18, -40, 32, -405, 23, -21, 53, -14, 55, -404, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 242, 62, 242, 13, 242, 23, 242, 41, 242, 55, 242, 20, 242, 18, 242, 21, 242, 61, 242, 34, 242, 35, 242, 12, 242, 26, 242, 29, 242, 27, 242, 42, 242, 33, 242, 19, 242, 24, 242, 22, 242, 28, 242, 10, 242, 43, 242, 2, 242, 30, 242, 25, 242, 36, 242, 53, 242, 63, 242, 64, 242, 65, 242, 66, 242, 67, 242, 68, 242, 69, 242],
	#[65535, 251, 13, 251, 23, 251, 55, 251, 21, 251, 20, 251, 19, 251, 22, 251, 28, 251, 12, 251, 53, 251, 34, 251, 2, 251, 36, 251, 35, 251, 42, 251, 10, 251, 25, 251, 29, 251, 41, 251, 43, 251, 33, 251, 24, 251, 18, 251, 26, 251, 27, 251, 30, 251, 61, 251, 62, 251, 63, 251, 64, 251, 65, 251, 66, 251, 67, 251, 68, 251, 69, 251],
	#[12, -9, 18, -40, 29, -7, 13, -42, 2, 215, 30, -37, 25, -26, 41, -43, 23, -21, 32, -405, 36, -66, 20, -55, 35, -31, 26, -57, 22, -62, 28, -34, 43, -63, 19, -12, 21, -27, 24, -54, 42, -30, 27, -4, 34, -51, 33, -59, 53, -14, 55, -404, 61, -49, 62, -16, 63, -50, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 82, 2, 82],
	#[48, 221, 40, -414, 10, 89, 2, 89],
	#[65535, 243, 23, 243, 55, 243, 30, 243, 28, 243, 2, 243, 26, 243, 20, 243, 22, 243, 27, 243, 13, 243, 35, 243, 24, 243, 43, 243, 18, 243, 42, 243, 33, 243, 34, 243, 53, 243, 41, 243, 29, 243, 10, 243, 19, 243, 12, 243, 25, 243, 21, 243, 36, 243, 61, 243, 62, 243, 63, 243, 64, 243, 65, 243, 66, 243, 67, 243, 68, 243, 69, 243],
	#[65535, 86, 10, 86, 2, 86],
	#[65535, 246, 2, 246],
	#[10, -435, 2, 214],
	#[65535, 240, 2, 240],
	#[36, -66, 23, -21, 13, -42, 55, -404, 53, -14, 22, -62, 43, -63, 19, -12, 32, -405, 12, -9, 29, -7, 18, -40, 25, -26, 20, -55, 2, 249, 24, -54, 35, -31, 21, -27, 28, -34, 42, -30, 33, -59, 41, -43, 34, -51, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 244, 2, 244],
	#[27, -4, 43, -63, 42, -30, 29, -7, 22, -62, 35, -31, 20, -55, 30, -37, 32, -405, 41, -43, 24, -54, 12, -9, 23, -21, 13, -42, 53, -14, 36, -66, 21, -27, 19, -12, 34, -51, 28, -34, 33, -59, 18, -40, 2, 215, 26, -57, 25, -26, 55, -404, 61, -49, 62, -16, 63, -50, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[65535, 245, 2, 245],
	#[2, -442],
	#[47, -439, 2, 216],
	#[22, -62, 20, -55, 43, -63, 13, -42, 19, -12, 25, -26, 23, -21, 12, -9, 24, -54, 29, -7, 18, -40, 42, -30, 34, -51, 21, -27, 33, -59, 28, -34, 53, -14, 55, -36, 61, -49, 62, -16, 63, -50, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[55, -214, 46, -213, 57, -215, 2, 220],
	#[65535, 217, 2, 217],
	#[65535, 4, #"eoi", 4],
	#[2, -444],
	#[65535, 2, #"eoi", 2],
	#[10, -449, 2, 214],
	#[2, -448],
	#[65535, 81, 2, 81],
	#[65535, 5, #"eoi", 5],
	#[36, -66, 22, -62, 23, -21, 13, -42, 53, -14, 26, -57, 21, -27, 27, -4, 43, -63, 29, -7, 12, -9, 24, -54, 30, -37, 41, -43, 18, -40, 42, -30, 25, -26, 28, -34, 33, -59, 35, -31, 2, 215, 19, -12, 34, -51, 20, -55, 55, -36, 61, -49, 62, -16, 63, -50, 64, -47, 65, -61, 66, -52, 67, -23, 68, -56, 69, -58],
	#[2, -451],
	#[65535, 1, #"eoi", 1],
	#[65535, 10, #"eoi", 10],
	#[65535, 9, #"eoi", 9]],
  goto-table:
      #[#[155, 17, 149, 32, 134, 7, 64, 46, 125, 31, 137, 67, 150, 52, 124, 16, 130, 63, 61, 48, 138, 21, 26, 56, 65, 60, 83, 18, 98, 1, 81, 38, 27, 3, 69, 57, 88, 43, 11, 23, 87, 27, 62, 15, 90, 19, 55, 35, 154, 64, 68, 55, 43, 62, 41, 42, 67, 22, 25, 25, 30, 36, 28, 33, 24, 53, 75, 59, 66, 51, 71, 47, 42, 29, 22, 61, 20, 54, 36, 65, 0, 2, 33, 58, 8, 44, 63, 49, 34, 50, 21, 26, 35, 30, 18, 39, 76, 12, 53, 13, 13, 41, 4, 34, 29, 6, 9, 14, 5, 9, 19, 11, 12, 8, 7, 10, 121, 24, 89, 66, 153, 40, 146, 37, 135, 28, 23, 20, 6, 4, 3, 5, 136, 45],
	#[11, 451, 10, 452],
	#[1, 449],
	#[],
	#[87, 27, 22, 61, 90, 19, 27, 3, 155, 17, 18, 39, 138, 21, 125, 31, 25, 25, 67, 22, 12, 8, 28, 33, 97, 444, 35, 30, 134, 7, 19, 11, 53, 13, 98, 417, 42, 29, 88, 43, 29, 6, 153, 40, 146, 37, 81, 38, 121, 24, 76, 12, 83, 18, 94, 445, 124, 16, 137, 67, 64, 46, 55, 35, 13, 41, 66, 51, 62, 15, 130, 63, 136, 45, 135, 28, 41, 42, 26, 56, 61, 48, 21, 26, 154, 64, 20, 54, 36, 65, 33, 58, 149, 32, 95, 446, 65, 60, 43, 62, 63, 49, 34, 50, 24, 53, 150, 52, 23, 20, 68, 55, 30, 36, 89, 66, 69, 57, 75, 59],
	#[146, 442, 76, 12, 75, 59, 20, 54, 34, 50, 33, 58, 53, 13, 21, 26],
	#[],
	#[],
	#[],
	#[145, 436, 146, 437, 76, 12, 75, 59, 53, 13, 34, 50, 33, 58, 21, 26, 20, 54],
	#[163, 402, 161, 406, 159, 400, 158, 398, 157, 405, 155, 17, 154, 64, 153, 40, 150, 52, 149, 401, 148, 399, 146, 37, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 130, 63, 90, 19, 89, 66, 83, 18, 81, 38, 76, 12, 75, 59, 69, 57, 68, 55, 67, 22, 63, 49, 62, 15, 61, 48, 65, 60, 55, 403, 53, 13, 32, 404, 28, 33, 33, 58, 24, 53, 21, 26, 18, 39, 25, 25, 35, 30, 34, 50, 66, 51, 12, 8, 42, 29, 23, 20, 20, 54, 29, 6, 22, 61, 19, 11, 43, 62, 13, 41, 36, 65, 41, 42],
	#[],
	#[],
	#[],
	#[12, 81, 14, 85, 48, 98, 19, 11, 16, 89, 40, 73, 51, 77, 49, 74, 42, 91, 110, 75, 53, 80, 47, 71, 57, 87, 15, 110, 29, 6, 67, 22, 27, 3, 64, 46, 54, 106, 30, 36, 18, 39, 35, 95, 21, 26, 25, 25, 44, 94, 37, 96, 22, 61, 87, 27, 32, 101, 81, 38, 55, 99, 50, 100, 62, 82, 23, 20, 41, 105, 33, 58, 34, 50, 17, 111, 61, 103, 28, 33, 24, 53, 26, 56, 118, 93, 39, 113, 20, 54, 46, 97, 52, 102, 68, 55, 63, 109, 65, 60, 119, 107, 45, 116, 38, 90, 75, 59, 10, 108, 59, 117, 43, 114, 36, 115, 13, 104, 120, 70, 124, 83, 125, 31, 138, 86, 104, 92, 117, 78, 121, 88, 92, 112, 155, 84, 89, 66, 134, 76, 83, 18, 88, 43, 76, 79, 72, 69, 69, 57, 66, 51, 103, 396],
	#[],
	#[],
	#[],
	#[181, 245, 155, 239, 166, 244, 118, 243, 138, 86, 120, 232, 179, 251, 180, 236, 134, 234, 165, 235, 124, 238, 172, 249, 121, 241, 88, 43, 89, 66, 81, 38, 83, 18, 69, 57, 125, 31, 72, 231, 66, 51, 65, 60, 64, 46, 63, 109, 75, 59, 61, 103, 76, 237, 92, 250, 67, 22, 53, 80, 57, 240, 48, 98, 47, 71, 59, 254, 45, 253, 43, 252, 41, 105, 62, 82, 51, 77, 37, 96, 68, 55, 54, 106, 40, 233, 87, 27, 24, 53, 42, 242, 52, 102, 49, 74, 33, 58, 34, 50, 38, 90, 32, 101, 23, 20, 28, 33, 44, 246, 16, 89, 25, 25, 46, 97, 29, 6, 27, 3, 21, 26, 35, 95, 19, 11, 50, 100, 55, 247, 30, 36, 13, 104, 20, 54, 36, 115, 17, 111, 15, 110, 26, 56, 39, 113, 18, 39, 10, 248, 14, 85, 12, 81, 22, 61],
	#[155, 17, 146, 37, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 130, 211, 89, 66, 83, 18, 81, 38, 76, 12, 75, 59, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 49, 62, 15, 61, 48, 55, 35, 53, 13, 43, 62, 42, 29, 34, 50, 33, 58, 29, 6, 28, 33, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 13, 41, 12, 8],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[155, 304, 138, 86, 134, 76, 120, 70, 119, 107, 118, 93, 117, 302, 114, 305, 106, 301, 105, 306, 92, 112, 89, 66, 83, 18, 81, 38, 76, 79, 75, 59, 72, 69, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 109, 62, 82, 59, 117, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 57, 87, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 61, 103, 41, 105, 40, 303, 50, 100, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 42, 91, 43, 114, 29, 6, 28, 33, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 16, 89, 17, 111, 12, 81, 13, 104, 14, 85, 15, 110, 39, 113],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[153, 40, 130, 63, 138, 21, 136, 45, 146, 37, 137, 67, 154, 64, 155, 17, 149, 394, 135, 28, 150, 52, 90, 19, 89, 66, 134, 7, 83, 18, 81, 38, 76, 12, 75, 59, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 49, 62, 15, 61, 48, 55, 35, 53, 13, 43, 62, 42, 29, 41, 42, 35, 30, 29, 6, 28, 33, 33, 58, 25, 25, 24, 53, 34, 50, 36, 65, 18, 39, 19, 11, 20, 54, 21, 26, 22, 61, 12, 8, 13, 41, 23, 20],
	#[155, 17, 154, 64, 153, 40, 149, 367, 146, 37, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 150, 52, 130, 63, 90, 19, 89, 66, 83, 18, 76, 12, 81, 38, 69, 57, 66, 51, 65, 60, 63, 49, 55, 35, 34, 50, 43, 62, 62, 15, 18, 39, 24, 53, 19, 11, 13, 41, 41, 42, 33, 58, 75, 59, 68, 55, 21, 26, 42, 29, 25, 25, 28, 33, 53, 13, 29, 6, 20, 54, 36, 65, 61, 48, 67, 22, 22, 61, 12, 8, 35, 30, 23, 20],
	#[128, 284, 127, 286, 126, 285, 33, 287],
	#[],
	#[155, 84, 134, 76, 138, 86, 125, 31, 124, 83, 121, 88, 120, 70, 119, 107, 118, 93, 117, 78, 110, 75, 104, 92, 103, 283, 92, 112, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 79, 75, 59, 72, 69, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 61, 103, 59, 117, 57, 87, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 43, 114, 42, 91, 41, 105, 40, 73, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 30, 36, 29, 6, 28, 33, 27, 3, 26, 56, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 108],
	#[],
	#[],
	#[],
	#[],
	#[146, 275, 123, 278, 122, 276, 83, 274, 81, 277, 76, 12, 75, 59, 53, 13, 34, 50, 33, 58, 29, 6, 28, 33, 23, 20, 22, 61, 21, 26, 20, 54, 18, 39],
	#[101, 379, 100, 378, 99, 380, 41, 381],
	#[],
	#[],
	#[],
	#[],
	#[137, 127, 63, 49],
	#[],
	#[142, 120, 141, 119, 140, 125, 139, 126, 138, 21, 137, 67, 136, 122, 68, 55, 67, 22, 66, 51, 65, 60, 63, 49, 62, 15, 61, 48, 41, 124, 13, 41, 12, 8],
	#[91, 223, 38, 155, 37, 160, 36, 192, 39, 189],
	#[],
	#[],
	#[],
	#[],
	#[190, 140, 189, 183, 188, 161, 187, 146, 185, 169, 184, 154, 183, 138, 182, 180, 91, 164, 77, 159, 74, 176, 73, 151, 72, 135, 69, 187, 68, 184, 67, 153, 66, 181, 65, 190, 64, 174, 63, 179, 62, 147, 61, 175, 59, 194, 57, 150, 55, 166, 53, 145, 52, 173, 51, 143, 50, 168, 48, 165, 47, 137, 46, 162, 45, 193, 44, 158, 43, 191, 42, 156, 41, 170, 40, 141, 39, 189, 38, 155, 37, 160, 36, 192, 35, 157, 33, 188, 32, 172, 31, 142, 30, 167, 29, 139, 28, 163, 27, 136, 26, 186, 25, 152, 24, 185, 23, 149, 22, 182, 21, 148, 20, 177, 19, 144, 18, 171, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 178],
	#[],
	#[],
	#[136, 122, 137, 67, 138, 21, 139, 121, 140, 123, 141, 119, 142, 120, 68, 55, 67, 22, 66, 51, 65, 60, 63, 49, 62, 15, 61, 48, 41, 124, 13, 41, 12, 8],
	#[],
	#[],
	#[57, 214, 55, 213, 46, 212],
	#[],
	#[],
	#[55, 68],
	#[],
	#[155, 84, 134, 76, 138, 86, 125, 31, 124, 83, 120, 70, 121, 88, 104, 92, 103, 72, 117, 78, 118, 93, 92, 112, 119, 107, 89, 66, 88, 43, 110, 75, 83, 18, 81, 38, 76, 79, 75, 59, 72, 69, 69, 57, 68, 55, 66, 51, 64, 46, 63, 109, 62, 82, 61, 103, 65, 60, 57, 87, 55, 99, 54, 106, 48, 98, 44, 94, 43, 114, 49, 74, 67, 22, 40, 73, 51, 77, 36, 115, 52, 102, 53, 80, 59, 117, 32, 101, 42, 91, 30, 36, 87, 27, 50, 100, 24, 53, 34, 50, 35, 95, 21, 26, 16, 89, 26, 56, 27, 3, 25, 25, 39, 113, 46, 97, 28, 33, 19, 11, 41, 105, 47, 71, 45, 116, 17, 111, 37, 96, 12, 81, 33, 58, 22, 61, 13, 104, 18, 39, 29, 6, 20, 54, 14, 85, 10, 108, 23, 20, 15, 110, 38, 90],
	#[],
	#[],
	#[],
	#[56, 377],
	#[19, 11, 20, 54, 27, 3, 22, 61, 28, 33, 14, 85, 10, 108, 15, 110, 23, 20, 125, 31, 155, 84, 104, 92, 89, 66, 81, 38, 120, 70, 121, 88, 88, 43, 92, 112, 103, 316, 124, 83, 138, 86, 69, 57, 68, 55, 72, 69, 66, 51, 134, 76, 110, 75, 63, 109, 75, 59, 119, 107, 117, 78, 76, 79, 87, 27, 83, 18, 55, 99, 67, 22, 53, 80, 52, 102, 65, 60, 57, 87, 118, 93, 48, 98, 47, 71, 59, 117, 45, 116, 64, 46, 43, 114, 61, 103, 41, 105, 40, 73, 62, 82, 51, 77, 37, 96, 36, 115, 49, 74, 34, 50, 33, 58, 32, 101, 30, 36, 29, 6, 50, 100, 44, 94, 26, 56, 25, 25, 46, 97, 35, 95, 21, 26, 42, 91, 24, 53, 18, 39, 17, 111, 16, 89, 38, 90, 13, 104, 12, 81, 39, 113, 54, 106],
	#[],
	#[],
	#[],
	#[],
	#[155, 84, 138, 86, 134, 76, 125, 31, 124, 83, 121, 88, 120, 70, 119, 107, 118, 93, 117, 78, 110, 75, 104, 92, 103, 315, 92, 112, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 79, 75, 59, 72, 69, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 61, 103, 59, 117, 57, 87, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 43, 114, 42, 91, 41, 105, 40, 73, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 30, 36, 29, 6, 28, 33, 27, 3, 26, 56, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 108],
	#[],
	#[],
	#[],
	#[],
	#[112, 309, 111, 314, 10, 310],
	#[138, 86, 134, 76, 125, 31, 124, 83, 121, 88, 120, 70, 119, 107, 118, 93, 117, 78, 110, 312, 109, 313, 92, 112, 89, 66, 88, 43, 87, 27, 76, 79, 75, 59, 72, 69, 68, 55, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 61, 103, 59, 117, 57, 87, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 43, 114, 42, 91, 41, 105, 40, 73, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 30, 36, 27, 3, 26, 56, 25, 25, 24, 53, 21, 26, 20, 54, 19, 11, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 108],
	#[],
	#[],
	#[125, 31, 121, 88, 120, 70, 119, 107, 117, 78, 155, 84, 110, 75, 124, 83, 103, 307, 118, 93, 138, 86, 92, 112, 104, 92, 134, 76, 87, 27, 88, 43, 89, 66, 81, 38, 75, 59, 83, 18, 69, 57, 68, 55, 66, 51, 64, 46, 63, 109, 72, 69, 61, 103, 65, 60, 57, 87, 55, 99, 67, 22, 53, 80, 52, 102, 44, 94, 48, 98, 76, 79, 40, 73, 36, 115, 46, 97, 35, 95, 49, 74, 59, 117, 47, 71, 38, 90, 42, 91, 50, 100, 33, 58, 43, 114, 32, 101, 34, 50, 51, 77, 25, 25, 18, 39, 12, 81, 28, 33, 62, 82, 41, 105, 29, 6, 24, 53, 45, 116, 20, 54, 54, 106, 30, 36, 26, 56, 39, 113, 37, 96, 10, 108, 22, 61, 27, 3, 16, 89, 14, 85, 19, 11, 23, 20, 21, 26, 17, 111, 15, 110, 13, 104],
	#[112, 309, 111, 308, 10, 310],
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
	#[155, 84, 138, 86, 134, 76, 125, 31, 124, 83, 121, 88, 120, 70, 119, 107, 118, 93, 117, 78, 110, 75, 104, 92, 103, 300, 92, 112, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 79, 75, 59, 72, 69, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 61, 103, 59, 117, 57, 87, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 43, 114, 42, 91, 41, 105, 40, 73, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 30, 36, 29, 6, 28, 33, 27, 3, 26, 56, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 108],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[155, 84, 138, 86, 134, 76, 125, 31, 124, 83, 121, 88, 120, 70, 119, 107, 118, 93, 117, 78, 110, 75, 104, 92, 103, 282, 92, 112, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 79, 72, 69, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 59, 117, 55, 99, 54, 106, 44, 94, 49, 74, 40, 73, 51, 77, 37, 96, 53, 80, 48, 98, 42, 91, 57, 87, 41, 105, 46, 97, 21, 26, 20, 54, 25, 25, 16, 89, 15, 110, 27, 3, 28, 33, 29, 6, 35, 95, 30, 36, 50, 100, 33, 58, 24, 53, 17, 111, 32, 101, 10, 108, 18, 39, 12, 81, 19, 11, 38, 90, 14, 85, 34, 50, 47, 71, 61, 103, 52, 102, 26, 56, 22, 61, 13, 104, 75, 59, 45, 116, 23, 20, 43, 114, 36, 115, 39, 113],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[138, 86, 125, 31, 124, 83, 121, 88, 120, 70, 119, 107, 118, 93, 117, 78, 155, 84, 134, 76, 104, 92, 103, 118, 92, 112, 89, 66, 88, 43, 110, 75, 81, 38, 76, 79, 75, 59, 72, 69, 83, 18, 68, 55, 67, 22, 63, 109, 62, 82, 52, 102, 69, 57, 48, 98, 59, 117, 41, 105, 55, 99, 20, 54, 57, 87, 64, 46, 42, 91, 25, 25, 53, 80, 65, 60, 28, 33, 49, 74, 32, 101, 43, 114, 40, 73, 66, 51, 45, 116, 51, 77, 54, 106, 50, 100, 35, 95, 17, 111, 16, 89, 37, 96, 24, 53, 12, 81, 33, 58, 44, 94, 27, 3, 18, 39, 29, 6, 87, 27, 38, 90, 36, 115, 19, 11, 61, 103, 23, 20, 21, 26, 30, 36, 26, 56, 34, 50, 13, 104, 10, 108, 46, 97, 22, 61, 39, 113, 14, 85, 47, 71, 15, 110],
	#[60, 376],
	#[40, 129],
	#[],
	#[56, 134],
	#[],
	#[46, 131],
	#[],
	#[],
	#[58, 128],
	#[],
	#[],
	#[138, 21, 137, 67, 136, 122, 142, 130, 68, 55, 66, 51, 63, 49, 62, 15, 61, 48, 65, 60, 67, 22, 41, 124, 12, 8, 13, 41],
	#[],
	#[142, 132, 138, 21, 137, 67, 136, 122, 68, 55, 66, 51, 63, 49, 62, 15, 61, 48, 65, 60, 67, 22, 41, 124, 12, 8, 13, 41],
	#[56, 133],
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
	#[77, 270, 74, 176, 73, 151, 33, 188, 32, 172, 31, 142, 30, 167, 29, 139, 28, 163, 27, 136, 26, 186, 25, 152, 24, 185, 23, 149, 22, 182, 21, 148, 20, 177, 19, 144, 18, 171],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[74, 176, 184, 154, 65, 190, 185, 169, 61, 175, 64, 174, 190, 140, 73, 151, 72, 135, 188, 161, 91, 164, 189, 183, 50, 168, 57, 150, 43, 191, 40, 141, 48, 165, 42, 156, 51, 143, 41, 170, 52, 173, 68, 184, 77, 159, 183, 138, 187, 146, 66, 181, 53, 145, 67, 153, 182, 268, 69, 187, 31, 142, 32, 172, 16, 89, 62, 147, 38, 155, 55, 166, 19, 144, 23, 149, 24, 185, 18, 171, 25, 152, 37, 160, 44, 158, 35, 157, 13, 104, 59, 194, 46, 162, 29, 139, 20, 177, 36, 192, 45, 193, 26, 186, 63, 179, 21, 148, 12, 81, 28, 163, 33, 188, 17, 111, 30, 167, 27, 136, 10, 178, 47, 137, 39, 189, 14, 85, 22, 182, 15, 110],
	#[],
	#[],
	#[],
	#[190, 140, 189, 183, 188, 161, 187, 146, 185, 169, 184, 154, 183, 267, 91, 164, 77, 159, 74, 176, 72, 135, 69, 187, 73, 151, 65, 190, 64, 174, 63, 179, 62, 147, 52, 173, 68, 184, 57, 150, 48, 165, 59, 194, 66, 181, 43, 191, 61, 175, 67, 153, 50, 168, 51, 143, 37, 160, 36, 192, 32, 172, 42, 156, 40, 141, 29, 139, 24, 185, 46, 162, 35, 157, 33, 188, 47, 137, 45, 193, 41, 170, 31, 142, 27, 136, 55, 166, 53, 145, 20, 177, 18, 171, 30, 167, 25, 152, 26, 186, 39, 189, 19, 144, 12, 81, 28, 163, 44, 158, 17, 111, 38, 155, 14, 85, 16, 89, 23, 149, 21, 148, 15, 110, 13, 104, 10, 178, 22, 182],
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
	#[190, 140, 189, 183, 188, 161, 187, 146, 185, 169, 184, 154, 183, 138, 182, 265, 91, 164, 77, 159, 74, 176, 73, 151, 72, 135, 69, 187, 68, 184, 67, 153, 66, 181, 65, 190, 64, 174, 63, 179, 62, 147, 61, 175, 59, 194, 57, 150, 55, 166, 53, 145, 52, 173, 51, 143, 50, 168, 48, 165, 47, 137, 46, 162, 45, 193, 44, 158, 43, 191, 42, 156, 41, 170, 40, 141, 39, 189, 38, 155, 37, 160, 36, 192, 35, 157, 33, 188, 32, 172, 31, 142, 30, 167, 29, 139, 28, 163, 27, 136, 26, 186, 25, 152, 24, 185, 23, 149, 22, 182, 21, 148, 20, 177, 19, 144, 18, 171, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 178],
	#[],
	#[77, 263, 74, 176, 73, 151, 45, 264, 33, 188, 32, 172, 31, 142, 30, 167, 29, 139, 28, 163, 27, 136, 26, 186, 25, 152, 24, 185, 23, 149, 22, 182, 21, 148, 20, 177, 19, 144, 18, 171],
	#[190, 140, 189, 183, 188, 161, 187, 146, 185, 169, 183, 262, 184, 154, 91, 164, 77, 159, 74, 176, 73, 151, 72, 135, 69, 187, 68, 184, 67, 153, 66, 181, 65, 190, 64, 174, 63, 179, 62, 147, 61, 175, 59, 194, 57, 150, 55, 166, 53, 145, 52, 173, 51, 143, 50, 168, 48, 165, 47, 137, 46, 162, 45, 193, 44, 158, 43, 191, 42, 156, 41, 170, 40, 141, 39, 189, 38, 155, 37, 160, 36, 192, 35, 157, 33, 188, 32, 172, 31, 142, 30, 167, 29, 139, 28, 163, 27, 136, 26, 186, 25, 152, 24, 185, 23, 149, 22, 182, 21, 148, 20, 177, 19, 144, 18, 171, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 178],
	#[],
	#[],
	#[],
	#[77, 259, 74, 176, 73, 151, 59, 254, 33, 188, 32, 172, 31, 142, 30, 167, 29, 139, 28, 163, 27, 136, 26, 186, 25, 152, 24, 185, 23, 149, 22, 182, 21, 148, 20, 177, 19, 144, 18, 171, 181, 260],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[54, 258],
	#[60, 375],
	#[190, 140, 189, 183, 188, 161, 187, 146, 184, 154, 183, 138, 185, 169, 182, 256, 91, 164, 68, 184, 69, 187, 59, 194, 66, 181, 74, 176, 77, 159, 50, 168, 37, 160, 41, 170, 33, 188, 46, 162, 65, 190, 57, 150, 27, 136, 38, 155, 40, 141, 25, 152, 17, 111, 55, 166, 48, 165, 67, 153, 23, 149, 20, 177, 52, 173, 42, 156, 61, 175, 32, 172, 36, 192, 31, 142, 43, 191, 16, 89, 35, 157, 39, 189, 24, 185, 53, 145, 72, 135, 26, 186, 63, 179, 12, 81, 29, 139, 44, 158, 10, 178, 62, 147, 64, 174, 18, 171, 21, 148, 73, 151, 30, 167, 47, 137, 51, 143, 13, 104, 28, 163, 22, 182, 19, 144, 45, 193, 14, 85, 15, 110],
	#[],
	#[49, 198],
	#[],
	#[],
	#[],
	#[187, 146, 185, 169, 183, 138, 182, 197, 184, 154, 189, 183, 190, 140, 188, 161, 91, 164, 77, 159, 74, 176, 69, 187, 68, 184, 73, 151, 64, 174, 63, 179, 72, 135, 61, 175, 65, 190, 57, 150, 66, 181, 67, 153, 44, 158, 42, 156, 40, 141, 41, 170, 39, 189, 59, 194, 55, 166, 53, 145, 62, 147, 38, 155, 25, 152, 47, 137, 48, 165, 36, 192, 52, 173, 35, 157, 32, 172, 16, 89, 37, 160, 24, 185, 43, 191, 46, 162, 22, 182, 27, 136, 18, 171, 29, 139, 26, 186, 45, 193, 17, 111, 19, 144, 23, 149, 33, 188, 30, 167, 51, 143, 10, 178, 20, 177, 50, 168, 21, 148, 13, 104, 15, 110, 12, 81, 28, 163, 31, 142, 14, 85],
	#[],
	#[],
	#[187, 146, 185, 169, 183, 138, 182, 196, 184, 154, 189, 183, 91, 164, 188, 161, 77, 159, 74, 176, 73, 151, 72, 135, 69, 187, 68, 184, 67, 153, 66, 181, 65, 190, 64, 174, 63, 179, 62, 147, 61, 175, 59, 194, 57, 150, 55, 166, 53, 145, 52, 173, 51, 143, 50, 168, 48, 165, 47, 137, 46, 162, 45, 193, 44, 158, 43, 191, 42, 156, 41, 170, 40, 141, 39, 189, 38, 155, 37, 160, 36, 192, 35, 157, 33, 188, 32, 172, 31, 142, 30, 167, 29, 139, 28, 163, 27, 136, 26, 186, 25, 152, 24, 185, 23, 149, 22, 182, 21, 148, 20, 177, 19, 144, 18, 171, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 178, 190, 140],
	#[],
	#[],
	#[],
	#[190, 140, 189, 183, 188, 161, 187, 146, 185, 169, 184, 154, 183, 138, 182, 195, 91, 164, 77, 159, 74, 176, 73, 151, 72, 135, 69, 187, 68, 184, 67, 153, 66, 181, 65, 190, 63, 179, 62, 147, 61, 175, 64, 174, 57, 150, 55, 166, 53, 145, 52, 173, 51, 143, 50, 168, 48, 165, 47, 137, 59, 194, 45, 193, 44, 158, 43, 191, 42, 156, 41, 170, 40, 141, 39, 189, 38, 155, 37, 160, 36, 192, 35, 157, 33, 188, 32, 172, 31, 142, 30, 167, 29, 139, 28, 163, 27, 136, 26, 186, 25, 152, 24, 185, 46, 162, 22, 182, 21, 148, 20, 177, 19, 144, 18, 171, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 178, 23, 149],
	#[60, 374],
	#[56, 373],
	#[60, 372],
	#[193, 204, 77, 203, 74, 176, 73, 151, 63, 201, 55, 200, 45, 202, 41, 199, 33, 188, 32, 172, 31, 142, 30, 167, 29, 139, 28, 163, 27, 136, 26, 186, 25, 152, 24, 185, 23, 149, 22, 182, 21, 148, 20, 177, 19, 144, 18, 171],
	#[],
	#[155, 17, 154, 64, 153, 40, 150, 52, 149, 209, 146, 37, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 130, 63, 90, 19, 89, 66, 83, 18, 81, 38, 76, 12, 75, 59, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 49, 62, 15, 61, 48, 55, 35, 53, 13, 43, 62, 42, 29, 41, 42, 36, 65, 35, 30, 34, 50, 33, 58, 29, 6, 28, 33, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 13, 41, 12, 8],
	#[],
	#[],
	#[],
	#[192, 206, 191, 205, 54, 207],
	#[],
	#[],
	#[63, 208],
	#[],
	#[56, 210],
	#[],
	#[57, 214, 55, 213, 46, 212],
	#[146, 370, 76, 12, 75, 59, 53, 13, 34, 50, 33, 58, 21, 26, 20, 54],
	#[152, 218, 151, 216, 146, 37, 154, 221, 155, 17, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 133, 217, 132, 219, 131, 226, 130, 63, 143, 225, 90, 19, 89, 66, 83, 18, 81, 38, 76, 12, 75, 59, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 49, 62, 15, 61, 48, 55, 35, 53, 13, 43, 62, 42, 29, 41, 220, 36, 65, 35, 30, 34, 50, 33, 58, 29, 6, 28, 33, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 13, 41, 12, 8],
	#[155, 17, 154, 221, 152, 218, 151, 216, 146, 37, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 133, 217, 132, 219, 131, 215, 130, 63, 90, 19, 89, 66, 83, 18, 81, 38, 76, 12, 75, 59, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 49, 62, 15, 61, 48, 55, 35, 53, 13, 43, 62, 42, 29, 41, 220, 36, 65, 35, 30, 34, 50, 33, 58, 29, 6, 28, 33, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 13, 41, 12, 8, 18, 39, 19, 11, 20, 54],
	#[58, 371],
	#[],
	#[],
	#[91, 229, 39, 189, 38, 155, 37, 160, 36, 192],
	#[40, 227],
	#[155, 17, 154, 64, 153, 40, 150, 52, 149, 222, 146, 37, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 130, 63, 90, 19, 89, 66, 83, 18, 81, 38, 76, 12, 75, 59, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 49, 62, 15, 61, 48, 55, 35, 53, 13, 43, 62, 42, 29, 41, 42, 36, 65, 35, 30, 34, 50, 33, 58, 29, 6, 28, 33, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 13, 41, 12, 8],
	#[],
	#[],
	#[155, 17, 154, 64, 153, 224, 146, 37, 138, 21, 137, 67, 136, 45, 134, 7, 130, 63, 135, 28, 90, 19, 89, 66, 83, 18, 81, 38, 76, 12, 75, 59, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 49, 62, 15, 61, 48, 55, 35, 53, 13, 43, 62, 42, 29, 41, 42, 36, 65, 35, 30, 34, 50, 33, 58, 29, 6, 28, 33, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 13, 41, 12, 8],
	#[],
	#[56, 369],
	#[],
	#[155, 17, 154, 221, 152, 218, 151, 216, 146, 37, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 133, 228, 130, 63, 90, 19, 89, 66, 83, 18, 81, 38, 76, 12, 75, 59, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 49, 62, 15, 61, 48, 55, 35, 53, 13, 43, 62, 42, 29, 41, 220, 36, 65, 35, 30, 34, 50, 33, 58, 29, 6, 28, 33, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 13, 41, 12, 8],
	#[],
	#[155, 17, 154, 64, 153, 230, 146, 37, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 130, 63, 90, 19, 89, 66, 83, 18, 76, 12, 81, 38, 69, 57, 66, 51, 65, 60, 63, 49, 75, 59, 61, 48, 35, 30, 36, 65, 25, 25, 20, 54, 67, 22, 34, 50, 68, 55, 42, 29, 55, 35, 53, 13, 28, 33, 62, 15, 41, 42, 24, 53, 18, 39, 33, 58, 21, 26, 19, 11, 43, 62, 29, 6, 23, 20, 12, 8, 13, 41, 22, 61],
	#[],
	#[],
	#[],
	#[180, 236, 181, 245, 155, 239, 179, 251, 138, 86, 134, 234, 165, 346, 125, 31, 124, 238, 121, 241, 166, 244, 118, 243, 120, 232, 92, 250, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 237, 75, 59, 72, 231, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 61, 103, 57, 240, 55, 247, 54, 106, 53, 80, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 253, 44, 246, 43, 252, 42, 242, 41, 105, 40, 233, 35, 95, 33, 58, 32, 101, 30, 36, 29, 6, 28, 33, 27, 3, 26, 56, 24, 53, 23, 20, 19, 11, 16, 89, 14, 85, 17, 111, 37, 96, 12, 81, 59, 254, 25, 25, 10, 248, 20, 54, 18, 39, 34, 50, 22, 61, 13, 104, 69, 57, 38, 90, 172, 249, 52, 102, 68, 55, 21, 26, 15, 110, 39, 113, 36, 115],
	#[],
	#[156, 366, 31, 356],
	#[],
	#[],
	#[174, 338, 173, 345, 10, 339],
	#[181, 245, 180, 236, 179, 251, 172, 343, 171, 344, 138, 86, 134, 234, 125, 31, 124, 238, 121, 241, 120, 232, 118, 243, 92, 250, 89, 66, 88, 43, 87, 27, 68, 55, 66, 51, 64, 46, 75, 59, 72, 231, 76, 237, 36, 115, 50, 100, 65, 60, 67, 22, 49, 74, 48, 98, 51, 77, 25, 25, 44, 246, 42, 242, 57, 240, 59, 254, 41, 105, 63, 109, 61, 103, 20, 54, 32, 101, 52, 102, 46, 97, 37, 96, 33, 58, 55, 247, 53, 80, 40, 233, 24, 53, 62, 82, 38, 90, 43, 252, 34, 50, 47, 71, 45, 253, 16, 89, 54, 106, 30, 36, 35, 95, 26, 56, 39, 113, 19, 11, 12, 81, 10, 248, 27, 3, 17, 111, 21, 26, 15, 110, 13, 104, 14, 85],
	#[183, 138, 184, 154, 187, 146, 185, 169, 190, 140, 189, 183, 91, 164, 188, 161, 77, 159, 73, 151, 72, 135, 69, 187, 68, 184, 182, 341, 66, 181, 65, 190, 64, 174, 74, 176, 62, 147, 61, 175, 59, 194, 57, 150, 55, 166, 67, 153, 53, 145, 52, 173, 51, 143, 50, 168, 48, 165, 47, 137, 46, 162, 45, 193, 44, 158, 43, 191, 42, 156, 41, 170, 40, 141, 39, 189, 38, 155, 37, 160, 36, 192, 35, 157, 63, 179, 33, 188, 32, 172, 31, 142, 30, 167, 29, 139, 28, 163, 27, 136, 26, 186, 25, 152, 24, 185, 23, 149, 22, 182, 21, 148, 20, 177, 19, 144, 18, 171, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 178],
	#[174, 338, 173, 337, 10, 339],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[190, 140, 189, 183, 188, 161, 187, 146, 185, 169, 184, 154, 183, 138, 182, 335, 91, 164, 77, 159, 74, 176, 73, 151, 72, 135, 69, 187, 68, 184, 67, 153, 66, 181, 65, 190, 64, 174, 63, 179, 62, 147, 61, 175, 59, 194, 57, 150, 55, 166, 53, 145, 52, 173, 51, 143, 50, 168, 48, 165, 47, 137, 46, 162, 45, 193, 44, 158, 43, 191, 42, 156, 41, 170, 40, 141, 39, 189, 38, 155, 37, 160, 36, 192, 35, 157, 33, 188, 32, 172, 31, 142, 30, 167, 29, 139, 28, 163, 27, 136, 26, 186, 25, 152, 24, 185, 23, 149, 22, 182, 21, 148, 20, 177, 19, 144, 18, 171, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 178],
	#[172, 249, 179, 251, 165, 273, 180, 236, 181, 245, 155, 239, 166, 244, 138, 86, 134, 234, 125, 31, 124, 238, 121, 241, 120, 232, 118, 243, 92, 250, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 237, 75, 59, 72, 231, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 61, 103, 59, 254, 57, 240, 55, 247, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 253, 44, 246, 43, 252, 42, 242, 41, 105, 40, 233, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 30, 36, 29, 6, 28, 33, 27, 3, 26, 56, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 248],
	#[],
	#[],
	#[181, 245, 180, 236, 179, 251, 172, 249, 166, 244, 165, 272, 155, 239, 138, 86, 134, 234, 125, 31, 124, 238, 121, 241, 120, 232, 118, 243, 92, 250, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 237, 75, 59, 72, 231, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 61, 103, 59, 254, 57, 240, 55, 247, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 253, 44, 246, 43, 252, 42, 242, 41, 105, 40, 233, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 30, 36, 29, 6, 28, 33, 27, 3, 26, 56, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 248],
	#[],
	#[],
	#[190, 140, 189, 183, 188, 161, 187, 146, 185, 169, 184, 154, 183, 138, 182, 255, 91, 164, 77, 159, 74, 176, 73, 151, 72, 135, 69, 187, 68, 184, 67, 153, 66, 181, 65, 190, 64, 174, 63, 179, 62, 147, 61, 175, 59, 194, 57, 150, 55, 166, 52, 173, 51, 143, 48, 165, 46, 162, 44, 158, 42, 156, 41, 170, 40, 141, 38, 155, 35, 157, 33, 188, 29, 139, 28, 163, 24, 185, 32, 172, 16, 89, 27, 136, 13, 104, 17, 111, 43, 191, 20, 177, 36, 192, 22, 182, 25, 152, 18, 171, 50, 168, 23, 149, 26, 186, 47, 137, 45, 193, 15, 110, 30, 167, 21, 148, 37, 160, 12, 81, 10, 178, 31, 142, 14, 85, 19, 144, 53, 145, 39, 189],
	#[60, 271],
	#[58, 257],
	#[],
	#[],
	#[181, 261, 59, 254],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[56, 266],
	#[],
	#[],
	#[58, 269],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[179, 251, 165, 364, 181, 245, 180, 236, 166, 244, 155, 239, 138, 86, 134, 234, 125, 31, 124, 238, 121, 241, 120, 232, 118, 243, 172, 249, 92, 250, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 237, 75, 59, 72, 231, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 61, 103, 59, 254, 57, 240, 55, 247, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 253, 44, 246, 43, 252, 42, 242, 41, 105, 40, 233, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 30, 36, 29, 6, 28, 33, 27, 3, 26, 56, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 248],
	#[155, 84, 138, 86, 134, 76, 125, 31, 124, 83, 121, 88, 120, 70, 119, 107, 118, 93, 117, 78, 110, 75, 104, 92, 103, 362, 92, 112, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 79, 75, 59, 72, 69, 69, 57, 68, 55, 66, 51, 65, 60, 64, 46, 63, 109, 61, 103, 59, 117, 57, 87, 55, 99, 67, 22, 53, 80, 52, 102, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 42, 91, 41, 105, 39, 113, 51, 77, 37, 96, 36, 115, 40, 73, 34, 50, 33, 58, 32, 101, 54, 106, 30, 36, 29, 6, 28, 33, 44, 94, 26, 56, 24, 53, 23, 20, 22, 61, 21, 26, 19, 11, 25, 25, 17, 111, 62, 82, 14, 85, 13, 104, 16, 89, 35, 95, 18, 39, 43, 114, 20, 54, 27, 3, 10, 108, 12, 81, 38, 90, 15, 110],
	#[],
	#[155, 84, 138, 86, 134, 76, 125, 31, 124, 83, 121, 88, 120, 70, 119, 107, 118, 93, 117, 78, 110, 75, 104, 92, 103, 281, 92, 112, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 79, 75, 59, 72, 69, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 61, 103, 59, 117, 57, 87, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 42, 91, 41, 105, 40, 73, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 30, 36, 29, 6, 28, 33, 27, 3, 26, 56, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 108, 43, 114],
	#[40, 279],
	#[146, 275, 123, 278, 122, 280, 83, 274, 81, 277, 76, 12, 75, 59, 53, 13, 34, 50, 33, 58, 29, 6, 28, 33, 23, 20, 22, 61, 21, 26, 20, 54, 18, 39],
	#[],
	#[129, 361, 31, 323],
	#[],
	#[156, 357, 31, 356],
	#[],
	#[128, 355, 126, 285, 33, 287],
	#[86, 288, 85, 295, 78, 294, 34, 297, 25, 298, 24, 291, 23, 289, 22, 296, 29, 293, 20, 290, 21, 292],
	#[],
	#[155, 304, 138, 86, 134, 76, 120, 70, 119, 107, 118, 93, 117, 302, 114, 305, 106, 301, 105, 354, 92, 112, 89, 66, 83, 18, 81, 38, 76, 79, 75, 59, 72, 69, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 109, 62, 82, 61, 103, 59, 117, 57, 87, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 43, 114, 42, 91, 41, 105, 40, 303, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 29, 6, 28, 33, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[77, 333, 74, 176, 73, 151, 33, 188, 32, 172, 31, 142, 30, 167, 29, 139, 28, 163, 27, 136, 26, 186, 25, 152, 24, 185, 23, 149, 22, 182, 21, 148, 20, 177, 19, 144, 18, 171],
	#[155, 84, 138, 86, 134, 76, 125, 31, 124, 83, 121, 88, 120, 70, 119, 107, 118, 93, 117, 78, 110, 75, 104, 92, 103, 299, 92, 112, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 79, 75, 59, 72, 69, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 61, 103, 59, 117, 57, 87, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 43, 114, 42, 91, 41, 105, 40, 73, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 30, 36, 29, 6, 28, 33, 27, 3, 26, 56, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 108],
	#[],
	#[],
	#[],
	#[129, 324, 31, 323],
	#[56, 322],
	#[],
	#[155, 304, 138, 86, 134, 76, 120, 70, 119, 107, 118, 93, 117, 302, 114, 305, 106, 301, 105, 320, 92, 112, 89, 66, 83, 18, 81, 38, 76, 79, 75, 59, 72, 69, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 109, 62, 82, 61, 103, 59, 117, 57, 87, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 43, 114, 42, 91, 41, 105, 40, 303, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 29, 6, 28, 33, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81],
	#[155, 304, 138, 86, 120, 70, 119, 107, 118, 93, 117, 302, 114, 305, 106, 301, 105, 321, 89, 66, 66, 51, 81, 38, 72, 69, 47, 71, 59, 117, 134, 76, 52, 102, 48, 98, 32, 101, 83, 18, 65, 60, 57, 87, 67, 22, 44, 94, 55, 99, 76, 79, 92, 112, 75, 59, 35, 95, 49, 74, 42, 91, 36, 115, 37, 96, 68, 55, 62, 82, 53, 80, 24, 53, 41, 105, 69, 57, 28, 33, 40, 303, 54, 106, 45, 116, 50, 100, 63, 109, 61, 103, 20, 54, 21, 26, 34, 50, 46, 97, 51, 77, 33, 58, 25, 25, 18, 39, 12, 81, 23, 20, 19, 11, 38, 90, 43, 114, 17, 111, 39, 113, 22, 61, 13, 104, 29, 6, 16, 89, 14, 85, 15, 110],
	#[138, 86, 134, 76, 120, 70, 119, 107, 118, 93, 117, 302, 114, 318, 113, 319, 92, 112, 89, 66, 76, 79, 75, 59, 72, 69, 68, 55, 67, 22, 66, 51, 65, 60, 63, 109, 62, 82, 61, 103, 59, 117, 57, 87, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 43, 114, 42, 91, 41, 105, 40, 303, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 25, 25, 24, 53, 21, 26, 20, 54, 19, 11, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81],
	#[],
	#[],
	#[58, 317],
	#[],
	#[],
	#[155, 84, 138, 86, 134, 76, 125, 31, 124, 83, 121, 88, 120, 70, 119, 107, 118, 93, 117, 78, 104, 92, 103, 311, 92, 112, 89, 66, 88, 43, 110, 75, 83, 18, 76, 79, 81, 38, 72, 69, 65, 60, 48, 98, 49, 74, 40, 73, 25, 25, 87, 27, 53, 80, 50, 100, 19, 11, 34, 50, 67, 22, 30, 36, 68, 55, 32, 101, 37, 96, 27, 3, 64, 46, 14, 85, 21, 26, 16, 89, 75, 59, 35, 95, 22, 61, 57, 87, 41, 105, 51, 77, 38, 90, 66, 51, 12, 81, 28, 33, 13, 104, 10, 108, 18, 39, 24, 53, 26, 56, 62, 82, 39, 113, 33, 58, 17, 111, 20, 54, 36, 115, 43, 114, 45, 116, 55, 99, 59, 117, 52, 102, 61, 103, 63, 109, 42, 91, 69, 57, 23, 20, 29, 6, 44, 94, 47, 71, 46, 97, 54, 106, 15, 110],
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
	#[85, 329, 83, 327, 81, 328, 79, 331, 76, 330, 75, 59, 34, 50, 33, 58, 29, 6, 28, 33, 25, 298, 23, 325, 22, 61, 21, 26, 20, 326, 18, 39],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[83, 327, 81, 328, 79, 332, 76, 330, 75, 59, 34, 50, 33, 58, 29, 6, 28, 33, 23, 20, 22, 61, 21, 26, 20, 54, 18, 39],
	#[],
	#[],
	#[],
	#[165, 334, 166, 244, 155, 239, 179, 251, 172, 249, 134, 234, 180, 236, 138, 86, 125, 31, 124, 238, 121, 241, 120, 232, 118, 243, 181, 245, 92, 250, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 237, 75, 59, 72, 231, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 61, 103, 59, 254, 57, 240, 55, 247, 54, 106, 53, 80, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 253, 44, 246, 42, 242, 41, 105, 40, 233, 39, 113, 38, 90, 37, 96, 36, 115, 34, 50, 33, 58, 32, 101, 30, 36, 29, 6, 28, 33, 27, 3, 25, 25, 24, 53, 35, 95, 21, 26, 20, 54, 19, 11, 52, 102, 17, 111, 18, 39, 26, 56, 13, 104, 15, 110, 16, 89, 10, 248, 43, 252, 22, 61, 14, 85, 12, 81, 23, 20],
	#[194, 347, 31, 348],
	#[56, 336],
	#[],
	#[],
	#[],
	#[181, 245, 180, 236, 179, 251, 172, 249, 166, 244, 165, 340, 155, 239, 138, 86, 134, 234, 125, 31, 124, 238, 121, 241, 120, 232, 118, 243, 92, 250, 89, 66, 88, 43, 87, 27, 83, 18, 81, 38, 76, 237, 75, 59, 72, 231, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 64, 46, 63, 109, 62, 82, 61, 103, 59, 254, 57, 240, 55, 247, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 253, 44, 246, 43, 252, 42, 242, 41, 105, 40, 233, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 30, 36, 29, 6, 28, 33, 27, 3, 26, 56, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 10, 248],
	#[],
	#[58, 342],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[81, 328, 79, 352, 78, 350, 75, 59, 83, 327, 76, 330, 33, 58, 28, 33, 34, 351, 20, 54, 18, 39, 29, 349, 21, 26, 22, 61, 23, 20],
	#[],
	#[81, 328, 79, 353, 28, 33, 20, 54, 21, 26, 34, 50, 23, 20, 76, 330, 18, 39, 75, 59, 83, 327, 33, 58, 29, 6, 22, 61],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[83, 358, 81, 360, 80, 359, 29, 6, 28, 33, 23, 20, 22, 61, 18, 39],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[129, 363, 31, 323],
	#[],
	#[129, 365, 31, 323],
	#[],
	#[],
	#[56, 368],
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
	#[40, 392],
	#[2, 391],
	#[155, 383, 138, 86, 134, 76, 120, 70, 119, 107, 118, 93, 117, 382, 116, 386, 108, 384, 102, 385, 92, 112, 89, 66, 83, 18, 81, 38, 76, 79, 75, 59, 72, 69, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 109, 62, 82, 61, 103, 59, 117, 57, 87, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 43, 114, 42, 91, 41, 105, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 29, 6, 28, 33, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81],
	#[155, 383, 138, 86, 134, 76, 120, 70, 119, 107, 118, 93, 117, 382, 116, 386, 108, 390, 107, 389, 92, 112, 89, 66, 83, 18, 81, 38, 76, 79, 75, 59, 72, 69, 69, 57, 68, 55, 67, 22, 66, 51, 65, 60, 63, 109, 62, 82, 61, 103, 59, 117, 57, 87, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 43, 114, 42, 91, 41, 105, 39, 113, 38, 90, 37, 96, 36, 115, 35, 95, 34, 50, 33, 58, 32, 101, 29, 6, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 18, 39, 17, 111, 16, 89, 15, 110, 14, 85, 13, 104, 12, 81, 28, 33],
	#[138, 86, 134, 76, 120, 70, 119, 107, 118, 93, 117, 382, 116, 388, 115, 387, 92, 112, 89, 66, 76, 79, 75, 59, 72, 69, 68, 55, 67, 22, 66, 51, 65, 60, 63, 109, 62, 82, 61, 103, 59, 117, 57, 87, 55, 99, 54, 106, 53, 80, 52, 102, 51, 77, 50, 100, 49, 74, 48, 98, 47, 71, 46, 97, 45, 116, 44, 94, 43, 114, 42, 91, 41, 105, 39, 113, 38, 90, 37, 96, 33, 58, 32, 101, 25, 25, 24, 53, 34, 50, 35, 95, 21, 26, 20, 54, 16, 89, 14, 85, 13, 104, 12, 81, 17, 111, 19, 11, 36, 115, 15, 110],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[],
	#[100, 393, 101, 379, 41, 381],
	#[],
	#[2, 395],
	#[],
	#[2, 397],
	#[],
	#[],
	#[48, 423],
	#[162, 429, 160, 431, 155, 17, 154, 64, 146, 37, 153, 40, 138, 21, 137, 67, 134, 7, 136, 45, 150, 52, 125, 31, 149, 32, 12, 8, 27, 3, 19, 11, 83, 18, 97, 430, 53, 13, 25, 25, 18, 39, 35, 30, 90, 19, 67, 22, 68, 55, 81, 38, 121, 24, 87, 27, 28, 33, 55, 35, 42, 29, 76, 12, 29, 6, 24, 53, 124, 16, 41, 42, 135, 28, 22, 61, 61, 48, 88, 43, 21, 26, 130, 63, 62, 15, 26, 56, 95, 420, 13, 41, 34, 50, 98, 417, 30, 36, 20, 54, 36, 65, 43, 62, 33, 58, 64, 46, 65, 60, 23, 20, 10, 432, 75, 59, 89, 66, 69, 57, 66, 51, 63, 49],
	#[40, 413],
	#[162, 418, 155, 17, 154, 64, 153, 40, 150, 52, 149, 32, 146, 37, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 130, 63, 125, 31, 124, 16, 121, 24, 98, 417, 97, 419, 95, 420, 90, 19, 88, 43, 89, 66, 81, 38, 75, 59, 83, 18, 69, 57, 67, 22, 66, 51, 65, 60, 64, 46, 62, 15, 61, 48, 76, 12, 87, 27, 55, 35, 53, 13, 68, 55, 43, 62, 35, 30, 41, 42, 33, 58, 27, 3, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 42, 29, 19, 11, 18, 39, 26, 56, 13, 41, 12, 8, 28, 33, 29, 6, 30, 36, 63, 49, 20, 54, 36, 65, 34, 50, 10, 421],
	#[155, 17, 154, 64, 153, 40, 149, 410, 146, 37, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 130, 63, 68, 55, 75, 59, 89, 66, 81, 38, 83, 18, 43, 62, 76, 12, 67, 22, 62, 15, 65, 60, 36, 65, 35, 30, 53, 13, 33, 58, 42, 29, 29, 6, 28, 33, 41, 42, 55, 35, 25, 25, 24, 53, 61, 48, 66, 51, 21, 26, 20, 54, 19, 11, 18, 39, 13, 41, 69, 57, 63, 49, 90, 19, 34, 50, 22, 61, 12, 8, 23, 20, 150, 52],
	#[164, 408, 48, 409],
	#[2, 407],
	#[],
	#[],
	#[],
	#[],
	#[56, 368, 40, 411],
	#[155, 17, 154, 64, 153, 40, 150, 52, 149, 401, 148, 412, 146, 37, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 130, 63, 90, 19, 89, 66, 83, 18, 81, 38, 76, 12, 75, 59, 69, 57, 68, 55, 66, 51, 65, 60, 63, 49, 55, 35, 67, 22, 43, 62, 61, 48, 41, 42, 62, 15, 35, 30, 53, 13, 33, 58, 42, 29, 29, 6, 28, 33, 25, 25, 24, 53, 34, 50, 22, 61, 21, 26, 20, 54, 18, 39, 13, 41, 12, 8, 23, 20, 19, 11, 36, 65],
	#[56, 415],
	#[155, 17, 154, 64, 153, 40, 150, 52, 149, 401, 148, 414, 146, 37, 138, 21, 137, 67, 136, 45, 135, 28, 134, 7, 130, 63, 89, 66, 33, 58, 43, 62, 41, 42, 25, 25, 24, 53, 75, 59, 35, 30, 42, 29, 36, 65, 68, 55, 53, 13, 13, 41, 69, 57, 29, 6, 76, 12, 18, 39, 19, 11, 83, 18, 81, 38, 20, 54, 90, 19, 34, 50, 66, 51, 22, 61, 55, 35, 67, 22, 61, 48, 21, 26, 12, 8, 28, 33, 63, 49, 23, 20, 65, 60, 62, 15],
	#[],
	#[48, 416],
	#[],
	#[],
	#[],
	#[144, 425, 10, 424],
	#[],
	#[163, 422, 155, 17, 153, 40, 149, 401, 148, 399, 146, 37, 154, 64, 138, 21, 136, 45, 130, 63, 150, 52, 137, 67, 134, 7, 135, 28, 76, 12, 81, 38, 24, 53, 35, 30, 21, 26, 62, 15, 13, 41, 69, 57, 42, 29, 19, 11, 83, 18, 61, 48, 20, 54, 34, 50, 66, 51, 41, 42, 33, 58, 55, 403, 53, 13, 65, 60, 28, 33, 32, 404, 43, 62, 25, 25, 67, 22, 36, 65, 75, 59, 68, 55, 90, 19, 12, 8, 89, 66, 63, 49, 18, 39, 29, 6, 23, 20, 22, 61],
	#[],
	#[],
	#[124, 16, 136, 45, 83, 18, 121, 24, 67, 22, 155, 17, 138, 21, 146, 37, 64, 46, 98, 428, 134, 7, 153, 40, 89, 66, 62, 15, 88, 43, 65, 60, 75, 59, 90, 19, 53, 13, 163, 427, 81, 38, 42, 29, 125, 31, 29, 6, 28, 33, 27, 3, 55, 403, 76, 12, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 19, 11, 25, 25, 32, 404, 149, 426, 26, 56, 13, 41, 12, 8, 66, 51, 35, 30, 18, 39, 41, 42, 63, 49, 61, 48, 36, 65, 34, 50, 87, 27, 33, 58, 137, 67, 148, 399, 69, 57, 43, 62, 135, 28, 150, 52, 130, 63, 30, 36, 68, 55, 154, 64],
	#[],
	#[40, 413],
	#[],
	#[],
	#[],
	#[144, 425, 10, 434],
	#[],
	#[146, 37, 161, 406, 153, 40, 163, 402, 134, 7, 159, 400, 155, 17, 138, 21, 136, 45, 154, 64, 130, 63, 137, 67, 158, 433, 149, 401, 135, 28, 150, 52, 148, 399, 83, 18, 89, 66, 81, 38, 76, 12, 75, 59, 69, 57, 90, 19, 63, 49, 62, 15, 61, 48, 55, 403, 67, 22, 68, 55, 35, 30, 53, 13, 66, 51, 32, 404, 42, 29, 65, 60, 29, 6, 28, 33, 41, 42, 25, 25, 24, 53, 23, 20, 22, 61, 21, 26, 20, 54, 43, 62, 13, 41, 12, 8, 18, 39, 19, 11, 33, 58, 36, 65, 34, 50],
	#[],
	#[149, 426, 138, 21, 153, 40, 125, 31, 137, 67, 155, 17, 148, 399, 154, 64, 124, 16, 135, 28, 150, 52, 146, 37, 136, 45, 121, 24, 90, 19, 158, 435, 87, 27, 130, 63, 76, 12, 81, 38, 159, 400, 134, 7, 55, 403, 25, 25, 22, 61, 21, 26, 42, 29, 83, 18, 32, 404, 27, 3, 13, 41, 12, 8, 28, 33, 29, 6, 35, 30, 88, 43, 163, 402, 64, 46, 20, 54, 66, 51, 33, 58, 75, 59, 53, 13, 65, 60, 23, 20, 19, 11, 41, 42, 43, 62, 34, 50, 67, 22, 98, 428, 161, 406, 30, 36, 68, 55, 26, 56, 24, 53, 89, 66, 63, 49, 18, 39, 61, 48, 69, 57, 36, 65, 62, 15],
	#[],
	#[2, 441],
	#[47, 438],
	#[55, 35, 24, 53, 83, 18, 66, 51, 25, 25, 136, 45, 12, 8, 81, 38, 18, 39, 33, 58, 137, 67, 34, 50, 67, 22, 135, 28, 53, 13, 13, 41, 138, 21, 29, 6, 146, 37, 19, 11, 43, 62, 65, 60, 61, 48, 62, 15, 63, 49, 155, 17, 75, 59, 28, 33, 89, 66, 76, 12, 23, 20, 20, 54, 22, 61, 68, 55, 21, 26, 42, 29, 147, 440, 69, 57, 130, 439, 134, 7],
	#[55, 213, 57, 214, 46, 212],
	#[],
	#[],
	#[2, 443],
	#[],
	#[144, 425, 10, 448],
	#[2, 447],
	#[],
	#[],
	#[155, 17, 153, 40, 149, 32, 146, 37, 154, 64, 138, 21, 136, 45, 125, 31, 130, 63, 150, 52, 137, 67, 55, 35, 121, 24, 64, 46, 21, 26, 124, 16, 67, 22, 27, 3, 25, 25, 35, 30, 19, 11, 83, 18, 62, 15, 33, 58, 88, 43, 90, 19, 12, 8, 28, 33, 98, 428, 24, 53, 41, 42, 30, 36, 61, 48, 18, 39, 81, 38, 75, 59, 34, 50, 13, 41, 29, 6, 76, 12, 42, 29, 23, 20, 20, 54, 135, 28, 66, 51, 65, 60, 26, 56, 63, 49, 36, 65, 53, 13, 43, 62, 89, 66, 22, 61, 134, 7, 68, 55, 69, 57, 87, 27],
	#[2, 450],
	#[],
	#[],
	#[]],
  action-function-table:
	 vector(infix-dylan-parser-action0,
		infix-dylan-parser-action1,
		infix-dylan-parser-action1,
		infix-dylan-parser-action1,
		infix-dylan-parser-action1,
		infix-dylan-parser-action1,
		infix-dylan-parser-action1,
		infix-dylan-parser-action1,
		infix-dylan-parser-action1,
		infix-dylan-parser-action9,
		infix-dylan-parser-action9,
		infix-dylan-parser-action11,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action46,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action46,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action46,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action80,
		infix-dylan-parser-action0,
		infix-dylan-parser-action82,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action85,
		infix-dylan-parser-action86,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action93,
		infix-dylan-parser-action94,
		infix-dylan-parser-action0,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action94,
		infix-dylan-parser-action94,
		infix-dylan-parser-action94,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action146,
		infix-dylan-parser-action147,
		infix-dylan-parser-action148,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action154,
		infix-dylan-parser-action154,
		infix-dylan-parser-action0,
		infix-dylan-parser-action157,
		infix-dylan-parser-action158,
		infix-dylan-parser-action159,
		infix-dylan-parser-action160,
		infix-dylan-parser-action161,
		infix-dylan-parser-action0,
		infix-dylan-parser-action163,
		infix-dylan-parser-action164,
		infix-dylan-parser-action165,
		infix-dylan-parser-action0,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action157,
		infix-dylan-parser-action94,
		infix-dylan-parser-action171,
		infix-dylan-parser-action172,
		infix-dylan-parser-action173,
		infix-dylan-parser-action174,
		infix-dylan-parser-action175,
		infix-dylan-parser-action176,
		infix-dylan-parser-action0,
		infix-dylan-parser-action178,
		infix-dylan-parser-action85,
		infix-dylan-parser-action86,
		infix-dylan-parser-action181,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action184,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action1,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action199,
		infix-dylan-parser-action200,
		infix-dylan-parser-action201,
		infix-dylan-parser-action202,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action178,
		infix-dylan-parser-action85,
		infix-dylan-parser-action86,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action46,
		infix-dylan-parser-action0,
		infix-dylan-parser-action157,
		infix-dylan-parser-action217,
		infix-dylan-parser-action0,
		infix-dylan-parser-action219,
		infix-dylan-parser-action0,
		infix-dylan-parser-action157,
		infix-dylan-parser-action158,
		infix-dylan-parser-action223,
		infix-dylan-parser-action224,
		infix-dylan-parser-action225,
		infix-dylan-parser-action223,
		infix-dylan-parser-action0,
		infix-dylan-parser-action225,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action232,
		infix-dylan-parser-action233,
		infix-dylan-parser-action234,
		infix-dylan-parser-action235,
		infix-dylan-parser-action236,
		infix-dylan-parser-action9,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action240,
		infix-dylan-parser-action0,
		infix-dylan-parser-action242,
		infix-dylan-parser-action243,
		infix-dylan-parser-action94,
		infix-dylan-parser-action245,
		infix-dylan-parser-action0,
		infix-dylan-parser-action240,
		infix-dylan-parser-action157,
		infix-dylan-parser-action157,
		infix-dylan-parser-action83,
		infix-dylan-parser-action251,
		infix-dylan-parser-action252,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action157,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action94,
		infix-dylan-parser-action94,
		infix-dylan-parser-action94,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action94,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action94,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action146,
		infix-dylan-parser-action147,
		infix-dylan-parser-action0,
		infix-dylan-parser-action148,
		infix-dylan-parser-action83,
		infix-dylan-parser-action0,
		infix-dylan-parser-action157,
		infix-dylan-parser-action94,
		infix-dylan-parser-action0,
		infix-dylan-parser-action240,
		infix-dylan-parser-action306,
		infix-dylan-parser-action306,
		infix-dylan-parser-action306,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action146,
		infix-dylan-parser-action147,
		infix-dylan-parser-action148,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action46,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action336,
		infix-dylan-parser-action337,
		infix-dylan-parser-action337,
		infix-dylan-parser-action0,
		infix-dylan-parser-action340,
		infix-dylan-parser-action341,
		infix-dylan-parser-action342,
		infix-dylan-parser-action46,
		infix-dylan-parser-action0,
		infix-dylan-parser-action9,
		infix-dylan-parser-action46,
		infix-dylan-parser-action0,
		infix-dylan-parser-action348,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action0,
		infix-dylan-parser-action353,
		infix-dylan-parser-action171,
		infix-dylan-parser-action172,
		infix-dylan-parser-action173),
  action-nargs-table: #[1, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 2, 0, 1, 1, 3, 1, 1, 1, 0, 1, 1, 3, 2, 1, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 2, 2, 2, 2, 2, 0, 1, 2, 0, 1, 2, 2, 0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1, 1, 1, 1, 1, 2, 2, 1, 1, 3, 3, 3, 3, 1, 5, 4, 6, 1, 0, 1, 1, 2, 1, 2, 3, 4, 4, 3, 1, 1, 1, 3, 2, 1, 1, 4, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 5, 3, 3, 1, 1, 0, 1, 1, 1, 3, 1, 1, 0, 1, 0, 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 1, 2, 3, 3, 3, 2, 2, 0, 1, 2, 1, 3, 4, 2, 3, 1, 2, 1, 1, 0, 2, 6, 2, 0, 1, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 2, 2, 2, 2, 2, 0, 1, 2, 0, 1, 2, 2, 0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 3, 0, 1, 1, 2, 1, 2, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 4, 2, 2, 1, 2, 3, 2, 0, 1, 2, 0, 1, 2, 1, 1, 1, 1, 3, 1, 2, 3],
  action-nt-table: #[70, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 72, 72, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 74, 74, 74, 75, 75, 76, 76, 76, 77, 77, 78, 78, 79, 79, 79, 80, 80, 81, 81, 81, 82, 82, 83, 83, 84, 84, 85, 85, 85, 86, 86, 86, 87, 88, 89, 89, 89, 90, 90, 91, 91, 91, 91, 92, 92, 92, 92, 92, 93, 94, 94, 95, 96, 96, 97, 97, 98, 98, 98, 99, 99, 100, 100, 101, 102, 103, 103, 104, 104, 105, 105, 106, 106, 107, 107, 108, 108, 109, 109, 110, 110, 110, 110, 110, 111, 111, 112, 113, 113, 114, 114, 115, 115, 116, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 119, 119, 119, 120, 120, 120, 120, 120, 121, 121, 121, 122, 122, 123, 123, 123, 124, 125, 125, 125, 126, 127, 127, 128, 128, 129, 129, 129, 130, 130, 130, 130, 131, 132, 132, 133, 133, 133, 134, 135, 135, 135, 135, 135, 135, 135, 136, 136, 136, 136, 136, 136, 137, 137, 138, 138, 138, 138, 138, 139, 139, 140, 141, 141, 142, 142, 143, 143, 144, 144, 145, 145, 146, 146, 147, 148, 148, 149, 150, 150, 151, 152, 152, 153, 153, 154, 154, 155, 155, 155, 156, 156, 157, 157, 158, 158, 159, 159, 160, 160, 160, 161, 162, 162, 162, 163, 163, 163, 164, 164, 165, 165, 166, 166, 167, 167, 168, 168, 169, 169, 170, 170, 171, 171, 172, 172, 172, 172, 172, 173, 173, 174, 175, 175, 176, 176, 177, 177, 178, 179, 179, 179, 179, 179, 179, 179, 179, 179, 179, 179, 180, 180, 180, 181, 182, 182, 183, 183, 183, 183, 184, 184, 184, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 185, 186, 186, 187, 187, 187, 188, 188, 188, 188, 188, 188, 188, 189, 189, 190, 191, 191, 192, 193, 193, 193, 193, 193, 194, 194, 194]);

// eof
