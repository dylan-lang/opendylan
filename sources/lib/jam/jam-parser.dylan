Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $EOF-token = 0;
define constant $%LOCAL-token = 1;
define constant $%SEMIC-token = 2;
define constant $%EQUALS-token = 3;
define constant $%LBRACE-token = 4;
define constant $%RBRACE-token = 5;
define constant $%INCLUDE-token = 6;
define constant $%ON-token = 7;
define constant $%BREAK-token = 8;
define constant $%CONTINUE-token = 9;
define constant $%RETURN-token = 10;
define constant $%FOR-token = 11;
define constant $%ARG-token = 12;
define constant $%IN-token = 13;
define constant $%SWITCH-token = 14;
define constant $%IF-token = 15;
define constant $%ELSE-token = 16;
define constant $%WHILE-token = 17;
define constant $%RULE-token = 18;
define constant $%ACTIONS-token = 19;
define constant $%STRING-token = 20;
define constant $%PLUS-EQUALS-token = 21;
define constant $%QUESTION-EQUALS-token = 22;
define constant $%DEFAULT-token = 23;
define constant $%BAR-token = 24;
define constant $%BARBAR-token = 25;
define constant $%AMPER-token = 26;
define constant $%AMPERAMPER-token = 27;
define constant $%BANG-EQUALS-token = 28;
define constant $%LANGLE-token = 29;
define constant $%LANGLE-EQUALS-token = 30;
define constant $%RANGLE-token = 31;
define constant $%RANGLE-EQUALS-token = 32;
define constant $%BANG-token = 33;
define constant $%LPAREN-token = 34;
define constant $%RPAREN-token = 35;
define constant $%CASE-token = 36;
define constant $%COLON-token = 37;
define constant $%LBRACKET-token = 38;
define constant $%RBRACKET-token = 39;
define constant $%UPDATED-token = 40;
define constant $%TOGETHER-token = 41;
define constant $%IGNORE-token = 42;
define constant $%QUIETLY-token = 43;
define constant $%PIECEMEAL-token = 44;
define constant $%EXISTING-token = 45;
define constant $%MAXLINE-token = 46;
define constant $%BIND-token = 47;

define function jam-parser-action0 (arg$1, arg$2) => (value)
  #f
end jam-parser-action0;

define function jam-parser-action1 () => (value)
  #f
end jam-parser-action1;

define function jam-parser-action2 (arg$1) => (value)
  *jam-toplevel-statements* := arg$1
end jam-parser-action2;

define function jam-parser-action3 () => (value)
  #()
end jam-parser-action3;

define function jam-parser-action4 (arg$1) => (value)
  arg$1
end jam-parser-action4;

define function jam-parser-action5 (arg$1) => (value)
  list(arg$1)
end jam-parser-action5;

define function jam-parser-action6 (arg$1, arg$2) => (value)
  pair(arg$1, arg$2)
end jam-parser-action6;

define function jam-parser-action7 (arg$1, arg$2, arg$3, arg$4) => (value)
  list(make(<jam-block-statement>,
                 local-vars: arg$2, statements: arg$4))
end jam-parser-action7;

define function jam-parser-action8 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6) => (value)
  list(make(<jam-block-statement>,
                 local-vars: arg$2, local-values: arg$4, statements: arg$6))
end jam-parser-action8;

define function jam-parser-action9 (arg$1, arg$2, arg$3) => (value)
  make(<jam-block-statement>, statements: arg$2)
end jam-parser-action9;

define function jam-parser-action10 (arg$1, arg$2, arg$3) => (value)
  make(<jam-include-statement>, list: arg$2)
end jam-parser-action10;

define function jam-parser-action11 (arg$1, arg$2, arg$3) => (value)
  make(<jam-invocation-statement>, rulename: arg$1, fields: arg$2)
end jam-parser-action11;

define function jam-parser-action12 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<jam-assignment-statement>,
            variable: arg$1, kind: arg$2, values: arg$3)
end jam-parser-action12;

define function jam-parser-action13 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6) => (value)
  make(<jam-on-assignment-statement>,
            variable: arg$1, targets: arg$3, kind: arg$4, values: arg$5)
end jam-parser-action13;

define function jam-parser-action14 (arg$1, arg$2, arg$3) => (value)
  make(<jam-break-statement>, values: arg$2)
end jam-parser-action14;

define function jam-parser-action15 (arg$1, arg$2, arg$3) => (value)
  make(<jam-continue-statement>, values: arg$2)
end jam-parser-action15;

define function jam-parser-action16 (arg$1, arg$2, arg$3) => (value)
  make(<jam-return-statement>, values: arg$2)
end jam-parser-action16;

define function jam-parser-action17 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6, arg$7) => (value)
  make(<jam-for-statement>,
            var: arg$2, values: arg$4, statements: arg$6)
end jam-parser-action17;

define function jam-parser-action18 (arg$1, arg$2, arg$3, arg$4, arg$5) => (value)
  make(<jam-switch-statement>, values: arg$2, cases: arg$4)
end jam-parser-action18;

define function jam-parser-action19 (arg$1, arg$2, arg$3, arg$4, arg$5) => (value)
  make(<jam-if-statement>, condition: arg$2, statements: arg$4)
end jam-parser-action19;

define function jam-parser-action20 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6, arg$7) => (value)
  make(<jam-if-statement>,
            condition: arg$2, statements: arg$4, else: arg$7)
end jam-parser-action20;

define function jam-parser-action21 (arg$1, arg$2, arg$3, arg$4, arg$5) => (value)
  make(<jam-while-statement>, condition: arg$2, statements: arg$4)
end jam-parser-action21;

define function jam-parser-action22 (arg$1, arg$2, arg$3) => (value)
  make(<jam-on-statement>, targets: arg$2, statement: arg$3)
end jam-parser-action22;

define function jam-parser-action23 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6) => (value)
  make(<jam-ruledef-statement>,
            name: arg$2, params: arg$3, statements: arg$5)
end jam-parser-action23;

define function jam-parser-action24 (arg$1, arg$2, arg$3, arg$4, arg$5, arg$6, arg$7, arg$8, arg$9) => (value)
  make(<jam-actiondef-statement>,
            name: arg$3, flags: arg$2, bindlist: arg$4, commands: arg$7)
end jam-parser-action24;

define function jam-parser-action25 () => (value)
  *jam-input-state*.jam-input-mode := #"string"
end jam-parser-action25;

define function jam-parser-action26 () => (value)
  *jam-input-state*.jam-input-mode := #"normal"
end jam-parser-action26;

define function jam-parser-action27 (arg$1) => (value)
  #"="
end jam-parser-action27;

define function jam-parser-action28 (arg$1) => (value)
  #"+="
end jam-parser-action28;

define function jam-parser-action29 (arg$1) => (value)
  #"?="
end jam-parser-action29;

define function jam-parser-action30 (arg$1, arg$2) => (value)
  #"?="
end jam-parser-action30;

define function jam-parser-action32 (arg$1, arg$2, arg$3) => (value)
  make(<jam-or-expression>, left: arg$1, right: arg$3)
end jam-parser-action32;

define function jam-parser-action35 (arg$1, arg$2, arg$3) => (value)
  make(<jam-and-expression>, left: arg$1, right: arg$3)
end jam-parser-action35;

define function jam-parser-action38 (arg$1, arg$2, arg$3) => (value)
  make(<jam-eq-expression>, left: arg$1, right: arg$3)
end jam-parser-action38;

define function jam-parser-action39 (arg$1, arg$2, arg$3) => (value)
  make(<jam-ne-expression>, left: arg$1, right: arg$3)
end jam-parser-action39;

define function jam-parser-action41 (arg$1, arg$2, arg$3) => (value)
  make(<jam-lt-expression>, left: arg$1, right: arg$3)
end jam-parser-action41;

define function jam-parser-action42 (arg$1, arg$2, arg$3) => (value)
  make(<jam-le-expression>, left: arg$1, right: arg$3)
end jam-parser-action42;

define function jam-parser-action43 (arg$1, arg$2, arg$3) => (value)
  make(<jam-gt-expression>, left: arg$1, right: arg$3)
end jam-parser-action43;

define function jam-parser-action44 (arg$1, arg$2, arg$3) => (value)
  make(<jam-ge-expression>, left: arg$1, right: arg$3)
end jam-parser-action44;

define function jam-parser-action46 (arg$1, arg$2) => (value)
  make(<jam-not-expression>, left: arg$2)
end jam-parser-action46;

define function jam-parser-action47 (arg$1, arg$2, arg$3) => (value)
  make(<jam-leaf-expression>, argument: arg$1, list: arg$3)
end jam-parser-action47;

define function jam-parser-action48 (arg$1) => (value)
  make(<jam-leaf-expression>, argument: arg$1)
end jam-parser-action48;

define function jam-parser-action49 (arg$1, arg$2, arg$3) => (value)
  arg$2
end jam-parser-action49;

define function jam-parser-action50 () => (value)
  make(<stretchy-vector>)
end jam-parser-action50;

define function jam-parser-action51 (arg$1, arg$2) => (value)
  add!(arg$1, arg$2)
end jam-parser-action51;

define function jam-parser-action52 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<jam-case>,
            pattern: arg$2,
            match-function: glob-match-function(arg$2),
            statements: arg$4)
end jam-parser-action52;

define function jam-parser-action54 (arg$1, arg$2, arg$3) => (value)
  pair(arg$1, arg$3)
end jam-parser-action54;

define function jam-parser-action58 (arg$1) => (value)
  begin
         *jam-input-state*.jam-input-mode := #"normal";
         arg$1;
       end
end jam-parser-action58;

define function jam-parser-action59 () => (value)
  begin
       *jam-input-state*.jam-input-mode := #"punctuation";
       make(<stretchy-vector>);
     end
end jam-parser-action59;

define function jam-parser-action62 (arg$1, arg$2, arg$3, arg$4) => (value)
  arg$3
end jam-parser-action62;

define function jam-parser-action63 (arg$1, arg$2) => (value)
  make(<jam-invocation-statement>, rulename: arg$1, fields: arg$2)
end jam-parser-action63;

define function jam-parser-action64 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<jam-on-statement>,
            targets: arg$2,
            statement: make(<jam-invocation-statement>,
                            rulename: arg$3, fields: arg$4))
end jam-parser-action64;

define function jam-parser-action65 (arg$1, arg$2, arg$3, arg$4) => (value)
  make(<jam-on-statement>,
            targets: arg$2,
            statement: make(<jam-return-statement>, values: arg$4))
end jam-parser-action65;

define function jam-parser-action67 (arg$1, arg$2) => (value)
  pair(updated?:, pair(#t, arg$1))
end jam-parser-action67;

define function jam-parser-action68 (arg$1, arg$2) => (value)
  pair(together?:, pair(#t, arg$1))
end jam-parser-action68;

define function jam-parser-action69 (arg$1, arg$2) => (value)
  pair(ignore?:, pair(#t, arg$1))
end jam-parser-action69;

define function jam-parser-action70 (arg$1, arg$2) => (value)
  pair(quietly?:, pair(#t, arg$1))
end jam-parser-action70;

define function jam-parser-action71 (arg$1, arg$2) => (value)
  pair(piecemeal?:, pair(#t, arg$1))
end jam-parser-action71;

define function jam-parser-action72 (arg$1, arg$2) => (value)
  pair(existing?:, pair(#t, arg$1))
end jam-parser-action72;

define function jam-parser-action73 (arg$1, arg$2, arg$3) => (value)
  pair(maxline:, pair(arg$3, arg$1))
end jam-parser-action73;

define function jam-parser-action75 (arg$1, arg$2) => (value)
  arg$2
end jam-parser-action75;

define constant jam-parser :: <parser>
  = make(<parser>,
  action-table:
      #[#[1, -14, 9, -7, 11, -9, 12, -18, 14, -2, 15, -12, 19, -13, 38, -10, 7, -5, 0, 1, 17, -6, 6, -20, 8, -15, 4, -4, 18, -17, 10, -16],
	#[65535, 59, 4, 59, 12, 59, 38, 59],
	#[2, 59, 3, -126, 7, -125, 12, 59, 21, -127, 22, -131, 23, -129, 37, 59, 38, 59],
	#[38, -10, 7, -5, 6, -20, 5, 3, 4, -4, 1, -14, 12, -18, 18, -17, 10, -16, 8, -15, 9, -7, 17, -6, 11, -9, 14, -2, 15, -12, 19, -13],
	#[12, -18, 38, -10],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[65535, 59, 12, 59, 38, 59, 2, 59],
	#[65535, 2, 0, 2],
	#[12, -111],
	#[65535, 26, 7, 26, 12, 26, 38, 26],
	#[0, 5, 1, -14, 4, -4, 5, 5, 6, -20, 7, -5, 8, -15, 9, -7, 10, -16, 11, -9, 12, -18, 14, -2, 15, -12, 17, -6, 18, -17, 19, -13, 36, 5, 38, -10],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[65535, 66, 12, 66, 40, 66, 41, 66, 42, 66, 43, 66, 44, 66, 45, 66, 46, 66],
	#[65535, 59, 2, 59, 3, 59, 12, 59, 38, 59],
	#[65535, 59, 2, 59, 12, 59, 38, 59],
	#[65535, 59, 2, 59, 12, 59, 38, 59],
	#[12, -40],
	#[65535, 61, 7, 61, 2, 61, 3, 61, 4, 61, 6, 61, 18, 61, 8, 61, 9, 61, 10, 61, 11, 61, 12, 61, 13, 61, 14, 61, 15, 61, 17, 61, 37, 61, 19, 61, 21, 61, 22, 61, 23, 61, 24, 61, 25, 61, 26, 61, 27, 61, 28, 61, 29, 61, 30, 61, 31, 61, 32, 61, 35, 61, 38, 61, 39, 61],
	#[0, -39],
	#[65535, 59, 2, 59, 12, 59, 38, 59],
	#[31, 58, 22, 58, 2, 58, 3, 58, 4, 58, 30, 58, 35, 58, 39, 58, 12, -18, 27, 58, 38, -10, 37, 58, 21, 58, 23, 58, 24, 58, 25, 58, 26, 58, 32, 58, 28, 58, 29, 58],
	#[2, -23],
	#[65535, 10, 1, 10, 11, 10, 10, 10, 0, 10, 9, 10, 4, 10, 5, 10, 6, 10, 7, 10, 8, 10, 12, 10, 14, 10, 15, 10, 17, 10, 18, 10, 19, 10, 36, 10, 38, 10],
	#[65535, 60, 22, 60, 2, 60, 12, 60, 31, 60, 3, 60, 4, 60, 30, 60, 35, 60, 32, 60, 27, 60, 38, 60, 37, 60, 21, 60, 23, 60, 24, 60, 25, 60, 26, 60, 28, 60, 29, 60, 39, 60],
	#[7, -27, 12, -18, 38, -10],
	#[65535, 59, 12, 59, 37, 59, 38, 59, 39, 59],
	#[12, -18, 38, -10],
	#[39, -29],
	#[65535, 62, 22, 62, 2, 62, 3, 62, 4, 62, 6, 62, 7, 62, 8, 62, 9, 62, 10, 62, 11, 62, 12, 62, 13, 62, 14, 62, 15, 62, 17, 62, 18, 62, 19, 62, 21, 62, 23, 62, 24, 62, 25, 62, 26, 62, 27, 62, 28, 62, 29, 62, 30, 62, 31, 62, 32, 62, 35, 62, 37, 62, 38, 62, 39, 62],
	#[10, -32, 12, -18, 38, -10],
	#[65535, 59, 12, 59, 37, 59, 38, 59, 39, 59],
	#[65535, 59, 39, 59, 12, 59, 38, 59],
	#[65535, 65, 39, 65],
	#[2, 56, 37, -36, 39, 56],
	#[65535, 64, 39, 64],
	#[65535, 59, 2, 59, 12, 59, 37, 59, 38, 59, 39, 59],
	#[65535, 57, 2, 57, 39, 57],
	#[65535, 63, 39, 63],
	#[#"eoi", #"accept"],
	#[4, 53, 12, -42],
	#[4, -45],
	#[4, 55, 37, -43],
	#[4, 53, 12, -42],
	#[65535, 54, 4, 54],
	#[1, -14, 4, -4, 5, 3, 6, -20, 7, -5, 8, -15, 9, -7, 10, -16, 11, -9, 12, -18, 14, -2, 15, -12, 17, -6, 18, -17, 19, -13, 38, -10],
	#[65535, 4, 0, 4, 5, 4, 36, 4],
	#[5, -157],
	#[29, 48, 3, 48, 4, 48, 13, -68, 27, 48, 25, 48, 35, 48, 24, 48, 26, 48, 32, 48, 28, 48, 30, 48, 31, 48],
	#[65535, 40, 3, 40, 4, 40, 24, 40, 25, 40, 26, 40, 27, 40, 28, 40, 29, 40, 30, 40, 31, 40, 32, 40, 35, 40],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[4, -82, 24, -79, 25, -77],
	#[65535, 45, 3, 45, 4, 45, 24, 45, 25, 45, 26, 45, 27, 45, 28, 45, 29, 45, 30, 45, 31, 45, 32, 45, 35, 45],
	#[4, 31, 24, 31, 25, 31, 26, -72, 27, -73, 35, 31],
	#[3, 37, 4, 37, 27, 37, 24, 37, 25, 37, 26, 37, 32, -63, 28, 37, 29, -61, 30, -60, 31, -62, 35, 37],
	#[4, 34, 3, -57, 24, 34, 25, 34, 26, 34, 27, 34, 28, -58, 35, 34],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[3, 39, 4, 39, 24, 39, 25, 39, 26, 39, 27, 39, 28, 39, 29, -61, 30, -60, 31, -62, 32, -63, 35, 39],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[65535, 44, 4, 44, 3, 44, 24, 44, 25, 44, 26, 44, 27, 44, 28, 44, 29, 44, 30, 44, 31, 44, 32, 44, 35, 44],
	#[65535, 46, 3, 46, 4, 46, 24, 46, 25, 46, 26, 46, 27, 46, 28, 46, 29, 46, 30, 46, 31, 46, 32, 46, 35, 46],
	#[24, -79, 25, -77, 35, -78],
	#[65535, 43, 3, 43, 4, 43, 24, 43, 25, 43, 26, 43, 27, 43, 28, 43, 29, 43, 30, 43, 31, 43, 32, 43, 35, 43],
	#[65535, 59, 31, 59, 3, 59, 4, 59, 12, 59, 27, 59, 24, 59, 25, 59, 26, 59, 32, 59, 28, 59, 29, 59, 30, 59, 35, 59, 38, 59],
	#[65535, 47, 3, 47, 4, 47, 24, 47, 25, 47, 26, 47, 27, 47, 28, 47, 29, 47, 30, 47, 31, 47, 32, 47, 35, 47],
	#[65535, 41, 31, 41, 3, 41, 4, 41, 27, 41, 24, 41, 25, 41, 26, 41, 32, 41, 28, 41, 29, 41, 30, 41, 35, 41],
	#[65535, 42, 30, 42, 31, 42, 3, 42, 4, 42, 27, 42, 24, 42, 25, 42, 26, 42, 32, 42, 28, 42, 29, 42, 35, 42],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[3, -57, 4, 36, 24, 36, 25, 36, 26, 36, 27, 36, 28, -58, 35, 36],
	#[3, 38, 4, 38, 24, 38, 25, 38, 26, 38, 27, 38, 28, 38, 29, -61, 30, -60, 31, -62, 32, -63, 35, 38],
	#[3, -57, 4, 35, 24, 35, 25, 35, 26, 35, 27, 35, 28, -58, 35, 35],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[65535, 49, 31, 49, 3, 49, 4, 49, 27, 49, 24, 49, 25, 49, 26, 49, 32, 49, 28, 49, 29, 49, 30, 49, 35, 49],
	#[12, -18, 33, -51, 34, -50, 38, -10],
	#[4, 32, 24, 32, 25, 32, 26, -72, 27, -73, 35, 32],
	#[4, 33, 24, 33, 25, 33, 26, -72, 27, -73, 35, 33],
	#[1, -14, 4, -4, 5, 3, 6, -20, 7, -5, 8, -15, 9, -7, 10, -16, 11, -9, 12, -18, 14, -2, 15, -12, 17, -6, 18, -17, 19, -13, 38, -10],
	#[5, -154],
	#[2, -85],
	#[65535, 16, 6, 16, 15, 16, 0, 16, 1, 16, 4, 16, 5, 16, 19, 16, 7, 16, 8, 16, 9, 16, 10, 16, 11, 16, 12, 16, 14, 16, 38, 16, 17, 16, 18, 16, 36, 16],
	#[2, -87],
	#[65535, 14, 10, 14, 0, 14, 1, 14, 8, 14, 4, 14, 5, 14, 6, 14, 7, 14, 9, 14, 11, 14, 12, 14, 14, 14, 15, 14, 17, 14, 18, 14, 19, 14, 36, 14, 38, 14],
	#[2, -90, 3, -89],
	#[65535, 59, 2, 59, 12, 59, 38, 59],
	#[11, -9, 14, -2, 15, -12, 17, -6, 18, -17, 19, -13, 36, 3, 38, -10, 12, -18, 0, 3, 1, -14, 4, -4, 5, 3, 6, -20, 7, -5, 8, -15, 9, -7, 10, -16],
	#[65535, 7, 0, 7, 5, 7, 36, 7],
	#[12, -99, 40, -93, 41, -95, 42, -96, 43, -100, 44, -98, 45, -94, 46, -97],
	#[65535, 67, 41, 67, 44, 67, 12, 67, 45, 67, 40, 67, 46, 67, 43, 67, 42, 67],
	#[65535, 72, 12, 72, 43, 72, 40, 72, 41, 72, 42, 72, 44, 72, 45, 72, 46, 72],
	#[65535, 68, 12, 68, 46, 68, 45, 68, 44, 68, 42, 68, 41, 68, 40, 68, 43, 68],
	#[65535, 69, 12, 69, 40, 69, 41, 69, 42, 69, 43, 69, 44, 69, 45, 69, 46, 69],
	#[12, -109],
	#[65535, 71, 12, 71, 40, 71, 41, 71, 42, 71, 43, 71, 44, 71, 45, 71, 46, 71],
	#[4, 74, 47, -102],
	#[65535, 70, 12, 70, 40, 70, 41, 70, 42, 70, 43, 70, 44, 70, 45, 70, 46, 70],
	#[4, -104],
	#[65535, 59, 4, 59, 12, 59, 38, 59],
	#[65535, 75, 4, 75],
	#[65535, 25, 20, 25],
	#[20, -106],
	#[65535, 26, 5, 26],
	#[5, -108],
	#[65535, 24, 11, 24, 0, 24, 1, 24, 4, 24, 5, 24, 6, 24, 7, 24, 8, 24, 9, 24, 10, 24, 12, 24, 14, 24, 15, 24, 17, 24, 18, 24, 19, 24, 36, 24, 38, 24],
	#[65535, 73, 12, 73, 40, 73, 41, 73, 42, 73, 43, 73, 44, 73, 45, 73, 46, 73],
	#[65535, 6, 0, 6, 5, 6, 36, 6],
	#[13, -112],
	#[65535, 59, 4, 59, 12, 59, 38, 59],
	#[4, -114],
	#[18, -17, 1, -14, 4, -4, 5, 3, 6, -20, 7, -5, 8, -15, 9, -7, 10, -16, 11, -9, 12, -18, 14, -2, 15, -12, 17, -6, 19, -13, 38, -10],
	#[5, -116],
	#[65535, 17, 0, 17, 1, 17, 4, 17, 5, 17, 6, 17, 7, 17, 8, 17, 9, 17, 10, 17, 11, 17, 12, 17, 14, 17, 15, 17, 17, 17, 18, 17, 19, 17, 36, 17, 38, 17],
	#[2, -118],
	#[65535, 15, 7, 15, 6, 15, 15, 15, 14, 15, 38, 15, 10, 15, 0, 15, 1, 15, 36, 15, 17, 15, 4, 15, 5, 15, 19, 15, 18, 15, 8, 15, 9, 15, 11, 15, 12, 15],
	#[4, -120, 24, -79, 25, -77],
	#[1, -14, 4, -4, 5, 3, 6, -20, 7, -5, 8, -15, 9, -7, 10, -16, 11, -9, 12, -18, 14, -2, 15, -12, 17, -6, 18, -17, 19, -13, 38, -10],
	#[5, -122],
	#[65535, 21, 0, 21, 1, 21, 4, 21, 5, 21, 6, 21, 7, 21, 8, 21, 9, 21, 10, 21, 11, 21, 12, 21, 14, 21, 15, 21, 17, 21, 18, 21, 19, 21, 36, 21, 38, 21],
	#[15, -12, 4, -4, 6, -20, 7, -5, 8, -15, 9, -7, 10, -16, 11, -9, 12, -18, 14, -2, 38, -10, 17, -6, 18, -17, 19, -13],
	#[65535, 22, 10, 22, 0, 22, 1, 22, 4, 22, 5, 22, 6, 22, 7, 22, 8, 22, 9, 22, 11, 22, 12, 22, 14, 22, 15, 22, 17, 22, 18, 22, 19, 22, 36, 22, 38, 22],
	#[65535, 59, 23, 59, 3, 59, 12, 59, 38, 59, 21, 59, 22, 59],
	#[65535, 27, 2, 27, 12, 27, 38, 27],
	#[65535, 28, 2, 28, 12, 28, 38, 28],
	#[65535, 59, 2, 59, 12, 59, 38, 59],
	#[3, -133],
	#[2, -132],
	#[65535, 29, 2, 29, 12, 29, 38, 29],
	#[65535, 11, 15, 11, 38, 11, 19, 11, 0, 11, 1, 11, 4, 11, 5, 11, 6, 11, 7, 11, 8, 11, 9, 11, 10, 11, 11, 11, 12, 11, 14, 11, 17, 11, 18, 11, 36, 11],
	#[65535, 30, 2, 30, 12, 30, 38, 30],
	#[2, -135],
	#[65535, 12, 15, 12, 7, 12, 6, 12, 14, 12, 38, 12, 19, 12, 12, 12, 18, 12, 11, 12, 10, 12, 0, 12, 1, 12, 9, 12, 8, 12, 4, 12, 5, 12, 17, 12, 36, 12],
	#[21, -127, 3, -126, 22, -131, 23, -129],
	#[65535, 59, 2, 59, 38, 59, 12, 59],
	#[2, -139],
	#[65535, 13, 15, 13, 7, 13, 14, 13, 12, 13, 18, 13, 11, 13, 10, 13, 0, 13, 1, 13, 9, 13, 8, 13, 4, 13, 5, 13, 6, 13, 17, 13, 19, 13, 36, 13, 38, 13],
	#[5, -141],
	#[65535, 9, 15, 9, 7, 9, 19, 9, 14, 9, 12, 9, 18, 9, 11, 9, 10, 9, 0, 9, 1, 9, 9, 9, 8, 9, 4, 9, 5, 9, 6, 9, 17, 9, 36, 9, 38, 9],
	#[4, -143],
	#[65535, 50, 5, 50, 36, 50],
	#[36, -145, 5, -147],
	#[12, -148],
	#[65535, 51, 5, 51, 36, 51],
	#[65535, 18, 19, 18, 0, 18, 1, 18, 17, 18, 4, 18, 5, 18, 6, 18, 7, 18, 8, 18, 9, 18, 10, 18, 11, 18, 12, 18, 14, 18, 15, 18, 18, 18, 36, 18, 38, 18],
	#[37, -149],
	#[1, -14, 17, -6, 4, -4, 5, 3, 6, -20, 7, -5, 8, -15, 9, -7, 10, -16, 11, -9, 12, -18, 14, -2, 15, -12, 18, -17, 19, -13, 36, 3, 38, -10],
	#[65535, 52, 5, 52, 36, 52],
	#[2, -152],
	#[7, -5, 38, -10, 18, -17, 0, 3, 1, -14, 36, 3, 8, -15, 4, -4, 5, 3, 6, -20, 9, -7, 10, -16, 11, -9, 12, -18, 14, -2, 15, -12, 17, -6, 19, -13],
	#[65535, 8, 0, 8, 5, 8, 36, 8],
	#[15, 19, 7, 19, 12, 19, 14, 19, 38, 19, 19, 19, 18, 19, 11, 19, 10, 19, 0, 19, 1, 19, 9, 19, 8, 19, 4, 19, 5, 19, 6, 19, 16, -155, 17, 19, 36, 19],
	#[15, -12, 7, -5, 6, -20, 14, -2, 38, -10, 12, -18, 18, -17, 11, -9, 10, -16, 9, -7, 8, -15, 4, -4, 17, -6, 19, -13],
	#[65535, 20, 15, 20, 7, 20, 19, 20, 14, 20, 38, 20, 12, 20, 18, 20, 11, 20, 10, 20, 0, 20, 1, 20, 9, 20, 8, 20, 4, 20, 5, 20, 6, 20, 17, 20, 36, 20],
	#[65535, 23, 6, 23, 12, 23, 15, 23, 7, 23, 14, 23, 38, 23, 19, 23, 9, 23, 18, 23, 11, 23, 10, 23, 0, 23, 1, 23, 36, 23, 8, 23, 4, 23, 5, 23, 17, 23]],
  goto-table:
      #[#[10, 15, 11, 8, 12, 17, 19, 12, 7, 4, 68, 2, 52, 10, 51, 7, 49, 18, 38, 9, 18, 16, 15, 11, 14, 1, 17, 5, 9, 6, 8, 14, 6, 19, 4, 3, 1, 13],
	#[67, 20, 66, 141],
	#[67, 20, 66, 33, 65, 129, 55, 127, 23, 128, 22, 130, 21, 126, 7, 124, 3, 125],
	#[68, 2, 50, 139, 51, 45, 52, 10, 17, 5, 38, 9, 14, 1, 12, 17, 11, 8, 10, 15, 9, 6, 8, 14, 7, 4, 19, 12, 4, 3, 1, 13, 18, 16, 6, 19, 15, 11],
	#[68, 122, 38, 9, 12, 17],
	#[68, 47, 61, 52, 60, 48, 59, 54, 58, 55, 57, 53, 56, 118, 38, 9, 34, 49, 33, 50, 12, 17],
	#[67, 20, 66, 116],
	#[],
	#[12, 110],
	#[54, 24],
	#[68, 2, 51, 109, 38, 9, 19, 12, 18, 16, 15, 11, 14, 1, 12, 17, 10, 15, 8, 14, 7, 4, 6, 19, 4, 3, 17, 5, 9, 6, 52, 10, 1, 13, 11, 8],
	#[68, 47, 56, 51, 59, 54, 60, 48, 61, 52, 57, 53, 33, 50, 58, 55, 12, 17, 38, 9, 34, 49],
	#[70, 91],
	#[67, 20, 66, 87],
	#[67, 20, 66, 85],
	#[67, 20, 66, 83],
	#[12, 39],
	#[],
	#[0, 38],
	#[66, 21, 67, 20],
	#[68, 23, 38, 9, 12, 17],
	#[2, 22],
	#[],
	#[],
	#[38, 9, 12, 17, 7, 26, 69, 27, 68, 25],
	#[67, 20, 66, 33, 65, 37],
	#[68, 29, 38, 9, 12, 17],
	#[39, 28],
	#[],
	#[68, 30, 38, 9, 12, 17, 10, 31],
	#[67, 20, 66, 33, 65, 34],
	#[67, 20, 66, 32],
	#[],
	#[37, 35],
	#[],
	#[66, 33, 67, 20, 65, 36],
	#[],
	#[],
	#[],
	#[64, 40, 12, 41],
	#[4, 44],
	#[37, 42],
	#[64, 43, 12, 41],
	#[],
	#[68, 2, 52, 10, 51, 45, 50, 46, 38, 9, 19, 12, 18, 16, 17, 5, 15, 11, 14, 1, 12, 17, 11, 8, 10, 15, 9, 6, 8, 14, 7, 4, 1, 13, 4, 3, 6, 19],
	#[],
	#[5, 156],
	#[13, 67],
	#[],
	#[68, 47, 61, 52, 60, 48, 59, 54, 58, 55, 57, 53, 56, 65, 38, 9, 34, 49, 33, 50, 12, 17],
	#[68, 47, 60, 64, 61, 52, 38, 9, 34, 49, 33, 50, 12, 17],
	#[25, 76, 24, 78, 4, 81],
	#[],
	#[27, 72, 26, 71],
	#[32, 62, 31, 61, 30, 59, 29, 60],
	#[28, 57, 3, 56],
	#[68, 47, 61, 52, 60, 48, 59, 74, 38, 9, 34, 49, 33, 50, 12, 17],
	#[68, 47, 61, 52, 60, 48, 59, 58, 34, 49, 33, 50, 12, 17, 38, 9],
	#[30, 59, 29, 60, 32, 62, 31, 61],
	#[68, 47, 61, 52, 60, 70, 38, 9, 34, 49, 33, 50, 12, 17],
	#[68, 47, 61, 52, 60, 69, 38, 9, 34, 49, 33, 50, 12, 17],
	#[68, 47, 61, 52, 60, 66, 38, 9, 34, 49, 33, 50, 12, 17],
	#[68, 47, 61, 52, 60, 63, 38, 9, 34, 49, 33, 50, 12, 17],
	#[],
	#[],
	#[35, 77, 24, 78, 25, 76],
	#[],
	#[67, 20, 66, 68],
	#[],
	#[],
	#[],
	#[12, 17, 68, 47, 61, 52, 60, 48, 58, 75, 59, 54, 38, 9, 34, 49, 33, 50],
	#[68, 47, 61, 52, 60, 48, 59, 54, 58, 73, 38, 9, 34, 49, 33, 50, 12, 17],
	#[28, 57, 3, 56],
	#[32, 62, 29, 60, 30, 59, 31, 61],
	#[28, 57, 3, 56],
	#[61, 52, 60, 48, 58, 55, 57, 80, 68, 47, 59, 54, 38, 9, 34, 49, 33, 50, 12, 17],
	#[],
	#[61, 52, 60, 48, 59, 54, 58, 55, 57, 79, 68, 47, 38, 9, 34, 49, 33, 50, 12, 17],
	#[27, 72, 26, 71],
	#[27, 72, 26, 71],
	#[68, 2, 52, 10, 51, 45, 50, 82, 38, 9, 19, 12, 18, 16, 17, 5, 15, 11, 14, 1, 12, 17, 11, 8, 10, 15, 9, 6, 8, 14, 7, 4, 6, 19, 4, 3, 1, 13],
	#[5, 153],
	#[2, 84],
	#[],
	#[2, 86],
	#[],
	#[3, 88, 2, 89],
	#[66, 150, 67, 20],
	#[68, 2, 51, 45, 50, 90, 52, 10, 19, 12, 17, 5, 38, 9, 10, 15, 9, 6, 8, 14, 7, 4, 6, 19, 4, 3, 1, 13, 11, 8, 18, 16, 12, 17, 14, 1, 15, 11],
	#[],
	#[46, 96, 45, 93, 44, 97, 43, 99, 42, 95, 41, 94, 40, 92, 12, 98],
	#[],
	#[],
	#[],
	#[],
	#[12, 108],
	#[],
	#[71, 100, 47, 101],
	#[],
	#[4, 103],
	#[67, 20, 66, 102],
	#[],
	#[53, 104],
	#[20, 105],
	#[54, 106],
	#[5, 107],
	#[],
	#[],
	#[],
	#[13, 111],
	#[67, 20, 66, 112],
	#[4, 113],
	#[68, 2, 51, 45, 50, 114, 38, 9, 19, 12, 18, 16, 17, 5, 14, 1, 11, 8, 10, 15, 9, 6, 8, 14, 7, 4, 6, 19, 4, 3, 1, 13, 12, 17, 15, 11, 52, 10],
	#[5, 115],
	#[],
	#[2, 117],
	#[],
	#[24, 78, 25, 76, 4, 119],
	#[68, 2, 50, 120, 51, 45, 52, 10, 19, 12, 18, 16, 38, 9, 14, 1, 12, 17, 11, 8, 17, 5, 9, 6, 8, 14, 7, 4, 6, 19, 4, 3, 1, 13, 10, 15, 15, 11],
	#[5, 121],
	#[],
	#[68, 2, 52, 123, 38, 9, 19, 12, 18, 16, 17, 5, 15, 11, 12, 17, 11, 8, 10, 15, 9, 6, 8, 14, 7, 4, 6, 19, 4, 3, 14, 1],
	#[],
	#[67, 20, 66, 135],
	#[],
	#[],
	#[67, 20, 66, 133],
	#[3, 132],
	#[2, 131],
	#[],
	#[],
	#[],
	#[2, 134],
	#[],
	#[55, 136, 23, 128, 22, 130, 21, 126, 3, 125],
	#[67, 20, 66, 137],
	#[2, 138],
	#[],
	#[5, 140],
	#[],
	#[4, 142],
	#[62, 143],
	#[36, 144, 63, 145, 5, 146],
	#[12, 147],
	#[],
	#[],
	#[37, 148],
	#[68, 2, 50, 149, 51, 45, 52, 10, 19, 12, 18, 16, 17, 5, 38, 9, 14, 1, 12, 17, 11, 8, 10, 15, 9, 6, 8, 14, 7, 4, 6, 19, 1, 13, 4, 3, 15, 11],
	#[],
	#[2, 151],
	#[68, 2, 50, 152, 51, 45, 52, 10, 19, 12, 18, 16, 17, 5, 14, 1, 12, 17, 11, 8, 10, 15, 9, 6, 1, 13, 8, 14, 38, 9, 15, 11, 6, 19, 4, 3, 7, 4],
	#[],
	#[16, 154],
	#[52, 155, 68, 2, 38, 9, 19, 12, 15, 11, 12, 17, 11, 8, 17, 5, 9, 6, 8, 14, 7, 4, 6, 19, 10, 15, 14, 1, 4, 3, 18, 16],
	#[],
	#[]],
  action-function-table:
	 vector(jam-parser-action0,
		jam-parser-action1,
		jam-parser-action2,
		jam-parser-action3,
		jam-parser-action4,
		jam-parser-action5,
		jam-parser-action6,
		jam-parser-action7,
		jam-parser-action8,
		jam-parser-action9,
		jam-parser-action10,
		jam-parser-action11,
		jam-parser-action12,
		jam-parser-action13,
		jam-parser-action14,
		jam-parser-action15,
		jam-parser-action16,
		jam-parser-action17,
		jam-parser-action18,
		jam-parser-action19,
		jam-parser-action20,
		jam-parser-action21,
		jam-parser-action22,
		jam-parser-action23,
		jam-parser-action24,
		jam-parser-action25,
		jam-parser-action26,
		jam-parser-action27,
		jam-parser-action28,
		jam-parser-action29,
		jam-parser-action30,
		jam-parser-action4,
		jam-parser-action32,
		jam-parser-action32,
		jam-parser-action4,
		jam-parser-action35,
		jam-parser-action35,
		jam-parser-action4,
		jam-parser-action38,
		jam-parser-action39,
		jam-parser-action4,
		jam-parser-action41,
		jam-parser-action42,
		jam-parser-action43,
		jam-parser-action44,
		jam-parser-action4,
		jam-parser-action46,
		jam-parser-action47,
		jam-parser-action48,
		jam-parser-action49,
		jam-parser-action50,
		jam-parser-action51,
		jam-parser-action52,
		jam-parser-action3,
		jam-parser-action54,
		jam-parser-action5,
		jam-parser-action5,
		jam-parser-action54,
		jam-parser-action58,
		jam-parser-action59,
		jam-parser-action51,
		jam-parser-action4,
		jam-parser-action62,
		jam-parser-action63,
		jam-parser-action64,
		jam-parser-action65,
		jam-parser-action3,
		jam-parser-action67,
		jam-parser-action68,
		jam-parser-action69,
		jam-parser-action70,
		jam-parser-action71,
		jam-parser-action72,
		jam-parser-action73,
		jam-parser-action1,
		jam-parser-action75),
  action-nargs-table: #[2, 0, 1, 0, 1, 1, 2, 4, 6, 3, 3, 3, 4, 6, 3, 3, 3, 7, 5, 5, 7, 5, 3, 6, 9, 0, 0, 1, 1, 1, 2, 1, 3, 3, 1, 3, 3, 1, 3, 3, 1, 3, 3, 3, 3, 1, 2, 3, 1, 3, 0, 2, 4, 0, 3, 1, 1, 3, 1, 0, 2, 1, 4, 2, 4, 4, 0, 2, 2, 2, 2, 2, 2, 3, 0, 2],
  action-nt-table: #[48, 49, 49, 50, 50, 51, 51, 51, 51, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 53, 54, 55, 55, 55, 55, 56, 56, 56, 57, 57, 57, 58, 58, 58, 59, 59, 59, 59, 59, 60, 60, 61, 61, 61, 62, 62, 63, 64, 64, 64, 65, 65, 66, 67, 67, 68, 68, 69, 69, 69, 70, 70, 70, 70, 70, 70, 70, 70, 71, 71]);
