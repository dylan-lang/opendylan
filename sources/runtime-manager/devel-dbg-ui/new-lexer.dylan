module:       devel-dbg-ui
synopsis:     A better command-line system for the console debugger
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// Token Codes

define constant $tokenIllegal                 = 100;
// define constant $tokenQualifier               = 101;
define constant $tokenLiteralAddress          = 102;
define constant $tokenLiteralInteger          = 103;
define constant $tokenLiteralCharacter        = 104;
define constant $tokenLiteralEmptyList        = 105;
define constant $tokenLiteralTrue             = 106;
define constant $tokenLiteralFalse            = 107;
define constant $tokenHistoryVariable         = 108;
define constant $tokenComma                   = 109;
define constant $tokenOpenBracket             = 110;
define constant $tokenCloseBracket            = 111;
define constant $tokenListCloser              = 111; // Deliberate duplicate!
define constant $tokenDylanSymbol             = 112;
define constant $tokenCSymbol                 = 113;
define constant $tokenString                  = 114;
// define constant $tokenPlingPling              = 115;
// define constant $tokenDot                     = 116;
define constant $tokenDylanKeyword            = 117;
define constant $tokenModuleQualifiedSymbol   = 118;
define constant $tokenLibraryQualifiedSymbol  = 119;
define constant $tokenTerminator              = 120;
define constant $tokenRegisterName            = 121;
define constant $tokenListOpener              = 122;
define constant $tokenVectorOpener            = 123;
define constant $tokenVectorCloser            = 124;
// define constant $tokenOpenCurlyBrace          = 125;
// define constant $tokenCloseCurlyBrace         = 126;
// define constant $tokenPureSymbol              = 127;
define constant $tokenRawString                  = 128;

///// Token codes for debugger command-line keywords.

define constant $tokenOpen                    = 200;
define constant $tokenRestart                 = 201;
define constant $tokenContinue                = 202;
define constant $tokenKill                    = 203;
define constant $tokenQuit                    = 204;
define constant $tokenSuspend                 = 205;
define constant $tokenResume                  = 206;
define constant $tokenStep                    = 207;
define constant $tokenOver                    = 208;
define constant $tokenOut                     = 209;
define constant $tokenInto                    = 210;
define constant $tokenBreak                   = 211;
define constant $tokenClear                   = 212;
define constant $tokenBreakpoints             = 213;
define constant $tokenTrace                   = 214;
define constant $tokenUntrace                 = 215;
define constant $tokenBacktrace               = 216;
define constant $tokenFrame                   = 217;
define constant $tokenUp                      = 218;
define constant $tokenDown                    = 219;
define constant $tokenTop                     = 220;
define constant $tokenBottom                  = 221;
define constant $tokenWhere                   = 222;
define constant $tokenPrint                   = 223;
define constant $tokenHelp                    = 224;
define constant $tokenDescribe                = 225;
define constant $tokenEvaluate                = 226;
define constant $tokenDisplay                 = 227;
define constant $tokenTo                      = 228;
define constant $tokenConnect                 = 229;
define constant $tokenShow                    = 230;
define constant $tokenDisassembly             = 231;
define constant $tokenBytes                   = 232;
define constant $tokenAscii                   = 233;
define constant $tokenThreads                 = 240;
define constant $tokenLibraries               = 250;
define constant $tokenRegisters               = 251;
define constant $tokenIn                      = 252;
define constant $tokenLibrary                 = 253;
define constant $tokenModule                  = 254;
define constant $tokenThread                  = 255;
define constant $tokenLocal                   = 256;
define constant $tokenProfile                 = 257;
define constant $tokenNearto                  = 258;
define constant $tokenAt                      = 259;
define constant $tokenDepth                   = 260;
define constant $tokenResults                 = 261;
define constant $tokenAsk                     = 262;
define constant $tokenAll                     = 263;
define constant $tokenSet                     = 264;
define constant $tokenLine                    = 265;
define constant $tokenColumn                  = 266;
define constant $tokenOf                      = 267;
define constant $tokenEnable                  = 268;
define constant $tokenDisable                 = 269;
define constant $tokenIgnore                  = 270;
define constant $tokenOff                     = 271;
define constant $tokenLines                   = 272;
define constant $tokenFor                     = 273;
define constant $tokenMV                      = 274;
define constant $tokenOptions                 = 275;
define constant $tokenExceptions              = 276;
define constant $tokenVerbose                 = 277;
define constant $tokenBug                     = 278;
define constant $tokenFilter                  = 279;
define constant $tokenReveal                  = 280;
define constant $tokenContext                 = 281;
define constant $tokenPrimitives              = 282;
define constant $tokenCleanups                = 283;
define constant $tokenWallClockTime           = 284;
define constant $tokenDownload                = 285;
define constant $tokenFrom                    = 286;
define constant $tokenDW                      = 287;
define constant $tokenAllocation              = 288;
define constant $tokenFaults                  = 289;
define constant $tokenInteract                = 290;
define constant $tokenExact                   = 291;
define constant $tokenExplode                 = 292;
define constant $tokenIopen                   = 293;
define constant $tokenWalk                    = 294;
define constant $tokenInclude                 = 295;
define constant $tokenExclude                 = 296;
define constant $tokenShutUp                  = 297;
define constant $tokenDirective               = 298;
define constant $tokenHotspots                = 299;
define constant $tokenNot                     = 300;
define constant $tokenPercentile              = 301;
define constant $tokenFile                    = 302;
define constant $tokenAggregates              = 303;
define constant $tokenAggregates1             = 304;
define constant $tokenAggregates2             = 305;
define constant $tokenFilter1                 = 307;
define constant $tokenFilter2                 = 308;
define constant $tokenSets                    = 309;
define constant $tokenUnion                   = 310;
define constant $tokenIntersection            = 311;
define constant $tokenEmpty                   = 312;
define constant $tokenFull                    = 313;
define constant $tokenLimit                   = 314;
define constant $tokenLimit0                  = 315;
define constant $tokenLimit1                  = 316;
define constant $tokenLimit2                  = 317;
define constant $tokenTopN                    = 318;
define constant $tokenTopN0                   = 319;
define constant $tokenTopN1                   = 320;
define constant $tokenTopN2                   = 321;
define constant $tokenContains                = 322;
define constant $tokenDebug                   = 323;
define constant $tokenInclusive               = 324;
define constant $tokenExclusive               = 325;
define constant $tokenYes                     = 326;
define constant $tokenNo                      = 327;
define constant $tokenHeight                  = 328;
define constant $tokenDll                     = 329;
define constant $tokenOn                      = 330;
define constant $tokenList                    = 331;
define constant $tokenClass                   = 332;

// Command Line Keywords.

///// <TOKEN>
//    Describes a lexical element of the command language.

define class <token> (<object>)

  constant slot representation :: <string>,
    init-value: "",
    init-keyword: representation:;

  constant slot code :: <integer>,
    init-value: $tokenIllegal,
    init-keyword: code:;

end class;


///// <DLL-SENSITIVE-TOKEN>
//    A token that might have a DLL prefix.

define class <DLL-sensitive-token> (<token>)

  constant slot dll-prefix :: <remote-library>,
    required-init-keyword: dll-prefix:;

end class;


///// TOKEN-DLL-CONTEXT
//    Returns #f for tokens that aren't DLL-sensitive, and returns the
//    <remote-library> in other cases.

define method token-dll-context (token :: <token>)
    => (maybe-dll :: false-or(<remote-library>))
  #f;
end method;

define method token-dll-context (token :: <dll-sensitive-token>)
    => (maybe-dll :: false-or(<remote-library>))
  token.dll-prefix;
end method;


///// <COMMAND-LINE-KEYWORD>
//    A lookup table entry for a reserved keyword.

define class <command-line-keyword> (<token>)

  constant slot abbreviation :: <string>,
    init-value: "",
    init-keyword: abbreviation:;

end class;

//---*** andrewa: we should remove this, string-to-integer will
//---*** work just as well, plus this is a sideways method.
define sideways method as (to == <integer>, s :: <string>) => (int-thing)
  let i = 0;
  let x = 0;
       
  local method character-to-integer (c :: <character>)
          if (c = '0')
            0
          elseif (c = '1')
            1
          elseif (c = '2')
            2
          elseif (c = '3')
            3
          elseif (c = '4')
            4
          elseif (c = '5')
            5
          elseif (c = '6')
            6
          elseif (c = '7')
            7
          elseif (c = '8')
            8
          elseif (c = '9')
            9
          else
            0
          end if
        end method;

  while (i < size(s))
    x := (10 * x) + character-to-integer(s[i]);
    i := i + 1;
  end while;
  x;
end method;

define constant $decimal-digit-set = 
   #['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

define method decimal-digit? (c :: <character>) => (answer :: <boolean>)
  member? (c, $decimal-digit-set);
end method;

define constant $hex-digit-set = 
   #['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
     'A', 'B', 'C', 'D', 'E', 'F',
     'a', 'b', 'c', 'd', 'e', 'f'];

define method hex-digit? (c :: <character>) => (answer :: <boolean>)
  member? (c, $hex-digit-set);
end method;

define constant $reserved-set = 
   #['"', ',', ':', '(', ')', '#', ' ', '[', ']'];

define method reserved? (c :: <character>) => (answer :: <boolean>)
  member? (c, $reserved-set);
end method;

define method decimal-string-to-integer 
    (lexeme :: <string>) => (i :: <integer>)
  string-to-integer (lexeme)
end method;

define method previous-result-string-to-integer (lexeme :: <string>)
    => (i :: <integer>)
  lexeme[0] := '0';        // Replace '$' with leading '0'
  as (<integer>, lexeme);  // Convert as per the dylan library.
end method;

/*
define method hex-string-to-integer 
    (lexeme :: <string>) => (i :: <integer>)

  local method hex-character-to-integer (c :: <character>)
          if (c = '0')
            0
          elseif (c = '1')
            1
          elseif (c = '2')
            2
          elseif (c = '3')
            3
          elseif (c = '4')
            4
          elseif (c = '5')
            5
          elseif (c = '6')
            6
          elseif (c = '7')
            7
          elseif (c = '8')
            8
          elseif (c = '9')
            9
          elseif ((c = 'A') | (c = 'a'))
            10
          elseif ((c = 'B') | (c = 'b'))
            11
          elseif ((c = 'C') | (c = 'c'))
            12
          elseif ((c = 'D') | (c = 'd'))
            13
          elseif ((c = 'E') | (c = 'e'))
            14
          elseif ((c = 'F') | (c = 'f'))
            15
          end if
       end method;

  let x :: <integer> = 0;
  let i = 2; // The first useful digit is at this position.

  // The tokenizer has accepted this string, so we can assume it's format
  // to be correct - no need for fiddly error checks on the digits.

  while (i < size(lexeme))
    x := (16 * x) + hex-character-to-integer (lexeme[i]);
    i := i + 1;
  end while;
  x;
end method;
*/


///// $KEYWORD-LOOKUP-TABLE
//    Contains mappings from keywords to their token codes.

define constant $keyword-lookup-table =
  vector(
    make(<command-line-keyword>,
         representation: "open", abbreviation: "o", code: $tokenOpen),
    make(<command-line-keyword>,
         representation: "restart", abbreviation: "r", code: $tokenRestart),
    make(<command-line-keyword>,
         representation: "continue", abbreviation: "c", code: $tokenContinue),
    make(<command-line-keyword>,
         representation: "kill", abbreviation: "k", code: $tokenKill),
    make(<command-line-keyword>,
         representation: "quit", abbreviation: "q", code: $tokenQuit),
    make(<command-line-keyword>,
         representation: "exit", abbreviation: "ex", code: $tokenQuit),
    make(<command-line-keyword>,
         representation: "suspend", abbreviation: "sus", code: $tokenSuspend),
    make(<command-line-keyword>,
         representation: "resume", abbreviation: "res", code: $tokenResume),
    make(<command-line-keyword>,
         representation: "step", abbreviation: "s", code: $tokenStep),
    make(<command-line-keyword>,
         representation: "over", abbreviation: "over", code: $tokenOver),
    make(<command-line-keyword>,
         representation: "out", abbreviation: "out", code: $tokenOut),
    make(<command-line-keyword>,
         representation: "into", abbreviation: "into", code: $tokenInto),
    make(<command-line-keyword>,
         representation: "break", abbreviation: "b", code: $tokenBreak),
    make(<command-line-keyword>,
         representation: "clear", abbreviation: "cl", code: $tokenClear),
    make(<command-line-keyword>,
         representation: "breakpoints", abbreviation: "bp", 
         code: $tokenBreakpoints),
    make(<command-line-keyword>,
         representation: "trace", abbreviation: "t", code: $tokenTrace),
    make(<command-line-keyword>,
         representation: "untrace", abbreviation: "ut", code: $tokenUntrace),
    make(<command-line-keyword>,
         representation: "backtrace", abbreviation: "bt",
         code: $tokenBacktrace),
    make(<command-line-keyword>,
         representation: "frame", abbreviation: "f", code: $tokenFrame),
    make(<command-line-keyword>,
         representation: "up", abbreviation: "u", code: $tokenUp),
    make(<command-line-keyword>,
         representation: "down", abbreviation: "d", code: $tokenDown),
    make(<command-line-keyword>,
         representation: "top", abbreviation: "top", code: $tokenTop),
    make(<command-line-keyword>,
         representation: "bottom", abbreviation: "bot", code: $tokenBottom),
    make(<command-line-keyword>,
         representation: "where", abbreviation: "w", code: $tokenWhere),
    make(<command-line-keyword>,
         representation: "print", abbreviation: "p", code: $tokenPrint),
    make(<command-line-keyword>,
         representation: "describe", abbreviation: "des", 
         code: $tokenDescribe),
    make(<command-line-keyword>,
         representation: "evaluate", abbreviation: "eval", 
         code: $tokenEvaluate),
    make(<command-line-keyword>,
         representation: "display", abbreviation: "disp", code: $tokenDisplay),
    make(<command-line-keyword>,
         representation: "to", abbreviation: "to", code: $tokenTo),
    make(<command-line-keyword>,
         representation: "show", abbreviation: "sh", code: $tokenShow),
    make(<command-line-keyword>,
         representation: "threads", abbreviation: "thrs", code: $tokenThreads),
    make(<command-line-keyword>,
         representation: "libraries", abbreviation: "libs", 
         code: $tokenLibraries),
    make(<command-line-keyword>,
         representation: "registers", abbreviation: "regs", 
         code: $tokenRegisters),
    make(<command-line-keyword>,
         representation: "in", abbreviation: "in", code: $tokenIn),
    make(<command-line-keyword>,
         representation: "library", abbreviation: "lib", code: $tokenLibrary),
    make(<command-line-keyword>,
         representation: "module", abbreviation: "mod", code: $tokenModule),
    make(<command-line-keyword>,
         representation: "thread", abbreviation: "thr", code: $tokenThread),
    make(<command-line-keyword>,
         representation: "local", abbreviation: "loc", code: $tokenLocal),
    make(<command-line-keyword>,
         representation: "profile", abbreviation: "prof", code: $tokenProfile),
    make(<command-line-keyword>,
         representation: "nearto", abbreviation: "n", code: $tokenNearto),
    make(<command-line-keyword>,
         representation: "at", abbreviation: "at", code: $tokenAt),
    make(<command-line-keyword>,
         representation: "depth", abbreviation: "depth", code: $tokenDepth),
    make(<command-line-keyword>,
         representation: "results", abbreviation: "res", code: $tokenResults),
    make(<command-line-keyword>,
         representation: "ask", abbreviation: "ask", code: $tokenAsk),
    make(<command-line-keyword>,
         representation: "set", abbreviation: "set", code: $tokenSet),
    make(<command-line-keyword>,
         representation: "line", abbreviation: "ln", code: $tokenLine),
    make(<command-line-keyword>,
         representation: "column", abbreviation: "col", code: $tokenColumn),
    make(<command-line-keyword>,
         representation: "of", abbreviation: "of", code: $tokenOf),
    make(<command-line-keyword>,
         representation: "all", abbreviation: "all", code: $tokenAll),
    make(<command-line-keyword>,
         representation: "help", abbreviation: "h", code: $tokenHelp),
    make(<command-line-keyword>,
         representation: "enable", abbreviation: "be", code: $tokenEnable),
    make(<command-line-keyword>,
         representation: "disable", abbreviation: "bd", code: $tokenDisable),
    make(<command-line-keyword>,
         representation: "ignore", abbreviation: "bi", code: $tokenIgnore),
    make(<command-line-keyword>,
         representation: "off", abbreviation: "off", code: $tokenOff),
    make(<command-line-keyword>,
         representation: "lines", abbreviation: "l", code: $tokenLines),
    make(<command-line-keyword>,
         representation: "for", abbreviation: "for", code: $tokenFor),
    make(<command-line-keyword>,
         representation: "mv", abbreviation: "mv", code: $tokenMV),
    make(<command-line-keyword>,
         representation: "options", abbreviation: "opt", code: $tokenOptions),
    make(<command-line-keyword>,
         representation: "exceptions", abbreviation: "ex", 
         code: $tokenExceptions),
    make(<command-line-keyword>,
         representation: "verbose", abbreviation: "v", code: $tokenVerbose),
    make(<command-line-keyword>,
         representation: "report", abbreviation: "br", code: $tokenBug),
    make(<command-line-keyword>,
         representation: "filter", abbreviation: "ff", code: $tokenFilter),
    make(<command-line-keyword>,
         representation: "reveal", abbreviation: "fr", code: $tokenReveal),
    make(<command-line-keyword>,
         representation: "primitives", abbreviation: "prim",
         code: $tokenPrimitives),
    make(<command-line-keyword>,
         representation: "cleanups", abbreviation: "cl", code: $tokenCleanups),
    make(<command-line-keyword>,
         representation: "context", abbreviation: "con", code: $tokenContext),
    make(<command-line-keyword>,
         representation: "download", abbreviation: "dl",
         code: $tokenDownload),
    make(<command-line-keyword>,
         representation: "from", abbreviation: "from",
         code: $tokenFrom),
    make(<command-line-keyword>,
         representation: "dw", abbreviation: "from",
         code: $tokenDW),
    make(<command-line-keyword>,
         representation: "allocation", abbreviation: "alloc",
         code: $tokenAllocation),
    make(<command-line-keyword>,
         representation: "class", abbreviation: "class",
         code: $tokenClass),
    make(<command-line-keyword>,
         representation: "faults", abbreviation: "pf",
         code: $tokenFaults),
    make(<command-line-keyword>,
         representation: "wallclock", abbreviation: "wc",
         code: $tokenWallClockTime),
    make(<command-line-keyword>,
         representation: "connect", abbreviation: "con",
         code: $tokenConnect),
    make(<command-line-keyword>,
         representation: "on", abbreviation: "on",
         code: $tokenOn),
    make(<command-line-keyword>,
         representation: "list", abbreviation: "list",
         code: $tokenList),
    make(<command-line-keyword>,
         representation: "interact", abbreviation: "int",
         code: $tokenInteract),
    make(<command-line-keyword>,
         representation: "bytes", abbreviation: "bytes",
         code: $tokenBytes),
    make(<command-line-keyword>,
         representation: "ascii", abbreviation: "asc",
         code: $tokenAscii),
    make(<command-line-keyword>,
         representation: "disassembly", abbreviation: "disasm",
         code: $tokenDisassembly),
    make(<command-line-keyword>,
         representation: "explode", abbreviation: "exp",
         code: $tokenExplode),
    make(<command-line-keyword>,
         representation: "exact", abbreviation: "exact",
         code: $tokenExact),
    make(<command-line-keyword>,
         representation: "iopen", abbreviation: "io",
         code: $tokenIopen),
    make(<command-line-keyword>,
         representation: "walk", abbreviation: "walk", code: $tokenWalk),
    make(<command-line-keyword>,
         representation: "include", abbreviation: "inc", code: $tokenInclude),
    make(<command-line-keyword>,
         representation: "exclude", abbreviation: "exc", code: $tokenExclude),
    make(<command-line-keyword>,
         representation: "shutup", abbreviation: "su", code: $tokenShutUp),
    make(<command-line-keyword>,
         representation: "directive", abbreviation: "pd",
         code: $tokenDirective),
    make(<command-line-keyword>,
         representation: "hotspots", abbreviation: "hot", code: $tokenHotspots),
    make(<command-line-keyword>,
         representation: "not", abbreviation: "not", code: $tokenNot),
    make(<command-line-keyword>,
         representation: "union", abbreviation: "union", code: $tokenUnion),
    make(<command-line-keyword>,
         representation: "intersection", abbreviation: "intersection", code: $tokenIntersection),
    make(<command-line-keyword>,
         representation: "percentile", abbreviation: "percent", code: $tokenPercentile),
    make(<command-line-keyword>,
         representation: "file", abbreviation: "file", code: $tokenFile),
    make(<command-line-keyword>,
         representation: "aggregates", abbreviation: "lumps", code: $tokenAggregates),
    make(<command-line-keyword>,
         representation: "aggregates1", abbreviation: "lumps1", code: $tokenAggregates1),
    make(<command-line-keyword>,
         representation: "aggregates2", abbreviation: "lumps2", code: $tokenAggregates2),
    make(<command-line-keyword>,
         representation: "filter1", abbreviation: "filter1", code: $tokenFilter1),
    make(<command-line-keyword>,
         representation: "filter2", abbreviation: "filter2", code: $tokenFilter2),
    make(<command-line-keyword>,
         representation: "sets", abbreviation: "sets", code: $tokenSets),
    make(<command-line-keyword>,
         representation: "empty", abbreviation: "null", code: $tokenEmpty),
    make(<command-line-keyword>,
         representation: "full", abbreviation: "any", code: $tokenFull),
    make(<command-line-keyword>,
         representation: "limit", abbreviation: "limit", code: $tokenLimit),
    make(<command-line-keyword>,
         representation: "limit0", abbreviation: "limit0", code: $tokenLimit0),
    make(<command-line-keyword>,
         representation: "limit1", abbreviation: "limit1", code: $tokenLimit1),
    make(<command-line-keyword>,
         representation: "limit2", abbreviation: "limit2", code: $tokenLimit2),
    make(<command-line-keyword>,
         representation: "top-n", abbreviation: "top-n", code: $tokenTopN),
    make(<command-line-keyword>,
         representation: "top-n0", abbreviation: "top-n0", code: $tokenTopN0),
    make(<command-line-keyword>,
         representation: "top-n1", abbreviation: "top-n1", code: $tokenTopN1),
    make(<command-line-keyword>,
         representation: "top-n2", abbreviation: "top-n2", code: $tokenTopN2),
    make(<command-line-keyword>,
         representation: "contains", abbreviation: "contains", code: $tokenContains),
    make(<command-line-keyword>,
         representation: "debug", abbreviation: "debug", code: $tokenDebug),
    make(<command-line-keyword>,
         representation: "inclusive", abbreviation: "inclusive", code: $tokenInclusive),
    make(<command-line-keyword>,
         representation: "exclusive", abbreviation: "exclusive", code: $tokenExclusive),
    make(<command-line-keyword>,
         representation: "yes", abbreviation: "yes", code: $tokenYes),
    make(<command-line-keyword>,
         representation: "no", abbreviation: "no", code: $tokenNo),
    make(<command-line-keyword>,
         representation: "height", abbreviation: "height", code: $tokenHeight),
    make(<command-line-keyword>,
         representation: "dll", abbreviation: "dll", code: $tokenDll)
  );


///// CHECK-KEYWORD
//    Given a string, returns a <command-line-keyword> if the string is
//    a keyword or allowable abbreviation.

define method check-keyword (s :: <string>) => (tok :: <token>)
  let lim = size($keyword-lookup-table);
  let found = #f;
  let i = 0;
  while ((i < lim) & (~found))
    if (($keyword-lookup-table[i].representation = s) |
        ($keyword-lookup-table[i].abbreviation = s))
      found := $keyword-lookup-table[i];
    else
      i := i + 1;
    end if
  end while;
  if (found)
    make(<command-line-keyword>, representation: s, 
         abbreviation: found.abbreviation, code: found.code)
  else
    make(<token>, representation: s, code: $tokenDylanSymbol)
  end if;
end method;


///// LEXER STATES

define constant $stateNewToken                            = 0;
define constant $stateEndOfToken                          = 1;
define constant $stateGettingIntegerLiteral               = 2;
define constant $stateGettingForeignSymbol                = 3;
define constant $stateGettingHexLiteral1                  = 4;
define constant $stateGettingHexLiteral2                  = 5;
define constant $stateGettingHexLiteral3                  = 6;
define constant $stateGotHash                             = 7;
define constant $stateGettingEmptyList1                   = 8;
// define constant $stateGettingEmptyList2                   = 9;
define constant $stateGettingDylanSymbolOrKeyword         = 10;
define constant $stateGettingCharacterLiteral1            = 11;
define constant $stateGettingCharacterLiteral2            = 12;
define constant $stateGettingCharacterLiteral3            = 13;
define constant $stateGettingString1                      = 14;
define constant $stateGettingRawString1                   = 15;
define constant $stateGettingHistoryVariable1             = 16;
define constant $stateGettingHistoryVariable2             = 17;
define constant $stateGotOneQualifier                     = 18;
define constant $stateGotTwoQualifiers                    = 19;
define constant $stateGettingModuleQualifiedSymbol        = 20;
define constant $stateGettingLibraryQualifiedSymbol       = 21;
define constant $stateGettingRegisterName                 = 22;
define constant $stateGotDllPrefix                        = 23;

///// TOKENIZE
//    Given a parse string, returns a sequence of <token> objects.

define variable current-character :: <character> = ' ';

define method tokenize (s :: <string>) => (x :: <sequence>)
  let current-position = 
    if ((s.size > 0) & (s[0] == '!')) 1 else 0 end if;
  current-character := ' ';
  let limit = size(s);
  let still-going? = #t;
  let tokens = make(<stretchy-vector>, size: 0);
  let this-token-string = "";
  let this-dll-prefix = #f;
  let current-state = $stateNewToken;
  
  local method get-next-character ()
          if (current-position < limit)
            current-character := s[current-position];
            current-position := current-position + 1;
          elseif (current-position == limit)
            current-character := ' ';
            current-position := current-position + 1;
          else
            still-going? := #f;
            current-character := ' ';
          end if
        end method;

  local method transition (state :: <integer>)
          current-state := state;
          this-token-string := 
            concatenate(this-token-string, add!("", current-character));
          get-next-character();
        end method;

  local method transition-ignore (state :: <integer>)
          current-state := state;
          get-next-character();
        end method;

  local method epsilon-transition (state :: <integer>)
          current-state := state;
        end method;

  local method set-token (t :: <token>)
          if (this-dll-prefix)
            let mod-t = make(<DLL-sensitive-token>,
                             code: t.code,
                             representation: t.representation,
                             dll-prefix: this-dll-prefix);
            add!(tokens, mod-t);
          else
            add!(tokens, t);
          end if;
        end method;

  while (still-going?)
    epsilon-transition($stateNewToken);
    this-token-string := "";
    this-dll-prefix := #f;
    while ((still-going?) & (current-state ~== $stateEndOfToken))
      select(current-state)
        $stateNewToken =>
          while ((still-going?) & (current-character == ' '))
            get-next-character();
          end while;
          select(current-character)
            '0' => transition($stateGettingHexLiteral1);
            '[' => transition($stateGettingRegisterName);
            '$' => transition($stateGettingHistoryVariable1);
            '_' => transition-ignore($stateGettingForeignSymbol);
            '#' => transition($stateGotHash);
            '"' => transition($stateGettingString1);
            '\'' => transition($stateGettingCharacterLiteral1);
            '(' => transition($stateEndOfToken);
                   set-token(make(<token>, code: $tokenOpenBracket,
                                           representation: this-token-string));
            ')' => transition($stateEndOfToken);
                   set-token(make(<token>, code: $tokenCloseBracket,
                                           representation: this-token-string));
            ']' => transition($stateEndOfToken);
                   set-token(make(<token>, code: $tokenVectorCloser,
                                           representation: this-token-string));
            ',' => transition($stateEndOfToken);
                   set-token(make(<token>, code: $tokenComma,
                                           representation: this-token-string));
            otherwise =>
              if (decimal-digit?(current-character))
                transition($stateGettingIntegerLiteral);
              else
                transition($stateGettingDylanSymbolOrKeyword);
              end if;

          end select;

        $stateGotDllPrefix =>
          select(current-character)
            '_' => transition-ignore($stateGettingForeignSymbol);
            '"' => transition($stateGettingString1);
          otherwise =>
            transition($stateGettingDylanSymbolOrKeyword);
          end select;

        $stateGettingIntegerLiteral =>
          if (decimal-digit?(current-character))
            transition($stateGettingIntegerLiteral);
          else
            epsilon-transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenLiteralInteger,
                                    representation: this-token-string));
          end if;

        $stateGettingForeignSymbol =>
          if (current-character == '"')
            transition($stateGettingRawString1);
          elseif (reserved?(current-character))
            epsilon-transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenCSymbol,
                                    representation: this-token-string));
          else
            transition($stateGettingForeignSymbol);
          end if;

        $stateGettingRegisterName =>
          if (current-character == ']')
            transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenRegisterName,
                                    representation: this-token-string));
          elseif (reserved?(current-character))
            epsilon-transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenIllegal,
                                    representation: this-token-string));
          else
            transition($stateGettingRegisterName);
          end if;

        $stateGettingHistoryVariable1 =>
          if (decimal-digit?(current-character))
            transition($stateGettingHistoryVariable2);
          else
            transition($stateGettingDylanSymbolOrKeyword);
          end if;

        $stateGettingHistoryVariable2 =>
          if (decimal-digit?(current-character))
            transition($stateGettingHistoryVariable2)
          else
            epsilon-transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenHistoryVariable,
                                    representation: this-token-string));
          end if;

        $stateGettingHexLiteral1 =>
          select(current-character)
            'x' => transition($stateGettingHexLiteral2);
            otherwise =>
              if (decimal-digit?(current-character))
                transition($stateGettingIntegerLiteral);
              elseif (reserved?(current-character))
                epsilon-transition($stateEndOfToken);
                set-token(make(<token>, code: $tokenLiteralInteger,
                                        representation: this-token-string));
              else
                epsilon-transition($stateEndOfToken);
                set-token(make(<token>, code: $tokenIllegal,
                                        representation: this-token-string));
              end if;
          end select;

        $stateGettingHexLiteral2 =>
          if (hex-digit?(current-character))
            transition($stateGettingHexLiteral3)
          else
            epsilon-transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenIllegal,
                                    representation: this-token-string));
          end if;

        $stateGettingHexLiteral3 =>
          if (hex-digit?(current-character))
            transition($stateGettingHexLiteral3)
          else
            epsilon-transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenLiteralAddress,
                                    representation: this-token-string));
          end if;

        $stateGotHash =>
          select(current-character)
            'f' =>
              transition($stateEndOfToken);
              set-token(make(<token>, code: $tokenLiteralFalse,
                                      representation: this-token-string));

            't' =>
              transition($stateEndOfToken);
              set-token(make(<token>, code: $tokenLiteralTrue,
                                      representation: this-token-string));

            '[' =>
              transition($stateEndOfToken);
              set-token(make(<token>, code: $tokenVectorOpener,
                                      representation: this-token-string));

            '(' =>
              transition($stateGettingEmptyList1);

            otherwise =>
              epsilon-transition($stateEndOfToken);
              set-token(make(<token>, code: $tokenIllegal,
                                      representation: this-token-string));
          end select;

        $stateGettingEmptyList1 =>
          if (current-character == ')')
            transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenLiteralEmptyList,
                                    representation: this-token-string));
          else
            epsilon-transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenListOpener,
                                    representation: this-token-string));
          end if;

        $stateGettingDylanSymbolOrKeyword =>
          if (current-character == ':')
            transition($stateGotOneQualifier);
          elseif ((current-character == '!') & (~this-dll-prefix))
            let matching-library =
              if (*open-application*)
                core-name-to-library(*open-application*, this-token-string);
              else
                #f
              end if;
            if (matching-library)
              this-dll-prefix := matching-library;
              transition($stateGotDllPrefix);
              this-token-string := "";
            else
              transition($stateGettingDylanSymbolOrKeyword);
            end if;
          elseif (reserved?(current-character))
            epsilon-transition($stateEndOfToken);
            set-token(check-keyword(this-token-string));
          else
            transition($stateGettingDylanSymbolOrKeyword);
          end if;

        $stateGotOneQualifier =>
          if (reserved?(current-character))
            epsilon-transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenDylanKeyword,
                                    representation: this-token-string));
          else
            transition($stateGettingModuleQualifiedSymbol);
          end if;

        $stateGettingModuleQualifiedSymbol =>
          if (current-character == ':')
            transition($stateGotTwoQualifiers);
          elseif (reserved?(current-character))
            epsilon-transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenModuleQualifiedSymbol,
                                    representation: this-token-string));
          else
            transition($stateGettingModuleQualifiedSymbol)
          end if;
            
        $stateGotTwoQualifiers =>
          if (reserved?(current-character))
            epsilon-transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenIllegal,
                                    representation: this-token-string));
          else
            transition($stateGettingLibraryQualifiedSymbol);
          end if;

        $stateGettingLibraryQualifiedSymbol =>
          if (reserved?(current-character))
            epsilon-transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenLibraryQualifiedSymbol,
                                    representation: this-token-string));
          else
            transition($stateGettingLibraryQualifiedSymbol)
          end if;

        $stateGettingCharacterLiteral1 =>
          if (current-character == '\\')
            transition($stateGettingCharacterLiteral2);
          else
            transition($stateGettingCharacterLiteral3);
          end if;

        $stateGettingCharacterLiteral2 =>
          transition($stateGettingCharacterLiteral3);

        $stateGettingCharacterLiteral3 =>
          if (current-character == '\'')
            transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenLiteralCharacter,
                                    representation: this-token-string));
          else
            epsilon-transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenIllegal,
                                    representation: this-token-string));
          end if;

        $stateGettingString1 =>
          if (current-character == '"')
            transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenString,
                                    representation: this-token-string));
          else
            transition($stateGettingString1)
          end if;

        $stateGettingRawString1 =>
          if (current-character == '"')
            transition($stateEndOfToken);
            set-token(make(<token>, code: $tokenRawString,
                                    representation: this-token-string));
          else
            transition($stateGettingRawString1)
          end if;

      end select;
    end while
  end while;
  set-token(make(<token>, code: $tokenTerminator, representation: "END"));
  tokens;
end method;


///// CHECK-LEXICAL-CORRECTNESS
//    Given a sequence of tokens, checks that there are no illegal tokens.
//    If there are no illegal tokens, this function returns #t, otherwise
//    it returns #f, and generates reports on the illegal tokens.

define method check-lexical-correctness
    (token-sequence :: <sequence>) => (correct? :: <boolean>)
  let correct? = #t;
  for (token in token-sequence)
    if (token.code == $tokenIllegal)
      debugger-message("Illegal token %s in command string.",
                       token.representation);
      correct? := #f;
    end if
  end for;
  correct?
end method;

/*
///// GENERATE-NAMESPACE
//    Given a token that is ostensibly a module qualified symbol, actually
//    treat it as a module-name/library-name pair separated by a Colon.

define method generate-namespace
    (token :: <token>) => (modname :: <string>, libname :: <string>)
  if (token.code == $tokenModuleQualifiedSymbol)
    let mod = "";
    let lib = "";
    let full = token.representation;
    let i = 0;
    let limit = size(full);
    while ((i < limit) & (full[i] ~== ':'))
      mod := concatenate(mod, add!("", full[i]));
      i := i + 1;
    end while;
    i := i + 1;
    while (i < limit)
      lib := concatenate(lib, add!("", full[i]));
      i := i + 1;
    end while;
    values(mod, lib);
  else
    values("poo", "pants");
  end if
end method;
*/

///// GENERATE-NAME-AND-CONTEXT
//    Given a token that is a dylan symbol, optionally including module and
//    library qualifiers, and a default <dylan-name-context>, returns
//    the name of the symbol, and another (correctly adjusted)
//    <dylan-name-context>.

define method generate-name-and-context
    (token :: <token>, default-context :: <dylan-name-context>)
       => (name :: <string>, actual-context :: <dylan-name-context>)
  let actual-context = make(<dylan-name-context>);
  let name = "";
  let mod = "";
  let lib = "";
  let full = token.representation;

  select (token.code)
    $tokenDylanSymbol =>
      name := full;
      mod := default-context.context-module;
      lib := default-context.context-library;

    $tokenModuleQualifiedSymbol =>
      let i = 0;
      let limit = size(full);
      lib := default-context.context-library;
      while ((i < limit) & (full[i] ~== ':'))
        name := concatenate(name, add!("", full[i]));
        i := i + 1;
      end while;
      i := i + 1;
      while (i < limit)
        mod := concatenate(mod, add!("", full[i]));
        i := i + 1;
      end while;

    $tokenLibraryQualifiedSymbol =>
      let i = 0;
      let limit = size(full);
      while ((i < limit) & (full[i] ~== ':'))
        name := concatenate(name, add!("", full[i]));
        i := i + 1;
      end while;
      i := i + 1;
      while ((i < limit) & (full[i] ~== ':'))
        mod := concatenate(mod, add!("", full[i]));
        i := i + 1;
      end while;
      i := i + 1;
      while (i < limit)
        lib := concatenate(lib, add!("", full[i]));
        i := i + 1;
      end while;

  end select;

  actual-context.context-library := as-lowercase(lib);
  actual-context.context-module := as-lowercase(mod);
  values(name, actual-context);
end method;


///// GENERATE-ACTUAL-STRING
//    Given a token that is of type $tokenString, this returns the actual
//    string value (ie without the surrounding quotes that make up the
//    token);

define method generate-actual-string
    (token :: <token>) => (str :: <string>)
  let str = make(<byte-string>, size: size(token.representation) - 2);
  for (i from 1 to size(token.representation) - 2)
    str[i - 1] := token.representation[i];
  end for;
  str;
end method;


///// GENERATE-ACTUAL-REGISTER-NAME
//    Given a token that is of type $tokenString, this returns the actual
//    string value (ie without the surrounding quotes that make up the
//    token);

define method generate-actual-register-name
    (token :: <token>) => (reg-name :: <string>)
  let str = make(<byte-string>, size: size(token.representation) - 2);
  for (i from 1 to size(token.representation) - 2)
    str[i - 1] := token.representation[i];
  end for;
  as-uppercase(str);
end method;


///// GENERATE-ACTUAL-KEYWORD
//    Given a token that is of type $tokenDylanKeyword, this returns the name
//    of the keyword without any syntactic baggage, so that the DM can resolve
//    it to an address.

define method generate-actual-keyword
    (token :: <token>) => (keyword-name :: <string>)
  if (token.code == $tokenDylanKeyword)
    let full = token.representation;
    let limit = size(full) - 1;
    let result = make(<byte-string>, size: limit);
    for (i from 0 below limit)
      result[i] := full[i];
    end for;
    result;
  else
    "naffing-naffetty?"
  end if
end method;


///// CHARACTER-ESCAPE
//    Given a character, returns the (unprintable) character that it would
//    become if it had a backslash before it.

define method character-escape (c :: <character>) => (e :: <character>)
  select (as-lowercase(c))
    'n' => '\n';
    't' => '\t';
    'r' => '\r';
    '0' => '\0';
  otherwise =>
    c
  end select;
end method;


///// TRANSLATE-ESCAPES-IN-STRING
//    Given a string, returns the string with all its escape sequences
//    translated to the actual escaped character codes.

define method translate-escapes-in-string
    (s :: <string>) => (es :: <string>)
  let es = "";
  let lim = size(s);
  let i = 0;
  while (i < lim)
    let this-char = s[i];
    if (this-char == '\\')
      let nxt = element(s, i + 1, default: '\\');
      if (nxt == '\\')
        es := concatenate(es, add!("", nxt));
      else
        es := concatenate(es, add!("", character-escape(nxt)));
      end if;
      i := i + 2;
    else
      es := concatenate(es, add!("", this-char));
      i := i + 1;
    end if;
  end while;
  es;
end method;
