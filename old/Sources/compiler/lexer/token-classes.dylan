Module: lexer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Token classes as used by the parser:

define variable $end-of-input  = identity(eoi:);

define variable $symbol  = #"<symbol>".identity;
define variable $keyword = #"<keyword>".identity;
define variable $literal = #"<literal>".identity;
define variable $string  = #"<string>".identity;
define variable $binary-operator  = #"<binary-operator>".identity;

define variable $lbracket = #"<lbracket>".identity;
define variable $rbracket = #"<rbracket>".identity;
define variable $var-sep = #"<var-sep>".identity;
define variable $dot = #"<dot>".identity;
define variable $statement-sep  = #"<statement-sep>".identity;
define variable $lsbracket = #"<lsbracket>".identity;
define variable $rsbracket = #"<rsbracket>".identity;
define variable $lcbracket = #"<lcbracket>".identity;
define variable $rcbracket = #"<rcbracket>".identity;

define variable $var-type-sep = #"<var-type-sep>".identity;
define variable $minus = #"<minus>".identity;
define variable $not = #"<not>".identity;
define variable $binds = #"<binds>".identity;
define variable $var-singleton-sep = #"<var-singleton-sep>".identity;
define variable $implies = #"<implies>".identity;
define variable $becomes = #"<becomes>".identity;
define variable $list-open = #"<list-open>".identity;
define variable $vector-open = #"<vector-open>".identity;

define variable $next = identity(&next:);
define variable $rest = identity(&rest:);
define variable $key = identity(&key:);
define variable $all-keys = identity(&all-keys:);

define variable $define = identity(define:);
define variable $end = identity(end:);
define variable $generic = identity(generic:);
define variable $handler = identity(handler:);
define variable $let = identity(let:);
define variable $local = identity(local:);
define variable $method = identity(method:);
define variable $macro = identity(macro:);
define variable $otherwise = identity(otherwise:);

define variable $seal-word = #"<seal-word>".identity;

define variable $simple-begin-word = #"<simple-begin-word>".identity;
define variable $expr-begin-word = #"<expr-begin-word>".identity;
define variable $details-begin-word = #"<details-begin-word>".identity;

define variable $simple-intermediate-word 
  = #"<simple-intermediate-word>".identity;
define variable $expr-intermediate-word 
  = #"<expr-intermediate-word>".identity;
define variable $details-intermediate-word 
  = #"<details-intermediate-word>".identity;

define variable $defining-word = #"<defining-word>".identity;

// The default classifier:

define constant $dylan-words :: <object-table> 
  = make(<object-table>);

define constant register-dylan-word
  = method (word, type)
      $dylan-words[word] := type
    end;

define constant default-classifier 
  = method (sym :: <symbol>) 
      let type = element($dylan-words, sym, default: $symbol);
      values(type, sym)
    end;

define constant register-type-words
  = method (type :: <symbol>, #rest words)
      for (word :: <symbol> in words)
        register-dylan-word(word, type);
      end;
    end;

define constant register-infix-macro-word
  = method (word :: <symbol>, type)
      register-dylan-word(word, type);
    end;

register-dylan-word(#"define", $define);
register-dylan-word(#"end", $end);
register-dylan-word(#"generic", $generic);
register-dylan-word(#"handler", $handler);
register-dylan-word(#"let", $let);
register-dylan-word(#"local", $local);
register-dylan-word(#"method", $method);
register-dylan-word(#"macro", $macro);
register-dylan-word(#"otherwise", $otherwise);

register-dylan-word(#"::", $var-type-sep);
register-dylan-word(#"-", $minus);
register-dylan-word(#"~", $not);
register-dylan-word(#"=", $binds);
register-dylan-word(#"==", $var-singleton-sep);
register-dylan-word(#"=>", $implies);
register-dylan-word(#":=", $becomes);

register-type-words($binary-operator,
  #"+", #"*", #"/", #"^", #"~=", #"<", #"<=", #">", #">=");
register-type-words($binary-operator,
  #"&", #"|");

register-type-words($seal-word, #"seal");      
register-type-words($simple-begin-word, #"begin", #"case");
register-type-words($expr-begin-word, 
  #"if", #"unless", #"until", #"while", #"select");
register-type-words($details-begin-word, 
  #"block", #"for");
register-type-words($simple-intermediate-word,
  #"afterwards", #"cleanup", #"else", #"finally");
register-type-words($expr-intermediate-word,
  #"elseif");
register-type-words($details-intermediate-word,
  #"exception");
register-type-words($defining-word,
  #"constant", #"variable", #"class", #"module", #"library");

// eof
