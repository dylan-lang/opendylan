Module: gabriel-benchmarks
Author: Carl Gay
Synopsis: BOYER - Converted from the Common Lisp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// BOYER -- Logic programming benchmark, originally written by Bob Boyer.
/// Fairly CONS intensive.

define variable **unify-subst** = #f;
define variable **temp-temp** = #f;

define function add-lemma (term)
  if (~atom?(term)
      & (first(term) == #"equal")
      & ~atom?(second(term)))
    get(first(second(term)), #"lemmas")
      := pair(term, get(first(second(term)), #"lemmas"));
  else
    error("ADD-LEMMA did not like term %=", term);
  end if
end function add-lemma;

define function add-lemma-lst (lst)
  if (null?(lst))
    #t
  else
    add-lemma(first(lst));
    add-lemma-lst(tail(lst))
  end if
end function add-lemma-lst;

define function apply-subst (alist, term)
  if (atom?(term))
    if (**temp-temp** := assoc(term, alist, test: \==))
      tail(**temp-temp**)
    end if
  else
    pair(first(term), apply-subst-lst(alist, tail(term)))
  end if
end function apply-subst;

define function apply-subst-lst (alist, lst)
  if (null?(lst))
    #() // nil
  else
    pair(apply-subst(alist, first(lst)),
         apply-subst-lst(alist, tail(lst)))
  end if
end function apply-subst-lst;

define function truep (x, lst)
  x = #(#"t") | member?(x, lst)
end function truep;

define function falsep (x, lst)
  x = #(#"f") | member?(x, lst)
end function falsep;

define function one-way-unify (term1, term2)
  **unify-subst** := #();  // nil
  one-way-unify1(term1, term2)
end function one-way-unify;

define function one-way-unify1 (term1, term2)
// => (b :: <boolean>)
  if (atom?(term2))
    if (**temp-temp** := assoc(term2, **unify-subst**, test: \==))
      term1 = tail(**temp-temp**)
    else
      **unify-subst** := pair(pair(term2, term1), **unify-subst**);
      #t
    end if
  elseif (atom?(term1))
    #f // nil
  elseif (first(term1) == first(term2))
    one-way-unify1-&lst(tail(term1), tail(term2))
  else
    #f // nil
  end if
end function one-way-unify1;

define function one-way-unify1-&lst (lst1, lst2)
// => (b :: <boolean>)
  if (null?(lst1))
    #t
  elseif (one-way-unify1(first(lst1), first(lst2)))
    one-way-unify1-&lst(tail(lst1), tail(lst2))
  else
    #f // nil
  end if
end function one-way-unify1-&lst;

define function rewrite (term)
  if (atom?(term))
    term
  else
    rewrite-with-lemmas(pair(first(term), rewrite-args(tail(term))),
                        get(first(term), #"lemmas"))
  end if
end function rewrite;

define function rewrite-args (lst)
  if (null?(lst))
    #() // nil
  else
    pair(rewrite(first(lst)), rewrite-args(tail(lst)))
  end if
end function rewrite-args;

define function rewrite-with-lemmas (term, lst)
  if (null?(lst))
    term
  elseif (one-way-unify(term, second(first(lst))))
    rewrite(apply-subst(**unify-subst**, third(first(lst))))
  else
    rewrite-with-lemmas(term, tail(lst))
  end if
end function rewrite-with-lemmas;

define function boyer-setup ()
  add-lemma-lst
  // What a mess.  Maybe I should use keyword: syntax.
  (#(#(#"equal", #(#"compile", #"form"),
       #(#"reverse", #(#"codegen", #(#"optimize", #"form"),
                       #(#"nil")))),
     #(#"equal", #(#"eqp", #"x", #"y"),
       #(#"equal", #(#"fix", #"x"), #(#"fix", #"y"))),
     #(#"equal", #(#"greaterp", #"x", #"y"),
       #(#"lessp", #"y", #"x")),
     #(#"equal", #(#"lesseqp", #"x", #"y"),
       #(#"not", #(#"lessp", #"y", #"x"))),
     #(#"equal", #(#"greatereqp", #"x", #"y"),
       #(#"not", #(#"lessp", #"x", #"y"))),
     #(#"equal", #(#"boolean", #"x"),
       #(#"or", #(#"equal", #"x", #(#"t")),
         #(#"equal", #"x", #(#"f")))),
     #(#"equal", #(#"iff", #"x", #"y"),
       #(#"and", #(#"implies", #"x", #"y"),
         #(#"implies", #"y", #"x"))),
     #(#"equal", #(#"even1", #"x"),
       #(#"if", #(#"zerop", #"x"),
         #(#"t"),
         #(#"odd", #(#"1-", #"x")))),
     #(#"equal", #(#"countps-", #"l", #"pred"),
       #(#"countps-loop", #"l", #"pred", #(#"zero"))),
     #(#"equal", #(#"fact-", #"i"),
       #(#"fact-loop", #"i", #"1")),
     #(#"equal", #(#"reverse-", #"x"),
       #(#"reverse-loop", #"x", #(#"nil"))),
     #(#"equal", #(#"divides", #"x", #"y"),
       #(#"zerop", #(#"remainder", #"y", #"x"))),
     #(#"equal", #(#"assume-true", #"var", #"alist"),
       #(#"cons", #(#"cons", #"var", #(#"t")),
         #"alist")),
     #(#"equal", #(#"assume-false", #"var", #"alist"),
       #(#"cons", #(#"cons", #"var", #(#"f")),
         #"alist")),
     #(#"equal", #(#"tautology-checker", #"x"),
       #(#"tautologyp", #(#"normalize", #"x"),
         #(#"nil"))),
     #(#"equal", #(#"falsify", #"x"),
       #(#"falsify1", #(#"normalize", #"x"),
         #(#"nil"))),
     #(#"equal", #(#"prime", #"x"),
       #(#"and", #(#"not", #(#"zerop", #"x")),
         #(#"not", #(#"equal", #"x", #(#"add1", #(#"zero")))),
         #(#"prime1", #"x", #(#"1-", #"x")))),
     #(#"equal", #(#"and", #"p", #"q"),
       #(#"if", #"p", #(#"if", #"q", #(#"t"),
                        #(#"f")),
         #(#"f"))),
     #(#"equal", #(#"or", #"p", #"q"),
       #(#"if", #"p", #(#"t"),
         #(#"if", #"q", #(#"t"),
           #(#"f")),
         #(#"f"))),
     #(#"equal", #(#"not", #"p"),
       #(#"if", #"p", #(#"f"),
         #(#"t"))),
     #(#"equal", #(#"implies", #"p", #"q"),
       #(#"if", #"p", #(#"if", #"q", #(#"t"),
                        #(#"f")),
         #(#"t"))),
     #(#"equal", #(#"fix", #"x"),
       #(#"if", #(#"numberp", #"x"),
         #"x",
         #(#"zero"))),
     #(#"equal", #(#"if", #(#"if", #"a", #"b", #"c"),
                   #"d", #"e"),
       #(#"if", #"a", #(#"if", #"b", #"d", #"e"),
         #(#"if", #"c", #"d", #"e"))),
     #(#"equal", #(#"zerop", #"x"),
       #(#"or", #(#"equal", #"x", #(#"zero")),
         #(#"not", #(#"numberp", #"x")))),
     #(#"equal", #(#"plus", #(#"plus", #"x", #"y"),
                   #"z"),
       #(#"plus", #"x", #(#"plus", #"y", #"z"))),
     #(#"equal", #(#"equal", #(#"plus", #"a", #"b"),
                   #(#"zero")),
       #(#"and", #(#"zerop", #"a"),
         #(#"zerop", #"b"))),
     #(#"equal", #(#"difference", #"x", #"x"),
       #(#"zero")),
     #(#"equal", #(#"equal", #(#"plus", #"a", #"b"),
                   #(#"plus", #"a", #"c")),
       #(#"equal", #(#"fix", #"b"),
         #(#"fix", #"c"))),
     #(#"equal", #(#"equal", #(#"zero"),
                   #(#"difference", #"x", #"y")),
       #(#"not", #(#"lessp", #"y", #"x"))),
     #(#"equal", #(#"equal", #"x", #(#"difference", #"x", #"y")),
       #(#"and", #(#"numberp", #"x"),
         #(#"or", #(#"equal", #"x", #(#"zero")),
           #(#"zerop", #"y")))),
     #(#"equal", #(#"meaning", #(#"plus-tree", #(#"append", #"x", #"y")),
                   #"a"),
       #(#"plus", #(#"meaning", #(#"plus-tree", #"x"),
                    #"a"),
         #(#"meaning", #(#"plus-tree", #"y"),
           #"a"))),
     #(#"equal", #(#"meaning", #(#"plus-tree", #(#"plus-fringe", #"x")),
                   #"a"),
       #(#"fix", #(#"meaning", #"x", #"a"))),
     #(#"equal", #(#"append", #(#"append", #"x", #"y"),
                   #"z"),
       #(#"append", #"x", #(#"append", #"y", #"z"))),
     #(#"equal", #(#"reverse", #(#"append", #"a", #"b")),
       #(#"append", #(#"reverse", #"b"),
         #(#"reverse", #"a"))),
     #(#"equal", #(#"times", #"x", #(#"plus", #"y", #"z")),
       #(#"plus", #(#"times", #"x", #"y"),
         #(#"times", #"x", #"z"))),
     #(#"equal", #(#"times", #(#"times", #"x", #"y"),
                   #"z"),
       #(#"times", #"x", #(#"times", #"y", #"z"))),
     #(#"equal", #(#"equal", #(#"times", #"x", #"y"),
                   #(#"zero")),
       #(#"or", #(#"zerop", #"x"),
         #(#"zerop", #"y"))),
     #(#"equal", #(#"exec", #(#"append", #"x", #"y"),
                   #"pds", #"envrn"),
       #(#"exec", #"y", #(#"exec", #"x", #"pds", #"envrn"),
         #"envrn")),
     #(#"equal", #(#"mc-flatten", #"x", #"y"),
       #(#"append", #(#"flatten", #"x"),
         #"y")),
     #(#"equal", #(#"member", #"x", #(#"append", #"a", #"b")),
       #(#"or", #(#"member", #"x", #"a"),
         #(#"member", #"x", #"b"))),
     #(#"equal", #(#"member", #"x", #(#"reverse", #"y")),
       #(#"member", #"x", #"y")),
     #(#"equal", #(#"length", #(#"reverse", #"x")),
       #(#"length", #"x")),
     #(#"equal", #(#"member", #"a", #(#"intersect", #"b", #"c")),
       #(#"and", #(#"member", #"a", #"b"),
         #(#"member", #"a", #"c"))),
     #(#"equal", #(#"nth", #(#"zero"),
                   #"i"),
       #(#"zero")),
     #(#"equal", #(#"exp", #"i", #(#"plus", #"j", #"k")),
       #(#"times", #(#"exp", #"i", #"j"),
         #(#"exp", #"i", #"k"))),
     #(#"equal", #(#"exp", #"i", #(#"times", #"j", #"k")),
       #(#"exp", #(#"exp", #"i", #"j"),
         #"k")),
     #(#"equal", #(#"reverse-loop", #"x", #"y"),
       #(#"append", #(#"reverse", #"x"),
         #"y")),
     #(#"equal", #(#"reverse-loop", #"x", #(#"nil")),
       #(#"reverse", #"x")),
     #(#"equal", #(#"count-list", #"z", #(#"sort-lp", #"x", #"y")),
       #(#"plus", #(#"count-list", #"z", #"x"),
         #(#"count-list", #"z", #"y"))),
     #(#"equal", #(#"equal", #(#"append", #"a", #"b"),
                   #(#"append", #"a", #"c")),
       #(#"equal", #"b", #"c")),
     #(#"equal", #(#"plus", #(#"remainder", #"x", #"y"),
                   #(#"times", #"y", #(#"quotient", #"x", #"y"))),
       #(#"fix", #"x")),
     #(#"equal", #(#"power-eval", #(#"big-plus1", #"l", #"i", #"base"),
                   #"base"),
       #(#"plus", #(#"power-eval", #"l", #"base"),
         #"i")),
     #(#"equal", #(#"power-eval", #(#"big-plus", #"x", #"y", #"i", #"base"),
                   #"base"),
       #(#"plus", #"i", #(#"plus", #(#"power-eval", #"x", #"base"),
                          #(#"power-eval", #"y", #"base")))),
     #(#"equal", #(#"remainder", #"y", #"1"),
       #(#"zero")),
     #(#"equal", #(#"lessp", #(#"remainder", #"x", #"y"),
                   #"y"),
       #(#"not", #(#"zerop", #"y"))),
     #(#"equal", #(#"remainder", #"x", #"x"),
       #(#"zero")),
     #(#"equal", #(#"lessp", #(#"quotient", #"i", #"j"),
                   #"i"),
       #(#"and", #(#"not", #(#"zerop", #"i")),
         #(#"or", #(#"zerop", #"j"),
           #(#"not", #(#"equal", #"j", #"1"))))),
     #(#"equal", #(#"lessp", #(#"remainder", #"x", #"y"),
                   #"x"),
       #(#"and", #(#"not", #(#"zerop", #"y")),
         #(#"not", #(#"zerop", #"x")),
         #(#"not", #(#"lessp", #"x", #"y")))),
     #(#"equal", #(#"power-eval", #(#"power-rep", #"i", #"base"),
                   #"base"),
       #(#"fix", #"i")),
     #(#"equal", #(#"power-eval", #(#"big-plus", #(#"power-rep", #"i", #"base"),
                                    #(#"power-rep", #"j", #"base"),
                                    #(#"zero"),
                                    #"base"),
                   #"base"),
       #(#"plus", #"i", #"j")),
     #(#"equal", #(#"gcd", #"x", #"y"),
       #(#"gcd", #"y", #"x")),
     #(#"equal", #(#"nth", #(#"append", #"a", #"b"),
                   #"i"),
       #(#"append", #(#"nth", #"a", #"i"),
         #(#"nth", #"b", #(#"difference", #"i", #(#"length", #"a"))))),
     #(#"equal", #(#"difference", #(#"plus", #"x", #"y"),
                   #"x"),
       #(#"fix", #"y")),
     #(#"equal", #(#"difference", #(#"plus", #"y", #"x"),
                   #"x"),
       #(#"fix", #"y")),
     #(#"equal", #(#"difference", #(#"plus", #"x", #"y"),
                   #(#"plus", #"x", #"z")),
       #(#"difference", #"y", #"z")),
     #(#"equal", #(#"times", #"x", #(#"difference", #"c", #"w")),
       #(#"difference", #(#"times", #"c", #"x"),
         #(#"times", #"w", #"x"))),
     #(#"equal", #(#"remainder", #(#"times", #"x", #"z"),
                   #"z"),
       #(#"zero")),
     #(#"equal", #(#"difference", #(#"plus", #"b", #(#"plus", #"a", #"c")),
                   #"a"),
       #(#"plus", #"b", #"c")),
     #(#"equal", #(#"difference", #(#"add1", #(#"plus", #"y", #"z")),
                   #"z"),
       #(#"add1", #"y")),
     #(#"equal", #(#"lessp", #(#"plus", #"x", #"y"),
                   #(#"plus", #"x", #"z")),
       #(#"lessp", #"y", #"z")),
     #(#"equal", #(#"lessp", #(#"times", #"x", #"z"),
                   #(#"times", #"y", #"z")),
       #(#"and", #(#"not", #(#"zerop", #"z")),
         #(#"lessp", #"x", #"y"))),
     #(#"equal", #(#"lessp", #"y", #(#"plus", #"x", #"y")),
       #(#"not", #(#"zerop", #"x"))),
     #(#"equal", #(#"gcd", #(#"times", #"x", #"z"),
                   #(#"times", #"y", #"z")),
       #(#"times", #"z", #(#"gcd", #"x", #"y"))),
     #(#"equal", #(#"value", #(#"normalize", #"x"),
                   #"a"),
       #(#"value", #"x", #"a")),
     #(#"equal", #(#"equal", #(#"flatten", #"x"),
                   #(#"cons", #"y", #(#"nil"))),
       #(#"and", #(#"nlistp", #"x"),
         #(#"equal", #"x", #"y"))),
     #(#"equal", #(#"listp", #(#"gopher", #"x")),
       #(#"listp", #"x")),
     #(#"equal", #(#"samefringe", #"x", #"y"),
       #(#"equal", #(#"flatten", #"x"),
         #(#"flatten", #"y"))),
     #(#"equal", #(#"equal", #(#"greatest-factor", #"x", #"y"),
                   #(#"zero")),
       #(#"and", #(#"or", #(#"zerop", #"y"),
                   #(#"equal", #"y", #"1")),
         #(#"equal", #"x", #(#"zero")))),
     #(#"equal", #(#"equal", #(#"greatest-factor", #"x", #"y"),
                   #"1"),
       #(#"equal", #"x", #"1")),
     #(#"equal", #(#"numberp", #(#"greatest-factor", #"x", #"y")),
       #(#"not", #(#"and", #(#"or", #(#"zerop", #"y"),
                             #(#"equal", #"y", #"1")),
                   #(#"not", #(#"numberp", #"x"))))),
     #(#"equal", #(#"times-list", #(#"append", #"x", #"y")),
       #(#"times", #(#"times-list", #"x"),
         #(#"times-list", #"y"))),
     #(#"equal", #(#"prime-list", #(#"append", #"x", #"y")),
       #(#"and", #(#"prime-list", #"x"),
         #(#"prime-list", #"y"))),
     #(#"equal", #(#"equal", #"z", #(#"times", #"w", #"z")),
       #(#"and", #(#"numberp", #"z"),
         #(#"or", #(#"equal", #"z", #(#"zero")),
           #(#"equal", #"w", #"1")))),
     #(#"equal", #(#"greatereqpr", #"x", #"y"),
       #(#"not", #(#"lessp", #"x", #"y"))),
     #(#"equal", #(#"equal", #"x", #(#"times", #"x", #"y")),
       #(#"or", #(#"equal", #"x", #(#"zero")),
         #(#"and", #(#"numberp", #"x"),
           #(#"equal", #"y", #"1")))),
     #(#"equal", #(#"remainder", #(#"times", #"y", #"x"),
                   #"y"),
       #(#"zero")),
     #(#"equal", #(#"equal", #(#"times", #"a", #"b"),
                   #"1"),
       #(#"and", #(#"not", #(#"equal", #"a", #(#"zero"))),
         #(#"not", #(#"equal", #"b", #(#"zero"))),
         #(#"numberp", #"a"),
         #(#"numberp", #"b"),
         #(#"equal", #(#"1-", #"a"),
           #(#"zero")),
         #(#"equal", #(#"1-", #"b"),
           #(#"zero")))),
     #(#"equal", #(#"lessp", #(#"length", #(#"delete", #"x", #"l")),
                   #(#"length", #"l")),
       #(#"member", #"x", #"l")),
     #(#"equal", #(#"sort2", #(#"delete", #"x", #"l")),
       #(#"delete", #"x", #(#"sort2", #"l"))),
     #(#"equal", #(#"dsort", #"x"),
       #(#"sort2", #"x")),
     #(#"equal", #(#"length", #(#"cons", #"x1",
                                #(#"cons", #"x2",
                                  #(#"cons", #"x3", #(#"cons", #"x4",
                                                      #(#"cons", #"x5",
                                                        #(#"cons", #"x6", #"x7"))))))),
       #(#"plus", #"6", #(#"length", #"x7"))),
     #(#"equal", #(#"difference", #(#"add1", #(#"add1", #"x")),
                   #"2"),
       #(#"fix", #"x")),
     #(#"equal", #(#"quotient", #(#"plus", #"x", #(#"plus", #"x", #"y")),
                   #"2"),
       #(#"plus", #"x", #(#"quotient", #"y", #"2"))),
     #(#"equal", #(#"sigma", #(#"zero"),
                   #"i"),
       #(#"quotient", #(#"times", #"i", #(#"add1", #"i")),
         #"2")),
     #(#"equal", #(#"plus", #"x", #(#"add1", #"y")),
       #(#"if", #(#"numberp", #"y"),
         #(#"add1", #(#"plus", #"x", #"y")),
         #(#"add1", #"x"))),
     #(#"equal", #(#"equal", #(#"difference", #"x", #"y"),
                   #(#"difference", #"z", #"y")),
       #(#"if", #(#"lessp", #"x", #"y"),
         #(#"not", #(#"lessp", #"y", #"z")),
         #(#"if", #(#"lessp", #"z", #"y"),
           #(#"not", #(#"lessp", #"y", #"x")),
           #(#"equal", #(#"fix", #"x"),
             #(#"fix", #"z"))))),
     #(#"equal", #(#"meaning", #(#"plus-tree", #(#"delete", #"x", #"y")),
                   #"a"),
       #(#"if", #(#"member", #"x", #"y"),
         #(#"difference", #(#"meaning", #(#"plus-tree", #"y"),
                            #"a"),
           #(#"meaning", #"x", #"a")),
         #(#"meaning", #(#"plus-tree", #"y"),
           #"a"))),
     #(#"equal", #(#"times", #"x", #(#"add1", #"y")),
       #(#"if", #(#"numberp", #"y"),
         #(#"plus", #"x", #(#"times", #"x", #"y")),
         #(#"fix", #"x"))),
     #(#"equal", #(#"nth", #(#"nil"),
                   #"i"),
       #(#"if", #(#"zerop", #"i"),
         #(#"nil"),
         #(#"zero"))),
     #(#"equal", #(#"last", #(#"append", #"a", #"b")),
       #(#"if", #(#"listp", #"b"),
         #(#"last", #"b"),
         #(#"if", #(#"listp", #"a"),
           #(#"cons", #(#"car", #(#"last", #"a")),
             #"b"),
           #"b"))),
     #(#"equal", #(#"equal", #(#"lessp", #"x", #"y"),
                   #"z"),
       #(#"if", #(#"lessp", #"x", #"y"),
         #(#"equal", #"t", #"z"),
         #(#"equal", #"f", #"z"))),
     #(#"equal", #(#"assignment", #"x", #(#"append", #"a", #"b")),
       #(#"if", #(#"assignedp", #"x", #"a"),
         #(#"assignment", #"x", #"a"),
         #(#"assignment", #"x", #"b"))),
     #(#"equal", #(#"car", #(#"gopher", #"x")),
       #(#"if", #(#"listp", #"x"),
         #(#"car", #(#"flatten", #"x")),
         #(#"zero"))),
     #(#"equal", #(#"flatten", #(#"cdr", #(#"gopher", #"x"))),
       #(#"if", #(#"listp", #"x"),
         #(#"cdr", #(#"flatten", #"x")),
         #(#"cons", #(#"zero"),
           #(#"nil")))),
     #(#"equal", #(#"quotient", #(#"times", #"y", #"x"),
                   #"y"),
       #(#"if", #(#"zerop", #"y"),
         #(#"zero"),
         #(#"fix", #"x"))),
     #(#"equal", #(#"get", #"j", #(#"set", #"i", #"val", #"mem")),
       #(#"if", #(#"eqp", #"j", #"i"),
         #"val",
         #(#"get", #"j", #"mem")))))
end function boyer-setup;

define function tautologyp (x, true-lst, false-lst)
// => (b :: <boolean>)
  case
    truep(x, true-lst)   => #t;
    falsep(x, false-lst) => #f; // nil
    atom?(x)             => #f; // nil
    first(x) == #"if"    =>
      case
        truep(second(x), true-lst)   => tautologyp(third(x), true-lst, false-lst);
        falsep(second(x), false-lst) => tautologyp(fourth(x), true-lst, false-lst);
        otherwise                  =>
          tautologyp(third(x), pair(second(x), true-lst), false-lst)
          & tautologyp(fourth(x), true-lst, pair(second(x), false-lst));
      end case;
    otherwise            => #f; // nil
  end case
end function tautologyp;

define function tautp (x)
// => (b :: <boolean>)
  tautologyp(rewrite(x), #(), #())  // nil nil
end function tautp;

define function boyer-test ()
    tautp(apply-subst(#(#(#"x", #"f", #(#"plus", #(#"plus", #"a", #"b"),
                                        #(#"plus", #"c", #(#"zero")))),
                        #(#"y", #"f", #(#"times", #(#"times", #"a", #"b"),
                                        #(#"plus", #"c", #"d"))),
                        #(#"z", #"f", #(#"reverse", #(#"append", #(#"append", #"a", #"b"),
                                                      #(#"nil")))),
                        #(#"u", #"equal", #(#"plus", #"a", #"b"),
                          #(#"difference", #"x", #"y")),
                        #(#"w", #"lessp", #(#"remainder", #"a", #"b"),
                          #(#"member", #"a", #(#"length", #"b")))),
                      #(#"implies", #(#"and", #(#"implies", #"x", #"y"),
                                      #(#"and", #(#"implies", #"y", #"z"),
                                        #(#"and", #(#"implies", #"z", #"u"),
                                          #(#"implies", #"u", #"w")))),
                        #(#"implies", #"x", #"w"))))
end function boyer-test;

/*
#|
(defun trans-of-implies (n)
  (list (quote implies)
        (trans-of-implies1 n)
        (list (quote implies)
              0 n)))

(defun trans-of-implies1 (n)
  (cond ((eql n 1)
         (list (quote implies)
               0 1))
        (t (list (quote and)
                 (list (quote implies)
                       (1- n)
                       n)
                 (trans-of-implies1 (1- n))))))
|#
*/

//(defvar setup-performed-p (prog1 t (boyer-setup)))
boyer-setup();

define function testboyer ()
  boyer-test();
end function testboyer;

define benchmark boyer = testboyer;

