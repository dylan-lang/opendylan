module:       dm-internals
synopsis:     Integration Place Holders:
              Functions I hope to get from someplace else at some point...
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*

THIS FILE IS THE DEEP, DARK PIT OF FRAGMENTARY DEFINITIONS AND API'S THAT
WILL BE NEEDED BY THE DM ONE DAY.

*/


define constant $dummyfile = 
 "/u/phoward/dylan/runtime-manager/dummy-access-path/dummy-source.pseudodylan";

///// <SOURCE-LOCATOR>
//    This should eventually be imported from the source-records library.
//    Should be fairly painless to swap in the real definition.

define class <source-locator> (<object>)

  slot source-locator-file :: <string>,
    required-init-keyword: filename:;

  slot source-locator-linenumber :: <integer>,
    required-init-keyword: line:;

end class;


///// LOC
//    A convenience function for constructing a source-locator.

define method loc (f :: <string>, l :: <integer>) => (scl)
  make(<source-locator>,
       filename: f,
       line: l);
end method;


///// <COMPILER-MODEL>
//    Place-holder - the superclass of any compiler model object. Stores
//    the actual name and the mangled name of the emitted symbolic
//    object.

define class <compiler-model> (<object>)

  slot actual-dylan-name :: <string>,
    required-init-keyword: source-name:;

  slot mangled-name :: <string>,
    required-init-keyword: mangled-name:;

end class;


///// <FUNCTION-MODEL>
//    Models a dylan function.

define class <function-model> (<compiler-model>)

  slot source-locations :: <sequence>,
    init-value: #[],
    init-keyword: source-locations:;

  slot code-location-offsets :: <sequence>,
    init-value: #[],
    init-keyword: code-offsets:;

  slot lexical-variables :: <sequence>,
    init-value: #[],
    init-keyword: lexicals:;

end class;


///// <LEXICAL-VARIABLE-MODEL>
//    Models a dylan lexical variable.

define class <lexical-variable-model> (<compiler-model>)

  slot live-range-start-line :: <integer>,
    required-init-keyword: live-from-line:;

  slot live-range-end-line :: <integer>,
    required-init-keyword: live-until-line:;

end class;


/* ************************************************************************* */
/* **                          DUMMY STUFF                                ** */
/* ** Fake model objects corresponding to the dummy-access-path's runtime ** */
/* ************************************************************************* */


define constant $func-model =
  make(<function-model>,
       source-name: "func",
       mangled-name: "KfuncYdummymodVdummylib",
       source-locations: vector(loc($dummyfile, 15),
                                loc($dummyfile, 16),
                                loc($dummyfile, 18),
                                loc($dummyfile, 19),
                                loc($dummyfile, 20),
                                loc($dummyfile, 21)),
       code-offsets: #[0, 1, 4, 7, 10, 14],
       lexicals:
         vector(
           make(<lexical-variable-model>,
                source-name: "x", mangled-name: "x",
                live-from-line: 15, live-until-line: 21),
           make(<lexical-variable-model>,
                source-name: "y", mangled-name: "y",
                live-from-line: 15, live-until-line: 21),
           make(<lexical-variable-model>,
                source-name: "z", mangled-name: "z",
                live-from-line: 15, live-until-line: 21),
           make(<lexical-variable-model>,
                source-name: "pants", mangled-name: "pants_M1",
                live-from-line: 16, live-until-line: 20)));

define constant $second-thread-model =
  make(<function-model>,
       source-name: "second-thread",
       mangled-name: "Ksecond_threadYdummymodVdummylib",
       source-locations: vector(loc($dummyfile, 32),
                                loc($dummyfile, 33),
                                loc($dummyfile, 34),
                                loc($dummyfile, 35),
                                loc($dummyfile, 36)),
       code-offsets: #[0, 1, 3, 5, 10],
       lexicals: #[]);

define constant $kick-off-recursion-model =
  make(<function-model>,
       source-name: "kick-off-recursion",
       mangled-name: "Kkick_off_recursionYdummymodVdummylib",
       source-locations: vector(loc($dummyfile, 23),
                                loc($dummyfile, 24),
                                loc($dummyfile, 25)),
       code-offsets: #[0, 3, 5],
       lexicals: #[]);

define constant $recursive-function-model =
  make(<function-model>,
       source-name: "recursive-function",
       mangled-name: "Krecursive_functionYdummymodVdummylib",
       source-locations: vector(loc($dummyfile, 27),
                                loc($dummyfile, 28),
                                loc($dummyfile, 29),
                                loc($dummyfile, 30)),
       code-offsets: #[0, 1, 8, 10],
       lexicals:
         vector(
           make(<lexical-variable-model>,
                source-name: "thingy", mangled-name: "thingy_M1",
                live-from-line: 28, live-until-line: 28)));

define constant $main-model =
  make(<function-model>,
       source-name: "top-level-form-dummymod",
       mangled-name: "KmainYdummymodVdummylib",
       source-locations: vector(loc($dummyfile, 38),
                                loc($dummyfile, 39),
                                loc($dummyfile, 40),
                                loc($dummyfile, 41),
                                loc($dummyfile, 42)),
       code-offsets: #[0, 4, 6, 10, 12],
       lexicals:
         vector(
           make(<lexical-variable-model>,
                source-name: "i", mangled-name: "i_M0",
                live-from-line: 38, live-until-line: 41)));

define constant $compiler-database =
  vector($func-model,
         $second-thread-model,
         $main-model,
         $kick-off-recursion-model,
         $recursive-function-model);


/* ************************ END OF DUMMY STUFF ***************************** */


///// ACQUIRE-COMPILER-MODEL
//    Given a mangled name, gets the compiler's model object.

define method acquire-compiler-model
    (application :: <debug-target>, sym :: <remote-symbol>)
      => (maybe-model :: false-or(<compiler-model>))
  let found = #f;
  let index = 0;
  let limit = size($compiler-database);
  while ((~found) & (index < limit))
    if (sym.remote-symbol-name = $compiler-database[index].mangled-name)
      found := $compiler-database[index];
    else
      index := index + 1;
    end if
  end while;
  found;
end method;


///// ALL-SCOPED-LEXICAL-VARIABLES
//    Given a compiler model and a source locator, returns a sequence of
//    <lexical-variable-model> objects.

define method all-scoped-lexical-variables
    (application :: <debug-target>, func :: <function-model>,
     scl :: <source-locator>)
      => (vars :: <sequence>)
  let ln = scl.source-locator-linenumber;
  let vars = make(<stretchy-vector>, size: 0);
  for (v in func.lexical-variables)
    if ((v.live-range-start-line <= ln) &
        (ln <= v.live-range-end-line))
      add!(vars, v);
    end if
  end for;
  vars;
end method;


///// THREAD-LOCAL-NAME?
//    Determines whether a name should be interpreted as a thread-local
//    symbol.

define method thread-local-name?
    (application :: <debug-target>, name :: <string>)
       => (thread-local? :: <boolean>)
  #f
end method;

