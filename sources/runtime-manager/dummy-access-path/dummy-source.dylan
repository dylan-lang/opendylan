module:      access-path-implementation
synopsis:    Modeling the source code of the dummy runtime program
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $the-effing-source-code
  = vector 
      ("",                                                             // 00
       "module:        dummymod",                                      // 01
       "synopsis:      Colourless green ideas sleep furiously",        // 02
       "author:        Guess Who",                                     // 03
       "copyright:     (c) 2001 Amblin Entertainment Inc",             // 04
       "",                                                             // 05
       "// What a huge pile of shyte this is.",                        // 06
       "",                                                             // 07
       "define class <ph-class> (<object>)",                           // 08
       "  slot poo-slot :: <object>, init-keyword: poo:;",             // 09
       "  slot pants-slot :: <object>, init-keyword: pants:;",         // 10
       "end class;",                                                   // 11
       "",                                                             // 12
       "define variable ph1 = make(<ph-class>, poo: 5, pants: 10);",   // 13
       "",                                                             // 14
       "define method func(x, y, z) => ()",                            // 15
       "  let pants = #f;",                                            // 16
       "  x := 1;",                                                    // 17
       "  y := ph1;",                                                  // 18
       "  z := 3;",                                                    // 19
       "  pants := 4;",                                                // 20
       "end method;",                                                  // 21
       "",                                                             // 22
       "define method kick-off-recursion() => ()",                     // 23
       "  recursive-function();",                                      // 24
       "end method;",                                                  // 25
       "",                                                             // 26
       "define method recursive-function() => ()",                     // 27
       "  let thingy = ph1;",                                          // 28
       "  recursive-function();",                                      // 29
       "end method;",                                                  // 30
       "",                                                             // 31
       "define method second-thread() => ()",                          // 32
       "  func(thingy1, thingy2, thingy3);",                           // 33
       "  func(sugar, plum, fairy);",                                  // 34
       "  func(sling, yer, ook);",                                     // 35
       "end method;",                                                  // 36
       "",                                                             // 37
       "for (i from 1 to 3)",                                          // 38
       "  func(plink, plop, fizzzzzzz);",                              // 39
       "  make(<thread>, function: second-thread);",                   // 40
       "end for;",                                                     // 41
       "kick-off-recursion();",                                        // 42
       "");                                                            // 43


define method dummy-read-line (i :: <integer>) => (ln :: <string>)
  $the-effing-source-code[i];
end method;

