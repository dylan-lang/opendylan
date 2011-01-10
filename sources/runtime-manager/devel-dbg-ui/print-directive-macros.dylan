module:     devel-dbg-ui
synopsis:   Utilities for defining print directives in the debugger.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <DEBUGGER-PRINT-DIRECTIVE>
//    A class that describes how a particular dylan object should be
//    printed by the debugger.

define class <debugger-print-directive> (<object>)

  constant slot print-directive-class :: <string>,
    required-init-keyword: print-class-name:;

  constant slot print-directive-module :: <string>,
    required-init-keyword: print-module-name:;

  constant slot print-directive-instructions :: <sequence>,
    required-init-keyword: print-instructions:;

end class;

/*--- Not working, currently
define macro debugger-print-directives-definer

  { define debugger-print-directives for-objects ?classes end }
    => { define constant $debugger-print-directives = vector(?classes); }

   classes:
    { } => { }
    { ?treated-class, ...} 
      => { make(<debugger-print-directive>, ?treated-class), ...}

   treated-class:
    { of-class ?class-name:expression from-module ?module-name:expression
       printing-as ?directive }
    => { print-class-name: ?class-name,
         print-module-name: ?module-name,
         print-instructions: vector(?directive) }

   directive:
    { } => { }
    { ?directive-element ; ...} => { ?directive-element, ...}

   directive-element:
    { string ?string:expression } => { pair(#"string", ?string) }
    { slot-called ?slot-name:expression } => { pair(#"slot", ?slot-name) }
    { indirect-slot ?indirect-slot-sequence }
    => { pair (#"indirect-slot", vector(?indirect-slot-sequence)) }

   indirect-slot-sequence:
    { } => { }
    { ?slot-name:expression, ... } => { ?slot-name, ...}

end macro;
*/
