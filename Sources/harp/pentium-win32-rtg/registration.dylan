module:    pentium-win32-rtg
Synopsis:  Support for registering top-level items for the Win32 Pentium runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define macro win32-runtime-primitive-definer
  { define ?adjectives:* win32-runtime-primitive ?:name 
      ?body:*
    end }
    => {
         define sideways method "genop--" ## ?name
           (?=be :: <pentium-windows-back-end>) => ()
           with-harp (?=be)
             ?body
           end with-harp;
         end method;
       }
end macro;

define macro win32-API-runtime-primitive-definer
  { define ?adjectives:* win32-API-runtime-primitive ?:name 
        (?win-name:expression, ?index:expression)
      ?body:*
    end }
    => { define ?adjectives call-in runtime-function-aux "primitive-" ## ?name
             (rcurry(stdcall-mangle, ?index), ?win-name, ?adjectives)
           ?body
         end }
end macro;


define macro primitive-reference
  { primitive-reference(?:name) }
    =>
    { ins--constant-ref
       (?=be, primitive-name(?=be, ?"name")) }
end macro;
