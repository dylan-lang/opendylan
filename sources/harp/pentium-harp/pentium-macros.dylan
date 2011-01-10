module:    pentium-harp
Synopsis:  Macro support for the Pentium instruction set
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Macro support for the pentium instruction set


//  pentium-template defines the pentium backend pattern matchers.

define template-definer-macro pentium-template
  (<pentium-back-end>, pentium-instructions)
end;

define local-template-definer-macro local-pentium-template
  (<pentium-back-end>)
end;



// pentium-method defines methods for pentium clash functions etc.


define macro pentium-method 
  { pentium-method () ?:body end }
    => { method (?=backend :: <pentium-back-end>, ?=ins :: <integer>)
           ignore(?=backend); 
           ignore(?=ins); 
           ?body
         end }

  { pentium-method (?type:name) ?:body end }
    => { method (?=backend :: <pentium-back-end>, ?=ins :: <integer>)
           let ?=sv-ins = ?=backend.variables.sv-instructions;
           "with-" ## ?type (?=sv-ins at ?=ins)
             ?body
           end
         end }

end macro;
