module:    threads-internal
Synopsis:  The threads macros
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// Dynamic-Bind


define macro dynamic-bind
 
  { dynamic-bind () ?:body end }
    => { ?body }

  { dynamic-bind (?:name = ?init:expression, ?others:*) ?:body end }
    => { %dynamic-bind-variable(?name, ?init, dynamic-bind (?others) ?body end) }

  { dynamic-bind (?name:name (?arguments:*) = ?init:expression, ?others:*) ?:body end }
    => { ?name ## "-dynamic-binder"
	   (?init, method () dynamic-bind (?others) ?body end end, ?arguments) }

  { dynamic-bind (?complex-place = ?init:expression, ?others:*) ?:body end }
    => { dynamic-bind (?complex-place = ?init, ?others) ?body end }
 
complex-place:
  { ?prefix:* \. ?:name }            // Support for dot notation
    => { ?name(?prefix) }

  { ?prefix:* [ ?elt:expression ] }  // Support for element via []
    => { ?=element(?prefix, ?elt) }

  { ?prefix:* [ ?arguments:* ] }     // Support for aref via []
    => { ?=aref(?prefix, ?arguments) }

prefix:
  { ?:expression} => { ?expression }

arguments:
  { } => { }
  { ?:expression, ... } => { ?expression, ... }

end macro;




// %dynamic-bind-variable should be special syntax so that the compiler
// can do checking that the variable has been defined as a thread variable.
// Here is a portable definition to get things started.


define macro %dynamic-bind-variable
  { %dynamic-bind-variable (?variable:name, ?init:expression, ?body:body) }
    => { begin
           let %old-value% = ?variable;
           block ()
             ?variable := ?init;
             ?body
           cleanup
             ?variable := %old-value%
           end block
         end }
end macro;






/// WITH-LOCK


define macro with-lock

  { with-lock (?lock:expression, ?options:*) 
      ?body:body 
      ?failure
    end }
    => { begin
           let $lock$ = ?lock;
           if (wait-for($lock$, ?options))
             block ()
               ?body
             cleanup
               release($lock$)
             end block
           else
             ?failure
           end if
         end }

failure:
  { failure ?:body }
    => { ?body }
  { }
    => { signal(make(<timeout-expired>, synchronization: $lock$)) }

end macro;




/// CONDITIONAL-UPDATE!




define macro conditional-update-aux

  { conditional-update-aux (?conditional-updater:*)
      success ?success:body
      failure ?failure:body
    end }
    => { if (?conditional-updater)
           ?success
         else ?failure
         end if }

conditional-updater:
  { ?lvar:name, ?new-val:expression, ?place:name }
    => { %conditional-update-variable(?place, ?new-val, ?lvar) }

  { ?lvar:name, ?new-val:expression, ?fn:name (?arguments:*) }
    => { ?fn ## "-conditional-updater" (?new-val, ?lvar, ?arguments) }

  { ?lvar:name, ?new-val:expression, ?prefix:* \. ?fn:name }
    => { ?fn ## "-conditional-updater" (?new-val, ?lvar, ?prefix) }

  { ?lvar:name, ?new-val:expression, ?prefix:* [ ?elt:expression ] }
    => { ?=element-conditional-updater (?new-val, ?lvar, ?prefix, ?elt) }

  { ?lvar:name, ?new-val:expression, ?prefix:* [ ?arguments:* ] }
    => { ?=aref-conditional-updater (?new-val, ?lvar, ?prefix, ?arguments) }

prefix:
  { ?:expression} => { ?expression }

arguments:
  { } => { }
  { ?:expression, ... } => { ?expression, ... }

end macro;


define macro conditional-update!

  // Case for variables
  { conditional-update! (?lvar:name :: ?type:expression = ?place:*) 
      ?:body
      ?result-options:*
    end }
    => { begin
           let ?lvar :: ?type = ?place;
           let $result$ = ?body;
           conditional-update-aux (?lvar, $result$, ?place)
             ?result-options
           end;
         end }

result-options:
  { success ?success:body ?fail }
    => { success ?success ?fail }
  { ?fail }
    => { success $result$ ?fail }

fail:
  { failure ?:body } 
    => { failure ?body }
  {} 
    => { failure signal(make(<conditional-update-error>)) }

end macro;




define macro conditional-set-variable!
  // Case for variables
  { conditional-set-variable! 
      (?lvar:name, ?newval:expression, ?comparison:expression) }
    => { %conditional-update-variable (?lvar, ?newval, ?comparison) }
end macro;


define macro atomic-increment! 

  { atomic-increment! (?place:*, ?by:expression) }
    => { begin
           let by = ?by;
           local method update ()
                   conditional-update! (value = ?place)
                     value + by;
                   failure update()
                   end conditional-update!;
                 end method;
           update();
         end }


  { atomic-increment! (?place:*) }
    => { atomic-increment!(?place, 1) }

end macro;



define macro atomic-decrement! 

  { atomic-decrement! (?place:*, ?by:expression) }
    => { atomic-increment!(?place, - (?by)) }

  { atomic-decrement! (?place:*) }
    => { atomic-increment!(?place, -1) }

end macro;


