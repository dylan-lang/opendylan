Module:   dfmc-conditions-implementation
Author:   haahr, jonathan, keith, swm
Synopsis: Handing and presenting program conditions.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Presenting conditions

// The normal Dylan condition recovery doesn't entirely fit our
// purposes.  The observation by Kim Barrett and Bob Cassels is that
// using a combination of default-handler and dynamic handlers has the
// wrong behavior for managing errors outside the program-condition
// hierarchy, but yet ones that we still want to track.

// Consider a <file-not-found> error while processing a define
// interface form.  Since it comes from outside the compiler, it
// shouldn't be a <program-condition> error, so our default handlers
// won't catch it.  On the other hand, wrapping the entire compiler in
// a "let handler" form catching all conditions would be a mistake,
// because it would block sensible external handlers which try to
// recover.

// Thus, we invented a new mechanism, which is similar to one used in
// Apple Dylan.  The default-handler for warning does not ``print the
// warning's message in an implemented-defined way,'' but instead
// calls *warning-presentation-hook*, which is a thread variable.
// Similarly, the default method for serious condition calls the thread
// variable *error-presentation-hook* instead of invoking ``an
// implemented-defined debugger.''  The initial values of these
// variables are functions which have the appropriate behavior.

// These generic functions are used as the replacements for the hooked
// functions.  The protocol for present-program-error is that it may
// not return, whereas present-program-note is likely to return.

define generic present-program-error (condition :: <condition>) => ();
define generic present-program-note (condition :: <condition>) => ();

// The macro
//
//   with-program-conditions
//     ...
//   end;
//
// runs its body in a dynamic context where the condition presentation
// hooks are bound to the program-condition versions of the functions.

define macro with-program-conditions
  { with-program-conditions ?:body end }
  /*
  => { dynamic-bind (*error-presentation-hook* = present-program-error,
		     *warning-presentation-hook* = present-program-note)
	 ?body
       end }
  */
  => { do-with-program-conditions(method () ?body end) }
end macro with-program-conditions;

define function do-with-program-conditions (body)
  let handler <program-warning>
    = method (condition, next-handler)
        present-program-note(condition)
      end;
  // We want to collect program errors, but have no set up to handle
  // them within the compiler at the moment. 8(
  let handler (<error>, 
                 test: method (condition)
                         present-program-note(condition);
                         #f
                       end)
      = method (condition, next-handler) end; // never gets called
  body();
end function;

// Store away the original values of the condition presentation hooks,
// for possible use in the new presenters.

define constant $default-error-presenter :: <function>
  = *error-presentation-hook*;
// define constant $default-warning-presenter :: <function>
//  = *warning-presentation-hook*;


// The presentation hooks for conditions while the compiler is running
// record the conditions.  The one for errors is not permitted to
// return, whereas the one for warnings can just return.

define method present-program-error (condition :: <condition>) => ()
  add-program-condition(condition);
  terminate-and-restart(condition);
end method present-program-error;

define method present-program-error (condition :: <program-note>) => ()
  let fun = condition.object-class.program-note-filter;
  if (fun) fun(condition) end;
end method present-program-error;

define method present-program-note (condition :: <condition>) => ()
  add-program-condition(condition);
  report-condition(condition);
end method present-program-note;

// Presentation of notes is more complicated, because we have to decide,
// based on the compiler policy object (and potentially other factors?)
// how this condition is treated.
//
// Even if the note is treated seriously (i.e., like an error) we push
// a restart for ignoring it.
//
// Open issues:
//   - Should the <ignore-serious-note> restart be a default restart?
//   - Should uninteresting conditions be added to the source-record's
//     deque of conditions?

define program-restart <ignore-serious-note>
  slot ignore-note, required-init-keyword: note:;
  format-string "Ignore \"%s\" and continue.";
  format-arguments note;
end program-restart <ignore-serious-note>;

define method present-program-note (note :: <program-note>) => ()
  let fun = note.object-class.program-note-filter;
  if (fun) fun(note) end;
//  next-method();
//  case
//    note.serious-note?
//      => condition-block
//           terminate-and-restart(note);
//         default restart <ignore-serious-note>, note: note
//           => values()
//         end condition-block;
//    note.object-class.program-note-filter == $signal-program-note
//      => report-condition(note);
//    otherwise
//      => values();
//  end case;
end method present-program-note;

// Since returning from notes is allowed -- it ignores the note -- we
// have a bit of protocol to define.  (I think.  I never was to sure
// of this whole part of conditions.)

define method return-allowed? (note :: <program-note>) => (res :: <boolean>)
  #t
end method return-allowed?;

define method return-description (note :: <program-note>) => (res :: <string>)
  "Ignore the note."
end method return-description;

// Open question:
//    - What role would more specific methods play?  Should these methods
//      cover inert domains?


/// Recovery and restarting

// There are several options for responding to errors and serious notes,
// varying in what restart is used and how it is chosen.  The policy is
// chosen (for now) based on the variable *error-recovery-model*.  The
// current set of permissible values are #"batch", meaning pick the
// default restart, and #"interactive", meaning present the user with
// a selection of restarts to choose from.
//
// This option needs to move to the compiler policy object.

define variable *error-recovery-model* = #"batch";

// Errors and serious conditions invoke terminate-and-restart to
// stop the current processing and choose an appropriate restart.

define method terminate-and-restart (condition :: <condition>) => ()
  terminate-and-choose-restart(condition, *error-recovery-model*);
end method terminate-and-restart;

// The batch-mode mechanism for restarting is to pick the innermost
// default restart.

define method terminate-and-choose-restart
    (condition :: <condition>, mechanism == #"batch") => ()
  do-handlers
    (method (type, applicable?, handler-fn, init-arguments)
       if (subtype?(type, <program-restart>)
	   & key-value(init-arguments, default?:))
         let restart = apply(make, type, init-arguments);
         if (applicable?(restart))
           report-condition(condition);
           handler-fn(restart, no-next-handler)
         end if;
       end if;
     end method);
  // If there was no usable restart, now we fail to user level by
  // invoking the debugger that (we're promised) lives in the error
  // presentation hook.
  $default-error-presenter(condition);
end method terminate-and-choose-restart;

// The interactive mechanism for choosing a restart is to present several
// choices to the user.  Currently, this is done by printing them and
// breaking to a listener.  The user then can invoke the function
// "dfmc-restart(n)", where "n" is the number identifying the restart.
// The function dfmc-continue() picks the innermost default restart.
//
// A GUI-based interface to this is needed when we have a real
// environment to work with or stop running under LispWorks.

define thread variable dfmc-continue
  = method () error("dfmc-continue outside of restart chooser") end;

define thread variable dfmc-restart
  = method () error("dfmc-restart outside of restart chooser") end;

define method terminate-and-choose-restart
    (condition :: <condition>, mechanism == #"interactive") => ()
  let (restarts, handlers, default-index)
    = gather-restarts-and-handlers(condition);
  print-restart-options(condition, restarts, handlers, default-index);
  local method select-restart (n :: <integer>)
          handlers[n](restarts[n], no-next-handler)
        end method select-restart;
  dynamic-bind (dfmc-restart = select-restart,
		dfmc-continue = curry(select-restart, default-index))
    /* $default-error-presenter(condition); */
    invoke-debugger(condition);
    // if the user just tries to continue here, do the obvious thing.
    dfmc-continue();
  end dynamic-bind;
end method terminate-and-choose-restart;

define method gather-restarts-and-handlers (condition :: <condition>)
 => (restarts :: <sequence>, handlers :: <sequence>, default :: <integer>)
  let restarts = make(<stretchy-vector>);
  let handlers = make(<stretchy-vector>);
  let default-index = #f;
  do-handlers
    (method (type, applicable?, handler-fn, init-arguments)
       if (subtype?(type, <program-restart>))
         let restart = apply(make, type, init-arguments);
         if (applicable?(restart))
           if (~default-index & key-value(init-arguments, default?:))
             default-index := restarts.size;
           end if;
           add!(restarts, restart);
           // I'm not sure that I can legally store away this handler
	   // function as I'm doing here, since the description of
	   // do-handlers (DRM, p349) says that it has dynamic extent.
	   // In practice, that probably means it's only live in the
	   // dynamic extent of the handler, but the letter of the law
	   // probably means that its dynamic extent is only during
           // the call to the argument of do-handlers.  Workarounds
           // are possibly but awkward.
           add!(handlers, handler-fn);
         end if;
       end if;
     end method);
  values(restarts, handlers, default-index);
end method gather-restarts-and-handlers;

define method print-restart-options
    (condition :: <condition>, restarts, handlers, default-index)
 => ()
  report-condition(condition);
  format-out("\nChoose a restart option:\n\n");
  for (restart in restarts, index from 0)
    format-out(" %s %d  %=",
               if (index == default-index) "*"; else " "; end,
               index,
               restart);
  end for;
  format-out
    ("\n\n"
     "Use dfmc-restart(n) to choose restart n.\n"
     "Use dfmc-continue() to choose restart %d, the default.\n",
     default-index);  
end method print-restart-options;

// For now, there seems to be limited utility in allow restart handlers
// to call next-handler, and too much bother in implementing it.  They
// all get this canonical failure method.  If we need this functionality,
// it's not that hard to implement, but it doesn't seem worth it right now.

define method no-next-handler () => ()
  error("no next-handler available for program restarts");
end method no-next-handler;

// A utility function for treating init-arguments as key/value
// pairs (aka association lists), mainly in order to determine
// if a restart handler is the default one.

define method key-value
    (sequence :: <sequence>, key :: <symbol>, #key default)
 => (value :: <object>)
  block (return)
    for (k in sequence, n from 0)
      if (even?(n) & k == key)
        return(sequence[n + 1]);
      end if;
    finally
      default
    end for
  end block
end method key-value;
