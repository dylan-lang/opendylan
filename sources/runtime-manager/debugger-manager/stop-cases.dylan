module:     dm-internals
synopsis:   Definitions of external (non access-path) stop reasons.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// IMPORTED: <external-stop-reason>
///// IMPORTED: <language-level-stop-reason>


///// <DEBUGGER-GENERATED-STOP-REASON>
//    The application stopped due to some behaviour on behalf of the
//    debugger.

define open abstract class <debugger-generated-stop-reason>
                             (<external-stop-reason>)

  constant slot stop-reason-client-data :: <object>,
    init-value: #f,
    init-keyword: client-data:;

end class;


///// <DEBUGGER-STOP-APPLICATION-STOP-REASON>
//    The application stopped as a result of a 'stop' action generated
//    by the debugger. Eg. the user pressed the STOP button in the
//    toolbar.

define class <debugger-stop-application-stop-reason>
                             (<debugger-generated-stop-reason>)
end class;


///// <WITH-STACK-PROTOCOL-STOP-REASON>
//    A dylan stop-reason that requires a format string and format args
//    to be read from the stack when it occurs. Examples are
//    <dylan-debug-message-stop-reason> and <dylan-invoke-debugger-stop-reason>

define abstract class <with-stack-protocol-stop-reason>
                           (<language-level-stop-reason>)

  constant slot stored-debug-target :: <debug-target>,
    required-init-keyword: target:;

  slot formatted-string-cache :: false-or(<string>),
    init-value: #f;

  constant slot control-string-instance :: <remote-value>,
    required-init-keyword: string-instance:;

  constant slot format-args-vector :: <vector>,
    required-init-keyword: format-args:;

end class;


///// <DYLAN-DEBUG-MESSAGE-STOP-REASON>
//    The debugger was notified because of a runtime call to debug-message.

define class <dylan-debug-message-stop-reason>
                (<with-stack-protocol-stop-reason>)
end class;


///// <DYLAN-INVOKE-DEBUGGER-STOP-REASON>
//    The debugger was invoked because of an unhandled dylan condition.

define class <dylan-invoke-debugger-stop-reason>
                (<with-stack-protocol-stop-reason>)
end class;


// For now, class breakpoints are modelled as language-level-stop-reason

define class <class-breakpoint-stop-reason>
                           (<language-level-stop-reason>)

  constant slot class-breakpoint-class :: <remote-value>,
    required-init-keyword: class:;

  constant slot class-breakpoint-size :: <integer>,
    required-init-keyword: size:;

end class;

///// <INTERACTIVE-THREAD-INITIALIZED-STOP-REASON>
//    The debugger was invoked by a thread created by the DM itself
//    via a call to SPAWN-INTERACTIVE-THREAD.

define class <interactive-thread-initialized-stop-reason>
                (<language-level-stop-reason>)

  constant slot interactive-thread-name :: <byte-string>,
    required-init-keyword: name:;

end class;


///// <SOURCE-CODE-ALIGNMENT-STOP-REASON> 

define class <source-code-alignment-stop-reason> 
                 (<language-level-stop-reason>)
end class;


///// <INTERACTOR-RETURN-STOP-REASON>
//    The debugger was invoked because it was tracking the execution
//    of an interactive form, and the execution context has just
//    returned.

define class <interactor-return-stop-reason>
               (<language-level-stop-reason>)

  constant slot interactor-transaction-id :: <object>,
    required-init-keyword: transaction-id:;

end class;


///// EXPORTED GENERIC FUNCTIONS

define generic interactor-return-values
    (sr :: <interactor-return-stop-reason>) => (vals :: <sequence>);

define generic dylan-debug-message-string
    (sr :: <dylan-debug-message-stop-reason>)
      => (str :: <string>);

define generic dylan-error-message-string
    (sr :: <dylan-invoke-debugger-stop-reason>)
      => (str :: <string>);

define generic stop-reason-debug-points
    (application :: <debug-target>, sr :: <debug-point-stop-reason>)
      => (interested-debug-points :: <sequence>);


///// INTERACTOR-RETURN-VALUES
//    TODO: Implement

define method interactor-return-values
    (sr :: <interactor-return-stop-reason>) => (vals :: <sequence>)
  #[]
end method;


///// DYLAN-DEBUG-MESSAGE-STRING

define method dylan-debug-message-string
    (sr :: <dylan-debug-message-stop-reason>)
      => (str :: <string>)
  let application = sr.stored-debug-target;
  if (sr.formatted-string-cache)
    sr.formatted-string-cache
  else
    sr.formatted-string-cache :=
      apply(remote-format-to-string,
            application,
            dylan-string-data(application, sr.control-string-instance),
            sr.format-args-vector);
    sr.formatted-string-cache
  end if
end method;


///// DYLAN-ERROR-MESSAGE-STRING

define method dylan-error-message-string
    (sr :: <dylan-invoke-debugger-stop-reason>)
      => (str :: <string>)
  let application = sr.stored-debug-target;
  if (sr.formatted-string-cache)
    sr.formatted-string-cache
  else
    sr.formatted-string-cache :=
      apply(remote-format-to-string,
            application,
            dylan-string-data(application, sr.control-string-instance),
            sr.format-args-vector);
    sr.formatted-string-cache
  end if
end method;


///// STOP-REASON-DEBUG-POINTS

define method stop-reason-debug-points
    (application :: <debug-target>, sr :: <debug-point-stop-reason>)
      => (interested-debug-points :: <sequence>)
  application.signalling-debug-points;
end method;
