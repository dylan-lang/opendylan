Module:    dfmc-application
Synopsis:  <local-variable-object> environment protocols from the application
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Note:
// The application proxy for <local-variable-object> is
// <application-local-variable>, defined in this library. It holds the
// value of the variable as either a <remote-value> or a tracked
// <remote-object>.


///// <APPLICATION-LOCAL-VARIABLE>
//    The superclass of all application local variable proxies.

define sealed abstract class <application-local-variable> (<object>)

  constant slot local-lexical-name :: <string>,
    required-init-keyword: name:;

  constant slot is-argument? :: <boolean> = #f,
    init-keyword: argument?:;

end class;


///// <FOREIGN-LOCAL-VARIABLE>
//    An application local variable that is not from Dylan.

define class <foreign-local-variable> (<application-local-variable>)

  constant slot local-foreign-value :: <remote-value>,
    required-init-keyword: foreign-value:;

end class;


///// <DYLAN-LOCAL-VARIABLE>
//    An application local variable that _is_ from Dylan.

define class <dylan-local-variable> (<application-local-variable>)

  constant slot dm-compiler-model :: <object> = #f,
    init-keyword: model-object:;

  constant slot local-dylan-location :: <remote-location>,
    init-keyword: location:;

end class;


///// GET-LOCAL-VARIABLE-VALUE
//    An internal function. Callers must already have ensured that a 
//    debugger transaction is in effect before calling this function.
//    Returns the <remote-value> corresponding to any lexical variable.

define method get-local-variable-value
    (application :: <dfmc-application>, v :: <foreign-local-variable>)
       => (val :: <remote-value>)
  v.local-foreign-value
end method;

define method get-local-variable-value
    (application :: <dfmc-application>, v :: <dylan-local-variable>)
       => (val :: <remote-value>)
   let target = application.application-target-app;
   read-dylan-value(target, v.local-dylan-location) | $stale-remote-value
end method;


///// ALL-FRAME-LOCAL-VARIABLES
//    An internal function. Callers must already have ensured that a 
//    debugger transaction is in effect before calling this function.
//    Constructs <application-local-variable> objects for all of the
//    live variables in a stack frame.

define method all-frame-local-variables
    (application :: <dfmc-application>, frame :: <application-stack-frame>)
       => (var-seq :: <sequence>)

  // This default method returns an empty sequence. If the frame is not
  // a call frame, then there are no local variables.

  #[]
end method;


define method all-frame-local-variables
    (application :: <dfmc-application>, frame :: <call-frame>)
        => (var-seq :: <sequence>)
  
  let target = application.application-target-app;

  // We know that a debugger transaction is in effect. If this method is
  // being called, then the stack frame must be a foreign call frame.
  // We get a list of all live lexical variables from the DM, and
  // build a <foreign-local-variable> wrapper around each one.

  if (dylan-call-frame?(target, frame))
    let (names, types, models, vals, locations)
      = active-dylan-lexical-variables(target, frame);
    let var-seq = make(<vector>, size: size(names));
    for (i from 0 below size(names))
      var-seq[i] := make(<dylan-local-variable>,
                         name: names[i],
                         location: locations[i],
                         argument?: types[i] == #"function-argument",
                         model-object: models[i]);
      ignore(var-seq[i].dm-compiler-model);
    end for;
    var-seq;
  else
    let (vars, vals) = live-frame-lexical-variables(target, frame);
    let var-seq = make(<vector>, size: size(vars));
    for (i from 0 below size(vars))
      var-seq[i] := make(<foreign-local-variable>,
                         name: vars[i].lexical-variable-name,
                         foreign-value: vals[i])
    end for;
    var-seq;
  end if;
end method;


///// COUNT-FRAME-LOCAL-VARIABLES
//    An internal function. Callers must already have ensured that a
//    debugger transaction is in effect before calling this function.
//    Returns an integer for the number of live lexical variables in a
//    stack frame.

define method count-frame-local-variables
    (application :: <dfmc-application>, frame :: <application-stack-frame>)
       => (var-count :: <integer>)

  // This default method returns an empty sequence. If the frame is not
  // a call frame, then there are no local variables.

  0
end method;

define method count-frame-local-variables
    (application :: <dfmc-application>, frame :: <call-frame>)
        => (var-count :: <integer>)
  let target = application.application-target-app;
  if (dylan-call-frame?(target, frame))
    number-of-active-dylan-variables(target, frame);
  else
    number-of-lexical-variables(target, frame);
  end if;
end method;


///// VARIABLE-VALUE (Environment Protocol Method)
//    Returns an environment object being the value of a local
//    variable.

define method variable-value 
    (application :: <dfmc-application>, variable :: <local-variable-object>,
     #key thread)
 => (value :: <application-object>);
  ignore(thread);

  let target = application.application-target-app;
  let val = #f;

  // Ensuring a debugger transaction, get the <remote-value> for the
  // local variable, and construct the right kind of environment object
  // for it.

  perform-debugger-transaction
     (target,
      method ()
        ignore(variable.application-object-proxy.is-argument?);
        val :=
          make-environment-object-for-runtime-value
             (application,
              get-local-variable-value
                (application, variable.application-object-proxy));
      end method);

  val;
end method;


