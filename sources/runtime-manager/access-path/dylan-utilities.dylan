module:          access-path-implementation
synopsis:        Special access-path functions that take advantage of low-level
                 knowledge of the dylan implementation.
author:          Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// CLASSIFY-SYMBOLIC-NAME
//    Decides what language code should be assigned to a symbol.
//    Although several language codes are defined, we have only so-far
//    been using $symbol-language-Dylan for dylan symbols, and
//    $symbol-language-C for all non-dylan (foreign) symbols.

//    TODO:
//      This function was added hurriedly, and is really in the wrong
//      place.
//      Ideally, the primary argument would be the access-path-abstract-
//      handle, and then this could be made an open generic function.
//      The debugger-manager could then write a method for <debug-target>,
//      and we could just have an unused default method here.
//      This way, the access-path won't need to know anything about
//      the "shape" of dylan symbols.

//    NB:
//      This function is definitely not 100% accurate! But the most
//      important thing is that it doesn't mistake a dylan _function_
//      name for a foreign one. Therefore, the only cases examined
//      here are the standard entry point pattern ("K*I") and the
//      form initializer pattern ("_Init_*"). Unfortunately, this
//      library shouldn't need to be aware of either pattern, but
//      see my comment above.

define method classify-symbolic-name
    (path, name :: <string>)
  => (classification)
  ignore(path);
  let length = name.size;
  if (length > 0)
    if ((name[0] == 'K') & (name[length - 1] == 'I'))
      $symbol-language-Dylan
    elseif ((length > 5) &
            (name[0] == '_') &
            (name[1] == 'I') &
            (name[2] == 'n') &
            (name[3] == 'i') &
            (name[4] == 't') &
            (name[5] == '_'))
      $symbol-language-Dylan
    else
      $symbol-language-C
    end if
  else
    $symbol-language-C
  end if;
end method;


///// DYLAN-CALCULATE-DESTINATION-FOR-STEP-INTO
//    Given a thread that poised at a recorded location, calculates
//    the address that should be breakpointed in order to effect
//    a "step-into" operation. It may also decide that the function
//    register should be used instead. (This can be obtained with the
//    DYLAN-CURRENT-FUNCTION utility).

define method dylan-calculate-destination-for-step-into
    (path :: <access-path>, thread :: <remote-thread>)
  => (address :: <remote-value>,
      use-function-register? :: <boolean>,
      success? :: <boolean>)
  calculate-step-into-on-connection(path.connection, thread);
end method;

define open generic calculate-step-into-on-connection
    (conn :: <access-connection>, thread :: <remote-thread>)
 => (address :: <remote-value>,
     use-function-register? :: <boolean>,
     success? :: <boolean>);

define method calculate-step-into-on-connection
    (conn :: <local-access-connection>, thread :: <remote-thread>)
  => (address :: <remote-value>,
      use-function-register? :: <boolean>,
      success? :: <boolean>)
  let (address :: <remote-value>,
       use-freg :: <integer>,
       ok :: <integer>) =
    nub-dylan-calculate-step-into(conn.connection-process, thread.nub-descriptor);
  values (address,
          use-freg == 1,
          ok == 1)
end method;


///// DYLAN-THREAD-ENVIRONMENT-BLOCK-ADDRESS
//    Gets the thread-local pointer to the dylan-level thread environment
//    block.

define method dylan-thread-environment-block-address
    (ap :: <access-path>, thread :: <remote-thread>)
       => (teb :: <remote-value>)
  teb-on-connection(ap.connection, thread);
end method;

define open generic teb-on-connection
    (conn :: <access-connection>, thread :: <remote-thread>)
       => (teb :: <remote-value>);

define method teb-on-connection
    (conn :: <local-access-connection>, thread :: <remote-thread>)
       => (teb :: <remote-value>)
  let (teb-pointer :: <remote-value>, valid :: <integer>)
    = nub-dylan-thread-environment-block-address
        (conn.connection-process, thread.nub-descriptor);
  if (valid == 1)
    teb-pointer;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-CURRENT-FUNCTION
//    Returns the value of the function register. This cannot determine
//    liveness of the function register. That check should already have been
//    made before calling this method.

define method dylan-current-function
    (ap :: <access-path>, thread :: <remote-thread>)
       => (remote-lambda :: <remote-value>)
  current-function-on-connection(ap.connection, thread);
end method;

define open generic current-function-on-connection
    (conn :: <access-connection>, thread :: <remote-thread>)
 => (remote-lambda :: <remote-value>);

define method current-function-on-connection
    (conn :: <local-access-connection>, thread :: <remote-thread>)
       => (remote-lambda :: <remote-value>)
  nub-dylan-current-function(conn.connection-process, thread.nub-descriptor);
end method;


///// DYLAN-THREAD-MV-BUFFER-LIVE?
//    Queries the necessary flags in the context of a dylan thread to
//    decide whether the contents of the MV buffer are current.

define method dylan-thread-mv-buffer-live?
    (path :: <access-path>, thread :: <remote-thread>) => (well? :: <boolean>)
  mv-buffer-live-on-connection(path.connection, thread) == 1;
end method;

define open generic mv-buffer-live-on-connection
    (conn :: <access-connection>, thread :: <remote-thread>)
  => (code :: <integer>);

define method mv-buffer-live-on-connection
    (conn :: <local-access-connection>, thread :: <remote-thread>)
  => (code :: <integer>)
  nub-dylan-thread-mv-buffer-live(conn.connection-process, thread.nub-descriptor)
end method;
