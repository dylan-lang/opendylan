Module:    dfmc-application
Synopsis:  Serving the slot-object protocols from the application
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// SLOT-CLASS (Environment Protocol Method)
//    Returns a <class-object> for the class that owns this slot.

define method slot-class
    (application :: <dfmc-application>, slot :: <slot-object>) 
         => (class :: <class-object>)

  let target = application.application-target-app;
  let slot-proxy = slot.application-object-proxy;
  let class-object = #f;

  // Within a debugger transaction, call the DM to inspect the slot.
  // Build a <class-object> for the slot's owner class.

  perform-debugger-transaction
     (target,
      method ()
        let slot-descriptor = 
          runtime-proxy-to-remote-value(application, slot-proxy);

        // Call the DM's inspector for slots.

        let (basic-name, basic-type, owner-class, getter, setter,
             init-key, init-req?, init-val, specializer)
               = remote-slot-inspect(target, slot-descriptor);

        // We are interested in the owner class, so build the
        // correct environment object for it.
        class-object
	  := make-environment-object-for-runtime-value
	       (application, owner-class)
      end method);

  // Return the environment object.
  class-object;
end method;


///// SLOT-GETTER (Environment Protocol Method)
//    Returns a <function-object> (if applicable) for the getter function
//    corresponding to this slot.

define method slot-getter 
    (application :: <dfmc-application>, slot :: <slot-object>)
         => (getter :: false-or(<function-object>))

  let target = application.application-target-app;
  let slot-proxy = slot.application-object-proxy;
  let function-object = #f;

  // Within a debugger transaction, call the DM to inspect the slot.
  // Build a <function-object> for the getter function, if it exists.

  perform-debugger-transaction
     (target,
      method ()
        let slot-descriptor = 
          runtime-proxy-to-remote-value(application, slot-proxy);

        // Call the DM's inspector for slots.

        let (basic-name, basic-type, owner-class, getter, setter,
             init-key, init-req?, init-val, specializer)
               = remote-slot-inspect(target, slot-descriptor);

        // We are interested in the getter function, but it might be #f.
        // If it is a <remote-value>, then build the correct environment 
	// object.

        if (getter)
          function-object
	    := make-environment-object-for-runtime-value
	         (application, getter)
        end if
      end method);

  // Return the environment object.
  function-object;
end method;


///// SLOT-SETTER (Environment Protocol Method)
//    Returns a <function-object> (if applicable) for the setter function
//    corresponding to this slot.

define method slot-setter 
    (application :: <dfmc-application>, slot :: <slot-object>)
         => (setter :: false-or(<function-object>))

  let target = application.application-target-app;
  let slot-proxy = slot.application-object-proxy;
  let function-object = #f;

  // Within a debugger transaction, call the DM to inspect the slot.
  // Build a <function-object> for the setter function, if it exists.

  perform-debugger-transaction
     (target,
      method ()
        let slot-descriptor = 
          runtime-proxy-to-remote-value(application, slot-proxy);

        // Call the DM's inspector for slots.

        let (basic-name, basic-type, owner-class, getter, setter,
             init-key, init-req?, init-val, specializer)
               = remote-slot-inspect(target, slot-descriptor);

        // We are interested in the setter function, but it might be #f.
        // If it is a <remote-value>, then build the correct environment 
	// object.

        if (setter)
          function-object 
	    := make-environment-object-for-runtime-value
	         (application, setter)
        end if
      end method);

  // Return the environment object.
  function-object;
end method;


///// SLOT-TYPE (Environment Protocol Method)
//    Returns a <type-object> for the specializer type corresponding to this
//    slot.

define method slot-type 
    (application :: <dfmc-application>, slot :: <slot-object>)
         => (type :: <type-object>)

  let target = application.application-target-app;
  let slot-proxy = slot.application-object-proxy;
  let type = #f;

  // Within a debugger transaction, call the DM to inspect the slot.
  // Build a <symbol-object> for the init-keyword, if it exists.

  perform-debugger-transaction
     (target,
      method ()
        let slot-descriptor = 
          runtime-proxy-to-remote-value(application, slot-proxy);

        // Call the DM's inspector for slots.

        let (basic-name, basic-type, owner-class, getter, setter,
             init-key, init-req?, init-val, specializer)
               = remote-slot-inspect(target, slot-descriptor);

        type := make-environment-object-for-runtime-value
                   (application, specializer);

      end method);

  type        
end method;

/* -- This is currently commented-out at the protocol level, so I need
      to mask away this method too, otherwise the compiler warns about
      it.

///// SLOT-INIT-VALUE (Environment Protocol Method)
//    If applicable, returns an <application-object> for the init-value
//    used in this slot. 

define method slot-init-value 
    (application :: <dfmc-application>, slot :: <slot-object>)
 => (value :: false-or(<environment-object>))

  let target = application.application-target-app;
  let slot-proxy = slot.application-object-proxy;
  let init = #f;

  // Within a debugger transaction, call the DM to inspect the slot.
  // Build a <symbol-object> for the init-keyword, if it exists.

  perform-debugger-transaction
     (target,
      method ()
        let slot-descriptor = 
          runtime-proxy-to-remote-value(application, slot-proxy);

        // Call the DM's inspector for slots.

        let (basic-name, basic-type, owner-class, getter, setter,
             init-key, init-req?, init-val, specializer)
               = remote-slot-inspect(target, slot-descriptor);

        if (init-val)
          init := make-environment-object-for-runtime-value
                     (application, init-val);
        end if;

      end method);

  init 
end method;
*/

///// SLOT-INIT-KIND (Environment Protocol Method)
//    Haven't the foggiest, actually!

define method slot-init-kind
    (application :: <dfmc-application>, slot :: <slot-object>)
         => (kind :: false-or(<symbol>))
  #"dunno-mate"
end method;


///// SLOT-INIT-KEYWORD (Environment Protocol Method)
//    Returns a <symbol-object> for the init-keyword corresponding to this
//    slot, if applicable. Also returns a flag to indicate whether the
//    keyword is required in order to initialize the slot.

define method slot-init-keyword 
    (application :: <dfmc-application>, slot :: <slot-object>)
         => (keyword :: false-or(<symbol>), required? :: <boolean>)
  //---*** andrewa: Paul will fix this at some time in the future,
  //---*** here's a fake implementation for now.
  values(#f, #f)
/*
  let target = application.application-target-app;
  let slot-proxy = slot.application-object-proxy;
  let symbol-object = #f;
  let required? = #f;

  // Within a debugger transaction, call the DM to inspect the slot.
  // Build a <symbol-object> for the init-keyword, if it exists.

  perform-debugger-transaction
     (target,
      method ()
        let slot-descriptor = 
          runtime-proxy-to-remote-value(application, slot-proxy);

        // Call the DM's inspector for slots.

        let (basic-name, basic-type, owner-class, getter, setter,
             init-key, init-req?, init-val, specializer)
               = remote-slot-inspect(target, slot-descriptor);

        // We are interested in the init-keyword, but it might be #f.
        // If it is a <remote-value>, build the correct environment object.

        if (init-key)
          symbol-object
	    := make-environment-object-for-runtime-value
	         (application, init-key)

          // And set the required? flag.
          required? := init-req?;
        end if
      end method);

  // Return the environment object, and the required? flag.
  values(symbol-object, required?)
*/
end method;


///// SLOT-ALLOCATION (Environment Protocol Method)
//    What??

define method slot-allocation
    (application :: <dfmc-application>, slot :: <slot-object>)
         => (keywords :: <sequence>)
  #[]
end method;
