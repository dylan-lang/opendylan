Module:    dfmc-application
Synopsis:  <user-object> environment protocols from the application.
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// USER-OBJECT-SLOT-VALUE
//   Returns the value of a particular slot in a general composite
//   dylan object.

define method user-object-slot-value
    (application :: <dfmc-application>, obj :: <user-object>,
     slt :: <definition-id>,
     #key repeated-element = 0)
 => (val :: false-or(<environment-object>))

  let target = application.application-target-app;

  // Within a debugger transaction, explosively inspect the user's object.
  // This will give us pairs of slot names and slot values. The supplied
  // slot object will also have a name in the runtime, hence we try to
  // find a match between the desired slot name, and the name of a slot
  // in the instance. If we find a match, we return and environment object
  // for the corresponding slot value.

  with-debugger-transaction (target)
    let proxy = obj.application-object-proxy;
    let class-proxy
      = exchange-value-proxy-for-browsable-class-proxy(application, proxy);
    if (class-proxy)
      let proxy-value = runtime-proxy-to-remote-value(application, proxy);
      let (class, incarnation, current-incarnation, immediate?)
	= dylan-object-class(target, proxy-value);
      let (slots, navigation, repeat, count-offset, element-size,
	   element-offset)
	= class-proxy-browser-information
	    (application, class-proxy, incarnation: incarnation);
      
      // If this function has succeeded, 'target-val' should now hold a
      // <remote-value>. However, the logic of this function is such that
      // it might still hold #f. In the latter case, we can't build an
      // environment object.

      let slot-name = slt.id-name;
      let offset = element(navigation, slot-name, default: #f);

      if (offset)
	let target-val
	  = read-dylan-value(target, 
			     indexed-remote-value(proxy-value, offset));
	make-environment-object-for-runtime-value(application, target-val)
      end
    end
  end
end method user-object-slot-value;

define method user-object-slot-value
    (application :: <dfmc-application>, obj :: <user-object>,
     slt :: <slot-object>, 
     #key repeated-element = 0)
 => (val :: false-or(<environment-object>))

  let target = application.application-target-app;
 
  // Within a debugger transaction, explosively inspect the user's object.
  // This will give us pairs of slot names and slot values. The supplied
  // slot object will also have a name in the runtime, hence we try to
  // find a match between the desired slot name, and the name of a slot
  // in the instance. If we find a match, we return and environment object
  // for the corresponding slot value.

  with-debugger-transaction (target)
    let proxy = obj.application-object-proxy;
    let class-proxy
      = exchange-value-proxy-for-browsable-class-proxy(application, proxy);
    if (class-proxy)
      let proxy-value = runtime-proxy-to-remote-value(application, proxy);
      let slot-proxy = slt.application-object-proxy;
      let actual-slot-descriptor
	= runtime-proxy-to-remote-value(application, slot-proxy);
      let (class, incarnation, current-incarnation, immediate?)
	= dylan-object-class(target, proxy-value);
      let (slots, navigation, repeat, count-offset, element-size,
	   element-offset)
	= class-proxy-browser-information
	    (application, class-proxy, incarnation: incarnation);
      let slot-name = remote-slot-inspect(target, actual-slot-descriptor);
      let offset = element(navigation, slot-name, default: #f);
      if (offset)
	let target-val
	  = read-dylan-value(target, 
			     indexed-remote-value(proxy-value, offset));
	make-environment-object-for-runtime-value(application, target-val)
      end
    end
  end
end method user-object-slot-value;
