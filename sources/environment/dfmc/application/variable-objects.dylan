Module:    dfmc-application
Synopsis:  <variable-object> environment protocols from the application.
Author:    Bill Chiles, Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// <APPLICATION-VARIABLE> (Server proxy class)
//    Used as the application-object proxy for <VARIABLE-OBJECT>.

define abstract class <application-variable> (<object>)

  constant slot application-variable-name :: <byte-string>,
    required-init-keyword: name:;

  constant slot application-variable-namespace :: <dylan-name-context>,
    required-init-keyword: namespace:;

  constant slot application-variable-address :: <remote-value>,
    required-init-keyword: address:;

end class;

define class <application-thread-global-variable> (<application-variable>)
end class;

define class <application-thread-local-variable> (<application-variable>)
end class;


///// GET-APPLICATION-VARIABLE-VALUE (Internal)
//    Must be called within a debugger transaction. Used to get the
//    value of a variable.

define method get-application-variable-value
    (application :: <dfmc-application>,
     proxy :: <application-thread-global-variable>,
     #key thread = #f)
       => (variable-value :: <remote-value>)
  let target = application.application-target-app;
  ignore(proxy.application-variable-name);
  ignore(proxy.application-variable-namespace);
  read-dylan-value(target, proxy.application-variable-address);
end method;

// TODO:
// "The right thing" for this method!
// In this case, the "value" of the variable is only an integer-encoded
// offset into the thread's local variable block. We need to call the
// spy to read the value from that offset.

define method get-application-variable-value
    (application :: <dfmc-application>,
     proxy :: <application-thread-local-variable>,
     #key thread = #f)
       => (variable-value :: <remote-value>)
  let target = application.application-target-app;
  if (thread)
    evaluate-thread-local-variable(target,
                                   thread,
                                   proxy.application-variable-address);
  else
    read-dylan-value(target, proxy.application-variable-address);
  end if;
end method;

/* -- Currently not needed by the environment.

///// SET-APPLICATION-VARIABLE-VALUE (Internal)
//    Must be called within a debugger transaction. Used to set the
//    value of a variable.

define method set-application-variable-value
    (application :: <dfmc-application>,
     proxy :: <application-thread-global-variable>, value :: <remote-value>,
     #key thread = #f)
       => ()
  let target = application.application-target-app;
  write-dylan-value(target, proxy.application-variable-address, value);
end method;

// TODO:
// "The right thing" as well.

define method set-application-variable-value
    (application :: <dfmc-application>,
     proxy :: <application-thread-local-variable>, value :: <remote-value>,
     #key thread = #f)
       => ()
  let target = application.application-target-app;
  if (thread)
    set-thread-local-variable
      (target, thread, proxy.application-variable-address, value);
  else
    write-dylan-value(target, proxy.application-variable-address, value);
  end if;
end method;
*/

///// VARIABLE-VALUE (Environment Protocol Method)
//    Returns an environment object being the value of a variable.

define method variable-value (application :: <dfmc-application>,
                              variable :: <global-variable-object>,
                              #key thread = #f)
    => (value :: false-or(<application-object>));

  let target = application.application-target-app;
  let value = #f;

  perform-debugger-transaction
    (target,
     method () => ()
       let proxy =
          ensure-application-global-variable-proxy(application, variable);
       let remote-value =
         if (proxy)
           get-application-variable-value(application, proxy);
         else
           dylan-runtime-unbound-marker(target)
         end if;
       value := make-environment-object-for-runtime-value
                   (application, remote-value);
     end);

  value;
end method;


define method variable-value (application :: <dfmc-application>,
                              variable :: <constant-object>,
                              #key thread = #f)
    => (value :: false-or(<application-object>));

  let target = application.application-target-app;
  let value = #f;

  // At the moment, constants are proxied in the same way as
  // thread-global variables, so this method is identical. This
  // may need to change one day.

  perform-debugger-transaction
    (target,
     method () => ()
       let proxy =
          ensure-application-global-variable-proxy(application, variable);
       let remote-value =
         if (proxy)
           get-application-variable-value(application, proxy);
         else
           dylan-runtime-unbound-marker(target)
         end if;
       value := make-environment-object-for-runtime-value
                   (application, remote-value);
     end);

  value;
end method;


define method variable-value (application :: <dfmc-application>,
                              variable :: <thread-variable-object>,
                              #key thread = #f)
    => (value :: false-or(<application-object>));

  let target = application.application-target-app;
  let value = #f;

  perform-debugger-transaction
    (target,
     method () => ()
       let proxy =
           ensure-application-thread-variable-proxy(application, variable);
       let remote-value =
         if (proxy)
           get-application-variable-value
              (application, proxy, thread: thread.application-object-proxy);
         else
           dylan-runtime-unbound-marker(target)
         end if;
       value := make-environment-object-for-runtime-value
                   (application, remote-value);
     end);

  value;
end method;


///// VARIABLE-TYPE (Environment Protocol Method)
//    Returns an environment object being the type of a variable. I think
//    it's unlikely that the project will ever dispatch to this method,
//    hence the shorthanded implementation.
//    (This method is currently also the only one provided for
//    <local-variable-object> as well).

define method variable-type
    (application :: <dfmc-application>, variable :: <variable-object>)
        => (type :: <type-object>)
  application-object-class
      (application, variable-value(application, variable));
end method;
