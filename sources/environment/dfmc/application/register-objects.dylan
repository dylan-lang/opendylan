Module:    dfmc-application
Synopsis:  Implementation of the <register-object> class and its protocols.
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// DO-APPLICATION-REGISTERS (Environment Protocols)

define method do-application-registers
    (f :: <function>, application :: <dfmc-application>,
     #key category = #f)
 => ()

  local method wrap-for-env 
                 (reg :: <remote-register>) => (obj :: <register-object>)
          make-environment-object
            (<register-object>,
             project: application.server-project,
             application-object-proxy: reg)
        end method;

  local method convert-category
                 (env-cat :: false-or(<register-category>)) 
                    => (ap-cat :: false-or(<symbol>))
          select(env-cat)
            #"general-purpose"   => #"general";
            #"special-purpose"   => #"special";
            #"floating-point"    => #"floating";
            #f                   => #f;
          end select
        end method;

  let target = application.application-target-app;
  perform-debugger-transaction
    (target,
     method ()
       let path = target.debug-target-access-path;
       do-registers
         (method (r :: <remote-register>) => ()
            f(wrap-for-env(r))
          end method,
          path,
          type: convert-category(category))
     end method);
end method;


///// REGISTER-CONTENTS (Environment Protocols)

define method register-contents
    (application :: <dfmc-application>, reg :: <register-object>,
     thread :: <thread-object>,
     #key stack-frame-context = #f)
 => (obj :: false-or(<application-object>))
  let target = application.application-target-app;
  let obj = #f;
  perform-debugger-transaction
    (target,
     method ()
       let path = target.debug-target-access-path;
       let reg-proxy = reg.application-object-proxy;
       let remote-thread = thread.application-object-proxy;
       let frame = stack-frame-context & 
                   stack-frame-context.application-object-proxy;
       let low-level-frame =
         if (instance?(frame, <call-frame>))
           call-frame-description(target, frame)
         end if;
       let context-sensitive-register = 
         active-register(path, remote-thread, reg-proxy);
       let value = 
         read-value(path, context-sensitive-register,
                    stack-frame: low-level-frame);
       obj :=
         make-environment-object-for-runtime-value(application, value);
     end method);
  obj
end method;


///// REGISTER-CONTENTS-ADDRESS (Environment Protocols)

define method register-contents-address
    (application :: <dfmc-application>, reg :: <register-object>,
     thread :: <thread-object>,
     #key stack-frame-context = #f)
 => (obj :: false-or(<address-object>))
  let target = application.application-target-app;
  let obj = #f;
  perform-debugger-transaction
    (target,
     method ()
       let path = target.debug-target-access-path;
       let reg-proxy = reg.application-object-proxy;
       let remote-thread = thread.application-object-proxy;
       let frame = stack-frame-context & 
                   stack-frame-context.application-object-proxy;
       let low-level-frame =
         if (instance?(frame, <call-frame>))
           call-frame-description(target, frame)
         end if;
       let context-sensitive-register = 
         active-register(path, remote-thread, reg-proxy);
       let value = 
         read-value(path, context-sensitive-register,
                    stack-frame: low-level-frame);
       obj := make-environment-object(<address-object>,
                                      project: application.server-project,
                                      application-object-proxy: value);
     end method);
  obj
end method;


///// LOOKUP-REGISTER-BY-NAME (Environment Protocols)

define method lookup-register-by-name
    (application :: <dfmc-application>, name :: <string>)
  => (reg :: false-or(<register-object>))
  let reg = #f;
  let target = application.application-target-app;
  perform-debugger-transaction
    (target,
     method ()
       let path = target.debug-target-access-path;
       block ()
         let ap-reg =
           enumeration-code-to-register(path, name);
         reg := 
           make-environment-object(<register-object>,
                                   project: application.server-project,
                                   application-object-proxy: ap-reg);
       exception(<error>)
         // TODO:
         // Obviously, this blanket error-catching is wrong! Unfortunately,
         // the underlying access-path API signals an undisciplined error
         // if the lookup fails. The preferred fix would be to have the
         // access-path library signal a _documented_ condition class,
         // which we can just catch instead of <error>.
         // However, the following is still the correct course of
         // action.
         reg := #f;
       end block;
     end method);
  reg
end method;
