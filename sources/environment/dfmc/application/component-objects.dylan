Module:    dfmc-application
Synopsis:  Implementation of the <component-object> protocols.
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// COMPONENT-IMAGE-FILENAME (Environment Protocols)

define method component-image-filename
    (application :: <dfmc-application>, component :: <component-object>)
 => (name :: <file-locator>)
  as(<file-locator>, component.application-object-proxy.library-image-name)
end method;


///// COMPONENT-VERSION (Environment Protocols)

define method component-version
    (application :: <dfmc-application>, component :: <component-object>)
 => (major-version :: <integer>, minor-version :: <integer>)
  component.application-object-proxy.library-version
end method;


///// LOOKUP-COMPONENT-BY-NAME (Environment Protocols)

define method lookup-component-by-name
    (application :: <dfmc-application>, name :: <string>)
 => (component :: false-or(<component-object>))
  let target = application.application-target-app;
  let component = #f;

  local method wrap-for-env (l :: <remote-library>) => (c :: <component-object>)
          make-environment-object(<component-object>,
                                  project: application.server-project,
                                  application-object-proxy: l)
        end method;

  perform-debugger-transaction
    (target,
     method ()
       let path = target.debug-target-access-path;
       component :=
         block(return)
           do-libraries(method (l :: <remote-library>) => ()
                          if (l.library-core-name = name)
                            return(wrap-for-env(l))
                          end if
                        end method,
                        path);
           return(#f)
         end block
     end method);
  component
end method;


///// DO-APPLICATION-COMPONENTS (Environment Protocols)

define method do-application-components
    (f :: <function>, application :: <dfmc-application>)
 => ()
  let target = application.application-target-app;

  local method wrap-for-env (l :: <remote-library>) => (c :: <component-object>)
          make-environment-object(<component-object>,
                                  project: application.server-project,
                                  application-object-proxy: l)
        end method;

  perform-debugger-transaction
    (target,
     method ()
       let path = target.debug-target-access-path;
       do-libraries(method (l :: <remote-library>) => ()
                      f(wrap-for-env(l))
                    end method,
                    path);
     end method);
end method;
