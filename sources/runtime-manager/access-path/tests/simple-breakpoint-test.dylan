Module:       access-path-test-suite
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2015 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $debugger-wait-timeout = 2000;

define constant $test-module-name = "debugger-test-target-app";
define constant $test-library-name = "debugger-test-target-app";

define constant $iep-mangler = make(<mangler-with-options>, iep-extension: #t);

define function mangle-iep-name
    (binding-name :: <string>,
     module-name :: <string>,
     library-name :: <string>)
 => (mangled :: <string>);
  mangle-binding-spread($iep-mangler, binding-name, module-name, library-name)
end function;

define test simple-breakpoint-test ()
  let access-path = #f;
  check-no-errors("Instantiate access path",
                  access-path := make-test-access-path("simple-breakpoint"));

  restart(access-path);

  local
    method wait-for-stop-reason-aux(access-path :: <access-path>) => (stop-reason :: <stop-reason>);
      let stop-reason
        = wait-for-stop-reason(access-path, timeout: $debugger-wait-timeout);
      if (instance?(stop-reason, <load-library-stop-reason>))
        continue(access-path);
        wait-for-stop-reason-aux(access-path)
      elseif (~stop-reason)
        error("Timed out waiting for stop");
      else
        stop-reason
      end if;
    end method;

  check-instance?("Stop at process creation",
                  <create-process-stop-reason>,
                  wait-for-stop-reason-aux(access-path));

  continue(access-path);
  check-instance?("Stop at system initialized",
                  <system-initialized-stop-reason>,
                  wait-for-stop-reason-aux(access-path));

  let name
    = mangle-iep-name("simple-breakpoint",
                      $test-module-name,
                      $test-library-name);
  let place = find-symbol(access-path, name);
  check-true("Symbol for simple-breakpoint found", place);
  check-true("Set breakpoint at simple-breakpoint",
             enable-breakpoint(access-path,
                               first-frame-breakable-address(place)));

  continue(access-path);
  check-instance?("Stop at breakpoint",
                  <breakpoint-stop-reason>,
                  wait-for-stop-reason-aux(access-path));

  continue(access-path);
  check-instance?("Stop at process exit",
                  <exit-process-stop-reason>,
                  wait-for-stop-reason-aux(access-path));

  check-no-errors("Close access path",
                  close-application(access-path));
end test;
