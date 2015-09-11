Module:       access-path-test-suite
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2015 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $debugger-wait-timeout = 2000;

define test access-path-basic-test ()
  let access-path = #f;
  check-no-errors("instantiate access path",
                  access-path := make-test-instance(<access-path>));

  restart(access-path);

  local
    method wait-for-stop-reason-aux(access-path :: <access-path>) => (stop-reason :: <stop-reason>);
      let stop-reason = wait-for-stop-reason(access-path, timeout: $debugger-wait-timeout);
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

  continue(access-path);
  check-instance?("Stop at process exit",
                  <exit-process-stop-reason>,
                  wait-for-stop-reason-aux(access-path));

  check-no-errors("close access path",
                  close-application(access-path));
end test;
