module:    threads-internal
Synopsis:  The implementation of the <notification> class
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define sealed class <notification> (<portable-container>, <synchronization>)

  /* constant */ slot associated-lock :: <simple-lock>,
    required-init-keyword: lock:;

end class;


define sealed domain synchronization-name (<notification>);


// A little grounding goes a long way.
define constant make-notification = method (lock :: <simple-lock>)
 => (notification :: <notification>);
  let instance :: <notification> = system-allocate-simple-instance(<notification>, fill: #f);
  associated-lock(instance) := lock;
  initialize-notification(instance);
  instance
end method;

define constant initialize-notification =
  method (notif :: <notification>) => ()
    drain-finalization-queue();
    let res = primitive-make-notification(notif, notif.synchronization-name);
    check-synchronization-creation(notif, res);
    finalize-when-unreachable(notif);
  end method;

define sealed method initialize (notif :: <notification>, #key) => ()
  next-method();
  initialize-notification(notif);
end method;


define function notification-release-result
    (notif :: <notification>, res :: <integer>) => ()
  unless (res == $success)
    select (res)
      $unlocked => error(make(<not-owned-error>,
                              lock: notif.associated-lock));
      otherwise => error(make(<unexpected-synchronization-error>,
                              synchronization: notif));
    end select;
  end unless;
end function notification-release-result;


define sealed method release (notif :: <notification>, #key) => ()
  let res = primitive-release-notification(notif, notif.associated-lock);
  notification-release-result(notif, res);
end method;


define function release-all (notif :: <notification>, #key) => ()
  let res = primitive-release-all-notification(notif, notif.associated-lock);
  notification-release-result(notif, res);
end;


define sealed method wait-for (notif :: <notification>, #key timeout) => (success?)
  let lock = notif.associated-lock;
  let res
    = if (timeout)
        primitive-wait-for-notification-timed(notif, lock, timeout.millisecs)
      else
        primitive-wait-for-notification(notif, lock)
      end if;
  select (res)
    $success => #t;
    $timeout => #f;
    $unlocked => error(make(<not-owned-error>, lock: lock));
    otherwise => error(make(<unexpected-synchronization-error>,
                            synchronization: notif));
  end select;
end method;


define method finalize (notif :: <notification>) => ()
  let res = primitive-destroy-notification(notif);
  check-synchronization-finalization(notif, res);
end method;

