module:    threads-internal
Synopsis:  Condition class definitions for the threads library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define abstract class <thread-error> (<error>)
  constant slot thread, required-init-keyword: thread:
end class;

// This slot is not actually needed, but keep it for the keyword...
ignore(thread);

define class <thread-inactive-error> (<thread-error>)
end class;

define class <duplicate-join-error> (<thread-error>)
end class;

define class <thread-creation-error> (<thread-error>)
end class;

define class <thread-priority-error> (<thread-error>)
end class;

define class <unexpected-thread-error> (<thread-error>)
end class;

define class <thread-finalization-error> (<thread-error>)
end class;


define class <timeout-expired> (<serious-condition>)
  constant slot synchronization, required-init-keyword: synchronization:
end class;

// This slot is not actually needed, but keep it for the keyword...
ignore(synchronization);


define abstract class <synchronization-error> (<error>)
  constant slot synchronization, required-init-keyword: synchronization:
end class;

define class <unexpected-synchronization-error> (<synchronization-error>)
end class;

define class <synchronization-creation-error> (<synchronization-error>)
end class;

define class <synchronization-finalization-error> (<synchronization-error>)
end class;



define abstract class <lock-error> (<error>)
  constant slot lock, required-init-keyword: lock:
end class;

// This slot is not actually needed, but keep it for the keyword...
ignore(lock);

define class <count-exceeded-error> (<lock-error>)
end class;

define class <not-owned-error> (<lock-error>)
end class;

define class <already-owned-error> (<lock-error>)
end class;


define class <conditional-update-error> (<error>)
end class;


