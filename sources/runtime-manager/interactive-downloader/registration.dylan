module:       interactive-downloader-internals
synopsis:     Regstration of interactive memory with the Memory Manager
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// PERFORM-ALL-REGISTRATIONS
//    By now, the relocations have been performed, and the sections of
//    memory that require GC registration can in fact be registered.
//    The DM provides functions to actually perform the registration -
//    which it implements by calling SPY functions.

define method perform-all-registrations (trans :: <interactive-transaction>)
     => ()
  let application = 
    trans.transaction-downloader-target.interactive-application;
  let thread = trans.transaction-thread;
  for (registration in trans.transaction-deferred-registrations)
    let lo = registration.memory-registration-lower-address;
    let hi = registration.memory-registration-upper-address;
    select (registration.memory-registration-style)
      $registration-style-fixup =>
         fixup-imported-data-region(application, lo, hi, thread: thread);
      $registration-style-import =>
         fixup-unimported-data-region(application, lo, hi, thread: thread);
      $registration-style-exact =>
         register-exact-roots(application, lo, hi, thread: thread);
      $registration-style-ambiguous =>
         register-ambiguous-roots(application, lo, hi, thread: thread);
      $registration-style-static =>
         register-static-roots(application, lo, hi, thread: thread);
      $registration-style-code =>
         register-interactive-code(application, lo, hi);
    end select
  end for;
end method;
