module:       dm-internals
synopsis:     The registration of remote objects for tracking by the
              garbage collector
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <RUNTIME-REGISTERED-HANDLE>
//    Just a wrapper for the "bare" type <REGISTERED-HANDLE>.
//    The name's a bit crap, though...

define abstract class <runtime-registered-handle> (<object>)

  constant slot runtime-handle :: <remote-value>,
    required-init-keyword: handle:;

end class;


///// <OBJECT-REGISTRATION-ERROR>
//    Signalled when these functions fail.

define class <object-registration-error> (<error>)
end class;


///// <REMOTE-OBJECT>
//    And the various permutations of finalizable and weakly registered
//    remote objects.

define abstract class <remote-object> (<runtime-registered-handle>)

  constant slot remote-object-debug-target :: <debug-target>,
    required-init-keyword: debug-target:;

  slot explicitly-freed? :: <boolean>,
    init-value: #f;

end class;

define abstract class <finalized-remote-object> (<remote-object>)
end class;

define class <only-finalized-remote-object> (<finalized-remote-object>)
end class;

define abstract class <weak-remote-object> (<remote-object>)
end class;

define class <only-weak-remote-object> (<weak-remote-object>)
end class;

define class <normal-remote-object> (<remote-object>)
end class;

define class <weak-finalized-remote-object>
               (<weak-remote-object>, <finalized-remote-object>)
end class;


///// REGISTER-REMOTE-OBJECT

define method register-remote-object
   (application :: <debug-target>, value :: <remote-value>,
    #key finalize = #t, weak = #f, thread = #f)
       => (robj :: <remote-object>)
  let spy-thread =
    thread | select-thread-for-spy(application);
  if (weak)
    if (finalize)
      create-registered-object
        (application, value, spy-thread, #"weak-finalized")
    else
      create-registered-object
        (application, value, spy-thread, #"weak")
    end if
  else
    if (finalize)
      create-registered-object
        (application, value, spy-thread, #"finalized")
    else
      create-registered-object
        (application, value, spy-thread, #"normal")
    end if
  end if
end method;


///// FREE-REMOTE-OBJECT

define method free-remote-object
    (application :: <debug-target>,
     robj :: <weak-remote-object>,
     #key thread = #f) 
       => ()
  unless (robj.explicitly-freed?)
    let spy-thread =
      thread | select-thread-for-spy(application);
    run-spy-on-thread(application,
                      spy-thread,
                      application.dylan-spy.remote-object-weak-free,
                      robj.runtime-handle);
    robj.explicitly-freed? := #t;
  end unless;
end method;

define method free-remote-object
    (application :: <debug-target>,
     robj :: <remote-object>,
     #key thread = #f) 
       => ()
  unless (robj.explicitly-freed?)
    let spy-thread =
      thread | select-thread-for-spy(application);
    run-spy-on-thread(application,
                      spy-thread,
                      application.dylan-spy.remote-object-strong-free,
                      robj.runtime-handle);
    robj.explicitly-freed? := #t;
  end unless;
end method;


///// REMOTE-OBJECT-VALUE

define method remote-object-value
  (application :: <debug-target>,
   robj :: <remote-object>,
   #key thread = #f) 
      => (val-or-false :: false-or (<remote-value>))
 
  if (robj.explicitly-freed?)
    #f
  else
    let spy-thread = thread | select-thread-for-spy(application);
    run-spy-on-thread(application, spy-thread,
                      application.dylan-spy.remote-object-strong-lookup,
                      robj.runtime-handle);
  end if;
end method;

define method remote-object-value
  (application :: <debug-target>,
   robj :: <weak-remote-object>,
   #key thread = #f) 
      => (val-or-false :: false-or (<remote-value>))
 
  if (robj.explicitly-freed?)
    #f
  else
    let spy-thread = thread | select-thread-for-spy(application);
    run-spy-on-thread(application, spy-thread,
                      application.dylan-spy.remote-object-weak-lookup,
                      robj.runtime-handle);
  end if;
end method;


///// CREATE-REGISTERED-OBJECT
//    Performs the work of registering the remote object, weakly if
//    necessary and finalizing if necessary. Hence there are four
//    permutations, implemented as four methods on a GF, with a
//    symbol argument used to discriminate.

define method create-registered-object
    (application :: <debug-target>, value :: <remote-value>,
     spy-thread :: <remote-thread>, style == #"normal")
       => (rob)
  let handle =
    run-spy-on-thread(application,
                      spy-thread,
                      application.dylan-spy.remote-object-strong-register,
                      value);
  if (handle)
    make(<normal-remote-object>,
         handle: handle,
         debug-target: application);
  else
    signal(make(<object-registration-error>));
    #f;
  end if
end method;

define method create-registered-object
    (application :: <debug-target>, value :: <remote-value>,
     spy-thread :: <remote-thread>, style == #"weak")
       => (rob)
  let handle =
    run-spy-on-thread(application,
                      spy-thread,
                      application.dylan-spy.remote-object-weak-register,
                      value);
  if (handle)
    make(<only-weak-remote-object>,
         handle: handle,
         debug-target: application);
  else
    signal(make(<object-registration-error>));
    #f;
  end if
end method;

define method create-registered-object
    (application :: <debug-target>, value :: <remote-value>,
     spy-thread :: <remote-thread>, style == #"finalized")
       => (rob)
  let handle =
    run-spy-on-thread(application,
                      spy-thread,
                      application.dylan-spy.remote-object-strong-register,
                      value);
  if (handle)
    let obj =
      make(<only-finalized-remote-object>,
           handle: handle,
           debug-target: application);
    terminate-when-unreachable(obj);
    obj;
  else
    signal(make(<object-registration-error>));
    #f;
  end if
end method;

define method create-registered-object
    (application :: <debug-target>, value :: <remote-value>,
     spy-thread :: <remote-thread>, style == #"weak-finalized")
       => (rob)
  let handle =
    run-spy-on-thread(application,
                      spy-thread,
                      application.dylan-spy.remote-object-weak-register,
                      value);
  if (handle)
    let obj =
      make(<weak-finalized-remote-object>,
           handle: handle,
           debug-target: application);
    terminate-when-unreachable(obj);
    ignore(obj.remote-object-debug-target);
    obj;
  else
    signal(make(<object-registration-error>));
    #f;
  end if
end method;


///// OBJECT-REQUIRES-REGISTRATION?
//    Given an object known to be a DYLAN object, determines whether the
//    object is subject to any relocations by the garbage collector. At
//    the moment, registration is deemed unnecessary for:

//        a.) Tagged immediate dylan objects (integers and characters).
//        b.) Untagged dylan objects that have precise symbolic names that
//            begin with a capital 'K'.

//    TODO: (b) might be considered suspect. Perhaps a predicate in the Spy
//          would be better.

//    It does not make sense to call this function for a foreign object.

define method object-requires-registration?
    (application :: <debug-target>, instance :: <remote-value>)
        => (answer :: <boolean>)
  let classification = classify-dylan-object(application, instance);
  select (classification)
    $integer-type => #f;
    $character-type => #f;
    otherwise =>
      let (sym, offset) = symbol-table-symbol-relative-address
                              (application.debug-target-symbol-table,
                               instance);
      if (sym & (offset == 0) & ((sym.remote-symbol-name[0] = 'K') |
                                 (sym.remote-symbol-name[0] = '_')))
        #f
      else
        #t
      end if;
  end select;
end method;


///// REGISTER-INTERACTIVE-CODE
//    A provision for the interactive downloader.
//    Allows a portion of memory (delimited by two <remote-value> addresses)
//    to be registered with the access path as containing dynamically
//    created code. The debugger nub needs to know about such regions when
//    tracing the stack.

define method register-interactive-code
    (application :: <debug-target>, lo :: <remote-value>, hi :: <remote-value>)
      => ()
  register-interactive-code-segment
    (application.debug-target-access-path, lo, hi);
end method;


///// REGISTER-EXACT-ROOTS
//    A provision for the interactive downloader.
//    Allows a portion of memory (delimited by two <remote-value> addresses)
//    to be registered with the memory manager as contiguous value cells,
//    ie. known references to Dylan objects.

define method register-exact-roots
    (application :: <debug-target>, lo :: <remote-value>, hi :: <remote-value>,
     #key thread = #f)
       => ()
  let spy-thread = thread | select-thread-for-spy(application);
  run-spy-on-thread(application,
                    spy-thread,
                    application.C-spy.mm-root-exact,
                    lo,
                    hi);
end method;


///// REGISTER-STATIC-ROOTS
//    A provision for the interactive downloader.
//    Allows a portion of memory (delimited by two <remote-value> addresses)
//    to be registered with the memory manager as a section of initialized
//    Dylan data, ie. statically heaped Dylan objects.

define method register-static-roots
    (application :: <debug-target>, lo :: <remote-value>, hi :: <remote-value>,
     #key thread = #f)
       => ()
  let spy-thread = thread | select-thread-for-spy(application);
  run-spy-on-thread(application,
                    spy-thread,
                    application.C-spy.mm-root-static,
                    lo,
                    hi);
end method;


///// REGISTER-AMBIGOUS-ROOTS
//    A provision for the interactive downloader.
//    Allows a portion of memory (delimited by two <remote-value> addresses)
//    to be registered with the memory manager as a section of initialized
//    data which is ambiguous, and must therefore be conservatively scanned
//    by the collector.

define method register-ambiguous-roots
    (application :: <debug-target>, lo :: <remote-value>, hi :: <remote-value>,
     #key thread = #f)
       => ()
  let spy-thread = thread | select-thread-for-spy(application);
  run-spy-on-thread(application,
                    spy-thread,
                    application.C-spy.mm-root-ambig,
                    lo,
                    hi);
end method;


///// FIXUP-IMPORTED-DATA-REGION
//    A provision for the interactive downloader.

define method fixup-imported-data-region
    (application :: <debug-target>, lo :: <remote-value>, hi :: <remote-value>,
     #key thread = #f)
       => ()
  let spy-thread = thread | select-thread-for-spy(application);
  run-spy-on-thread(application,
                    spy-thread,
                    application.C-spy.fixup-imported-data,
                    lo,
                    hi);
end method;

define method fixup-unimported-data-region
    (application :: <debug-target>, lo :: <remote-value>, hi :: <remote-value>,
     #key thread = #f)
       => ()
  let spy-thread = thread | select-thread-for-spy(application);
  run-spy-on-thread(application,
                    spy-thread,
                    application.C-spy.fixup-unimported-data,
                    lo,
                    hi);
end method;


// TODO:
// Remove these two functions when finalization arrives on the scene.

///// TERMINATE-WHEN-UNREACHABLE
//    Dummy stub for finalizations

define method terminate-when-unreachable (anything)
end method;


