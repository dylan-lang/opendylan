module:      dm-internals
synopsis:    Describing dylan libraries in the target application,
             and tracking their static and dynamic initializations.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <INITIALIZATION-STATE>
//    Describes the states in which a dylan library can be.

define constant <initialization-state> =
   one-of(#"uninitialized", 
          #"statically-initialized", 
          #"dynamically-initialized",
          #"unloaded",
          #"foreign");


///// <LIBRARY-INITIALIZATION-PHASE>
//    Either START or END.

define constant <library-initialization-phase> = one-of(#"start", #"end");


///// HANDLE-LIBRARY-INITIALIZATION-PHASE
//    An open generic to allow clients to perform specific processing
//    when various libraries begin and end their initializations.
//    (The default method does nothing, and registers no interest).

define open generic handle-library-initialization-phase
    (application :: <debug-target>, thread :: <remote-thread>,
     remote-library :: <remote-library>, 
     phase :: <library-initialization-phase>, top-level? :: <boolean>)
 => (interested? :: <boolean>);

define method handle-library-initialization-phase
    (application :: <debug-target>, thread :: <remote-thread>,
     remote-library :: <remote-library>, 
     phase :: <library-initialization-phase>, top-level? :: <boolean>)
 => (interested? :: <boolean>)
  #f
end method;


///// <LIBRARY-INITIALIZATION-TRACKER>
//    Contains all of the information to associate <remote-library> with
//    <dylan-library>, and the information necessary to track the
//    initialization of libraries.

define abstract class <library-initialization-tracker> (<object>)

  constant slot tracker-debug-target :: <debug-target>,
    required-init-keyword: debug-target:;

  constant slot tracker-remote-library :: <remote-library>,
    required-init-keyword: remote-library:;

  slot tracker-initialization-state :: <initialization-state>;

  constant slot tracker-top-level? :: <boolean>,
    required-init-keyword: top-level?:;

end class;

define class <foreign-library-initialization-tracker> 
    (<library-initialization-tracker>)

  inherited slot tracker-initialization-state,
    init-value: #"foreign";

end class;

define class <dylan-library-initialization-tracker> 
    (<library-initialization-tracker>)

  inherited slot tracker-initialization-state,
    init-value: #"uninitialized";

  constant slot tracker-initialization-symbol :: <remote-symbol>,
    required-init-keyword: initializer-function:;

end class;


///// INITIALIZE (<DYLAN-LIBRARY-INITIALIZATION-TRACKER>)

define sealed method initialize
    (entry-init :: <dylan-library-initialization-tracker>, #key) => ()
  let lib = entry-init.tracker-remote-library;
  let target = entry-init.tracker-debug-target;
  let path = target.debug-target-access-path;
  let core = lib.library-core-name;
  let init-fun = entry-init.tracker-initialization-symbol;
  let tracer = make(<starting-dynamic-initialization>,
                    address: init-fun.remote-symbol-address,
                    callback: dynamic-initializer-start-callback,
                    return-callback: dynamic-initializer-done-callback,
                    tracker: entry-init);
  register-debug-point(target, tracer);
end method;


///// <STARTING-DYNAMIC-INITIALIZATION>
//    A one-shot entry tracepoint that is signalled when a dylan library
//    begins its dynamic initialization. When this tracepoint is
//    signalled, the library is known to be statically initialized.

define class <starting-dynamic-initialization> (<entry-tracepoint>)

  constant 
    slot 
      entry-initialization-tracker :: <dylan-library-initialization-tracker>,
      required-init-keyword: tracker:;

end class;


///// <DONE-DYNAMIC-INITIALIZATION>
//    A one-shot exit tracepoint that is signalled when a dylan library
//    has finished its dynamic initialization.

define class <done-dynamic-initialization> (<return-tracepoint>)

  constant
    slot 
      exit-initialization-tracker :: <dylan-library-initialization-tracker>,
      required-init-keyword: tracker:;

end class;


///// MAKE-RETURN-TRACEPOINT
//    Instructs the DM to make a <done-dynamic-initialization> object
//    when a <starting-dynamic-initialization> tracepoint is signalled.

define sealed method make-return-tracepoint
    (application :: <debug-target>, entry :: <starting-dynamic-initialization>,
     thread :: <remote-thread>, #rest keys, #key, #all-keys)
 => (ret :: <done-dynamic-initialization>)
  apply(make, <done-dynamic-initialization>, 
        tracker: entry.entry-initialization-tracker,
        keys)
end method;


///// DYNAMIC-INITIALIZER-START-CALLBACK
//    The callback routine for the entry tracepoint.

define method dynamic-initializer-start-callback
    (application :: <debug-target>, start :: <starting-dynamic-initialization>,
     thread :: <remote-thread>)
 => (interested? :: <boolean>)
  ignore
    (start.entry-initialization-tracker.tracker-initialization-state);
  start.entry-initialization-tracker.tracker-initialization-state :=
     #"statically-initialized";
  if (start.entry-initialization-tracker.tracker-remote-library ==
         application.application-dylan-library)
    allocate-temporary-download-block-in(application, thread);
  end if;
  if (start.entry-initialization-tracker.tracker-top-level?)
    construct-component-name-table(application);
    initialize-static-keywords(application, thread);
  end if;
  handle-library-initialization-phase
    (application, thread,
     start.entry-initialization-tracker.tracker-remote-library,
     #"start",
     start.entry-initialization-tracker.tracker-top-level?);
end method;


///// DYNAMIC-INITIALIZER-DONE-CALLBACK
//    The callback routine for the exit tracepoint.

define method dynamic-initializer-done-callback
    (application :: <debug-target>, done :: <done-dynamic-initialization>,
     thread :: <remote-thread>)
  => (interested? :: <boolean>)
  deregister-debug-point(application, done);
  deregister-debug-point(application, done.corresponding-entry-tracepoint);
  done.exit-initialization-tracker.tracker-initialization-state := 
     #"dynamically-initialized";
  handle-library-initialization-phase
    (application, thread,
     done.exit-initialization-tracker.tracker-remote-library,
     #"end",
     done.exit-initialization-tracker.tracker-top-level?);
end method;


///// CONSTRUCT-COMPONENT-NAME-TABLE
//    Constructs a mapping from dylan library definition names to
//    <REMOTE-LIBRARY> objects. This is done by examining the
//    object table stored in the variable *dylan-runtime-modules*
//    in the internal:dylan namespace.

//    Define a constant to name the runtime variable.
//    TODO: Require a function in dfmc-browser-support to return
//          this name instead.

define constant $runtime-module-variable-name = "*dylan-runtime-modules*";

define method construct-component-name-table
    (application :: <debug-target>) => ()

  let path = application.debug-target-access-path;
  let cache = pair(as-remote-value(0), #f);

  ///// GET-LIBRARY-FROM-BASE-ADDRESS
  //    A local utility to scan over the known <REMOTE-LIBRARY>
  //    images, and return one whose base address matches the
  //    one given. Essentially, we assume we will find a match
  //    in every case. However, given the inherent danger of
  //    problems while examining remote dylan objects, we
  //    allow this function to be defensive against failure.
  //    It will return the <remote-library> that corresponds to
  //    the application's executable file in those cases where
  //    no match is found.

  local method get-library-from-base-address (bx :: <remote-value>)
                => (lib :: <remote-library>)
          if (bx = cache.head)
            cache.tail
          else
            block(return)
              do-libraries
                (method (l :: <remote-library>) => ()
                   if (l.library-base-address = bx)
                     cache.head := bx;
                     cache.tail := l;
                     return(l);
                   end if
                 end method,
                 path);
              debug-out(#"debugger-manager", "Did not match a <remote-library> to %=", bx);
              return(application.application-executable);
            end block;
          end if;
        end method;


  ///// GET-LIBRARY-FROM-COMPONENT-NAME
  //    A local utility to scan over the known <REMOTE-LIBRARY>
  //    images, and return one whose core name matches the one
  //    given. If none is found, the top-level executable is
  //    returned instead.

  local method get-library-from-component-name (cname :: <string>)
                => (lib :: <remote-library>)
          block(return)
            do-libraries
              (method (l :: <remote-library>) => ()
                 if (as-uppercase(l.library-core-name) = cname)
                   return(l)
                 end if
               end method,
               path);
            return(application.application-executable);
          end block;
        end method;

  let dylan-lib = application.application-dylan-library;
  let (object-table, addr) =
     resolve-dylan-name(application,
                        $runtime-module-variable-name,
                        $dylan-internal,  // Assume the namespace.
                        indirect?: #t,    // We know it's a variable.
                        library: dylan-lib);

  // Sanity check.
  // Ensure that we managed to resolve this name, and that the value
  // we have obtained is indeed a Dylan table object. If not,
  // we can't construct the mapping.

  if (object-table & 
      (classify-dylan-object(application, object-table) == $table-type))

    let (symbolic-keys, base-addr-machine-words)
       = remote-table-inspect(application, object-table);

    // For each mapping in the _runtime_ table, we construct our
    // own entry by taking the following steps:

    //    - Reading the key. (A remote instance of <symbol>).
    //    - From the symbol, importing the actual string value.
    //    - Reading the value.
    //    - From the value, importing the actual machine-word data.
    //    - Calling the utility function to find a <REMOTE-LIBRARY>
    //      that matches the base address.
    //    - Installing the mapping from the library name to the
    //      <REMOTE-LIBRARY>.

    for (i from 0 below symbolic-keys.size)
      let remote-sym = symbolic-keys[i];
      let remote-mw = base-addr-machine-words[i];
      let remote-libname = dylan-symbol-name(application, remote-sym);
      let remote-base-addr = dylan-machine-word-data(application, remote-mw);
      let lib = get-library-from-base-address(remote-base-addr);
      let libname = dylan-string-data(application, remote-libname);
      application.library-component-names[libname] := lib;
    end for;

    // At the time we are examining this table, non of the top-level
    // library's initialization code will have run, so its own entry
    // will not be in the table. However, we probably know enough to
    // make the entry ourselves:

    application.library-component-names
        [application.top-level-library-definition-name]
           := get-library-from-component-name
                (application.top-level-library-name);

    // Another special case is needed for the Dylan library, which is
    // also not present in the table.

    application.library-component-names["dylan"] :=
      application.application-dylan-library;
  else
    debug-out(#"debugger-manager",
              "Resolving %s yielded %=, which is not an object table.",
              $runtime-module-variable-name, object-table);
  end if;
end method;


///// REGISTER-DYLAN-RUNTIME-LIBRARY
//    Called when the target application loads the DLL that is known to
//    correspond to the Dylan library.
//    (At time of writing, it is called HQN-DYLAN.DLL, but we use a
//    compiler API to determine this where possible)

define method register-dylan-library
    (application :: <debug-target>, dylan-library :: <remote-library>)
  => ()
  application.application-dylan-library := dylan-library;
  application.dylan-application? := #t;
  application.dylan-spy := make(<dylan-spy-catalogue>);
  initialize-static-wrappers(application);
  initialize-static-objects(application);
end method;

define method deregister-dylan-library (application :: <debug-target>) => ()
  application.application-dylan-library := #f;
  application.dylan-application? := #f;
  // Trash the directory of wrappers and objects.
  application.static-object-directory := make(<static-object-directory>);
end method;


///// REGISTER-DYLAN-RUNTIME-LIBRARY
//    Called when the target applications loads the DLL that is known to
//    contain the lowlevel dylan runtime system. We use a compiler API to
//    determine this. At time of writing, it is HQN-DYLAN.DLL, ie. the same
//    as the Dylan library.

define constant $running-under-dylan-debugger? = "_Prunning_under_dylan_debuggerQ";

define method register-dylan-runtime-library
    (application :: <debug-target>, runtime-library :: <remote-library>)
  => ()
  let path = application.debug-target-access-path;
  let one = as-remote-value(1);
  application.application-dylan-runtime-library := runtime-library;
  application.C-spy := make(<C-spy-catalogue>);
  locate-runtime-primitives(application);
  let debug-variable-sym =
    find-symbol(path,
                $running-under-dylan-debugger?,
                library: runtime-library);
  if (debug-variable-sym)
    write-value(path, debug-variable-sym.remote-symbol-address, one)
  end if;
end method;

define method deregister-dylan-runtime-library
    (application :: <debug-target>) => ()
  application.application-dylan-runtime-library := #f;
  invalidate-runtime-primitives(application);
end method;


///// DYLAN-AND-RUNTIME-LIBRARY-NAMES
//    Where possible, calls the compiler debug-info API to determine the
//    names of the Dylan and low-level runtime libraries.
//    Always coerces the names to uppercase.

define method dylan-and-runtime-library-names
    (application :: <debug-target>) 
  => (dylan-library-name :: <string>, runtime-library-name :: <string>)
  let context = application.debug-target-compilation-context;
  let dyl = (context & context.compilation-context-dylan-component-name)
                  | obtain-component-name(application, "dylan");
  let rtl = (context & context.compilation-context-runtime-component-name)
                  | obtain-component-name(application, "dylan");
  values(as-uppercase(dyl), as-uppercase(rtl));
end method;


///// TOP-LEVEL-LIBRARY-NAME
//    Where possible, calls the compiler debug-info API to determine the
//    core name of the application's top-level library. Will be coerced
//    to upper case.

define method top-level-library-name
    (application :: <debug-target>) => (name :: <string>)
  let context = application.debug-target-compilation-context;
  let name =
    (context & context.compilation-context-component-name) |
    application.top-level-component-name |
    "unknown";
  as-uppercase(name);
end method;


///// TOP-LEVEL-LIBRARY-DEFINITION-NAME
//    Where possible, calls the compiler to determine the name of the
//    top-level dylan library. (This now means "library" in the Dylan
//    language sense, and not the component sense). The result is coerced
//    to lower case.

define method top-level-library-definition-name
   (application :: <debug-target>) => (name :: <string>)
  let context = application.debug-target-compilation-context;
  let name =
    (context & context.compilation-context-library-name) |
    application.top-level-component-name |
    "unknown";
  as-lowercase(name);
end method;
