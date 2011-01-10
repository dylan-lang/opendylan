module:       dm-internals
synopsis:     Descriptions of applications being debugged.
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



///// <RUNTIME-PRIMITIVE>
//    Describes everything about a primitive in the runtime.

define class <runtime-primitive> (<interpreted-runtime-entry-point>)

  slot last-matched-address :: false-or(<remote-value>),
    init-value: #f;

end class;


///// <DEBUG-TARGET>


define open abstract class <debug-target> (<object>)

  // Exported slots.

  slot debug-target-access-path :: <access-path>;
  slot debug-target-symbol-table :: <interactive-symbol-table>;
  slot debug-target-compilation-context,
    init-value: #f,
    init-keyword: compilation-context:;

  // Implementation slots.

  // The directory of non-relocatable objects, wrappers and
  // keywords.

  slot static-object-directory :: <static-object-directory>
    = make(<static-object-directory>);

  slot last-classified-object :: <pair>
    = pair(as-remote-value(0), $unknown-type);

  // Top-level-component-name
  // The name of the "top level" library is normally obtained via
  // the supplied <compilation-context>, but it is necessary for
  // clients to specify it directly if no compilation-context is
  // installed in the <debug-target>.

  constant slot top-level-component-name :: false-or(<string>),
    init-value: #f,
    init-keyword: top-level-component-name:;

  // Certain callbacks may request that the application be stopped
  // or killed. Such requests are stored in the following slots
  // until they can be processed.

  slot application-killed? :: <boolean>,
       init-value: #f;

  slot application-stopped? :: <boolean>,
       init-value: #f;

  slot debugger-generated-stop-reason :: <debugger-generated-stop-reason>
       = make(<debugger-stop-application-stop-reason>);

  slot up-and-running? :: <boolean>,
       init-value: #f;

  slot application-restarted? :: <boolean>,
       init-value: #f;

  slot application-running-on-code-entry? :: <boolean> = #f;

  slot application-just-interacted? :: <boolean> = #f;

  // When some code is entered into the interactor for a thread that is
  // running, or paused at an interactive location, the interaction will
  // be deferred, and the ID generated will be an <thread-interaction-request>.
  // It is the deferred ID that the environment will be expecting, but
  // the debugger-manager will obviously pass an _actual_ ID when it has
  // finished genuinely executing the code. To bridge this gap, a mapping
  // from the actual ID to the deferred ID is stored in this table at the
  // point where the debugger-manager actually starts to execute the
  // interactive code.

  constant slot interactor-deferred-id-table :: <table> = make(<table>);

  slot application-threads :: <table>
       = make(<table>);

  slot application-primary-thread :: false-or(<remote-thread>),
    init-value: #f;

  slot application-selected-thread :: false-or(<remote-thread>),
    init-value: #f;

  // registered-debug-points is a table holding all known <debug-points>
  // that have been registered with this <debug-target>

  slot registered-debug-points :: <stretchy-vector>
       = make(<stretchy-vector>, size: 0);

  slot need-to-clear-debug-points? :: <boolean>,
       init-value: #f;

  // Tracking the loading and initialization of shared libraries.

  slot dylan-application? :: <boolean>, init-value: #f;
  slot application-dylan-library :: false-or(<remote-library>),
    init-value: #f;
  slot application-dylan-runtime-library :: false-or(<remote-library>),
    init-value: #f;
  constant slot library-initialization-trackers :: <table> 
    = make(<table>);
  constant slot library-component-names :: <string-table> 
    = make(<string-table>);
  slot application-executable :: false-or(<remote-library>) = #f;

  slot signalling-debug-points :: <sequence>,
       init-value: #[];

  // Catalogues of spy routines. One set for those with C calling
  // convention, and another set for those with dylan calling convention.
  // The latter set cannot be called directly, but we have spy functions
  // that can call into dylan, even though they themselves adopt a
  // C calling convention.

  slot C-spy :: <C-spy-catalogue> = make(<C-spy-catalogue>);

  slot dylan-spy :: <dylan-spy-catalogue> = make(<dylan-spy-catalogue>);

  // A slot to hold the thread that we use for spy calls. Higher levels
  // can hint at which thread this should be at any one time.

  slot target-spy-thread :: false-or(<remote-thread>),
    init-value: #f;

  // A Debugger Manager client can create a special reserved thread for
  // running the Spy on behalf of regular application threads;
  // the client sets this slot if that's the case

  slot reserved-spy-thread? :: <boolean>,
    init-value: #f;

  // ***************** PRIMITIVES FROM THE RUNTIME ***************

  constant slot nlx-primitive :: <runtime-primitive>
    = make(<runtime-primitive>, 
	   runtime-name: "primitive_nlx");

  constant slot debug-message-primitive :: <runtime-primitive>
    = make(<runtime-primitive>, 
	   runtime-name: "primitive_debug_message");

  constant slot invoke-debugger-primitive :: <runtime-primitive>
    = make(<runtime-primitive>, 
	   runtime-name: "primitive_invoke_debugger");

  constant slot exit-application-primitive :: <runtime-primitive>
    = make(<runtime-primitive>, 
	   runtime-name: "_spy_exit_application");

  constant slot class-breakpoint-primitive :: <runtime-primitive>
    = make(<runtime-primitive>, 
	   runtime-name: "_class_allocation_break");

  constant slot wrapper-wrapper-primitive :: <runtime-primitive>
    = make(<runtime-primitive>,
	   runtime-name: 
		mangle-in-context("<mm-wrapper>", $dylan-internal,
				   as-wrapper?: #t,
				   as-static-object?: #t));

  // application-profile-state stores the state of the current profile run
  constant slot application-profile-state :: <profile-state>
    = make(<profile-state>);

  // The current interactor level. This is incremented whenever a new
  // interactive form is invoked. When a form returns, the level
  // is restored. Note that, due to the potential of non-local exits,
  // "restored" does not necessarily mean "decremented".

  slot current-interactor-level :: <integer>,
    init-value: 0;

  // Temporary download block
  // This is currently only used by the resolve-dylan-keyword API,
  // which downloads the keyword's name into the runtime before
  // effectively calling as(<symbol>, x) on it. The tether-downloader
  // is called upon to create this block the first time it is needed.

  slot temporary-download-block :: false-or(<static-block>),
    init-value: #f;

  // We also need some space to download the names and "break"
  // messages of interactively-created threads.

  slot interactive-thread-download-block :: false-or(<static-block>),
    init-value: #f;

  // A boolean which is set to #f when a new transaction starts. This
  // enables read-dylan-value to know when it is being called for the
  // first time in a transaction so it can flush
  // pages-safe-to-read-cache.

  slot new-debugger-transaction? :: <boolean>,
    init-value: #f;

  // This is a cache of the pages which have been read from during
  // the current debugger transaction. It is guaranteed that these
  // pages can be read safely using the access-path.

  slot pages-safe-to-read-cache :: <set> = make(<set>);
/*
  // This slot holds info about the MM entry functions so that we
  // can detect when threads are in the MM at the start of debugger
  // transactions. The info is created in the first transaction.

  slot mm-function-info :: <remote-function-info>
    = make(<remote-function-info>);
*/
end class;


define class <dm-debug-target> (<debug-target>)
end class;


// Make sure that <DEBUG-TARGET> is instantiable...

define method make (class == <debug-target>, #rest keys, #key, #all-keys)
    => (debug-target)
  apply (make, <dm-debug-target>, keys)
end method;


define method initialize
    (t :: <debug-target>,
     #rest keys, #key application-object, #all-keys)

  // Obligatory call to next-method...

  next-method();

  let application-object =
    if (application-object) application-object
    else t
    end if;

  // Construct the appropriate access path for this debuggee, passing
  // down whatever keywords were supplied.

  t.debug-target-access-path :=
    apply (make, <access-path>,
	   application-object: application-object,
	   keys);

  // It must always be possible to map in either direction between
  // the <debug-target> and the <access-path>. Hence, set up a
  // circularity with this statement...

  t.debug-target-access-path.access-path-abstract-handle := t;

  // The debug target must also have an interactive symbol table linked
  // to the same <access-path>.

  t.debug-target-symbol-table := make(<interactive-symbol-table>,
                                      access-path: t.debug-target-access-path);

end method;


///// ADDRESS-CORRESPONDS-TO-PRIMITIVE? (Internal function)
//    Decides whether a given instruction pointer is within the definition
//    of a runtime primitive.

define method address-corresponds-to-primitive?
    (application :: <debug-target>, address :: <remote-value>,
     primitive :: <runtime-primitive>) => (answer :: <boolean>)
  if (primitive.last-matched-address & 
         (primitive.last-matched-address = address))
    #t
  elseif (primitive.runtime-symbol)
    let primsym = primitive.runtime-symbol;
    let (closest, offset)
      = symbol-relative-address(application.debug-target-access-path, 
                                address);
    if (closest & 
        (closest.remote-symbol-address = primsym.remote-symbol-address))
      primitive.last-matched-address := address;
      #t
    end if;
  else
    #f
  end if
end method;


///// LOCATE-RUNTIME-PRIMITIVES
//    Called once the application has loaded up the DLL (remote library)
//    that contains the lowlevel runtime system, this function tries to
//    find addresses for all required runtime primitives.

define method locate-runtime-primitives (application :: <debug-target>) => ()
  let path = application.debug-target-access-path;
  let lib = application.application-dylan-runtime-library;

  local method locate-this-one (primitive :: <runtime-primitive>) => ()
          let name = primitive.runtime-name;
          let sym = find-symbol(path, name, library: lib);
          if (sym)
            primitive.runtime-symbol := sym;
          end if;
          primitive.attempted-to-locate? := #t;
        end method;

  locate-this-one(application.nlx-primitive);
  locate-this-one(application.debug-message-primitive);
  locate-this-one(application.invoke-debugger-primitive);
  locate-this-one(application.class-breakpoint-primitive);
  locate-this-one(application.exit-application-primitive);
  locate-this-one(application.wrapper-wrapper-primitive);

  register-exit-process-function
    (path,
     application.exit-application-primitive.runtime-symbol);

end method;


///// INVALIDATE-RUNTIME-PRIMITIVES
//    Called once the application has unloaded the DLL (remote library)
//    that contains the lowlevel runtime system, this function de-caches
//    the information about runtime primitives.

define method invalidate-runtime-primitives 
     (application :: <debug-target>) => ()

  let path = application.debug-target-access-path;
  let lib = application.application-dylan-runtime-library;

  local method invalidate-this-one (primitive :: <runtime-primitive>) => ()
          primitive.runtime-symbol := #f;
          primitive.last-matched-address := #f;
          primitive.attempted-to-locate? := #f;
        end method;

  invalidate-this-one(application.nlx-primitive);
  invalidate-this-one(application.debug-message-primitive);
  invalidate-this-one(application.invoke-debugger-primitive);
  invalidate-this-one(application.class-breakpoint-primitive);
  invalidate-this-one(application.exit-application-primitive);
  invalidate-this-one(application.wrapper-wrapper-primitive);

end method;


///// DEFINABLE ENTRY POINTS

define macro spy-function-definer
  { define spy-function ?dylan-name:name ?stuff end }
    => { define constant ?dylan-name =
           make(<C-spy-function-descriptor>,
                ?stuff) }
  stuff:
    { } => { }
    { ?thingy:* ; ...} => { ?thingy , ...}
end macro;


define inline method application-just-interacted-on-running-thread?
    (application :: <debug-target>)
  application.application-just-interacted?
    & application.application-running-on-code-entry?
end method;
