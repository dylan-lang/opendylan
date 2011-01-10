module:      devel-dbg-ui
synopsis:    Definitions for describing an application being debugged.
author:      Paul Howard and Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// LANGUAGE MODES (For devel, this is just Dylan or C. Later
//                    we can get up to funnies with <remote-type>s...)

define constant $language-mode-dylan   = 1;
define constant $language-mode-C       = 2;


///// *CURRENT-BP-COOKIE*
//    This is a global variable. An integer cookie that gets assigned to
//    breakpoints when they are set.

define variable *current-bp-cookie* = 0;


///// *CURRENT-COMPILATION-CONTEXT*
//    A global variable holding the open compilation context. This never
//    contains anything except #f, unless this is INTERACTIVE-COMPILER,
//    in which case it holds the compilation context for whatever library
//    we are in.

define variable *current-compilation-context* = #f;


///// <APPLICATION> Describes the application being debugged.

define class <application> (<debug-target>)

       slot profiler-entry-breakpoints :: <sequence> = #[],
            init-keyword: entry-breakpoints:;

       slot application-profiler-run :: false-or(<profiler-run>) = #f;

       slot still-in-stop-mode? :: <boolean>,
            init-value: #f;

       slot language-mode :: <integer>,
            init-value: $language-mode-dylan;

       slot application-thread-table :: <stretchy-vector>
            = make(<stretchy-vector>);

       constant slot default-name-context :: <dylan-name-context>
            = make(<dylan-name-context>);

       slot current-stack :: <stretchy-vector>
            = make(<stretchy-vector>);

       slot active-breaks :: <stretchy-vector>
            = make(<stretchy-vector>, size: 0);

       slot active-source-breaks :: <stretchy-vector>
            = make(<stretchy-vector>, size: 0);

       slot active-traces :: <stretchy-vector>
            = make(<stretchy-vector>, size: 0);

       slot current-frame-index :: <integer>,
            init-value: 0;

       slot stopped-thread :: <object>,
            init-value: #f;

       slot most-recent-interesting-stop-reason :: false-or(<stop-reason>),
            init-value: #f;

       slot selected-thread :: <integer>,
            init-value: 1;

       constant slot allocated-static-blocks :: <stretchy-vector>
            = make(<stretchy-vector>, size: 0);

       slot current-static-block :: false-or(<static-block>),
            init-value: #f;

       slot further-downloading-possible? :: <boolean>,
            init-value: #t;

       slot downloader-initialized? :: <boolean>,
            init-value: #f;

       slot runtime-allocator-primitive :: false-or(<remote-symbol>),
            init-value: #f;

       slot runtime-byte-string-wrapper :: false-or(<remote-symbol>),
            init-value: #f;

       slot runtime-sov-wrapper :: false-or(<remote-symbol>),
            init-value: #f;

       slot runtime-pair-wrapper :: false-or(<remote-symbol>),
            init-value: #f;

       slot runtime-empty-list-object :: false-or(<remote-symbol>),
            init-value: #f;

       constant slot runtime-allocator-name :: <string> = 
            "_dylan__malloc__misc";

       constant slot application-profile-sets :: <table> = 
            make(<table>);

       slot application-profile-filter-1 :: <profile-set> = make(<profile-set-empty>);

       slot application-profile-filter-2 :: <profile-set> = make(<profile-set-empty>);

       slot application-profile-aggregates-1 :: <table> = make(<table>);

       slot application-profile-aggregates-2 :: <table> = make(<table>);

       slot application-profile-functions :: <function-tree> = #f;

       slot application-profile-limit-0 :: <single-float> = 95.0;

       slot application-profile-limit-1 :: <single-float> = 99.0;

       slot application-profile-limit-2 :: <single-float> = 99.0;

       slot application-profile-top-n-0 :: false-or(<integer>) = 25;

       slot application-profile-top-n-1 :: false-or(<integer>) = #f;

       slot application-profile-top-n-2 :: false-or(<integer>) = #f;

       slot application-profile-inclusive? :: <boolean> = #t;

       slot application-profile-exclusive? :: <boolean> = #t;

       slot application-profile-height :: false-or(<integer>) = #f;

       constant slot application-likely-namespaces :: <stretchy-vector>
         = make(<stretchy-vector>);

end class;

define constant application-profile-filter     
  = application-profile-filter-1;
//define constant application-profile-filter-setter
//  = application-profile-filter-1-setter;
define constant application-profile-aggregates
  = application-profile-aggregates-1;
//define constant application-profile-aggregates-setter 
//  = application-profile-aggregates-1-setter;

define method initialize (application :: <application>, #key, #all-keys) => ()
  next-method();
  application-register-sets(application);
end method;

define method application-register-profile-set
   (application :: <application>, name :: <symbol>, set :: <profile-set>)
  application-profile-sets(application)[name] := set;
  profile-set-name(set) := name;
end method;

define method application-register-profile-aggregate-2
   (application :: <application>, set :: <profile-set>)
  application-profile-aggregates-2
    (application)[profile-set-name(set)] := set;
end method;

define method application-register-sets (app :: <application>) => ()
  let sets       = application-profile-sets(app);
  let aggregates = application-profile-aggregates(app);
  let dylan-dll-set = make(<profile-set-dll>, 
                           dll: as(<symbol>, $dylan-lib));
  let system-set = 
    make(<profile-set-union>,
	 unionees: vector(dylan-dll-set,
	                  make(<profile-set-dll>, dll: #"ntdll"),
	                  make(<profile-set-dll>, dll: #"kernel32")));
  local method union-set (#rest sets)
	  make(<profile-set-union>, unionees: sets)
	end method,
        method in-dylan-set (#rest sets)
	  make(<profile-set-intersection>, 
	       intersectionees: vector(dylan-dll-set, apply(union-set, sets)))
	end method;
			
  let io-set =
     union-set
       (make(<profile-set-contains>, contains: "win32-read"),
	make(<profile-set-contains>, contains: "win32-write"),
	make(<profile-set-contains>, contains: "ZwReply"),
	make(<profile-set-contains>, contains: "_lopen"),
	make(<profile-set-contains>, contains: "NtSetEvent"),
	make(<profile-set-contains>, contains: "NtQueryInformationPort"),
	make(<profile-set-contains>, contains: "DeleteFileW"));

  let call-set =
    union-set
      (make(<profile-set-contains>, contains: "BasePushProcessParameters"),
       in-dylan-set
	 (make(<profile-set-contains>, contains: "xep"),
	  make(<profile-set-contains>, contains: "mep"),
	  make(<profile-set-contains>, contains: "primitive_process_keys"),
	  make(<profile-set-contains>, contains: "primitive_check_specializers"),
	  make(<profile-set-contains>, contains: "primitive_pad_mv"),
	  make(<profile-set-contains>, contains: "remove_optionals")));

  let dispatch-file-set =
    union-set(make(<profile-set-file>, file: "new-dispatch.dylan"),
	      make(<profile-set-file>, file: "dispatch.dylan"),
	      make(<profile-set-file>, file: "discrimination.dylan"),
	      make(<profile-set-file>, file: "slot-dispatch.dylan"),
	      make(<profile-set-file>, file: "dispatch-prologue.dylan"));

  let dispatch-set =
    union-set
      (make(<profile-set-contains>, contains: "gf_xep"),
       in-dylan-set
	 (dispatch-file-set,
	  make(<profile-set-contains>, contains: "discriminat"),
	  make(<profile-set-contains>, contains: "engine"),
	  make(<profile-set-contains>, contains: "getter_entry"),
	  make(<profile-set-contains>, contains: "method_entry")));

  let runtime-set =
    in-dylan-set
      (make(<profile-set-contains>, contains: "primitive_unwind_protect_cleanup"),
       make(<profile-set-contains>, contains: "dylan_init_thread"));

  let copy-vector-set =
    make(<profile-set-contains>, contains: "primitive_copy_vector");

  let closure-set =
    in-dylan-set
      (make(<profile-set-contains>, contains: "closure"));

  let mm-set =
    in-dylan-set
      (make(<profile-set-contains>, contains: "MM"),
       make(<profile-set-contains>, contains: "mps"),
       make(<profile-set-contains>, contains: "alloc"),
       make(<profile-set-contains>, contains: "Arena"),
       make(<profile-set-contains>, contains: "Buffer"),
       make(<profile-set-contains>, contains: "Pool"),
       make(<profile-set-contains>, contains: "Trace"),
       make(<profile-set-contains>, contains: "Prot"),
       make(<profile-set-contains>, contains: "Root"),
       make(<profile-set-contains>, contains: "Action"),
       make(<profile-set-contains>, contains: "Shield"),
       make(<profile-set-contains>, contains: "Seg"),
       make(<profile-set-contains>, contains: "AMC"),
       make(<profile-set-contains>, contains: "LD"));

  let allocation-set =
     union-set(copy-vector-set,
	       mm-set,
	       closure-set,
	       make(<profile-set-contains>, contains: "Z32Z"));

  let type-checks-set =
    in-dylan-set
      (make(<profile-set-contains>, contains: "primitive_type_check"),
       make(<profile-set-contains>, contains: "instanceQ"));

  /// SUPPORT SETS

  application-register-profile-set
    (app, #"closure", closure-set);
                 
  application-register-profile-set
    (app, #"copy-vector", copy-vector-set);
                 
  application-register-profile-set
    (app, #"gc", mm-set);
                 
  /// MAIN SETS

  application-register-profile-set
    (app, #"malloc", allocation-set);
                 
  application-register-profile-set
    (app, #"calls", call-set);
                 
  application-register-profile-set
    (app, #"dispatch", dispatch-set);

  application-register-profile-set
    (app, #"type-checks", type-checks-set);
                 
  application-register-profile-set
    (app, #"runtime", runtime-set);
                 
  application-register-profile-set
    (app, #"input-output", io-set);
                 
  /// USEFUL EXTRA SETS

  application-register-profile-set
    (app, #"dylan", dylan-dll-set);

  application-register-profile-set
    (app, #"none", make(<profile-set-empty>));

  application-register-profile-set
    (app, #"every", make(<profile-set-full>));

  application-register-profile-set
    (app, #"system", system-set);

  application-register-profile-set
    (app, #"user", make(<profile-set-complement>, complementee: system-set));

  application-register-profile-aggregate-2(app, allocation-set);
  application-register-profile-aggregate-2(app, call-set);
  application-register-profile-aggregate-2(app, dispatch-set);
  application-register-profile-aggregate-2(app, type-checks-set);
  application-register-profile-aggregate-2(app, runtime-set);
  application-register-profile-aggregate-2(app, io-set);
  application-profile-filter-1(app) := system-set;

end method;

///// *OPEN-APPLICATION*
//    A global variable that holds the <application> object describing the
//    currently open application. This is the poor bastard under the scalpel.
//    This debugger only allows one open application at a time, but it
//    should be possible to kill an app and fire up a different one without
//    quitting the session. (It might not be wise in practice. Things have
//    been known to get kinda screwy in those circumstances).

define variable *open-application* = #f;


///// AS-THREAD-SEQUENCE
//    Given a sequence of integers, returns a sequence of <remote-thread>
//    objects.

define method as-thread-sequence
    (application :: <application>, integer-sequence :: <sequence>)
       => (thread-sequence :: <sequence>)
  let thread-sequence = make(<stretchy-vector>, size: 0);
  let limit = size(application.application-thread-table);
  for (i in integer-sequence)
    if ((i > 0) & (i <= limit))
      add!(thread-sequence, application.application-thread-table[i - 1]);
    else
      debugger-message("*** Warning: Thread number %d does not exist.", i);
    end if
  end for;
  thread-sequence;
end method;


///// ALL-THREADS
//    Returns a sequence of integers indexing all known threads.

define method all-threads
    (application :: <application>) => (thread-sequence :: <sequence>)
  let thread-sequence = make(<stretchy-vector>, size: 0);
  let limit = size(application.application-thread-table);
  for (t from 0 below limit)
    add!(thread-sequence, t + 1);
  end for;
  thread-sequence;
end method;
