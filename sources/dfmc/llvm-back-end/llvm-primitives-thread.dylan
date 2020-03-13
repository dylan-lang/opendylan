Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Thread environment

define method initialize-teb-struct-type (back-end :: <llvm-back-end>) => ()
  // Note that this layout is assumed by the debugger-manager library
  back-end.llvm-teb-struct-type
    := make(<&raw-struct-type>,
	    debug-name: "dylan-teb",
	    options: #[],
	    members:
	      vector(// Offset 0: Current bind-exit frame on stack
		     make(<raw-aggregate-ordinary-member>,
			  name: #"teb-dynamic-environment",
                          raw-type: dylan-value(#"<raw-pointer>")),
		     // Offset 1: thread variables <simple-object-vector>
		     make(<raw-aggregate-ordinary-member>,
			  name: #"teb-thread-local-variables",
                          raw-type: dylan-value(#"<raw-pointer>")),
		     // Offset 2: <thread> object for this thread
		     make(<raw-aggregate-ordinary-member>,
			  name: #"teb-current-thread",
                          raw-type: dylan-value(#"<raw-pointer>")),
		     // Offset 3: OS handle for this thread
		     make(<raw-aggregate-ordinary-member>,
			  name: #"teb-current-thread-handle",
                          raw-type: dylan-value(#"<raw-pointer>")),
		     // Offset 4: <list> of current <handler> objects
		     make(<raw-aggregate-ordinary-member>,
			  name: #"teb-current-handler",
                          raw-type: dylan-value(#"<raw-pointer>")),
		     // Offset 5: FFI barrier (inside or outside Dylan)
		     make(<raw-aggregate-ordinary-member>,
		     	  name: #"teb-runtime-state",
                          raw-type: dylan-value(#"<raw-pointer>")),
		     // Padding
		     make(<raw-aggregate-array-member>,
			  name: #"teb-pad",
			  array-length: 2,
                          raw-type: dylan-value(#"<raw-pointer>")),
		     // Offset 8: MV count
		     make(<raw-aggregate-ordinary-member>,
		     	  name: #"teb-mv-count",
                          raw-type: dylan-value(#"<raw-pointer>")),
		     // Offset 9: MV area
		     make(<raw-aggregate-array-member>,
			  name: #"teb-mv-area",
			  array-length: $maximum-value-count,
                          raw-type: dylan-value(#"<raw-pointer>"))));

  // Record TEB structure field indicies
  for (member in back-end.llvm-teb-struct-type.raw-aggregate-members,
       index from 0)
    back-end.%raw-struct-field-index[member.member-name] := i32(index);
  end for;
end method;

// The current TEB (on platforms that support thread-local variables),
// or the TEB of the initial thread (on platforms without TLV support).
define thread-local runtime-variable %teb :: <teb> = #f;

// %teb-tlv-index is the variable which contains the handle for the
// Windows TLV/pthreads key which holds the TEB for each thread on platforms
//
define runtime-variable %teb-tlv-index :: <raw-machine-word>
  = make-raw-literal(as(<machine-word>, -1));

// %teb-chain is a variable holding the current TEB in a chain of live TEBs
//
define runtime-variable %teb-chain :: <raw-address>
  = make-raw-literal(0);

define method op--teb
    (be :: <llvm-back-end>) => (teb :: <llvm-value>);
  let module = be.llvm-builder-module;
  llvm-runtime-variable(be, module, %teb-descriptor)
end method;

define method op--teb
    (be :: <llvm-windows-back-end>) => (teb :: <llvm-value>);
  error("FIXME windows TEB");
end method;

define method op--teb-getelementptr
    (be :: <llvm-back-end>, field :: <symbol>, #rest indices)
 => (pointer :: <llvm-value>);
  let teb = op--teb(be);
  let index = be.%raw-struct-field-index[field];
  apply(ins--gep-inbounds, be, teb, 0, index, indices)
end method;


/// Thread-local variables

define runtime-variable %tlv-initializations :: <simple-object-vector> = #[],
  section: #"variables";

// Element index of the first unused vector element
define runtime-variable %tlv-initializations-cursor :: <raw-integer>
  = make-raw-literal(0), section: #"data";

// Marker for TLV initializations already perfomed in the current thread
define thread-local runtime-variable %tlv-initializations-local-cursor :: <raw-integer>
  = make-raw-literal(0);


/// Thread primitives

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-make-thread
    (thread :: <object>, function :: <function>, synchronous? :: <raw-boolean>)
  => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-destroy-thread
    (thread :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-thread-join-single
    (thread :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-thread-join-multiple
    (thread-vector :: <simple-object-vector>) => (res :: <object>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-thread-yield
    () => ();

define side-effect-free stateless dynamic-extent &c-primitive-descriptor primitive-current-thread
    () => (thread :: <object>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-simple-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-recursive-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-semaphore
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-notification
    (notif :: <object>, lock :: <object>)
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-simple-lock-timed
    (lock :: <object>, ms :: <integer>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-recursive-lock-timed
    (lock :: <object>, ms :: <integer>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-semaphore-timed
    (lock :: <object>, ms :: <integer>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-notification-timed
    (notif :: <object>, lock :: <object>, ms :: <integer>)
     => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-release-simple-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-release-recursive-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-release-semaphore
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-release-notification
    (notif :: <object>, lock :: <object>)
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-release-all-notification
    (notif :: <object>, lock :: <object>)
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-make-recursive-lock
    (lock :: <object>, name :: <object>)
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-destroy-recursive-lock
    (obj) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-make-simple-lock
    (lock :: <object>, name :: <object>)
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-destroy-simple-lock
    (lock :: <object>) => (res :: <integer>);

define side-effect-free stateful dynamic-extent &c-primitive-descriptor primitive-owned-simple-lock
    (lock :: <object>)  => (res :: <integer>);

define side-effect-free stateful dynamic-extent &c-primitive-descriptor primitive-owned-recursive-lock
    (lock :: <object>)  => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-make-semaphore
    (lock :: <object>, name :: <object>,
    init :: <integer>, max :: <integer>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-destroy-semaphore
    (obj :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-make-notification
    (lock :: <object>, name :: <object>)
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-destroy-notification
    (obj :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-sleep
    (ms :: <integer>) => ();

/*
define side-effecting stateful dynamic-extent &primitive-descriptor primitive-assign-atomic-memory
    (location :: <raw-pointer>, newval :: <object>) => (newval)
end;

define side-effecting stateful dynamic-extent &primitive-descriptor primitive-conditional-update-memory
    (location :: <raw-pointer>, newval, oldval)
    => (res :: <integer>)
end;
*/

define side-effecting stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-register-thread-variable-initializer
    (initial-value :: <object>, initializer-function :: <raw-pointer>) => ();

define side-effecting stateful dynamic-extent auxiliary &c-primitive-descriptor primitive-initialize-thread-variables
    () => ();

define method op--initialize-thread-variables
    (back-end :: <llvm-back-end>) => ();
  let word-size = back-end-word-size(back-end);
  let m = back-end.llvm-builder-module;

  let init-bb = make(<llvm-basic-block>);
  let common-bb = make(<llvm-basic-block>);

  let cursor
    = ins--load(back-end, llvm-runtime-variable(back-end, m, %tlv-initializations-cursor-descriptor),
                alignment: word-size);
  let local-cursor
    = ins--load(back-end, llvm-runtime-variable(back-end, m, %tlv-initializations-local-cursor-descriptor),
                alignment: word-size);
  let cmp = ins--icmp-ult(back-end, local-cursor, cursor);
  ins--br(back-end, op--unlikely(back-end, cmp), init-bb, common-bb);

  ins--block(back-end, init-bb);
  call-primitive(back-end, primitive-initialize-thread-variables-descriptor);
  ins--br(back-end, common-bb);

  ins--block(back-end, common-bb);
end method;

define side-effecting stateful indefinite-extent &c-primitive-descriptor primitive-allocate-thread-variable
    (initial-value :: <object>) => (handle :: <raw-pointer>);

define side-effect-free dynamic-extent stateless &unimplemented-primitive-descriptor primitive-read-thread-variable
    (handle :: <raw-pointer>) => (value :: <object>);
  //---*** Fill this in...
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-write-thread-variable
    (handle :: <raw-pointer>, newval :: <object>) => (newval);
  //---*** Fill this in...
end;

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-initialize-current-thread
    (thread :: <object>, synchronous? :: <raw-boolean>) => ();

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-initialize-special-thread
    (thread :: <object>) => ();

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-detach-thread
    (thread :: <object>) => ();

/*
define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-unlock-simple-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-unlock-recursive-lock
    (lock :: <object>) => (res :: <integer>);
*/

define stateful side-effect-free dynamic-extent &unimplemented-primitive-descriptor primitive-sequence-point () => ();
  //---*** Fill this in...
end;

define side-effecting stateful dynamic-extent &primitive-descriptor primitive-synchronize-side-effects
    () => ();
  ins--fence(be, ordering: #"sequentially-consistent");
end;
