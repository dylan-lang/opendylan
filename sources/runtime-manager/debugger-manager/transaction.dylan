module:      dm-internals
synopsis:    Debugger Transaction
author:      Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
// A list of function names which can be used to determine whether a thread
// is in the memory manager. If one of these functions appears anywhere on
// the thread's stack then the thread is in the MM.
//
define constant $mm-function-names
  = list("mps_amc_apply",
	 "ArenaAccess",
	 "mps_message_finalization_ref",
	 "mps_message_queue_type",
	 "mps_message_get",
	 "mps_message_type_enable",
	 "mps_message_discard",
	 "mps_message_type",
	 "mps_message_poll",
	 "mps_finalize",
	 "mps_ld_reset",
	 "mps_thread_dereg",
	 "mps_thread_reg",
	 "mps_root_destroy",
	 "mps_root_create_reg",
	 "mps_root_create_fmt",
	 "mps_root_create_table_masked",
	 "mps_root_create_table",
	 "mps_root_create",
	 "mps_ap_trip",
	 "mps_ap_fill",
	 "mps_ap_destroy",
	 "mps_ap_create_v",
	 "mps_ap_create",
	 "mps_free",
	 "mps_alloc_v",
	 "mps_pool_destroy",
	 "mps_pool_create_v",
	 "mps_fmt_destroy",
	 "mps_fmt_create_A",
	 "mps_arena_collect",
	 "mps_arena_park",
	 "mps_arena_release",
	 "mps_arena_clamp",
	 "mps_arena_committed",
	 "mps_arena_reserved",
	 "mps_arena_extend");



// A type which describes the range of addresses a function in the
// debug target occupies.
//
define class <address-range> (<object>)
  slot address-range-from :: <remote-value>, required-init-keyword: from:;
  slot address-range-to :: <remote-value>, required-init-keyword: to:;
end class;

define method inside-address-range?
    (address :: <remote-value>, range :: <address-range>)
 => (well? :: <boolean>)
  remote-value-<=(address, range.address-range-to)
  & remote-value-<(range.address-range-from, address);
end method;



// A type which describes address information for a group of functions
// in the debug target. This is implemented as a vector of <address-
// range>s.
//
define constant <remote-function-info> = <stretchy-vector>;


// Returns #t if and only if the address supplied falls inside the
// address range of one of the functions in the remote function info.
//
define function address-inside-function?
    (address :: <remote-value>, info :: <remote-function-info>)
 => (well? :: <boolean>)
  any?(curry(inside-address-range?,  address), info);
end function;


// This is called when a new debugger transaction starts. It ensures
// that address information for the MM functions has been initialized.
//
define function ensure-mm-function-info-initialized
    (application :: <debug-target>) => ()
  if (size(application.mm-function-info) = 0)
    let access-path = application.debug-target-access-path;
//    let n = size($mm-function-names);
    let new-info = make(<stretchy-vector>);
    for (name in $mm-function-names)
      let symbol = find-symbol(access-path, name);
      if (symbol)
	let address = remote-symbol-address(symbol);
	let (start, finish)
	  = function-bounding-addresses(access-path, address);
	add!(new-info, make(<address-range>, from: start, to: finish));
      end if;
    end for;
    application.mm-function-info := new-info;
  end if;
end function;


// Steps through each thread in the debug target. If the thread is in the
// MM, then all other threads are suspended and the thread is allowed to
// run until it leaves the MM.
//
define function step-all-threads-out-of-mm (application :: <debug-target>)
 => ()
  let access-path = application.debug-target-access-path;

  // Returns true if the stack frame belongs to a know MM function 
  //
  local method mm-stack-frame? (frame :: <stack-frame>) => (well? :: <boolean>)
    let address = frame-instruction-address(access-path, frame);
    address-inside-function?(address, application.mm-function-info);
  end method;

  // Returns the oldest MM frame on the thread's stack, or #f if there
  // are none.
  //
  local method mm-entry-stack-frame (thread :: <remote-thread>)
    => (frame :: false-or(<stack-frame>))
    block(return)
      let stack-frame = initialize-stack-trace(access-path, thread);
      if (mm-stack-frame?(stack-frame))
        let oldest-mm-frame = stack-frame;
        stack-frame := previous-frame(access-path, stack-frame);
        while ((stack-frame) & (mm-stack-frame?(stack-frame)))
          oldest-mm-frame := stack-frame;
          stack-frame := previous-frame(access-path, stack-frame);
	end while;
        oldest-mm-frame;
      else
        return(#f)
      end if
    end block;
  end method;

  // Returns #t if this stop reason is due to hitting a breakpoint
  // set at the supplied address.
  //
  local method our-debug-point?
      (stop-reason :: false-or(<stop-reason>), address :: <remote-value>)
   => (well? :: <boolean>)
    (instance?(stop-reason, <debug-point-stop-reason>)
      & (stop-reason-debug-point-address(stop-reason) = address));
  end method;

  // Steps a single thread we know is in the MM out of the MM.
  //
  local method step-thread-out-of-mm
      (mm-thread :: <remote-thread>, mm-frame :: <stack-frame>) => ()

    // suspend all threads except the one we are stepping
    do-threads(method (thread)
		 if (thread ~== mm-thread)
		   suspend-thread(access-path, thread);
		 end if;
	       end method, access-path);

    // set a breakpoint on the return address of the oldest MM frame.
    let address = frame-return-address(access-path, mm-frame);
    let breakpoint-already? = query-breakpoint?(access-path, address);
    unless (breakpoint-already?)
      enable-breakpoint(access-path, address);
    end unless;

    // Step the thread
    while (mm-entry-stack-frame(mm-thread))
      continue(access-path);
      let stop-reason = wait-for-stop-reason(access-path, timeout: 1000);
      while(~(our-debug-point?(stop-reason, address)))
        if (stop-reason)
          continue(access-path);
	end if;
        stop-reason := wait-for-stop-reason(access-path, timeout: 1000);
      end while;
    end while;

    // remove the breakpoint
    unless (breakpoint-already?)
      disable-breakpoint(access-path, address)
    end unless;

    // resume all suspended threads
    do-threads(curry(resume-thread, access-path), access-path);

    // Work on the principle that a thread that has been successfully
    // recovered from the MM should now be happy to run spy code without
    // freezing up.
    use-thread-for-spy-functions(application, mm-thread);
  end method;

  // Step through each thread in turn, stepping it out of the MM if
  // necessary.
  do-threads(method (thread :: <remote-thread>)
	       let frame = mm-entry-stack-frame(thread);
	       if (frame)
		 step-thread-out-of-mm(thread, frame);
	       end if;
	     end method, access-path);
end function;


// This is called at the start of a debugger transaction once all the
// debug target's threads are known not to be in the MM. It informs
// the MM that a transaction has started.
//
define function mm-start-debugger-transaction (dt :: <debug-target>) => ()
  let spy-thread = select-thread-for-spy(dt);
  if (spy-thread)
    run-spy-on-thread(dt, spy-thread, dt.C-spy.start-debugger-transaction);
  end if;
end function;


// This is called at the end of a debugger transaction. It informs
// the MM that the current transaction has finished.
//
define function mm-end-debugger-transaction (dt :: <debug-target>) => ()
  let spy-thread = select-thread-for-spy(dt);
  if (spy-thread)
    run-spy-on-thread(dt, spy-thread, dt.C-spy.end-debugger-transaction);
  end if;
end function;
*/

///// <PAGE-RELATIVE-OBJECT-TABLE>
//    A class used to store remote dylan objects in fast-lookup form.
//    The values in the table can be arbitrary information that needs to
//    be obtained from the key.

define open abstract class <page-relative-object-table> (<object>)

  constant slot lookup-table-debug-target :: <debug-target>,
    required-init-keyword: debug-target:;

  slot objects-by-virtual-page :: <table> = make(<table>);

end class;

// Make it instantiable.

define class <dm-page-relative-object-table> (<page-relative-object-table>)
end class;

define method make
    (c == <page-relative-object-table>, #rest keys, #key, #all-keys)
 => (inst :: <page-relative-object-table>)
  apply(make, <dm-page-relative-object-table>, keys)
end method;


///// <PAGE-RELATIVE-OBJECT-TABLE-ENTRY>
//    Any object stored as a value in a <page-relative-object-table> must
//    be a general instance of this class.

define open abstract class <page-relative-object-table-entry> (<object>)
end class;


///// INVALIDATE-PAGE-RELATIVE-OBJECT-TABLE
//    Removes all entries.

define open generic invalidate-page-relative-object-table
    (table :: <page-relative-object-table>) => ();

define method invalidate-page-relative-object-table
    (table :: <page-relative-object-table>) => ()
  table.objects-by-virtual-page := make(<table>)
end method;


///// ENQUIRE-OBJECT
//    Checks to see whether a dylan object is present in a table. If so,
//    returns the description that was supplied to ADD-OBJECT when the
//    object was put into the table, otherwise returns #f.

define open generic enquire-object
    (table :: <page-relative-object-table>, instance :: <remote-value>)
 => (entry :: false-or(<page-relative-object-table-entry>));

define method enquire-object
    (table :: <page-relative-object-table>, instance :: <remote-value>)
 => (entry :: false-or(<page-relative-object-table-entry>))
  let application = table.lookup-table-debug-target;
  let page-table = table.objects-by-virtual-page;
  let path = application.debug-target-access-path;
  let (page, offset) = page-relative-address(path, instance);
  let subtable = element(page-table, page, default: #f);
  if (subtable)
    element(subtable, offset, default: #f)
  else
    #f
  end if
end method;


///// ADD-OBJECT
//    Adds an object to the a page-relative-object-table. If there is
//    already an entry for the object, it will be overwritten with this
//    entry.
//    (Generic function and its default method)

define open generic add-object
    (table :: <page-relative-object-table>, instance :: <remote-value>,
     entry :: <page-relative-object-table-entry>)
 => ();

define method add-object
    (table :: <page-relative-object-table>, instance :: <remote-value>,
     entry :: <page-relative-object-table-entry>)
 => ()
  let application = table.lookup-table-debug-target;
  let page-table = table.objects-by-virtual-page;
  let path = application.debug-target-access-path;
  let (page, offset) = page-relative-address(path, instance);
  let subtable = element(page-table, page, default: #f);
  unless (subtable)
    subtable := make(<table>);
    page-table[page] := subtable;
  end unless;
  subtable[offset] := entry;
end method;


///// REMOVE-OBJECT
//    Removes an object from a page-relative table. Silently does nothing
//    if the object is not present in the table.
//    (Generic function and its default method).

define open generic remove-object
    (table :: <page-relative-object-table>, instance :: <remote-value>)
 => ();

define method remove-object
    (table :: <page-relative-object-table>, instance :: <remote-value>)
 => ()
  let application = table.lookup-table-debug-target;
  let page-table = table.objects-by-virtual-page;
  let path = application.debug-target-access-path;
  let (page, offset) = page-relative-address(path, instance);
  let subtable = element(page-table, page, default: #f);
  if (subtable)
    let entry-count = subtable.size;
    let entry = element(subtable, offset, default: #f);
    if (entry)
      remove-key!(subtable, offset);
      if (entry-count == 1)
        remove-key!(page-table, page);
      end if
    end if
  end if
end method;


