Module:    dfmc-application
Synopsis:  collection environment protocols application backend
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// File conventions ...
///
/// Calls to error() indicate flag programmer errors on the part of those
/// using this code, or defensive programming checks for situations that
/// shouldn't occur in correct use of this code.  Should these calls to
/// error actually execute, then something in the Dylan product needs to be
/// fixed.
///
/// Calls to break() indicate where we need to call into some dev-env-wide
/// convention for recoverable situation where we need to tell the user
/// something and then forget about the problem.
///
/// This file uses McKay's commenting style of using "---" at the beginning
/// of a comment to mark it as a meta comment about some interim aspect of
/// the code, or some questionable code that may change when we get more
/// info.  Comments that begin with "---***" indicate serious comments about
/// work arounds for known bugs in our Dylan (or similar situations).
///

/// ---*** This file contains pretty dubious code.  I cannot really test
/// this at all until I have a real target app and can see what sorts of
/// descriptions come back from get-inspector-values().
///



/// Keys and Elements for General Collections.
///

/// collection-keys -- Method on Exported Interface.
///
define method collection-keys (application :: <dfmc-application>,
			       collection :: <collection-object>,
			       #key range: range-arg :: false-or(<range>))
    => (keys :: false-or(<sequence>))
  let app = application.application-target-app;
  let proxy = get-app-obj-proxy(application, collection);
  //
  // Test for collection being a table.  This test now is just a placeholder
  // to remind me that tables need to be handled differently than other
  // collections.
  if (remote-value-env-obj-class(app, proxy) == <table-object>)
    table-collection-keys(app, proxy);
  else
    //
    // Query for the inspectors values for proxy.
    let (class-rem-val :: <remote-value>, repeats :: false-or(<integer>))
      // Choose any values since let won't let me leave the vars unbound.
      = values(proxy, #f);
    do-env-prot-query
      (app, method () => ()
	      let (temp-class :: <remote-value>, temp-slots :: <sequence>,
		   temp-getters :: <sequence>, temp-setters :: <sequence>,
		   temp-repeats :: false-or(<integer>))
		= get-inspector-values(app, proxy);
	      ignore(temp-slots);
	      ignore(temp-getters);
	      ignore(temp-setters);
	      class-rem-val := temp-class;
	      repeats := temp-repeats;
	    end);
    //
    // Return the appropriate keys.
    let (start :: <integer>, stop :: <integer>)
      = collection-keys-bounds(repeats, range-arg);
    range(from: start, by: 1, below: stop);
  end if;
end method;

/// ---*** Test
/// I need this eventually, but I'm whipping it up here just to see if it
/// gets me around a compiler error.
///
/// Only really need this for <application-and-compiler-object>s, but I also
/// use this function for its type sig.  The compiler won't compile my code
/// due to application-object-proxy allowing for #f return.  I know for
/// non-app-and-comp-objs, I have to have a proxy (or the obj wouldn't
/// exist).
///
define function get-app-obj-proxy (app :: <dfmc-application>,
				   obj :: <application-object>)
    => (proxy :: <remote-value>)
  let proxy = obj.application-object-proxy;
  if (proxy)
    proxy;
  else
    obj.application-object-proxy
      := find-application-proxy(app.server-project, obj.compiler-object-proxy);
  end;
end function;

define function table-collection-keys (app :: <target-application>,
				       table :: <remote-value>)
    => (keys :: false-or(<sequence>))
  // --- When code moves off the Emulator, remove call to assert-arg-type.
  assert-arg-type(app, <target-application>);
  assert-arg-type(table, <remote-value>);
  #f;
end function;


/// collection-elements -- Method on Exported Interface.
///
define method collection-elements (application :: <dfmc-application>,
				   collection :: <collection-object>,
				   #key range: range-arg :: false-or(<range>))
    => (elts :: false-or(<sequence>))
  let app = application.application-target-app;
  let proxy = get-app-obj-proxy(application, collection);
  //
  // Test for collection being a table.  This test now is just a placeholder
  // to remind me that tables need to be handled differently than other
  // collections.
  if (remote-value-env-obj-class(app, proxy) == <table-object>)
    table-collection-elements(app, proxy);
  else
    //
    // Query for the inspectors values for proxy.
    let (class-rem-val :: <remote-value>, slots :: <sequence>,
	 getters :: <sequence>, repeats :: false-or(<integer>),
	 repeated-slots :: false-or(<remote-value>),
	 repeated-getter :: false-or(<function>))
      // Choose any values since let won't let me leave the vars unbound.
      = values(proxy, #(), #(), #f, #f, #f);
    do-env-prot-query
      (app, method () => ()
	      let (temp-class :: <remote-value>, temp-slots :: <sequence>,
		   temp-getters :: <sequence>, temp-setters :: <sequence>,
		   temp-repeats :: false-or(<integer>),
		   temp-repeated-slots :: false-or(<remote-value>),
		   temp-repeated-getter :: false-or(<function>))
		= get-inspector-values(app, proxy);
	      ignore(temp-setters);
	      class-rem-val := temp-class;
	      slots := temp-slots;
	      getters := temp-getters;
	      repeats := temp-repeats;
	      repeated-slots := temp-repeated-slots;
	      repeated-getter := temp-repeated-getter;
	    end);
    //
    // Return the appropriate elements.
    let (start :: <integer>, stop :: <integer>)
      = collection-keys-bounds(repeats, range-arg);
    let result :: <simple-object-vector>
      = make(<simple-object-vector>, size: (stop - start));
    for (i :: <integer> from start below stop)
      let val :: <remote-value> = repeated-getter(i);
      result[i] :=  make-environment-object
		      (remote-value-env-obj-class(app, val),
		       project: application.server-project,
		       application-object-proxy: val);
    end;
    result;
  end if;
end method;

define function table-collection-elements (app :: <target-application>,
					   table :: <remote-value>)
    => (elts :: false-or(<sequence>))
  // --- When code moves off the Emulator, remove call to assert-arg-type.
  assert-arg-type(app, <target-application>);
  assert-arg-type(table, <remote-value>);
  #f;
end function;


/// collection-keys-bounds -- Internal.
///
/// This function is used in collection-keys and collection-elements to
/// return the start and exclusive stop indexes of the elements to consider
/// for those functions.
///
define function collection-keys-bounds (repeats :: false-or(<integer>),
					range-arg :: false-or(<range>))
    => (start :: <integer>, stop :: <integer>)
  let start :: <integer> = if (range-arg) range-arg[0] else 0 end;
  if (start < 0) error("Range start less than zero -- %S.", start) end;
  let stop :: <integer>
    = if (range-arg)
	let stop = range-arg[range-arg.size - 1] + 1;
	if (stop > repeats)
	  error("Range end greater than size of collection -- %S.", stop);
	end;
	stop;
      else
	if (~ repeats)
	  // Defensive programming check.  If this error occurs, then we
	  // probably need to specially handle whatever type of collection
	  // we have, as we do with tables.
	  error("What?!  No repeated slot info for collection object -- %S.",
		range-arg);
	else
	  repeats;
	end;
      end;
  values(start, stop);
end function;



/// Ranges.
///

/// These constants access the info by the DM about range objects.  These
/// constants are determined by inspecting range objects, and if the
/// compiler/runtime groups ever change the object layouts, this code needs
/// to be updated.  An alternative approach to fetching the info required by
/// these env prots is to string match the name of the slot, but this could
/// also change at an arbitrary time.  I use the name as a check that I'm
/// accessing the right slot, sort of a poor man's defensive programming.
///
define constant range-start-slot-offset :: <integer> = 0;
define constant range-start-slot-name :: <byte-string> = "range-from";
///
define constant range-by-slot-offset :: <integer> = 1;
define constant range-by-slot-name :: <byte-string> = "range-by";
///
define constant range-size-slot-offset :: <integer> = 2;
define constant range-size-slot-name :: <byte-string> = "size";


/// range-start -- Method on Exported Interface.
///
define method range-start (application :: <dfmc-application>,
			   range :: <range-object>)
    => (start :: <number>)
  let app = application.application-target-app;
  let (class-name :: <string>, slots :: <sequence>, rem-vals :: <sequence>)
    // Choose any value since let won't let me leave the vars unbound.
    = values ("", #(), #());
  do-env-prot-query
    (app, method () => ()
	    let (temp-class-name :: <string>, temp-slots :: <sequence>,
		 temp-rem-vals :: <sequence>)
	      = describe-dylan-object(app, range.application-object-proxy);
	    class-name := temp-class-name;
	    slots := temp-slots;
	    rem-vals := temp-rem-vals;
	  end);
  do-range-start(app, slots, rem-vals);
end method;

/// do-range-start -- Internal.
///
/// This function pulls the info from the data retrieved from the DM in
/// range-start().  This function is also used by range-end().
///
/// Double check slot access by seeing if slot is named what we expect; this
/// is just a defensive programming kludge to ensure the runtime/compiler
/// groups haven't changed the implementation of ranges without this code
/// being updated.
///
define function do-range-start (app :: <target-application>,
				slots :: <sequence>, rem-vals :: <sequence>)
    => (start :: <number>)
  // --- When code moves off the Emulator, remove call to assert-arg-type.
  assert-arg-type(app, <target-application>);
  assert-arg-type(slots, <sequence>);
  assert-arg-type(rem-vals, <sequence>);
  if (as-lowercase(slots[range-start-slot-offset]) = range-start-slot-name)
    // Don't bother checking valid? return value since we know this must win.
    dylan-object-immediate-value(app, rem-vals[range-start-slot-offset]);
  else
    error("Eh?!  Range object's description has changed?"
	  "  Can't find %S slot.", range-start-slot-name);
  end;
end function;

/// range-by -- Method on Exported Interface.
///
define method range-by (application :: <dfmc-application>,
			range :: <range-object>)
    => (by :: <number>)
  let app = application.application-target-app;
  // Choose any value since let won't let me leave the vars unbound.
  let (slots :: <sequence>, rem-vals :: <sequence>) = values ("", #(), #());
  do-env-prot-query
    (app, method () => ()
	    let (class-name :: <string>, temp-slots :: <sequence>,
		 temp-rem-vals :: <sequence>)
	      = describe-dylan-object(app, range.application-object-proxy);
	    ignore(class-name);
	    slots := temp-slots;
	    rem-vals := temp-rem-vals;
	  end);
  do-range-by(app, slots, rem-vals);
end method;

/// do-range-by -- Internal.
///
/// This function pulls the info from the data retrieved from the DM in
/// range-by().  This function is also used by range-end().
///
/// Double check slot access by seeing if slot is named what we expect; this
/// is just a defensive programming kludge to ensure the runtime/compiler
/// groups haven't changed the implementation of ranges without this code
/// being updated.
///
define function do-range-by (app :: <target-application>,
			     slots :: <sequence>, rem-vals :: <sequence>)
    => (start :: <number>)
  // --- When code moves off the Emulator, remove call to assert-arg-type.
  assert-arg-type(app, <target-application>);
  assert-arg-type(slots, <sequence>);
  assert-arg-type(rem-vals, <sequence>);
  if (as-lowercase(slots[range-by-slot-offset]) = range-by-slot-name)
    // Don't bother checking valid? return value since we know this must win.
    dylan-object-immediate-value(app, rem-vals[range-by-slot-offset]);
  else
    error("Eh?!  Range object's description has changed?"
	  "  Can't find %S slot.", range-by-slot-name);
  end;
end function;

/// range-end -- Method on Exported Interface.
///
define method range-end (application :: <dfmc-application>,
		         range :: <range-object>)
    => (result :: false-or(<number>))
  let app = application.application-target-app;
  let (class-name :: <string>, slots :: <sequence>, rem-vals :: <sequence>)
    // Choose any value since let won't let me leave the vars unbound.
    = values ("", #(), #());
  do-env-prot-query
    (app, method () => ()
	    let (temp-class-name :: <string>, temp-slots :: <sequence>,
		 temp-rem-vals :: <sequence>)
	      = describe-dylan-object(app, range.application-object-proxy);
	    class-name := temp-class-name;
	    slots := temp-slots;
	    rem-vals := temp-rem-vals;
	  end);
  //
  // Check range not infinite.  Yes, this is gross code!!!
  if (as-lowercase(class-name) = "<finite-range>")
    //
    // Double check slot access to see if it is named correctly
    if (as-lowercase(slots[range-size-slot-offset]) = range-size-slot-name)
      do-range-start(app, slots, rem-vals)
	+ (do-range-by(app, slots, rem-vals)
	     // Don't bother checking valid? return value since we know this
	     // must win.
	     * dylan-object-immediate-value(app,
					    rem-vals[range-size-slot-offset]));
    else
      error("Eh?!  Range object's description has changed?"
	    "  Can't find %S slot.", range-size-slot-name);
    end;
  else
    #f;
  end;
end method;


/// pair-head -- Method on Exported Interface.
///
define method pair-head (application :: <dfmc-application>,
			 pair :: <pair-object>)
    => (head :: <application-object>)
end method;

/// pair-tail -- Method on Exported Interface.
///
define method pair-tail (application :: <dfmc-application>,
			 pair :: <pair-object>)
    => (head :: <application-object>)
end method;

