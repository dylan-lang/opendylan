module:    devel-dbg-ui
synopsis:  Counter groups are a collection of counters for functions
author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////
// A <function-tree> is used in the implementation of each
// <thread-counter-group>. It is a binary search tree whose keys are
// function address ranges, and whose nodes contain a <function-counters>
// object for maintaining the top-of-stack and seen-on-stack counts for
// the corresponding function.
//

define constant <function-tree> = false-or(<function-tree-node>);

define class <function-tree-node> (<object>)

  constant slot function-tree-parent :: <function-tree> = #f,
       required-init-keyword: parent:;

  slot function-tree-left :: <function-tree> = #f;

  slot function-tree-right :: <function-tree> = #f;

  constant slot function-tree-lower-limit :: <integer>,
       required-init-keyword: lower-limit:;

  constant slot function-tree-upper-limit :: <integer>,
       required-init-keyword: upper-limit:;

  constant
    slot function-tree-function-descriptor :: <single-function-descriptor>,
      required-init-keyword: function:;

end;


//////
// Calls the supplied function with each node of a <function-tree> in turn
// as the function's argument. This an in-order walk of the tree.
//
define method function-tree-walk (node :: <function-tree>, f :: <function>) => ()
  if (node)
    function-tree-walk (node.function-tree-left, f);
    f (node);
    ignore(node.function-tree-parent); // Surpress !$&*@ compiler warnings!
    function-tree-walk (node.function-tree-right, f);
  end if;
end method;


//////
// Calculate the maximum depth of a <function-tree>
//
define method function-tree-max-depth (node :: <function-tree>)
                                => (depth :: <integer>)

  if (node)
    let left-depth  = function-tree-max-depth (node.function-tree-left);
    let right-depth = function-tree-max-depth (node.function-tree-right);
    max (left-depth, right-depth) + 1
  else
    0
  end if
end method;


//////
// Find the node in a <function-tree> corresponding to the instruction
// address supplied (which is a <remote-value>). If no node exists for
// the address, one is created using the supplied make-node function
// and placed in the correct position in the tree, and returned as the
// result of the find operation.
//
define method function-tree-find-node
    (node :: <function-tree>, ip :: <remote-value>, make-node :: <function>)
 => (node :: <function-tree-node>)

  let iip = as-integer-losing-precision (ip);
  let parent = node;
  let found = #f;

  until (found)
    if (iip < node.function-tree-lower-limit)
      unless (node.function-tree-left)
        node.function-tree-left := make-node (parent, ip);
        found := #t;
      end unless;
      parent := node;
      node := node.function-tree-left;

    elseif (iip > node.function-tree-upper-limit)
      unless (node.function-tree-right)
        node.function-tree-right := make-node (parent, ip);
        found := #t;
      end unless;
      parent := node;
      node := node.function-tree-right;

    else
      found := #t;
    end if
  end until;

  node;
end method;

define method function-tree-make-node
    (parent :: <function-tree>, ip :: <remote-value>, target :: <debug-target>) 
 => (function :: <function-tree-node>)
  let ap = debug-target-access-path(target);
  let (start-ip, finish-ip) = function-bounding-addresses (ap, ip);
  let start-integer = as-integer-losing-precision (start-ip);
  let finish-integer = as-integer-losing-precision (finish-ip);
  let start = min (start-integer, finish-integer);
  let finish = max (start-integer, finish-integer);
  let function = make (<single-function-descriptor>, 
	               address: ip, debug-target: target);
  make (<function-tree-node>, parent: parent, 
        lower-limit: start, upper-limit: finish, function: function);
end method;

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////


//////
// A <thread-counter-group> is a set of counters which maintain the top-
// of-stack and seen-on-stack counts for the functions in a thread.
// 
//
define class <thread-counter-group> (<object>)

  constant slot thread-counter-group-debug-target :: <debug-target>,
       required-init-keyword: debug-target:;

  slot thread-counter-group-tree :: <function-tree> = #f;

  constant slot thread-counter-group-counters :: <table> = make(<table>);

  slot thread-counter-group-cpu-time :: <integer> = 0;

  slot thread-counter-group-samples :: <integer> = 0;

end class;

define method thread-counter-group-find-counters
    (group :: <thread-counter-group>, function :: <function-descriptor>)
 => (counters :: <first-function-counters>)
  let counters = thread-counter-group-counters(group);
  element(counters, function, default: #f)
    | (element(counters, function) 
         := make(<first-function-counters>, function: function))
end method;

//////
// Retrieve the <function-descriptor> object for the given instruction
// pointer from the thread counter group. If such an object does not
// exist for this address then one is created.
//
define method thread-counter-group-find-function
    (group :: <thread-counter-group>, ip :: <remote-value>)
 => (function :: <single-function-descriptor>)

  let target = group.thread-counter-group-debug-target;
  let tree   = group.thread-counter-group-tree;
  let node =
    if (tree)
      function-tree-find-node
	(tree, ip, rcurry(function-tree-make-node, target))
    else
      group.thread-counter-group-tree 
	:= function-tree-make-node(#f, ip, target)
    end if;

  node.function-tree-function-descriptor
end method;

/*
define method thread-counter-group-as-sequences 
    (group :: <thread-counter-group>) 
 => (seen :: <simple-object-vector>, top :: <simple-object-vector>)
  let seen = as(<simple-object-vector>, thread-counter-group-counters(group));
  let top  = choose(method (counters) function-counters-top(counters) > 0 end, seen);
  values(seen, top)
end method;
*/

//////
// Update the counters in a group according to a thread profile snapshot.
//

/// TODO: SPLIT OUT REDUNDANT BITS

define method normalize-second-counters 
    (counters-list :: <stretchy-vector>, weight :: <single-float>,
     inclusive? :: <boolean>, exclusive? :: <boolean>)
  for (counters :: <first-function-counters> in counters-list)
    if (inclusive?)
      let normalizer :: <single-float>
	= as(<single-float>, function-counters-callee-normalizer(counters));
      for (callee :: <second-function-counters> in function-counters-callees(counters))
	let subtotal = function-counters-subtotal(callee);
	if (subtotal > 0)
	  incf(function-counters-seen(callee), subtotal * weight / normalizer);
	  function-counters-subtotal(callee) := 0;
	end if;
      end for;
      function-counters-callee-normalizer(counters) := 0;
      let normalizer
	= as(<single-float>, function-counters-caller-normalizer(counters));
      for (caller :: <second-function-counters> in function-counters-callers(counters))
	let subtotal = function-counters-subtotal(caller);
	if (subtotal > 0)
	  incf(function-counters-seen(caller), subtotal * weight / normalizer);
	  function-counters-subtotal(caller) := 0;
	end if;
      end for;
      function-counters-caller-normalizer(counters) := 0;
    end if;

    if (exclusive?)
      let top-normalizer
	= as(<single-float>, function-counters-caller-top-normalizer(counters));
      for (caller :: <second-function-counters> in function-counters-callers(counters))
	let subtotal = function-counters-top-subtotal(caller);
	if (subtotal > 0)
	  incf(function-counters-top(caller), subtotal * weight / top-normalizer);
	  function-counters-top-subtotal(caller) := 0;
	end if;
      end for;
      function-counters-caller-top-normalizer(counters) := 0;
      let top-normalizer :: <single-float>
	= as(<single-float>, function-counters-callee-top-normalizer(counters));
      for (callee :: <second-function-counters> in function-counters-callees(counters))
	let subtotal = function-counters-top-subtotal(callee);
	if (subtotal > 0)
	  incf(function-counters-top(callee), subtotal * weight / top-normalizer);
	  function-counters-top-subtotal(callee) := 0;
	end if;
      end for;
      function-counters-callee-top-normalizer(counters) := 0;
    end if;

  end for;
end method;

define method print-snapshot 
    (group :: <thread-counter-group>, snapshot :: <thread-snapshot>, ambiguous-index :: <integer>)
  format-out("\nSNAPSHOT\n\n");
  let ips = instruction-pointers(snapshot);
  for (ip in ips,
       i :: <integer> from size(ips) by -1)
    let function     = thread-counter-group-find-function (group, ip);
    let display-name = function-descriptor-display-name(function);
    format-out("%s%s\n", if (i = ambiguous-index) "@ " else "  " end, display-name);
  end for;
end method;

define variable *profiler-debug?* = #f;

define inline method debug-format (string :: <byte-string>, #rest args)
  if (*profiler-debug?*)
    apply(format-out, string, args);
  end if;
end method;

define method thread-snapshots-ambiguous-index 
    (s0 :: false-or(<thread-snapshot>), s1 :: false-or(<thread-snapshot>)) 
 => (res :: <integer>)
  if (s0 & s1)
    for (ip0 in instruction-pointers(s0) using backward-iteration-protocol,
	 ip1 in instruction-pointers(s1) using backward-iteration-protocol,
	 i :: <integer> from 0,
	 until: ip0 ~== ip1)
    finally    
      i
    end for;
  else
    0
  end if;
end method;

define inline method update-second-statistics 
    (group :: <thread-counter-group>, snapshot-weight :: <integer>,
     aggregates-1 :: <simple-object-vector>, aggregates-2 :: <simple-object-vector>, 
     counters-functions :: <function>, counters-functions-setter :: <function>,
     counters-subtotal :: <function>, counters-subtotal-setter :: <function>,
     counters-normalizer :: <function>, counters-normalizer-setter :: <function>)
  for (aggregate-1 in aggregates-1)
    let counters-1
      = thread-counter-group-find-counters(group, aggregate-1);
    for (aggregate-2 in aggregates-2)
      unless (aggregate-1 == aggregate-2) // skip self calls
	let counters-2
	  = element(counters-1.counters-functions, aggregate-2, default: #f)
	  | (element(counters-1.counters-functions, aggregate-2) 
	       := make(<second-function-counters>, function: aggregate-2));
	debug-format("  AGG-1 %s AGG-2 %s\n", 
		     function-descriptor-display-name(aggregate-1),
		     function-descriptor-display-name(aggregate-2));
	incf(function-counters-called(counters-2));
	incf(counters-subtotal(counters-2), snapshot-weight);
	incf(counters-normalizer(counters-1), snapshot-weight);
      end unless;
    end for;
  end for;
end method;

define method thread-counter-group-process-snapshot
    (group :: <thread-counter-group>, snapshot-weight :: <integer>,
     thread-snapshot :: <thread-snapshot>,
     last-thread-snapshot :: false-or(<thread-snapshot>),
     #key inclusive? = #t, exclusive? = #t)
 => ()
  let seen-counters-1-list :: <stretchy-vector> = make(<stretchy-vector>);
  let callee-aggregates-1 :: <simple-object-vector> = #[];
  let callee-aggregates-2 :: <simple-object-vector> = #[];
  let ambiguous-index
    = thread-snapshots-ambiguous-index(thread-snapshot, last-thread-snapshot);
  if (*profiler-debug?*)
    print-snapshot(group, thread-snapshot, ambiguous-index);
    format-out("\nPROCESSING RESULTS...\n\n");
  end if;
  debug-format("AMBIGUOUS INDEX %s\n", ambiguous-index);
  let ips = instruction-pointers (thread-snapshot);
  let count :: <integer> = 0;
  let count-increment :: <integer> = 0;
  for (ip in ips,
       i :: <integer> from size(ips) by -1)
    if (inclusive? | (exclusive? & count < 2))
      let function     = thread-counter-group-find-function (group, ip);
      let aggregates-1 = function-descriptor-aggregates-1(function);
      let aggregates-2 = function-descriptor-aggregates-2(function);
      debug-format("PROCESSING %s\n", function-descriptor-display-name(function));
      for (function in aggregates-1)
	debug-format("  LUMP %s\n", function-descriptor-display-name(function));
	let counters-1 = thread-counter-group-find-counters(group, function);
	if (member?(counters-1, seen-counters-1-list))
	  count-increment := 0;
	else 
	  if (count = 0)
	    debug-format("  TOP %s\n", function-descriptor-display-name(function));
	    incf(function-counters-top(counters-1), snapshot-weight);
	    group.thread-counter-group-cpu-time
	      := group.thread-counter-group-cpu-time + snapshot-weight;
	  end if;
	  count-increment := 1;
	  add!(seen-counters-1-list, counters-1);
	  incf(function-counters-seen(counters-1), as(<single-float>, snapshot-weight));
	  debug-format("  SEEN %s\n", function-descriptor-display-name(function));
	end if;
	if (i > ambiguous-index)
	  incf(function-counters-called(counters-1));
	end if;
      end for;
      if (i > ambiguous-index)
	if (exclusive?)
	  if (count = 0 & count-increment = 1)
	    update-second-statistics
	      (group, snapshot-weight, aggregates-1, callee-aggregates-2,
	       function-counters-callees, function-counters-callees-setter,
	       function-counters-top-subtotal, function-counters-top-subtotal-setter,
	       function-counters-callee-top-normalizer, function-counters-callee-top-normalizer-setter);
	  end if;
	  if (count = 1)
	    update-second-statistics
	      (group, snapshot-weight, callee-aggregates-1, aggregates-2,
	       function-counters-callers, function-counters-callers-setter,
	       function-counters-top-subtotal, function-counters-top-subtotal-setter,
	       function-counters-caller-top-normalizer, function-counters-caller-top-normalizer-setter);
	  end if;
	end if;
	if (inclusive?)
	  update-second-statistics
	    (group, snapshot-weight, aggregates-1, callee-aggregates-2,
	     function-counters-callees, function-counters-callees-setter,
	     function-counters-subtotal, function-counters-subtotal-setter,
	     function-counters-callee-normalizer, function-counters-callee-normalizer-setter);
	  update-second-statistics
	    (group, snapshot-weight, callee-aggregates-1, aggregates-2,
	     function-counters-callers, function-counters-callers-setter,
	     function-counters-subtotal, function-counters-subtotal-setter,
	     function-counters-caller-normalizer, function-counters-caller-normalizer-setter);
	end if;
	unless (empty?(aggregates-2) & empty?(callee-aggregates-1)) 
	  debug-format("  RESETTING CALLEE-AGGREGATES-2\n");
	  callee-aggregates-2 := aggregates-2; // 2nd order recorded move on
	end unless;
	unless (empty?(aggregates-1) & empty?(callee-aggregates-2))
	  debug-format("  RESETTING CALLEE-AGGREGATES-1\n");
	  callee-aggregates-1 := aggregates-1; // 2nd order recorded move on
	end unless;
      end if;
      incf(count, count-increment);
    end if;
  end for;

  normalize-second-counters
    (seen-counters-1-list, as(<single-float>, snapshot-weight),
     inclusive?, exclusive?);

end method;

define thread variable *function-descriptor-index* = 0;
define thread variable *function-descriptors* = #();

/*
define method index-and-collect-function-descriptors 
    (counters-list :: <simple-object-vector>)
  do (method (counter) 
	let function = function-counters-descriptor(counter);
	if (instance?(function, <single-function-descriptor>))
	  unless (function-descriptor-index(function))
	    function-descriptor-index(function) := *function-descriptor-index*;
	    *function-descriptors* := add!(*function-descriptors*, function);
	    incf(*function-descriptor-index*);
	  end unless;
	end if;
      end method,
      counters-list);
end method;
*/

define method function-counters-display-sorted-table
    (counters :: <table>, target :: <debug-target>, 
     counter :: <function>, total :: false-or(<single-float>), 
     limits :: <list>, top-ns :: <list>, depth :: <integer>, max-depth :: <integer>,
     max-height :: false-or(<integer>))
  let sorted-seen :: <simple-object-vector>
    = sort(as(<vector>, counters), test: method(x, y) counter(x) > counter(y) end);
  // index-and-collect-function-descriptors(sorted-seen);
  let actual-total :: <single-float>
    = reduce(method (r :: <single-float>, c) r + counter(c) end, 0.0, sorted-seen);
  let total :: <single-float> = total | actual-total;
  let cumulative :: <single-float> = 0.0;
  local method limit-precision 
            (x :: <number>, digits :: <integer>) => (res :: <string>)
          if (instance?(x, <float>))
	    let s = float-to-string(x);
            let i = position(s, '.') | size(s);
	    copy-sequence(s, end: min(i + digits + 1, size(s)));
	  elseif (instance?(x, <integer>))
	    integer-to-string(x)
	  else
	    error("Whoops! Cannot convert %= to a string", x)
	  end if
	end method,
        method display-results (value, count, height, percentage, cumulative, separator, name)
	  format-out("%s%10s %8s %6s %7s %s %s\n",
		     if (depth = 1) "" else "  " end if,
		     value, count, /* height, */ percentage, cumulative, separator, name)
	end method;
  let top-n = head(top-ns);
  let limit = head(limits);
  block (blow)
    let count :: <integer> = 0;
    for (counters :: <function-counters> in sorted-seen)
      unless (total <= 0.0 | actual-total <= 0.0)
	let function = function-counters-descriptor(counters);
	if (instance?(function, <single-function-descriptor>))
	  unless (function-descriptor-index(function))
	    function-descriptor-index(function) := *function-descriptor-index*;
	    *function-descriptors* := add!(*function-descriptors*, function);
	    incf(*function-descriptor-index*);
	  end unless;
	end if;
	let height = if (max-height) function-counters-height(counters) else 0 end;
	case
	  (cumulative > limit) | (top-n & count >= top-n) =>
	    if (top-n & count > 0)
	      display-results
		("...", "...", "...", limit-precision(max(100 - cumulative, 0.0), 2), 
		 limit-precision(100.0, 2), ".", "...");
	      blow();
	    end if;
	  ~max-height | (height < max-height) =>
	    let value :: <single-float>      = as(<single-float>, counter(counters));
	    let called                       = function-counters-called(counters);
	    let percentage :: <single-float> = value * 100.0 / total;
	    let actual-percentage :: <single-float> = value * 100.0 / actual-total;
	    incf(count);
	    incf(cumulative, actual-percentage);
	    if (depth < max-depth)
	      function-counters-display-sorted-table
		(function-counters-callees(counters), 
		 target, counter, #f, tail(limits), tail(top-ns), 
		 depth + 1, max-depth, #f);
	    end if;
	    let function
	      = function-counters-descriptor(counters);
	    let display-name
	      = format-to-string
		  ("%s %s",
		   function-descriptor-display-name(function),
		   function-descriptor-index-name(function));
	    display-results
	      (limit-precision(value, 2), limit-precision(called, 2),
	       limit-precision(height, 2),
	       limit-precision(percentage, 2), limit-precision(cumulative, 2), 
	       if (depth = 1) "+" else "|" end, display-name);
	    if (depth < max-depth)
	      function-counters-display-sorted-table
		(function-counters-callers(counters), 
		 target, counter, #f, tail(limits), tail(top-ns), 
		 depth + 1, max-depth, #f);
	      format-out("\n");
	    end if;
	end case;
      end unless;
    end for;
  end block;
end method;

/*
define method compute-call-height (group :: <thread-counter-group>) => ()
  local method function-name (counter)
	  function-descriptor-display-name(function-counters-descriptor(counter))
	end method;
  let counters = 
    thread-counter-group-counters(group);
  format-out("CALLEES\n");
  for (counter :: <first-function-counters> in counters)
    format-out("  FUNCTION %s\n", function-name(counter));
    for (callee in function-counters-callers(counter))
      let callee :: <first-function-counters> = 
	thread-counter-group-find-counters
	  (group, function-counters-descriptor(callee));
      format-out("    CALLEE %s\n", function-name(callee));
    end for;
  end for;
  format-out("LEAVES\n");
  let queue :: <deque> =
    collecting (as <deque>)
      for (counter :: <first-function-counters> in counters)
	if (empty?(function-counters-callees(counter)))
	  format-out("  %s\n", function-name(counter));
          collect(counter)
	end if
      end for;
    end collecting;
  let visited? :: <set> = make(<set>);
  until (empty?(queue))
    let counters :: <first-function-counters> = pop(queue);
    add!(visited?, counters);
    format-out("VISITING %s HEIGHT %=\n", 
	       function-name(counters), function-counters-height(counters));
    for (caller-2nd-counters in function-counters-callers(counters))
      let caller :: <first-function-counters> = 
	thread-counter-group-find-counters
	  (group, function-counters-descriptor(caller-2nd-counters));
      format-out("  CALLER %s HEIGHT %=\n", 
		 function-name(caller), function-counters-height(caller));
      function-counters-height(caller)
	:= max(function-counters-height(caller), 
	       function-counters-height(counters) + 1);
      unless (member?(caller, visited?))
	push-last(queue, caller);
      end unless;
    end for;
  end until;
  format-out("\n");
end method;
*/

//////
// Display <thread-counter-group> results.
//
define method thread-counter-group-display
    (group :: <thread-counter-group>, heading :: <string>, 
     #key inclusive? = #t, exclusive? = #t, index? = #t)
 => ()

  let cpu-time = as(<single-float>, group.thread-counter-group-cpu-time);
  let samples = group.thread-counter-group-samples;
  let debug-target = group.thread-counter-group-debug-target;
  let limits0 = list(application-profile-limit-0(debug-target));
  let top-ns0 = list(application-profile-top-n-0(debug-target));
  let limits = list(application-profile-limit-1(debug-target), application-profile-limit-2(debug-target));
  let top-ns = list(application-profile-top-n-1(debug-target), application-profile-top-n-2(debug-target));
  let summary-top-n = 10;

  dynamic-bind (*function-descriptor-index* = 0,
		*function-descriptors* = make(<stretchy-vector>))
    format-out ("  Total %s: %d\n", heading, cpu-time);
    format-out ("  Number of samples: %d\n\n", samples);
    if (samples > 0)

      if (inclusive?)
	// compute-call-height(group); 

	// Display the seen counts
	format-out ("  Inclusive Summary Report\n");
	format-out ("  ====================\n\n");
	format-out ("  %16s |   Function name\n", heading);
	format-out ("  ----------------------------------------------------------------------------\n");
	function-counters-display-sorted-table
	  (thread-counter-group-counters(group), debug-target, 
	   function-counters-seen, as(<single-float>, cpu-time), limits0, top-ns0, 1, 1,
	   application-profile-height(debug-target));
	format-out ("\n\n");
	format-out ("  Inclusive Report\n");
	format-out ("  ====================\n\n");
	format-out ("  %16s |   Function name\n", heading);
	format-out ("  ----------------------------------------------------------------------------\n");
	function-counters-display-sorted-table
	  (thread-counter-group-counters(group), debug-target, 
	   function-counters-seen, as(<single-float>, cpu-time), limits, top-ns, 1, 2,
	   application-profile-height(debug-target));
      end if;

      if (exclusive?)
	// Display the top counts
	format-out ("  Exclusive Summary Report\n");
	format-out ("  =======================\n\n");
	format-out ("  %16s |   Function name\n", heading);
	format-out ("  ----------------------------------------------------------------------------\n");
	function-counters-display-sorted-table
	  (thread-counter-group-counters(group), debug-target, 
	   function-counters-top, as(<single-float>, cpu-time), limits0, top-ns0, 1, 1, #f);
	format-out ("\n\n");
	format-out ("  Exclusive Report\n");
	format-out ("  =======================\n\n");
	format-out ("  %16s |   Function name\n", heading);
	format-out ("  ----------------------------------------------------------------------------\n");
	function-counters-display-sorted-table
	  (thread-counter-group-counters(group), debug-target, 
	   function-counters-top, as(<single-float>, cpu-time), limits, top-ns, 1, 2, #f);
      end if;

      if (index?)
	// Display the function descriptor info
	format-out ("\n\n");
	format-out ("  Function Descriptions\n");
	format-out ("  =======================\n\n");
	for (function in *function-descriptors*)
	  format-out("  %6s %s %s\n", 
		     function-descriptor-index-name(function), 
		     function-descriptor-display-name(function),
		     function-descriptor-locator-name(function));
	end for;
      end if;

    end if;
  end dynamic-bind;
end method;

