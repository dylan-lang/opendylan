module:    test-profiler
synopsis:  Counter groups and associated methods
author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Counters can be put together into counter groups. Each type of count
// the profiler calculates has a different counter group associated with it.
// (The profiler only keeps two counts at the moment - the number of times
// each function was seen on the stack, and the number of times it was seen
// at the top of the stack).

// A counter group allows counters to be retrieved by indexing with the
// counter's name or an instruction pointer. There is a one-to-one
// correspondence between names and counters, but there may be a many-to-one
// correspondence between instruction pointers and counters. A counter group
// must be supplied with a method to translate from instruction pointers to
// names when it is created.
//
// Internally, counter groups have two tables which share the group of
// counter objects; one indexed by names, the other by instruction pointers.
// A counter may be referenced by only one entry in the name table, but may
// be referenced by more than one entry in the instruction pointer table.
// Every counter in the group is referenced from an entry in the name table.
//
define class <counter-group> (<object>)

  // A method for translating from instruction pointers to names
  slot as-name :: <function>,
       required-init-keyword: as-name:;

  // A table indexed by instruction pointers with counters as elements
  slot ip-to-counter-table :: <table>,
       init-function: method () make (<table>) end;

  // A table indexed by names with counters as elements
  slot name-to-counter-table :: <string-table>,
       init-function: method () make (<string-table>) end;
end;


// Retrieve the counter for specified name. If a counter does not exist for
// the name then one is created.
//
define method find-counter (group :: <counter-group>, name :: <string>)
                        => (counter :: <counter>)

  let counter = element (group.name-to-counter-table, name, default: #f);
  unless (counter)
    counter := make (<counter>, name: name);
    group.name-to-counter-table[name] := counter;
  end unless;
  counter
end method;


// Retrieve the counter for the given instruction pointer. If a counter does
// not exist, the instruction pointer is translated to name which is used
// to find the counter (creating one if there is none for that name).
//
define method find-counter (group :: <counter-group>, ip :: <remote-value>)
                        => (counter :: <counter>)

  let iip = as-integer (ip);
  let counter = element (group.ip-to-counter-table, iip, default: #f);
  unless (counter)
    let name = (group.as-name)(ip);
    counter := find-counter (group, name);
    group.ip-to-counter-table[iip] := counter;
  end unless;
  counter
end method;


// Put the counters into a sequence for sorting and displaying. Since
// there is a one-to-one correspondence between names and counters, if
// we step through every entry of the name-to-counter table we get every
// counter in the group without any repititions.
//
define method as-counter-sequence (group :: <counter-group>)
                               => (counters :: <counter-sequence>)

  let counters = make (<counter-sequence>,
                       size: size (group.name-to-counter-table));
  for (index from 0, counter in group.name-to-counter-table)
    counters[index] := counter;
  end for;
  counters
end method;

