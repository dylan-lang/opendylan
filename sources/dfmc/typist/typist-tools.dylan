Module:    DFMC-Typist
Author:    Steve Rowley
Synopsis:  Tools used in the typist, i.e., a grab-bag of randumb stuff.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
/// These are general utilities used in the typist, but which aren't
/// themselves about type definition, checking, or inference.  Possibly
/// they should go somewhere else.  Feel free to do the right thing.
///

//define macro when
//  // OK, I admit I can't do without it.
//  { when (?test:expression) ?the-body:body end }
//    => { if (?test) ?the-body end }
//end;

define macro ecase
  // Like case, with an otherwise-error clause at the end.
  { ecase ?clauses:case-body end }
    => { case ?clauses; otherwise => error("Fell off ecase.") end }
end;

define function print-separated-collection(coll :: <collection>,
                                           #key stream      = *standard-output*,
                                                printer     = print-object,
                                                separator   = ", ",
                                                conjunction = #f)
                                          => (coll :: <collection>)
  // [How many times have you re-written this?  (CLIM users need not apply.)]
  //
  // Print a collection of values on stream.  Put separater between adjacent
  // elements.  If conjunction is supplied, it goes between the last 2 elements
  // instead of the separator; spaces get put around it.  E.g.: 1, 2, 3, and 4
  //
  // Separator = #f uses a space; for no space at all, use separator = "".
  //
  // Implementation note: this uses the forward iteration protocol, as opposed
  // to the simpler "for (x in coll) ... end" in order to be able to identify
  // the last iteration (when conjunction is used instead of separator).
  // It's also written to be loadable in the absence of format.
  // It uses write because of the problems with print libraries in emulator.
  let (init, limit, next, finished?, key-fn, val-fn) =       // ignore key-fn
    forward-iteration-protocol(coll);
  for (first? = #t then #f,                                  // First iteration?
       state  = init then next(coll, state),                 // Loop over coll
       until: finished?(coll, state, limit))
    unless (first?)                                          // 1st iter: no sep
      if (conjunction & finished?(coll, next(coll, state), limit))
        write-element(stream, ' ');
        write(stream, conjunction);
        write-element(stream, ' ')
      elseif (separator)                                     // Not last iter
        write(stream, separator)
      else
        write-element(stream, ' ')                           // else no sep
      end
    end;
    printer(val-fn(coll, state), stream)                     // print coll item
  end;
  coll                                                       // Return coll
end;

define inline function map-table(table :: <table>, fn :: <function>) => ()
  // MAPHASH revenant.  Calls fn on key/value pairs of table.
  let (init, limit, next, finished?, key-fn, val-fn)
    = forward-iteration-protocol(table);
  for (state = init then next(table, state),
       until: finished?(table, state, limit))
    fn(key-fn(table, state), val-fn(table, state))
  end;
  values()
end;

// It looks like #f is sometimes used to represent the empty table
// internally, hence these ugly type declarations.

define function table=?
    (t1 :: false-or(<table>), t2 :: false-or(<table>), val=? :: <function>)
 => (same? :: <boolean>)
  // Predicate to compare tables for equality of keys & values.
  // *** Is there an \= method on table I should use instead of this?
  block(bail)
    when (t1 == t2) bail(#t) end;                             // Cheap EQ check
    map-table(t1,
              method (key1, val1)
                let val2 = element(t2, key1, default: $unfound);
                when (unfound?(val2) | ~val=?(val1, val2))  // or different
                  bail (#f)                                   // so fail.
                end
              end);
    // Now we know t1 is a subset of t2.  "t2 subset of t1" is a length check.
    size(t1) = size(t2)
  end
end;

define function table-key-subset?(t1 :: <table>, t2 :: <table>)
   => (subsets? :: <boolean>)
  // Are the keys of t1 a subset of the keys of t2?
  // There's probably a much more efficient implementation of this!
  let keys1 = key-sequence(t1);
  let keys2 = key-sequence(t2);
  every?(rcurry(member?, keys2), keys1) // should use a key test.
end;

