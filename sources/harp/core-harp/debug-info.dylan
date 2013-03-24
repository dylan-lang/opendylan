module:    harp-debug-info
Synopsis:  HARP debug info generation
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Revised to compact debug-info    Nosa Omo  May, 1998

//// Support for source code locations

/// HARP has its own model of what source locators look like. This makes
/// use of the <source-record> class - but avoids the use of <source-location>
/// so that it's sensible to store the locators persistently.


define abstract class <source-position-internal> (<object>)
end class;

define constant <source-position> = type-union(<integer>, <source-position-internal>);



// The class which corresponds to a source code location point within a lambda.
// Locations are relative to the absolute source position associated with the 
// lambda.
//
// NB the relative numbering starts at 0, not 1 as used sometimes by COFF.

define class <relative-source-position-internal> (<source-position-internal>)

  constant slot function-relative-line-number-internal :: <integer>,
    required-init-keyword: line-number:;

  constant slot function-relative-code-position-internal :: <integer>,
    required-init-keyword: code-position:;

end class;


// Conditionally pack <relative-source-position> in its entirety;
// these can be represented as an integer for max 2^12 line number and
// max 2^17 code-position

define constant <relative-source-position> = type-union(<integer>, <relative-source-position-internal>);
define constant <relative-source-position-unpacked-slots> = <relative-source-position-internal>;
define constant relative-source-position-packed-slot = identity;

// define constant dummy-setter = method(value, instance) value end;

define packed-slots? (<relative-source-position>)
  constant function-relative-line-number      :: <integer>, init-keyword: line-number:,     width: 12;
  constant function-relative-code-position    :: <integer>, init-keyword: code-position:,   width: 17;
end;




// The class which encapsulates the source record corresponding
// to a lambda. 

define class <absolute-source-position> (<source-position-internal>)

  constant slot source-position-source-record :: <source-record>,
    required-init-keyword: source-record:;

  constant slot start-offset-into-source-record :: <integer>,
    required-init-keyword: start-offset:;

  constant slot end-offset-into-source-record :: <integer>,
    required-init-keyword: end-offset:;

end class;


define generic source-record-start-line 
    (pos :: <absolute-source-position>) => (line :: <integer>);


define generic source-record-end-line 
    (pos :: <absolute-source-position>) => (line :: <integer>);


define generic source-record-file-name 
    (pos :: <absolute-source-position>) => (file :: false-or(<byte-string>));



/// Here's the implementation of the protocol for <absolute-source-position>

define method source-record-start-line
    (pos :: <absolute-source-position>)  => (line :: <integer>)
  let sr = pos.source-position-source-record;
  let line = pos.start-offset-into-source-record;
  let (name, adjusted-line) = source-line-location(sr, line);
  adjusted-line
end method;

define method source-record-end-line
    (pos :: <absolute-source-position>)  => (line :: <integer>)
  let sr = pos.source-position-source-record;
  let line = pos.end-offset-into-source-record;
  let (name, adjusted-line) = source-line-location(sr, line);
  adjusted-line
end method;

define method source-record-file-name
    (pos :: <absolute-source-position>)  => (name :: false-or(<byte-string>))
  pos.source-position-source-record.source-record-name;
end method;



//// Support for coercing <source-location> objects into <source-position>s


define open generic locator-as-absolute-source-position 
    (locator) => (source-pos :: <absolute-source-position>);


define open generic make-relative-source-position 
    (abs :: <absolute-source-position>, 
     locator,
     code-pos :: <integer>) 
     => (source-pos :: <relative-source-position>);


define method locator-as-absolute-source-position 
    (locator :: <source-location>) => (source-pos :: <absolute-source-position>)
  make(<absolute-source-position>,
       source-record: locator.source-location-source-record, 
       start-offset: locator.source-location-start-offset.source-offset-line,
       end-offset: locator.source-location-end-offset.source-offset-line)
end method;


define method make-relative-source-position 
    (abs :: <absolute-source-position>, 
     locator :: <source-location>, 
     code-position :: <integer>)
     => (source-pos :: <relative-source-position>)
  let abs-sr = abs.source-position-source-record;
  let rel-sr = locator.source-location-source-record;
  let rel-line = locator.source-location-start-offset.source-offset-line;
  let base-line = abs.source-record-start-line;
  let (name, this-line) = source-line-location(rel-sr, rel-line);
  unless (abs-sr == rel-sr)
    harp-error("Encountered a source locator (%=) which is outside its parent (%=).",
               locator, abs-sr);
  end unless;
  let line-number = this-line - base-line;

  make-relative-source-position-packed-slot?(line-number,
					     code-position)
  | (make(<relative-source-position-internal>,
	    line-number: line-number,
	    code-position: code-position));

end method;


define open generic dummy-harp-source-locator?(locator) => (dummy? :: <boolean>);

define method dummy-harp-source-locator?(locator) => (dummy? :: <boolean>)
  #f
end method;


//// Support for variables and scopes



/// <named-variable>
///
/// This includes name and location information, in a similar manner to
/// a <virtual-register>. We don't just use <virtual-register> because
/// there is no need to hang on to all the baggage associated with how
/// to do the register allocation. Instances of <named-variable> should be 
/// as simple as possible, because they are persistently stored for incremental
/// compilation.

define abstract primary class <named-variable> (<local-variable>)
  constant slot variable-name :: <byte-string>,
    required-init-keyword: name:;
end class;



/// Define abstract classes for register and spill variables., and for 
/// variables naming indirections (for closures etc).
/// We will eventually need separate subclasses for raw types too.

define abstract primary class <variable-in-register> (<named-variable>)
  constant slot variable-register-enumeration :: <integer>,
    required-init-keyword: enumeration:;
end class;


define abstract primary class <variable-in-spill> (<named-variable>)
  constant slot variable-frame-pointer-offset :: <integer>,
    required-init-keyword: offset:;
end class;


define abstract primary class <variable-in-frame-spill> (<variable-in-spill>)
end class;


define abstract primary class <variable-in-leaf-spill> (<variable-in-spill>)
end class;


define abstract class <variable-indirections> (<local-variable>)
  // Contains a vector of <local-variable> objects
  constant slot variable-indirections :: <simple-object-vector>,
    required-init-keyword: indirections:;
end class;


define abstract class <variable-indirection-offset> (<local-variable>)
  constant slot variable-indirection-offset :: <integer>,
    required-init-keyword: indirection-offset:;
end class;




/// Define concrete classes for all variables.

define class <named-variable-in-register> (<variable-in-register>)
end class;


define class <named-variable-in-spill> (<variable-in-frame-spill>)
end class;


define class <named-variable-in-leaf-spill> (<variable-in-leaf-spill>)
end class;


define class <indirections-variable-in-register> (<variable-indirections>, <variable-in-register>)
end class;


define class <indirections-variable-in-spill> (<variable-indirections>, <variable-in-frame-spill>)
end class;


define class <indirections-variable-in-leaf-spill> (<variable-indirections>, <variable-in-leaf-spill>)
end class;


define class <indirections-variable-in-indirection> (<variable-indirections>, <variable-indirection-offset>)
end class;


define class <named-indirection> (<named-variable>, <variable-indirection-offset>)
end class;



/// Clients of HARP should use make-indirection-variable to define each node in
/// the indirection tree for defining closure maps etc.

define method make-indirection-variable
      (be :: <harp-back-end>, offset :: <integer>,
       #key name, sub-indirections :: <simple-object-vector> = #[])
     => (indirection :: <variable-indirection-offset>)
  if (sub-indirections.empty?)
    if (instance?(name, <byte-string>))
      make(<named-indirection>, name: name, indirection-offset: offset);
    else
	harp-error("Attempt to make an indirection variable with no name or dependents");
    end if;
  else
    make(<indirections-variable-in-indirection>, 
	 indirection-offset: offset, indirections: sub-indirections);
  end if;
end method;






define method make-named-variable 
      (be :: <harp-back-end>, name :: <byte-string>, 
       location :: <real-register>, with-frame :: <boolean>)
      => (var :: <named-variable>)
  make(<named-variable-in-register>, name: name, enumeration: 
       real-register-debug-info-enumeration(be, location));
end method;


define method make-named-variable 
      (be :: <harp-back-end>, name :: <byte-string>, 
       location :: <spill>, with-frame :: <boolean>)
      => (var :: <named-variable>)
  if (with-frame)
    make(<named-variable-in-spill>, name: name, 
	 offset: spill-frame-pointer-offset(be, location, with-frame));
  else
    make(<named-variable-in-leaf-spill>, name: name, 
	 offset: spill-frame-pointer-offset(be, location, with-frame));
  end if;
end method;


define method make-named-variable 
      (be :: <harp-back-end>, name :: <byte-string>, 
       location :: <integer>, with-frame :: <boolean>)
      => (var :: <named-variable>)
  if (with-frame)
    make(<named-variable-in-spill>, name: name, 
	 offset: spill-frame-pointer-offset(be, location, with-frame));
  else
    make(<named-variable-in-leaf-spill>, name: name, 
	 offset: spill-frame-pointer-offset(be, location, with-frame));
  end if;
end method;



define method make-named-indirections
      (be :: <harp-back-end>, name :: <byte-string>, 
       indirections :: <simple-object-vector>,
       location :: <real-register>, with-frame :: <boolean>)
      => (var :: <named-variable>)
  make(<indirections-variable-in-register>, name: name, 
       enumeration: real-register-debug-info-enumeration(be, location),
       indirections: indirections);
end method;


define method make-named-indirections
      (be :: <harp-back-end>, name :: <byte-string>, 
       indirections :: <simple-object-vector>,
       location :: <spill>, with-frame :: <boolean>)
      => (var :: <named-variable>)
  if (with-frame)
    make(<indirections-variable-in-spill>, name: name, 
	 offset: spill-frame-pointer-offset(be, location, with-frame),
	 indirections: indirections);
  else
    make(<indirections-variable-in-leaf-spill>, name: name, 
	 offset: spill-frame-pointer-offset(be, location, with-frame),
	 indirections: indirections);
  end if;
end method;


define method make-named-indirections
      (be :: <harp-back-end>, name :: <byte-string>, 
       indirections :: <simple-object-vector>,
       location :: <integer>, with-frame :: <boolean>)
      => (var :: <named-variable>)
  if (with-frame)
    make(<indirections-variable-in-spill>, name: name, 
	 offset: spill-frame-pointer-offset(be, location, with-frame),
	 indirections: indirections);
  else
    make(<indirections-variable-in-leaf-spill>, name: name, 
	 offset: spill-frame-pointer-offset(be, location, with-frame),
	 indirections: indirections);
  end if;
end method;




/// Back ends MUST specialize these: so that a platform-dependent 
/// debug-info encoding for variable locations can be determined.
/// spill may be either a <spill> or an <integer> corresponding to an
/// uncoloured arg-spill -  and both should be supported
/// NB: there is no default implementation


// spill-frame-pointer-offset returns an offset in bytes for the location of the 
// variable relative to the frame pointer

define open generic spill-frame-pointer-offset
      (backend :: <harp-back-end>, spill, with-frame :: <boolean>)
   => (offset :: <integer>);


// real-register-debug-info-enumeration returns a platform-specific integer
// encoding for the register. Back ends should choose an encoding which
// is appropriate for the native debug-info format (e.g. COFF / CodeView)

define open generic real-register-debug-info-enumeration
      (backend :: <harp-back-end>, register :: <real-register>)
   => (enumeration :: <integer>);


// real-register-from-debug-info-enumeration is the inverse of the above

define open generic real-register-from-debug-info-enumeration
      (backend :: <harp-back-end>, enumeration :: <integer>)
   => (register :: <real-register>);



/// Support for named variable tables:

define method table-of-named-variables 
    (back-end :: <harp-back-end>, register :: <real-register>, with-frame :: <boolean>)
    => (table :: <object-table>)
  // Use separate table for real-registers
  let tables :: <simple-object-vector> = back-end.variables.named-variables-tables;
  let index = 2;
  tables[index] | (tables[index] := make(<table>));
end method;

define method table-of-named-variables 
    (back-end :: <harp-back-end>, register, with-frame :: <boolean>)
    => (table :: <object-table>)
  // Use separate tables for with-frame & no-frame because
  // arg-spills have different offsets in either case
  let tables :: <simple-object-vector> = back-end.variables.named-variables-tables;
  let index = if (with-frame) 0 else 1 end;
  tables[index] | (tables[index] := make(<table>));
end method;

// Share lists of named-variables by using their original shared vectors
// as keys in a table

define method table-of-vectors-of-named-variables 
    (back-end :: <harp-back-end>, with-frame :: <boolean>)
    => (table :: <object-table>)
  // Use separate tables for with-frame & no-frame because
  // arg-spills have different offsets in either case
  let tables :: <simple-object-vector> = back-end.variables.named-variables-tables;
  let index = if (with-frame) 3 else 4 end;
  tables[index] | (tables[index] := make(<table>));
end method;


/// map-registers-as-variable attempts to use a shared list for regs-in-scope;
/// failing that, it reconstructs the list of named-variables

define method map-registers-as-variables
    (back-end :: <harp-back-end>, 
     with-frame :: <boolean>,
     regs-in-scope :: <simple-object-vector>) 
    => (vars-in-scope :: <list>)

  let table =
    table-of-vectors-of-named-variables(back-end, with-frame);
  let existing = element(table, regs-in-scope, default: #f);

  if (existing)
    existing
  else
    let vars-in-scope :: <list> =
      map-as(<list>, 
	     method (reg) 
	       map-register-as-variable(back-end, reg, with-frame) 
	     end,
	     regs-in-scope);
    table[regs-in-scope] := vars-in-scope;
    vars-in-scope
  end if;

end method; 


/// map-register-as-variable
/// maps between <virtual-register> and <named-variable> representations
/// of named variables. HARP uses <virtual-register> for register allocation 
/// initially - but the debug info uses the simpler representation of 
/// <named-variable>.

define method map-register-as-variable 
    (back-end :: <harp-back-end>, 
     reg :: <virtual-register>, 
     with-frame :: <boolean>) 
    => (var :: <named-variable>)
  let location = reg.virtual-register-colour;
  let table = table-of-named-variables(back-end, location, with-frame);
  let existing = element(table, reg, default: #f);
  if (existing)
    existing
  else
    let name = reg.virtual-register-name;
    let indirections = reg.virtual-register-named-indirections;
    let direct? = indirections.empty?;
    unless (name) 
      if (direct?)
	harp-warning(back-end, "Attempt to map an unnamed register as a variable");
	name := "unknown";
      else name := "_Environment_";
      end if;
    end unless;
    unless (location) 
      harp-warning(back-end, "Attempt to map an unallocated register as a variable");
    end unless;
    let new = if (direct?)
		make-named-variable(back-end, name, location, with-frame);
	      else 
		make-named-indirections(back-end, name, indirections, location, with-frame);
	      end if;
    table[reg] := new;
    new;
  end if;
end method; 



// <debug-scopes>
// Collection of debug-scopes is encoded as a (start-index, number-of-scopes)
// into a global vector of unique scopes; this can nearly always be encoded as
// an integer except if the index exceeds 2^17 and number exceeds 2^12

define class <debug-scopes-internal> (<object>)

  constant slot start-debug-scopes-internal :: <integer>,
    required-init-keyword: start:; 
    // The position in the code of the start of this scope

  constant slot size-debug-scopes-internal :: <integer>,
    required-init-keyword: size:; 
    // The position in the code of the end of this scope

end class;

define constant <debug-scopes> = type-union(<integer>, <debug-scopes-internal>);
define constant <debug-scopes-unpacked-slots> = <debug-scopes-internal>;
define constant debug-scopes-packed-slot = identity;


define packed-slots? (<debug-scopes>)
  constant start-debug-scopes   :: <integer>, init-keyword: start:,     width: 17;
  constant size-debug-scopes    :: <integer>, init-keyword: size:,      width: 12;
end;


/// <debug-scope> 
/// Defines a contiguous liveness scope for a set of <named-variable>s
/// Instances of <debug-scope> will be persistently stored in compiled lambdas.
/// NB, the externalized information for debug info for variables is stored in a 
/// scoped fashion because that's convenient for native debug formats. The scoping
/// is re-constituted here, though - and HARP clients are not required to provide
/// fully scoped variables, according to the source code. This permits any
/// possible compiler optimizations related to live range reduction, or elimination.


define abstract class <debug-scope-internal> (<object>)
  slot named-variables = #(),
    init-keyword: variables:;  
    // list of <named-variable>s for variables introduced in this scope

  slot nested-scopes :: type-union(<debug-scopes>, <list>) = #();
    // list of <debug-scope>s which are nested inside this one

  slot debug-scope-internal-packed-slot :: 
    type-union(<integer>, <debug-scope-internal-unpacked-slots>) = 0,
    init-keyword: slots:;

end class;

// Pack code start- and end- offsets as an integer if start and end are
// less than 2^14 or 2^15

define class <debug-scope-internal-unpacked-slots> (<object>)

  constant slot start-code-offset-internal :: <integer>,
    required-init-keyword: start:; 
    // The position in the code of the start of this scope

  slot end-code-offset-internal :: <integer>,
    required-init-keyword: end:; 
    // The position in the code of the end of this scope

end class;

define packed-slots? (<debug-scope-internal>)
  constant start-code-offset   :: <integer>, init-keyword: start:,     width: 14;
  end-code-offset     :: <integer>, init-keyword: end:,       width: 15;
end;

// Conditionally pack <debug-scope> in its entirety

// We can further pack debug-scope if the following conditions are met:
//   - 7 or fewer named-variables;
//   - first 16 encountered debug-scopes for this lambda;
//   - up to 3 immediately nested scopes;
//   - up to 2^7 start-code-offset; 2^8 end-code-offset

define constant <packed-debug-scope> = <integer>;
define constant <debug-scope> = type-union(<packed-debug-scope>, <debug-scope-internal>);

define constant <packed-debug-scope-unpacked-slots> = <debug-scope-internal>;
define constant packed-debug-scope-packed-slot = identity;


define packed-slots? (<packed-debug-scope>)
  constant debug-scope-with-frame?          :: <boolean>, init-keyword: frame?:;
  constant named-variables-in-scope         :: <integer>, init-keyword: variables:,         width: 7;
  constant start-nested-scopes              :: <integer>, init-keyword: start:,             width: 4;
  constant size-nested-scopes               :: <integer>, init-keyword: size:,              width: 2;
  constant begin-code-offset                :: <integer>, init-keyword: start-offset:,      width: 7;
  constant finish-code-offset               :: <integer>, init-keyword: end-offset:,        width: 8;
end;

define method debug-scope-with-frame?(debug-scope :: <debug-scope-internal>)
 => (with-frame? :: <boolean>)
  instance?(debug-scope, <debug-scope-with-frame>)
end method;

define method named-variables(debug-scope :: <integer>)
 => (scopes :: <debug-scopes>)
  debug-scope.named-variables-in-scope
end method;

define method nested-scopes(debug-scope :: <integer>)
 => (scopes :: <debug-scopes>)
  let start-scopes :: <integer> = start-nested-scopes(debug-scope);
  let size-scopes :: <integer> = size-nested-scopes(debug-scope);
  make-debug-scopes-packed-slot?(start-scopes, size-scopes);
end method;

define method start-code-offset(debug-scope :: <integer>) => (start-offset :: <integer>)
  begin-code-offset(debug-scope);
end method;

define method end-code-offset(debug-scope :: <integer>) => (end-offset :: <integer>)
  finish-code-offset(debug-scope);
end method;

define method pack-debug-scope
    (prototype :: <debug-scope-internal>,
     with-frame? :: <boolean>,
     named-variables :: <integer>,
     nested-scopes :: <integer>,
     code-offsets :: <integer>) 
    => (debug-scope :: <debug-scope>)
  
  let start-offset :: <integer> = start-code-offset-internal(code-offsets);
  let end-offset :: <integer> = end-code-offset-internal(code-offsets);
  let start-scopes :: <integer> = start-debug-scopes(nested-scopes);
  let size-scopes :: <integer> = size-debug-scopes(nested-scopes);

    make-packed-debug-scope-packed-slot?(with-frame?,
					 named-variables,
					 start-scopes, size-scopes,
					 start-offset, end-offset)
  | prototype;

end method;

define method pack-debug-scope
    (prototype :: <debug-scope-internal>,
     with-frame?,
     named-variables,
     nested-scopes,
     code-offsets) 
    => (debug-scope :: <debug-scope-internal>)
  
  prototype

end method;


// Walk debug-scopes for lambda after scopes generation has been completed
// and encode as follows:
//   - single global vector representations for named-variables and nested-scopes;
//   - integer encodings for named-variables and nested-scopes wrt global vectors;
//   - further pack debug-scopes that can be packed entirely as an integer

define method pack-debug-scopes(back-end :: <harp-back-end>, debug-scopes :: <list>)
 => (debug-scopes :: <debug-scopes>, all-debug-scopes :: <simple-object-vector>,
     all-debug-scopes :: <simple-object-vector>)
  /*
  format-out("\n### debug-scopes before packing\n");
  print-scopes(back-end, debug-scopes, *standard-output*, 2);
  */
  let all-debug-scopes :: <stretchy-vector> = make(<stretchy-vector>);
  let all-debug-names :: <table> = make(<table>);
  let variable-count :: <integer> = 0;

  local method pack-variables(set :: <vector-32bit>, variables :: <list>)
	 => (packed-value)
	  iterate process-variables
	    (bitset :: <vector-32bit> = set,
	     variables :: <list> = variables)
	    if (variables.empty?)
	      pack-bitset(bitset)
	    else
	    let variable :: <named-variable> = variables.head;
	    let variable-pos :: <integer> = element(all-debug-names, variable, default: -1);
	    let bitset :: <vector-32bit> =
	      if (variable-pos = -1)
		let count :: <integer> = variable-count;
		all-debug-names[variable] := count;
		variable-count := count + 1;
		set-bit-in-set!(bitset, count);
	      else
		set-bit-in-set!(bitset, variable-pos);
	      end if;
	    let variables :: <list> = variables.tail;
	    process-variables(bitset, variables);
	    end if;
	  end iterate;
	end method;
  local method pack-nested-debug-scopes(debug-scopes :: <list>)
	 => (debug-scopes :: <debug-scopes>)
	  let bitset :: <vector-32bit> = make(<vector-32bit>);
	  for (debug-scope :: <debug-scope-internal> in debug-scopes)
	    debug-scope.named-variables :=
	      pack-variables(bitset, debug-scope.named-variables);
	    debug-scope.nested-scopes :=
	      pack-nested-debug-scopes(debug-scope.nested-scopes);
	  end for;
	  let all-debug-scopes-size :: <integer> = all-debug-scopes.size;
	  let debug-scopes-size :: <integer> = 0;
	  for (debug-scope :: <debug-scope-internal> in debug-scopes,
	       count from 0)
	    let debug-scope :: <debug-scope> =
	      pack-debug-scope(debug-scope,
			       debug-scope.debug-scope-with-frame?,
			       debug-scope.named-variables,
			       debug-scope.nested-scopes,
			       debug-scope.debug-scope-internal-packed-slot);
	    add!(all-debug-scopes, debug-scope);
	  finally
	    debug-scopes-size := count
	  end for;
	  make-debug-scopes(all-debug-scopes-size, debug-scopes-size);
	end method;

  let debug-scopes :: <debug-scopes> = pack-nested-debug-scopes(debug-scopes);
  let all-debug-scopes :: <simple-object-vector> = as(<simple-object-vector>, all-debug-scopes);
  let debug-names = key-sequence(all-debug-names);
  let debug-names-vector :: <simple-object-vector> = make(<vector>, size: variable-count);
  for (name :: <named-variable> in debug-names)
    let pos :: <integer> = all-debug-names[name];
    debug-names-vector[pos] := name
  end for;
  /*
  format-out("\n### debug-scopes after packing %=\n", all-debug-scopes);
  print-scopes(back-end, debug-scopes, *standard-output*, 2,
	       all-scopes: all-debug-scopes, all-names: debug-names-vector);
  */

  values(debug-scopes,
	 all-debug-scopes,
	 debug-names-vector);
end method;

// Iterate over all debug-scopes in a specified range in a supplied vector 
// of all available scopes

define macro for-debug-scope
  { for-debug-scope (?debug-scope:name in ?debug-scopes:expression of ?all-debug-scopes:expression)
     ?:body ?other
    end }
    => { 
	let debug-scopes :: <debug-scopes> = ?debug-scopes;
	let ?=debug-scope-start :: <integer> = start-debug-scopes(debug-scopes);
	let ?=debug-scope-size :: <integer> = size-debug-scopes(debug-scopes);
	let scopes-vector :: <vector> = ?all-debug-scopes;

	for (?=debug-scope-index :: <integer> from ?=debug-scope-start below ?=debug-scope-start + ?=debug-scope-size)
	  let ?debug-scope :: <debug-scope> = scopes-vector[?=debug-scope-index];
          ?body
        finally
          ?other
	end for
       }

other:
    { }                   =>   { #f }
    { finally-do ?:body }    =>   { ?body }
end macro for-debug-scope;

// Reverse the natural order of iteration on scopes

define macro for-reversed-debug-scope
  { for-reversed-debug-scope (?debug-scope:name in ?debug-scopes:expression of ?all-debug-scopes:expression)
     ?:body ?other
    end }
    => { 
	let debug-scopes :: <debug-scopes> = ?debug-scopes;
	let ?=debug-scope-start :: <integer> = start-debug-scopes(debug-scopes);
	let ?=debug-scope-size :: <integer> = size-debug-scopes(debug-scopes);
	let scopes-vector :: <vector> = ?all-debug-scopes;

	for (?=debug-scope-index :: <integer> from ?=debug-scope-start + ?=debug-scope-size - 1 to ?=debug-scope-start by -1)
	  let ?debug-scope :: <debug-scope> = scopes-vector[?=debug-scope-index];
          ?body
        finally
          ?other
	end for
       }

other:
    { }                   =>   { #f }
    { finally-do ?:body }    =>   { ?body }
end macro for-reversed-debug-scope;

// Iteration for named-variables in specified range of all named-variables

define macro for-debug-var
  { for-debug-var (?debug-var:name in ?debug-vars:expression of ?all-debug-vars:expression)
     ?:body ?other
    end }
    => { 
	let vars :: <vector-32bit> = unpack-value-as-bitset(?debug-vars);
	let all-vars :: <simple-object-vector> = ?all-debug-vars;
	for (?debug-var :: <named-variable> in all-vars,
	     index :: <integer> from 0)
	  if (get-bit-from-set!(vars, index))
	    ?body
	  end if;
        finally
          ?other
	end for
       }

other:
    { }                   =>   { #f }
    { finally-do ?:body }    =>   { ?body }
end macro for-debug-var;


define inline method debug-scopes-as-vector
    (debug-scopes :: <debug-scopes>,
     all-scopes :: <simple-object-vector>)
 => (debug-scopes :: <simple-object-vector>)
  let start :: <integer> = start-debug-scopes(debug-scopes);
  let size :: <integer> = size-debug-scopes(debug-scopes);
  let scopes-vector :: <vector> = all-scopes;
  let vector :: <simple-object-vector> = make(<vector>, size: size);

  for (i :: <integer> from start below start + size)
    vector[i - start] := scopes-vector[i];
  end for;
  vector
end method;

define method debug-vars-as-list(vars :: <vector-32bit>, all-vars :: <simple-object-vector>) => (vars :: <list>)
  let all-vars-size :: <integer> = all-vars.size;
  iterate process-vars(result :: <list> = #(),
		       index :: <integer> = all-vars-size - 1)
    if (index = -1)
      result
    else
      let var = get-bit-from-set!(vars, index) & all-vars[index];
      if (var)
	process-vars(add(result, var), index - 1)
      else
	process-vars(result, index - 1)
      end if;
    end if;
  end iterate;
end method;

// Encodes a list of debug-scopes as a packed integer (start-scopes, size-scopes)
// of global vector of scopes

define inline method make-debug-scopes(start :: <integer>, size :: <integer>)
 => (debug-scopes :: <debug-scopes>)
  make-debug-scopes-packed-slot?(start, size)
  | (make(<debug-scopes-internal>, start: start, size: size));
end method;

define inline method make-zero-debug-scopes()
 => (debug-scopes :: <debug-scopes>)
  0
end method;

define inline method empty-debug-scopes?(debug-scopes :: <debug-scopes>)
 => (empty? :: <boolean>)
  size-debug-scopes(debug-scopes) = 0
end method;

define inline method empty-variables?(vars)
 => (empty? :: <boolean>)
  vars = 0
end method;

define method concatenate-variables(result :: <vector-32bit>, vars) => (vars :: <vector-32bit>)
  bit-set-or!(result, unpack-value-as-bitset(vars))
end method;

// <debug-scope-with-frame> is for non-leaf-case scopes

define class <debug-scope-with-frame> (<debug-scope-internal>)
end class;

// while <debug-scope-no-frame> is for leaf-case scopes

define class <debug-scope-no-frame> (<debug-scope-internal>)
end class;


define method make-debug-scope
    (with-frame :: <boolean>, vars :: <list>, 
     start :: <integer>, finish :: <integer>)
    => (new :: <debug-scope-internal>)

  let slots =
    make-debug-scope-internal-packed-slot?(start, finish)
    | (make(<debug-scope-internal-unpacked-slots>, start: start, end: finish));

  if (with-frame)
    make(<debug-scope-with-frame>, variables: vars, slots: slots);
  else
    make(<debug-scope-no-frame>, variables: vars, slots: slots);
  end if;

end method;


define method debug-scope-same-frame?
    (scope :: <debug-scope-with-frame>, with-frame? :: <boolean>)
    => (the-same :: <boolean>)
  with-frame?;
end method;

define method debug-scope-same-frame?
    (scope :: <debug-scope-no-frame>, with-frame? :: <boolean>)
    => (the-same :: <boolean>)
  ~ with-frame?;
end method;



/// add-debug-scope
///
/// Adds a new scope of live variables to an existing scope. The caller passes
/// in contiguous ranges along with the live named virtual registers for each 
/// range. The function calculates a nested scope representation from this data.
///
/// Callers of this are expected to incrementally add ranges in ascending order
/// of code location. The empty list should be used to represent an empty scope 
/// for the first call to this function. Adjacent ranges are assumed to be 
/// contiguous, in the sense that any variable which is live in two adjacent ranges
/// may be merged into a shared nested scope.

define method add-debug-scope
    (back-end :: <harp-back-end>, 
     debug-scopes :: <list>, 
     with-frame :: <boolean>,
     code-start :: <integer>,
     code-end :: <integer>,
     regs-in-scope :: <simple-object-vector>)
    => (new-scopes :: <list>)

  let vars-in-scope :: <list>
  = map-registers-as-variables(back-end, with-frame, regs-in-scope);

  add-debug-scope-internal
    (debug-scopes, with-frame, code-start, code-end, vars-in-scope);

end method;


define method add-debug-scope-internal
    (debug-scopes :: <list>, 
     with-frame :: <boolean>,
     code-start :: <integer>,
     code-end :: <integer>,
     vars-to-add :: <list>) 
    => (new-scopes :: <list>)
  let last-end = if (debug-scopes.empty?)
                   0;
                 else
                   debug-scopes.last.end-code-offset;
                 end if;
  append-to-debug-scopes
    (debug-scopes, with-frame, code-start, code-end, last-end, vars-to-add, #t);
end method;


// apppend-to-debug-scopes 
// Takes a list of scopes, and returns a new list which includes additional 
// range information. All consistency checks are performed

define method append-to-debug-scopes
    (debug-scopes :: <list>, 
     with-frame :: <boolean>,
     code-start :: <integer>,
     code-end :: <integer>,
     old-end :: <integer>,
     vars-to-add :: <list>,
     top-level :: <boolean>) 
    => (new-scopes :: <list>)

  if (debug-scopes.empty?)
    list(make-debug-scope(with-frame, vars-to-add, code-start, code-end));
  elseif (debug-scopes.size == 1 & debug-scopes[0].end-code-offset == old-end)
    merge-debug-scopes(debug-scopes[0], with-frame, 
                       code-start, code-end, old-end, 
                       vars-to-add, top-level);
  else
    debug-scopes.tail := append-to-debug-scopes(debug-scopes.tail, with-frame, 
                                                code-start, code-end, old-end, 
                                                vars-to-add, top-level);
    debug-scopes;
  end if;

end method;


// merge-debug-scopes
// Takes a single existing scope, and a new range and returns a new list of scopes
// which has merged scope data. The new list might contain either one or two elements.

define method merge-debug-scopes
    (debug-scope :: <debug-scope-internal>, 
     with-frame :: <boolean>,
     code-start :: <integer>,
     code-end :: <integer>,
     old-end :: <integer>,
     vars-to-add :: <list>,
     top-level :: <boolean>) 
    => (new-scopes :: <list>)

  local method make-disjoint-scopes ()
          list(debug-scope, 
               make-debug-scope(with-frame, vars-to-add, code-start, code-end));
        end method;

  if (debug-scope.end-code-offset == old-end 
      & debug-scope-same-frame?(debug-scope, with-frame))
    let existing-vars = debug-scope.named-variables;
    let (shared :: <list>, unshared?) =
      intersection-vars(existing-vars, vars-to-add);
    if (shared.empty? & ~ top-level)
      // If we have nothing in common, then make a disjoint scope - unless
      // this is tope level, in which case we want a common (empty)
      // parent
      make-disjoint-scopes();
    else

      // If we get here, then it is worth sharing some of the current debug scope.
      if ((shared == existing-vars) | (shared.size == existing-vars.size))
         // We need all the vars in existing-vars, so use the existing scope 
        
         // First increase it's code range
         debug-scope.end-code-offset := code-end;

         // Next, splice any new data into the nested scopes
         if ((existing-vars == vars-to-add) | (existing-vars.size = vars-to-add.size))
	 else
           // Don't do this if there are no new vars to add
           let uniques :: <list> =
	     if (unshared?)
	       unshared?
	     else
	       difference-vars(vars-to-add, existing-vars);
	     end;
           debug-scope.nested-scopes 
             := append-to-debug-scopes(debug-scope.nested-scopes, 
                                       with-frame, code-start, code-end, old-end, 
                                       uniques, #f);
         end if;

         // Finally, return a list containing just the updated scope
         list(debug-scope);
      else
         // We need only some of the existing-vars, so split the existing scope
         // into a nested scope whose parent has exact existing vars
         let unshared :: <list> =
	     if (unshared?)
	       unshared?
	     else
	       difference-vars(existing-vars, shared);
	     end;
         let old-start = debug-scope.start-code-offset;
         let sub-scope = make-debug-scope(with-frame, unshared, old-start, old-end);
         sub-scope.nested-scopes := debug-scope.nested-scopes;
         debug-scope.nested-scopes := list(sub-scope);
         debug-scope.named-variables := shared;
         // now try this process again
         merge-debug-scopes(debug-scope, with-frame, 
                            code-start, code-end, old-end, 
                            vars-to-add, top-level);
      end if;
    end if;
  else
    make-disjoint-scopes();
  end if;
end method;


/* Add specialized methods for named-variables lists

   This takes advantage of the following properties of named-variables:

   1. They usually all have a common prefix of named-variables, the params
      to the lambda

   2. A lot of these lists are shared

   3. There are no duplicates

   Nosa  Jan 25, 1999 */


// This also returns the difference between the two lists of variables
// when it is trivial to do so;
// Presently, in the compiler there is a 90% chance of this happening

define method intersection-vars
    (vars1 :: <list>, vars2 :: <list>)
 => (intersection :: <list>, difference? :: false-or(<list>))

  if (vars1 == vars2)
    values(vars1, #());
  else

  iterate intersection(l1 :: <list> = vars1, l2 :: <list> = vars2)
    if (l2.empty?)
      values(vars2, l1);
    elseif (l1.empty?)
      values(vars1, l2);
    else
      let v1 :: <named-variable> = l1.head;
      let v2 :: <named-variable> = l2.head;
      if (v1 == v2)
	let l1-tail :: <list> = l1.tail;
	let l2-tail :: <list> = l2.tail;
	intersection(l1-tail, l2-tail);
      else
	values(partial-intersection(vars1, l1, l2), #f);
      end;
    end;
  end;

  end if;

end method;

define method partial-intersection
    (vars :: <list>, vars1 :: <list>, vars2 :: <list>)
 => (intersection :: <list>)

  let result :: <list> = #();

  let result-tail :: <list> =

  // This is just copy-sequence; start is 0; end is where
  // vars1 begins

  iterate prolog (vars :: <list> = vars, l :: <list> = #())
    if (vars == vars1) l
    else
      let var :: <named-variable> = vars.head;
      let vars-tail :: <list> = vars.tail;
      let p :: <list> = pair(var, #());

      if (l.empty?) result := p
      else l.tail := p;
      end;
      prolog(vars-tail, p)
    end;
  end;

  // Now really do the intersection over the unprocessed portions
  // of the lists

  iterate intersection(l1 :: <list> = vars1, l :: <list> = #())
    if (l1.empty?)
    else
      let v :: <named-variable> = l1.head;
      if (member?(v, vars2))
	let p :: <list> = pair(v, #());
	if (l.empty?)
	  if (result-tail.empty?) result := result-tail := p;
	  else result-tail.tail := p;
	  end;
	else l.tail := p;
	end;
	let l1-tail :: <list> = l1.tail;
	intersection(l1-tail, p);
      else
	let l1-tail :: <list> = l1.tail;
	intersection(l1-tail, l);
      end;
    end;
  end;

  result
end method;

define method difference-vars
    (vars1 :: <list>, vars2 :: <list>) => (difference :: <list>)
  iterate difference(l1 :: <list> = vars1, l2 :: <list> = vars2)
    if (l2.empty?) l1
    else
      let v :: <named-variable> = l2.head;
      let l2-tail :: <list> = l2.tail;
      difference(remove-var(l1, v), l2-tail);
    end;
  end;
end method;

// This is just remove first occurrence, since there are no duplicates;
// Usually, the element to be removed is at the head of the list

define inline method remove-var
    (vars :: <list>, var :: <named-variable>)
 => (vars :: <list>)
  let result :: <list> = #();

  iterate remove (vars :: <list> = vars, l :: <list> = #())
    if (vars.empty?) result
    else
      let var2 :: <named-variable> = vars.head;
      let vars-tail :: <list> = vars.tail;
      if (var == var2)
	if (l.empty?)
	  vars-tail
	else
	  l.tail := vars-tail; result
	end;
      else
	let p :: <list> = pair(var2, #());
	if (l.empty?) result := p
	else l.tail := p;
	end;
	remove(vars-tail, p)
      end;
    end;
  end;
end method;



//// Some printing methods 


// print-debug-scopes
// This is an exported interface for printing a list of scopes.


define method print-debug-scopes 
    (be :: <harp-back-end>, scopes :: <list>, stream :: <stream>,
     #key all-scopes, all-names) => ()
  print-scopes(be, scopes, stream, 0);
end method;

define method print-debug-scopes 
    (be :: <harp-back-end>, scopes :: <debug-scopes>, stream :: <stream>,
     #key all-scopes, all-names) => ()
  print-scopes(be, scopes, stream, 0,
	       all-scopes: all-scopes,
	       all-names: all-names);
end method;

define method print-object (obj :: <debug-scope-with-frame>, stream :: <stream>) => ()
  format(stream, "{Scope %d-%d}", obj.start-code-offset, obj.end-code-offset);
end method;


define method print-object (obj :: <debug-scope-no-frame>, stream :: <stream>) => ()
  format(stream, "{Leaf Scope %d-%d}", obj.start-code-offset, obj.end-code-offset);
end method;


define method print-object (obj :: <variable-in-register>, stream :: <stream>) => ()
  format(stream, "%=/Reg%d", obj.variable-name, obj.variable-register-enumeration);
end method;


define method print-object (obj :: <variable-in-frame-spill>, stream :: <stream>) => ()
  let fp-offset = obj.variable-frame-pointer-offset;
  if (fp-offset < 0)
    format(stream, "%=/FP%d", obj.variable-name, fp-offset);
  else
    format(stream, "%=/FP+%d", obj.variable-name, fp-offset);
  end if;
end method;


define method print-object (obj :: <variable-in-leaf-spill>, stream :: <stream>) => ()
  let sp-offset = obj.variable-frame-pointer-offset;
  if (sp-offset < 0)
    format(stream, "%=/SP%d", obj.variable-name, sp-offset);
  else
    format(stream, "%=/SP+%d", obj.variable-name, sp-offset);
  end if;
end method;


define method print-object (obj :: <variable-indirections>, stream :: <stream>) => ()
  next-method();
  format(stream, "/%=", obj.variable-indirections);
end method;


define method print-object (obj :: <variable-indirection-offset>, stream :: <stream>) => ()
  format(stream, "[%d]", obj.variable-indirection-offset);
end method;


define method print-object
   (object :: <named-indirection>, stream :: <stream>) => ()
  format(stream, "[%s,%=]", 
	 object.variable-name, 
	 object.variable-indirection-offset);
end;

define method print-object
   (object :: <indirections-variable-in-indirection>, stream :: <stream>) => ()
  format(stream, "[%=,%=]", 
	 object.variable-indirections, 
	 object.variable-indirection-offset);
end;




define method stack-description 
  (scope :: <debug-scope>) => (r :: <byte-string>)
  if (scope.debug-scope-with-frame?)
    "with frame";
  else
    "no frame";
  end if;
end method;



define method print-scopes 
    (be :: <harp-back-end>, scopes :: <list>, 
     stream :: <stream>, indent :: <integer>,
     #key all-scopes, all-names)
    => ()
  for (scope in scopes)
    print-scope(be, scope, stream, indent);
  end for;
end method;

define method print-scopes 
    (be :: <harp-back-end>, scopes :: <debug-scopes>, 
     stream :: <stream>, indent :: <integer>,
     #key all-scopes, all-names)
    => ()
  for-debug-scope (scope in scopes of all-scopes)
    print-scope(be, scope, stream, indent,
		all-scopes: all-scopes,
		all-names: all-names);
  end for-debug-scope;
end method;


define method print-scope 
    (be :: <harp-back-end>, scope :: <debug-scope>, 
     stream :: <stream>, indent :: <integer>,
     #key all-scopes, all-names)
    => ()
  for (i from 0 below indent) format(stream, " ") end;
  format(stream, "Start scope at %d %s for ", 
         scope.start-code-offset, scope.stack-description);
  print-named-variables(be, scope.named-variables, stream,
			all-names: all-names);
  format(stream, "\n");
  print-scopes(be, scope.nested-scopes, stream, indent + 2,
	       all-scopes: all-scopes,
	       all-names: all-names);
  for (i from 0 below indent) format(stream, " ") end;
  format(stream, "End scope at %d\n", scope.end-code-offset);
end method;


define method print-named-variables 
    (be :: <harp-back-end>, vars :: <list>, stream :: <stream>,
     #key all-names) => ()
  if (vars.empty?)
    format(stream, "no variables");
  else
    for (var in vars)
      print-named-variable(be, var, stream);
    end for;
  end if;
end method;

define method print-named-variables 
    (be :: <harp-back-end>, vars, stream :: <stream>,
     #key all-names) => ()
  if (vars = 0 | vars = #[])
    format(stream, "no variables");
  else
    for-debug-var (var in vars of all-names)
      print-named-variable(be, var, stream);
    end for-debug-var;
  end if;
end method;


define method print-named-variable
    (be :: <harp-back-end>, var :: <local-variable>, stream :: <stream>)
    => ()
  format(stream, "%= ", var);
end method;


define method print-named-variable
    (be :: <harp-back-end>, var :: <variable-in-register>, stream :: <stream>)
    => ()
  format(stream, "%=/%= ", var.variable-name, 
         real-register-from-debug-info-enumeration
           (be, var.variable-register-enumeration));
end method;

define method print-named-variable
    (be :: <harp-back-end>, var :: <indirections-variable-in-register>, stream :: <stream>)
    => ()
  format(stream, "%=/%=/%= ", var.variable-name, 
         real-register-from-debug-info-enumeration
           (be, var.variable-register-enumeration),
	 var.variable-indirections);
end method;


/*

/// Debugging and testing for scope generation

define class <dummy-back-end> (<harp-back-end>) end;

define variable dummy-back-end = make(<dummy-back-end>);

define variable dummy-reg = make(<real-register>, pname: "dummy");

define method real-register-debug-info-enumeration 
    (backend :: <dummy-back-end>, register == dummy-reg)
    => (enumeration :: <integer>)  
  999;
end method;

define method real-register-from-debug-info-enumeration
    (backend :: <dummy-back-end>, enumeration == 999) 
    => (reg :: <real-register>)
  dummy-reg;
end method;


define variable v0 = make-named-variable(dummy-back-end, "Var0", dummy-reg, #t);
define variable v1 = make-named-variable(dummy-back-end, "Var1", dummy-reg, #t);
define variable v2 = make-named-variable(dummy-back-end, "Var2", dummy-reg, #t);
define variable v3 = make-named-variable(dummy-back-end, "Var3", dummy-reg, #t);
define variable v4 = make-named-variable(dummy-back-end, "Var4", dummy-reg, #t);
define variable v5 = make-named-variable(dummy-back-end, "Var5", dummy-reg, #t);
define variable v6 = make-named-variable(dummy-back-end, "Var6", dummy-reg, #t);
define variable v7 = make-named-variable(dummy-back-end, "Var7", dummy-reg, #t);
define variable v8 = make-named-variable(dummy-back-end, "Var8", dummy-reg, #t);
define variable v9 = make-named-variable(dummy-back-end, "Var9", dummy-reg, #t);

define variable test-scope = #();

define method add-test-scope 
  (from :: <integer>, to :: <integer>, stack? :: <boolean>, #rest vars)
  format(*standard-output*, "\n\nAdd %= from %d to %d\n", vars, from, to);
  let new-scope = add-debug-scope-internal(test-scope, stack?, from, to, vars);
  print-scopes(dummy-back-end, new-scope, *standard-output*, 2);
  test-scope := new-scope;
  values();
end method;



define method do-tests ()
  test-scope := #();
  add-test-scope(0, 10, #t, v0, v1);
  add-test-scope(11, 20, #t, v1);
  add-test-scope(21, 30, #t, v1, v2, v3, v4);
  add-test-scope(31, 40, #t, v1, v2, v3, v4, v5);
  add-test-scope(41, 50, #t, v1, v2, v3, v4, v6);
  add-test-scope(51, 60, #t, v1, v2, v3, v6);
  add-test-scope(61, 70, #t, v1, v2);
  add-test-scope(71, 80, #f, v1, v2);
  add-test-scope(81, 90, #f, v1);
  add-test-scope(91, 100, #f, v1, v2, v3, v4, v5, v6);
  add-test-scope(101, 110, #f, v1, v2, v3, v4, v7, v8, v9);
end method;

do-tests();


// do-tests() is expected to generate output ending in
//
//  Start scope at 0 with frame for "Var1"/dummy 
//    Start scope at 0 with frame for "Var0"/dummy 
//    End scope at 10
//    Start scope at 21 with frame for "Var2"/dummy 
//      Start scope at 21 with frame for "Var3"/dummy 
//        Start scope at 21 with frame for "Var4"/dummy 
//          Start scope at 31 with frame for "Var5"/dummy 
//          End scope at 40
//          Start scope at 41 with frame for "Var6"/dummy 
//          End scope at 50
//        End scope at 50
//        Start scope at 51 with frame for "Var6"/dummy 
//        End scope at 60
//      End scope at 60
//    End scope at 70
//  End scope at 70
//  Start scope at 71 no frame for "Var1"/dummy 
//    Start scope at 71 no frame for "Var2"/dummy 
//    End scope at 80
//    Start scope at 91 no frame for "Var4"/dummy "Var3"/dummy "Var2"/dummy 
//      Start scope at 91 no frame for "Var5"/dummy "Var6"/dummy 
//      End scope at 100
//      Start scope at 101 no frame for "Var7"/dummy "Var8"/dummy "Var9"/dummy 
//      End scope at 110
//    End scope at 110
//  End scope at 110


define method do-more-tests ()
  test-scope := #();
  add-test-scope(0, 10, #t, v0, v1);
  add-test-scope(11, 20, #t, v1);
  add-test-scope(21, 30, #t, v1, v2, v3, v4);
  add-test-scope(61, 70, #t, v1, v2);
  add-test-scope(71, 80, #t);
  add-test-scope(81, 90, #t, v1);
end method;

// do-more-tests is expected to generate output ending in 
//  Start scope at 0 with frame for no variables
//    Start scope at 0 with frame for "Var1"/dummy 
//      Start scope at 0 with frame for "Var0"/dummy 
//      End scope at 10
//      Start scope at 21 with frame for "Var2"/dummy 
//        Start scope at 21 with frame for "Var3"/dummy "Var4"/dummy 
//        End scope at 30
//      End scope at 70
//    End scope at 70
//    Start scope at 81 with frame for "Var1"/dummy 
//    End scope at 90
//  End scope at 90


*/
