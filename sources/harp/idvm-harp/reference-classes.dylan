module:    idvm-harp
Synopsis:  Class definitions for IDVM references
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Core HARP provides the <constant-reference> class. But we need more
/// for IDVM, because we refer to variables via accessor functions too.
/// We also have to ensure a complete graph of circularities, so that 
/// DOSS can be made to resolve them correctly. For this reason, the 
/// reference objects are either shared with the definition objects when
/// possible (local & public constants) or have back-pointers to definition
/// objects (variable readers and writers)
///
/// We distinguish between the following types of definitions & references:
///
/// 	type of definition		class
///
///     variable			<variable-definer>
///     constant (public to file)	<constant-definer>
///     constant (local to file)	<local-constant-reference>
///
/// 	type of reference		class
///
///	Constant (external to file)	<constant-reference> (absolute variety)
///	Constant (internal to file)	<local-constant-reference>
///	Read of a variable		<variable-reader>
///	Write to a variable		<variable-writer>


/// Support for variable references:


define abstract class <defined-constant-reference> (<i-address-constant-reference>)
  // DEFINER slot is a  <definer> if the constant was defined in this file,
  // otherwise it is #f if the constant was defined elsewhere
  slot constant-reference-definer,          
    init-value: #f,
    init-keyword: definer:;
end class;

define class <variable-reader> (<defined-constant-reference>)
end class;

define class <variable-writer> (<defined-constant-reference>)
end class;


define method make-variable-reader (reference, definer)
  make(<variable-reader>, refers-to: reference, definer: definer);
end method;

define instruction-function variable-reader
       (backend :: <idvm-back-end>, reference, #key definer) 
    => (ref :: <variable-reader>)
  make-variable-reader(reference, definer);
end;

define method make-variable-writer (reference, definer)
  make(<variable-writer>, refers-to: reference, definer: definer);
end method;

define instruction-function variable-writer
       (backend :: <idvm-back-end>, reference, #key definer) 
    => (ref :: <variable-writer>)
  make-variable-writer(reference, definer);
end;


/// support for constant references which define values, whether or
/// not the values are associated with names

define abstract class <value-constant-reference> (<i-address-constant-reference>)
  slot constant-reference-value, init-keyword: value:;
end class;



/// Support for local constant references


define class <local-constant-reference> (<value-constant-reference>)
end class;

define instruction-function local-constant-ref
       (backend :: <idvm-back-end>, #key name = #"local-ref", value)
    => (ref :: <local-constant-reference>)
  make(<local-constant-reference>, refers-to: name, value: value);
end;


/// Support for definitions:
///
/// This is a means of associating names with values in the DOSS tables


// The abstract class <definition-ref> is inherited by all classes which 
// introduce names into the DOSS naming tables. There are 3 DOSS tables
// specifically used by IDVM: (global, variable-reader and variable-writer). 
// There are only 2 definition classes, though, because variable readers 
// and writers are always created as pairs.

define abstract class <definition-ref> (<value-constant-reference>)
end class;



/// Support for variable definition

define class <variable-definition> (<definition-ref>)
  slot variable-reader :: <variable-reader>;
  slot variable-writer :: <variable-writer>;
end class;


define instruction-function variable-definer
   (backend :: <idvm-back-end>, name) => (ref :: <variable-definition>)
  let ref = make(<variable-definition>, refers-to: name);
  ref.variable-reader := make-variable-reader(name, ref);
  ref.variable-writer := make-variable-writer(name, ref);
  ref;
end;



/// Support for constant definition



define class <constant-definition> (<definition-ref>)
end class;


define instruction-function constant-definer
   (backend :: <idvm-back-end>, name) => (ref :: <constant-definition>)
  make(<constant-definition>, refers-to: name);
end;


