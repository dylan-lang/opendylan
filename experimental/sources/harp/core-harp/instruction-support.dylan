module:    base-harp
Synopsis:  Infrastructure for inheritance of instruction sets.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Support for instruction sets
//
// Each backend is expected to define its own instruction set class.
// 
// A protocol is provided to inherit properties of instructions from
// instruction superclasses. In order to register with this protocol,
// a backend should define a method on parent-instruction-set which 
// returns the instruction set that inheritance should come from.
//
// When an instance is created, all the inherited instruction slots are 
// initialized with copies of the instructions in the parent instruction set.
//
// It is up to the backend to default any slots which are defined locally 
// for the backend.
//
// If an instruction set class is desined for later subclassing, then 
// a method should be defined on initialize-instruction-set-defaults. The
// first argument will be an instance of a subclass which needs defaulting
// from the local slots of the second argument. This method must invoke the 
// next method.
//
// The macro define instructions does everything that is necessary to fit 
// in with all of these protocols.

// The common ancestor just is
define open primary class <abstract-instruction-set> (<object>)
end;

// The initialize method arranges for all inherited slots to be 
// copied from the default for the superclass.
// Subclasses may define their own arbitrary methods on initialize if
// they wish - but it should not be necessary for the protocol.

define method initialize 
    (set :: <abstract-instruction-set>, #key) 
    => (new :: <abstract-instruction-set>)
  next-method();
  let default = parent-instruction-set(set);
  initialize-instruction-set-defaults(set, default);
  set;
end;




define open generic make-instruction-set (class :: <class>)
  => (instruction-set :: <abstract-instruction-set>);


define open generic parent-instruction-set 
    (set :: <abstract-instruction-set>)
 => (parent :: <abstract-instruction-set>);

define method parent-instruction-set 
    (set :: <abstract-instruction-set>)
 => (parent :: <abstract-instruction-set>)
  set;
end;

define open generic initialize-instruction-set-defaults
   (child :: <abstract-instruction-set>, default :: <abstract-instruction-set>)
   => ();

define method initialize-instruction-set-defaults
   (child :: <abstract-instruction-set>, default :: <abstract-instruction-set>)
   => ()
end;

define constant default-abstract-instruction-set = 
  make(<abstract-instruction-set>);

