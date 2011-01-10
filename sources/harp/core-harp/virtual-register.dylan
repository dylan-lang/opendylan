module:    harp-registers
Synopsis:  The <virtual-register> class.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// the common ancestor just is
define abstract open class <register> (<object>)
end;        


define abstract primary open class <virtual-register> (<register>)
  slot virtual-register-id :: <integer>, 
    required-init-keyword: id:;      
                                // to allow us  to uniquely print them

  slot virtual-register-name = #f,
    init-keyword: name:;      

  slot virtual-register-named-indirections :: <simple-object-vector> = #[],
    init-keyword: indirections:;      
                                // for source level debugging
  slot virtual-register-colour = #f;
                                // the colouring - real register or spill
  slot virtual-register-available-colours :: <integer> = 0; 
                                // set of all allocatable registers less
			        // those disallowed by context, type or
		 		// colours of neighbours
  slot virtual-register-clash-start :: <integer> = -1;
                                // There is a global commutative clash set.
                                // This points to the start of this register's
                                // entries for itself and all younger registers
  slot virtual-register-colour-pref :: <simple-integer-vector> = #[];
                                // to be filled in ..., 
  slot virtual-register-spill-set :: <spill-set>;
                                // The set of available spill locations.
                                // The compiler uses this for chaining os 
                                //  reusable registers Y 10/4/93

  slot virtual-register-clash-count :: <integer> = 0;
                                // somewhere to put the clash count
                                // for sorting
  slot virtual-register-virtual-pref :: <list> = #();
		                // a list of virtuals for preferencing
  slot virtual-register-vreg-state :: <vreg-state>,
    init-keyword: vreg-state:;
                                // may be used for a consistency check
end;


define class <integer-virtual-register> (<virtual-register>)
end;

define class <floating-virtual-register> (<virtual-register>)
end;

define class <greg> (<integer-virtual-register>)
end;

define class <nreg> (<integer-virtual-register>)
end;

define class <sfreg> (<floating-virtual-register>)
end;

define class <dfreg> (<floating-virtual-register>)
end;

/// The structure we use to represent the used spill location set.


define class <spill-set> (<object>)
  slot spill-set-limit :: <integer>, init-keyword: limit:;
        // the lowest available element in contiguous set
  slot spill-set-scatter :: <list>, init-value: #(), init-keyword: scatter:;
        // the scattering of elements less than that
  slot spill-set-central :: <central-spill>, required-init-keyword: central:;
       // the central dispensing structure
end;



define method make-spill-set (central :: <central-spill>)
 => (spill-set :: <spill-set>)
  make(<spill-set>, limit: size(central.central-spill-holder),
                    central: central);
end;


define class <central-spill> (<object>)
  slot central-spill-holder :: <stretchy-vector>, init-keyword: holder:;
  slot central-spill-maker :: <function>, init-keyword: maker:;
        // the thing to make spills of this type
  slot central-spill-lower-limit :: <integer>, 
        init-value: 0, init-keyword: lower-limit:;
end;


define method initialize
   (obj :: <central-spill>, #key) => (new :: <central-spill>)
  next-method();
  obj.central-spill-holder := make(<stretchy-vector>);
  obj;
end;

define constant $spill-set-no-reusage =
  make-spill-set(make(<central-spill>));

define constant $invalid-virtual-register =
  make(<greg>, id: 0, name: #f, indirections: #[]);

define inline function invalid-virtual-register?
    (vr) => (invalid? :: <boolean>)
  vr == $invalid-virtual-register
end;

define inline function invalid-virtual-register
    () => (reg)
  $invalid-virtual-register
end;

define constant <simple-virtual-register-vector> =
  limited(<simple-vector>, of: <virtual-register>);

define constant <stretchy-virtual-register-vector> =
  limited(<any-stretchy-vector>, of: <virtual-register>);

define constant $evrv :: <simple-virtual-register-vector> =
  make(<simple-virtual-register-vector>, size: 0);

