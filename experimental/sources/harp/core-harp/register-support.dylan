module:    base-harp
Synopsis:  Support for creating and destroying virtual registers.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Support for virtual registers
//
// Ordering problems mean that this cannot be in the same file 
// as the class definitions.

define method really-new-greg 
    (state :: <vreg-state>, name, indirections) => (new :: <greg>)
  let reg = make(<greg>, id: inc!(state.next-vreg-id), 
		 name: name, indirections: indirections);
  reg.virtual-register-vreg-state := state;
  add!(state.vr-vect, reg);
  inc!(state.fresh-vreg-count);
  reg;
end;

define method really-new-nreg
    (state :: <vreg-state>, name, indirections) => (new :: <nreg>)
  let reg = make(<nreg>, id: inc!(state.next-vreg-id), 
		 name: name, indirections: indirections);
  reg.virtual-register-vreg-state := state;
  add!(state.vr-vect, reg);
  inc!(state.fresh-vreg-count);
  reg;
end;


// ensure that all registers with indirections are also named,
// so that they are properly recorded in SCL instructions:-

define method ensure-name (name, indirections) => (name)
  if (name | indirections.empty?)
    name;
  else "_Environment_"; // a dummy name which might be useful
  end if;
end method;

define method make-sf-register
     (backend :: <harp-back-end>, #key name, indirections = #[]) 
     => (new :: <sfreg>)
  let state = backend.variables.vreg-state;
  let name = ensure-name(name, indirections);
  let reg = make(<sfreg>, id: inc!(state.next-vreg-id), 
		 name: name, indirections: indirections);
  reg.virtual-register-vreg-state := state;
  add!(state.vr-vect, reg);
  reg;
end;

define method make-df-register 
    (backend :: <harp-back-end>, #key name, indirections = #[])
    => (new :: <dfreg>)
  let state = backend.variables.vreg-state;
  let name = ensure-name(name, indirections);
  let reg = make(<dfreg>, id: inc!(state.next-vreg-id), 
		 name: name, indirections: indirections);
  reg.virtual-register-vreg-state := state;
  add!(state.vr-vect, reg);
  reg;
end;

define method make-g-register 
    (backend :: <harp-back-end>, #key name, indirections = #[]) 
    => (new :: <greg>)
  let state = backend.variables.vreg-state;
  let name = ensure-name(name, indirections);
  if (state.dont-reuse-vregs | state.gregs-to-reuse.empty? | name)
    really-new-greg(state, name, indirections);
  else
    inc!(state.reused-vreg-count);
    stretchy-vector-pop!(state.gregs-to-reuse);
  end
end;
     
define method make-n-register 
    (backend :: <harp-back-end>, #key name, indirections = #[]) 
    => (new :: <nreg>)
  let state = backend.variables.vreg-state;
  let name = ensure-name(name, indirections);
  if (state.dont-reuse-vregs | state.nregs-to-reuse.empty? | name)
    really-new-nreg(state, name, indirections);
  else
    inc!(state.reused-vreg-count);
    stretchy-vector-pop!(state.nregs-to-reuse);
  end
end;

define open generic make-temp-register
    (be :: <harp-back-end>, n :: <integer>);

define method make-temp-register
    (be :: <harp-back-end>, n :: <integer>)
  make-n-register(be);
end method;


// MOVE-REG
// Methods will be created for this when instructions sets are defined
//
define open generic move-reg 
    (backend :: <harp-back-end>, toreg :: <register>, fromreg :: <register>)
    => ();


// perhaps the following should be done with singleton methods ??

define generic make-register
   (backend :: <harp-back-end>, #key reg-class, name, indirections)
   => (new :: <virtual-register>);

define method make-register
   (backend :: <harp-back-end>, 
    #key reg-class :: <class> = <greg>, name = #f, indirections = #[])
   => (new :: <virtual-register>)
  select (reg-class)
    <greg> => make-g-register(backend, name: name, indirections: indirections);
    <nreg> => make-n-register(backend, name: name, indirections: indirections);
    <sfreg> => make-sf-register(backend, name: name, indirections: indirections);
    <dfreg> => make-df-register(backend, name: name, indirections: indirections);
    otherwise => error("Invalid register class %=.", reg-class)
  end select;
end;


define open generic mark-vreg-no-reusage (reg) => ();

define method mark-vreg-no-reusage (reg :: <virtual-register>) => ()
  reg.virtual-register-spill-set := $spill-set-no-reusage;
end;

define method mark-vreg-no-reusage (reg :: <object>) => ()
end;


define open generic note-vreg-dead (state :: <vreg-state>, reg) => ();

define method note-vreg-dead (state :: <vreg-state>, vreg :: <nreg>) => ()
  unless (state.dont-reuse-vregs | vreg.virtual-register-spill-set == $spill-set-no-reusage)
    add!(state.nregs-to-reuse, vreg);
  end;
end;

define method note-vreg-dead (state :: <vreg-state>, vreg :: <greg>) => ()
  unless (state.dont-reuse-vregs | vreg.virtual-register-spill-set == $spill-set-no-reusage)
    add!(state.gregs-to-reuse, vreg);
  end;
end;

define method note-vreg-dead (state :: <vreg-state>, vreg :: <object>) => ()
end;

define method note-vrvect-truncated
   (state :: <vreg-state>, new-end :: <integer>)  => ()
  unless (state.dont-reuse-vregs)
    size(state.nregs-to-reuse) := 0;
    size(state.gregs-to-reuse) := 0;
  end;
  size(state.vr-vect) := new-end;
  state.next-vreg-id := new-end - 1;
end;


define generic colour (register :: <object>) => (colour :: <object>);

define method colour (x :: <virtual-register>) => (colour)
  x.virtual-register-colour;
end;

define method colour (x :: <object>) => (x)
  x;
end;

// arg-spill-location returns the integer offset of an arg-spill, or #F
// otherwise. It may be a useful utility for the generation of tail-calls.

define open generic arg-spill-location 
    (register :: <object>) => (colour :: <object>);

define method arg-spill-location (x :: <virtual-register>) => (loc)
  let colour = x.virtual-register-colour;
  if (instance?(colour, <integer>))
    colour;
  else
    #f;
  end if;
end;

define method arg-spill-location (x :: <object>) => (false)
  #f;
end;


// There is a global bit-set that represents the commutative
// relationship between all virtual-registers in the lambda.
// Each virtual-register entry contains (n - i) entries (one for
// each younger virtual-register) where n = no-of-virtuals,
// i = virtual-register-id. For every pair of virtual-registers,
// there is only one bit entry. This enables tighter bit-packing
// and reduces space by a factor of 3 for 128 virtual-registers,
// 
//   nwords = 128!/32 + 2 = 128.129/64 = 260 words
// 
// compared to:
// 
//   nwords = 128.(4 + 2) = 768 words
// 
// 
// Saves space by a factor approaching 2 at infinity.
// 
//    Nosa  Jan 25, 1999
// 

/*
define method virtual-registers-clash?
    (vars :: <harp-variables>,
     v1 :: <virtual-register>, v2 :: <virtual-register>)
  => (clash? :: <boolean>)
  let v1-id :: <integer> = v1.virtual-register-id;
  let v2-id :: <integer> = v2.virtual-register-id;
  let (v1 :: <virtual-register>, v2-id :: <integer>) =
    if (v1-id <= v2-id)
      values(v1, v2-id);
    else
      values(v2, v1-id);
    end;
  get-bit-from-set(vars.virtual-register-clashes,
		   v1.virtual-register-clash-start + v2-id);
end method;
*/

define inline method virtual-registers-clash
    (vars :: <harp-variables>,
     v1 :: <virtual-register>, v1-id :: <integer>,
     v2 :: <virtual-register>, v2-id :: <integer>)
  => ()
  let (v1 :: <virtual-register>, v2-id :: <integer>) =
    if (v1-id <= v2-id)
      values(v1, v2-id);
    else
      values(v2, v1-id);
    end;
  set-bit-in-set(vars.virtual-register-clashes,
		 v1.virtual-register-clash-start + v2-id);
  /*
  format-out("\n### virtual-registers-clash: %=, %= at %=\n",
	     v1.virtual-register-id, v2-id, v1.virtual-register-clash-start + v2-id);
  format-out("\n### clashes  %=\n",
	     reduce
	       (method (string :: <string>, word :: <machine-word>)
		  concatenate(string, ".",
			      machine-word-to-string(word));
		end,
		"",
		vars.virtual-register-clashes));
  */
end method;

define macro do-virtual-register-clashes
  { do-virtual-register-clashes(?v1:name, ?v2:name in ?vars:expression)
      ?:body
    end }
    => {
	let vars = ?vars;
	let clashes = vars.virtual-register-clashes;
	let vr-vect = vars.vreg-state.vr-vect;
	let v1-id :: <integer> = ?v1.virtual-register-id;

	for (i :: <integer> from 0 below v1-id)
	  let ?v2 :: <virtual-register> = element-no-bounds-check(vr-vect, i);
	  let v2-start :: <integer> = ?v2.virtual-register-clash-start;
	  let ?=$num$ :: <integer> = v2-start + v1-id;
	  if (get-bit-from-set(clashes, ?=$num$))
	    ?body
	  end;
	end;

	let vr-size = vr-vect.size;
	let v1-start :: <integer> = ?v1.virtual-register-clash-start;

	for (i :: <integer> from v1-id below vr-size)
	  let ?=$num$ :: <integer> = v1-start + i;
	  if (get-bit-from-set(clashes, ?=$num$))
	    let ?v2 :: <virtual-register> = element-no-bounds-check(vr-vect, i);
	    ?body
	  end;
	end;

	}

end macro;

/* The number of bits required to encode the commutative relationship
   between n elements is

   n + (n - 1) + (n - 2) + ... + 1  =  n * (n + 1) / 2

   Nosa  Jan 25, 1999 */

define method factorial-add(n :: <integer>)
  => (result :: <integer>)
  truncate/(n * (n + 1), 2)
end method;
