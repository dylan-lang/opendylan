module:    main-harp
Synopsis:  General utilities for colouring support etc.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Pattern matching and reference predicates

//////
////// Copyright (c) Functional Objects, Inc. 1994
//////


////// These three are not used here, but in processor specific packages which
////// import HARP.


// Use explicit copy-down for efficiency

define method prefer (any :: <virtual-register>, 
                      real-list :: <list>)
  let pref-vect = any.virtual-register-colour-pref;
  for (real :: <real-register> in real-list)
    inc!(pref-vect[real.real-register-number], 4);
  end;
end;

define method prefer (any :: <virtual-register>,
                       real-list :: <simple-object-vector>)
  let pref-vect = any.virtual-register-colour-pref;
  for (real :: <real-register> in real-list)
    inc!(pref-vect[real.real-register-number], 4);
  end;
end;

define method prefer (any :: <virtual-register>, 
                      real-list :: <sequence>)
  let pref-vect = any.virtual-register-colour-pref;
  for (real :: <real-register> in real-list)
    inc!(pref-vect[real.real-register-number], 4);
  end;
end;

define method prefer (any :: <object>, real-list :: <sequence>)
end;


////// A couple of small but useful thingies for rsets of real registers

// NB. In the following functions, the name "list" is used for LW 
// compatibility, and does not imply the technical Dylan meaning of <list>.

// Use explicit copy-down for efficiency

define method rset-from-list (lst :: <sequence>) => (set :: <integer>)
  let rset = 0;
  for (item :: <real-register> in lst)
    rset := logior(rset, item.real-register-mask);
  end;
  rset;
end;

define method rset-from-list (lst :: <simple-object-vector>)
                             => (set :: <integer>)
  let rset = 0;
  for (item :: <real-register> in lst)
    rset := logior(rset, item.real-register-mask);
  end;
  rset;
end;

define method rset-from-list (lst :: <list>) => (set :: <integer>)
  let rset = 0;
  for (item :: <real-register> in lst)
    rset := logior(rset, item.real-register-mask);
  end;
  rset;
end;


define method rset-from-args (#rest args) => (set :: <integer>)
  rset-from-list(args);
end; 

define method list-from-register-vector
   (register-vector :: <vector>, rset :: <integer>) => (l :: <list>)
  let lst = #();
  for (reg :: <real-register> in register-vector)
    unless (logand(rset, reg.real-register-mask) = 0)
      push!(reg, lst);
    end unless;
  end for;
  lst;
end;

define method list-from-rset
   (backend :: <harp-back-end>, rset :: <integer>) => (l :: <list>)
  let reg-vec = backend.registers.real-register-vector;
  list-from-register-vector(reg-vec, rset);
end;



define method prset-from-list
     (backend :: <harp-back-end>, lst :: <sequence>) => (set :: <integer>)
  let rset = 0;
  let call-in = backend.variables.compiling-call-in;
  if (call-in)
    for (item :: <real-register> in lst)
      rset := logior(rset, item.real-register-c-preserved-mask) 
    end for;
  else
    for (item :: <real-register> in lst)
      rset := logior(rset, item.real-register-preserved-mask) 
    end for;
  end if;
  rset;
end;

define method prset-from-list
     (backend :: <harp-back-end>, lst :: <simple-object-vector>)
      => (set :: <integer>)
  let rset = 0;
  let call-in = backend.variables.compiling-call-in;
  if (call-in)
    for (item :: <real-register> in lst)
      rset := logior(rset, item.real-register-c-preserved-mask) 
    end for;
  else
    for (item :: <real-register> in lst)
      rset := logior(rset, item.real-register-preserved-mask) 
    end for;
  end if;
  rset;
end;


define method list-from-prset
   (backend :: <harp-back-end>, rset :: <integer>) => (l :: <list>)
  let lst = #();
  let call-in = backend.variables.compiling-call-in;
  let reg-vec = if (call-in)
                  backend.registers.c-preserved-register-vector;
                else backend.registers.preserved-register-vector;
                end if;
  for (reg :: <real-register> in reg-vec)
    let mask = if (call-in) 
                 reg.real-register-c-preserved-mask
               else reg.real-register-preserved-mask 
               end if;
    unless (logand(rset, mask) = 0)
      push!(reg, lst);
    end unless;
  end for;
  lst;
end;



////// These hacks just to make sure we get best code for set bashing.

define inline method r-set-diff (x :: <integer>, y :: <integer>) => (r :: <integer>)
  logand(x, lognot(y));
end;

define constant r-union = logior;


// Use explicit copy-down for efficiency

define method r-union-of-real-regs (regs :: <list>) => (r :: <integer>)
  let res :: <integer> = 0;
  for (reg :: <real-register> in regs)
    res := r-union(res, reg.real-register-mask);
  end for;
  res;
end;

define method r-union-of-real-regs (regs :: <simple-object-vector>)
                                   => (r :: <integer>)
  let res :: <integer> = 0;
  for (reg :: <real-register> in regs)
    res := r-union(res, reg.real-register-mask);
  end for;
  res;
end;

define method r-union-of-real-regs (regs :: <sequence>) => (r :: <integer>)
  let res :: <integer> = 0;
  for (reg :: <real-register> in regs)
    res := r-union(res, reg.real-register-mask);
  end for;
  res;
end;


define inline method r-membr (reg :: <real-register>, set :: <integer>) => (r :: <boolean>)
  ~ (logand(set, reg.real-register-mask) = 0);
end;


/// Particular backends may want to specialize these ...


define open generic signed-frame-pointer-offset
   (backend :: <harp-back-end>, spill :: <spill>) => (o :: <integer>);

define method signed-frame-pointer-offset
   (backend :: <harp-back-end>, spill :: <spill>) => (o :: <integer>)
  -4 * (1 + // NO for the code, one for the spill location itself
        spill.spill-offset);
end;

define method frame-pointer-offset
   (backend :: <harp-back-end>, spill :: <spill>) => (o :: <integer>)
  logand(#xffff, signed-frame-pointer-offset(backend, spill));
end;



//////

/***

define method single-bit? (x :: <integer>)
  member?(x, #[#x1, #x2, #x4, #x8, 
	       #x10, #x20, #x40, #x80, 
               #x100, #x200, #x400, #x800,
               #x1000, #x2000, #x4000, #x8000, 
               #x10000, #x20000, #x40000, #x80000, 
               #x100000, #x200000, #x400000, #x800000, 
               #x1000000, #x2000000, #x4000000, #x8000000, 
               #x10000000, #x20000000, #x40000000, #x80000000]);
end;


****/
