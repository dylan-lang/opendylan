module:          dm-internals
synopsis:        Registration and deregistration of "things"
author:          Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// At the moment, stuff in this file just supports <debug-point> registration,
// but it might have more general use as the DM gets more fully implemented.

define abstract class <dm-registered-descriptor> (<object>)

       slot marked-for-removal :: <boolean>,
            init-value: #f;

end class;


// This function is really not necessary, but it provides neat symmetry.

define method dm-register   (x :: <dm-registered-descriptor>) => ()
  x.marked-for-removal := #f
end method;

define method dm-deregister (x :: <dm-registered-descriptor>) => ()
  x.marked-for-removal := #t
end method;

define method marked-for-deregistration? (x :: <dm-registered-descriptor>)
    => (b :: <boolean>)
  x.marked-for-removal
end method;


///// DM-TIDY-SEQUENCE
//    Given a sequence S of <dm-registered-descriptor> objects, this
//    returns S without those descriptors that have since been invalidated
//    with a call to dm-deregister. Since we're not allowed to remove
//    objects from a collection while iterating over it, this is achieved
//    by building a temporary sequence of the deregistered descriptors
//    only, and iterating over that temporary sequence to effect the
//    removal.

define method dm-tidy-sequence (s :: <sequence>) => (s :: <sequence>)
  let deletables = make (<stretchy-vector>, size: 0);
  for (thingy in s)
    if (thingy.marked-for-removal)
      deletables := add! (deletables, thingy)
    end if
  end for;
  for (dead-thingy in deletables)
    s := remove! (s, dead-thingy)
  end for;
  s;
end method;
