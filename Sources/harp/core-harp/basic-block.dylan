module:    harp-basic-block
Synopsis:  The definition of the <basic-block> class.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The initial value of the colour field, set by the front end


define class <basic-block> (<object>)

  //// where the instructions are
  slot bb-start :: <integer>, required-init-keyword: start:;

  slot bb-end :: <integer>, required-init-keyword: end:;

  //// slots for dataflow analysis and general flow of control information
  slot bb-taags :: <list>, init-value: #();
				// a list of tags attached to entry of this
				// block 
                                
  slot bb-fall-thru, init-value: #f;
				// the block which should follow this one in
				// linearised code sequence if possible
                                // shared - fixed-offset

  slot bb-next-set :: <list>, init-value: #();
				// a list of blocks control may pass to from
				// this block including the fall thru and any
				// active catchers

  slot bb-prev-set :: <list>, init-value: #();
  				// a list of blocks from which control may reach
				// this block. ie those blocks which have this
				// block in their next-set -- also used to hold
				// a list of sdis jumping to this block after
				// asm-code-select

  slot bb-other-set :: <list>, init-value: #();
				// we should think about trying to get rid of
				// this one

  //// live register info
  
  slot bb-defs, init-value: #f;
	         		// registers this block defines before
				// using
                                // (shared - seen, code-ptr)

  slot bb-last-entry :: <integer>, init-value: -1;
		   		// and how it looked last time
                                // (shared - preceding-sdis)

  slot bb-live-entry, init-value: #f;
		   		// registers live on entry to block
                                // (shared - branch-inf)

  slot bb-properties :: <integer> = 0;

  //// other stuff


  slot bb-copy-of, init-value: #f; 
                                // a <basic-block>;

end;

define method copy-bb (old :: <basic-block>) => (new :: <basic-block>)
  let new :: <basic-block> =
    make(<basic-block>,
	 start: old.bb-start,
	 end: old.bb-end);
  new.bb-colour := old.bb-colour;
  new.bb-taags := old.bb-taags;
  new.bb-fall-thru := old.bb-fall-thru;
  new.bb-next-set := old.bb-next-set;
  new.bb-prev-set := old.bb-prev-set;
  new.bb-other-set := old.bb-other-set;
  new.bb-defs := old.bb-defs;
  new.bb-last-entry := old.bb-last-entry;
  new.bb-live-entry := old.bb-live-entry;
  new.bb-stack-state := old.bb-stack-state;
  new.bb-loop-depth := old.bb-loop-depth;
  new.bb-copy-of := old.bb-copy-of;
  new;
end;



define constant $empty-basic-block = 
    make(<basic-block>, start: 0, end: 0);

define constant <simple-basic-block-vector> =
  limited(<simple-vector>, of: <basic-block>);

define constant <stretchy-basic-block-vector> =
  limited(<any-stretchy-vector>, of: <basic-block>);



// Packed slots of <basic-block>

define leaf packed-slots bb-properties (<basic-block>, <object>)

  field   slot bb-colour = 0, field-size: 3, init-keyword: colour:;
		                // either red, green, yellow or brown used
				// for leaf case optimization
                                // It is also used as a flag from the
				// compiler to the backend for blocks
				// that are before the argument moving 
  
  field   slot bb-stack-state = 0, field-size: 3;
		         	// the state of the stack when this block is
				// executed - can be one of before, with,
				// self, lf, constants and #F

  field   slot bb-loop-depth = 1, field-size: 4;
				// rough approximation of how many nested loops
				// currently within (used in ra)
                                // (shared - copy-of)

  boolean slot bb-needs-leaf = #f;
				// If true, then don't set stack-state to with.

end packed-slots;

/*  This allocates too many vectors and machine-words!

define method initialize (bb :: <basic-block>, #rest all-keys, #key colour = $no-colour)
 => (bb :: <basic-block>)
  next-method();
  apply(initialize-packed-slots, bb, colour: colour, all-keys);
  bb
end method;
*/

define constant $no-colour               = 0;
define constant $red-colour              = 1;
define constant $green-colour            = 2;
define constant $yellow-colour           = 3;
define constant $brown-colour            = 4;
define constant $before-move-home-colour = 5;


define constant $no-stack-state        = 0;
define constant $before-stack-state    = 1;
define constant $with-stack-state      = 2;
define constant $self-stack-state      = 3;
define constant $lf-stack-state        = 4;
define constant $constants-stack-state = 5;


define method print-bb-colour(bb :: <basic-block>) => (colour)
  select (bb.bb-colour by \=)
    $no-colour                => #f;
    $red-colour               => #"red";
    $green-colour             => #"green";
    $yellow-colour            => #"yellow";
    $brown-colour             => #"brown";
    $before-move-home-colour  => #"before-move-home";
  end;
end method;

define method print-bb-stack-state(bb :: <basic-block>) => (state)
  select (bb.bb-stack-state by \=)
    $no-stack-state         => #f;
    $before-stack-state     => #"before";
    $with-stack-state       => #"with";
    $self-stack-state       => #"self";
    $lf-stack-state         => #"lf";
    $constants-stack-state  => #"constants";
  end;
end method;

