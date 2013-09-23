Module:   dfmc-back-end
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: utilities for labeling nodes in prepartion for emission
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// accessors

define method label? (c :: <computation>)
  c.%label
end method;

define method label (c :: <computation>)
  let label = c.%label;
  unless (label)
    error("%=: unlabeled state", c);
  end unless;
  label
end method;

// labeling

define class <labeling-state> (<object>)
  slot next-label, init-value: 0;
end class <labeling-state>;

define method new-label! (state :: <labeling-state>)
  block ()
    state.next-label
  afterwards
    state.next-label := state.next-label + 1
  end block;
end method;

define method maybe-label-with-state!
    (c :: <computation>, state :: <labeling-state>)
  c.%label | set-label!(c, state)
end method;

define method set-label! (c :: <computation>, state :: <labeling-state>)
  c.%label := new-label!(state)
end method;

define thread variable *init-labeling-state* = #f;

define macro with-labeling-from-dynamic
  { with-labeling-from-dynamic ?:body end }
  => { dynamic-bind (*init-labeling-state* = make(<labeling-state>))
         ?body
       end dynamic-bind }
end macro;

define method maybe-label! (c :: <computation>)
  maybe-label-with-state!(c, *init-labeling-state*)
end method;

// TODO: RIP OUT USES

define method label! (o :: <&iep>) => ()
end method label!;

define method label! (o :: <&lambda>) => ()
  label!(o.iep)
end method label!;
