module:    dylan-user
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library walker
  use dylan;
  use collections;
  use io;
  use common-dylan, import: { common-extensions };
  export walker;
end library;

define module walker
  use dylan;
  use dylan-extensions;
  use common-extensions, import: { $unfound, found? };
  use format-out;
  use byte-vector;
  use collectors;
  export
    <slot-sequence-type>,
    <walker-slot-descriptor>,
    <walker-slot-sequence-type>,
    walker-slot-value,
    walker-slot-value-setter,
    as-walker-slot-descriptor;

  export
    <walker-defaulted-descriptor>,
    make-walker-defaulted-descriptor,
    walker-defaulted-getter?,
    walker-default-getter,
    walker-default-thunk,
    walker-default-slot-descriptor,
    <walker-defaulted-slot-sequence-type>;
    
  export
    walker-allocate-object,
    walker-allocate-simple-object,
    walker-allocate-repeated-object;
  export
    dont-walk-slots-definer,
    dont-walk-object-definer,
    <walker>,
    walker-walked,
    walker-reset,
    maybe-do-deep-walk,
    deep-walk;
  export // hygiene glitch
    walker-shallow-getters;
  export
    dont-copy-slots-definer,
    dont-copy-object-definer,
    <copier>,
    maybe-do-deep-copy,
    do-deep-copy,
    copier-register-copied,
    deep-copy;
  export
    shallow-copy-instance;
  export
    copier-reset;
  export
    walker-instance-statistics,
    walker-display-statistics,
    walker-merge-statistics,
    walker-diff-last-two-statistics,
    walker-stats;
end module;
