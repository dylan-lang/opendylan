Module:       dylan-user
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dood
  use functional-dylan;
  use mop;
  use system;
  use variable-search;
  use collections;
  use walker;
  export dood;
end library;

// TODO: EMULATOR SPECIFIC

define module dood
  use functional-dylan;
  use dylan-extensions;
  use threads;
  use internal, 
    import: {reinitialize,
             unbound, 
             allocate, 
             table-table, 
             table-table-setter, 
             table-values};
  use mop;
  use file-system;
  use byte-vector;
  use streams;
  use streams-internals;
  use standard-io;
  use print;
  use format-out;
  use variable-search;
  use machine-words;
  use collectors;
  use walker;
  use locators;
  export
    <dood-disk-pointer>,
    <dood-disk-address>,
    $max-dood-integer,
    $min-dood-integer;
  export
    <dood-world>,
    dood-world-default, dood-world-default-setter,
    dood-world-reset,
    dood-world-find-dood,
    dood-number-of-buffers, dood-number-of-buffers-setter,
    dood-buffer-size, dood-buffer-size-setter;
  
  export
    <dood-segment>,
    <dood-typed-segment>,
    <dood-functional-segment>;
  
  export
    <dood>,
    dood-name,
    dood-size,
    dood-read-only?,
    dood-world,
    dood-free-address,
    dood-root, dood-root-setter;

  export
    <dood-opening-warning>,
      dood-failed-dood,
    <dood-version-warning>,
    <dood-user-version-warning>,
    <dood-corruption-warning>;

  export
    <dood-mapped-object>,
    <dood-mapped-and-owned-object>;

  export
    dood-close,
    dood-flush,
    dood-flush-from,
    dood-flush-from-if,
    dood-flush-all,
    // dood-reset-live-objects,
    object-dood, object-dood-setter,
    dood-address, dood-pointer,
    dood-object;

  export
    dood-compute-instance-size,
    dood-compute-standard-instance-size;

  export
    dood-reinitialize,
    dood-flush-lazy-slots;

  export
    dood-instance-statistics,
    dood-display-statistics,
    dood-merge-statistics,
    dood-diff-last-two-statistics,
    dood-statistics;

  export
    \with-walk-progress,
    dood-initialize-walker!,
    dood-reset-walker!,
    dood-walk-from,
    dood-walk;

  export
    // dood-commit-from,
    dood-commit;

  export
    dood-lazy-value?,
    <dood-proxy>,
      dood-proxy-value,
    <dood-proxy-error>,
    <dood-program-module-proxy>,
    <dood-program-binding-proxy>,
      dood-make-program-binding-proxy,
    dood-as-proxy,
    dood-disk-object,
    dood-disk-object-default,
    dood-restore-proxy,
    <dood-address-proxy>,
      dood-force-address-proxy,
      dood-maybe-force-address-proxy,
    <dood-slot-value-proxy>,
      dood-force-slot-value-proxy,
      dood-maybe-force-slot-value-proxy,
    <dood-cross-proxy>,
      dood-proxy-dood-name,
    dood-make-cross-proxy,
    <dood-wrapper-proxy>,
      dood-wrapper-proxy-object,
      dood-wrapper-proxy-object-address;

  export
    <dood-lazy-table>,
      dood-lazy-table-data,
      dood-lazy-table-source, dood-lazy-table-source-setter,
      dood-lazy-forward-iteration-protocol,
    <dood-first-lazy-table>,
    <dood-lazy-symbol-table>;

  export
    \dood-class-definer;
    
  /// HYGIENE

  export
    *walk-progress-function*,
    dood-weak-reinitialize,
    dood-lazy-getters,
    dood-weak-getters,
    dood-lazy-slot-value,
    dood-disk-slot-value,
    dood-disk-slot-value-setter;

  /// INTERNALS FOR DEBUGGING 

  export
    dump;

  export
    dood-watchpoint-class, dood-watchpoint-class-setter,
    dood-watchpoint-dood, dood-watchpoint-dood-setter;

  export
    *trace-allocation?*,
    *dood-debug?*,  
    *dood-dump?*,
    debug, no-debug;

end module;

// eof
