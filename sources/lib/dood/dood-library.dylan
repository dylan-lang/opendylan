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
  use system;
  use collections;
  use io;
  use variable-search;
  use walker;
  export dood;
end library;

define module dood
  use functional-dylan;
  use dylan-extensions;
  use dylan-primitives;
  use dylan-incremental;
  use machine-word-lowlevel;
  use byte-vector;
  use threads;
  use file-system;
  use file-system-internals;
  use streams;
  use streams-internals;
  use standard-io;
  use print;
  use format;
  use format-out;
  use variable-search;
  use collectors;
  use walker;
  use locators;
  // use machine-words;
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
    dood-locator,
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
    dood-address, 
    dood-object;

  export
    dood-compute-instance-size,
    dood-compute-standard-instance-size;

  export
    dood-reinitialize,
    dood-flush-lazy-slots;

  export
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
      // dood-proxy-value,
    <dood-proxy-error>,
    <dood-program-module-proxy>,
    <dood-program-binding-proxy>,
      dood-make-program-binding-proxy,
    <dood-class-program-binding-proxy>,
      dood-make-class-program-binding-proxy,
    dood-as-proxy,
    dood-disk-object,
    dood-disk-object-default,
    dood-restore-proxy,
    <dood-cross-proxy>,
      dood-proxy-dood-name,
    dood-make-cross-proxy,
    <dood-address-proxy>,
      dood-force-address-proxy,
      dood-maybe-force-address-proxy,
    <dood-slot-value-proxy>,
      dood-force-slot-value-proxy,
      dood-maybe-force-slot-value-proxy,
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
    
  export
    make-dood-stream;

  /// HYGIENE

  export
    // dood-weak-reinitialize,
    dood-lazy-getters,
    dood-weak-getters,
    dood-lazy-slot-value,
    dood-disk-slot-value;

  /// INTERNALS FOR DEBUGGING 

  export
    dump;

  export
    dood-watchpoint-class, dood-watchpoint-class-setter,
    dood-watchpoint-dood,  dood-watchpoint-dood-setter;

  export
    *dood-debug?*,  
    *dood-dump?*,
    debug, no-debug;

  // These should be in some utilities library..
  export
    key-sequence-vector,
    symbol-less-than?;
end module;

// eof
