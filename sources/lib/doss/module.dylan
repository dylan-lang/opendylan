Module:    dylan-user
Synopsis:  Define the DOSS modules
Author:    Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/* Modules:

   Doss            - The client API.
   Doss-internals  - Union of the client and extender APIs.

*/

define module doss-internals
  use dylan; 
  use functional-dylan;
  use set;
  use byte-vector;
  use mop;
  use variable-search;
  use streams-internals;
  use internal,
    import: {allocate, <hashed-collection>, void-element, 
             rehash!, includes-key?};

  // For debugging:

  use simple-streams,
    import: { format, *standard-output* };

  export

    <doss-io-manager>,
      <doss-dumper>,
      <doss-loader>,
    <doss-policy>,
      <basic-doss-policy>,
    <loaded-object-proxy>,

    <end-of-doss-stream>,

    // External interface (doss module)
    fetch-object,
    fetch-next-object,
    store-object,

    put-reference,
    put-variable,
    put-class-description,
    put-apply,
    put-object,
    put-header,
    put-footer,

    policy,
    policy-setter,
    stream,
    stream-setter,

    post-load-cleanup,

    doss-dumpable-slots,
    has-repeated-slots?,
    number-of-repeated-slots,

    doss-slot-value,
    doss-repeated-slot-element,
    unbound-proxy,

    put-specially,

    // Internal interface (for use by emulator doss module) slots:
    $apply-code,
    $class-code,
    $double-code,
    $empty-list-code,
    $extended-code,
    $false-code,
    $float-code,
    $keyword-code,
    $object-code,
    $object-id-code,
    $pair-code,
    $repeat-code,
    $string-code,
    $symbol-code,
    $true-code,
    $unbound-code,
    $val-obj-id-code,
    $variable-code,
    $void-code,
    $footer-code,

    $character-start,
    $integer-start,

    class-slot-info,
    doss-version,
    doss-version-string,
    header-size,
    header-size-offset,
    next-id,
    object-ids,
    restored,
    repeat-count,
    repeated-object,

    // Internal interface (for use by emulator doss module) methods:
    check-and-skip-header,
    check-dump-object-id,
    check-dump-value-object-id,

    store-and-traverse,

    dump-header,
    dump-int,
    dump-object,
    dump-repeated-object,
    dump-string,
    dump-variable,

    get-anonymous-variable,
    get-apply,
    get-class-definition,
    get-int,
    get-keyword,
    get-next-int,
    get-next-object,
    get-object-definition,
    get-object-id,
    get-pair,
    get-string,
    get-symbol,
    get-value-object-definition,
    get-variable,

    all-dumpable-slot-descriptors,
    restore,
    do-setters,
    slot-info-for-class,
    resolve-slot-descriptor,
    repeated-slots-stored,
    start-repeat,
    end-repeat,

    locate-variable-via-policy,

    add-fixup!,
    fixups,
    loaded-object,

    my-format,
    *debug-stream*,
    *debug-print*;

end module doss-internals;

define module doss
  use dylan;
  use streams-internals;
  use doss-internals,
   export: {

    <doss-io-manager>,
      <doss-dumper>,
      <doss-loader>,
    <doss-policy>,
      <basic-doss-policy>,
    <loaded-object-proxy>,

    <end-of-doss-stream>,
    // <doss-loader> protocol

    fetch-object,            // loader::<doss-loader>
    fetch-next-object,       // loader::<doss-loader>

    // <doss-dumper> protocol
    store-object,             // obj, dumper::<doss-dumper>

      // special mechanisms
    put-reference,         // o, dd::<doss-dumper>
    put-variable,          // o, dd::<doss-dumper>,variable-name,module-name,library-name
    put-class-description, // c::<class>, dd::<doss-dumper>
    put-apply,             // o, dd::<doss-dumper>, f::<function>, #rest args
    put-object,            // o, dumper::<doss-dumper>
    put-header,            // dumper::<doss-dumper>
    put-footer,

    locate-variable-via-policy, // obj, policy::<doss-policy>

    // simple accessors
    policy,
    policy-setter,
    stream,
    stream-setter,

    // Interface for post-load fixup (e.g. table rehashing)
    post-load-cleanup,      // object

    // Interface for slot handling
    doss-dumpable-slots,        // c::<class>, dd::<doss-dumper>
                                //  => <sequence>
    has-repeated-slots?,        // c::<class>
                                //  => forty-two::<boolean>
    number-of-repeated-slots,   // o::<sequence>
                                //  => n::<integer>

    doss-slot-value,            // getter::<function>, o, dd::<doss-dumper>
                                // => val
    doss-repeated-slot-element, // o, i::<integer>, dd <doss-dumper>
    unbound-proxy,


    // <doss-policy> protocol
    put-specially,              // o, policy::<doss-policy>, dd::<doss-dumper>
                                //  => object-dumped?::<boolean>

    // loading utilities
    load-doss-stream,

    fixups,
    loaded-object,

    // bugs?
    // no way to specialise slot access on loading (slot setting)

    *debug-stream*,
    *debug-print*
   };

end module doss;
