module:    dylan-user
Synopsis:  The library definition for the Binary Builder
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library binary-builder
  use dylan;
  use common-dylan;
  use generic-arithmetic;
  use big-integers;
  use collections;
  use io;
  use system;
  use binary-manager;

  export binary-builder;
end library;


define module dylan-and-big-ints
  use dylan, export: all;
  use common-extensions, export: {unsupplied, supplied?, not-found, not-found?};
  use dylan-extensions, export: all;
  use big-integers, prefix: "generic-", export: all;
end module;

define module binary-builder
  use dylan-and-big-ints;
  use byte-vector;
  use table-extensions, import: {<string-table>};
  use format;
  use streams;
  use locators;
  use binary-manager;

  export 
    <section-with-fixups>,
    actual-section, import-fixups, id-import-fixups,
    <binary-builder>,
    current-section, current-section-setter,
    debug-section, debug-section-setter,
    optional-data-section, optional-data-section-setter,
    optional-vars-section, optional-vars-section-setter,
    optional-objs-section, optional-objs-section-setter,
    optional-untraced-objs-section, optional-untraced-objs-section-setter,
    optional-untraced-data-section, optional-untraced-data-section-setter,
    code-section, code-section-setter,
    init-code-section, init-code-section-setter,
    source-file, source-file-setter,
    current-fixups,
    id-current-fixups,
    fixups-element, fixups-element-setter,
    current-position, current-position-setter,
    dynamic-linking?, dynamic-linking?-setter,
    binary-file,
    destination,
    def-file,
    make-binary-builder,
    make-binary-section,
    add-symbol-export,
    add-symbol-def,
    add-symbol-definition,
    add-binary-symbol-definition,
    add-data, 
    add-integer-data, 
    add-integer-data-short, 
    add-data-byte, 
    add-data-string,
    add-fixup-data,
    select-binary-section,
    select-dylan-section,
    ensure-size-of-section-data,
    share-or-create,
    
    $data-start-symbol,
    $data-end-symbol,
    $vars-start-symbol,
    $vars-end-symbol,
    $objs-start-symbol,
    $objs-end-symbol,
    $fixup-start-symbol,
    $fixup-end-symbol,
    $import-start-symbol,
    $import-end-symbol,
    $data-start-section,
    $data-end-section,
    $vars-section,
    $vars-start-section,
    $vars-end-section,
    $objs-start-section,
    $objs-end-section,
    $fixup-start-section,
    $fixup-end-section,
    $import-start-section,
    $import-end-section,
    and-force-dll-exports?,
    and-emit-dll?,
    *dll-support*,
    $fixup-section,
    $import-section,
    $obj-file-start-data-symbol,
    $obj-file-start-vars-symbol,
    $obj-file-start-objs-symbol,
    $obj-file-start-untraced-objs-symbol,
    $obj-file-start-untraced-data-symbol,
    $code-section, $init-code-section,
    $directives-section,
    directives-section,

    directives-flags, data-flags, dylan-data-flags,
    code-flags, init-code-flags,
    init-flags, fixup-flags,

    add-word-to-section,
    add-short-to-section,
    add-byte-to-section,
    add-string-to-section,
    fill-section-data,

    add-imported-data,
    add-imported-data-fixups,
    $imported-name-mangler,
    align-section-data
    
    ;

end module;


