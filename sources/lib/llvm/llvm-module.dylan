Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <llvm-module> (<object>)
  constant slot llvm-module-name :: <string>,
    required-init-keyword: name:;

  slot llvm-module-target-triple :: <string>,
    init-value: "", init-keyword: target-triple:;
  slot llvm-module-source-filename :: <string>,
    init-value: "", init-keyword: source-filename:;
  slot llvm-module-data-layout :: <string>,
    init-value: "", init-keyword: data-layout:;
  slot llvm-module-asm :: <string>,
    init-value: "";

  constant slot llvm-module-globals :: <sequence>
    = make(<stretchy-object-vector>);
  constant slot llvm-module-functions :: <sequence>
    = make(<stretchy-object-vector>);
  constant slot llvm-module-aliases :: <sequence>
    = make(<stretchy-object-vector>);
  constant slot llvm-module-named-metadata :: <sequence>
    = make(<stretchy-object-vector>);
  constant slot %named-metadata-table :: <mutable-explicit-key-collection>
    = make(<string-table>);

  constant slot %metadata-kind-table :: <mutable-explicit-key-collection>
    = make(<string-table>);

  constant slot llvm-type-table :: <mutable-explicit-key-collection>
    = make(<string-table>);
  constant slot llvm-global-table :: <mutable-explicit-key-collection>
    = make(<string-table>);
end class;

define sealed method initialize
    (module :: <llvm-module>, #key, #all-keys)
 => ();
  // Initialize the metadata kind mapping
  let t = module.%metadata-kind-table;
  t["dbg"] := $llvm-metadata-kind-dbg;
  t["tbaa"] := $llvm-metadata-kind-tbaa;
  t["prof"] := $llvm-metadata-kind-prof;
  t["fpmath"] := $llvm-metadata-kind-fpmath;
  t["range"] := $llvm-metadata-kind-range;
  t["tbaa.struct"] := $llvm-metadata-kind-tbaa-struct;
  t["invariant.load"] := $llvm-metadata-kind-invariant-load;
  t["alias.scope"] := $llvm-metadata-kind-alias-scope;
  t["noalias"] := $llvm-metadata-kind-noalias;
  t["nontemporal"] := $llvm-metadata-kind-nontemporal;
  t["llvm.mem.parallel_loop_access"]
    := $llvm-metadata-kind-mem-parallel-loop-access;
  t["nonnull"] := $llvm-metadata-kind-nonnull;
  t["dereferenceable"] := $llvm-metadata-kind-dereferenceable;
  t["dereferenceable_or_null"] := $llvm-metadata-kind-dereferenceable-or-null;
  t["make.implicit"] := $llvm-metadata-kind-make-implicit;
  t["unpredictable"] := $llvm-metadata-kind-unpredictable;
  t["invariant.group"] := $llvm-metadata-kind-invariant-group;
  t["align"] := $llvm-metadata-kind-align;
  t["llvm.loop"] := $llvm-metadata-kind-loop;
  t["type"] := $llvm-metadata-kind-type;
  t["section_prefix"] := $llvm-metadata-kind-section-prefix;
  t["absolute_symbol"] := $llvm-metadata-kind-absolute-symbol;
end method;

define method llvm-module-add-flag
    (module :: <llvm-module>, behavior :: <symbol>, name :: <string>,
     value :: <llvm-value>)
  => ();
  let name-metadata = make(<llvm-metadata-string>, string: name);
  let behavior-enum
    = select (behavior)
        #"error" => 1;
        #"warning" => 2;
        #"require" => 3;
        #"override" => 4;
        #"append" => 5;
        #"append-unique" => 6;
      end select;
  let behavior-metadata
    = make(<llvm-value-metadata>, value: i32(behavior-enum));
  let value-metadata
    = make(<llvm-value-metadata>, value: value);
  let node = make(<llvm-metadata-node>,
                  node-values: vector(behavior-metadata,
                                      name-metadata,
                                      value-metadata));
  add-to-named-metadata(module, "llvm.module.flags", node);
end method;
