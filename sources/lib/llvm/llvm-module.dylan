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

  constant slot llvm-module-dependent-libraries :: <sequence>
    = make(<stretchy-object-vector>);

  constant slot llvm-type-table :: <mutable-explicit-key-collection>
    = make(<string-table>);
  constant slot llvm-global-table :: <mutable-explicit-key-collection>
    = make(<string-table>);
end class;

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
  let node = make(<llvm-metadata-node>,
                  function-local?: #f,
                  node-values: vector(i32(behavior-enum),
                                      name-metadata,
                                      value));
  add-to-named-metadata(module, "llvm.module.flags", node);
end method;
