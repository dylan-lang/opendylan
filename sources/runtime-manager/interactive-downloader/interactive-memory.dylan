module:       interactive-downloader-internals
synopsis:     Abstract descriptions of regions of interactive memory.
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <INTERACTIVE-REGION>
//    Describes a segment of interactive memory that is all used for one
//    purpose, but potentially made up of may low-level <static-block>s.

define sealed class <interactive-region> (<object>)

  // This is the <static-block> currently being used for
  // downloading in this region. 

  slot region-current-static-block :: false-or(<static-block>),
    init-value: #f;

  // The region also maintains a list of static blocks that have
  // been allocated, but also filled. As soon as there is no
  // room in a block for a segment, it gets tacked onto the
  // end of this vector. However, there may be an opportunity
  // to use any remaining space in one of these blocks later.

  constant slot region-allocated-static-blocks :: <stretchy-vector>
    = make(<stretchy-vector>);

  // The allocator entry point points to a C-callable routine
  // that is used to allocate each static block.

  slot region-allocator-entry-point :: false-or(<remote-value>),
    init-value: #f;

  // This slot names the allocator routine. When it is first needed,
  // this name will be resolved to the entry point by an access-path
  // symbolic search.

  constant slot region-allocator-name :: <string>,
    required-init-keyword: allocator:;

  // The following slot is used to classify what kind of memory is
  // allocated for this region, eg: #"compiled-code".

  constant slot region-classification :: <symbol>,
    required-init-keyword: classification:;

  // This slot flags whether the allocator routine name has been
  // resolved to an address. If this value becomes #t without
  // the entry point being assigned, it will be impossible to use
  // this region, and an error will occur.

  slot region-searched-for-allocator? :: <boolean>,
    init-value: #f;

end class;


///// $ILLEGAL-ADDRESS
//    Saves on continual filthy use of false-or(<remote-value>) for
//    operations that might fail.

define constant $illegal-address = as-remote-value(0);


///// <DOWNLOADER-TARGET>
//    Delegates to (rather than subclasses) <debug-target>. Manages
//    access to regions of interactive memory in the application.

define sealed class <downloader-target> (<object>)

  constant slot interactive-application :: <debug-target>,
    required-init-keyword: target:;

  constant slot dylan-import-table   :: <interactive-region>
      = make(<interactive-region>, allocator: "dylan__malloc__misc",
             classification: #"dylan-import");

  constant slot dylan-variables      :: <interactive-region>
      = make(<interactive-region>, allocator: "dylan__malloc__misc",
             classification: #"dylan-variable");

  constant slot dylan-static-objects :: <interactive-region>
      = make(<interactive-region>, allocator: "dylan__malloc__misc",
             classification: #"dylan-static-object");

  constant slot dylan-ambiguous-data :: <interactive-region>
      = make(<interactive-region>, allocator: "dylan__malloc__misc",
             classification: #"dylan-static-object");

  constant slot dylan-fixups :: <interactive-region>
      = make(<interactive-region>, allocator: "dylan__malloc__misc",
             classification: #"dylan-fixup-code");

  constant slot dylan-imports :: <interactive-region>
      = make(<interactive-region>, allocator: "dylan__malloc__misc",
             classification: #"dylan-import-code");

  constant slot dylan-untraced-data :: <interactive-region>
      = make(<interactive-region>, allocator: "dylan__malloc__misc",
             classification: #"dylan-static-object");

  constant slot dylan-interactive-reference-value-cells :: <interactive-region>
      = make(<interactive-region>, allocator: "dylan__malloc__misc",
             classification: #"dylan-variable");

  constant slot compiled-code  :: <interactive-region>
      = make(<interactive-region>, allocator: "dylan__malloc__misc",
             classification: #"compiled-code");

  constant slot foreign-initialized-data :: <interactive-region>
      = make(<interactive-region>, allocator: "dylan__malloc__misc",
             classification: #"initialized-data");

  constant slot miscellaneous-section :: <interactive-region>
      = make(<interactive-region>, allocator: "dylan__malloc__misc",
             classification: #"misc");

end class;


///// *KNOWN-TARGETS*
//    A maintained table that can map a <debug-target> to a 
//    <downloader-target>.

define variable *known-targets* = make(<table>, weak: #"key");


///// FIND-DOWNLOADER-TARGET
//    Searches for (or creates) a <downloader-target> corresponding to a
//    <debug-target>.

define method find-downloader-target
    (application :: <debug-target>) => (dt :: <downloader-target>)

  // If we already have this application in our table, then just return
  // the existing descriptor for it. Otherwise, create a new descriptor,
  // and add this application to the table.

  let got-it-already = element(*known-targets*, application, 
                               default: #f);
  if (got-it-already)
    got-it-already
  else
    *known-targets*[application] := make(<downloader-target>,
                                         target: application);
    *known-targets*[application]
  end if
end method;

