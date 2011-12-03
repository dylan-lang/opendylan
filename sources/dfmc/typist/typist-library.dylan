Module:    Dylan-User
Author:    Steve Rowley
Synopsis:  Library and module definitions for the typist.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
/// The Typist -- A type-inference module for the DFM.
///

define library dfmc-typist
  // Library for the type inference tool.
  use dylan;
  use dfmc-core;
  use dfmc-reader;

  export dfmc-typist;
end;

define module dfmc-typist
  // Module for the typist.
  use dylan,
    rename: { make => dylan/make };
  use dfmc-core;
  use dfmc-reader;
  use dfmc-imports;

  export 
   // Classes                               Generics
   // =======                               ========
   <justification>,                    justification-rule, justification-lhs,
                                       justification-rhs,
   <type-cache>,
   <type-variable>,                    type-variable-contents,
                                       type-variable-supporters,
                                       type-variable-supportees,

   <type-estimate>,                    type-estimate-debug-name,
   <type-estimate-top>,
   <type-estimate-class>,              type-estimate-class,
   <type-estimate-raw>,                type-estimate-raw,
   <type-estimate-values>,             type-estimate-fixed-values,
                                       type-estimate-rest-values,
                                       type-estimate-values-ref,
   <type-estimate-limited>, 
   <type-estimate-limited-integer>,    type-estimate-min, type-estimate-max,
   <type-estimate-limited-class>,
   <type-estimate-limited-instance>,   type-estimate-singleton,
   <type-estimate-limited-collection>, type-estimate-of, type-estimate-size,
                                       type-estimate-concrete-class,
                                       type-estimate-dimensions,
   <type-estimate-limited-function>,   type-estimate-requireds,
                                       type-estimate-rest?, type-estimate-keys,
                                       type-estimate-all-keys?,
                                       type-estimate-values,
   <type-estimate-union>,              type-estimate-unionees,
   <type-estimate-bottom>,
   
   // Algebra of <type-estimate>s.
   type-estimate-normalize, type-estimate-union, type-estimate-intersection,
   type-estimate-difference, type-estimate-base, type-estimate-match?,
   type-estimate-hash, 

   // Tables of <type-estimate>s hashing with type-estimate-match? invariant.
   <type-estimate-match-table>, <type-estimate-pair-match-table>,

   // Predicates on the estimated types.
   type-estimate-instance?, type-estimate-disjoint?, type-estimate-subtype?,
   type-estimate=?, type-estimate-pseudosubtype?,
   type-estimate-values-element-subtype?, type-estimate-values-rest-subtype?,

   // Main GF's for calling the type inferencer
   type-estimate, // remove this at some point!!!
   lookup-type,     // new typist api, gts
   type-initializer-method, // new typist api, gts
   type-estimate-explain, type-estimate-retract,
   type-estimate-in-cache, // for typist inference tests only
   type-estimate-top-level-form, // Interface to rest of compiler

   // Dispatch caching support
   type-estimate-dispatch-cache-lookup,
   add-type-estimate-dispatch-cache-entry,

   // Utilities
   make-type-estimate,
   ^make-method?,
   ^make-return-class-from-signature,
   constant-value?,

   // Random tuulz
   print-separated-collection, map-table, table=?,
   ^classes-guaranteed-disjoint?, \with-infer-stepping, 
     *stepping-infer?*, *tracing-infer?*;
end;
