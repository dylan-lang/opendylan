module:    harp-native-rtg
Synopsis:  Constant definitions for the Dylan runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



//// Define any constants that don't obviously live somewhere specific



/// Externals

define dylan-internal runtime-external dylan-unbound      = "%unbound";
define dylan-internal runtime-external dylan-false        = "%false";
define dylan-internal runtime-external dylan-true         = "%true";
define dylan-internal runtime-external dylan-empty-list   = "%empty-list";
define dylan-internal runtime-external dylan-empty-vector = "%empty-vector";

define dylan-internal-indirect runtime-external dylan-direct-object-mm-wrappers = "$direct-object-mm-wrappers";
define dylan-internal-indirect runtime-external dylan-direct-object-classes     = "$direct-object-classes";

define internal-wrapper runtime-external dylan-value-cell-class          = "<traceable-value-cell>";
define internal-wrapper runtime-external raw-value-cell-class            = "<untraceable-value-cell>";
define internal-wrapper runtime-external raw-double-value-cell-class     = "<untraceable-double-value-cell>";

define extensions-wrapper runtime-external machine-word-class = "<machine-word>";

define extensions-indirect runtime-external dispatch-profiling? = "*dispatch-profiling-enabled?*";
define extensions-indirect runtime-external class-profiling? = "*class-profiling-enabled?*";

define dylan-library runtime-external object-class-class      = "<object>";
define dylan-wrapper runtime-external object-class-wrapper    = "<object>";
define dylan-wrapper runtime-external single-float-class      = "<single-float>";
define dylan-wrapper runtime-external double-float-class      = "<double-float>";
define dylan-wrapper runtime-external dylan-sov-class         = "<simple-object-vector>";
define dylan-wrapper runtime-external dylan-byte-string-class = "<byte-string>";
define dylan-wrapper runtime-external dylan-method-class      = "<method>";
define dylan-library runtime-external dylan-error-function    = "error";

define internal-iep runtime-external dylan-arg-count-error   = "argument-count-error";
define internal-iep runtime-external dylan-odd-keys-error    = "odd-keyword-arguments-error";
define internal-iep runtime-external dylan-unknown-key-error = "unknown-keyword-argument-error";
define internal-iep runtime-external dylan-type-check-error  = "type-check-error";
define internal-iep runtime-external dylan-stack-overflow-error = "stack-overflow-error";
define extensions-iep runtime-external dylan-grounded-instance = "grounded-instance?";

define c-fun runtime-external mm-collect-count     = "MMCollectCount";
define c-fun runtime-external raw-malloc           = "dylan__malloc__misc";
define c-fun runtime-external raw-malloc-ambig     = "dylan__malloc__ambig";
define c-fun runtime-external raw-malloc-exact     = "dylan__malloc__exact";
define c-fun runtime-external raw-free-root        = "dylan__free__root";
define c-fun runtime-external mps-malloc           = "mps__malloc";
define c-fun runtime-external mps-free             = "mps__free";
define c-fun runtime-external mps-park             = "primitive_mps_park";
define c-fun runtime-external mps-release          = "primitive_mps_release";



/// Literals - define these last because they may depend on externals (e.g. wrappers)

define runtime-literal primitive-error-string 
  = "primitive-error-string", data: "Primitive error.";

