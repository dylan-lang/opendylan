module:    main-harp
Synopsis:  <harp-back-end> Virtual slot accessors for constants
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// define inline function lomask (field-size :: <integer>)
//  => (mask :: <integer>)
//   ash(1, field-size) - 1
// end function;


// Indexes into function objects


define open generic function-xep-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method function-xep-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 4
end method;


define open generic function-signature-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method function-signature-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 8
end method;


define open generic function-mep-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method function-mep-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 12
end method;


define open generic method-size
      (be :: <harp-back-end>) => (i :: <integer>);

define method method-size
      (be :: <harp-back-end>) => (i :: <integer>)
 16
end method;


// Keyword functions

define open generic function-iep-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method function-iep-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 16
end method;


define open generic function-keywords-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method function-keywords-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 20
end method;


define open generic keyword-method-size
      (be :: <harp-back-end>) => (i :: <integer>);

define method keyword-method-size
      (be :: <harp-back-end>) => (i :: <integer>)
 24
end method;


// Closures

define open generic closure-environment-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method closure-environment-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 16
end method;


define open generic simple-closure-size
      (be :: <harp-back-end>) => (i :: <integer>);

define method simple-closure-size
      (be :: <harp-back-end>) => (i :: <integer>)
 20
end method;


// Keyword closures

define open generic keyword-closure-environment-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method keyword-closure-environment-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 24
end method;


define open generic keyword-closure-size
      (be :: <harp-back-end>) => (i :: <integer>);

define method keyword-closure-size
      (be :: <harp-back-end>) => (i :: <integer>)
 28
end method;


define open generic generic-function-engine-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method generic-function-engine-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 24
end method;




// Indexes into engine node objects


define open generic engine-node-properties-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method engine-node-properties-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 4
end method;


define open generic engine-node-callback-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method engine-node-callback-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 8
end method;


define open generic engine-node-entry-point-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method engine-node-entry-point-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 12
end method;


define open generic engine-node-data-offset
      (be :: <harp-back-end>) => (i :: <integer>);

define method engine-node-data-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 16
end method;


define open generic engine-node-data-1-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method engine-node-data-1-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 engine-node-data-offset(be)
end method;


define open generic engine-node-data-2-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method engine-node-data-2-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 engine-node-data-offset(be) + 4
end method;


define open generic engine-node-data-3-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method engine-node-data-3-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 engine-node-data-offset(be) + 8
end method;



define open generic cache-header-engine-node-next-offset
    (be :: <harp-back-end>) => (i :: <integer>);

define method cache-header-engine-node-next-offset
    (be :: <harp-back-end>) => (i :: <integer>)
  engine-node-data-offset(be)
end method;


define open generic profiling-cache-header-engine-node-count-1-offset
    (be :: <harp-back-end>) => (i :: <integer>);

define method profiling-cache-header-engine-node-count-1-offset
    (be :: <harp-back-end>) => (i :: <integer>)
  engine-node-data-offset(be) + 8
end method;

define open generic profiling-cache-header-engine-node-count-2-offset
    (be :: <harp-back-end>) => (i :: <integer>);

define method profiling-cache-header-engine-node-count-2-offset
    (be :: <harp-back-end>) => (i :: <integer>)
  engine-node-data-offset(be) + 12
end method;

    
// Indexes into wrapper objects


define open generic mm-wrapper-subtype-mask-offset
    (be :: <harp-back-end>) => (i :: <integer>);

define method mm-wrapper-subtype-mask-offset
    (be :: <harp-back-end>) => (i :: <integer>);
  8
end method;

// **** These constants are derived (manually) from $dylan-system-subtype-bit-names, in
// **** D-dfmc-modeling:objects.dylan.  They are also exported from harp-dylan-parameterizations.

define constant $mm-wrapper-subtype-value-object-mask = 1;
define constant $mm-wrapper-subtype-mm-wrapper-mask = 2;
define constant $mm-wrapper-subtype-class-mask = 4;
define constant $mm-wrapper-subtype-implementation-class-mask = 8;
define constant $mm-wrapper-subtype-by-class-discriminator-mask = 16;
define constant $mm-wrapper-subtype-abstract-integer-mask = 32;
define constant $mm-wrapper-subtype-function-mask = 64;
define constant $mm-wrapper-subtype-sequence-mask = 128;
define constant $mm-wrapper-subtype-string-mask = 256;
define constant $mm-wrapper-subtype-error-mask = 512;
define constant $mm-wrapper-subtype-collection-mask = 1024;
define constant $mm-wrapper-subtype-cache-header-engine-node-mask = 2048;


// **** The master definitions of all these constants is in D-lib-dylan:dispatch-prologue.dylan.
// **** They are copied to here, with adjustment of the first field position,
// **** and exported from the harp-dylan-parameterizations module.

define constant properties$v-entry-type = 2;
define constant properties$s-entry-type = 6;

define constant properties$v-data-start = properties$s-entry-type + properties$v-entry-type;

define constant engine-node$v-data-start = 14 + properties$v-entry-type;

define constant smen$v-nrequired = properties$v-data-start;
define constant smen$s-nrequired = 8;
define constant smen$m-nrequired = ash(ash(1, smen$s-nrequired) - 1, smen$v-nrequired);
define constant smen$v-restp = smen$v-nrequired + smen$s-nrequired;
define constant smen$v-data-start = smen$v-restp + 1;


define constant $simple-typechecked-cache-arguments-limit = 8;

define constant stchen$v-checkedmask = engine-node$v-data-start;
define constant stchen$s-checkedmask = $simple-typechecked-cache-arguments-limit;
define constant stchen$m-checkedmask = ash(ash(1, stchen$s-checkedmask) - 1, stchen$v-checkedmask);

define constant discriminator$v-argnum = properties$v-data-start;

define constant discriminator$s-argnum = 8;

define constant discriminator$v-nrequired = discriminator$v-argnum + discriminator$s-argnum;
define constant discriminator$s-nrequired = 8;

define constant discriminator$v-restp = discriminator$v-nrequired + discriminator$s-nrequired;

define constant discriminator$v-data-start = 23;


// Indexes into signature objects


define open generic signature-properties-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method signature-properties-offset
      (be :: <harp-back-end>) => (i :: <integer>)
  4
end method;

define open generic signature-required-offset 
      (be :: <harp-back-end>) => (i :: <integer>);

define method signature-required-offset
      (be :: <harp-back-end>) => (i :: <integer>)
 8
end method;




// Information about functions in types


define open generic type-instancep-function-offset
      (be :: <harp-back-end>) => (i :: <integer>);

define method type-instancep-function-offset
      (be :: <harp-back-end>) => (i :: <integer>)
  4
end method;




// Information about stack frames

define open generic size-of-unwind-protect-frame 
      (be :: <harp-back-end>) => (i :: <integer>);

define method size-of-unwind-protect-frame
      (be :: <harp-back-end>) => (i :: <integer>)
 12
end method;


define open generic size-of-bind-exit-frame 
      (be :: <harp-back-end>) => (i :: <integer>);

define method size-of-bind-exit-frame
      (be :: <harp-back-end>) => (i :: <integer>)
 56
end method;


