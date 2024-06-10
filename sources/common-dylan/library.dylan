Module:       dylan-user
Synopsis:     Common Dylan library definition
Author:       Andy Armstrong
Version:      $HostName$
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library common-dylan
  use dylan,
    export: { dylan,
              finalization,
              threads };
  export
    common-dylan,
    common-extensions,
    streams-protocol,
    locators-protocol,
    machine-words,
    simple-random,
    simple-profiling,
    simple-timers,
    simple-format,
    byte-vector,
    transcendentals;

  // For the test suite only.
  export
    common-dylan-internals;
end library common-dylan;

define module simple-profiling
  create \timing;

  create \profiling,
         <profiling-state>,
         start-profiling,
         start-profiling-type,
         stop-profiling,
         stop-profiling-type,
         profiling-type-result;
end module simple-profiling;

define module simple-timers
  create <profiling-timer>,
         timer-start,
         timer-stop,
         timer-accumulated-time,
         timer-running?;
end module simple-timers;

define module byte-vector
  use dylan-extensions,
    export: { <byte> };
  create <byte-vector>,
         byte-vector-fill,
         byte-vector-ref,
         byte-vector-ref-setter,
         copy-bytes,
         byte-storage-address,
         byte-storage-offset-address,
         hexstring, from-hexstring
end module byte-vector;

define module common-extensions
  use dylan-extensions,
    export: { <bottom>,
              <format-string-condition>, // Deprecated...
              <simple-condition>,        // ...use this instead.
                <stack-overflow-error>,
                <arithmetic-error>,
                  <division-by-zero-error>,
                  <arithmetic-domain-error>,
                  <arithmetic-overflow-error>,
                  <arithmetic-underflow-error>,
              <stretchy-object-vector>,
              <object-deque>,
              <stretchy-sequence>,
              <string-table>,
              false-or,
              ignorable,
              ignore,
              \iterate,
              one-of,
              remove-all-keys!,
              rest,
              subclass,
              \when,
              register-application-exit-function };
  use simple-debugging,
    export: { \assert,
              \debug-assert,
              debug-message };
  use simple-profiling,
    export: { \profiling,
              profiling-type-result };
  use byte-vector,
    export: { <byte-vector>,
              byte-storage-address,
              byte-storage-offset-address};
  create <closable-object>,
         <stream>,
         close,
         integer-length,
         <float-classification>,
         classify-float,
         decode-float,
         scale-float,
         float-radix,
         float-digits,
         float-precision,
         $single-float-epsilon,
         $double-float-epsilon,
         $minimum-single-float-exponent,
         $maximum-single-float-exponent,
         $minimum-double-float-exponent,
         $maximum-double-float-exponent,
         $unsupplied, unsupplied, unsupplied?, supplied?,
         $unfound,    unfound,    unfound?,    found?,
         true?, false?,
         concatenate!,
         condition-to-string,
         difference,
         position,
         split,
         join,
         fill-table!,
         find-element,
         float-to-string,
         integer-to-string,
         number-to-string,
         string-to-integer,
         machine-word-to-string,
         string-to-machine-word,
         \table-definer,
         application-name,
         application-filename,
         application-arguments,
         tokenize-command-line,
         exit-application;
end module common-extensions;

define module common-dylan
  use dylan, export: all;
  use common-extensions, export: all;
end module common-dylan;

define module simple-format
  create format-out,
         format-to-string;
end module simple-format;

define module simple-random
  create <random>,
         random;
end module simple-random;

define module locators-protocol
  create <locator>;
  create supports-open-locator?,
         open-locator,
         supports-list-locator?,
         list-locator;

  create <server-locator>,
         <physical-locator>;
end module locators-protocol;

define module streams-protocol
  use common-extensions,
    import: { <stream>, close },
    export: all;
  // Conditions
  create <stream-error>,
           stream-error-stream,
         <end-of-stream-error>,
           <incomplete-read-error>,
             stream-error-sequence,
             stream-error-count,
           <incomplete-write-error>,
             stream-error-count;
  // Opening streams
  create open-file-stream;
  // Reading from streams
  create read-element,
         unread-element,
         peek,
         read,
         read-into!,
         discard-input,
         stream-input-available?,
         stream-contents,
         stream-contents-as;
  // Writing to streams
  create write-element,
         write,
         force-output,
         wait-for-io-completion,
         synchronize-output,
         discard-output;
  // Querying streams
  create stream-open?,
         stream-element-type,
         stream-at-end?,
         stream-size;
  // Positioning streams
  create <positionable-stream>,
         stream-position,
         stream-position-setter,
         adjust-stream-position;
  // Wrapper streams
  create outer-stream,
         outer-stream-setter;
end module streams-protocol;

define module transcendentals
  use dylan;
  use dylan-primitives;
  export $single-pi, $double-pi, // $extended-pi,
         $single-e,  $double-e,  // $extended-e,
         sqrt,
         isqrt,
         log,
         exp,
         logn,
         ilog2,
         sin,
         cos,
         tan,
         sincos,
         asin,
         acos,
         atan,
         atan2,
         sinh,
         cosh,
         tanh,
         asinh,
         acosh,
         atanh,
         hypot;
  // In addition to the above exports, these methods are added:
  //   ^(<single-float>, <single-float>)
  //   ^(<double-float>, <double-float>)
  //   ^(<single-float>, <double-float>)
  //   ^(<double-float>, <single-float>)
end module transcendentals;

define module machine-words
  use dylan-extensions,
    export: {<machine-word>,
             $machine-word-size,
             $maximum-signed-machine-word,
             $minimum-signed-machine-word,
             $maximum-unsigned-machine-word,
             $minimum-unsigned-machine-word,
             as-unsigned };
  create %logior,
         %logxor,
         %logand,
         %lognot,
         %logbit?,
         %count-low-zeros,
         %count-high-zeros,
         %count-ones,
         \%+,
         \%-,
         \%*,
         %floor/,
         %ceiling/,
         %round/,
         %truncate/,
         %divide,
         %negative,
         %abs,
         %shift-left,
         %shift-right;
  create so%+,
         so%-,
         so%*,
         so%negative,
         so%abs,
         so%shift-left;
  create d%floor/,
         d%ceiling/,
         d%round/,
         d%truncate/,
         d%divide;
  create u%+,
         u%-,
         u%*,
         u%divide,
         u%rotate-left,
         u%rotate-right,
         u%shift-left,
         u%shift-right,
         u%<;
  create ud%divide,
         ud%shift-left,
         ud%shift-right;
end module machine-words;

define module common-dylan-internals
  use common-dylan;
  use dylan-extensions;
  use dylan-direct-c-ffi;
  use machine-word-lowlevel;
  use machine-words;
  use threads;
  use transcendentals;
  use byte-vector;
  use streams-protocol;
  use locators-protocol;
  use simple-random;
  use simple-profiling;
  use simple-timers;
  use simple-format;

  // Exports for the test suite only.
  export
    character-to-integer;
end module common-dylan-internals;
