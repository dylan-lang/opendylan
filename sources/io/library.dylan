Module:       dylan-user
Synopsis:     A portable IO library
Author:       Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library io
  use functional-dylan;
  export
    streams,
    streams-internals,
    pprint,
    print,
    print-internals,
    format,
    format-internals,
    standard-io,
    format-out;
end library io;

define module streams
  use streams-protocol,
    export: all;
  use dylan-extensions,
    export: { <byte> };
  use functional-dylan,
    export: { <byte-character>, <unicode-character> };
  use byte-vector,
    export: { <byte-vector> };

  // Basic stream class
  create <basic-stream>;

  // Conditions
  create <stream-position-error>,
	 <stream-closed-error>,
	 <stream-not-writable>,
	 <stream-not-readable>;

  // Reading from streams
  create read-character,
	 read-text,
	 read-text-into!;

  // Writing to streams
  create write-text;

  // Convenience functions
  create read-to,
	 read-through,
	 read-to-end,
	 skip-through;

  // Line-oriented functions
  create read-line,
	 read-line-into!,
	 write-line,
	 new-line;

  // Used mainly by the above convenience functions and line-oriented functions.
  create stream-sequence-class;

  // Positionable streams
  create <basic-positionable-stream>,
	 <position-type>,
	 <stream-position>;

  // Locking streams
  create \with-stream-locked,
	 stream-lock, stream-lock-setter,
	 // obsolete but still used, actually these don't really work but
	 // code still apparently depends on them 
	 lock-stream, unlock-stream, stream-locked?;

  // Buffers
  create <buffer>,
	 <buffer-index>,
	 buffer-start, buffer-start-setter,
	 buffer-end, buffer-end-setter,
	 buffer-next, buffer-next-setter,
	 buffer-dirty?, buffer-dirty?-setter,
	 buffer-position, buffer-position-setter,
         buffer-fill,
	 buffer-size,
	 buffer-subsequence,
	 copy-into-buffer!,
	 copy-from-buffer!;

  // Buffered streams
  create <buffered-stream>,
	 get-input-buffer,     do-get-input-buffer,
	 release-input-buffer, do-release-input-buffer,
	 next-input-buffer,    do-next-input-buffer,
	 \with-input-buffer,
	 input-available-at-source?, do-input-available-at-source?,
	 get-output-buffer,     do-get-output-buffer,
	 release-output-buffer, do-release-output-buffer,
	 next-output-buffer,    do-next-output-buffer,
	 \with-output-buffer,
	 force-output-buffers,  do-force-output-buffers;

  // Sequence streams
  create \with-output-to-string,
	 <sequence-stream>,
	 <string-stream>,
	 <byte-string-stream>,
	 <unicode-string-stream>,
	 type-for-sequence-stream,
	 stream-limit;

  // Wrapper streams
  create <wrapper-stream>,
         inner-stream, inner-stream-setter,
         outer-stream, outer-stream-setter;
end module streams;

define module streams-internals
  use functional-dylan;
  use functional-locators-protocol;
  use simple-format;
  use dylan-extensions;
  use dylan-direct-c-ffi;
  use byte-vector;
  use threads;
  use streams,
    export: all;

  // Basic stream classes
  export <typed-stream>,
	 <general-typed-stream>,
	 <byte-element-stream>,
	 <byte-char-element-stream>;

  // Efficient querying direction
  export readable?, writable?, closed?, read-only?, write-only?, read-write?;

  // Conditions
  export end-of-stream-value,
	 stream-error-requested-position,
	 stream-error-size-of-stream,
	 ensure-readable, ensure-writable;

  // Querying streams
  export stream-direction;

  // Positionable streams
  export current-position, current-position-setter,
	 initial-position,
	 final-position;

  // Buffers and buffered streams
  export *multi-buffer-bytes*,
	 <power-of-two-buffer>,
	 <single-buffered-stream>,
	 <double-buffered-stream>,
	 make-<power-of-two-buffer>,
	 buffer-off-page-bits,
	 buffer-on-page-bits,
	 buffer-owning-stream, buffer-owning-stream-setter,
	 buffer-use-count, buffer-use-count-setter,
	 ensure-input-buffer, ensure-output-buffer,
	 coerce-to-element,
	 coerce-from-element,
	 coerce-to-sequence,
	 coerce-from-sequence,
	 round-to-power-of-two,
	 stream-input-buffer,  stream-input-buffer-setter,
	 stream-output-buffer, stream-output-buffer-setter,
	 stream-shared-buffer, stream-shared-buffer-setter,
	 actual-stream-input-buffer, actual-stream-input-buffer-setter,
	 actual-stream-output-buffer, actual-stream-output-buffer-setter;

  // File streams
  export <file-stream>,
         <byte-file-stream>,
         <external-file-accessor>,
         type-for-file-stream,
         stream-locator,
         writable-file-stream-position-setter,
         <general-file-stream>,
         <byte-char-file-stream>;
 
  // Multi-buffered streams
  export <buffer-vector>,
         <multi-buffered-stream>,
         multi-buffered-stream-position-setter,
         write-4-aligned-bytes-from-word,
         read-4-aligned-bytes-as-word,
         write-4-aligned-bytes, write-8-aligned-bytes,
         read-4-aligned-bytes, read-8-aligned-bytes,
         multi-buffer-working-set,
         multi-buffer-reads,
         multi-buffer-bytes,
         multi-buffer-total-working-set,
         multi-buffer-total-reads,
         multi-buffer-total-bytes,
         <general-multi-buffered-stream>,
         <byte-multi-buffered-stream>,
         <byte-char-multi-buffered-stream>;

  // Sequence streams
  export clear-contents,
         newline-sequence,
	 stream-limit-setter,
	 stream-sequence;

  // Stream access paths
  export <external-stream-accessor>,
	 <external-stream>,
	 platform-accessor-class,
	 new-accessor,
	 accessor, accessor-setter,
	 accessor-open,
	 accessor-open?,
	 accessor-close,
         accessor-at-end?,
         accessor-at-end?-setter,
         accessor-size,
         accessor-size-setter,
         accessor-positionable?,
         accessor-position,
         accessor-position-setter,
	 accessor-force-output,
	 accessor-wait-for-completion,
	 accessor-newline-sequence,
	 accessor-preferred-buffer-size,
	 accessor-fd,
	 accessor-synchronize,
	 accessor-read-into!,
	 accessor-write-from,
	 *open-accessors*;

  // "High performance"
  export \copy-down-stream-definer,
         read-skip,
         write-fill;

  // Asynchronous writes
  export <pending-operation>,
         <pending-write>,
         async-check-for-errors,
         async-wait-for-completion,
         async-wait-for-overlapping-write-completion,
         enqueue-operation,
         enqueue-write,
         pending-accessor,
         pending-buffer,
         pending-buffer-offset,
         pending-count,
         pending-file-offset,
         pending-stream;

  // File accessors
  create <native-file-accessor>;
end module streams-internals;

define module pprint
  create <pretty-stream>,
	 \printing-logical-block,
	 pprint-logical-block,
	 pprint-newline,
	 pprint-indent,
	 pprint-tab,
	 *default-line-length*,
	 *print-miser-width*;
end module pprint;

define module print
  create print,
	 print-object,
	 print-to-string,
	 *print-length*,
	 *print-level*,
	 *print-circle?*,
	 *print-pretty?*,
	 *print-escape?*;

  create \printing-object,
         do-printing-object;
end module print;

define module print-internals
  use functional-dylan;
  use functional-locators-protocol;
  use transcendentals;
  use threads;
  use dylan-extensions;
  use dylan-primitives;
  use byte-vector;
  use streams-internals;
  use pprint,
    export: all;
  use print,
    export: all;

  export *default-length*,
	 *default-level*,
	 *default-circle?*,
	 *default-pretty?*,
	 *default-escape?*,
	 *print-depth*;
end module print-internals;

define module format
  create format,
	 format-to-string,
	 print-message;
end module format;

define module format-internals
  use functional-dylan,
    exclude: { format-to-string };
  use dylan-extensions;
  use transcendentals;
  use threads;
  use streams-internals;
  use print;
  use format,
    export: all;
end module format-internals;

define module standard-io
  create *standard-input*,
	 *standard-output*,
	 *standard-error*;
end module standard-io;

define module format-out
  create format-out;
end module format-out;

define module io-internals
  use functional-dylan,
    exclude: { format-to-string };
  use dylan-direct-c-ffi;
  use threads;
  use streams-internals;
  use format-internals;
  use standard-io;
  use format-out;
end module io-internals;
