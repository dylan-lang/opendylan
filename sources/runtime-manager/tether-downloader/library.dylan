module:      dylan-user
synopsis:    The lowlevel component of the interactive downloader (Experiment)
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library tether-downloader
  use common-dylan;
  use collections;
  use io;
  use access-path;
  export
      tether-downloader;
end library;

define module tether-downloader
  use common-dylan;
  use byte-vector;
  use format;
  use access-path;
  create
     // The static block class.

     <static-block>,

     // Associated error classes.

     <static-block-error>,
     <no-room-in-block-error>,
     <downloading-error>,
     <block-allocation-error>,
     <wrong-access-path-error>,
     offending-static-block,
     failed-allocator-routine,
     bad-access-path,
     failed-request-size,

     // Exported accessors.

     static-block-size,                    // static-block => integer
     static-block-base-address,            // static-block => remote-value
     static-block-remaining-size,          // static-block => integer
     static-block-current-address,         // static-block => remote-value

     // Downloading operations.

     download-byte-vector-into,
     download-remote-value-into,
     download-remote-value-vector-into,
     download-byte-string-into,

     // Alignment
     // (Functions called to ensure that the next download operation is to
     // an aligned address)

     block-align-1,   // Probably a NOP, but exported for completeness.
     block-align-2,
     block-align-4,
     block-align-8,
     block-align-n,
     block-align-remote-value,

     // Allocation and management.

     recycle-static-block,
     allocate-single-static-block,  // access-path => false-or(static-block)
     allocate-multiple-static-blocks; // access-path integer => sequence

end module;

define module tether-downloader-internals
  use common-dylan;
  use byte-vector;
  use format;
  use format-out;
  use access-path;
  use tether-downloader;
end module;

// Note: format-out is imported for debug code only. Should be removed.
