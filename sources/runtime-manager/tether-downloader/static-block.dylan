module:      tether-downloader-internals
synopsis:    Definition of a static block
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <STATIC-BLOCK>
//    This describes a block of memory allocated over the tether. The block
//    is not relocatable. It is a handle via which byte vectors can be
//    loaded into the runtime, into a region of memory known to be valid
//    and safe. The size of a block is equal to the virtual-memory page size
//    of the machine to which we are tethered.

define sealed class <static-block> (<object>)

  // Exported slots

  constant slot static-block-size :: <integer>,
    required-init-keyword: size:;

  constant slot static-block-base-address :: <remote-value>,
    required-init-keyword: base-address:;

  slot static-block-remaining-size :: <integer>,
    required-init-keyword: remaining-size:;

  // Internal slots

  slot block-byte-cursor :: <integer>,
    init-value: 0;

  constant slot block-access-path :: <access-path>,
    required-init-keyword: access-path:;

end class;


///// <STATIC-BLOCK-ERROR>
//    The superclass of all error types signalled specifically by this
//    library.

define class <static-block-error> (<error>)

  constant slot offending-static-block :: <static-block>,
    required-init-keyword: static-block:;

end class;


///// <DOWNLOADING-ERROR>
//    An actual downloading operation failed, even though there was enough
//    space left in the block.

define class <downloading-error> (<static-block-error>)
end class;


///// <BLOCK-ALLOCATION-ERROR>
//    An attempt was made to allocate a block, but it didn't work.

define class <block-allocation-error> (<static-block-error>)

  constant slot failed-allocator-routine :: <remote-value>,
    required-init-keyword: allocator-routine:;

end class;


///// <WRONG-ACCESS-PATH-ERROR>
//    An attempt was made to download data into a static block, via a
//    different <access-path> instance from the one that was used to
//    allocate the block in the first place.

define class <wrong-access-path-error> (<static-block-error>)

  constant slot bad-access-path :: <access-path>,
    required-init-keyword: bad-access-path:;

end class;


///// <NO-ROOM-IN-BLOCK-ERROR>
//    An attempt was made to download data that was too big to fit in the
//    static block.

define class <no-room-in-block-error> (<static-block-error>)

  constant slot failed-request-size :: <integer>,
    required-init-keyword: request-size:;

end class;

  
///// RECYCLE-STATIC-BLOCK
//    Makes it possible to begin downloading data into a block that is full,
//    by overwriting what was there previously. No data in a <static-block>
//    that was downloaded _before_ such a call can still be considered
//    valid afterwards.

define method recycle-static-block (blk :: <static-block>) => ()
  blk.block-byte-cursor := 0;
  blk.static-block-remaining-size := blk.static-block-size;
end method;


///// STATIC-BLOCK-CURRENT-ADDRESS
//    Returns the address of the next free location in the static block.

define method static-block-current-address
    (sb :: <static-block>) => (addr :: <remote-value>)
  byte-indexed-remote-value(sb.static-block-base-address, sb.block-byte-cursor)
end method;

