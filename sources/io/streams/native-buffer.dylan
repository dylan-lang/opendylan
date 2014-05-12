Module:       streams-internals
Synopsis:     Definition of buffer class and methods for buffered streams
Author:       Toby Weinberg, Scott McKay, Marc Ferguson, Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Buffers

// TODO: EVENTUALLY WANT
//   define constant <byte-value>              = <byte>;
//   define constant <byte-representation>     = <byte>;
//   define constant $byte-representation-fill = 0;
// BUT CURRENTLY TYPIST IS NOT UP TO IT

define constant <byte-value>              = <integer>;
define constant <byte-representation>     = <byte-character>;
define constant $byte-representation-fill = ' ';

define sealed primary class <buffer> (<vector>)
  slot buffer-next   :: <buffer-index> = 0, init-keyword: buffer-next:;
  slot buffer-end    :: <buffer-index> = 0, init-keyword: buffer-end:;
  slot buffer-position :: <buffer-index> = 0, init-keyword: buffer-position:;
  slot buffer-dirty?  :: <boolean> = #f, init-keyword: buffer-dirty?:;
  // Stuff for <power-of-two-buffer>
  slot buffer-start :: <buffer-index> = 0, init-keyword: buffer-start:;
  slot buffer-on-page-bits :: <integer>;
  slot buffer-off-page-bits :: <integer>;
  // Stuff for <multi-buffered-stream>
  slot buffer-use-count :: <integer> = 0, init-keyword: use-count:;
  slot buffer-owning-stream :: false-or(<integer>) = #f, init-keyword: stream-id:;
  // DA DATA
  repeated slot buffer-element :: <byte-representation>,
    init-value:        $byte-representation-fill,
    init-keyword:      fill:,
    size-init-value:   0,
    size-init-keyword: size:,
    size-getter:       size;
end class;

define sealed domain size (<buffer>);
define sealed domain make (singleton(<buffer>));
define sealed domain initialize (<buffer>);

define inline sealed method make
    (class :: subclass(<buffer>),
     #rest all-keys, #key fill = $byte-representation-fill, #all-keys)
 => (buffer :: <buffer>)
  apply(next-method, class, fill: as(<byte-representation>, fill), all-keys)
end method;

define inline sealed method buffer-size
    (buffer :: <buffer>) => (start :: <buffer-index>)
  buffer.size
end method buffer-size;

//
// ELEMENT
//

define /* inline */ sealed method element
    (buffer :: <buffer>, index :: <integer>,
     #key default = unsupplied()) => (result :: <byte-value>)
  if (element-range-check(index, size(buffer)))
    element-no-bounds-check(buffer, index)
  else
    if (unsupplied?(default))
      element-range-error(buffer, index)
    else
      default
    end if
  end if
end method;

//
// buffer-element returns a <character>
//

//
// buffer-element returns a <character> not an <integer>
//

define inline sealed method element-no-bounds-check
    (buffer :: <buffer>, index :: <integer>, #key default)
         => (res :: <byte-value>)
  as(<byte-value>, buffer-element(buffer, index))
end method;

//
// ELEMENT-SETTER
//

define /* inline */ sealed method element-setter
    (new-value :: <byte-value>,
     vector :: <buffer>, index :: <integer>) => (value)
  if (element-range-check(index, size(vector)))
    buffer-element(vector, index) := as(<byte-representation>, new-value);
  else
    element-range-error(vector, index);
  end if
end method;

define /* inline */ sealed method element-setter
    (new-value :: <byte-character>,
     vector :: <buffer>, index :: <integer>) => (value)
  if (element-range-check(index, size(vector)))
    buffer-element(vector, index) := as(<byte-representation>, new-value)
  else
    element-range-error(vector, index)
  end if
end method;


//
// ELEMENT-NO-BOUNDS-CHECK-SETTER
//

define inline sealed method element-no-bounds-check-setter
    (new-value :: <byte-value>,
     buffer :: <buffer>, index :: <integer>) => (value)
  buffer-element(buffer, index) := as(<byte-representation>, new-value);
end method;

define inline sealed method element-no-bounds-check-setter
    (new-value :: <byte-character>,
     buffer :: <buffer>, index :: <integer>) => (value)
  buffer-element(buffer, index) := as(<byte-representation>, new-value);
end method;

//
// EMPTY?
//

define inline sealed method empty? (buffer :: <buffer>)
 => (result :: <boolean>)
  size(buffer) = 0
end method;

/// Special <buffer> <-> <byte-string> coercions

define sealed method as (bsc == <byte-string>,  buffer :: <buffer>)
 => (bs :: <byte-string>)
  let bs :: <byte-string> = make(<byte-string>, size: buffer.size);
  without-bounds-checks
    for (i :: <integer> from 0 below buffer.size)
      // buffer-element returns a character
      element(bs, i) := as(<byte-character>, element(buffer, i));
    end for;
  end without-bounds-checks;
  bs
end method;

define sealed method as (buffer-class == <buffer>, bs :: <byte-string>)
 => (buffer :: <buffer>)
  let buffer :: <buffer> = make(<buffer>, size: bs.size);
  without-bounds-checks
    for (i :: <integer> from 0 below bs.size)
      element(buffer, i) := as(<byte-representation>, element(bs, i));
    end for;
  end without-bounds-checks;
  buffer
end method;

// already is a vector.  Maybe want method for simple object vector?
define sealed method as
    (class == <vector>, x :: <buffer>) => (vector :: <vector>)
  x
end method;


//// ITERATION

define inline function buffer-current-element
    (buffer :: <buffer>, state :: <integer>)
 => (result :: <byte-value>)
  element-no-bounds-check(buffer, state);
end function;

define inline function buffer-current-element-setter
    (new-value :: <byte-value>,
     buffer :: <buffer>, state :: <integer>) => ()
  element-no-bounds-check(buffer, state) := new-value;
end function;

define inline method forward-iteration-protocol
    (buffer :: <buffer>)
 => (initial-state :: <integer>, limit :: <integer>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>);
  let (initial-state, limit, next-state, finished-state?, current-key,
       current-element, current-element-setter, copy-state)
    = next-method();
  ignore(current-element, current-element-setter);
  values
    (initial-state, limit, next-state, finished-state?, current-key,
     buffer-current-element, buffer-current-element-setter,
     copy-state)
end method forward-iteration-protocol;


/// Fast buffer copying

define inline function buffer-ref
    (buffer :: <buffer>, index :: <integer>)
 => (result :: <byte-value>)
  element-no-bounds-check(buffer, index)
end function buffer-ref;

define inline function buffer-ref-setter
    (value :: <byte-value>, buffer :: <buffer>, index :: <integer>)
  element-no-bounds-check(buffer, index) := value;
end function buffer-ref-setter;


define inline-only function fill-bytes!
    (target :: <buffer>, value :: <byte-value>,
     start :: <integer>, last :: <integer>) => ()
  primitive-fill-bytes!
    (target, primitive-repeated-slot-offset(target), integer-as-raw(start),
     integer-as-raw(last - start), integer-as-raw(value))
end;

define sealed method buffer-fill
    (target :: <buffer>, value :: <byte-value>,
     #key start :: <integer> = 0, end: last = size(target)) => ();
  let last :: <integer> =
    check-start-compute-end(target, start, last);
  fill-bytes!(target, value, start, last)
end;

define sealed method buffer-fill
    (target :: <buffer>, value :: <byte-character>,
     #key start :: <integer> = 0, end: last = size(target)) => ();
  buffer-fill(target, as(<byte-value>, value), start: start, end: last)
end;

//---*** It would sure be nice to have low-level run-time support for this

define function copy-bytes-range-error
    (src, src-start :: <integer>, dst, dst-start :: <integer>, n :: <integer>)
 => ()
  error("SRC-START %d DST-START %d AND N %d OUTSIDE OF SRC %= AND DST %=",
        src-start, dst-start, n, src, dst);
end function;

define sealed method copy-bytes
    (dst :: <byte-string>, dst-start :: <integer>,
     src :: <buffer>, src-start :: <integer>, n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src) & dst-end <= size(dst))
    primitive-replace-bytes!
      (dst, primitive-repeated-slot-offset(dst), integer-as-raw(dst-start),
       src, primitive-repeated-slot-offset(src), integer-as-raw(src-start),
       integer-as-raw(n));
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if;
end method;

define sealed method copy-bytes
    (dst :: <buffer>, dst-start :: <integer>,
     src :: <byte-string>, src-start :: <integer>, n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src)
        & dst-end <= size(dst))
    primitive-replace-bytes!
      (dst, primitive-repeated-slot-offset(dst), integer-as-raw(dst-start),
       src, primitive-repeated-slot-offset(src), integer-as-raw(src-start),
       integer-as-raw(n));
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if;
end method;

define sealed method copy-bytes
    (dst :: <buffer>, dst-start :: <integer>,
     src :: <buffer>, src-start :: <integer>, n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src) & dst-end <= size(dst))
    primitive-replace-bytes!
      (dst, primitive-repeated-slot-offset(dst), integer-as-raw(dst-start),
       src, primitive-repeated-slot-offset(src), integer-as-raw(src-start),
       integer-as-raw(n));
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if;
end method;

define sealed method copy-bytes
    (dst :: <byte-vector>, dst-start :: <integer>,
     src :: <buffer>, src-start :: <integer>, n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src) & dst-end <= size(dst))
    primitive-replace-bytes!
      (dst, primitive-repeated-slot-offset(dst), integer-as-raw(dst-start),
       src, primitive-repeated-slot-offset(src), integer-as-raw(src-start),
       integer-as-raw(n));
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if;
end method;

define sealed method copy-bytes
    (dst :: <buffer>, dst-start :: <integer>,
     src :: <byte-vector>, src-start :: <integer>, n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src)
        & dst-end <= size(dst))
    primitive-replace-bytes!
      (dst, primitive-repeated-slot-offset(dst), integer-as-raw(dst-start),
       src, primitive-repeated-slot-offset(src), integer-as-raw(src-start),
       integer-as-raw(n));
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if;
end method;

define sealed method copy-bytes
    (dst :: <buffer>, dst-start :: <integer>,
     src :: <simple-object-vector>, src-start :: <integer>, n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src) & dst-end <= size(dst))
    for (src-i :: <integer> from src-start below src-end,
         dst-i :: <integer> from dst-start)
      buffer-element(dst, dst-i)
        := as(<byte-representation>, element-no-bounds-check(src, src-i));
    end for;
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if;
end method;

define sealed method copy-bytes
    (dst :: <simple-object-vector>, dst-start :: <integer>,
     src :: <buffer>, src-start :: <integer>, n :: <integer>)
 => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src) & dst-end <= size(dst))
    without-bounds-checks
      for (src-i :: <integer> from src-start below src-end,
           dst-i :: <integer> from dst-start)
        dst[dst-i] := src[src-i];
      end for;
    end without-bounds-checks;
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if;
end method;
