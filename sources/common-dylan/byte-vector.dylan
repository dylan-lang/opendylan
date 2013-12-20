Module:       common-dylan-internals
Author:       Toby Weinberg, Jonathan Bachrach + Eliot Miranda, Scott McKay
Synopsis:     Native byte vectors
Copyright:    Original Code is Copyright 1995-2011 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.


/////
///// BYTE-VECTOR
/////

define constant <byte-vector> = limited(<vector>, of: <byte>);

/// Fast byte vector copying

define function byte-vector-ref
    (byte-vector :: <byte-vector>, index :: <integer>)
  element(byte-vector, index)
end function byte-vector-ref;

define function byte-vector-ref-setter
    (value, byte-vector :: <byte-vector>, index :: <integer>)
  element(byte-vector, index) := value;
end function byte-vector-ref-setter;


define sealed method byte-vector-fill
    (target :: <byte-vector>, value :: <integer>,
     #key start :: <integer> = 0, end: last = size(target)) => ()
  let last :: <integer> = check-start-compute-end(target, start, last);
  primitive-fill-bytes!
    (target, primitive-repeated-slot-offset(target), integer-as-raw(start),
     integer-as-raw(last - start + 1), integer-as-raw(value));
end;

define sealed method byte-vector-fill
    (target :: <byte-vector>, value :: <byte-character>,
     #key start :: <integer> = 0, end: last = size(target)) => ()
  let last :: <integer> = check-start-compute-end(target, start, last);
  primitive-fill-bytes!
    (target, primitive-repeated-slot-offset(target), integer-as-raw(start),
     integer-as-raw(last - start + 1), integer-as-raw(as(<integer>, value)));
end method;

//---*** It would sure be nice to have low-level run-time support for this
define open generic copy-bytes (dst, dst-start, src, src-start, n) => ();

define open method copy-bytes
    (dst :: <sequence>, dst-start :: <integer>,
     src :: <sequence>, src-start :: <integer>, n :: <integer>)
 => ()
  for (i :: <integer> from 0 below n)
    dst[dst-start + i] := src[src-start + i]
  end
end method;

define open method copy-bytes
    (dst :: <vector>, dst-start :: <integer>,
     src :: <vector>, src-start :: <integer>, n :: <integer>)
 => ()
  for (i :: <integer> from 0 below n)
    dst[dst-start + i] := src[src-start + i]
  end
end method;

define open method copy-bytes
    (dst :: <string>, dst-start :: <integer>,
     src :: <string>, src-start :: <integer>, n :: <integer>)
 => ()
  for (i :: <integer> from 0 below n)
    dst[dst-start + i] := src[src-start + i]
  end
end method;

define open method copy-bytes
    (dst :: <string>, dst-start :: <integer>,
     src :: <vector>, src-start :: <integer>, n :: <integer>)
 => ()
  for (i :: <integer> from 0 below n)
    dst[dst-start + i] := as(<character>, src[src-start + i])
  end
end method;

define open method copy-bytes
    (dst :: <vector>, dst-start :: <integer>,
     src :: <string>, src-start :: <integer>, n :: <integer>)
 => ()
  for (i :: <integer> from 0 below n)
    dst[dst-start + i] := as(<integer>, src[src-start + i])
  end
end method;

define function copy-bytes-range-error
    (src, src-start :: <integer>, dst, dst-start :: <integer>, n :: <integer>)
 => ()
  error("SRC-START %d DST-START %d AND N %d OUTSIDE OF SRC %= AND DST %=",
        src-start, dst-start, n, src, dst);
end function;

define sealed method copy-bytes
    (dst :: <byte-vector>, dst-start :: <integer>,
     src :: <byte-vector>, src-start :: <integer>, n :: <integer>) => ()
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
    (dst :: <byte-string>, dst-start :: <integer>,
     src :: <byte-vector>, src-start :: <integer>, n :: <integer>) => ()
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
     src :: <byte-string>, src-start :: <integer>, n :: <integer>) => ()
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
    (dst :: <byte-string>, dst-start :: <integer>,
     src :: <byte-string>, src-start :: <integer>, n :: <integer>) => ()
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
     src :: <simple-object-vector>, src-start :: <integer>, n :: <integer>) => ()
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

define sealed method copy-bytes
    (dst :: <simple-object-vector>, dst-start :: <integer>,
     src :: <byte-vector>, src-start :: <integer>, n :: <integer>)
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

define open generic byte-storage-address
    (the-buffer)
 => (result-offset :: <machine-word>);

define open generic byte-storage-offset-address
    (the-buffer, data-offset :: <integer>)
 => (result-offset :: <machine-word>);

define constant <byte-vector-like> = type-union(<byte-string>, <byte-vector>);

define sealed inline method byte-storage-address
    (the-buffer :: <byte-vector-like>)
 => (result-offset :: <machine-word>)
      primitive-wrap-machine-word
        (primitive-repeated-slot-as-raw
           (the-buffer, primitive-repeated-slot-offset(the-buffer)))
end method;

define sealed inline method byte-storage-offset-address
    (the-buffer :: <byte-vector-like>, data-offset :: <integer>)
 => (result-offset :: <machine-word>)
  u%+(data-offset,
      primitive-wrap-machine-word
        (primitive-repeated-slot-as-raw
           (the-buffer, primitive-repeated-slot-offset(the-buffer))))
end method;
