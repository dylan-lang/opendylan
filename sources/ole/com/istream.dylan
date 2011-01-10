Module:    COM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Abstract superclass of both client-side and server-side IStream objects.
define C-subtype <storage-istream> (<Interface>, <positionable-stream>) end;

// Client-side IStream object.  Server-side is defined in com-intf.
define sealed C-subtype <LPSTREAM> (<C-Interface>, <storage-istream>) end;
define sealed domain make(singleton(<LPSTREAM>));
define sealed domain initialize(<LPSTREAM>);

define constant $null-istream :: <LPSTREAM> = null-pointer(<LPSTREAM>);


// Implementation of stream protocol.  Wonder if it's worth copying it down
// for client-side access.

define sealed method stream-size (istream :: <storage-istream>)
 => (size :: <integer>)
  with-stack-structure (st :: <LPSTATSTG>)
    let status = IStream/Stat(istream, st, $statflag-noname);
    check-ole-status(status, "IStream/Stat", istream);
    st.cbSize-value.pointer-value
  end with-stack-structure;
end stream-size;

define sealed method adjust-stream-position (istream :: <storage-istream>,
					     delta :: <integer>,
					     #key from = #"current")
 => (pos :: <abstract-integer>)
  let origin = select (from)
		 #"current" => $stream-seek-cur;
		 #"start" => $stream-seek-set;
		 #"end" => $stream-seek-end;
	       end;
  with-stack-structure (req :: <plarge-integer>)
    with-stack-structure (new :: <pularge-integer>)
      req.pointer-value := delta;
      let status = IStream/Seek(istream, req, origin, new);
      check-ole-status(status, "IStream/Seek", istream);
      new.pointer-value;
    end with-stack-structure;
  end with-stack-structure;
end adjust-stream-position;
  
define sealed inline method stream-position (istream :: <storage-istream>)
 => (pos :: <abstract-integer>)
  adjust-stream-position(istream, 0, from: #"current")
end;

define sealed inline method stream-position-setter
    (pos, istream :: <storage-istream>) => (pos)
  if (member?(pos, #(#"start", #"current", #"end")))
    adjust-stream-position(istream, 0, from: pos);
  else
    adjust-stream-position(istream, pos, from: #"start");
  end;
  pos
end;

define sealed method read-element (istream :: <storage-istream>,
				   #key on-end-of-stream = unsupplied())
 => (ch-or-default)
  with-stack-structure (c-string :: <C-string>, size: 3)
    let (status, count) = IStream/Read(istream, c-string, 1);
    if (count == 1)
      c-string[0];
    else
      do-on-end-of-stream(istream, status, on-end-of-stream, on-end-of-stream);
    end;
  end with-stack-structure;
end read-element;

define sealed method peek (istream :: <storage-istream>,
			   #key on-end-of-stream = unsupplied())
 => (ch-or-default)
  with-stack-structure (c-string :: <C-string>, size: 3)
    let (status, count) = IStream/Read(istream, c-string, 1);
    if (count == 1)
      adjust-stream-position(istream, -1);
      c-string[0];
    else
      do-on-end-of-stream(istream, status, on-end-of-stream, on-end-of-stream);
    end;
  end with-stack-structure;
end peek;

define sealed inline method unread-element
    (istream :: <storage-istream>, ch) => (ch)
  adjust-stream-position(istream, -1);
  ch
end;

define sealed method read (istream :: <storage-istream>, n :: <integer>,
			   #key on-end-of-stream = unsupplied())
  => (string-or-eof);
  with-stack-structure (c-string :: <C-string>, size: n)
    let (status, count) = IStream/Read(istream, c-string, n);
    let str = make(<byte-string>, size: count);
    for (i from 0 below count) str[i] := c-string[i] end;
    if (count == n)
      str
    elseif (count > 0 & unsupplied?(on-end-of-stream))
      signal(make(<incomplete-read-error>,
		  stream: istream, sequence: str, count: count));
    else
      do-on-end-of-stream(istream, status, on-end-of-stream, str);
    end;
  end with-stack-structure;
end read;

define sealed method read-into! (istream :: <storage-istream>,
				 n :: <integer>,
				 str :: <byte-string>,
				 #key start ::<integer> = 0,
				      on-end-of-stream = unsupplied())
  => (n-read-or-eof);
  let start = min(start, str.size);
  let n = min(n, str.size - start);
  with-c-string (c-string = str)
    let ptr = pointer-value-address(c-string, index: start);
    let (status, count) = IStream/Read(istream, ptr, n);
    if (count == n)
      count
    elseif (count > 0 & unsupplied?(on-end-of-stream))
      signal(make(<incomplete-read-error>,
		  stream: istream, sequence: str, count: count));
    else
      do-on-end-of-stream(istream, status, on-end-of-stream, count);
    end;
  end with-c-string;
end read-into!;

define sealed method read-into! (istream :: <storage-istream>,
				 n :: <integer>,
				 seq :: <mutable-sequence>,
				 #key start :: <integer> = 0,
				      on-end-of-stream = unsupplied())
  => (n-read-or-eof);
  if (subtype?(<byte-character>, collection-element-type(seq)))
    character-read-into!(istream, n, seq, start, on-end-of-stream);
  else
    // Handle <buffer>'s
    integer-read-into!(istream, n, seq, start, on-end-of-stream);
  end;
end;

define function character-read-into! (istream :: <storage-istream>,
				      n :: <integer>,
				      seq :: <mutable-sequence>,
				      start :: <integer>,
				      on-end-of-stream)
  let start = min(start, seq.size);
  let n = min(n, seq.size - start);
  with-stack-structure (c-string :: <C-string>, size: n)
    let (status, count) = IStream/Read(istream, c-string, n);
    for (i from 0 below count)
      seq[start + i] := c-string[i]
    end;
    if (count == n)
      count
    elseif (count > 0 & unsupplied?(on-end-of-stream))
      signal(make(<incomplete-read-error>,
		  stream: istream, sequence: seq, count: count));
    else
      do-on-end-of-stream(istream, status, on-end-of-stream, count);
    end;
  end with-stack-structure;
end character-read-into!;

define function integer-read-into! (istream :: <storage-istream>,
				    n :: <integer>,
				    seq :: <mutable-sequence>,
				    start :: <integer>,
				    on-end-of-stream)
  let start = min(start, seq.size);
  let n = min(n, seq.size - start);
  with-stack-structure (c-string :: <C-string>, size: n)
    let (status, count) = IStream/Read(istream, c-string, n);
    for (i from 0 below count)
      seq[start + i] := as(<integer>, c-string[i])
    end;
    if (count == n)
      count
    elseif (count > 0 & unsupplied?(on-end-of-stream))
      signal(make(<incomplete-read-error>,
		  stream: istream, sequence: seq, count: count));
    else
      do-on-end-of-stream(istream, status, on-end-of-stream, count);
    end;
  end with-stack-structure;
end integer-read-into!;

define inline function do-on-end-of-stream (istream, status, on-end-of-stream, val)
  // MS doc says implementations are allowed to return an error on eof, so
  // we have no way to distinguish eof from random I/O error... How wise.
  // if (FAILED?(status))  ole-error(status, "read-element", s.ptr)
  if (supplied?(on-end-of-stream))
    // Functional Developer doc says to return on-end-of-stream but none of the
    // other streams do that, so we don't either...
    val    //on-end-of-stream
  else
    signal(make(<end-of-stream-error>, stream: istream));
  end;
end;


define sealed method write-element (istream :: <storage-istream>,
				    ch :: <byte-character>) => ();
  with-stack-structure (c-string :: <C-string>, size: 3)
    c-string[0] := ch;
    let (status, count) = IStream/Write(istream, c-string, 1);
    unless (count == 1)
      ole-error(status, "IStream/Write", istream);
    end;
  end with-stack-structure;
end write-element;

define sealed method write (istream :: <storage-istream>, str :: <byte-string>,
			    #key start, end: end-pos) => ();
  let str-size = str.size;
  let start :: <integer> = min(str-size, start | 0);
  let n :: <integer> = max(0, (end-pos | str-size) - start);
  with-c-string (c-string = str)
    let ptr = pointer-value-address(c-string, index: start);
    let (status, count) = IStream/Write(istream, ptr, n);
    unless (count == n)
      ole-error(status, "IStream/Write", istream);
    end;
  end;
end write;

define sealed method write (istream :: <storage-istream>, seq :: <sequence>,
			    #key start, end: end-pos) => ();
  let str-size = seq.size;
  let start :: <integer> = min(str-size, start | 0);
  let n :: <integer> = max(0, (end-pos | str-size) - start);
  with-stack-structure (c-string :: <C-string>, size: n)
    for (i from 0 below n)
      c-string[i] := as(<character>, seq[start + i])
    end;
    let (status, count) = IStream/Write(istream, c-string, n);
    unless (count == n)
      ole-error(status, "IStream/Write", istream);
    end;
  end;
end write;
