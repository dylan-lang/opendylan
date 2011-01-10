Module:    scribble
Author:    Scott McKay and David Gray
Synopsis:  Simple OLE control (OCX) scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Persistent storage for the Scribble app as an OLE control (OCX)

// Arbitrary value to check version
define constant $validation-code :: <integer> = 10001;

define method save-frame-to-storage
    (frame :: <scribble-frame>, stream :: <storage-istream>) => ()
  let pane :: <drawing-pane> = frame.surface;
  let segments = pane.scribble-segments;
  istream-write-integer(stream, $validation-code);
  istream-write-integer(stream, size(segments));  
  for (segment :: <vector> in segments)
    istream-write-integer(stream, size(segment));  
    for (coordinate :: <integer> in segment)
      istream-write-int16(stream, coordinate);
    end
  end;
  istream-write-integer(stream, $validation-code)
end method save-frame-to-storage;

define method load-frame-from-storage
    (frame :: <scribble-frame>, stream :: <storage-istream>) => ()
  assert(istream-read-integer(stream) = $validation-code,
	 "Stored data has an incompatible version");
  let sheet :: <scribble-pane> = frame.surface;
  sheet.scribble-segments.size := 0;
  let n-segments :: <integer> = istream-read-integer(stream);
  for (i :: <integer> from 0 below n-segments)
    let seg-size :: <integer> = istream-read-integer(stream);
    let segment = make(<vector>, size: seg-size);
    for (j from 0 below seg-size)
      segment[j] := istream-read-int16(stream)
    end;
    add!(sheet.scribble-segments, segment);
  end;
  sheet.scribble-segment  := #f;
  assert(istream-read-integer(stream) = $validation-code,
	 "Store data is out of sync");
end method load-frame-from-storage;
