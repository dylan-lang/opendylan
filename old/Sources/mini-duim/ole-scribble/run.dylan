Module:    scribble
Synopsis:  Run the Scribble application as an OLE part.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $OLE-class-ID :: <REFGUID> =
  make-GUID(#x18479fa0, #xeda5, #x11cf,
	    #x89, #xfc, #x02, #x07, #x01, #x19, #xf6, #x39);

define constant $validation-code = 10001; // arbitrary value to check version

define method save-frame-to-storage ( frame :: <scribble-frame>,
				     stream :: <LPSTREAM> ) => ();
  istream-write-integer(stream, $validation-code);
  let pane :: <drawing-pane> = frame.surface;
  let segments = pane.scribble-segments;
  istream-write-integer(stream, size(segments) );  
  for ( segment :: <vector> in segments )
    istream-write-integer(stream, size(segment) );  
    for ( coordinate :: <integer> in segment )
      istream-write-int16(stream, coordinate );
    end for;
  end for;
  istream-write-integer(stream, $validation-code);
  values()
end method;


define method load-frame-from-storage ( frame :: <scribble-frame>,
				       stream :: <LPSTREAM> ) => ();
  unless ( istream-read-integer(stream) = $validation-code )
    error( "stored data is incompatible version" );
  end unless;
  let num-segments = istream-read-integer(stream);
  let segments = make(<stretchy-vector>, size: num-segments);
  for ( i from 0 below num-segments )
    let seg-size = istream-read-integer(stream);
    let segment = make(<vector>, size: seg-size);
    for ( j from 0 below seg-size )
      segment[j] := istream-read-int16(stream);
    end for;
    segments[i] := segment;
  end for;
  unless ( istream-read-integer(stream) = $validation-code )
    error( "data out of sync" );
  end unless;
  let pane :: <drawing-pane> = frame.surface;
  pane.scribble-segments := segments;
  pane.scribble-segment := #f;
  values()
end method;


define method run-scribble ();
  let frame =
    make(<scribble-frame>, class-id: $OLE-class-ID, title: "Scribble");
  start-frame(frame)
end method;

// run-scribble();
