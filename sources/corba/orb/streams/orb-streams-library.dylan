Module: dylan-user
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library orb-streams
  use corba-dylan;
  use orb-utilities;
  export orb-streams;
end library;

define module orb-streams
  use corba-dylan;
  use orb-utilities;
  export
    <giop-message-error>,
    <marshalling-stream>,
    <marshalling-octet-stream>,
    <marshalling-buffer>,
    marshalling-stream-input-index, marshalling-stream-input-index-setter,
    marshalling-stream-output-index, marshalling-stream-output-index-setter,
    marshalling-stream-little-endian?, marshalling-stream-little-endian?-setter,
    marshalling-stream-buffer,
    marshalling-buffer-as-string,
    string-as-marshalling-buffer,
    set-buffer-size,
    align-input-stream,
    align-output-stream,
    with-output-alignment-constraint,
    with-input-alignment-constraint,
    with-typecode-input-alignment,
    with-typecode-output-alignment,
    with-little-endianness,
    with-marshalling-stream,
    with-marshalling-octet-stream,
    typecode-alignment-from-nesting,
    typecode-nesting-from-alignment;
end module;
