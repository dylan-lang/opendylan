Module: orb-streams
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <MARSHALLING-OCTET-STREAM>

define class <marshalling-octet-stream> (<marshalling-stream>) 
end class;

define method align-input-stream (stream :: <marshalling-octet-stream>, alignment :: <integer>)
  let index = marshalling-stream-input-index(stream);
  let pseudo-index = index - input-marshalling-alignment-constraint();
  let alignment-diff = modulo(pseudo-index, alignment);
  unless (alignment-diff = 0)
    marshalling-stream-input-index(stream) := index + (alignment - alignment-diff);
  end unless;
end method;

define method read-element (stream :: <marshalling-octet-stream>, #key on-end-of-stream = unsupplied())
 => (byte)
  let buffer = marshalling-stream-buffer(stream);
  let index = marshalling-stream-input-index(stream);
  let elt = buffer[index];
  marshalling-stream-input-index(stream) := index + 1;
  elt;
end method;
