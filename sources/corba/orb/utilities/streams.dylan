Module: orb-utilities
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method file-as-string (ior-file :: <string>)
  with-open-file(stream = ior-file, direction: #"input")
    as(<string>, read-to-end(stream));
  end;
end method;

define method string-as-file (ior-file :: <string>, ior-string :: <string>)
  with-open-file(stream = ior-file, direction: #"output")
    write(stream, ior-string);
  end;
end method;

define open generic force-input (stream :: <stream>, #key)
 => ();

define open generic read-bytes
    (stream :: <stream>, no-of-elements :: dylan/<integer>,
     #key on-end-of-stream,
     signed? :: <boolean> = #f)
 => (result);

define open generic read-signed-bytes
    (stream :: <stream>, no-of-elements :: dylan/<integer>, #key on-end-of-stream)
 => (result);

define open generic read-unsigned-bytes
    (stream :: <stream>, no-of-elements :: dylan/<integer>, #key on-end-of-stream)
 => (result);

define open generic write-bytes
    (stream :: <stream>, object :: <object>, no-of-elements :: dylan/<integer>)
 => ();
