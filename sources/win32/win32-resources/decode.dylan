Module:    win32-resources-internal
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Decoding

define sealed method decode-resource
    (raw-id :: <raw-resource-id>)
 => (resource-id :: type-union(<unsigned-int>, <string>))
  let value = pointer-address(raw-id);
  if (zero?(%logand(as(<machine-word>, #xFFFF0000), value)))
    as(<integer>, value)
  else
    as(<byte-string>, raw-id)
  end
end method decode-resource;


/// Encoding

define sealed method encode-resource
    (resource-id :: <unsigned-int>) => (raw-id :: <raw-resource-id>)
  MAKEINTRESOURCE(resource-id)
end method encode-resource;

define sealed method encode-resource
    (resource-id :: <C-string>) => (raw-id :: <raw-resource-id>)
  as(<raw-resource-id>, resource-id)		// this should work
end method encode-resource;

define sealed method encode-resource
    (resource-id :: <C-unicode-string>) => (raw-id :: <raw-resource-id>)
  as(<raw-resource-id>, resource-id)		// this should work
end method encode-resource;

define sealed method encode-resource
    (resource-id :: <byte-string>) => (raw-id :: <raw-resource-id>)
  as(<raw-resource-id>, as(<C-string>, resource-id))
end method encode-resource;


/// Describing

define generic print-resource-id
    (id :: <resource-id>, name :: <string>) => ();

define sealed method print-resource-id
    (id :: <unsigned-int>, name :: <byte-string>) => ()
  if (id = 0)
    format-out("No %s resource present\n", name)
  else
    format-out("%s resource id = %d\n", name, id)
  end
end method print-resource-id;

define sealed method print-resource-id
    (id :: <string>, name :: <byte-string>) => ()
  format-out("%s resource id = %s\n", name, as(<byte-string>, id))
end method print-resource-id;

define sealed method print-resource-id
    (id :: <raw-resource-id>, name :: <byte-string>) => ()
  print-resource-id(decode-resource(id), name)
end method print-resource-id;


define generic print-resource-id-to-string
    (id :: <resource-id>, name :: <string>) => ();

define sealed method print-resource-id-to-string
    (id :: <unsigned-int>, name :: <byte-string>) => ()
  if (id = 0)
    format-to-string("No %s resource present\n", name)
  else
    format-to-string("%s resource id = %d\n", name, id)
  end
end method print-resource-id-to-string;

define sealed method print-resource-id-to-string
    (id :: <string>, name :: <byte-string>) => ()
  format-to-string("%s resource id = %s\n", name, as(<byte-string>, id))
end method print-resource-id-to-string;

define sealed method print-resource-id-to-string
    (id :: <raw-resource-id>, name :: <byte-string>) => ()
  print-resource-id-to-string(decode-resource(id), name)
end method print-resource-id-to-string;
