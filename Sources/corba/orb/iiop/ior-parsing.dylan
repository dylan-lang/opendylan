Module: orb-iiop
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $iiop-version = make(iiop/<version>, major: 1, minor: 0);
define constant $giop-version = make(giop/<version>, major: 1, minor: 0);
define constant $giop-magic = as(limited(corba/<array>, of: corba/<char>, dimensions: #[4]), "GIOP");

define method encapsulate-object (typecode :: <typecode>, object :: <object>, #key little-endian? = #t)
 => (encapsulation :: corba/<encapsulation>)
  with-marshalling-stream (buffer-stream, inner-stream: #f)
    let little-endian? :: <boolean> = little-endian?;
    with-little-endianness (buffer-stream, little-endian?)
      marshall(corba/$octet-typecode, if (little-endian?) 1 else 0 end if, buffer-stream);
      marshall(typecode, object, buffer-stream);
    end;
    copy-sequence(marshalling-stream-buffer(buffer-stream));
  end;
end method;

define method unencapsulate-object (typecode :: <typecode>, encapsulation :: corba/<encapsulation>)
 => (object :: <object>)
  with-marshalling-octet-stream(buffer-stream = encapsulation)
    let little-endian? = (unmarshall(corba/$octet-typecode, buffer-stream) ~= 0);
    with-little-endianness (buffer-stream, little-endian?)
      unmarshall(typecode, buffer-stream);
    end;
  end;
end method;

/// ---*** was make-ior-from-profile
define method profile-as-ior (type-id :: <string>, host :: <string>, port :: <integer>, key :: corba/<sequence>)
  make(iop/<ior>,
       type-id: type-id,
       profiles: begin
		   let type = limited(corba/<sequence>, of: iop/<taggedprofile>);
		   let profiles :: type = make(type);
		   profiles := add!(profiles, make(iop/<taggedprofile>,
						   tag: iop/$tag-internet-iop,
						   profile-data:
						     encapsulate-object(class-typecode(iiop/<ProfileBody-1-0>),
									make(iiop/<ProfileBody-1-0>,
									     iiop-version: $iiop-version,
									     host: host,
									     port: port,
									     object-key: key))));
		   profiles
		 end);
end method;
  
define method parse-ior (buffer :: <marshalling-buffer>)
 => (ior :: iop/<ior>)
  unencapsulate-object(class-typecode(iop/<ior>), buffer);
end method;

define method parse-ior-from-string (ior-string :: <string>)
  parse-ior(string-as-marshalling-buffer(copy-sequence(ior-string, start: 4)));
end method;

define method parse-ior-from-file (ior-file :: <string>)
  parse-ior(string-as-marshalling-buffer(copy-sequence(file-as-string(ior-file), start: 4)));
end method;

define method unparse-ior-to-file (ior :: iop/<ior>, ior-file :: <string>, #key little-endian? = #t)
  string-as-file(ior-file, unparse-ior(ior, little-endian?: little-endian?));
end method;

define method unparse-ior-to-string (ior :: iop/<ior>, #key little-endian? = #t)
  unparse-ior(ior, little-endian?: little-endian?);
end method;

define method unparse-ior (ior :: iop/<ior>, #key little-endian? = #t)
 => (stringified-ior :: <string>)
  concatenate("IOR:",
	      marshalling-buffer-as-string(encapsulate-object(class-typecode(iop/<ior>),
							      ior,
							      little-endian?: little-endian?)));
end method;

define constant $iiop-profile-cache :: <table> = make(<table>, weak: #"key");

define method get-iiop-profile (ior :: iop/<ior>)
 => (profile :: false-or(iiop/<profilebody-1-0>))
  element($iiop-profile-cache, ior, default: #f)
  | (element($iiop-profile-cache, ior) := compute-iiop-profile(ior));
end method;

define method compute-iiop-profile (ior :: iop/<ior>)
  block (return)
    for (profile in iop/ior/profiles(ior))
      if (iop/taggedprofile/tag(profile) = iop/$tag-internet-iop)
	return(unencapsulate-object(class-typecode(iiop/<profilebody-1-0>),

				    iop/taggedprofile/profile-data(profile)));
      end if;
    end for;
  end block;
end method;

// NB Moved shallow-copy(iop/<ior>) here from old iiop-protocol
// NB library because new iop-protocol library is auto generated.
// ---*** Perhaps there should be a generic shallow-copy method on corba/<struct>?
define sideways method shallow-copy (object :: iop/<ior>)
 => (copy :: iop/<ior>)
  make(iop/<ior>,
       type-id: iop/ior/type-id(object),
       profiles: iop/ior/profiles(object));
end method;

define sealed class iiop/<location> (corba/<struct>)
  slot iiop/location/repository-id :: <string>, required-init-keyword: repository-id:;
  slot iiop/location/host :: <string>, required-init-keyword: host:;
  slot iiop/location/port :: <integer>, required-init-keyword: port:;
  slot iiop/location/adaptor-path :: limited(<sequence>, of: <string>), required-init-keyword: adaptor-path:;
  slot iiop/location/objectid :: <string>, required-init-keyword: objectid:;
end class;

define sealed domain make (singleton(iiop/<location>));
define sealed domain initialize (iiop/<location>);

