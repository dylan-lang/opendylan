Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library orb-utilities
  use corba-dylan;
  export orb-utilities;
end library;

define module orb-utilities
  use corba-dylan;

  create
    file-as-string,
    string-as-file,
    force-input,
    read-bytes,
    read-signed-bytes,
    read-unsigned-bytes,
    write-bytes;

  create
    generate-name,
    architecture-little-endian?, architecture-little-endian?-setter,
    \with-architecture-little-endian?,
    invoke-architecture-little-endian,
    operating-system-user;

  create
    <mailbox>;

  create
    <ticket>,
    <boolean-ticket>,
    <integer-ticket>,
    valid-ticket?,
    initial-ticket,
    reissue-ticket,
    invalid-ticket;
end module;
