Module:       sockets-internals
Author:       Toby
Synopsis:     Internet address objects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

ignorable(%address-value-setter);

//  Wrappers for raw numeric internet addresses

define open abstract primary class <numeric-address> (<number>)
end class;

define sealed generic network-order (address :: <numeric-address>)
 => (network-order-address :: <object>);

define sealed generic host-order (address :: <numeric-address>)
 => (host-order-address :: <object>);

define open abstract primary class <ipv4-numeric-address> (<numeric-address>)
  slot %address-value :: <machine-word>, required-init-keyword: address:;
end class;

define sealed primary class 
    <ipv4-network-order-address> (<ipv4-numeric-address>)
end class;

define method network-order (the-address :: <ipv4-network-order-address>)
 => (result :: <machine-word>)
  the-address.%address-value
end method;

define method host-order (the-address :: <ipv4-network-order-address>)
 => (result :: <machine-word>)
  accessor-ntohl(the-address.%address-value)
end method;
 
define method as 
    (the-class == <string>, the-address :: <ipv4-numeric-address>)
 => (result :: <byte-string>)
  accessor-ipv4-address-to-presentation(the-address)
end method;

define sealed primary class 
    <ipv4-host-order-address> (<ipv4-numeric-address>)
end class;

define method network-order (the-address :: <ipv4-host-order-address>)
 => (result :: <machine-word>)
    accessor-htonl(the-address.%address-value)
end method;

define method host-order (the-address :: <ipv4-host-order-address>)
 => (result :: <machine-word>)
  the-address.%address-value
end method;

// <internet-address> class definition and initialization

define open primary class <internet-address> (<object>)
end class;

// For now just delegate to <ipv4-address>
define method make (class == <internet-address>, #rest initargs, #key)
 => (address-object :: <ipv4-address>)
  apply(make, <ipv4-address>, initargs)
end method make;

define open generic host-name 
    (the-address :: type-union(<internet-address>, <socket-condition>))
 => (the-name :: <object>);

define open generic host-address 
    (address-object :: type-union(<internet-address>, <socket-condition>))
 => (the-address :: <object>);

define open generic numeric-host-address (address-object :: <internet-address>)
 => (the-address :: <numeric-address>);

define open generic all-addresses (address-object :: <internet-address>)
 => (addresses :: <sequence>);

define open generic aliases (address-object :: <internet-address>)
 => (the-aliases :: <sequence>);

define sealed class <ipv4-address> (<internet-address>)
  slot %host-name :: false-or(<string>), init-value: #f;
  slot %host-address :: false-or(<ipv4-numeric-address>), init-value: #f;
  slot %aliases :: false-or(<sequence>), init-value: #f;
  slot %addresses :: false-or(<sequence>), init-value: #f;
  constant slot address-family :: <integer>, init-value: $AF-INET;
end class;

define method initialize
    (new-address :: <ipv4-address>, 
     #key address: 
       initialization-address :: 
         false-or(type-union(<string>, <ipv4-numeric-address>)) = #f,
     name: initialization-name :: false-or(<string>) = #f,
     aliases: initialization-aliases :: false-or(<sequence>) = #f,
     addresses: initialization-addresses :: false-or(<sequence>) = #f)
 => ()
  next-method();
  // Ok, here is the story.  If you give me an address: (or
  // addresses:) I will trust the rest of the information.  Any
  // missing information will be filled in lazily by the accessors.
  // In particular this means that if you give me a name which isn't
  // the canonical name the name might change to the canonical name
  // under your feet.  Tough.  If you give a name and no address then
  // the address, and everything else gets filled in now, eagerly, and
  // the information will come from the host_ent structure returned
  // from gethostbyname.

  // If the address: keyword is defined 
  if (initialization-address)
/* compiler doesn't like this code
    new-address.%host-address :=
      select (new-address by instance?)
	<ipv4-numeric-address> => initialization-address;
	<string> => 
	  accessor-ipv4-presentation-to-address(initialization-address);
      end select;
*/
    select (initialization-address by instance?)
      <ipv4-numeric-address> => 
	new-address.%host-address := initialization-address;
      <string> => 
	new-address.%host-address :=
	  accessor-ipv4-presentation-to-address(initialization-address);
    end select;

    //  Now just trust the rest. Maybe ought to check that the
    //  sequences are will formed.  Better might be to not document
    //  anything but the name: and address: initializers.  The rest
    //  are really for when you are making an <internet-address> from
    //  an existing <internet address>.
    new-address.%host-name := initialization-name;
    new-address.%aliases := initialization-aliases;
    new-address.%addresses := initialization-addresses;
  elseif (initialization-addresses)
    new-address.%host-name := initialization-name;
    new-address.%host-address := initialization-address;
    new-address.%aliases := initialization-aliases;
    new-address.%addresses := initialization-addresses;
  elseif (initialization-name)
    // Ignore all of the other initializers, if any, use the
    // information from the network.  Nyah-nyah.
    accessor-get-host-by-name(new-address, initialization-name);
  else 
    error("make(<ipv4-address>: address: or name: keyword must be supplied.");
  end if;
end method initialize;

// Virtual slot accessors

define method host-name
    (the-address :: <ipv4-address>) => (result :: <string>)
  unless (the-address.%host-name)
    accessor-get-host-by-address(the-address)
  end unless;
  the-address.%host-name
end method;
 
define sealed method host-address
    (the-address :: <ipv4-address>) => (result :: <string>)
  if (the-address.%host-address)
    accessor-ipv4-address-to-presentation(the-address.%host-address)
  elseif (the-address.%addresses)
    accessor-ipv4-address-to-presentation(the-address.%addresses.first)
  else
    error("internal error: <ipv4-address> doesn't have a valid host-address");
  end if
end method;

define sealed method numeric-host-address
    (the-address :: <ipv4-address>) => (result :: <ipv4-numeric-address>)
  if (the-address.%host-address)
    the-address.%host-address
  elseif (the-address.%addresses)
    the-address.%addresses.first
  else
    error("internal error: <ipv4-address> doesn't have a valid host-address");
  end if
end method;

define sealed method all-addresses
    (the-address :: <ipv4-address>) => (result :: <vector>)
  unless (the-address.%addresses)
    accessor-get-host-by-address(the-address)
  end unless;
  let numeric-addresses = the-address.%addresses;
  let vector-size = numeric-addresses.size;
  let result = 
    make(<simple-object-vector>, size: vector-size);
  // This is arguably unsafe since the various copied values might
  // conceivably be modified by somebody (and the modifications would
  // affect all the addresses.  Copying everything would be
  // painful however.  No way to make the collections read only.
  for (index from 0 below vector-size)
    result[index] :=
      make(<ipv4-address>, name: the-address.%host-name, 
	   address: numeric-addresses[index],
	   aliases: the-address.%aliases,
	   addresses: the-address.%addresses);
  end for;
  result
end method;

/*
define constant $loopback-address :: <ipv4-address> =
  make(<ipv4-address>, address: "127.0.0.1");

define constant $local-host-name :: <string> = accessor-local-host-name();

define constant $local-host :: <ipv4-address> =
  make(<ipv4-address>, name: $local-host-name);

*/

// Define these as variables instead of constants.  Put the
// initialization in the start-trap function.  Boo. Hiss.
define variable $loopback-address :: false-or(<ipv4-address>) = #f;

define variable $local-host-name :: <string> = "";

define variable $local-host :: false-or(<ipv4-address>) = #f;
