Module: inet-address-internals
hSynopsis: Models an internet style address.
Author: James Casey
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// XXX - I want host-name to be a required keyword.

define class <inet-address> (<object>)
  slot %host-name :: false-or(<byte-string>), init-value: #f;
  virtual slot host-name :: <byte-string>, init-value: "";
  slot address :: <byte-vector> , init-value: *unknown-address-vector*;
  virtual slot host-address :: <byte-string>; // dotted decimal string
  slot host-aliases :: limited(<vector>, of: <byte-string>);
  constant slot address-family :: <integer>, init-value: $AF-INET;
end class <inet-address>;

define method initialize(this :: <inet-address>, #key 
			   address: the-address :: false-or(<byte-vector>),
			   host-name: the-host-name :: false-or(<byte-string>)) 
 =>(retval :: <inet-address>)
  next-method();
  
  if (the-address)
    this.address := the-address;
    if (the-host-name) // If defined, set slot to it. Otherwise we calculate 
                       // on slot access
      this.%host-name := the-host-name
    end;
  else // ~ address
    if (the-host-name)
      this.%host-name := the-host-name;
      this.address := as(<byte-vector>,
			  get-host-addr-by-name(this.host-name));
    end;
  end if; // address
  this;
end method initialize;

// Virtual slot accessors
//
define method host-name(this :: <inet-address>)
 =>(retval :: <byte-string>)
  if (this.%host-name ~= #f)
    this.%host-name
  else
    block ()
      let name = get-host-name-by-addr(this.address);
      this.%host-name := name;
    exception(c :: <unknown-host-error>)
      this.%host-name := this.host-address
    end block;
  end if;
end method host-name;
 
define method host-address(this :: <inet-address>) 
 => (retval :: <byte-string>)
  let addr :: <byte-vector> = this.address;
  format-to-string("%=.%=.%=.%=",
		   addr[0], addr[1],
		   addr[2], addr[3]);
end method host-address;

// Constructor Function Definitions
//
define function inet-address-by-host-name(this :: <byte-string>)
 => (retval :: <inet-address>)
  
 /*  
  if (empty?(this))
    *loopback-host*;
  else
*/
   make(<inet-address>, host-name: this);
//  end;
end function inet-address-by-host-name;
  
define function any-local-address() 
 => (retval :: <inet-address>)
  *loopback-host*;
end function any-local-address;

//Define some globals

// Spin up a vector for the IP address of the loopback address
//
define function loopback-vector() 
 => (retval :: <byte-vector>);
  let bytes :: <byte-vector> = make(<byte-vector>, size: 4, fill: 0);
  bytes[0] := #x7F;
  bytes[1] := #x00;
  bytes[2] := #x00;
  bytes[3] := #x01;
  bytes;
end method loopback-vector;

define constant *unknown-address-vector* =  make(<byte-vector>, size: 4, fill: 0);

// Internal variable
//
define constant *loopback-host* :: <inet-address> =
  make(<inet-address>, host-name: as(<byte-string>,#"localhost"),
       address: loopback-vector());

define constant *local-host* :: <inet-address> =
  block()
    inet-address-by-host-name(local-host-name());
  exception(<unknown-host-error>)
    *loopback-host*
  end;

define function as-hostname(v :: <byte-vector>)
 =>( retval :: <byte-string>)
  format-to-string("%d.%d.%d.%d",v[0],v[1],v[2],v[3]);
end as-hostname;
    
define constant *any-local-address* :: <inet-address> =
  make(<inet-address>, host-name: as-hostname($ADDR-ANY),
       address: $ADDR-ANY);