Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *dood-dump?* = #F;

define variable *dood-debug?* = #f;

define method debug ()
  *dood-debug?* := #T; *dood-dump?*  := #T;
end method;

define method no-debug ()
  *dood-debug?* := #F; *dood-dump?*  := #F;
end method;

define constant $zero = list("ZERO");

define method dump-at (dood :: <dood>, address :: <address>) => (next-address)
  format-out("%d:\t", address);
  block () 
    let value = dood-read-at(dood, address);
    let value-at = dood-object(dood, address, default: #f);
    if (instance?(value-at, <byte-string>) | instance?(value-at, <symbol>))
      format-out("%=\n", value-at);
      address + dood-instance-size(dood, value-at)
    else
      case 
	$tag-pairs? & pair?(value) | address?(value)        => 
	  let address = untag(value);
	  let object  = dood-object(dood, address, default: $zero);
	  if ($tag-pairs? & pair?(value))
  	    format-out("!@%d", address); 
	  else
  	    format-out("@%d", address); 
	  end if;
	  unless (object == $zero)
	    format-out(" -> %=", object); 
	  end unless;
	integer?(value)        => 
	  format-out(" %d", untag(value));
	byte-character?(value) => 
	  format-out("'%s'", as(<character>, untag(value)));
      end case;
      format-out("\n");
      address + 1;
    end if
  exception (<error>)
    address + 1;
  end block;
end method;

define method dump-range 
    (dood :: <dood>, from-address :: <address>, below-address :: <address>)
  let old-dood-debug? = *dood-debug?*;
  *dood-debug?* := #f;
  block ()
    dynamic-bind (*print-length* = 5)
    dynamic-bind (*print-level* = 2)
      iterate loop (address = from-address)
	if (address < below-address)
	 loop(dump-at(dood, address))
	end if;
      end iterate;
    end dynamic-bind;
    end dynamic-bind;
  cleanup
    *dood-debug?* := old-dood-debug?;
  end block;
end method;

define method dump (dood :: <dood>)
  dump-range(dood, $dood-free-address-id, $dood-predefines-begin);
  dump-range(dood, $dood-predefines-begin, dood-free-address(dood));
  values(); 
end method;
