Module:    dfmc-application
Synopsis:  application server memory and register browsing.
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// NUMERICAL-BASE-DESCRIPTION
//    Number bases are described in the protocols layer as symbols. This
//    function translates the symbols to their actual number base.

define function numerical-base-description
   (format :: <data-display-format>) => (base :: <integer>)
  select(format)
    #"binary"       => 2;
    #"octal"        => 8;
    #"decimal"      => 10;
    #"hexadecimal"  => 16;
    otherwise       => 10
  end select
end function numerical-base-description;


///// INDEX-ACCORDING-TO-SIZE (Internal Function)
//    Adds an offset to a base address, where the offset is calculated by
//    multiplying an abstract index 'i' by the size of a data object.
//    (ie. Produces an array reference).
//    TODO: This function is a bit low-level for this layer of the
//          architecture. It, or something similar, should probably be
//          made available via the access-path library.

define function index-according-to-size
    (base-addr :: <remote-value>, i :: <integer>, size :: <data-display-size>)
 => (addr :: <remote-value>)
  select(size)
    #"word" => indexed-remote-value(base-addr, i);
    #"byte" => byte-indexed-remote-value(base-addr, i);

    // TODO: The following are all unmodular and platform-specific.
    //       The access-path library MUST be extended with appropriate
    //       abstractions (ie. some kind of "sizeof").

    // Assumes that #"float" is the same size as #"word"
    #"float" => indexed-remote-value(base-addr, i);

    // Assumes that #"double" is twice the size of #"float"
    #"double" => indexed-remote-value(base-addr, 2 * i);

    // Assumes that #"short" is twice the size of #"byte"
    #"short" => byte-indexed-remote-value(base-addr, 2 * i);

    // Assumes that #"long" is the same size as #"word"
    #"long" => indexed-remote-value(base-addr, i);

    // Assumes that #"hyper" is twice the size of #"word"
    #"hyper" => indexed-remote-value(base-addr, 2 * i);
  end select;
end function index-according-to-size;


///// ZERO-PAD!
//    The library function FORMAT-TO-STRING always pads with spaces when
//    formatting numerical fields to a specified width. This is not
//    desirable for the debugger, where standard practice is to use
//    zero-padding. This function simply strides through a string from
//    the left, and replaces leading spaces (only) with zero's.

define function zero-pad! (s :: <string>) => (s :: <string>)
  if (empty?(s))
    s
  else
    let i :: <integer> = 0;
    while (s[i] == ' ')
      s[i] := '0';
      i := i + 1;
    end while;
    s
  end if;
end function zero-pad!;


///// ADDRESS-TO-STRING (Environment Protocols)

define method address-to-string
    (application :: <dfmc-application>, address :: <address-object>,
     #key format :: <address-display-format> = #"hexadecimal")
  => (representation :: <string>)
  let target = application.application-target-app;
  let path = target.debug-target-access-path;
  let value = address.application-object-proxy;

  // The access-path can perform this transformation for us.
  with-debugger-transaction (target)
    remote-value-as-string(path, value, numerical-base-description(format))
  end
end method address-to-string;


///// STRING-TO-ADDRESS (Environment Protocols)

define method string-to-address
    (application :: <dfmc-application>, representation :: <string>,
     #key format :: <address-display-format> = #"hexadecimal")
  => (address :: <address-object>)
  let target = application.application-target-app;
  let path = target.debug-target-access-path;
  let address = $invalid-address-object;

  // The access-path can perform this transformation for us.
  with-debugger-transaction (target)
    let value 
      = string-as-remote-value(path, representation,
			       numerical-base-description(format));
    make-environment-object(<address-object>,
			    project: application.server-project,
			    application-object-proxy: value)
  end
end method string-to-address;


///// ADDRESS-APPLICATION-OBJECT (Environment Protocols)

define method address-application-object
    (application :: <dfmc-application>, address :: <address-object>)
  => (obj :: <application-object>)
  let target = application.application-target-app;
  let obj = address;
  with-debugger-transaction (target)
    let value = address.application-object-proxy;
    make-environment-object-for-runtime-value
      (application, value, address?: #t)
  end
end method address-application-object;


///// APPLICATION-OBJECT-ADDRESS (Environment Protocols)

define method application-object-address
    (application :: <dfmc-application>, obj :: <application-object>)
 => (address :: false-or(<address-object>))
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let proxy = obj.application-object-proxy;
    if (proxy)
      let value = runtime-proxy-to-remote-value(application, proxy);
      make-environment-object(<address-object>,
			      project: application.server-project,
			      application-object-proxy: value)
    end
  end
end method application-object-address;


///// INDIRECT-ADDRESS (Environment Protocols)

define method indirect-address
    (application :: <dfmc-application>, address :: <address-object>)
 => (i-address :: <address-object>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    block ()
      let raw-addr = address.application-object-proxy;
      let path = target.debug-target-access-path;
      let i-value = read-value(path, raw-addr);
      make-environment-object
	(<address-object>,
	 project: application.server-project,
	 application-object-proxy: i-value);
    exception(<remote-access-violation-error>)
      $invalid-address-object
    end block;
  end
end method indirect-address;


///// INDEXED-ADDRESS (Environment Protocols)

define method indexed-address
    (application :: <dfmc-application>, addr :: <address-object>,
     i :: <integer>,
     #key size :: <data-display-size> = #"word")
 => (i-addr :: <address-object>)
  let base-addr = addr.application-object-proxy;
  let indexed-addr =
    index-according-to-size(base-addr, i, size);
  make-environment-object(<address-object>,
                          project: application.server-project,
                          application-object-proxy: indexed-addr)
end method indexed-address;


///// ADDRESS-READ-APPLICATION-OBJECT (Environment Protocols)

define method address-read-application-object
    (application :: <dfmc-application>, address :: <address-object>)
 => (obj :: false-or(<application-object>))
  let target = application.application-target-app;
  with-debugger-transaction (target)
    block ()
      let raw-addr = address.application-object-proxy;
      let val = read-dylan-value(target, raw-addr);
      make-environment-object-for-runtime-value
	(application, val, address?: #t);
    exception(<remote-access-violation-error>)
      #f
    end
  end
end method address-read-application-object;


///// ADDRESS-READ-MEMORY-CONTENTS (Environment Protocols)
//    TODO: This method needs improvement. It's currently just based on
//          similar hacks in the console debugger's DISPLAY command.

define method address-read-memory-contents
    (application :: <dfmc-application>, address :: <address-object>,
     #key size :: <data-display-size> = #"word",
          format :: <data-display-format> = #"hexadecimal",
          from-index :: <integer> = 0,
          to-index :: <integer> = 7)
 => (printable-strings :: <sequence>, next-address :: <address-object>)
  let target = application.application-target-app;
  let sz = to-index - from-index + 1;
  let printable-strings = make(<vector>, size: sz, fill: "");
  with-debugger-transaction (target)
    let path = target.debug-target-access-path;
    let base-addr = address.application-object-proxy;
    let next-addr
      = select(size)
	  #"byte" =>
	    let next-addr = byte-indexed-remote-value(base-addr, to-index + 1);
	    let target-addr = byte-indexed-remote-value(base-addr, from-index);
	    let str
	      = block ()
		  read-byte-string(path, target-addr, sz)
		exception(<remote-access-violation-error>)
		  ""
		end;
	    for (x in as(<byte-vector>, str),
		 i from 0)
	      printable-strings[i]
		:= select(format)
		     #"hexadecimal"    => zero-pad!(format-to-string("%2x", x));
		     #"octal"          => zero-pad!(format-to-string("%3o", x));
		     #"decimal"        => zero-pad!(format-to-string("%3d", x));
		     #"byte-character" => format-to-string("%c", str[i]);
		     otherwise         => zero-pad!(format-to-string("%2x", x));
		   end;
	    end;
	    next-addr;
	  #"word" =>
	    let j = 0;
	    let next-addr = indexed-remote-value(base-addr, to-index + 1);
	    for (i from from-index to to-index)
	      let target-addr = indexed-remote-value(base-addr, i);
	      block()
		let content = read-value(path, target-addr);
		printable-strings[j]
		  := remote-value-as-string(path, content,
					    numerical-base-description(format));
	      exception(<remote-access-violation-error>)
		printable-strings[j] := "????????"
	      end block;
	      j := j + 1;
	    end for;
	    next-addr;
	  #"float" =>
	    let j = 0;
	    let next-addr = index-according-to-size(base-addr, to-index + 1, size);
	    for (i from from-index to to-index)
	      let target-addr = index-according-to-size(base-addr, i, size);
	      block()
		let content = read-single-float(path, target-addr);
		printable-strings[j] := format-to-string("%=", content);
	      exception(<remote-access-violation-error>)
		printable-strings[j] := "????????????";
	      end block;
	      j := j + 1;
	    end for;
	    next-addr;
	  #"double" =>
	    let j = 0;
	    let next-addr = index-according-to-size(base-addr, to-index + 1, size);
	    for (i from from-index to to-index)
	      let target-addr = index-according-to-size(base-addr, i, size);
	      block()
		let content = read-double-float(path, target-addr);
		printable-strings[j] := format-to-string("%=", content);
	      exception(<remote-access-violation-error>)
		printable-strings[j] := "????????????";
	      end block;
	      j := j + 1;
	    end for;
	    next-addr;
	  otherwise =>
	    // TODO: Implement other cases.
	    #f;
	end select;
    values(printable-strings,
	   if (next-addr)
	     make-environment-object(<address-object>,
				     project: application.server-project,
				     application-object-proxy: next-addr)
	   else
	     $invalid-address-object
	   end)
  end
end method address-read-memory-contents;
