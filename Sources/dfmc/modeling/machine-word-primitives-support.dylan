Module:  dfmc-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *target-machine-word-mask* :: false-or(<abstract-integer>) = #f;

/// Compute the proper mask to extract a <machine-word> for the target system
/// from a <double-integer> on the local system.  We need to use this mask as the
/// target and local system word sizes may differ.
define function target-machine-word-mask () => (mask :: <abstract-integer>)
  *target-machine-word-mask*
  | begin
      local method build (n :: <integer>) => (mask :: <machine-word>)
	      iterate loop (i :: <integer> = n, m = coerce-integer-to-machine-word(0))
	        if (i = 0)
		  m
		else
		  loop(i - 1, machine-word-logior(machine-word-unsigned-shift-left(m, 8),
						  coerce-integer-to-machine-word(#xFF)))
		end
	      end
	    end method;
      let native-word-size = truncate/($machine-word-size, 8);
      *target-machine-word-mask* 
	:= make(<double-integer>, low:  build(min(native-word-size, word-size())),
				  high: build(max(0, word-size() - native-word-size)))
    end
end function target-machine-word-mask;

define variable *sign-extension-shift* :: false-or(<integer>) = #f;

/// Compute the shift count needed to sign extend a <machine-word> for the
/// target system in a <double-integer> on the local system.
define function sign-extension-shift () => (shift :: <integer>)
  *sign-extension-shift*
  | begin
      *sign-extension-shift* := 2 * $machine-word-size - 8 * word-size()
    end
end function sign-extension-shift;

/// Extracts the operand to a <machine-word> primitive as an <abstract-integer>
/// taking care to mask the value of the operand to the size of a <machine-word>
/// on the target system.
define inline-only function extract-mw-operand-unsigned (rx :: <&raw-machine-word>)
 => (x :: <abstract-integer>)
  select (rx by instance?)
    <&raw-integer> =>
      generic/logand(^raw-object-value(rx), target-machine-word-mask());
    <&raw-byte-character> =>
      // No need to mask as <byte-character>s are always positive and "small" ...
      as(<integer>, ^raw-object-value(rx));
    <&raw-machine-word> =>
      let x = ^raw-object-value(rx);
      select (x by instance?)
	<abstract-integer> =>
	  generic/logand(x, target-machine-word-mask());
	<byte-character> =>
	  // No need to mask as <byte-character>s are always positive and "small" ...
	  as(<integer>, x);
	<machine-word> => 
	  //---*** NOTE: Should be coerce-machine-word-to-unsigned-abstract-integer(x)
	  //---*** but the primitive that implements it is broken!
	  generic/logand(make(<double-integer>, 
			      low: x, high: coerce-integer-to-machine-word(0)),
			 target-machine-word-mask())
      end
  end
end function extract-mw-operand-unsigned;

define inline-only function sign-extend (x :: <abstract-integer>) => (x :: <abstract-integer>)
  generic/ash(generic/lsh(generic/logand(x, target-machine-word-mask()),
			  sign-extension-shift()),
	      - sign-extension-shift())
end function sign-extend;

/// Extracts the operand to a <machine-word> primitive as an <abstract-integer>
/// while ensuring that the operand's sign is properly extended.
define inline-only function extract-mw-operand-signed (rx :: <&raw-machine-word>)
 => (x :: <abstract-integer>)
  select (rx by instance?)
    <&raw-integer> =>
      ^raw-object-value(rx);
    <&raw-byte-character> =>
      as(<integer>, ^raw-object-value(rx));
    <&raw-machine-word> =>
      let x = ^raw-object-value(rx);
      select (x by instance?)
	<integer> =>
	  x;
	<abstract-integer> =>
	  // TODO: the compiler should figure this out, but it doesn't
	  let x :: <abstract-integer> = x;
	  sign-extend(x);
	<byte-character> =>
	  // No need to mask as <byte-character>s are always positive and "small" ...
	  as(<integer>, x);
	<machine-word> => 
	  //---*** NOTE: Should be coerce-machine-word-to-unsigned-abstract-integer(x)
	  //---*** but the primitive that implements it is broken!
	  sign-extend(make(<double-integer>,
			   low: x, high: coerce-integer-to-machine-word(0)));
      end
  end
end function extract-mw-operand-signed;

/// Identical to make-raw-literal but checks that the value will actually
/// fit into a <machine-word> on the target system for use by folders for
/// primitives which are supposed to signal overflow.
define inline-only function make-raw-literal-with-overflow (object :: <abstract-integer>)
 => (literal :: <&raw-machine-word>)
  if (instance?(object, <integer>))
    make-raw-literal(object)
  elseif (begin
	    let inverse-mask = generic/lognot(target-machine-word-mask());
	    let sign 
	      = generic/ash(generic/logand(object, inverse-mask), - sign-extension-shift());
	    let signed-result = sign-extend(object);
	    (sign = 0 & ~negative?(signed-result)) | (sign = -1 & negative?(signed-result))
	  end)
    make-raw-literal(object)
  else
    // Should cause the folder/optimizer to punt ...
    error("<double-integer> won't fit in <machine-word>")
  end
end function make-raw-literal-with-overflow;
