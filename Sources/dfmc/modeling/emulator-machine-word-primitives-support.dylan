Module:  dfmc-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// The emulator does all its <integer> arithmetic using Lisp bignums which provide
/// arbitrary precision and, consequently, avoid all problems w.r.t. losing the 
/// high-order bits of folded constants.

/// Only needed because its referenced by make-raw-literal(<double-integer>)
define function target-machine-word-mask () => (mask :: <abstract-integer>)
  -1
end function target-machine-word-mask;

/// Extracts the operand to a <machine-word> primitive as an <abstract-integer>
/// taking care to mask the value of the operand to the size of a <machine-word>
/// on the target system.
define inline-only function extract-mw-operand-unsigned (rx :: <&raw-machine-word>)
 => (x :: <abstract-integer>)
  select (rx by instance?)
    <&raw-integer> =>
      ^raw-object-value(rx);
    <&raw-byte-character> =>
      // No need to mask as <byte-character>s are always positive and "small" ...
      as(<integer>, ^raw-object-value(rx));
    <&raw-machine-word> =>
      let x = ^raw-object-value(rx);
      select (x by instance?)
	<abstract-integer> =>
	  x;
	<byte-character> =>
	  // No need to mask as <byte-character>s are always positive and "small" ...
	  as(<integer>, x);
	<machine-word> => 
	  //---*** DO THESE HAPPEN IN THE EMULATOR????
	  error("Huh?")
      end
  end
end function extract-mw-operand-unsigned;

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
	<byte-character> =>
	  // No need to mask as <byte-character>s are always positive and "small" ...
	  as(<integer>, x);
	<machine-word> => 
	  //---*** DO THESE HAPPEN IN THE EMULATOR????
	  error("Huh?")
      end
  end
end function extract-mw-operand-signed;

/// Identical to make-raw-literal but checks that the value will actually
/// fit into a <machine-word> on the target system for use by folders for
/// primitives which are supposed to signal overflow.
define inline-only function make-raw-literal-with-overflow (object :: <abstract-integer>)
 => (literal :: <&raw-machine-word>)
  //---*** NOTE: I'm not absolutely sure that this is correct ...
  make-raw-literal(object)
end function make-raw-literal-with-overflow;
