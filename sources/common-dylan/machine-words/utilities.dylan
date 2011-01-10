Module:       common-dylan-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////////////////////////////////////////////////////////////////////////
// validating bit numbers

define inline function valid-bit-number? (bit :: <integer>)
  (0 <= bit) & (bit < word-size());
end;

define sealed abstract class <invalid-bit-number> (<error>)
  constant slot bit-number :: <integer>,
    required-init-keyword: bit-number:;
end class;

// This slot is not actually needed, but keep it for the keyword...
ignore(bit-number);


//////////////////////////////////////////////////////////////////////////////
// checking shift quantity

define inline function check-shift-quantity (count :: <integer>)
  unless (valid-bit-number?(count))
    invalid-shift-quantity(count);
  end unless;
end;

define sealed class <invalid-shift-quantity> (<invalid-bit-number>)
end class;

define sealed domain make (singleton(<invalid-shift-quantity>));
define sealed domain initialize (<invalid-shift-quantity>);

// --- need report method for <invalid-shift-quantity>

define function invalid-shift-quantity (count :: <integer>)
  error(make(<invalid-shift-quantity>, bit-number: count));
end;

//////////////////////////////////////////////////////////////////////////////
// checking logbit? index

define inline function check-bit-index (index :: <integer>)
  unless (valid-bit-number?(index))
    invalid-bit-index(index);
  end unless;
end;

define sealed class <invalid-bit-index> (<invalid-bit-number>)
end class;

define sealed domain make (singleton(<invalid-bit-index>));
define sealed domain initialize (<invalid-bit-index>);

// --- need report method for <invalid-bit-index>

define function invalid-bit-index (index :: <integer>)
  error(make(<invalid-bit-index>, bit-number: index));
end;


