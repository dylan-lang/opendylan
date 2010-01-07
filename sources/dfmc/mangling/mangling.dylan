Module: dfmc-mangling
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define constant $module-separator               = "Y";
define constant $library-separator              = "V";
define constant $local-suffix                   = "_";
define constant $hygiene-marker	                = "F";
define constant $escape-separator               = "Z";
define constant $constant-prefix                = "K";
define constant $symbol-prefix	                = "J";
define constant $indirection-prefix             = "I";
define constant $wrapper-suffix	                = "W";
define constant $iep-suffix 	                = "I";
define constant $method-mangled-marker-string   = "M";
define constant $slot-mangled-marker-string     = "H";
define constant $dylan-module-separator         = "K";
define constant $domain-mangled-marker-string   = "RD_";

// Note that the following must be characters rather than strings, since
// the initialization of mangles tables assumes that.

define constant $method-marker		   = '#';
define constant $method-mangled-marker	   = $method-mangled-marker-string[0];
define constant $slot-marker               = ','; 
define constant $slot-mangled-marker       = $slot-mangled-marker-string[0];
define constant $constant-marker           = $constant-prefix[0];
define constant $iep-marker                = $iep-suffix[0];
define constant $indirection-marker        = $indirection-prefix[0];
define constant $wrapper-marker            = $wrapper-suffix[0];
define constant $module-marker             = $module-separator[0];
define constant $library-marker            = $library-separator[0];
define constant $escape-marker             = $escape-separator[0];
define constant $symbol-marker             = $symbol-prefix[0];
define constant $local-marker              = $local-suffix[0];
define constant $hygiene-char              = $hygiene-marker[0];
define constant $dylan-module-marker       = $dylan-module-separator[0];

define constant $all-prefix-markers
   = vector($constant-marker, $indirection-marker, $symbol-marker);

define constant $all-suffix-markers
   = vector($local-marker, $wrapper-marker, $iep-marker);

define constant $all-decoration-markers
   = concatenate($all-prefix-markers, $all-suffix-markers);

define table $mangle-dylan-module =
  { #"dylan"                  => 'd',
    #"internal"               => 'i',
    #"dylan-primitives"       => 'p',
    #"dylan-extensions"       => 'e',
    #"dylan-c-ffi"            => 'c',
    #"dylan-incremental"      => 'n',
    #"dylan-threads"          => 't',
    #"dispatch-engine"        => 'g',
    #"machine-word-lowlevel"  => 'm' };

define constant $demangle-dylan-module =
  begin
    let tbl = make(<table>);
    for (abbrev keyed-by mod in $mangle-dylan-module)
      element(tbl, abbrev) := mod;
    end for;
    tbl
  end;


define constant $min-character-code = 0;
define constant $max-character-code = 255; // We allow 8 bit ascii.

/// MANGLING

define constant $mangles-data
  = vector(#('-', '_'), #('!', 'X'), #('$', 'D'), #('%', 'P'), #('*', 'T'), 
           #('/', 'S'), #('<', 'L'), #('>', 'G'), #('?', 'Q'), #('+', 'A'),
           #('&', 'B'), #('^', 'C'), #('_', 'U'), #('@', 'O'), #('=', 'E'),
           #('~', 'N'), 
           list($method-marker, $method-mangled-marker),
           list($slot-marker,   $slot-mangled-marker));

define abstract class <abstract-mangler> (<object>)
  constant slot mangler-buffer = make(<stretchy-vector>);
  constant slot mangler-table  = make(<vector>, size: $max-character-code + 1);
end class;

define abstract class <mangler> (<abstract-mangler>)
end class;

define class <simple-mangler> (<mangler>)
end class;

// Guarantee instantiability of the <MANGLER> class.

define sealed method make (c == <mangler>, #rest keys, #key, #all-keys)
    => (mangler :: <mangler>)
  apply(make, <simple-mangler>, keys)
end method;

define class <mangler-with-options> (<mangler>)
  // Options a assumed to be in the form of prefixes and/or
  // suffixes to the basic mangle. This are stored as
  // strings, which is convenient because they can be zero
  // or more characters in each case.
  slot mangler-prefix-options :: <byte-string> = "";
  slot mangler-suffix-options :: <byte-string> = "";
end class;

define method mangler-position (mangler :: <mangler>) => (res :: <integer>)
  size(mangler-buffer(mangler))
end method;

define method initialize (mangler :: <mangler>, #key, #all-keys) => ()
  next-method();
  initialize-mangler-table(mangler);
end method;

// TODO:
// At the moment, this method only deals with the options that are
// needed by the debugger, and the processing of those keywords is
// quite simple-minded. The interface and its implementation may need
// to be generalized somewhat.

define sealed method initialize
    (mangler :: <mangler-with-options>,
     #key constant-object-extension = #f,
          wrapper-object-extension = #f,
          iep-extension = #f,
     #all-keys) => ()
  next-method();
  let prefix = "";
  let suffix = "";
  if (constant-object-extension)
    prefix := $constant-prefix;
  elseif (wrapper-object-extension)
    prefix := $constant-prefix;
    suffix := $wrapper-suffix;
  elseif (iep-extension)
    prefix := $constant-prefix;
    suffix := $iep-suffix;
  end if;
  mangler.mangler-prefix-options     := prefix;
  mangler.mangler-suffix-options     := suffix;
end method;

define method initialize-mangler-table (mangler :: <mangler>) => ()
  let table = mangler-table(mangler);
  // fill with default manglings
  for (i from $min-character-code to $max-character-code) 
    table[i] 
      := concatenate
           ($escape-separator, mangle-integer(i), $escape-separator);
  end for;
  // fill in special cases
  for (mangle in $mangles-data)
    table[as(<integer>, mangle[0])] := mangle[1];
  end for;
  // fill C allowable versions
  for (i from as(<integer>, 'a') to as(<integer>, 'z'))
    table[i] := as(<character>, i);
  end for;
  for (i from as(<integer>, '0') to as(<integer>, '9'))
    table[i] := as(<character>, i);
  end for;
end method;

define method mangler-as-string 
    (mangler :: <abstract-mangler>, #key start :: <integer> = 0) 
 => (res :: <byte-string>)
  // if (start = 0)
  //   as(<byte-string>, mangler-buffer(mangler))
  // else
  //   as(<byte-string>, copy-sequence(mangler-buffer(mangler), start: start))
  // end if
  let buffer :: <stretchy-vector>
    = mangler-buffer(mangler);
  let buffer-size :: <integer>
    = size(buffer);
  let string :: <byte-string>
    = make(<byte-string>, size: buffer-size - start);
  // without-bounds-checks
    for (i :: <integer> from start below buffer-size,
         j :: <integer> from 0)
      string[j] := buffer[i];
    end for;
  // end without-bounds-checks;
  string
end method;

// The method for <mangler-with-options> is responsible for
// installing the prefixes and suffixes in the final string.
// (Is this the most efficient way? It certainly seems to be
// the cleanest).

define method mangler-as-string
    (mangler :: <mangler-with-options>, #key start :: <integer> = 0)
 => (res :: <byte-string>)
  concatenate
     (mangler.mangler-prefix-options,
      next-method(),
      mangler.mangler-suffix-options)
end method;

define method mangler-reset
    (mangler :: <abstract-mangler>) => (res :: <abstract-mangler>)
  size(mangler-buffer(mangler)) := 0;
  mangler
end method;

define inline method mangle-raw-into 
    (mangler :: <abstract-mangler>, name :: <byte-character>)
  add!(mangler-buffer(mangler), name);
end method;

define inline method mangle-raw-into 
    (mangler :: <abstract-mangler>, name :: <byte-string>)
  concatenate!(mangler-buffer(mangler), name);
end method;

define inline method mangle-raw-into 
    (mangler :: <abstract-mangler>, name :: <symbol>)
  concatenate!
    (mangler-buffer(mangler), as-lowercase(as(<byte-string>, name)));
end method;

define method mangle-name-into 
    (mangler :: <mangler>, name :: <byte-string>)
  for (c in name)
    mangle-raw-into(mangler, mangler-table(mangler)[as(<integer>, c)]);
  end for;
end method;

define method mangle-name-into 
    (mangler :: <mangler>, name)
  mangle-name-into(mangler, as-lowercase(as(<byte-string>, name)))
end method;

define method mangle-name-raw
   (mangler :: <mangler>, name) => (res :: <byte-string>)
  mangler-reset(mangler);
  mangle-name-into(mangler, name);
  mangler-as-string(mangler)
end method;

define method mangle-name-locally
   (mangler :: <mangler>, name) => (res :: <byte-string>)
  mangler-reset(mangler);
  mangle-name-into(mangler, name);
  mangle-raw-into(mangler, $local-suffix);
  mangler-as-string(mangler)
end method;

define method mangle-name-hygienically
    (mangler :: <mangler>, name, marker :: <integer>)
 => (res :: <byte-string>)
  mangler-reset(mangler);
  mangle-name-into(mangler, name);
  concatenate
    (mangler-as-string(mangler), $hygiene-marker, mangle-integer(marker))
end method;

define method mangle-binding-spread
    (mangler :: <mangler>, variable-name, module-name, library-name)
 => (res :: <byte-string>)
  mangler-reset(mangler);
  mangle-name-into(mangler, variable-name);
  mangle-namespace-spread-into(mangler, module-name, library-name);
  mangler-as-string(mangler)
end method;

define method mangle-namespace-spread-into
    (mangler :: <mangler>, module-name, library-name)
  local method non-dylan-mangle ()
	  unless (module-name = library-name)
	    mangle-raw-into(mangler, $module-separator);
	    mangle-name-into(mangler, module-name);
	  end unless;
	  mangle-raw-into(mangler, $library-separator);
	  mangle-name-into(mangler, library-name);
	end method;
  if (as(<symbol>, library-name) = #"dylan")
    let abbreviation 
      = element($mangle-dylan-module, as(<symbol>, module-name), default: #f);
    if (abbreviation)
      mangle-raw-into(mangler, $library-separator);
      mangle-raw-into(mangler, $dylan-module-separator);
      mangle-raw-into(mangler, abbreviation);
    else
      non-dylan-mangle()
    end if
  else
    non-dylan-mangle()
  end if;
end method;



define constant $number-characters :: <byte-string> = "0123456789";

define method mangle-integer (number :: <integer>) => (mangled-number :: <byte-string>)

  iterate process-integer (number :: <integer> = number, index :: <integer> = 1)
    let (quotient :: <integer>, remainder :: <integer>) = truncate/(number, 10);
    let digit :: <byte-character> = $number-characters[remainder];
    
    if (quotient = 0)
      let result :: <byte-string> = make(<byte-string>, size: index);
      result[0] := digit;
      result
    else
      let result :: <byte-string> = process-integer(quotient, index + 1);
      result[result.size - index] := digit;
      result
    end if;

  end iterate;
end method;


// Higher Level DFMC Mangling functions

define inline method mangle-constant(name :: <string>) => (name :: <string>)
  concatenate($constant-prefix, name)
end method;

define inline method mangle-symbol(name :: <string>) => (name :: <string>)
  concatenate($constant-prefix, $symbol-prefix, name)
end method;

define inline method mangle-generic-method
    (mangler :: <mangler>, name :: <string>, number :: <integer>,
     method-library-name, generic-library-name) => (name :: <string>)
  let library-name
    = if (generic-library-name == method-library-name)
        ""
      else
        mangle-name-raw(mangler, method-library-name)
      end if;
  concatenate($constant-prefix,
	      name,
	      $method-mangled-marker-string,
	      library-name,
	      $method-mangled-marker-string,
	      mangle-integer(number))
end method;

define inline method mangle-local-method
    (name :: <string>, number :: <integer>) => (name :: <string>)
  concatenate($constant-prefix,
	      name,
	      $hygiene-marker,
	      mangle-integer(number))
end method;

define inline method mangle-domain
    (name :: <string>, number :: <integer>, library-name :: <string>) => (name :: <string>)
  concatenate($constant-prefix,
	      name,
	      $domain-mangled-marker-string,
	      library-name,
	      $domain-mangled-marker-string,
	      mangle-integer(number))
end method;

define inline method mangle-slot-descriptor
    (mangler :: <mangler>, slot-name :: <string>, slot-library,
     owner-name :: <string>, owner-module, owner-library) => (name :: <string>)
  if (slot-library == owner-library)
    concatenate($constant-prefix,
		slot-name,
		$slot-mangled-marker-string,
		owner-name)
  else
    let namespace-part :: <string> =
      begin
	mangler-reset(mangler);
	mangle-namespace-spread-into(mangler, owner-module, owner-library);
	mangler-as-string(mangler)
      end;
    concatenate($constant-prefix,
		slot-name,
		$slot-mangled-marker-string,
		owner-name,
		namespace-part)
  end if
end method;

define inline method mangle-wrapper(name :: <string>) => (name :: <string>)
  concatenate($constant-prefix, name, $wrapper-suffix)
end method;
