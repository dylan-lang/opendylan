Module:   dfmc-modeling
Synopsis: Limited collections
Author:   Jonathan Bachrach and Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <limited-element-type-mapping> (<object>)
  constant slot limited-element-type-mapping-default :: false-or(<symbol>), 
    required-init-keyword: default:;
  constant slot limited-class-element-type-mappings :: <simple-object-vector>, 
    required-init-keyword: class-mappings:;
  constant slot limited-limited-integer-element-type-mappings :: <simple-object-vector>, 
    required-init-keyword: limited-integer-mappings:;
end class;

define constant $limited-element-type-mappings
  = make(<stretchy-vector>);

define method install-limited-element-type-mappings 
    (collection :: <symbol>, mappings :: <limited-element-type-mapping>)
  add!($limited-element-type-mappings, pair(collection, mappings));
end method;

define macro limited-element-type-mappings-definer
  { define limited-element-type-mappings (?collection:name)
      ?mappings:*
    end }
    => { define limited-element-type-mappings-aux (?collection)
           (?mappings) (?mappings) (?mappings)
         end }
end macro;

define macro limited-element-type-mappings-aux-definer
  { define limited-element-type-mappings-aux (?collection:name)
      (?class-mappings) (?limited-integer-mappings) (?default-mapping)
    end }
    => { define constant "$" ## ?collection ## "-mappings" 
           = make(<limited-element-type-mapping>,
		  default:                  ?default-mapping,
		  class-mappings:           vector(?class-mappings),
		  limited-integer-mappings: vector(?limited-integer-mappings));
         install-limited-element-type-mappings
           (?#"collection", "$" ## ?collection ## "-mappings") }
  class-mappings:
    { } 
      => { }
    { otherwise
        => ?concrete-class:name; ... }
      => { ... }
    { ?element-type:name
        => ?concrete-class:name; ... }
      => { pair(?#"element-type", ?#"concrete-class"), ... }
    { ?anything:*
        => ?concrete-class:name; ... }
      => { ... }
  limited-integer-mappings:
    { } 
      => { }
    { limited(<integer>, min: ?min:expression, max: ?max:expression)
        => ?concrete-class:name; ... }
      => { pair(pair(?min, ?max), ?#"concrete-class"), ... }
    { ?anything:*
        => ?concrete-class:name; ... }
      => { ... }
  default-mapping:
    { } 
      => { #f }
    { otherwise
        => ?concrete-class:name; ... }
      => { ?#"concrete-class" }
    { ?anything:*
        => ?concrete-class:name; ... }
      => {  ... }
end macro;

define method lookup-limited-collection-concrete-class
    (element-type :: <&type>, mappings :: <limited-element-type-mapping>)
 => (concrete-class :: <&class>, default :: <&class>)
  let default = dylan-value(limited-element-type-mapping-default(mappings));
  block (return)
    if (instance?(element-type, <&limited-integer>))
      for (limited-integer-mapping in limited-limited-integer-element-type-mappings(mappings))
	let limited-integer-min-max
	  = head(limited-integer-mapping);
	let limited-integer
	  = ^limited-integer(min: head(limited-integer-min-max), max: tail(limited-integer-min-max));
	if (^subtype?(element-type, limited-integer))
	  return(dylan-value(tail(limited-integer-mapping)), default);
	end if
      end for;
    else 
      for (class-mapping in limited-class-element-type-mappings(mappings))
	if (element-type == dylan-value(head(class-mapping)))
	  return(dylan-value(tail(class-mapping)), default);
	end if
      end for;
    end if;
    values(default, default)
  end block;
end method;

define method lookup-limited-collection-element-type
    (concrete-class :: <&class>, mappings :: <limited-element-type-mapping>)
 => (element-type :: false-or(<&type>))
  block (return)
    for (class-mapping in limited-class-element-type-mappings(mappings))
      if (concrete-class == dylan-value(tail(class-mapping)))
	return(dylan-value(head(class-mapping)));
      end if
    end for;
    for (limited-integer-mapping in limited-limited-integer-element-type-mappings(mappings))
      let limited-integer = head(limited-integer-mapping);
      if (concrete-class == dylan-value(tail(limited-integer-mapping)))
	return(^limited-integer(min: head(limited-integer), max: tail(limited-integer)))
      end if
    end for;
    if (concrete-class == dylan-value(limited-element-type-mapping-default(mappings)))
      dylan-value(#"<object>")
    else 
      #f
    end if
  end block;
end method;

define method lookup-any-limited-collection-element-type
    (concrete-class :: <&class>)
 => (element-type :: false-or(<&type>))
  block (return)
    for (mapping in $limited-element-type-mappings)
      if (^subtype?(concrete-class, dylan-value(head(mapping))))
	return(lookup-limited-collection-element-type(concrete-class, tail(mapping)))
      end if
    end for;
    #f
  end block;
end method;

define limited-element-type-mappings (<string>)
  <byte-character>    => <byte-string>;
  <unicode-character> => <unicode-string>;
  otherwise           => <byte-string>;
end limited-element-type-mappings;
    
define method select-limited-string (of, size)
  let concrete-class 
    = lookup-limited-collection-concrete-class(of, $<string>-mappings);
  if (size)
    ^make(<&limited-vector-type>,
	  class:          dylan-value(#"<string>"),
	  concrete-class: concrete-class,
	  element-type:   of,
	  size:           size);
  else 
    concrete-class
  end if;
end method;

define limited-element-type-mappings (<vector>)
  <object>
    => <simple-object-vector>;
  <integer>
    => <simple-integer-vector>;
  <machine-word>
    => <simple-machine-word-vector>;
  <single-float>
    => <simple-single-float-vector>;
  <double-float>
    => <simple-double-float-vector>;
  limited(<integer>, min: 0, max: 255)
    => <simple-byte-vector>;
  limited(<integer>, min: 0, max: 65535)
    => <simple-double-byte-vector>;
  otherwise
    => <simple-element-type-vector>;
end limited-element-type-mappings;
    
define method select-limited-vector (of, size)
  let (concrete-class, default-concrete-class)
    = lookup-limited-collection-concrete-class(of, $<vector>-mappings);
  if (size | concrete-class == default-concrete-class)
    ^make(<&limited-vector-type>,
	  class:          dylan-value(#"<simple-vector>"),
	  concrete-class: concrete-class,
	  element-type:   of,
	  size:           size);
  else 
    concrete-class
  end if;
end method;

define limited-element-type-mappings (<array>)
  <object>
    => <simple-object-array>;
  <integer>
    => <simple-integer-array>;
  <machine-word>
    => <simple-machine-word-array>;
  <single-float>
    => <simple-single-float-array>;
  <double-float>
    => <simple-double-float-array>;
  limited(<integer>, min: 0, max: 255)
    => <simple-byte-array>;
  limited(<integer>, min: 0, max: 65535)
    => <simple-double-byte-array>;
  otherwise
    => <simple-element-type-array>;
end limited-element-type-mappings;
    
define method select-limited-array (of, sz, dimensions)
  if (sz)	
    select-limited-vector(of, sz)
  elseif (dimensions & size(dimensions) = 1)
    select-limited-vector(of, first(dimensions))
  else
    let (concrete-class, default-concrete-class)
      = lookup-limited-collection-concrete-class(of, $<array>-mappings);
    if (dimensions | concrete-class == default-concrete-class)
      ^make(<&limited-array-type>,
	    class:          dylan-value(#"<array>"),
	    concrete-class: concrete-class,
	    element-type:   of,
	    dimensions:     dimensions);
    else 
      concrete-class
    end if;
  end if;
end method;

define limited-element-type-mappings (<stretchy-vector>)
  <object>
    => <stretchy-object-vector>;
  <byte-character>
    => <stretchy-byte-character-vector>;
  limited(<integer>, min: 0, max: 255)
    => <stretchy-byte-vector>;
  otherwise
    => <stretchy-element-type-vector>;
end limited-element-type-mappings;
    
define method select-limited-stretchy-vector (of)
  let (concrete-class, default-concrete-class)
    = lookup-limited-collection-concrete-class(of, $<stretchy-vector>-mappings);
  if (concrete-class == default-concrete-class)
    ^make(<&limited-stretchy-vector-type>,
	  class:          dylan-value(#"<stretchy-vector>"),
	  concrete-class: concrete-class,
	  element-type:   of);
  else 
    concrete-class
  end if
end method;

define limited-element-type-mappings (<table>)
  <object>
    => <object-table>;
  otherwise
    => <standard-object-table>;
end limited-element-type-mappings;
    
define method select-limited-table (of, size)
  let (concrete-class, default-concrete-class)
    = lookup-limited-collection-concrete-class(of, $<table>-mappings);
  if (size | concrete-class == default-concrete-class)
    ^make(<&limited-table-type>,
	  class:          dylan-value(#"<table>"),
	  concrete-class: dylan-value(#"<object-table>"),
	  element-type:   of,
	  size:           size);
  else 
    concrete-class
  end if;
end method;

define limited-element-type-mappings (<set>)
  <object>
    => <object-set>;
  otherwise
    => <object-set>;
end limited-element-type-mappings;
    
define method select-limited-set (of, size)
  let (concrete-class, default-concrete-class)
    = lookup-limited-collection-concrete-class(of, $<set>-mappings);
  if (size | concrete-class == default-concrete-class)
    ^make(<&limited-set-type>,
	  class:          dylan-value(#"<set>"),
	  concrete-class: concrete-class,
	  element-type:   of,
	  size:           size);
  else
    concrete-class
  end if
end method;

define limited-element-type-mappings (<deque>)
  <object>
    => <object-deque>;
  otherwise
    => <object-deque>;
end limited-element-type-mappings;
    
define method select-limited-deque (of)
  let (concrete-class, default-concrete-class)
    = lookup-limited-collection-concrete-class(of, $<deque>-mappings);
  if (size | concrete-class == default-concrete-class)
    ^make(<&limited-deque-type>,
	  class:          dylan-value(#"<deque>"),
	  concrete-class: concrete-class,
	  element-type:   of);
  else 
    concrete-class
  end if
end method;

define method ^limited-collection 
    (class :: <&class>, #rest all-keys, #key of, size, dimensions, #all-keys)
  let of         = of | lookup-keyword-value(all-keys, #"of"); // HACK: FOR EMULATOR
  let size       = size | lookup-keyword-value(all-keys, #"size"); // HACK: FOR EMULATOR
  let dimensions = dimensions | lookup-keyword-value(all-keys, #"dimensions"); // HACK: FOR EMULATOR
  if (of) 
    // PARALLELS RUNTIME METHODS ON LIMITED
    select (class)
      dylan-value(#"<range>")  // TODO: NOT YET IMPLEMENTED
	=> class;
      dylan-value(#"<string>")
	=> select-limited-string(of, size);
      dylan-value(#"<deque>")
	=> select-limited-deque(of);
      dylan-value(#"<stretchy-vector>") 
	=> select-limited-stretchy-vector(of);
      dylan-value(#"<vector>"), dylan-value(#"<simple-vector>")
	=> select-limited-vector(of, size);
      dylan-value(#"<array>") 
	=> select-limited-array(of, size, dimensions);
      dylan-value(#"<set>")
	=> select-limited-set(of, size);
      dylan-value(#"<table>"), dylan-value(#"<object-table>")
	=> select-limited-table(of, size);
      // UNINSTANTIATEABLE LIMITED COLLECTION TYPES
      dylan-value(#"<collection>")
	=> ^make(<&limited-collection-type>,
		 class:          class,
		 element-type:   of,
		 size:           size);
      dylan-value(#"<explicit-key-collection>")
	=> ^make(<&limited-explicit-key-collection-type>,
		 class:          class,
		 element-type:   of,
		 size:           size);
      dylan-value(#"<mutable-collection>")
	=> ^make(<&limited-mutable-collection-type>,
		 class:          class,
		 element-type:   of,
		 size:           size);
      dylan-value(#"<stretchy-collection>")
	=> ^make(<&limited-stretchy-collection-type>,
		 class:          class,
		 element-type:   of);
      dylan-value(#"<mutable-explicit-key-collection>")
	=> ^make(<&limited-mutable-explicit-key-collection-type>,
		 class:          class,
		 element-type:   of,
		 size:           size);
      dylan-value(#"<sequence>")
	=> ^make(<&limited-sequence-type>,
		 class:          class,
		 element-type:   of,
		 size:           size);
      dylan-value(#"<mutable-sequence>")
	=> ^make(<&limited-mutable-sequence-type>,
		 class:          class,
		 element-type:   of,
		 size:           size);
      otherwise 
	=> #f;
    end select	
  else
    class
  end if;
end method;

// eof
