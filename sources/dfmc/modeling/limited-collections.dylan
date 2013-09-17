Module:   dfmc-modeling
Synopsis: Limited collections
Author:   Jonathan Bachrach and Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <limited-element-type-mapping> (<object>)
  constant slot limited-element-type-mapping-default :: false-or(<symbol>), 
    required-init-keyword: default:;
  constant slot limited-class-element-type-mappings :: <simple-object-vector>, 
    required-init-keyword: class-mappings:;
  constant slot limited-limited-integer-element-type-mappings :: <simple-object-vector>, 
    required-init-keyword: limited-integer-mappings:;
end class;

define class <limited-element-type-mapping-item> (<object>)
  // #f is don't care
  constant slot mapping-element-type :: false-or(type-union(<pair>, <symbol>)),
    required-init-keyword: element-type:;
  constant slot mapping-consider-fill-value? :: <boolean> = #f,
    init-keyword: consider-fill-value?:;
  constant slot mapping-fill-value :: <object> = #f,
    init-keyword: fill-value:;
  constant slot mapping-concrete-class :: <symbol>,
    required-init-keyword: concrete-class:;
end class;

define method make
    (class == <limited-element-type-mapping-item>, #rest all-keys, #key)
 => (item :: <limited-element-type-mapping-item>)
  let keywords = choose-by(even?, range(), all-keys);
  if (member?(#"fill-value", keywords))
    apply(next-method, class, consider-fill-value?:, #t, all-keys)
  else
    next-method();
  end if
end make;

define constant $limited-element-type-mappings
  = make(<stretchy-vector>);

define method install-limited-element-type-mappings 
    (collection :: <symbol>, mappings :: <limited-element-type-mapping>)
  add!($limited-element-type-mappings, pair(collection, mappings));
end method;

// Mappings should be listed in order, from best match to worst match. This matters
// for matching the default-fill; matching the tighter limited integer type; and for
// matching defaults. The lookup function will return the first suitable match.
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
    { any,
      fill: ?fill:expression
        => ?concrete-class:name; ... }
      => { make(<limited-element-type-mapping-item>,
                element-type: #f, fill-value: ?fill,
                concrete-class: ?#"concrete-class"), ... }
    { ?element-type:name,
      fill: ?fill:expression
        => ?concrete-class:name; ... }
      => { make(<limited-element-type-mapping-item>,
                element-type: ?#"element-type", fill-value: ?fill,
                concrete-class: ?#"concrete-class"), ... }
    { ?element-type:name
        => ?concrete-class:name; ... }
      => { make(<limited-element-type-mapping-item>,
                element-type: ?#"element-type",
                concrete-class: ?#"concrete-class"), ... }
    { ?anything:*
        => ?concrete-class:name; ... }
      => { ... }
  limited-integer-mappings:
    { } 
      => { }
    { limited(<integer>, min: ?min:expression, max: ?max:expression),
      fill: ?fill:expression
        => ?concrete-class:name; ... }
      => { make(<limited-element-type-mapping-item>,
                element-type: pair(?min, ?max), fill-value: ?fill,
                concrete-class: ?#"concrete-class"), ... }
    { limited(<integer>, min: ?min:expression, max: ?max:expression)
        => ?concrete-class:name; ... }
      => { make(<limited-element-type-mapping-item>,
                element-type: pair(?min, ?max),
                concrete-class: ?#"concrete-class"), ... }
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
    (element-type :: <&type>, element-type-fill, mappings :: <limited-element-type-mapping>)
 => (concrete-class :: <&class>, includes-element-type? :: <boolean>, includes-default-fill? :: <boolean>)
  let default = dylan-value(limited-element-type-mapping-default(mappings));
  block (return)
    if (instance?(element-type, <&limited-integer>))
      for (limited-integer-mapping :: <limited-element-type-mapping-item>
           in limited-limited-integer-element-type-mappings(mappings))
        let match-element-type? = true?(limited-integer-mapping.mapping-element-type);
        let match-fill-value? = limited-integer-mapping.mapping-consider-fill-value?;
        let matching-element-type?
          = if (match-element-type?)
              let limited-integer-min-max :: <pair>
                = limited-integer-mapping.mapping-element-type;
              let limited-integer
                = ^limited-integer(min: head(limited-integer-min-max), max: tail(limited-integer-min-max));
              ^subtype?(element-type, limited-integer)
            else
              #t
            end if;
        let matching-default-fill?
          = if (match-fill-value?)
              element-type-fill == limited-integer-mapping.mapping-fill-value
            else
              #t
            end if;
        if (matching-element-type? & matching-default-fill?)
          return(dylan-value(limited-integer-mapping.mapping-concrete-class),
                 match-element-type?, match-fill-value?)
        end if
      end for;
    else 
      for (class-mapping :: <limited-element-type-mapping-item>
           in limited-class-element-type-mappings(mappings))
        let match-element-type? = true?(class-mapping.mapping-element-type);
        let match-fill-value? = class-mapping.mapping-consider-fill-value?;
        let matching-element-type?
          = if (match-element-type?)
              element-type == dylan-value(class-mapping.mapping-element-type)
            else
              #t
            end if;
        let matching-default-fill?
          = if (match-fill-value?)
              element-type-fill == class-mapping.mapping-fill-value
            else
              #t
            end if;
        if (matching-element-type? & matching-default-fill?)
          return(dylan-value(class-mapping.mapping-concrete-class),
                 match-element-type?, match-fill-value?);
        end if
      end for;
    end if;
    values(default, #f, #f)
  end block;
end method;

define method lookup-limited-collection-element-type
    (concrete-class :: <&class>, mappings :: <limited-element-type-mapping>)
 => (element-type :: false-or(<&type>))
  block (return)
    for (class-mapping :: <limited-element-type-mapping-item>
         in limited-class-element-type-mappings(mappings))
      if (concrete-class == dylan-value(class-mapping.mapping-concrete-class))
        if (class-mapping.mapping-element-type)
          return(dylan-value(class-mapping.mapping-element-type));
        else
          return(#f)
        end if
      end if
    end for;
    for (limited-integer-mapping :: <limited-element-type-mapping-item>
         in limited-limited-integer-element-type-mappings(mappings))
      if (concrete-class == dylan-value(limited-integer-mapping.mapping-concrete-class))
        if (limited-integer-mapping.mapping-element-type)
          let limited-integer = limited-integer-mapping.mapping-element-type;
          return(^limited-integer(min: head(limited-integer), max: tail(limited-integer)))
        else
          return(#f)
        end if
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
 => (element-type :: false-or(<&type>), base-class :: false-or(<&type>))
  block (return)
    for (mapping in $limited-element-type-mappings)
      if (^subtype?(concrete-class, dylan-value(head(mapping))))
        return(lookup-limited-collection-element-type(concrete-class, tail(mapping)),
               dylan-value(head(mapping)))
      end if
    end for;
    values(#f, #f)
  end block;
end method;

define limited-element-type-mappings (<string>)
  <byte-character>, fill: as(<byte-character>, ' ')
    => <byte-string>;
  <byte-character>
    => <byte-with-fill-string>;

  <unicode-character>, fill: as(<unicode-character>, ' ')
    => <unicode-string>;
  <unicode-character>
    => <unicode-with-fill-string>;

  any, fill: as(<byte-character>, ' ')
    => <byte-string>;
  otherwise
    => <byte-with-fill-string>;
end limited-element-type-mappings;
    
define method select-limited-string (of, default-fill, size)
  let (concrete-class, includes-element-type?, includes-default-fill?)
    = lookup-limited-collection-concrete-class(of, default-fill, $<string>-mappings);
  if (size | ~includes-element-type? | ~includes-default-fill?)
    ^make(<&limited-vector-type>,
          class:          dylan-value(#"<string>"),
          concrete-class: concrete-class,
          element-type:   of,
          default-fill:   default-fill,
          size:           size);
  else 
    concrete-class
  end if;
end method;

define limited-element-type-mappings (<vector>)
  <machine-word>, fill: as(<machine-word>, 0)
    => <simple-machine-word-vector>;
  <machine-word>
    => <simple-machine-word-with-fill-vector>;

  <single-float>, fill: as(<single-float>, 0.0)
    => <simple-single-float-vector>;
  <single-float>
    => <simple-single-float-with-fill-vector>;

  <double-float>, fill: as(<double-float>, 0.0)
    => <simple-double-float-vector>;
  <double-float>
    => <simple-double-float-with-fill-vector>;

  limited(<integer>, min: 0, max: 255), fill: 0
    => <simple-byte-vector>;
  limited(<integer>, min: 0, max: 255)
    => <simple-byte-with-fill-vector>;
  limited(<integer>, min: 0, max: 65535), fill: 0
    => <simple-double-byte-vector>;
  limited(<integer>, min: 0, max: 65535)
    => <simple-double-byte-with-fill-vector>;

  <integer>, fill: 0
    => <simple-integer-vector>;
  <integer>
    => <simple-integer-with-fill-vector>;

  <object>, fill: #f
    => <simple-object-vector>;

  any, fill: #f
    => <simple-element-type-vector>;
  otherwise
    => <simple-element-type-with-fill-vector>;
end limited-element-type-mappings;
    
define method select-limited-vector (of, default-fill, size)
  let (concrete-class, includes-element-type?, includes-default-fill?)
    = lookup-limited-collection-concrete-class(of, default-fill, $<vector>-mappings);
  if (size | ~includes-element-type? | ~includes-default-fill?)
    ^make(<&limited-vector-type>,
          class:          dylan-value(#"<simple-vector>"),
          concrete-class: concrete-class,
          element-type:   of,
          default-fill:   default-fill,
          size:           size);
  else 
    concrete-class
  end if;
end method;

define limited-element-type-mappings (<array>)
  <machine-word>, fill: as(<machine-word>, 0)
    => <simple-machine-word-array>;
  <machine-word>
    => <simple-machine-word-with-fill-array>;

  <single-float>, fill: as(<single-float>, 0.0)
    => <simple-single-float-array>;
  <single-float>
    => <simple-single-float-with-fill-array>;

  <double-float>, fill: as(<double-float>, 0.0)
    => <simple-double-float-array>;
  <double-float>
    => <simple-double-float-with-fill-array>;

  limited(<integer>, min: 0, max: 255), fill: 0
    => <simple-byte-array>;
  limited(<integer>, min: 0, max: 255)
    => <simple-byte-with-fill-array>;

  limited(<integer>, min: 0, max: 65535), fill: 0
    => <simple-double-byte-array>;
  limited(<integer>, min: 0, max: 65535)
    => <simple-double-byte-with-fill-array>;

  <integer>, fill: 0
    => <simple-integer-array>;
  <integer>
    => <simple-integer-with-fill-array>;

  <object>, fill: #f
    => <simple-object-array>;

  any, fill: #f
    => <simple-element-type-array>;
  otherwise
    => <simple-element-type-with-fill-array>;
end limited-element-type-mappings;
    
define method select-limited-array (of, default-fill, sz, dimensions)
  if (sz)       
    select-limited-vector(of, default-fill, sz)
  elseif (dimensions & size(dimensions) = 1)
    select-limited-vector(of, default-fill, first(dimensions))
  else
    let (concrete-class, includes-element-type?, includes-default-fill?)
      = lookup-limited-collection-concrete-class(of, default-fill, $<array>-mappings);
    if (size | ~includes-element-type? | ~includes-default-fill?)
      ^make(<&limited-array-type>,
            class:          dylan-value(#"<array>"),
            concrete-class: concrete-class,
            element-type:   of,
            default-fill:   default-fill,
            dimensions:     dimensions);
    else 
      concrete-class
    end if;
  end if;
end method;

define limited-element-type-mappings (<stretchy-vector>)
  <byte-character>, fill: as(<byte-character>, ' ')
    => <stretchy-byte-character-vector>;
  <byte-character>
    => <stretchy-byte-character-with-fill-vector>;

  limited(<integer>, min: 0, max: 255), fill: 0
    => <stretchy-byte-vector>;
  limited(<integer>, min: 0, max: 255)
    => <stretchy-byte-with-fill-vector>;

  <object>, fill: #f
    => <stretchy-object-vector>;

  any, fill: #f
    => <stretchy-element-type-vector>;
  otherwise
    => <stretchy-element-type-with-fill-vector>;
end limited-element-type-mappings;
    
define method select-limited-stretchy-vector (of, default-fill)
  let (concrete-class, includes-element-type?, includes-default-fill?)
    = lookup-limited-collection-concrete-class(of, default-fill, $<stretchy-vector>-mappings);
  if (~includes-element-type? | ~includes-default-fill?)
    ^make(<&limited-stretchy-vector-type>,
          class:          dylan-value(#"<stretchy-vector>"),
          concrete-class: concrete-class,
          default-fill:   default-fill,
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
  let (concrete-class, includes-element-type?, includes-default-fill?)
    = lookup-limited-collection-concrete-class(of, #f, $<table>-mappings);
    if (size | ~includes-element-type?)
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
  let (concrete-class, includes-element-type?, includes-default-fill?)
    = lookup-limited-collection-concrete-class(of, #f, $<set>-mappings);
  if (size | ~includes-element-type?)
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
  <object>, fill: #f
    => <object-deque>;
  otherwise
    => <object-deque>;
end limited-element-type-mappings;
    
define method select-limited-deque (of, default-fill)
  let (concrete-class, includes-element-type?, includes-default-fill?)
    = lookup-limited-collection-concrete-class(of, default-fill, $<deque>-mappings);
  if (size | ~includes-element-type? | ~includes-default-fill?)
    ^make(<&limited-deque-type>,
          class:          dylan-value(#"<deque>"),
          concrete-class: concrete-class,
          default-fill:   default-fill,
          element-type:   of);
  else 
    concrete-class
  end if
end method;

define method select-default-fill (class, of)
  select (class)
    dylan-value(#"<string>")
      => select (of by ^subtype?)
           dylan-value(#"<unicode-character>")
             => as(<unicode-character>, ' ');
           otherwise
             => as(<byte-character>, ' ');
         end select;
    dylan-value(#"<deque>")
      => #f;
    dylan-value(#"<stretchy-vector>")
      => select (of by ^subtype?)
           dylan-value(#"<byte-character>")
             => as(<byte-character>, ' ');
           dylan-value(#"<integer>")
             => 0;
           otherwise
             => #f;
         end select;
    dylan-value(#"<vector>"),
    dylan-value(#"<simple-vector>"),
    dylan-value(#"<array>")
      => select (of by ^subtype?)
           dylan-value(#"<machine-word>")
             => as(<machine-word>, 0);
           dylan-value(#"<single-float>")
             => 0.0;
           dylan-value(#"<double-float>")
             => as(<double-float>, 0.0);
           dylan-value(#"<integer>")
             => 0;
           otherwise
             => #f;
         end select;
    otherwise
      => #f;
  end select
end method;

define method ^limited-collection 
    (class :: <&class>, #rest all-keys,
     #key of, default-fill, size, dimensions, #all-keys)
  if (of)
    let keywords = choose-by(even?, range(), all-keys);
    let default-fill
      = if (member?(#"default-fill", keywords))
          default-fill
        else
          select-default-fill(class, of)
        end if;
    // PARALLELS RUNTIME METHODS ON LIMITED
    select (class)
      dylan-value(#"<range>")  // TODO: NOT YET IMPLEMENTED
        => class;
      dylan-value(#"<string>")
        => select-limited-string(of, default-fill, size);
      dylan-value(#"<deque>")
        => select-limited-deque(of, default-fill);
      dylan-value(#"<stretchy-vector>") 
        => select-limited-stretchy-vector(of, default-fill);
      dylan-value(#"<vector>"), dylan-value(#"<simple-vector>")
        => select-limited-vector(of, default-fill, size);
      dylan-value(#"<array>") 
        => select-limited-array(of, default-fill, size, dimensions);
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
