Module:       duim-dcs-internals
Synopsis:     DUIM display device contexts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Images

define protocol <<image-protocol>> ()
  getter image-width
    (image :: <image>) => (width  :: <integer>);
  getter image-height
    (image :: <image>) => (height :: <integer>);
  getter image-depth
    (image :: <image>) => (depth  :: <integer>);
  // Reading and writing images
  function read-image
    (locator, #key image-type, #all-keys) => (image :: false-or(<image>));
  function read-image-as
    (class :: <class>, locator, image-type, #key, #all-keys)
 => (image :: false-or(<image>));
  function write-image
    (image :: <image>, locator) => ();
  // Image conversion
  function convert-image
    (image :: <image>, image-type) => (image :: <image>);
  function image-convertible?
    (image :: <image>, image-type) => (true? :: <boolean>);
end protocol <<image-protocol>>;


/// Stencils

define constant $stencil-colors :: <simple-object-vector>
    = vector($background, $foreground);

// A stencil is just a bitmap of the foreground and background colors
define sealed class <stencil> (<image>)
  sealed constant slot %array :: <array>,
    required-init-keyword: array:;
  sealed slot %transform = #f,
    init-keyword: transform:;
end class <stencil>;

define sealed domain make (singleton(<stencil>));
define sealed domain initialize (<stencil>);

define protocol-predicate stencil;

define method make-stencil
    (array :: <array>) => (stencil :: <stencil>)
  make(<stencil>, array: array)
end method make-stencil;

define method make-stencil
    (sequence :: <sequence>) => (stencil :: <stencil>)
  let array = make-array-from-contents(sequence);
  make(<stencil>, array: array)
end method make-stencil;

define macro stencil-definer
  { define stencil ?:name ?array:* end }
    => { define constant ?name :: <stencil> = make-stencil(list(?array)) }
 array:
  { } => { }
  { ?row:*; ... } => { list(?row), ... }
 row:
  { } => { }
  { ?cell:*, ... } => { ?cell, ... }
end macro stencil-definer;

define method box-edges
    (stencil :: <stencil>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(0, 0, dimension(stencil.%array, 1), dimension(stencil.%array, 0))
end method box-edges;

define method image-width (stencil :: <stencil>) => (width :: <integer>)
  dimension(stencil.%array, 1)
end method image-width;

define method image-height (stencil :: <stencil>) => (width :: <integer>)
  dimension(stencil.%array, 0)
end method image-height;

define method decode-pattern (stencil :: <stencil>) => (array, colors, transform)
  values(stencil.%array, $stencil-colors, stencil.%transform)
end method decode-pattern;

define method transform-image
    (transform :: <transform>, stencil :: <stencil>) => (stencil :: <stencil>)
  if (identity-transform?(transform))
    stencil
  else
    make(<stencil>,
	 array: stencil.%array,
	 transform: transform)
  end
end method transform-image;


/// Patterns

// A pattern is a portable bitmap of arbitrary colors
define sealed class <pattern> (<stencil>)
  sealed constant slot %colors :: <vector>,
    required-init-keyword: colors:;
end class <pattern>;

define sealed domain make (singleton(<pattern>));
define sealed domain initialize (<pattern>);

define protocol-predicate pattern;

define method make-pattern 
    (array :: <array>, colors) => (pattern :: <pattern>)
  make(<pattern>,
       array:  array,
       colors: as(<simple-vector>, colors))
end method make-pattern;

define method make-pattern 
    (sequence :: <sequence>, colors) => (pattern :: <pattern>)
  let array = make-array-from-contents(sequence);
  make(<pattern>,
       array:  array,
       colors: as(<simple-vector>, colors))
end method make-pattern;

define macro pattern-definer
  { define pattern ?:name (?colors:expression) ?array:* end }
    => { define constant ?name :: <pattern> = make-pattern(list(?array), ?colors) }
 array:
  { } => { }
  { ?row:*; ... } => { list(?row), ... }
 row:
  { } => { }
  { ?cell:*, ... } => { ?cell, ... }
end macro pattern-definer;

define method decode-pattern
    (pattern :: <pattern>) => (array, colors, transform)
  values(pattern.%array, pattern.%colors, pattern.%transform)
end method decode-pattern;

define method transform-image
    (transform :: <transform>, pattern :: <pattern>) => (pattern :: <pattern>)
  if (identity-transform?(transform))
    pattern
  else
    make(<pattern>,
	 array: pattern.%array, colors: pattern.%colors,
	 transform: transform)
  end
end method transform-image;


/// Images

define method read-image
    (locator, #rest keys, #key image-type, #all-keys)
 => (image :: false-or(<image>))
  dynamic-extent(keys);
  // The idea is that back ends have methods that '==' specialize
  // on the image type, and create various subclasses of <image>
  with-keywords-removed (keys = keys, #[image-type:])
    apply(read-image-as, <pattern>, locator, image-type, keys)
  end
end method read-image;
