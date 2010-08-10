Module:       llvm-tablegen
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <tablegen-type> (<object>)
end class;

// Simple named types
define class <tablegen-simple-type> (<tablegen-type>)
  constant slot simple-type-kind :: one-of(#"STRING", #"BIT", #"INT", #"CODE",
                                           #"DAG"),
    required-init-keyword: kind:;
end class;

define sealed method print-message
    (type :: <tablegen-simple-type>, stream :: <stream>)
    => ();
  let name
    = select (type.simple-type-kind)
        #"STRING" => "string";
        #"BIT"    => "bit";
        #"INT"    => "int";
        #"CODE"   => "code";
        #"DAG"    => "dag";
      end select;
  write-text(stream, name);
end method;

// Bitfield type
define class <tablegen-bits-type> (<tablegen-type>)
  constant slot bits-type-size :: <integer>,
    required-init-keyword: size:;
end class;

define sealed method print-message
    (type :: <tablegen-bits-type>, stream :: <stream>)
    => ();
  format(stream, "bits<%d>", type.bits-type-size);
end method;

// List type
define class <tablegen-list-type> (<tablegen-type>)
  constant slot list-type-of :: <tablegen-type>,
    required-init-keyword: of:;
end class;

define sealed method print-message
    (type :: <tablegen-list-type>, stream :: <stream>)
    => ();
  format(stream, "list<%s>", type.list-type-of);
end method;

// Class type
define class <tablegen-class-type> (<tablegen-type>)
  constant slot class-type-class :: <tablegen-class>,
    required-init-keyword: class:;
end class;

define sealed method print-message
    (type :: <tablegen-class-type>, stream :: <stream>)
    => ();
  write-text(stream, type.class-type-class.record-name);
end method;
