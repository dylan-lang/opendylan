Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <llvm-value> (<object>)
end class;

define generic llvm-value-type
    (value :: <llvm-value>) => (type :: <llvm-type>);

define generic value-forward
    (value :: <llvm-value>)
 => (value :: <llvm-value>);

define method value-forward
    (value :: <llvm-value>)
 => (value :: <llvm-value>);
  value
end method;

define generic value-partition-key
    (value :: <llvm-value>)
 => (key :: <vector>);

// Default method
define method value-partition-key
    (value :: <llvm-value>)
 => (key :: <vector>);
  vector(object-class(value))
end method;

define generic value-referenced-values
    (value :: <llvm-value>)
 => (referenced :: <sequence>);

// Default method
define method value-referenced-values
    (value :: <llvm-value>)
 => (referenced :: <vector>);
  #[]
end method;

define generic value-referenced-types
    (value :: <llvm-value>)
 => (referenced :: <sequence>);

// Default method
define method value-referenced-types
    (value :: <llvm-value>)
 => (referenced :: <vector>);
  vector(llvm-value-type(value))
end method;


/// Placeholder values

define abstract class <llvm-placeholder-value> (<llvm-value>)
  slot llvm-placeholder-value-forward :: <llvm-value>;
end class;
