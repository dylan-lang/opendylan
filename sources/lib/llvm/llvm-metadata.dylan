Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <llvm-metadata-value> (<llvm-value>)
end class;

define method llvm-value-type
    (value :: <llvm-metadata-value>) => (type :: <llvm-type>);
  $llvm-metadata-type
end method;

define method llvm-metadata-function-local?
    (value :: <llvm-metadata-value>) => (function-local? :: <boolean>);
  #f
end method;

define class <llvm-metadata-string> (<llvm-metadata-value>)
  constant slot llvm-metadata-string :: <string>,
    required-init-keyword: string:;
end class;

define method value-partition-key
    (value :: <llvm-metadata-string>)
 => (key :: <vector>);
  apply(vector, object-class(value), value.llvm-metadata-string)
end method;

define class <llvm-metadata-node> (<llvm-metadata-value>)
  constant slot llvm-metadata-node-values :: <sequence>,
    required-init-keyword: node-values:;
  slot llvm-metadata-function-local? :: <boolean>,
    init-keyword: function-local?:;
end class;

define sealed method initialize
    (instance :: <llvm-metadata-node>, #key #all-keys)
 => ();
  unless (slot-initialized?(instance, llvm-metadata-function-local?))
    instance.llvm-metadata-function-local?
      := any?(method (value :: false-or(<llvm-value>))
                select (value by instance?)
                  <llvm-constant-value> => #f;
                  <llvm-metadata-value> => value.llvm-metadata-function-local?;
                  singleton(#f)         => #f;
                  otherwise             => #t;
                end select
              end,
              instance.llvm-metadata-node-values);
  end unless;
end method;

define method value-partition-key
    (value :: <llvm-metadata-node>)
 => (key :: <vector>);
  vector(object-class(value), value.llvm-metadata-node-values.size)
end method;

define method value-referenced-values
    (value :: <llvm-metadata-node>)
 => (referenced :: <vector>);
  as(<vector>, choose(true?, value.llvm-metadata-node-values))
end method;

define method value-referenced-types
    (value :: <llvm-metadata-node>)
 => (referenced :: <vector>);
  if (any?(false?, value.llvm-metadata-node-values))
    vector($llvm-metadata-type, $llvm-void-type)
  else
    vector($llvm-metadata-type)
  end if
end method;


/// Placeholder metadata values

define class <llvm-symbolic-metadata> (<llvm-metadata-value>,
                                       <llvm-placeholder-value>)
  constant slot llvm-symbolic-metadata-name :: <integer>,
    required-init-keyword: name:;
end class;

define method value-forward
    (value :: <llvm-symbolic-metadata>)
 => (value :: <llvm-metadata-value>);
  if (slot-initialized?(value, llvm-placeholder-value-forward))
    value-forward(value.llvm-placeholder-value-forward)
  else
    error("value !%d is not defined", value.llvm-symbolic-metadata-name);
  end if
end method;


/// Named metadata

define class <llvm-named-metadata> (<object>)
  constant slot llvm-named-metadata-name :: <string>,
    required-init-keyword: name:;
  constant slot llvm-named-metadata-operands :: <sequence>,
    required-init-keyword: operands:;
end class;
