Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2016 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Metadata as a value

define class <llvm-metadata-value> (<llvm-value>)
  constant slot llvm-metadata-value-metadata :: <llvm-metadata>,
    required-init-keyword: metadata:
end class;

define method llvm-value-type
    (value :: <llvm-metadata-value>) => (type :: <llvm-type>);
  $llvm-metadata-type
end method;

define method value-referenced-metadata
    (value :: <llvm-metadata-value>)
 => (referenced :: <vector>);
  vector(value.llvm-metadata-value-metadata)
end method;


/// Metadata

define abstract class <llvm-metadata> (<object>)
end class;

define generic llvm-metadata-forward
    (value :: <llvm-metadata>)
 => (value :: <llvm-metadata>);

// Default method
define method llvm-metadata-forward
    (metadata :: <llvm-metadata>)
 => (metadata :: <llvm-metadata>);
  metadata
end method;

define generic metadata-partition-key
    (metadata :: <llvm-metadata>)
  => (key :: <vector>);

// Default method
define method metadata-partition-key
    (value :: <llvm-metadata>)
 => (key :: <vector>);
  vector(object-class(value))
end method;

define generic metadata-referenced-values
    (metadata :: <llvm-metadata>)
 => (referenced :: <sequence>);

// Default method
define method metadata-referenced-values
    (metadata :: <llvm-metadata>)
 => (referenced :: <vector>);
  #[]
end method;

define generic metadata-referenced-metadata
    (metadata :: <llvm-metadata>)
 => (referenced :: <sequence>);

// Default method
define method metadata-referenced-metadata
    (metadata :: <llvm-metadata>)
 => (referenced :: <vector>);
  #[]
end method;

// Strings

define class <llvm-metadata-string> (<llvm-metadata>)
  constant slot llvm-metadata-string :: <string>,
    required-init-keyword: string:;
end class;

define method metadata-partition-key
    (value :: <llvm-metadata-string>)
 => (key :: <vector>);
  let key
    = make(<simple-object-vector>, size: value.llvm-metadata-string.size + 1);
  key[0] := object-class(value);
  replace-subsequence!(key, value.llvm-metadata-string, start: 1)
end method;

// Generic metadata nodes

define class <llvm-metadata-node> (<llvm-metadata>)
  constant slot llvm-metadata-distinct? :: <boolean>,
    init-keyword: distinct?:, init-value: #f;
  constant slot llvm-metadata-node-values :: <sequence>,
    required-init-keyword: node-values:;
end class;

define method metadata-partition-key
    (value :: <llvm-metadata-node>)
 => (key :: <vector>);
  if (value.llvm-metadata-distinct?)
    vector(value)
  else
    vector(object-class(value), value.llvm-metadata-node-values.size)
  end if
end method;

define method metadata-referenced-metadata
    (value :: <llvm-metadata-node>)
 => (referenced :: <vector>);
  as(<vector>, choose(true?, value.llvm-metadata-node-values))
end method;

// Metadata references to values
define class <llvm-value-metadata> (<llvm-metadata>)
  constant slot llvm-metadata-value :: <llvm-value>,
    required-init-keyword: value:;
end class;

define method metadata-referenced-values
    (metadata :: <llvm-value-metadata>)
 => (referenced :: <vector>);
  vector(metadata.llvm-metadata-value)
end method;


/// Placeholder metadata values

define class <llvm-symbolic-metadata> (<llvm-metadata>)
  slot llvm-symbolic-metadata-forward :: <llvm-metadata>;
  constant slot llvm-symbolic-metadata-name :: <integer>,
    required-init-keyword: name:;
end class;

define method llvm-metadata-forward
    (metadata :: <llvm-symbolic-metadata>)
 => (metadata :: <llvm-metadata>);
  if (slot-initialized?(metadata, llvm-symbolic-metadata-forward))
    llvm-metadata-forward(metadata.llvm-symbolic-metadata-forward)
  else
    error("metadata !%d is not defined", metadata.llvm-symbolic-metadata-name);
  end if
end method;


/// Named metadata

define class <llvm-named-metadata> (<object>)
  constant slot llvm-named-metadata-name :: <string>,
    required-init-keyword: name:;
  constant slot llvm-named-metadata-operands :: <sequence>,
    required-init-keyword: operands:;
end class;
