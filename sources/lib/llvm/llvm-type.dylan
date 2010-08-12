Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <llvm-type> (<object>)
  // Nothing yet
end class;

define generic type-partition-key
    (type :: <llvm-type>)
 => (key :: <vector>, splittable? :: <boolean>);

define generic type-referenced-types
    (type :: <llvm-type>)
 => (types :: <vector>);

define method type-referenced-types
    (type :: <llvm-type>)
 => (types :: <vector>);
  #[]
end method;

define generic type-forward
    (type :: <llvm-type>)
 => (type :: <llvm-type>);

define method type-forward
    (type :: <llvm-type>)
 => (type :: <llvm-type>);
  type
end method;

define constant <llvm-primitive-type-kind>
  = one-of(#"VOID", #"FLOAT", #"DOUBLE", #"LABEL",
           #"X86_FP80", #"FP128", #"PPC_FP128", #"METADATA");

define class <llvm-primitive-type> (<llvm-type>)
  constant slot llvm-primitive-type-kind :: <llvm-primitive-type-kind>,
    required-init-keyword: kind:;
end class;

define method type-partition-key
    (type :: <llvm-primitive-type>)
 => (key :: <vector>, splittable? :: <boolean>);
  values(vector(<llvm-primitive-type>, type.llvm-primitive-type-kind), #f)
end method;

define function llvm-void-type? (type :: <llvm-type>) => (void? :: <boolean>)
  instance?(type, <llvm-primitive-type>)
    & type.llvm-primitive-type-kind == #"VOID"
end function;

define class <llvm-integer-type> (<llvm-type>)
  constant slot llvm-integer-type-width :: <integer>,
    required-init-keyword: width:;
end class;

define method type-partition-key
    (type :: <llvm-integer-type>)
 => (key :: <vector>, splittable? :: <boolean>);
  values(vector(<llvm-integer-type>, type.llvm-integer-type-width), #f)
end method;

define class <llvm-pointer-type> (<llvm-type>)
  constant slot llvm-pointer-type-pointee :: <llvm-type>,
    required-init-keyword: pointee:;
  constant slot llvm-pointer-type-address-space :: <integer>,
    init-value: 0, init-keyword: address-space:;
end class;

define method type-partition-key
    (type :: <llvm-pointer-type>)
 => (key :: <vector>, splittable? :: <boolean>);
  values(vector(<llvm-pointer-type>, type.llvm-pointer-type-address-space), #t)
end method;

define method type-referenced-types
    (type :: <llvm-pointer-type>)
 => (types :: <vector>);
  vector(type.llvm-pointer-type-pointee);
end method;

define class <llvm-function-type> (<llvm-type>)
  constant slot llvm-function-type-return-type :: <llvm-type>,
    required-init-keyword: return-type:;
  constant slot llvm-function-type-parameter-types :: <sequence>,
    required-init-keyword: parameter-types:;
  constant slot llvm-function-type-varargs? :: <boolean>,
    required-init-keyword: varargs?:;
end class;

define method type-partition-key
    (type :: <llvm-function-type>)
 => (key :: <vector>, splittable? :: <boolean>);
  values(vector(<llvm-function-type>,
                type.llvm-function-type-varargs?,
                type.llvm-function-type-parameter-types.size),
         #t)
end method;

define method type-referenced-types
    (type :: <llvm-function-type>)
 => (types :: <vector>);
  let parameter-types = type.llvm-function-type-parameter-types;
  let types = make(<simple-object-vector>, size: parameter-types.size + 1);
  types[0] := type.llvm-function-type-return-type;
  replace-subsequence!(types, parameter-types, start: 1)
end method;

define abstract class <llvm-aggregate-type> (<llvm-type>)
end class;

define class <llvm-struct-type> (<llvm-aggregate-type>)
  constant slot llvm-struct-type-packed? :: <boolean>,
    init-value: #f, init-keyword: packed?:;
  constant slot llvm-struct-type-elements :: <sequence>,
    required-init-keyword: elements:;
end class;

define method type-partition-key
    (type :: <llvm-struct-type>)
 => (key :: <vector>, splittable? :: <boolean>);
  values(vector(<llvm-struct-type>,
                type.llvm-struct-type-packed?,
                type.llvm-struct-type-elements.size),
         #t)
end method;

define method type-referenced-types
    (type :: <llvm-struct-type>)
 => (types :: <vector>);
  as(<vector>, type.llvm-struct-type-elements)
end method;

define class <llvm-union-type> (<llvm-aggregate-type>)
  constant slot llvm-union-type-elements :: <sequence>,
    required-init-keyword: elements:;
end class;

define method type-partition-key
    (type :: <llvm-union-type>)
 => (key :: <vector>, splittable? :: <boolean>);
  values(vector(<llvm-union-type>,
                type.llvm-union-type-elements.size),
         #t)
end method;

define method type-referenced-types
    (type :: <llvm-union-type>)
 => (types :: <vector>);
  as(<vector>, type.llvm-union-type-elements)
end method;

define class <llvm-array-type> (<llvm-aggregate-type>)
  constant slot llvm-array-type-size :: <abstract-integer>,
    required-init-keyword: size:;
  constant slot llvm-array-type-element-type :: <llvm-type>,
    required-init-keyword: element-type:;
end class;

define method type-partition-key
    (type :: <llvm-array-type>)
 => (key :: <vector>, splittable? :: <boolean>);
  values(vector(<llvm-array-type>, type.llvm-array-type-size), #t)
end method;

define method type-referenced-types
    (type :: <llvm-array-type>)
 => (types :: <vector>);
  vector(type.llvm-array-type-element-type);
end method;

define class <llvm-vector-type> (<llvm-type>)
  constant slot llvm-vector-type-size :: <integer>,
    required-init-keyword: size:;
  constant slot llvm-vector-type-element-type :: <llvm-type>,
    required-init-keyword: element-type:;
end class;

define method type-partition-key
    (type :: <llvm-vector-type>)
 => (key :: <vector>, splittable? :: <boolean>);
  values(vector(<llvm-vector-type>, type.llvm-vector-type-size), #t)
end method;

define method type-referenced-types
    (type :: <llvm-vector-type>)
 => (types :: <vector>);
  vector(type.llvm-vector-type-element-type);
end method;


/// Placeholder types

define abstract class <llvm-placeholder-type> (<llvm-type>)
  slot llvm-placeholder-type-forward :: <llvm-type>;
end class;

define method llvm-constrain-type
    (constrained-type :: <llvm-type>, type :: <llvm-type>) => ()
  let real-type = type-forward(type);
  if (type-partition-key(constrained-type) ~= type-partition-key(real-type))
    error("Type %s does not match %s", constrained-type, type);
  end if;
end method;

define method llvm-constrain-type
    (constrained-type :: <llvm-placeholder-type>, type :: <llvm-type>) => ()
  if (slot-initialized?(constrained-type, llvm-placeholder-type-forward))
    llvm-constrain-type(constrained-type.llvm-placeholder-type-forward, type);
  elseif (constrained-type ~== type)
    constrained-type.llvm-placeholder-type-forward := type;
  end if;
end method;

define method llvm-constrain-type
    (constrained-type :: <llvm-type>, type :: <llvm-placeholder-type>) => ()
  if (slot-initialized?(type, llvm-placeholder-type-forward))
    llvm-constrain-type(constrained-type, type.llvm-placeholder-type-forward);
  elseif (constrained-type ~== type)
    type.llvm-placeholder-type-forward := constrained-type;
  end if;
end method;

define method llvm-constrain-type
    (constrained-type :: <llvm-placeholder-type>,
     type :: <llvm-placeholder-type>)
 => ()
  if (slot-initialized?(constrained-type, llvm-placeholder-type-forward))
    llvm-constrain-type(constrained-type.llvm-placeholder-type-forward, type);
  elseif (slot-initialized?(type, llvm-placeholder-type-forward))
    llvm-constrain-type(constrained-type, type.llvm-placeholder-type-forward);
  end if;
end method;

// Upval types

define class <llvm-upval-type> (<llvm-placeholder-type>)
  constant slot llvm-upval-type-index :: <integer>,
    required-init-keyword: index:;
end class;

define method llvm-type-resolve-upvals
    (root-type :: <llvm-type>)
 => (root-type :: <llvm-type>);
  local
    method traverse-type (type :: <llvm-type>, up-types :: <list>) => ();
      if (instance?(type, <llvm-upval-type>))
        let index = type.llvm-upval-type-index;
        type.llvm-placeholder-type-forward := up-types[index - 1];
      elseif (~instance?(type, <llvm-placeholder-type>))
        let inner-up-types = pair(type, up-types);
        for (referenced-type in type-referenced-types(type))
          traverse-type(referenced-type, inner-up-types);
        end for;
      end if;
    end method;

  traverse-type(root-type, #());
  root-type
end method;

define method type-forward
    (type :: <llvm-upval-type>)
 => (type :: <llvm-type>);
  if (slot-initialized?(type, llvm-placeholder-type-forward))
    type-forward(type.llvm-placeholder-type-forward)
  else
    error("upval \\%d was not resolved", type.llvm-upval-type-index);
  end if
end method;

define class <llvm-symbolic-type> (<llvm-placeholder-type>)
  constant slot llvm-symbolic-type-name :: type-union(<string>, <integer>),
    required-init-keyword: name:;
end class;

define method type-forward
    (type :: <llvm-symbolic-type>)
 => (type :: <llvm-type>);
  if (slot-initialized?(type, llvm-placeholder-type-forward))
    type-forward(type.llvm-placeholder-type-forward)
  else
    error("type %%%s was not resolved", type.llvm-symbolic-type-name);
  end if
end method;

define class <llvm-opaque-type> (<llvm-placeholder-type>)
end class;

define method type-partition-key
    (type :: <llvm-opaque-type>)
 => (key :: <vector>, splittable? :: <boolean>);
  values(vector(<llvm-opaque-type>, type), #f)
end method;

define method type-forward
    (type :: <llvm-opaque-type>)
 => (type :: <llvm-type>);
  if (slot-initialized?(type, llvm-placeholder-type-forward))
    type-forward(type.llvm-placeholder-type-forward)
  else
    type
  end if
end method;


/// Well-known types

define constant $llvm-label-type :: <llvm-type>
  = make(<llvm-primitive-type>, kind: #"LABEL");
define constant $llvm-void-type :: <llvm-type>
  = make(<llvm-primitive-type>, kind: #"VOID");
define constant $llvm-metadata-type :: <llvm-type>
  = make(<llvm-primitive-type>, kind: #"METADATA");
define constant $llvm-float-type :: <llvm-type>
  = make(<llvm-primitive-type>, kind: #"FLOAT");
define constant $llvm-double-type :: <llvm-type>
  = make(<llvm-primitive-type>, kind: #"DOUBLE");

define constant $llvm-i1-type :: <llvm-type>
  = make(<llvm-integer-type>, width: 1);

define constant $llvm-i8-type :: <llvm-type>
  = make(<llvm-integer-type>, width: 8);
define constant $llvm-i8*-type
  = make(<llvm-pointer-type>, pointee: $llvm-i8-type);

define constant $llvm-i16-type :: <llvm-type>
  = make(<llvm-integer-type>, width: 16);
define constant $llvm-i32-type :: <llvm-type>
  = make(<llvm-integer-type>, width: 32);
define constant $llvm-i64-type :: <llvm-type>
  = make(<llvm-integer-type>, width: 64);
