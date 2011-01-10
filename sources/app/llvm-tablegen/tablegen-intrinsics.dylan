Module:       llvm-tablegen
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function llvm-type-overloaded?
    (llvm-type :: <tablegen-definition>) => (result? :: <boolean>);
  let vt = record-field-value(llvm-type, "VT");
  vt == $tablegen-definitions["iAny"]
    | vt == $tablegen-definitions["fAny"]
    | vt == $tablegen-definitions["vAny"]
    | vt == $tablegen-definitions["iPTRAny"]
end function;

define function llvm-type-void?
    (llvm-type :: <tablegen-definition>) => (result? :: <boolean>);
  let vt = record-field-value(llvm-type, "VT");
  vt == $tablegen-definitions["isVoid"]
end function;

define table $valuetype-map :: <string-table>
  = {
     "i1"         => "$llvm-i1-type",
     "i8"         => "$llvm-i8-type",
     "i16"        => "$llvm-i16-type",
     "i32"        => "$llvm-i32-type",
     "i64"        => "$llvm-i64-type",
     "f32"        => "$llvm-float-type",
     "f64"        => "$llvm-double-type",
     "MetadataVT" => "$llvm-metadata-type",
     "OtherVT"    => "make(<llvm-struct-type>, elements: vector())",
    };

define function gen-type
    (llvm-type :: <tablegen-definition>, overload-index :: false-or(<integer>))
  if (tablegen-subclass?(llvm-type, "LLVMMatchType"))
    let number = record-field-value(llvm-type, "Number");
    if (tablegen-subclass?(llvm-type, "LLVMExtendedElementVectorType"))
      // Vector type twice the size of the specified vector type
      format(*standard-output*, "extend-vector-type(type%d)", number);
    elseif (tablegen-subclass?(llvm-type, "LLVMTruncatedElementVectorType"))
      // Vector type half the size of the specified vector type
      format(*standard-output*, "truncate-vector-type(type%d)", number);
    else
      // Type matching previously specified one
      format(*standard-output*, "type%d", number);
    end if;
  elseif (llvm-type-overloaded?(llvm-type))
    // "Any" integer/float/vector/pointer type
    format(*standard-output*, "type%d", overload-index);
  elseif (tablegen-subclass?(llvm-type, "LLVMPointerType"))
    // Pointer type
    let element-type = record-field-value(llvm-type, "ElTy");
    if (record-field-value(element-type, "VT") == $tablegen-definitions["i8"])
      // Pointer to i8 already has a canonical name
      format(*standard-output*, "$llvm-i8*-type");
    else
      // Pointer to some other type
      format(*standard-output*, "make(<llvm-pointer-type>, pointee: ");
      gen-type(element-type, #f);
      format(*standard-output*, ")");
    end if;
  else
    // Value types
    let vt-name = record-field-value(llvm-type, "VT").record-name;
    if (vt-name[0] == 'v')
      // Vector type: v{n}{vt}
      let (vector-size, start) = string-to-integer(vt-name, start: 1);
      let element-vt-name = copy-sequence(vt-name, start: start);
      format(*standard-output*,
             "make(<llvm-vector-type>, size: %d, element-type: %s)",
             vector-size, $valuetype-map[element-vt-name]);
    else
      write(*standard-output*, $valuetype-map[vt-name]);
    end if;
  end if;
end function;

define function gen-return-type
    (return-types :: <sequence>, override-indices :: <sequence>)
  if (empty?(return-types))
    // No return types
    format(*standard-output*, "$llvm-void-type")
  elseif (size(return-types) = 1)
    // One return type
    gen-type(return-types[0], override-indices[0]);
  else
    // Multiple return types: return a struct
    format(*standard-output*, "make(<llvm-struct-type>, elements: vector(");
    for (type in return-types, index in override-indices, first? = #t then #f)
      unless (first?) write(*standard-output*, ", ") end;
      gen-type(type, index);
    end for;
    format(*standard-output*, "))")
  end if;
end;

define function gen-parameter-types
    (parameter-types :: <sequence>, override-indices :: <sequence>)
  format(*standard-output*, "vector(");
  for (type in parameter-types, index in override-indices, first? = #t then #f)
    unless (first?) write(*standard-output*, ", ") end;
    gen-type(type, index)
  end for;
  format(*standard-output*, ")");
end;

define function gen-attribute-list
    (options :: <sequence>, parameter-list-size :: <integer>)
  let function-attributes
    = "default";
  let parameter-attributes
    = make(<vector>, size: parameter-list-size, fill: "$llvm-attribute-none");
  // Process the supplied options
  for (option in options)
    if (option.record-name = "IntrNoMem")
      function-attributes := "readnone";
    elseif (option.record-name = "IntrReadArgMem"
              | option.record-name = "IntrReadMem")
      function-attributes := "readonly";
    elseif (tablegen-subclass?(option, "NoCapture"))
      let argument = record-field-value(option, "ArgNo");
      parameter-attributes[argument] := "$llvm-attribute-nocapture";
    end if;
  end for;

  if (every?(curry(\=, "$llvm-attribute-none"), parameter-attributes))
    // If there are no parameter attributes, use one of the
    // pre-computed argument lists
    format(*standard-output*, "$llvm-intrinsic-%s-attribute-list",
           function-attributes);
  else
    // Otherwise explicitly construct a new attribute list
    format(*standard-output*, "make(<llvm-attribute-list>, ");
    format(*standard-output*, "function-attributes: ");
    format(*standard-output*, "$llvm-intrinsic-%s-function-attributes, ",
           function-attributes);
    format(*standard-output*, "parameter-attributes: vector(");
    for (parameter-attribute in parameter-attributes, first? = #t then #f)
      unless (first?) write(*standard-output*, ", ") end;
      write(*standard-output*, parameter-attribute);
    end for;
    write(*standard-output*, "))");
  end if;
end;

define function gen-overloaded
    (name :: <string>,
     return-types :: <sequence>,
     parameter-types :: <sequence>,
     varargs? :: <boolean>,
     properties :: <sequence>)
 => ();
  // Overloaded intrinsics: function with parameter types computed
  // based on the types of supplied arguments

  format(*standard-output*, "  $llvm-intrinsic-makers[\"%s\"]\n", name);
  format(*standard-output*, "    := method (arguments)\n");

  // Assign overloaded return value types
  let return-overload-indices = make(<vector>, size: return-types.size);
  let overload-index
    = iterate return-loop (index = 0, overload-index = 0)
        if (index < return-types.size)
          let type = return-types[index];
          if (llvm-type-overloaded?(type))
            // "Any" types in the return value list cannot be computed yet;
            // assign a placeholder type
            format(*standard-output*,
                   "         let type%d = make(<llvm-opaque-type>);\n",
                   overload-index);
            return-overload-indices[index] := overload-index;
            return-loop(index + 1, overload-index + 1)
          else
            // Other (concrete) type elements can be ignored
            return-loop(index + 1, overload-index)
          end if;
        else
          overload-index
        end if
      end iterate;

  // Assign overloaded parameter value types
  let parameter-overload-indices = make(<vector>, size: parameter-types.size);
  let overload-index
    = iterate parameter-loop (index = 0, overload-index = overload-index)
        if (index < parameter-types.size)
          let type = parameter-types[index];
          if (llvm-type-overloaded?(type))
            // "Any" types in the parameter list can be computed based
            // on the value type of the supplied parameter
            format(*standard-output*,
                   "         let type%d = llvm-value-type(arguments[%d]);\n",
                   overload-index, index);
            parameter-overload-indices[index] := overload-index;
            parameter-loop(index + 1, overload-index + 1)
          elseif (tablegen-subclass?(type, "LLVMMatchType")
                    & ~tablegen-subclass?(type,
                                          "LLVMExtendedElementVectorType")
                    & ~tablegen-subclass?(type,
                                          "LLVMTruncatedElementVectorType"))
            // Match types in the parameter list generate a constraint
            // to match against the value type of the supplied
            // parameter
            format(*standard-output*, "         llvm-constrain-type(type%d, ",
                   record-field-value(type, "Number"));
            format(*standard-output*, "llvm-value-type(arguments[%d]));\n",
                   index);
            parameter-loop(index + 1, overload-index)
          else
            // Other (concrete) type elements can be ignored
            parameter-loop(index + 1, overload-index)
          end if;
        else
          overload-index
        end if
      end iterate;

  // To compute the name of an overloaded intrinsic, we need a format
  // string and type name string arguments
  format(*standard-output*, "         let name = format-to-string(\"%s", name);

  // Each "Any" type contributes a format directive
  for (i from 0 below overload-index)
    write(*standard-output*, ".%s");
  end for;
  write(*standard-output*, "\"");

  // Each "Any" type also contributes an argument
  for (i from 0 below overload-index)
    format(*standard-output*, ", intrinsic-type-name(type%d)", i);
  end for;
  write(*standard-output*, ");\n\n");

  // Compute the function type
  format(*standard-output*, "         let function-type\n");
  format(*standard-output*, "           = make(<llvm-function-type>,\n");

  format(*standard-output*, "                  return-type: ");
  gen-return-type(return-types, return-overload-indices);
  format(*standard-output*, ",\n");

  format(*standard-output*, "                  parameter-types: ");
  gen-parameter-types(parameter-types, parameter-overload-indices);
  format(*standard-output*, ",\n");

  format(*standard-output*, "                  varargs?: ");
  write(*standard-output*, if (varargs?) "#t" else "#f" end);
  format(*standard-output*, ");\n");

  // Compute the function object
  format(*standard-output*, "         make(<llvm-function>,\n");
  format(*standard-output*, "              name: name,\n");
  format(*standard-output*, "              type: make(<llvm-pointer-type>, pointee: function-type),\n");
  format(*standard-output*, "              attribute-list: ");
  gen-attribute-list(properties, parameter-types.size);
  format(*standard-output*, ",\n");
  format(*standard-output*, "              linkage: #\"external\");\n");
  format(*standard-output*, "       end;\n\n");
end function;

define function gen-non-overloaded
    (name :: <string>,
     return-types :: <sequence>,
     parameter-types :: <sequence>,
     varargs? :: <boolean>,
     properties :: <sequence>)
 => ();
  // Non-overloaded intrinsics: single pre-computed function
  format(*standard-output*, "  $llvm-intrinsic-makers[\"%s\"]\n", name);
  format(*standard-output*, "    := begin\n");
  format(*standard-output*, "         let function-type\n");
  format(*standard-output*, "           = make(<llvm-function-type>,\n");

  format(*standard-output*, "                  return-type: ");
  gen-return-type(return-types, make(<vector>, size: return-types.size));
  format(*standard-output*, ",\n");

  format(*standard-output*, "                  parameter-types: ");
  gen-parameter-types(parameter-types,
                      make(<vector>, size: parameter-types.size));
  format(*standard-output*, ",\n");

  format(*standard-output*, "                  varargs?: ");
  write(*standard-output*, if (varargs?) "#t" else "#f" end);
  format(*standard-output*, ");\n");

  format(*standard-output*, "         let function\n");
  format(*standard-output*, "           = make(<llvm-function>,\n");
  format(*standard-output*, "                  name: \"%s\",\n", name);
  format(*standard-output*, "                  type: make(<llvm-pointer-type>, pointee: function-type),\n");

  format(*standard-output*, "                  attribute-list: ");
  gen-attribute-list(properties, parameter-types.size);
  format(*standard-output*, ",\n");

  format(*standard-output*, "                  linkage: #\"external\");\n");
  format(*standard-output*, "         method (arguments) function end\n");
  format(*standard-output*, "       end;\n\n");
end function;

define function tablegen-gen-intrinsic () => ();
  format(*standard-output*, "begin\n");

  for (def-name in sort(key-sequence($tablegen-definitions)))
    let def = $tablegen-definitions[def-name];
    if (tablegen-subclass?(def, "Intrinsic"))
      // Target prefix
      let target-prefix = record-field-value(def, "TargetPrefix", default: "");

      // FIXME: For now, only generate target-independent intrinsics,
      // and target-dependent intrinsics that we know we need
      if (empty?(target-prefix)
            | def-name = "int_x86_int")
        // Intrinsic name
        let llvm-name = record-field-value(def, "LLVMName", default: "");
        let name
          = if (empty?(llvm-name))
              concatenate("llvm.",
                          map(method (c)
                                if (c == '_') '.' else c end
                              end,
                              copy-sequence(def-name, start: 4)))
            else
              llvm-name
            end if;

        // Return types
        let return-types = record-field-value(def, "RetTypes");

        // Parameter types
        let param-types = record-field-value(def, "ParamTypes");
        let (parameter-types, varargs?)
          = if (~empty?(param-types) & llvm-type-void?(param-types.last))
              values(copy-sequence(param-types, end: param-types.size - 1), #t)
            else
              values(param-types, #f)
            end if;

        // Properties
        let properties = record-field-value(def, "Properties");

        if (any?(llvm-type-overloaded?, parameter-types)
              | any?(llvm-type-overloaded?, return-types))
          gen-overloaded
            (name, return-types, parameter-types, varargs?, properties);
        else
          gen-non-overloaded
            (name, return-types, parameter-types, varargs?, properties);
        end if;
      end if;
    end if;
  end for;

  format(*standard-output*, "end;\n");
end function;
