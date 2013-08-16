Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2011 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// FIXME get this from release-info
define constant $debug-producer = "Open Dylan 1.0";

define method llvm-compilation-record-dbg-compile-unit
    (back-end :: <llvm-back-end>, cr :: <compilation-record>)
 => ();
  let sr = cr.compilation-record-source-record;
  let location = sr.source-record-location;
  let functions = copy-sequence(back-end.llvm-back-end-dbg-functions);
  llvm-make-dbg-compile-unit($DW-LANG-Dylan,
                             location.locator-name,
                             location.locator-directory,
                             $debug-producer,
                             functions: functions,
                             module: back-end.llvm-builder-module);
end method;

define method llvm-source-record-dbg-file
    (back-end :: <llvm-back-end>, sr :: <source-record>)
 => (dbg-file :: <llvm-metadata-value>);
  element(back-end.%source-record-dbg-file-table, sr, default: #f)
    | begin
        let location = source-record-location(sr);
        back-end.%source-record-dbg-file-table[sr]
          := llvm-make-dbg-file(location.locator-name,
                                location.locator-directory)
      end
end method;

define method emit-lambda-dbg-function
    (back-end :: <llvm-back-end>, o :: <&iep>) => ()
  let fun = function(o);
  let signature = ^function-signature(fun);
  let sig-spec = signature-spec(fun);

  // Compute the source location
  let loc = fun.model-source-location;
  let (dbg-file, dbg-line)
    = if (instance?(loc, <source-location>))
        source-location-dbg-file-line(back-end, loc)
      else
        let cr = fun.model-compilation-record;
        let sr = cr.compilation-record-source-record;
        values(llvm-source-record-dbg-file(back-end, sr), 0)
      end if;

  // Compute the function type
  let (return-type :: false-or(<llvm-metadata-value>),
       parameter-types :: <sequence>)
    = llvm-signature-dbg-types(back-end, o, sig-spec, signature);
  let dbg-function-type
    = llvm-make-dbg-function-type(dbg-file, return-type, parameter-types);

  // Construct the function metadata
  let module = back-end.llvm-builder-module;
  let function-name = o.code.llvm-global-name;
  let dbg-name = if (o.binding-name) as(<string>, o.binding-name) else "" end;
  let dbg-function
    = llvm-make-dbg-function(dbg-file,
                             dbg-name,
                             function-name,
                             dbg-file,
                             dbg-line,
                             dbg-function-type,
                             definition?: #t,
                             function: o.code);
  add!(back-end.llvm-back-end-dbg-functions, dbg-function);

  // Emit a llvm.dbg.value call for each parameter
  ins--dbg(back-end, dbg-line, 0, dbg-function, #f);
  for (index from 1, param in parameters(o), param-type in parameter-types)
    let v
      = make(<llvm-metadata-node>,
             function-local?: #t,
             node-values: list(temporary-value(param)));
    let lv
      = llvm-make-dbg-local-variable(#"argument",
                                     dbg-function,
                                     as(<string>, param.name),
                                     dbg-file, dbg-line,
                                     param-type,
                                     arg: index,
                                     module: module,
                                     function-name: function-name);
    ins--call-intrinsic(back-end, "llvm.dbg.value", vector(v, i64(0), lv));
  end for;
end method;

define method llvm-signature-dbg-types
    (back-end :: <llvm-back-end>, o :: <&iep>,
     sig-spec :: <signature-spec>, signature :: <&signature>)
 => (return-type :: false-or(<llvm-metadata-value>),
     parameter-types :: <sequence>);
  let obj-type = dylan-value(#"<object>");
  let parameter-types = make(<stretchy-object-vector>);

  // Required parameters
  for (type in ^signature-required(signature),
       i from 0 below ^signature-number-required(signature))
    add!(parameter-types, llvm-reference-dbg-type(back-end, type));
  end for;
  // Optional parameters
  if (^signature-optionals?(signature))
    add!(parameter-types, llvm-reference-dbg-type(back-end, obj-type));
  end if;
  // Keyword parameters
  for (spec in spec-argument-key-variable-specs(sig-spec))
    add!(parameter-types, llvm-reference-dbg-type(back-end, obj-type));
  end for;
  // Calling convention parameters (next-methods, function)
  add!(parameter-types, llvm-reference-dbg-type(back-end, obj-type));
  add!(parameter-types, llvm-reference-dbg-type(back-end, obj-type));

  let return-type
    = llvm-reference-dbg-type(back-end, back-end.%mv-struct-type);
  values(return-type, parameter-types)
end method;

define method llvm-reference-dbg-type
    (back-end :: <llvm-back-end>, o :: <&raw-struct-type>)
 => (dbg-type :: <llvm-metadata-value>);
  element(back-end.%dbg-type-table, o, default: #f)
    | (back-end.%dbg-type-table[o]
         := begin
              let placeholder = make(<llvm-symbolic-metadata>, name: 0);
              let (type-size, type-alignment, member-bit-offsets)
                = compute-raw-aggregate-layout(o);
              let elements
                = map(method (member, index)
                        let member-type = member.member-raw-type;
                        let member-size = raw-type-size(member-type);
                        let member-alignment = raw-type-alignment(member-type);
                        let member-dbg-type
                          = llvm-reference-dbg-type(back-end, member-type);
                        let member-offset = member-bit-offsets[member];
                        llvm-make-dbg-derived-type
                          (#"member", placeholder,
                           format-to-string("F%d", index),
                           #f, #f, 8 * member-size, 8 * member-alignment,
                           member-offset, member-dbg-type)
                      end,
                      o.raw-aggregate-members, range(from: 0));
              let aggregate
                = llvm-make-dbg-composite-type
                    (#"struct", #f,
                     o.^debug-name | "",
                     #f, #f, type-size, type-alignment, elements, #f);
              placeholder.llvm-placeholder-value-forward := aggregate
            end)
end method;

define method llvm-reference-dbg-type
    (back-end :: <llvm-back-end>, o :: <&raw-type>)
 => (dbg-type :: <llvm-metadata-value>);
  element(back-end.%dbg-type-table, o, default: #f)
    | (back-end.%dbg-type-table[o]
         := begin
              let type-name = o.^debug-name;
              let type-size = o.raw-type-size;
              let type-alignment = o.raw-type-alignment;
              let kind = back-end.%raw-type-dbg-encoding-table[o];
              if (kind == #"pointer")
                llvm-make-dbg-derived-type
                  (kind, #f,
                   type-name, #f, #f, 8 * type-size, 8 * type-alignment, 0, #f)
              else
                llvm-make-dbg-basic-type
                  (kind, #f,
                   type-name, 8 * type-size, 8 * type-alignment, 0)
              end if
            end)
end method;

define method llvm-reference-dbg-type
    (back-end :: <llvm-back-end>, o :: <&type>)
 => (dbg-type :: <llvm-metadata-value>);
  let obj-type = dylan-value(#"<object>");
  let word-size = back-end-word-size(back-end);
  element(back-end.%dbg-type-table, obj-type, default: #f)
    | (back-end.%dbg-type-table[obj-type]
         := llvm-make-dbg-derived-type(#"pointer",
                                       #f,
                                       "<object>",
                                       #f, #f,
                                       8 * word-size, 8 * word-size, 0,
                                       #f))
end method;


/// Computation source line tracking

define function op--scl(back-end :: <llvm-back-end>, c :: <computation>) => ()
  let loc = dfm-source-location(c);
  if (instance?(loc, <source-location>))
    let sr = source-location-source-record(loc);
    let start-offset = source-location-start-offset(loc);
    let start-line = source-offset-line(start-offset);
    ins--dbg(back-end, start-line + source-record-start-line(sr), 0,
             back-end.llvm-back-end-dbg-functions.last, #f);
  end if;
end function;

define function source-location-dbg-file-line
    (back-end :: <llvm-back-end>, loc :: <source-location>)
 => (dbg-file :: <llvm-metadata-value>, dbg-line :: <integer>)
  let sr = source-location-source-record(loc);
  let start-offset = source-location-start-offset(loc);
  let start-line = source-offset-line(start-offset);
  values(llvm-source-record-dbg-file(back-end, sr),
         start-line + source-record-start-line(sr))
end function;
