Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2011 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method llvm-compilation-record-dbg-compile-unit
    (back-end :: <llvm-back-end>, cr :: <compilation-record>)
 => (dbg-compile-unit :: <llvm-metadata>);
  let sr = cr.compilation-record-source-record;
  let dbg-file = llvm-source-record-dbg-file(back-end, sr);
  llvm-make-dbg-compile-unit($DW-LANG-Dylan,
                             dbg-file,
                             release-full-name(),
                             module: back-end.llvm-builder-module)
end method;

define method llvm-source-record-dbg-file
    (back-end :: <llvm-back-end>, sr :: <source-record>)
 => (dbg-file :: <llvm-metadata>);
  element(back-end.%source-record-dbg-file-table, sr, default: #f)
    | begin
        let location = source-record-location(sr);
        back-end.%source-record-dbg-file-table[sr]
          := llvm-make-dbg-file(location.locator-name,
                                location.locator-directory)
      end
end method;

define method llvm-lambda-dbg-function
    (back-end :: <llvm-back-end>, fun :: <&lambda>)
 => (dbg-function :: <llvm-metadata>);
  element(back-end.%lambda-dbg-function-table, fun, default: #f)
    | begin
        let o = fun.^iep;
        let c-callable? = instance?(fun, <&c-callable-function>);
        let signature = ^function-signature(fun);
        let sig-spec = signature-spec(fun);

        // Compute the source location
        let loc = fun.model-source-location;
        let (dbg-file, dbg-line, dbg-column)
          = if (instance?(loc, <source-location>))
              source-location-dbg-loc(back-end, loc)
            else
              let cr = fun.model-compilation-record;
              let sr = cr.compilation-record-source-record;
              values(llvm-source-record-dbg-file(back-end, sr), 0, 0)
            end if;

        // Compute the function type
        let (dbg-return-type :: false-or(<llvm-metadata>),
             dbg-parameter-types :: <sequence>)
          = if (signature)
              llvm-signature-dbg-types(back-end, o, sig-spec, signature)
            else
              llvm-dynamic-signature-dbg-types(back-end, o, sig-spec)
            end if;

        let obj-dbg-type
          = llvm-reference-dbg-type(back-end, dylan-value(#"<object>"));
        let dbg-calling-convention-parameter-types
          = if (c-callable?)
              #[]
            else
              vector(obj-dbg-type,      // next-methods
                     obj-dbg-type)      // function
            end if;

        let dbg-function-parameter-types
          = if (o.parameters.size > $entry-point-argument-count)
              concatenate(copy-sequence(dbg-parameter-types,
                                        end: $entry-point-argument-count),
                          vector(llvm-dbg-pointer-to(back-end, obj-dbg-type)),
                          dbg-calling-convention-parameter-types)
            else
              concatenate(dbg-parameter-types,
                          dbg-calling-convention-parameter-types)
            end if;
        let dbg-function-type
          = llvm-make-dbg-function-type(dbg-file, dbg-return-type,
                                        dbg-function-parameter-types);

        // Construct the function metadata
        let dbg-name
          = if (o.binding-name) as(<string>, o.binding-name) else "" end;
        let definition?
          = model-compilation-record(fun) == current-compilation-record();
        element(back-end.%lambda-dbg-function-table, fun)
          := llvm-make-dbg-function(dbg-file,
                                    dbg-name,
                                    "",
                                    back-end.llvm-back-end-dbg-compile-unit,
                                    dbg-file,
                                    dbg-line,
                                    dbg-function-type,
                                    definition?: #t,
                                    module: back-end.llvm-builder-module,
                                    function: o.code)
      end
end method;

define method emit-lambda-dbg-function
    (back-end :: <llvm-back-end>, o :: <&iep>) => ()
  let fun = function(o);
  let c-callable? = instance?(fun, <&c-callable-function>);
  let dbg-function = llvm-lambda-dbg-function(back-end, fun);

  // Compute the source location
  let loc = fun.model-source-location;
  let (dbg-file, dbg-line, dbg-column)
    = if (instance?(loc, <source-location>))
        source-location-dbg-loc(back-end, loc)
      else
        let cr = fun.model-compilation-record;
        let sr = cr.compilation-record-source-record;
        values(llvm-source-record-dbg-file(back-end, sr), 0, 0)
      end if;

  let signature = ^function-signature(fun);
  let sig-spec = signature-spec(fun);
  let (dbg-return-type :: false-or(<llvm-metadata>),
       dbg-parameter-types :: <sequence>)
    = if (signature)
        llvm-signature-dbg-types(back-end, o, sig-spec, signature)
      else
        llvm-dynamic-signature-dbg-types(back-end, o, sig-spec)
      end if;

  // Emit a llvm.dbg.value call for each parameter
  // FIXME handle "extra" parameters
  ins--dbg(back-end, dbg-line, 0, dbg-function);
  for (index from 1, param in parameters(o),
       dbg-param-type in dbg-parameter-types)
    let v = llvm-make-dbg-value-metadata(temporary-value(param));
    let lv
      = llvm-make-dbg-local-variable(#"argument",
                                     dbg-function,
                                     as(<string>, param.name),
                                     dbg-file, dbg-line,
                                     dbg-param-type,
                                     arg: index);
    let lvv = make(<llvm-metadata-value>, metadata: lv);
    ins--call-intrinsic(back-end, "llvm.dbg.value",
                        vector(v, lvv, $empty-diexpression-value));
  end for;

  // Emit a llvm.dbg.value call for each of the calling convention
  // parameters
  local
    method artificial-parameter(name :: <string>, index :: <integer>)
      let v = llvm-make-dbg-value-metadata(llvm-builder-local(back-end, name));
      let obj-dbg-type
        = llvm-reference-dbg-type(back-end, dylan-value(#"<object>"));
      let lv
        = llvm-make-dbg-local-variable(#"argument",
                                       dbg-function,
                                       name,
                                       dbg-file, dbg-line,
                                       obj-dbg-type,
                                       arg: index + 1,
                                       artificial?: #t);
      let lvv = make(<llvm-metadata-value>, metadata: lv);
      ins--call-intrinsic(back-end, "llvm.dbg.value",
                          vector(v, lvv, $empty-diexpression-value));
    end method;
  unless (c-callable?)
    let calling-convention-index = parameters(o).size;
    artificial-parameter($next-methods-parameter-name,
                         calling-convention-index);
    artificial-parameter($function-parameter-name,
                         calling-convention-index + 1);
  end unless;

  // Assign debug scopes to each computation
  assign-computations-dbg-scope(back-end, dbg-function, o.body, #f);
end method;

define method llvm-signature-dbg-types
    (back-end :: <llvm-back-end>, o :: <&iep>,
     sig-spec :: <signature-spec>, signature :: <&signature>)
 => (return-type :: false-or(<llvm-metadata>),
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

  let return-type
    = llvm-reference-dbg-type(back-end, back-end.%mv-struct-type);
  values(return-type, parameter-types)
end method;

define method llvm-dynamic-signature-dbg-types
    (back-end :: <llvm-back-end>, o :: <&iep>,
     sig-spec :: <signature-spec>)
 => (return-type :: false-or(<llvm-metadata>),
     parameter-types :: <sequence>);
  let parameter-types = make(<stretchy-object-vector>);
  let obj-dbg-type
    = llvm-reference-dbg-type(back-end, dylan-value(#"<object>"));

  // Required parameters
  for (spec in spec-argument-required-variable-specs(sig-spec))
    add!(parameter-types, obj-dbg-type);
  end for;
  // Optional parameters
  if (spec-argument-optionals?(sig-spec))
    add!(parameter-types, obj-dbg-type);
  end if;
  // Keyword parameters
  for (spec in spec-argument-key-variable-specs(sig-spec))
    add!(parameter-types, obj-dbg-type);
  end for;

  let return-type
    = llvm-reference-dbg-type(back-end, back-end.%mv-struct-type);
  values(return-type, parameter-types)
end method;


/// Local variable declarations

define method emit-dbg-local-variable
    (back-end :: <llvm-back-end>, c :: <computation>, tmp :: <temporary>,
     kind :: one-of(#"auto", #"argument", #"return"),
     value :: <llvm-value>, #key address? = #f)
 => ();
  let dbg-scope = llvm-builder-dbg-scope(back-end);
  let dbg-line = llvm-builder-dbg-line(back-end);
  let dbg-file = llvm-builder-dbg-file(back-end);

  let var-type
    = llvm-reference-dbg-type(back-end, type-estimate(tmp));
  let v = llvm-make-dbg-value-metadata(value);
  let lv
    = llvm-make-dbg-local-variable(kind,
                                   dbg-scope,
                                   as(<string>, tmp.name),
                                   dbg-file, dbg-line,
                                   var-type);
  let lvv = make(<llvm-metadata-value>, metadata: lv);
  if (address?)
    ins--call-intrinsic(back-end, "llvm.dbg.addr",
                        vector(v, lvv, $empty-diexpression-value));
  else
    ins--call-intrinsic(back-end, "llvm.dbg.value",
                        vector(v, lvv, $empty-diexpression-value));
  end if;
end method;

define method llvm-reference-dbg-type
    (back-end :: <llvm-back-end>, o :: <&raw-struct-type>)
 => (dbg-type :: <llvm-metadata>);
  element(back-end.%dbg-type-table, o, default: #f)
    | (back-end.%dbg-type-table[o]
         := begin
              let dummy-file = llvm-make-dbg-file("dummy-raw-struct.dylan", "");
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
                           dummy-file, #f, 8 * member-size, 8 * member-alignment,
                           member-offset, member-dbg-type)
                      end,
                      o.raw-aggregate-members, range(from: 0));
              let aggregate
                = llvm-make-dbg-composite-type
                    (#"struct", #f,
                     o.^debug-name | "",
                     dummy-file, #f, type-size, type-alignment, elements, #f);
              placeholder.llvm-symbolic-metadata-forward := aggregate
            end)
end method;

define method llvm-reference-dbg-type
    (back-end :: <llvm-back-end>, o :: <&raw-type>)
 => (dbg-type :: <llvm-metadata>);
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

define function llvm-dbg-pointer-to
    (back-end :: <llvm-back-end>, dbg-type :: false-or(<llvm-metadata>))
 => (dbg-type :: <llvm-metadata>);
  let word-size = back-end-word-size(back-end);
  llvm-make-dbg-derived-type(#"pointer",
                             #f,
                             "",
                             #f, #f,
                             8 * word-size, 8 * word-size, 0,
                             #f)
end function;

define method llvm-reference-dbg-type
    (back-end :: <llvm-back-end>, o :: <&type>)
 => (dbg-type :: <llvm-metadata>);
  let obj-type = dylan-value(#"<object>");
  let word-size = back-end-word-size(back-end);
  element(back-end.%dbg-type-table, obj-type, default: #f)
    | begin
        let dummy-file = llvm-make-dbg-file("dummy-objects.dylan", "");
        let pointer-type = llvm-dbg-pointer-to(back-end, #f);
        back-end.%dbg-type-table[obj-type]
         := llvm-make-dbg-derived-type(#"typedef",
                                       #f,
                                       "dylan_value",
                                       dummy-file, #f,
                                       0, 0, 0,
                                       pointer-type)
      end
end method;

define method llvm-reference-dbg-type
    (back-end :: <llvm-back-end>, o :: <type-estimate>)
 => (dbg-type :: <llvm-metadata>);
  llvm-reference-dbg-type(back-end, dylan-value(#"<object>"))
end method;

define method llvm-reference-dbg-type
    (back-end :: <llvm-back-end>, o :: <type-estimate-raw>)
 => (dbg-type :: <llvm-metadata>);
  llvm-reference-dbg-type(back-end, o.type-estimate-raw)
end method;

define method llvm-reference-dbg-type
    (back-end :: <llvm-back-end>, o :: <type-estimate-limited-instance>)
 => (dbg-type :: <llvm-metadata>);
  llvm-reference-dbg-type(back-end, ^object-class(type-estimate-singleton(o)))
end method;

define method llvm-reference-dbg-type
    (back-end :: <llvm-back-end>, o :: <type-estimate-union>)
 => (dbg-type :: <llvm-metadata>);
  llvm-reference-dbg-type(back-end, first(type-estimate-unionees(o)))
end method;

define method llvm-reference-dbg-type
    (back-end :: <llvm-back-end>, o :: <type-estimate-values>)
 => (dbg-type :: <llvm-metadata>);
  let fixed-values = type-estimate-fixed-values(o);
  if (size(fixed-values) > 0)
    llvm-reference-dbg-type(back-end, fixed-values[0])
  else
    llvm-reference-dbg-type(back-end, dylan-value(#"<object>"))
  end if
end method;


/// Computation variable scope handling

define thread variable *computation-dbg-scope-table* :: false-or(<object-table>)
  = #f;

define method assign-computations-dbg-scope
    (back-end :: <llvm-back-end>, scope :: <llvm-metadata>,
     c :: <computation>, last)
 => ()
  iterate loop (c :: false-or(<computation>) = c,
                scope :: <llvm-metadata> = scope)
    if (c & c ~== last)
      let loc = dfm-source-location(c);
      let temp = c.temporary;
      if (loc & temp & temp.named?)
        let (dbg-file :: <llvm-metadata>,
             dbg-line :: <integer>,
             dbg-column :: <integer>)
          = source-location-dbg-loc(back-end, loc);
        let inner-scope
          = llvm-make-dbg-lexical-block(scope, dbg-file, dbg-line, dbg-column);
        assign-computation-dbg-scope(back-end, inner-scope, c);
        loop(c.next-computation, inner-scope);
      else
        assign-computation-dbg-scope(back-end, scope, c);
        loop(c.next-computation, scope);
      end if;
    end if;
  end iterate;
end method;

define method assign-computation-dbg-scope
    (back-end :: <llvm-back-end>, scope :: <llvm-metadata>,
     c :: <computation>)
 => ()
  *computation-dbg-scope-table*[c] := scope;
end method;

define method assign-computation-dbg-scope
    (back-end :: <llvm-back-end>, scope :: <llvm-metadata>,
     c :: <if>)
 => ()
  next-method();
  let merge :: <if-merge> = next-computation(c);
  assign-computations-dbg-scope(back-end, scope, c.consequent, merge);
  assign-computations-dbg-scope(back-end, scope, c.alternative, merge);
end method;

define method assign-computation-dbg-scope
    (back-end :: <llvm-back-end>, scope :: <llvm-metadata>,
     c :: <loop>)
 => ()
  next-method();
  assign-computations-dbg-scope(back-end, scope,
                                c.loop-body, c.next-computation);
end method;

define method assign-computation-dbg-scope
    (back-end :: <llvm-back-end>, scope :: <llvm-metadata>,
     c :: <block>)
 => ()
  next-method();
  assign-computations-dbg-scope(back-end, scope,
                                c.body, c.next-computation);
end method;

define method assign-computation-dbg-scope
    (back-end :: <llvm-back-end>, scope :: <llvm-metadata>,
     c :: <unwind-protect>)
 => ()
  next-method();
  assign-computations-dbg-scope(back-end, scope,
                                c.cleanups, c.next-computation);
end method;


/// Computation source line tracking

define function op--scl(back-end :: <llvm-back-end>, c :: <computation>) => ()
  iterate loop (origin = c.inlined-origin,
                loc = dfm-source-location(c),
                scope = *computation-dbg-scope-table*[c],
                inlined-at :: false-or(<llvm-metadata>) = #f)
    if (loc)
      let (dbg-file :: <llvm-metadata>, dbg-line :: <integer>,
           dbg-column :: <integer>)
        = source-location-dbg-loc(back-end, loc);
      if (origin)
        let here
          = make(<llvm-DILocation-metadata>,
                 line: dbg-line, column: dbg-column,
                 scope: scope, inlinedAt: inlined-at);
        loop(origin.origin-next,
             origin.origin-location,
             llvm-lambda-dbg-function(back-end, origin.origin-lambda),
             here);
      else
        ins--dbg(back-end, dbg-line, dbg-column, scope, inlined-at: inlined-at);
      end if;
    end if;
  end iterate;
end function;

define function source-location-dbg-loc
    (back-end :: <llvm-back-end>, loc :: <source-location>)
 => (dbg-file :: <llvm-metadata>, dbg-line :: <integer>, dbg-column :: <integer>)
  let sr = source-location-source-record(loc);
  let start-offset = source-location-start-offset(loc);
  let start-line = source-offset-line(start-offset);
  let start-column = source-offset-column(start-offset) + 1;
  values(llvm-source-record-dbg-file(back-end, sr),
         start-line + source-record-start-line(sr),
         start-column)
end function;
