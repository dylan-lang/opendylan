Module:    emulator-environment-backend
Synopsis:  Emulator Environment Backend
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Function handling

define method environment-object-home-name
    (database :: <emulator-database>, object :: <function-object>)
 => (name :: false-or(<name-object>))
  let name = dylan-function-name(application-object-proxy(object));
  name & ensure-server-object-of-class(database, name, <binding-name-object>);
end method environment-object-home-name;

define method coerce-function-parameter
    (project :: <emulator-project>, parameter :: <pair>)
 => (results :: <sequence>)
  make(<parameter>, 
       name: as-lowercase(as(<string>, head(parameter))),
       type: ensure-server-object(project, tail(parameter)))
end method coerce-function-parameter;

define method coerce-function-parameters
    (project :: <emulator-project>, parameters :: <sequence>)
 => (results :: <sequence>)
  let no-of-parameters = size(parameters);
  let results = make(<vector>, size: no-of-parameters);
  for (i from 0 below no-of-parameters,
       parameter in parameters)
    results[i] := coerce-function-parameter(project, parameter)
  end;
  results
end method coerce-function-parameters;

define method coerce-function-keywords
    (project :: <emulator-project>, keywords :: <sequence>)
 => (results :: <sequence>)
  let no-of-keywords = size(keywords);
  let results = make(<vector>, size: no-of-keywords);
  for (i from 0 below no-of-keywords,
       keyword in keywords)
    let keyword-name-info
      = if (instance?(keyword, <sequence>)) first(keyword) else keyword end;
    let keyword-name
      = if (instance?(keyword-name-info, <sequence>))
          first(keyword-name-info)
        else
          keyword-name-info
        end;
    let keyword-value
      = if (instance?(keyword, <sequence>)) second(keyword) else #f end;
    results[i]
      := make(<optional-parameter>,
              name: as-lowercase(as(<string>, keyword-name)),
              type: ensure-server-object(project, <object>),
              default-value: keyword-value & ensure-server-object(project, keyword-value))
  end;
  results
end method coerce-function-keywords;

define method function-parameters
    (project :: <emulator-project>, function :: <function-object>)
 => (required :: <parameters>,
     rest :: false-or(<parameter>),
     keys :: <optional-parameters>,
     next :: false-or(<parameter>),
     return-values :: <parameters>,
     rest-value :: false-or(<parameter>))
  let (required, rest, keys, next, return-values, rest-value)
    = dylan-function-parameters(application-object-proxy(function));
  values(coerce-function-parameters(project, required),
         rest & coerce-function-parameter(project, rest),
         coerce-function-keywords(project, keys),
         next & coerce-function-parameter(project, next),
         coerce-function-parameters(project, return-values),
         rest-value & coerce-function-parameter(project, rest-value))
end method function-parameters;


/// Method handling

define method method-specializers
    (database :: <emulator-database>, object :: <method-object>)
 => (specializers :: <sequence>)
  let lisp-specializers
    = dylan-method-specializers(application-object-proxy(object));
  let specializers = make(<vector>, size: size(lisp-specializers));
  for (lisp-specializer in lisp-specializers,
       index from 0)
    specializers[index]
      := if (dylan-class?(lisp-specializer))
           ensure-server-object-of-class(database, lisp-specializer, <class-object>)
         else
           ensure-server-object-of-class(database, lisp-specializer, <singleton-object>)
         end;
  end;
  specializers
end method method-specializers;

define method method-generic-function
    (database :: <emulator-database>, object :: <method-object>)
 => (function :: <generic-function>)
  let lisp-method = application-object-proxy(object);
  ensure-server-object(database, lisp-method-generic-function(lisp-method))
end method method-generic-function;


/// Singleton handling

define method singleton-value
    (database :: <emulator-database>, object :: <singleton-object>)
  let value = dylan-singleton-value(application-object-proxy(object));
  ensure-server-object(database, value)
end method singleton-value;


/// Generic function handling

define method do-generic-function-methods
    (function :: <function>,
     database :: <emulator-database>,
     gf :: <generic-function-object>,
     #key client)
 => ()
  do-server-environment-objects
    (function, database,
     dylan-generic-function-methods(application-object-proxy(gf)),
     <method-object>)
end method do-generic-function-methods;


/// Macro handling

//---*** We need to do something to model macros properly
