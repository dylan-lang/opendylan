module: idvm-namespace
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define generic query-error(cond-or-string :: <object>, #rest arguments);

define variable *namespace-debug-print* = #f;

/* Overview of name-space scheme.

  Constants are held in a namespace table, indexed by mangled name and stored
  in value slot of idvm-values.
  Variables are closed-over by a getter function (0 args) and a setter
  function (1 arg) stored in the getter and setter slots of idvm-values.
*/

define class <&object>(<object>)
  slot &value, init-keyword: value:;
  slot &getter :: <function>, init-keyword: getter:;
  slot &setter :: <function>, init-keyword: setter:;
end class <&object>;

define constant idvm-namespace = make(<equal-table>);

define variable *idvm-library-namespace* = list();

define variable *module-namespace* = #f;

define variable *current-namespace* = #f;

define constant shes-not-there = list("Let me tell you 'bout the way she walks.");

define method install-library(name :: <string>)
 *module-namespace* := make(<equal-table>);
 idvm-namespace[as(<symbol>, name)] := *module-namespace*;
end method install-library;

define method install-module(name :: <string>)
 *current-namespace* := make(<equal-table>);
 *module-namespace*[as(<symbol>, name)] := *current-namespace*;
end method install-module;

define method install-constant 
    (name :: <string>, value :: <object>) => ();
  *current-namespace*[name] := make(<&object>, value: value);
end method;

define method install-variable
    (name :: <string>, 
     variable-getter :: <function>, variable-setter :: <function>) 
    => ();
  *current-namespace*[name] := make(<&object>, getter: variable-getter, setter: variable-setter);
  variable-setter;
end method;

define method install-variable-reader
    (name :: <string>, variable-getter :: <function>) => ();
  *current-namespace*[name] := make(<&object>, getter: variable-getter);
  variable-getter;
end method;

define method lookup-namespace(name :: <string>, module-name :: <symbol>, library-name :: <symbol>) => <object>;
  let library =
    element(idvm-namespace, library-name, default: #f)
    | (element(idvm-namespace, library-name) := make(<equal-table>));
  let module =
    element(library, module-name, default: #f)
    | (element(library, module-name) := make(<equal-table>));

  *current-namespace* := module;
  element(module, name, default: shes-not-there);
end method;

define method lookup-variable(name :: <string>, module-name :: <symbol>, library-name :: <symbol>) => <object>;
  *namespace-debug-print* & format(*standard-output*, "Looking up variable %=\n", name);
  let object = lookup-namespace(name); 
  if (object == shes-not-there)
    query-error("Doss Names couldn't resolve %=\n", name)
  else
    &getter(object)
  end if
end method;

define method lookup-constant(name :: <string>, module-name :: <symbol>, library-name :: <symbol>) => <object>;
  *namespace-debug-print* & format(*standard-output*, "Looking up constant %=\n", name);
  let object = lookup-namespace(name, module-name, library-name);
  if (object == shes-not-there)
    query-error("Doss Names couldn't resolve %=\n", name)
  else
    &value(object)
  end if
end method;
