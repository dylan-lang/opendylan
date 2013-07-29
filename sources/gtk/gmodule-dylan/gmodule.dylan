module: gmodule
synopsis: generated bindings for the GModule library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;

define C-struct <_GModule>
  pointer-type-name: <GModule>;
end C-struct;

define C-function g-module-close
  input parameter self :: <GModule>;
  result res :: <C-boolean>;
  c-name: "g_module_close";
end;

define C-function g-module-make-resident
  input parameter self :: <GModule>;
  c-name: "g_module_make_resident";
end;

define C-function g-module-name
  input parameter self :: <GModule>;
  result res :: <C-string>;
  c-name: "g_module_name";
end;

define C-function g-module-symbol
  input parameter self :: <GModule>;
  input parameter symbol_name_ :: <C-string>;
  input parameter symbol_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_module_symbol";
end;

define C-function g-module-build-path
  input parameter directory_ :: <C-string>;
  input parameter module_name_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_module_build_path";
end;

define C-function g-module-error
  result res :: <C-string>;
  c-name: "g_module_error";
end;

define C-function g-module-supported
  result res :: <C-boolean>;
  c-name: "g_module_supported";
end;

define constant $G-MODULE-BIND-LAZY = 1;
define constant $G-MODULE-BIND-LOCAL = 2;
define constant $G-MODULE-BIND-MASK = 3;
define constant <GModuleFlags> = <C-int>;
define C-pointer-type <GModuleFlags*> => <GModuleFlags>;

