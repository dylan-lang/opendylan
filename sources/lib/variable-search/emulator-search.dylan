Module:   variable-search
Language: infix-dylan
Authors:  Keith Playford, Eliot Miranda
Synopsis: Module variable encoding/decoding interface for the emulator.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

import-cl-functions(
	  dylan(locate-package-variable) (as: locate-package-variable),
          dylan(package-variable-value)  (as: package-variable-value),
          system(function-parent)        (as: function-parent));

define method variable-hint (object) #f end method;

define method variable-hint (class :: <class>)
  translate-cl-class-name(class-debug-name(class))
end method;

define method variable-hint (gf :: <generic-function>)
  hack-function-debug-name(function-debug-name(gf))
end method;

define method variable-hint (gf :: <function>)
  let debug-name = function-debug-name(gf);
  if (instance?(debug-name, <integer>))
    // hack to identify subfunctions
    head(tail(function-parent(gf)))
  else
    // use parent if a subfunction
    hack-function-debug-name(debug-name)
  end if
end method;

define method encode-module-variable
    (object, module :: <translator-module>, library, hint-symbol-or-false)
  values(hint-symbol-or-false, module-name(module), library)
end method encode-module-variable;

define method locate-variable-in-modules (object, modules :: <table>)
  block (return)
    begin
      let hint = variable-hint(object);
      for (module in modules)
        if (variable-value(hint, module, #f, default: #f) == object)
          let (the-encoding, the-module, the-library)
              = encode-module-variable(object, module, #f, hint);
          return(the-encoding, the-module, the-library)
        end if;
      end for
    end;
    for (module in $translator-modules)
      let pkg = module-package(module);
      let name = locate-package-variable(object, pkg);
      if (name)
        let (the-encoding, the-module, the-library)
            = encode-module-variable(name, module-name(module), #f, #f);
        return(the-encoding, the-module, the-library)
      end if;
    end for;
    #f
  end block
end method;

define method locate-variable (object)
  locate-variable-in-modules(object, $translator-modules)
end method;

define constant unsupplied = list("z");

define method variable-value 
    (name, module :: <translator-module>, library, #key default = unsupplied)
  let result = package-variable-value(name,
                                      module-package(module),
	                              default: default);

  if (result ~== unsupplied)
    result
  else
    error("variable-value failed to resolve variable name %= %= %=",
          name, module, library)
  end if
end method;


define method variable-value 
    (name, module-name :: <symbol>, library, #key default = unsupplied)
  variable-value
    (name, find-translator-module(module-name), library, default: default)
end method;

// eof
