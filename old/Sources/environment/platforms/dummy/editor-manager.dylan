Module:    Editor-Manager
Synopsis:  Environment-Editor Interface pulled together -- DUMMY
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Define a dummy version of the only function most people will want to
// call.

define generic editor-call
    (command :: type-union(<symbol>),
     #key, #all-keys)
 => (#rest objects);

define method editor-call
    (command :: type-union(<symbol>),
     #rest args, #key, #all-keys)
 => (#rest objects)
  error("Dummy editor interface: no editor support available.");
end method;


/// Initialisation.

// [This being a dummy implementation, there's no initialisation.]

define function editor-register-all-factories () => ()
end function;

define function editor-init () => ()
end function;
