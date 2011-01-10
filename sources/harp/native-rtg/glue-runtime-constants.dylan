module:    native-rtg
Synopsis:  Constant definitions for the Dylan Native runtime glue
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



//// Define any constants that don't obviously live somewhere specific



/// Externals


define unmangled runtime-external primitive-wrap-machine-word-ref = 
    "primitive_wrap_machine_word";

define unmangled runtime-external primitive-unwrap-abstract-integer-ref = 
    "primitive_unwrap_abstract_integer";

define unmangled runtime-external primitive-exit-application-ref = 
    "primitive_exit_application";

define unmangled runtime-external primitive-error-ref = 
    "primitive_error";
