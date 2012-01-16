Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Calling Convention

define constant $function-parameter-name :: <string> = ".function";

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-function-parameter
    () => (fn :: <function>);
  llvm-builder-local(be, $function-parameter-name)
end;

/*
define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-lambda-parameter
    () => (fn :: <lambda>);
  //---*** Fill this in...
end;
*/

define constant $next-methods-parameter-name :: <string> = ".next";

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-next-methods-parameter
    () => (nm :: <list>);
  llvm-builder-local(be, $next-methods-parameter-name)
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-set-generic-function-entrypoints // runtime
    (gf :: <generic-function>) => ();
  //---*** Fill this in...
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-set-accessor-method-xep
    (accessor-method :: <accessor-method>)
 => (accessor-method :: <accessor-method>);
  //---*** Fill this in...
end;


/// Apply

define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-xep-apply
    (function :: <object>, buffer-size :: <raw-integer>, buffer :: <object>)
 => (#rest values);
  //---*** Fill this in...
end;

define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-mep-apply // runtime
    (function :: <object>, next-methods :: <object>,
     args :: <simple-object-vector>)
 => (#rest values);
  //---*** Fill this in...
end;

define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-mep-apply-with-optionals // runtime
    (function :: <object>, next-methods :: <object>, args :: <object>)
 => (#rest values);
  //---*** Fill this in...
end;

define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-engine-node-apply-with-optionals // runtime
    (function :: <object>, next-methods :: <object>, args :: <object>)
 => (#rest values);
  //---*** Fill this in...
end;

define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-iep-apply
    (function :: <object>, buffer-size :: <raw-integer>, buffer :: <object>)
 => (#rest values);
  //---*** Fill this in...
end;

// !@#$ needs to be built-in
define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-apply // runtime
    (fn :: <function>, size :: <integer>, #rest arguments)
 => (#rest values)
  //---*** Fill this in...
end;


/// Discriminator/engine-node Initialization

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-initialize-engine-node
    (engine-node :: <engine-node>) => (single-value :: <engine-node>);
  //---*** Fill this in...
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-initialize-discriminator
    (discriminator :: <discriminator>) => (single-value :: <discriminator>);
  //---*** Fill this in...
end;
