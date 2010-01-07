module:    idvm-harp
Synopsis:  Mocros to  support the definition of the IDVM instruction set
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// The main instruction-defining macro.

// Notes: The ## name splicing operator is a local extension.

define macro idvm-harp-inst-emitter-definer

  { define idvm-harp-inst-emitter ?:name (?bindings:*) ?:body end }
    => { define internal-idvm-harp-inst-emitter ?name (?bindings) (?bindings)
           ?body
         end }

end macro;


define macro internal-idvm-harp-inst-emitter-definer

  // ?bindings and ?emit-bindings should be the same.

  { define internal-idvm-harp-inst-emitter ?:name (?bindings) (?emit-bindings)
      ?:body
    end }
    => 
    { begin
        define constant "idvm-opcode-" ## ?name = make-idvm-opcode(?#"name");

        define method "emit-" ## ?name 
            (be :: <idvm-back-end>, ?bindings) => ();
          emit-idvm-opcode(be, "idvm-opcode-" ## ?name); 
          ?emit-bindings;
          ?body;
        end method;
      end;
     }

  bindings:
    { ?binding, ... }
      => { ?binding, ... }
    { }
      => { }
  
  binding:
    { ?:name }
      => { ?name }
    { ?dummy:name is ?fn:name(?var1:name, ?var2:name) }
      => { ?var1, ?var2 }

  emit-bindings:
    { ?emit-binding, ... }
      => { ?emit-binding; ... }
    { }
      => { }

  emit-binding:
    { ?:name }
      => { emit-idvm-operand(be, ?name) }
    { ?dummy:name is ?fn:name(?var1:name, ?var2:name) }
      => { "emit-idvm-" ## ?fn(be, ?var1, ?var2) }

end macro;





#| High-level instruction defining macros |#



// IDVM instruction which threads with the value of its body 
//  as the new result. 

define macro idvm-harp-inst-definer
  { define idvm-harp-inst ?:name (?bindings:*) ?:body end }
    => { define idvm-harp-inst-emitter ?name (?bindings)
           ?body
         end }
end macro;



// IDVM instruction which threads without changing the value of result.

define macro idvm-harp-inst-keep-result-definer
  { define idvm-harp-inst-keep-result ?:name (?bindings:*) ?:body end }
    => { define idvm-harp-inst-emitter ?name (?bindings) end }
end macro;



// IDVM instruction which returns the value of its body without threading.

define macro idvm-harp-inst-no-thread-definer
  { define idvm-harp-inst-no-thread ?:name (?bindings:*) ?:body end }
    => { define idvm-harp-inst-emitter ?name (?bindings) end }
end macro;



// Pair of IDVM instructions, one which threads with the value of its body 
// as the new result,   and one which returns the value of its body (named 
// idvm-foo-returning.


define macro idvm-harp-insts-definer
  { define idvm-harp-insts ?:name (?bindings:*) ?:body end }
    => { begin
           define idvm-harp-inst-emitter ?name (?bindings) end;
           define idvm-harp-inst-emitter ?name ## "-returning" (?bindings) end;
         end }
end macro;




#| The following for defining conditional instructions. |#



define macro idvm-harp-res-gets-local-op-local-inst-definer
  { define idvm-harp-res-gets-local-op-local-inst ?:name ?op:token ?val:name }
    => { define idvm-harp-inst-emitter ?name (descriptor is hilo(dst-index, src-index))
         end }
end macro;

define macro idvm-harp-res-gets-local-op-lit-inst-definer
  { define idvm-harp-res-gets-local-op-lit-inst ?:name ?op:token ?val:name }
    => { define idvm-harp-inst-emitter ?name (local-index, literal)
         end }
end macro;

define macro idvm-harp-res-gets-res-op-local-inst-definer
  { define idvm-harp-res-gets-res-op-local-inst ?:name ?op:token ?val:name }
    => { define idvm-harp-inst-emitter ?name (local-index)
         end }
end macro;

define macro idvm-harp-res-gets-res-op-lit-inst-definer
  { define idvm-harp-res-gets-res-op-lit-inst ?:name ?op:token ?val:name }
    => { define idvm-harp-inst-emitter ?name (literal)
         end }
end macro;



#| The following for defining conditional branch instructions. |#



define macro idvm-harp-branch-local-op-local-inst-definer
  { define idvm-harp-branch-local-op-local-inst ?:name ?op:token ?val:name }
    => { define idvm-harp-inst-emitter ?name 
           (descriptor is hilo(dst-index, src-index), branch-offset)
         end }
end macro;

define macro idvm-harp-branch-local-op-lit-inst-definer
  { define idvm-harp-branch-local-op-lit-inst ?:name ?op:token ?val:name }
    => { define idvm-harp-inst-emitter ?name (local-index, literal, branch-offset)
         end }
end macro;

define macro idvm-harp-branch-res-op-local-inst-definer
  { define idvm-harp-branch-res-op-local-inst ?:name ?op:token ?val:name }
    => { define idvm-harp-inst-emitter ?name (local-index, branch-offset)
         end }
end macro;

define macro idvm-harp-branch-res-op-lit-inst-definer
  { define idvm-harp-branch-res-op-lit-inst ?:name ?op:token ?val:name }
    => { define idvm-harp-inst-emitter ?name (literal, branch-offset)
         end }
end macro;
