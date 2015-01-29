Module:   dfmc-definitions
Synopsis: Primitive processing.
Author:   Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define dood-class <primitive-definition> (<function-defining-form>,
                                     <compiler-booted-form>)
  lazy constant slot form-primitive-value = #f,
    init-keyword: value:;
end dood-class;

define leaf packed-slots form-properties
    (<primitive-definition>, <function-defining-form>)
  boolean slot form-primitive-side-effecting?,
    required-init-keyword: side-effecting?:;
  boolean slot form-primitive-dynamic-extent?,
    required-init-keyword: dynamic-extent?:;
  boolean slot form-primitive-stateless?,
    required-init-keyword: stateless?:;
end packed-slots;

define method form-compile-stage-only?
    (form :: <primitive-definition>) => (compile-stage-only?)
  #t
end method;

// Adjective parsing.

define function parse-primitive-adjectives (name, adjectives) => initargs;
  local method verify-not-both (adj1, adj2)
          if (member?(adj1, adjectives) & member?(adj2, adjectives))
            error("Conflicting adjectives %s and %s for %s", adj1, adj2, name)
          end;
        end;
  verify-not-both(#"side-effecting", #"side-effect-free");
  verify-not-both(#"stateless", #"stateful");
  verify-not-both(#"dynamic-extent", #"indefinite-extent");
  let unk = choose(method (adj)
                     ~member?(adj, #(#"side-effecting", #"side-effect-free",
                                     #"stateless", #"stateful",
                                     #"dynamic-extent", #"indefinite-extent"))
                   end, adjectives);
  unless (empty?(unk)) error("Unknown adjectives %s for %s", unk, name) end;
  vector(side-effecting?: member?(#"side-effecting", adjectives),
         stateless?: ~member?(#"stateful", adjectives),
         dynamic-extent?: ~member?(#"indefinite-extent", adjectives))
end function;

// Signature parsing.

define constant <primitive-signature-spec> = <signature-spec>;
