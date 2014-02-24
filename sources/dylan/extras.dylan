Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function as-object (x :: <machine-word>)
  primitive-cast-raw-as-pointer
    (primitive-unwrap-machine-word(x))
end function;

define function remove-keyword-arguments
    (symbols/values :: <sequence>, removals :: <sequence>)
 => (new-symbols/values :: <sequence>)
  let new-symbols/values = make(<stretchy-vector>);
  with-fip-of symbols/values
    iterate check-and-copy (symbol-state = initial-state)
      unless (finished-state?(symbols/values, symbol-state, limit))
        let symbol = current-element(symbols/values, symbol-state);
        let value-state = next-state(symbols/values, symbol-state);
        unless (member?(symbol, removals))
          let value = current-element(symbols/values, value-state);
          add!(new-symbols/values, symbol);
          add!(new-symbols/values, value);
        end unless;
        check-and-copy(next-state(symbols/values, value-state))
      end unless
    end iterate
  end with-fip-of;
  new-symbols/values
end function;
