Module:       regular-expression
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This code is originally from the Monday project's regular library;
// see http://monday.sourceforge.net/lib/language/regular/regular-project.html
// for the weaved literate source code.
//

define primary abstract class <regular-expression> (<object>)
  slot regular-expression-nullable? :: <boolean> = #f;
  slot regular-expression-first-positions :: <list> = #();
  slot regular-expression-last-positions :: <list> = #();
  slot regular-expression-follow-positions :: <list> = #();
end class;
              
define generic copy-regular-expression
    (regex1 :: <regular-expression>)
 => (regex2 :: <regular-expression>);
              
define class <epsilon-regular-expression> (<regular-expression>)
  // no additional slots
end class;
              
define method copy-regular-expression
    (regex1 :: <epsilon-regular-expression>)
 => (regex2 :: <regular-expression>);
  make(<epsilon-regular-expression>);
end method;
              
define class <symbol-regular-expression>
    (<regular-expression>)
  constant slot regular-expression-symbol :: <object>,
    required-init-keyword: symbol:;
end class;
              
define method copy-regular-expression
    (regex1 :: <symbol-regular-expression>)
 => (regex2 :: <regular-expression>);
  make(<symbol-regular-expression>,
       symbol: regex1.regular-expression-symbol);
end method;
              
define class <symbol-set-regular-expression> (<regular-expression>)
  constant slot regular-expression-symbol-set :: <collection>,
    required-init-keyword: symbol-set:;
end class;
              
define method copy-regular-expression
    (regex1 :: <symbol-set-regular-expression>)
 => (regex2 :: <regular-expression>);
  make(<symbol-set-regular-expression>,
         symbol-set: regex1.regular-expression-symbol-set);
end method;
              
define class <union-regular-expression>
    (<regular-expression>)
  constant slot regular-expression-union1,
    required-init-keyword: union1:;
  constant slot regular-expression-union2,
    required-init-keyword: union2:;
end class;
              
define method copy-regular-expression
    (regex1 :: <union-regular-expression>)
 => (regex2 :: <regular-expression>);
  make(<union-regular-expression>,
         union1: copy-regular-expression(regex1.regular-expression-union1),
         union2: copy-regular-expression(regex1.regular-expression-union2));
end method;
              
define class <concatenation-regular-expression>
    (<regular-expression>)
  constant slot regular-expression-head,
    required-init-keyword: head:;
  constant slot regular-expression-tail,
    required-init-keyword: tail:;
end class;
              
define method copy-regular-expression
    (regex1 :: <concatenation-regular-expression>)
 => (regex2 :: <regular-expression>);
  make(<concatenation-regular-expression>,
       head: copy-regular-expression(regex1.regular-expression-head),
       tail: copy-regular-expression(regex1.regular-expression-tail));
end method;
              
define class <closure-regular-expression>
    (<regular-expression>)
  constant slot regular-expression-enclosed :: <object>,
    required-init-keyword: of:;
end class;
              
define method copy-regular-expression
    (regex1 :: <closure-regular-expression>)
 => (regex2 :: <regular-expression>);
  make(<closure-regular-expression>,
       of: copy-regular-expression(regex1.regular-expression-enclosed));
end method;
              
define /* open */ class <accept-regular-expression> (<regular-expression>)
  // no additional slots
end class;
              
define sealed method initialize
   (instance :: <epsilon-regular-expression>, #next next-method, #key)
  next-method();
  instance.regular-expression-nullable? := #t;
end method initialize;
        
define sealed method initialize
    (instance :: <symbol-regular-expression>, #next next-method, #key)
  next-method();
  instance.regular-expression-first-positions
    := instance.regular-expression-last-positions
    := list(instance);
end method initialize;
        
define sealed method initialize
    (instance :: <symbol-set-regular-expression>, #next next-method, #key)
  next-method();
  instance.regular-expression-first-positions
    := instance.regular-expression-last-positions
    := list(instance);
end method initialize;
        
define method initialize
    (instance :: <accept-regular-expression>, #next next-method, #key)
  next-method();
  instance.regular-expression-first-positions
    := instance.regular-expression-last-positions
    := list(instance);
end method initialize;
        
define sealed method initialize
    (instance :: <union-regular-expression>, #next next-method, #key)
  next-method();
  let union1 = instance.regular-expression-union1;
  let union2 = instance.regular-expression-union2;
  instance.regular-expression-nullable?
    := regular-expression-nullable?(union1)
         | regular-expression-nullable?(union2);
  instance.regular-expression-first-positions
    := concatenate(regular-expression-first-positions(union1),
                   regular-expression-first-positions(union2));
  instance.regular-expression-last-positions
    := concatenate(regular-expression-last-positions(union1),
                   regular-expression-last-positions(union2));
end method initialize;
        
define sealed method initialize
    (instance :: <concatenation-regular-expression>,
     #next next-method, #key)
  next-method();
  let re-head = instance.regular-expression-head;
  let re-tail = instance.regular-expression-tail;
  instance.regular-expression-nullable?
    := regular-expression-nullable?(re-head)
         & regular-expression-nullable?(re-tail);
  instance.regular-expression-first-positions
    := if(regular-expression-nullable?(re-head))
         concatenate(re-head.regular-expression-first-positions,
                     re-tail.regular-expression-first-positions);
       else
         re-head.regular-expression-first-positions;
       end if;
  instance.regular-expression-last-positions
    := if(regular-expression-nullable?(re-tail))
         concatenate(re-head.regular-expression-last-positions,
                     re-tail.regular-expression-last-positions);
       else
         re-tail.regular-expression-last-positions;
       end if;
  
  let followers :: <list> = re-tail.regular-expression-first-positions;
  for (node in re-head.regular-expression-last-positions)
    node.regular-expression-follow-positions
      := concatenate(node.regular-expression-follow-positions, followers);
  end for;
end method initialize;
        
define method initialize
    (instance :: <closure-regular-expression>, #next next-method, #key)
  next-method();
  instance.regular-expression-nullable? := #t;
  instance.regular-expression-first-positions 
    := instance.regular-expression-enclosed.regular-expression-first-positions;
  instance.regular-expression-last-positions 
    := instance.regular-expression-enclosed.regular-expression-last-positions;
  
  let enclosed = instance.regular-expression-enclosed;
  let followers :: <list> = enclosed.regular-expression-first-positions;
  for (node in enclosed.regular-expression-last-positions)
    node.regular-expression-follow-positions
      := union(node.regular-expression-follow-positions, followers, test: \==);
  end for;
end method;
        
define /* open */ class <regular-expression-dfa-state> (<object>)
  constant slot regular-expression-dfa-state-transitions
    :: <mutable-collection>,
    required-init-keyword: transitions:;
end class;
              
define function regular-expression-dfa
    (regular-expression :: <regular-expression>,
     #key deterministic? = #f,
          transition-collection-class = <object-table>,
          transition-collection-size = #f,
          state-class :: subclass(<regular-expression-dfa-state>)
            = <regular-expression-dfa-state>)
 => (start-state :: <regular-expression-dfa-state>,
     num-dfa-states :: <integer>);
  let worklist :: <deque> = make(<deque>);
  
  let states :: <set-table> = make(<set-table>);
  local
    method locate-state
        (label :: <list>)
     => (state :: <regular-expression-dfa-state>);
      let result = element(states, label, default: #f);
      if(result)
        result;
      else
        let transitions
          = if (transition-collection-size)
              make(transition-collection-class,
                   size: transition-collection-size);
            else
              make(transition-collection-class);
            end if;
        let state = make(state-class, transitions: transitions);
        states[label] := state;
        push-last(worklist, state);
        push-last(worklist, label);
        state;
      end if;
    end method;
  
  let start-state
    = locate-state(regular-expression.regular-expression-first-positions);
  while(~empty?(worklist))
    
    let state = pop(worklist);
    let label = pop(worklist);
    
    for(position in label)
      do-regular-expression-dfa-state-position(state, position,
                                               deterministic?: deterministic?);
    end for;
    
    
    let transitions = state.regular-expression-dfa-state-transitions;
    let (initial-state, limit, next-state, finished-state?, current-key,
         current-element, current-element-setter, copy-state)
      = forward-iteration-protocol(transitions);
    for(iteration = initial-state then next-state(transitions, iteration),
        until: finished-state?(transitions, iteration, limit))
      if(current-element(transitions, iteration))
        current-element(transitions, iteration)
          := locate-state(current-element(transitions, iteration));
      end if;
    end for;
  end while;
  values(start-state, states.size);
end function;
              
define class <set-table> (<table>)
  // No additional slots.
end class;
          
define method table-protocol(table :: <set-table>)
  => (test-function :: <function>, hash-function :: <function>);
  values(method(set1 :: <list>, set2 :: <list>) => (equivalent?);
           set1.size = set2.size
             & every?(rcurry(member?, set2), set1);
         end method,
         method(set :: <list>, initial-state :: <object>)
            => (hash-id :: <integer>, hash-state :: <object>);
           if (empty?(set))
             values(0, initial-state)
           else
             let (hash-id :: <integer>, hash-state :: <object>)
               = object-hash(first(set), initial-state);
             
             for(item in set, first? = #t then #f)
               if (~first?)
                 let (item-hash-id :: <integer>,
                      item-hash-state :: <object>)
                   = object-hash(item, hash-state);
                 hash-id := merge-hash-ids(hash-id, item-hash-id, ordered: #f);
                 hash-state := item-hash-state;
               end if;
             end for;
             values(hash-id, hash-state);
           end if;
         end method);
end method;
          
define /* open */ generic do-regular-expression-dfa-state-position
    (state :: <regular-expression-dfa-state>,
     position :: <regular-expression>,
     #key deterministic?)
 => ();
          
define sealed method do-regular-expression-dfa-state-position
    (state :: <regular-expression-dfa-state>,
     position :: <symbol-regular-expression>,
     #key deterministic? = #f)
 => ();
  let follow = position.regular-expression-follow-positions;
  let symbol = position.regular-expression-symbol;
  let present = element(state.regular-expression-dfa-state-transitions,
                        symbol, default: #f);
  if(present)
    if(deterministic?)
      error("regular expression is non-deterministic on symbol %=", symbol);
    else
      element(state.regular-expression-dfa-state-transitions, symbol)
        := union(present, follow);
    end if;
  else
    element(state.regular-expression-dfa-state-transitions, symbol)
      := follow;
  end;
end method;
          
define sealed method do-regular-expression-dfa-state-position
    (state :: <regular-expression-dfa-state>,
     position :: <symbol-set-regular-expression>,
     #key deterministic? = #f)
 => ();
  let follow = position.regular-expression-follow-positions;
  for(symbol in position.regular-expression-symbol-set)
    let present = element(state.regular-expression-dfa-state-transitions,
                          symbol, default: #f);
    if(present)
      if(deterministic?)
        error("regular expression is non-deterministic on symbol %=", symbol);
      else
        element(state.regular-expression-dfa-state-transitions, symbol)
          := union(present, follow);
      end if;
    else
     element(state.regular-expression-dfa-state-transitions, symbol)
        := follow;
    end;
  end for;
end method;
