Module: java-vm-code-generation
Author: Bunty the unruly ferret. (experimental code!)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* this file disabled for now

define class <type-stack-model> (<object>)
  slot depth :: <integer> = 0 ;
  slot stack :: <list>    = #() ;
end;

define sealed generic jvm-type-model-push
    (stack-state :: <type-stack-model>, type :: <java-type>) =>
    (stack-state :: <type-stack-model>) ;
define sealed generic jvm-type-model-pop  
    (stack-state :: <type-stack-model>, type :: <java-type>) =>
    (stack-state :: <type-stack-model>) ;
define sealed generic jvm-type-model-pop-pair 
    (stack-state :: <type-stack-model>) =>
    (stack-state :: <type-stack-model>, pair-code) ;
define sealed generic jvm-type-model-push-pair
    (stack-state :: <type-stack-model>, pair-code) =>
    (stack-state :: <type-stack-model>) ;

define constant <bit-stack-model> :: <type> = type-union (<integer>, <pair>) ;

define sealed generic jvm-bit-model-push 
    (stack-state :: <bit-stack-model>, double? :: <boolean>) => 
    (stack-state :: <bit-stack-model>) ;
define sealed generic jvm-bit-model-pop  
    (stack-state :: <bit-stack-model>, double? :: <boolean>) => 
    (stack-state :: <bit-stack-model>) ;
define sealed generic jvm-bit-model-pop-pair 
    (stack-state :: <bit-stack-model>) => 
    (stack-state :: <bit-stack-model>, pair-code) ;
define sealed generic jvm-bit-model-push-pair 
    (stack-state :: <bit-stack-model>, pair-code) => 
    (stack-state :: <bit-stack-model>) ;


// switching on which level of verification is to be employed
define function jvm-model-push (stack-state, type :: <java-type>) => (stack-state)
  (if (*check-stack-types*) jvm-type-model-push else jvm-bit-model-push end) (stack-state, type)
end;

define function jvm-model-pop (stack-state, type :: <java-type>) => (stack-state)
  (if (*check-stack-types*) jvm-type-model-pop else jvm-bit-model-pop end) (stack-state, type)
end;

define function jvm-model-push-pair (stack-state, pair-code) => (stack-state)
  (if (*check-stack-types*) jvm-type-model-push-pair else jvm-bit-model-push-pair end) (stack-state, pair-code)
end;

define function jvm-model-pop-pair (stack-state) => (stack-state, pair-code)
  (if (*check-stack-types*) jvm-type-model-pop-pair else jvm-bit-model-pop-pair end) (stack-state)
end;



// bit-model

define sealed generic jvm-bit-model-print (mod :: <bit-stack-model>) => () ;

define method jvm-bit-model-print (mod :: <integer>) => ()
  jvm-bit-model-print-aux (mod, #f)
end;
define method jvm-bit-model-print (mod :: <pair>) => ()
  jvm-bit-model-print-aux (mod.head, mod.tail)
end;

define function jvm-bit-model-print-aux (state :: <integer>, rest) => ()
  while (state > 1)
    if (logand (state, 3) = 3)
      format-out ("2") ;
      state := ash (state, -2)
    elseif (logand (state, 1) = 0)
      format-out ("1") ;
      state := ash (state, -1)
    else
      error ("malformed <bit-stack-model>")
    end      
  end;
  if (rest)
    if (instance? (rest, <integer>))
      jvm-bit-model-print-aux (rest, #f)
    else
      jvm-bit-model-print-aux (rest.head, rest.tail)
    end
  end
end;

define constant <count-stack-model> :: <type> = <integer> ;

define function jvm-count-model-depth (mod :: <count-stack-model>) => (depth :: <integer>)
  mod
end;

define function jvm-bit-model-depth (mod :: <bit-stack-model>) => (depth :: <integer>)
  let  depth :: <integer> = 0 ;
  local 
    method count-depth (state :: <integer>) => ()
      while (state > 1)
        if (logand (state, 3) = 3)
          depth := depth + 2 ;
          state := ash (state, -2)
        elseif (logand (state, 1) = 0)
          depth := depth + 1 ;
          state := ash (state, -1)
        else
          error ("malformed <bit-stack-model>")
        end
      end
    end;
      
  while (instance? (mod, <pair>))
    count-depth (mod.head) ;
    mod := mod.tail
  end;
  count-depth (mod);
  depth
end;


define function bit-timer-fn (model :: <bit-stack-model>) => (res :: <bit-stack-model>)
  for (i :: <integer> from 0 below 100000)
    model := jvm-bit-model-push (model, #t) ;
    model := jvm-bit-model-push (model, #f) ;
    model := jvm-bit-model-push (model, #t) ;
    model := jvm-bit-model-push (model, #f) ;
    model := jvm-bit-model-push (model, #t) ;
    model := jvm-bit-model-pop (model, #t) ;
    model := jvm-bit-model-pop (model, #f) ;
    model := jvm-bit-model-pop (model, #t) ;
    model := jvm-bit-model-pop (model, #f) ;
    model := jvm-bit-model-pop (model, #t) ;
  end;
  model
end;

define function count-timer-fn (model :: <count-stack-model>) => (res :: <count-stack-model>)
  for (i :: <integer> from 0 below 100000)
    model := jvm-count-model-push (model, #t) ;
    model := jvm-count-model-push (model, #f) ;
    model := jvm-count-model-push (model, #t) ;
    model := jvm-count-model-push (model, #f) ;
    model := jvm-count-model-push (model, #t) ;
    model := jvm-count-model-pop (model, #t) ;
    model := jvm-count-model-pop (model, #f) ;
    model := jvm-count-model-pop (model, #t) ;
    model := jvm-count-model-pop (model, #f) ;
    model := jvm-count-model-pop (model, #t) ;
  end;
  model
end;

define function type-timer-fn (model :: <type-stack-model>) => (res :: <type-stack-model>)
  for (i :: <integer> from 0 below 100000)
    model := jvm-type-model-push (model, $java-long-type$) ;
    model := jvm-type-model-push (model, $java-int-type$) ;
    model := jvm-type-model-push (model, $java-double-type$) ;
    model := jvm-type-model-push (model, $dylan-class-<integer>$) ;
    model := jvm-type-model-push (model, $java-double-type$) ;
    model := jvm-type-model-pop (model, $java-double-type$) ;
    model := jvm-type-model-pop (model, $dylan-class-<integer>$) ;
    model := jvm-type-model-pop (model, $java-double-type$) ;
    model := jvm-type-model-pop (model, $java-int-type$) ;
    model := jvm-type-model-pop (model, $java-long-type$) ;
  end;
  model
end;

// jvm-count-model-push
define method jvm-count-model-push (state :: <count-stack-model>, double? :: <boolean>) => (state :: <count-stack-model>)
  state + (if (double?) 2 else 1 end)
end;

// jvm-count-model-pop
define method jvm-count-model-pop (state :: <count-stack-model>, double? :: <boolean>) => (state :: <count-stack-model>)
  let  new-state = state - (if (double?) 2 else 1 end) ;
  if (new-state < 0)
    error ("JVM stack model underflow")
  end;
  new-state
end;

// jvm-bit-model-push

define method jvm-bit-model-push (state :: <integer>, double? :: <boolean>) => (state :: <bit-stack-model>)
  jvm-bit-model-push-aux (state, #f, double?)
end;

define method jvm-bit-model-push (pair-state :: <pair>, double? :: <boolean>) => (state :: <bit-stack-model>)
  jvm-bit-model-push-aux (pair-state.head, pair-state, double?)
end;

define function jvm-bit-model-push-aux (state :: <integer>, list, double? :: <boolean>) => (state :: <bit-stack-model>)
  let  new-state = if (double?)
		     ash (state, 2) + 3
                   else
                     ash (state, 1)
                   end;
  if (new-state > #x01000000)
    pair (if (double?) 7 else 2 end, list | state)
  elseif (list)    
    // non-destruct
//    pair (new-state, list.tail)
    // destruct
    list.head := new-state ;
    list
  else
    new-state
  end
end;


// jvm-type-model-push
define method jvm-bit-model-push (model :: <type-stack-model>, type :: <java-type>) => (model :: <type-stack-model>)
  let size = type.words-size ;
  if (size = 0)
    model
  else
    // non-destruct
//    make (<type-stack-model>, depth: model.depth + size, list: pair (type, model.list))
    // destruct
    model.depth := model.depth + size ;
    model.list  := pair (type, model.list) ;
    model
  end
end;
  

// jvm-bit-model-pop

define method jvm-bit-model-pop (state :: <integer>, double? :: <boolean>) => (state :: <integer>)
  if (state <= 1)
    error ("stack model underflow")
  end;
  if (double?)
    if (state <= 3 | logand (state, 3) ~= 3)
      error ("popping a 2-word entry when only single word present")
    end;
    ash (state, -2)
  else
    if (logand (state, 1) = 1)
      error ("popping only part of a two-word entry")
    end;
    ash (state, -1)
  end
end;

define method jvm-bit-model-pop (pair-state :: <pair>, double? :: <boolean>) => (state)
  let  state :: <integer> = pair-state.head ;
  let  rest  = pair-state.tail ;
  if (state <= 1)
    // punt to previous part of list
    jvm-bit-model-pop (rest, double?)
  else
    if (double?)
      if (state <= 3 | logand (state, 3) ~= 3)
        error ("popping a 2-word entry when only single word present")
      end; 
      // non-destruct:
//      pair (ash (state, -2), rest)
      // destruct:
      pair-state.head := ash (state, -2) ;
      pair-state
    else
      if (logand (state, 1) = 1)
        error ("popping only part of a two-word entry")
      end;
      // non-destruct:
//      pair (ash (state, -1), rest)
      // destruct:
      pair-state.head := ash (state, -1) ;
      pair-state
    end
  end
end;

// for dup2, pop2, dup2_x1 etc:
// jvm-count-model-pop-pair
define method jvm-count-model-pop-pair (state :: <count-stack-model>) => (state :: <count-stack-model>, pair-code)
  let  new-state = state - 2 ;
  if (new-state < 0)
    error ("JVM model stack underflow")
  end;
  values (new-state, #f)
end;

// jvm-bit-model-pop-pair

define method jvm-bit-model-pop-pair (state :: <integer>) => (state :: <integer>, pair-code :: <integer>)
  if (state <= 3)
    error ("stack model underflow")
  end;
  let  code = logand (state, 3) ;
  if (code ~= 3 | code ~= 0)
    error ("popping a 2-word entry when only single word present")
  end;
  values (ash (state, -2), code)
end;
  
 
define method jvm-bit-model-pop-pair (pair-state :: <pair>) => (state :: <bit-stack-model>, pair-code :: <integer>)
  let  state :: <integer> = pair-state.head ;
  let  rest  = pair-state.tail ;
  if (state <= 1)
    jvm-bit-model-pop-pair (rest)
  elseif (state = 2)
    // need to get one zero bit from rest!!!
  elseif (state = 3)
    error ("bad state code")
  else
    let  code = logand (state, 3) ;
    if (code ~= 3 | code ~= 0)
      error ("popping a 2-word entry when only single word present")
    end;
    // non-destruct:
//    values (pair (ash (state, -2), rest), code)
    // destruct:
    pair-state.head := ash (state, -2) ;
    values (pair-state, code)
  end
end;

// jvm-count-model-push-pair 
define method jvm-count-model-push-pair (state :: <count-stack-model>, pair-code) => (state :: <count-stack-model>)
  state + 2
end;

// jvm-bit-model-push-pair 

define method jvm-bit-model-push-pair (state :: <integer>, pair-code :: <integer>) => (state :: <bit-stack-model>)
  jvm-bit-model-push-pair-aux (state, #f, pair-code)
end;

define method jvm-bit-model-push-pair (pair-state :: <pair>, pair-code :: <integer>) => (state :: <bit-stack-model>)
  jvm-bit-model-push-pair-aux (pair-state.head, pair-state, pair-code)
end;

define function jvm-bit-model-push-pair-aux (state :: <integer>, list, pair-code :: <integer>) => (state :: <bit-stack-model>)
  if (pair-code ~= 0 | pair-code ~= 3)
    error ("pushing a bad 2-word pair-code")
  end;
  let  new-state = logior (ash (state, 2), pair-code) ;
  if (new-state > #x01000000)
    pair (logior (4, pair-code), list | state)
  elseif (list)    
    // non-destruct
//    pair (new-state, list.tail)
    // destruct
    list.head := new-state ;
    list
  else
    new-state
  end
end;


// abstract over the pair-modelling for dup2 & friends.

define abstract class <jvm-stack-pair-model> (<object>)
  slot double? :: <boolean>, required-init-keyword: double?: ;
end;

define class <jvm-stack-pair-bit-model> (<jvm-stack-pair-model>)
end;

define constant $jvm-bit-singles-pair :: <jvm-stack-pair-bit-model> =
  make (<jvm-stack-pair-bit-model>, double?: #f) ;

define constant $jvm-bit-double-pair :: <jvm-stack-pair-bit-model> =
  make (<jvm-stack-pair-bit-model>, double?: #t) ;



define abstract class <jvm-stack-pair-type-model> (<jvm-stack-pair-model>)
end;

define class <jvm-stack-pair-type-model-singles> (<jvm-stack-pair-type-model>)
  slot top-single :: <java-type>, required-init-keyword: top-single: ;
  slot next-single :: <java-type>, required-init-keyword: next-single: ;
end;

define class <jvm-stack-pair-type-model-double> (<jvm-stack-pair-type-model>)
  slot the-double :: <java-type>, required-init-keyword: the-double: ;
end;

*/
