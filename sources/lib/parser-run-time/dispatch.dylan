Module: parser-run-time
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $all-same-reduction-tag = 65535;

//// Utilities:

define /* inline */ function vector-property-value 
    (v :: <simple-object-vector>, key) => (value)
  /*
  block (return)
    for (i :: <integer> from 0 below v.size by 2)
      if (v[i] == key) return(v[i + 1]) end;
    end;
    #f
  end
  */
  let stop = v.size;
  iterate walk (i :: <integer> = 0)
    case
      pointer-id?(i, stop)
        => #f;
      pointer-id?(vector-element(v, i), key)
        => vector-element(v, i + 1);
      otherwise
        => walk(i + 2);
    end;
  end;
end function;

//// Parser: (this should be called grammar I guess)

define class <parser> (<object>)

  constant slot action-table :: <simple-object-vector>,
    required-init-keyword: action-table:;
  constant slot action-function-table :: <simple-object-vector>,
    required-init-keyword: action-function-table:;
  constant slot action-nargs-table :: <simple-object-vector>,
    required-init-keyword: action-nargs-table:;
  constant slot action-nt-table :: <simple-object-vector>,
    required-init-keyword: action-nt-table:;
  constant slot goto-table :: <simple-object-vector>, // <object-table>
    required-init-keyword: goto-table:;

  // TODO: these are not used anywhere, but need them for the keywords...
  constant slot error-productions,
    init-keyword: error-productions:;
  constant slot error-action-function-table,
    init-keyword: error-action-function-table:;
  constant slot error-action-nt-table,
    init-keyword: error-action-nt-table:;

end class;

ignore(error-action-nt-table);
ignore(error-productions);
ignore(error-action-function-table);

define sealed domain make (singleton(<parser>));
define sealed domain initialize (<parser>);

// Backward-compatibility..
define function maybe-tablify-properties (properties :: <simple-object-vector>)
  /*
  let tab :: <object-table> = make(<object-table>);
  for (i :: <integer> from 0 below size(properties) by 2)
    tab[properties[i]] := properties[i + 1];
  end;
  tab
  */
  let n = size(properties);
  if (n > 0 & ~instance?(properties[0], <integer>))
    // already tablified
    properties
  else
    let max-so-far = 0;
    for (i :: <integer> from 0 below n by 2)
      max-so-far := max(properties[i], max-so-far);
    end;
    let tab :: <simple-object-vector> 
      = make(<simple-object-vector>, size: max-so-far + 1);
    for (i :: <integer> from 0 below n by 2)
      tab[properties[i]] := properties[i + 1];
    end;
    tab
  end;
end function;

define inline function get-next-state
   (p :: <parser>, state :: <integer>, symbol)
  let next :: <simple-object-vector> = vector-element(p.goto-table, state);
  vector-property-value(next, symbol)
end function;

define method make 
    (class == <parser>, #rest initargs, 
       #key goto-table :: <simple-object-vector>) 
 => (object :: <parser>)
  apply(next-method, class, 
        goto-table: maybe-tablify-properties(goto-table), initargs);
end method;

define inline function get-action-function
   (p :: <parser>, action-number :: <integer>)
  vector-element(p.action-function-table, action-number)
end function;

define inline function get-action-nt
    (p :: <parser>, action-number :: <integer>)
  vector-element(p.action-nt-table, action-number)
end function;

define inline function get-action-nargs
    (p :: <parser>, action-number :: <integer>)
  vector-element(p.action-nargs-table, action-number)
end function;

define inline function get-actions
    (p :: <parser>, state :: <integer>)
  vector-element(p.action-table, state)
end function;

define inline function get-symbol-action
   (p :: <parser>, symbol, state :: <integer>)
  let actions :: <simple-object-vector> = get-actions(p, state);
  vector-property-value(actions, symbol)
end function;

/*
define inline function get-default-action
   (p :: <parser>, state :: <integer>)
  let actions :: <simple-object-vector> = get-actions(p, state);
  vector-property-value(actions, any:)
end function;
*/

define inline function get-action (p :: <parser>, state :: <integer>, symbol)
  get-symbol-action(p, symbol, state) // | get-default-action(p, state)
end function;

define inline function is-all-same-reduction
    (p :: <parser>, state :: <integer>)
  let actions :: <simple-object-vector> = get-actions(p, state);
  let flag = vector-element(actions, 0);
  if (pointer-id?(flag, $all-same-reduction-tag))
    vector-element(actions, 1);
  end;
  /*
  let actions :: <simple-object-vector> = get-actions(p, state);
  let first = actions.second;
  let action = if (is-reduction(first)) action-of-reduction(first) end;
  action
    & /*
      block (return)
        for (i :: <integer> from 2 below size(actions) by 2)
          let code = actions[i + 1];
          if (~(is-reduction(code) & action-of-reduction(code) == action))
            return(#f)
          end
        finally
          first
        end
      end
      */
      begin
        let stop = size(actions);
        iterate walk (i :: <integer> = 2)
          if (i == stop)
            first
          else
            let code = vector-element(actions, i + 1);
            if (~(is-reduction(code) & action-of-reduction(code) == action))
              #f
            else
              walk(i + 2);
            end;
          end;
        end;
      end;
  */
end function;

// Hack!!! Variable to get around bugs in the compiler's dispatching

// define constant *accept-actions* = #[eoi:, accept:];

define inline function is-all-accept (p :: <parser>, state :: <integer>)
  let actions :: <simple-object-vector> = get-actions(p, state);
  pointer-id?(size(actions), 2)
    & pointer-id?(vector-element(actions, 0), eoi:)
    & pointer-id?(vector-element(actions, 1), accept:)
end function;

//// Action types:

// Reductions:

/*
define inline method is-reduction (action :: <object>) #f end;
define inline method is-reduction (action :: <integer>) action >= 0 end;
*/

/*
define inline function is-reduction (action)
  if (instance?(action, <integer>))
    action >= 0
  end
end function;
*/

define inline function action-of-reduction (action :: <integer>) action end;

// Shifts:

/*
define inline method is-shift (action :: <object>) #f end;
define inline method is-shift (action :: <integer>) action < 0 end;
*/

define inline function is-shift (action)
  if (instance?(action, <integer>))
    action < 0
  end
end function;

define inline function next-state-of-shift (action :: <integer>)
  -1 - action 
end function;

// Accept:

define inline function is-accept (action) pointer-id?(action, accept:) end;

//// Parser state:

define constant $shared-value-stack :: <simple-object-vector>
  = make(<simple-object-vector>, size: 64 * 1024);

define constant $shared-state-stack :: <simple-object-vector>
  = make(<simple-object-vector>, size: 64 * 1024);

define class <parser-state> (<object>)
  constant slot state-stack :: <simple-object-vector> = $shared-state-stack;
  slot state-stack-ptr :: <integer> = 0;
  constant slot value-stack :: <simple-object-vector> = $shared-value-stack;
  slot value-stack-ptr :: <integer> = 0;
//  slot error-found = #f;
  slot first-token = #f;
end class;

define sealed domain make (singleton(<parser-state>));
define sealed domain initialize (<parser-state>);

define constant $shared-parser-state :: <parser-state>
  = make(<parser-state>);

define function get-history-sequence (ps :: <parser-state>)
  // Be careful to tidy up the values stack.
  let vals = ps.value-stack;
  let nvals = ps.value-stack-ptr;
  for (i from 0 below nvals)
    vector-element(vals, i) := #f;
  end;
  list(ps.first-token)
end function;

define inline function get-stack-values
    (ps :: <parser-state>, nargs :: <integer>)
  let args :: <simple-object-vector>
    = make(<simple-object-vector>, size: nargs);
  let values = ps.value-stack;
  // Top of stack is the last argument.
  for (i :: <integer> from nargs - 1 to 0 by -1, 
       ptr :: <integer> from ps.value-stack-ptr - 1 by -1)
    vector-element(args, i) := vector-element(values, ptr);
    // So we don't leave references to unused values on the values stack.
    vector-element(values, ptr) := #f; 
  finally
    ps.value-stack-ptr := ptr + 1;
    args
  end;
end function;

define inline function put-stack-value (ps :: <parser-state>, value)
  let ptr = ps.value-stack-ptr;
  vector-element(ps.value-stack, ptr) := value;
  ps.value-stack-ptr := ptr + 1;
  value
end function;

define inline function call-parser-action 
    (p :: <parser>, ps :: <parser-state>, action-number :: <integer>)
  let function = get-action-function(p, action-number);
  let nargs :: <integer> = get-action-nargs(p, action-number);
  let values :: <simple-object-vector> = ps.value-stack;
  let ptr :: <integer> = ps.value-stack-ptr - 1;
  let result
    = select (nargs)
	0 => function();
	1 => let a1 = vector-element(values, ptr); vector-element(values, ptr) := #f;  
             function(a1);
        2 => let a2 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a1 = vector-element(values, ptr); vector-element(values, ptr) := #f;  
             function(a1, a2);
        3 => let a3 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a2 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a1 = vector-element(values, ptr); vector-element(values, ptr) := #f;  
             function(a1, a2, a3);
        4 => let a4 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a3 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a2 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a1 = vector-element(values, ptr); vector-element(values, ptr) := #f;  
             function(a1, a2, a3, a4);
        5 => let a5 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a4 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a3 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a2 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a1 = vector-element(values, ptr); vector-element(values, ptr) := #f;  
             function(a1, a2, a3, a4, a5);
        6 => let a6 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a5 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a4 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a3 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a2 = vector-element(values, ptr); vector-element(values, ptr) := #f;  let ptr = ptr - 1;
             let a1 = vector-element(values, ptr); vector-element(values, ptr) := #f;  
             function(a1, a2, a3, a4, a5, a6);
        otherwise
          => let result = apply(function, get-stack-values(ps, nargs));
             result;
      end;
  ps.value-stack-ptr := ptr - nargs + 1;
  put-stack-value(ps, result);
end function;

define inline function shift-state (ps :: <parser-state>, state :: <integer>)
  let ptr = ps.state-stack-ptr;
  vector-element(ps.state-stack, ptr) := state;
  ps.state-stack-ptr := ptr + 1;
  #f
end function;

define inline function reduce-state 
    (p :: <parser>, ps :: <parser-state>, action :: <integer>)
  let reduce-string-length :: <integer> = get-action-nargs(p, action);
  let reduce-to-symbol = get-action-nt(p, action);
  let ptr = ps.state-stack-ptr - reduce-string-length;
  let state :: <integer> = vector-element(ps.state-stack, ptr - 1);
  vector-element(ps.state-stack, ptr) 
    := get-next-state(p, state, reduce-to-symbol);
  ps.state-stack-ptr := ptr + 1;
  action
end function;

// Main execution loop:

/// HACK: KEITH JB ADDED INFO

define function run-parser
    (info, p :: <parser>, lexer :: <function>, #key on-error = recover)
  block (return)
    // let ps :: <parser-state> = make(<parser-state>);
    let ps :: <parser-state> = $shared-parser-state;
    ps.state-stack-ptr := 0;
    ps.value-stack-ptr := 0;
    vector-element(ps.state-stack, 0) := 0; 
    ps.state-stack-ptr := ps.state-stack-ptr + 1;
    block (parsed)
      let state-result = #f;
      let token-class = #f;
      let token-value = #f;
      let first? = #t;
      while (#t)
	// Perform all reductions that are independent of input
        while (state-result := switch-state-lookahead(p, ps, parsed))
          let state-result :: <integer> = state-result;
	  call-parser-action(p, ps, state-result);
	end;
        let (next-token-class, next-token-value) = lexer();
	// Hack!!!
	if (pointer-id?(next-token-value, eoi:)) return (eoi:) end;
	token-class := next-token-class;
	token-value := next-token-value;
        if (first?)
          ps.first-token := next-token-value;
          first? := #f;
        end;
	while (state-result 
                 := switch-state
                      (p, ps, token-class, token-value, parsed, on-error))
          let state-result :: <integer> = state-result;
	  call-parser-action(p, ps, state-result);
	end;
	put-stack-value(ps, token-value);
      end while;
    end block;
    let vals = ps.value-stack;
    let result = vector-element(vals, 0); vector-element(vals, 0) := #f;
    result
  end;
end function;

/// HACK: KEITH LOOK THIS OVER

define method parser-error (#rest args) end; // HACK: DEFINE THIS FOR REAL

define method recover (symbol, value, history)
  let dodgy-string = value;
  parser-error
    (#f,
     "unexpected %= %= after \"%s\"" ,
     symbol,
     dodgy-string,
     reduce(method (acc, string)
	      concatenate(acc, format-to-string("%=", string), " ")
	    end,
	    make(<string>),
	    reverse!(tail(history))));
end method;
