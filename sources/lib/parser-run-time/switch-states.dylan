Module: parser-run-time
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline function switch-state-lookahead 
    (p :: <parser>, ps :: <parser-state>, parsed)
  let state :: <integer> 
    = vector-element(ps.state-stack, ps.state-stack-ptr - 1);
  let reduction = is-all-same-reduction(p, state);
  case
    reduction 
      => let reduction :: <integer> = reduction;
         reduce-state(p, ps, action-of-reduction(reduction));
    is-all-accept(p, state)
      => parsed();
    otherwise
      => #f;
  end
end function;

define inline function switch-state 
    (p :: <parser>, ps :: <parser-state>, symbol, value, parsed, on-error)
  let current-state :: <integer>
    = vector-element(ps.state-stack, ps.state-stack-ptr - 1);
  let action = get-action(p, current-state, symbol);
  case
    ~action
       => on-error(symbol, value, get-history-sequence(ps));
     is-accept(action)
       => parsed();
     otherwise 
       => let action :: <integer> = action;
          if (is-shift(action))
            shift-state(ps, next-state-of-shift(action));
          else
            reduce-state(p, ps, action-of-reduction(action));
          end;
  end;
end function;

/*
define method recover-from-error 
    (p :: <parser>, ps :: <parser-state>, symbol, parsed)
  error("Parse error on %=", symbol);
end method;
*/

// eof
