Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define inline function compose (function-1 :: <function>, #rest more-functions)
 => (composition :: <function>);
  if (empty?(more-functions))
    function-1
  else
    let composition = apply(compose, more-functions);
    method (#rest args)
      function-1(apply(composition, args))
    end
  end
end function compose;

define inline function complement (function :: <function>) => (result :: <function>)
  method (#rest args)
    ~apply(function, args)
  end method
end function complement;

define inline function disjoin (predicate-1 :: <function>, #rest more-predicates)
 => (disjunction :: <function>)
  if (empty?(more-predicates))
    predicate-1
  else
    let disjunction = apply(disjoin, more-predicates);
    method (#rest args)
      apply(predicate-1, args) | apply(disjunction, args)
    end
  end
end function disjoin;

define inline function conjoin (predicate-1 :: <function>, #rest more-predicates)
 => (conjunction :: <function>)
  if (empty?(more-predicates))
    predicate-1
  else
    let conjunction = apply(conjoin, more-predicates);
    method (#rest args)
      apply(predicate-1, args) & apply(conjunction, args)
    end
  end
end function conjoin;

/*
define inline function curry
    (function :: <function>, #rest curried-args) => (result :: <function>)
  let (required, rest?, key?) = function-arguments(function);
  let number-args = size(required) + size(curried-args);
  if (rest? | key? | number-args > 3)
    method (#rest args)
      apply(function, concatenate(curried-args, args))
    end method
  else
    select ()
      0 => method () function() end;
      1 => method (x) function(x) end;
      2 => method (x, y) function(x, y) end;
      3 => method (x, y, z) function(x, y, z) end;
    end select;
  end if
end function curry;
*/

define inline function curry
    (function :: <function>, #rest curried-args) => (result :: <function>)
  method (#rest args)
    %dynamic-extent(args);
    apply(function, concatenate-2(curried-args, args))
  end method
end function curry;

define inline function rcurry
    (function :: <function>, #rest curried-args) => (result :: <function>)
  method (#rest args)
    %dynamic-extent(args);
    apply(function, concatenate-2(args, curried-args))
  end method
end function rcurry;

define inline function always (object) => (result :: <function>)
  method (#rest args)
    object
  end method
end function always;
