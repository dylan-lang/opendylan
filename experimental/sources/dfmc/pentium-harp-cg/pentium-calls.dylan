Module: dfmc-pentium-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method stack-arguments-set-up(back-end :: <pentium-back-end>, arguments :: <sequence>) => ()
 for (index from arguments.size - 1 to 1 by -1)
      ins--push(back-end, arguments[index]);
 end for;
end method stack-arguments-set-up;

define sideways method register-arguments-set-up(back-end :: <pentium-back-end>, arguments :: <sequence>) => (arguments :: <sequence>)
 select(arguments.first by instance?)
   <sfreg>, <sf-constant-reference> =>
   ins--fmove(back-end, back-end.registers.reg-float-arg0, arguments.first);
   <dfreg>, <df-constant-reference> =>
   ins--dmove(back-end, back-end.registers.reg-float-arg0, arguments.first);
  otherwise =>
   ins--move(back-end, back-end.registers.reg-arg0, arguments.first);
 end select;
 copy-sequence(arguments, start: 1);
end method register-arguments-set-up;

define sideways method arguments-set-up
    (back-end :: <pentium-back-end>, c :: <simple-call>, args :: <sequence>) => (arguments :: <sequence>)
  if (c.optional-arguments?)
    let no-of-stack-alloc-args = args.size - back-end.registers.arguments-passed-in-registers;
    let stacked-vector-size = stack-vector-size(back-end, c);
    let stack-alloc-count = bytes%(back-end, 1 + no-of-stack-alloc-args + stacked-vector-size);
    concatenate(args, list(stack-alloc-count));
  else
    args;
  end if;
end method arguments-set-up;
