Module: dfmc-native-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method stack-arguments-set-up
    (back-end :: <harp-native-back-end>, arguments :: <sequence>) => ()
  for (index :: <integer>
	 from arguments.size - 1
	 to back-end.registers.arguments-passed-in-registers
	 by -1)
    ins--push(back-end, arguments[index]);
  end for;
end method stack-arguments-set-up;

define sideways method register-arguments-set-up
    (back-end :: <harp-native-back-end>, arguments :: <sequence>)
 => (arguments :: <sequence>)
  let arguments-passed-in-registers :: <integer> =
    arguments-in-registers(back-end, arguments);
  for (index :: <integer> from 0 below arguments-passed-in-registers)
    let argument = arguments[index];

    select(argument by instance?)
      <sfreg>, <sf-constant-reference> =>
	ins--fmove(back-end,
		   back-end.registers.reg-float-machine-arguments[index],
		   argument);
      <dfreg>, <df-constant-reference> =>
	ins--dmove(back-end,
		   back-end.registers.reg-float-machine-arguments[index],
		   argument);
      otherwise =>
	ins--move(back-end,
		  back-end.registers.reg-machine-arguments[index],
		  argument);
    end select;
  end for;
  copy-sequence(arguments, start: arguments-passed-in-registers);
end method register-arguments-set-up;

define sideways method arguments-set-up
    (back-end :: <harp-native-back-end>, c :: <simple-call>, args :: <sequence>)
 => (arguments :: <sequence>)
  if (c.optional-arguments?)
    // args contains at least #rest value
    let no-of-stack-alloc-args = arguments-on-stack(back-end, args);
    let stacked-vector-size = stack-vector-size(back-end, c);
    let stack-alloc-count =
      bytes%(back-end, 1 + no-of-stack-alloc-args + stacked-vector-size);
    let pad-size = argument-register-padding(back-end, args.size);
    let padding =
      if (pad-size == 0) #()
      else
	make(<vector>, size: pad-size, fill: $false);
      end;
    concatenate(args, padding, list(stack-alloc-count));
  else
    args;
  end if;
end method arguments-set-up;

