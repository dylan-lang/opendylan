Module: orb-core
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $orb-arg-processors :: <table> = make(<table>);
define variable *sorted-orb-arg-processors* :: <list> = #();

define method process-orb-arg-list (orb :: <orb>);
  process-orb-arg-initializers(orb);
  process-orb-arg-callbacks(orb);
end method;

define method process-orb-arg-initializers (orb :: <orb>)
 => ()
  for (processor in *sorted-orb-arg-processors*)
    let initializer = orb-arg-processor-initializer(processor);
    initializer & initializer(orb);
  end for;
end method;

define method process-orb-arg-callbacks (orb :: <orb>)
 => ()
  let args = as(<deque>, orb-arg-list(orb));
  while (~empty?(args))
    let arg = pop(args);
    if (prefix?(arg, "-ORB"))
      let (processor, rest-of-arg) = get-orb-arg-processor(orb, arg);
      if (rest-of-arg)
	push(args, rest-of-arg);
      end if;
      if (processor)
	process-orb-arg(orb, processor, args);
      else
	signal("unknown ORB option %s", arg)
      end if;
    end if;
  end while;
end method;

define method get-orb-arg-processor (orb :: <orb>, arg :: <string>)
  block (return)
    for (processor in *sorted-orb-arg-processors*)
      let syntax = orb-arg-processor-syntax(processor);
      if (prefix?(arg, syntax))
	let after-prefix = skip-whitespace(arg, start: size(syntax));
	let rest-of-arg = if (after-prefix < (size(arg) - 1))
			    copy-sequence(arg, start: after-prefix)
			  end if;
        return(processor, rest-of-arg);
      end if;
    end for;
  end block;
end method;

define method prefix? (big :: <sequence>, prefix :: <sequence>)
 => (prefix? :: <boolean>)
  (size(prefix) <= size(big)) & every?(\=, big, prefix)
end method;

define method skip-whitespace (string :: <string>, #key start = 0)
 => (index :: <integer>)
  let length = size(string);
  block (return)
    for (i from start below length)
      if (~member?(string[i], #(' ', '\t', '\n')))
	return(i)
      end if;
    end for;
    length
  end block;
end method;

define method process-orb-arg (orb :: <orb>, processor :: <orb-arg-processor>, args :: <deque>)
  let callback = orb-arg-processor-callback(processor);
  if (callback)
    if (orb-arg-processor-value?(processor))
      if (empty?(args))
	error("missing value for %s", orb-arg-processor-syntax(processor))
      else
	let next = pop(args);
	callback(orb, next);
      end if;
    else
      callback(orb);
    end if;
  end if;
end method;

define class <orb-arg-processor> (<object>)
  constant slot orb-arg-processor-syntax :: <string>, required-init-keyword: syntax:;
  constant slot orb-arg-processor-callback :: false-or(<function>) = #f, init-keyword: callback:;
  constant slot orb-arg-processor-initializer :: false-or(<function>) = #f, init-keyword: initializer:;
  constant slot orb-arg-processor-value? :: <boolean> = #f, init-keyword: value?:;
end class;

define sealed domain make (subclass(<orb-arg-processor>));
define sealed domain initialize (<orb-arg-processor>);

define method initialize (processor :: <orb-arg-processor>, #key)
  next-method();
  element($orb-arg-processors, as(<symbol>, orb-arg-processor-syntax(processor))) := processor;
  *sorted-orb-arg-processors* := insert-arg-processor!(processor, *sorted-orb-arg-processors*);
end method;

define macro orb-arg-processor-definer
  { define orb-arg-processor ?keys:* end }
    =>
    { make(<orb-arg-processor>, ?keys) }
end macro;

define method insert-arg-processor! (processor :: <orb-arg-processor>, sorted-processors :: <list>)
  if (empty?(sorted-processors))
    list(processor)
  else
    let next = head(sorted-processors);
    if (insert-arg-processor-here?(processor, next))
      pair(processor, sorted-processors)
    else
      tail(sorted-processors) := insert-arg-processor!(processor, tail(sorted-processors));
      sorted-processors
    end if;
  end if;
end method;

define method insert-arg-processor-here?
    (p1 :: <orb-arg-processor>, p2 :: <orb-arg-processor>)
 => (here? :: <boolean>)
  size(orb-arg-processor-syntax(p1)) > size(orb-arg-processor-syntax(p2))
end method;
