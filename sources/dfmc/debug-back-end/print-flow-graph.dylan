Module:   dfmc-debug-back-end
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: Printing of flow-graph classes.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// VARIABLES

define compiler-sideways method print-object (o :: <binding>, stream :: <stream>) => ()
  format(stream, "%s", o.name);
  //let te = type-estimate(o);
  //format(stream, "::%=", te);
end method;

define compiler-sideways method print-object (o :: <module-binding>, stream :: <stream>) => ()
  format(stream, "{%s in %s}", o.name, o.binding-home.debug-name);
// defined? is no longer just a slot, it's a database lookup and as such
// is tracked by dependency tracking, so it's not safe to use it here.
//  if (~o.defined?) format(stream, " // undefined") end;
end method;

define compiler-sideways method print-object (o :: <environment>, stream :: <stream>) => ()
  format(stream, "[GLOBAL ENV]");
end method;

define compiler-sideways method print-object (o :: <module>, stream :: <stream>) => ()
  format(stream, "[%sMODULE %s]",
	 if (instance?(o, <interactive-module>)) "Interactive " else "" end,
	 o.debug-name);
end method;

define compiler-sideways method print-object (o :: <library>, stream :: <stream>) => ()
  format(stream, "[%sLIBRARY %s]",
	 if (instance?(o, <interactive-library>)) "Interactive " else "" end,
	 o.debug-name);
end method;

define compiler-sideways method print-object
    (o :: <lexical-environment>, stream :: <stream>) => ()
  format(stream, "[ENV]");
end method;

define compiler-sideways method print-object (o :: <lexical-specialized-variable>, stream :: <stream>) => ()
  let spec = if (slot-initialized?(o, specializer) & o.specializer & o.specializer ~= dylan-value(#"<object>"))
               format-to-string(" (%=)", o.specializer);
             else
               ""
             end;
  format(stream, "var %s%s", o.name, spec);
end;

define compiler-sideways method print-object (o :: <cell>, stream :: <stream>) => ()
  let spec = if (slot-initialized?(o, %cell-type) & o.cell-type & o.cell-type ~= dylan-value(#"<object>"))
                 format-to-string(" (%=)", o.cell-type)
               else
                 ""
               end;
  format(stream, "cell %s%s", o.name, spec);
end;

define compiler-sideways method print-object (o :: <temporary>, stream :: <stream>) => ()
  block ()
    print-temporary-properties(stream, o);
    if (named?(o))
      format(stream, "%s/%=", o.name, o.frame-offset);
    elseif (instance?(o, <multiple-value-temporary>))
      format(stream, "MVT (%d%s)", required-values(o), if (rest-values?(o)) ", #rest" else "" end);
    else
      format(stream, "T");
    end if;
  exception (<error>)
  end block;
end method;

define compiler-sideways method print-referenced-object (o :: <object>, stream :: <stream>) => ()
  print-object(o, stream)
end method;

define compiler-sideways method print-object (o :: <object-reference>, stream :: <stream>) => ()
  format(stream, "^");
  print-referenced-object(o.reference-value, stream);
end method;

define compiler-sideways method print-object (o :: <defined-constant-reference>, stream :: <stream>) => ()
  format(stream, "$");
  print-referenced-object(o.referenced-binding, stream);
end method;

define method print-temporary-properties (stream, o :: <temporary>)
  case o.cell?        => format(stream, "@");
       o.closed-over? => format(stream, "&");
  end case;
  if (instance?(o, <stack-vector-temporary>))
    format(stream, "V");
  end if;
end method;

//// COMPUTATIONS

define method print-computations
    (stream :: <stream>, first :: <computation>, #key before: last) => ();
  let next = #f;
  for-computations (c from first before last)
    indent(stream, *offset*);
    format(stream, "%=\n", c);
    next := c.next-computation;
  end for-computations;
end method print-computations;

// define thread variable *verbose-computations?* = #t;

define compiler-sideways method print-object
    (c :: <computation>, stream :: <stream>) => ()
  block ()
    if (c.temporary & c.temporary.used?)
      format(stream, "%s := ", c.temporary);
    end if;
    print-computation(stream, c);
  /*
  if (current-library-description())
    format(stream, " :: %=", type-estimate(c))
  end if;
    format(stream, " :: %=", type-estimate(c))
  */
  exception (<error>)
  end block;
  values()
end method print-object;

define method print-computation
    (stream :: <stream>, c :: <computation>) => ();
  format(stream, "%=", c.object-class);
end method print-computation;

define method print-computation (stream :: <stream>, c :: <nop>) => ();
  format(stream, "nop");
end method print-computation;

define method print-computation
    (stream :: <stream>, c :: <keyword-default>) => ();
  format(stream, "keyword-default %d",
	 c.keyword-default-value-index);
end method print-computation;

define method print-computation (s :: <stream>, c :: <make-closure>)
  let lambda = computation-closure-method(c);
  let sigval = computation-signature-value(c);
  let extent = if (closure-has-dynamic-extent?(c)) " on stack" else "" end;
  if (computation-no-free-references?(c))
    format(s, "make-method-with-signature (%s, %=)%s", lambda, sigval, extent);
  else
    if (sigval)
      format(s, "make-closure-with-signature (%s, %=)%s", lambda, sigval, extent);
    else
      format(s, "make-closure (%s)%s", lambda, extent)
    end if
  end if;
end method;

define method print-computation (s :: <stream>, c :: <initialize-closure>)
  format(s, "init-closure(%=, %s)", 
         computation-closure(c), computation-closure-method(c));
end method;

define method print-computation (s :: <stream>, c :: <variable-reference>) 
  format(s, "^%=", c.referenced-binding);
end method;

define method print-computation (stream :: <stream>, c :: <temporary-transfer>)
  format(stream, "temporary-transfer");
end method;

define method entry-point-character (c :: <function-call>, o :: <object>)
  'o'
end method;

define method entry-point-character (c :: <function-call>, o :: <&lambda>)
  'x'
end method entry-point-character;

define method entry-point-character (c :: <function-call>, o :: <&iep>)
  'i'
end method entry-point-character;

define method operation-name (c :: <function-call>)
  "unknown-call"
end method operation-name;

define method operation-name (c :: <simple-call>)
  "call"
end method operation-name;

define method operation-name (c :: <method-call>)
  "method-call"
end method operation-name;

define method operation-name (c :: <apply>)
  "apply"
end method operation-name;

define method print-args (stream :: <stream>, arguments)
  for (first? = #t then #f, argument in arguments)
    unless (first?)
      format(stream, ", ");
    end unless;
    format(stream, "%=", argument);
  end;
end method print-args;

define method tail-position-and-computable? 
    (c :: <call>) => (tail-position?, computable?)
  block () 
    values(c.tail-position?, #t)
  exception (<error>)
    values(#f, #f)
  end;
end method;

define method print-tail-call-annotation 
    (stream :: <stream>, c :: <call>) 
  let (tail?, computable?) = tail-position-and-computable?(c);
  if (tail?)
    format(stream, " // tail call");
  elseif (~computable?)
    format(stream, " // tail call status unavailable");
  end if;
end method;

define method print-computation (stream :: <stream>, c :: <function-call>) 
  format(stream, "%s", c.operation-name);
end method;

define method print-computation (stream :: <stream>, c :: <slot-value>) 
  format(stream, "slot-value%s (%s)", 
         if (computation-guaranteed-initialized?(c)) "-initd" else "" end,
	 ^debug-name(computation-slot-descriptor(c)));
end method;

define method print-computation (stream :: <stream>, c :: <slot-value-setter>) 
  format(stream, "slot-value-setter (%s)", 
	 ^debug-name(computation-slot-descriptor(c)));
end method;

define method print-computation (stream :: <stream>, c :: <repeated-slot-value>) 
  format(stream, "repeated-slot-value (%s, %=)", 
	 ^debug-name(computation-slot-descriptor(c)),
	 computation-index(c));
end method;

define method print-computation 
    (stream :: <stream>, c :: <repeated-slot-value-setter>) 
  format(stream, "repeated-slot-value-setter (%s, %=)", 
	 ^debug-name(computation-slot-descriptor(c)), 
	 computation-index(c));
end method;

define method print-computation (stream :: <stream>, c :: <stack-vector>) 
  format(stream, "stack-vector");
end method;

define method print-computation (stream :: <stream>, c :: <loop>) 
  format(stream, "loop");
end method;

define method print-computation (stream :: <stream>, c :: <loop-call>) 
  format(stream, "continue %=", loop-call-arguments(c));
end method;

define method print-computation (stream :: <stream>, c :: <primitive-call>) 
  format(stream, "primitive %s", primitive-name(c.primitive));
end method;

define method print-computation (stream :: <stream>, c :: <if>) 
  format(stream, "if (%s) ... else ... end", c.test);
end method;

define method print-computation (stream :: <stream>, c :: <if-merge>) 
  format(stream, "if-merge");
end method;

define method print-computation (stream :: <stream>, c :: <loop-merge>) 
  format(stream, "loop-merge%s",
	 if (loop-merge-initial?(c)) "i" else "" end);
end method;

define method print-computation (stream :: <stream>, c :: <bind-exit-merge>) 
  format(stream, "bind-exit-merge");
end method;

define method print-computation (stream :: <stream>, c :: <return>) 
  format(stream, "return");
end method;

define method print-computation (stream :: <stream>, c :: <bind>) 
  format(stream, "bind");
end method;

define method print-computation (stream :: <stream>, c :: <definition>) 
  format(stream, "define %s", c.assigned-binding);
end method;

define method print-computation (stream :: <stream>, c :: <redefinition>) 
  format(stream, "redefine %s", c.assigned-binding);
end method;

define method print-computation (stream :: <stream>, c :: <type-definition>) 
  format(stream, "define-type %s", c.typed-binding);
end method;

define method print-computation 
    (stream :: <stream>, c :: <type-redefinition>) 
  format(stream, "redefine-type %s", c.typed-binding);
end method;

define method print-computation (stream :: <stream>, c :: <set!>) 
  format(stream, "set! %s", c.assigned-binding);
end method;

define method print-computation (stream :: <stream>, c :: <bind-exit>)
  format(stream, "bind-exit entry-state: %=", c.entry-state);
end method;

define method print-computation (stream :: <stream>, c :: <unwind-protect>)
  format(stream, "unwind-protect entry-state: %=", c.entry-state);
end method;

define method print-computation (stream :: <stream>, c :: <exit>)
  format(stream, "exit entry-state: %= value: %=", c.entry-state, c.computation-value);
end method;

define method print-computation (stream :: <stream>, c :: <end-loop>)
  format(stream, "break");
end method;

define method print-computation (stream :: <stream>, c :: <end-exit-block>)
  format(stream, "end-exit-block entry-state: %=", c.entry-state);
end method;

define method print-computation
    (stream :: <stream>, c :: <end-protected-block>)
  format(stream, "end-protected-block entry-state: %=", c.entry-state);
end method;

define method print-computation (stream :: <stream>, c :: <end-cleanup-block>)
  format(stream, "end-cleanup-block entry-state: %=", c.entry-state);
end method;

// multiple values

define method print-computation (stream :: <stream>, c :: <values>)
  format(stream, "values (#%d%s)", c.fixed-values.size, if (c.rest-value) " #rest" else "" end);
end method;

define method print-computation
    (stream :: <stream>, c :: <extract-single-value>)
  format(stream, "extract [%d]", c.index);
end method;

define method print-computation
    (stream :: <stream>, c :: <extract-rest-value>)
  format(stream, "extract #rest [%d]", c.index);
end method;

define method print-computation
    (stream :: <stream>, c :: <multiple-value-spill>)
  format(stream, "mv-spill");
end method;

define method print-computation
    (stream :: <stream>, c :: <multiple-value-unspill>)
  format(stream, "mv-unspill");
end method;

define method print-computation
    (stream :: <stream>, c :: <adjust-multiple-values>)
  format(stream, "adjust-mv %d",
	 c.number-of-required-values);
end method;

define method print-computation
    (stream :: <stream>, c :: <adjust-multiple-values-rest>)
  format(stream, "adjust-mv-rest %d",
	 c.number-of-required-values);
end method;

// types

define method print-computation
    (stream :: <stream>, c :: <single-value-check-type-computation>)
  format(stream, "check-type");
end method print-computation;

define method print-computation
    (stream :: <stream>, c :: <assignment-check-type>)
  format(stream, "assignment-check-type");
end method print-computation;

define method print-computation
    (stream :: <stream>, c :: <multiple-value-check-type-computation>)
  format(stream, "multiple-value-check-type");
end method print-computation;

define method print-computation
    (stream :: <stream>, c :: <multiple-value-check-type-rest>)
  format(stream, "multiple-value-check-type-rest");
end method print-computation;

define method print-computation
    (stream :: <stream>, c :: <guarantee-type>)
  format(stream, "guarantee-type");
end method print-computation;

// cells

define method print-computation (stream :: <stream>, c :: <make-cell>)
  format(stream, "make-cell")
end method print-computation;

define method print-computation (stream :: <stream>, c :: <get-cell-value>);
  format(stream, "cell-value")
end method print-computation;

define method print-computation (stream :: <stream>, c :: <set-cell-value!>);
  format(stream, "set-cell-value!");
end method print-computation;

// C-FFI

/* ********************
define method print-computation (s :: <stream>, c :: <begin-with-stack-structure>)
  format(s, "[BEGIN WITH-STACK-STRUCTURE %=, %=]", 
         wss-var(c), wss-size-temp(c));
end method;

define method print-computation (s :: <stream>, c :: <end-with-stack-structure>)
  format(s, "[END WITH-STACK-STRUCTURE]");
end method;

******************** */
