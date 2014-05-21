Module:   dfmc-debug-back-end
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: Printing of flow-graph classes.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// VARIABLES

define compiler-sideways method print-object
    (o :: <binding>, stream :: <stream>) => ()
  format(stream, "%s", o.name);
end method;

define compiler-sideways method print-object
    (o :: <module-binding>, stream :: <stream>) => ()
  format(stream, "{%s in %s}", o.name, o.binding-home.debug-name);
end method;

define compiler-sideways method print-object
    (o :: <environment>, stream :: <stream>) => ()
  format(stream, "[GLOBAL ENV]");
end method;

define compiler-sideways method print-object
    (o :: <module>, stream :: <stream>) => ()
  format(stream, "[%sMODULE %s]",
	 if (instance?(o, <interactive-module>)) "Interactive " else "" end,
	 o.debug-name);
end method;

define compiler-sideways method print-object
    (o :: <library>, stream :: <stream>) => ()
  format(stream, "[%sLIBRARY %s]",
	 if (instance?(o, <interactive-library>)) "Interactive " else "" end,
	 o.debug-name);
end method;

define compiler-sideways method print-object
    (o :: <lexical-environment>, stream :: <stream>) => ()
  format(stream, "[ENV]");
end method;

define compiler-sideways method print-object
    (o :: <temporary>, stream :: <stream>) => ()
  block ()
    print-temporary-properties(stream, o);
    if (named?(o))
      format(stream, "%s/%=", o.name, o.frame-offset);
    else
      format(stream, "t");
      if (o.frame-offset)
        format(stream, "%d",
	       o.frame-offset - o.environment.lambda.parameters.size);
      else
        format(stream, "?")
      end if;
    end if;
    if (instance?(o, <multiple-value-temporary>))
      format(stream, "(%d%s)", o.required-values,
        if (o.rest-values?) ",#rest" else "" end)
    end if;
  exception (<error>)
  end block;
end method;

define compiler-sideways method print-referenced-object
    (o :: <object>, stream :: <stream>) => ()
  print-object(o, stream)
end method;

define compiler-sideways method print-object
    (o :: <object-reference>, stream :: <stream>) => ()
  format(stream, "^");
  print-referenced-object(o.reference-value, stream);
end method;

define compiler-sideways method print-object
    (o :: <defined-constant-reference>, stream :: <stream>) => ()
  format(stream, "^");
  print-referenced-object(o.referenced-binding, stream);
end method;

define method print-temporary-properties
    (stream :: <stream>, o :: <temporary>)
  format(stream, o.temporary-properties);
end method;

define method temporary-properties (o :: <temporary>)
 => (res :: <string>)
  let fst
    = case o.cell?        => "@";
           o.closed-over? => "&";
           otherwise      => "";
      end case;
  if (instance?(o, <stack-vector-temporary>))
    concatenate(fst, "V");
  else
    fst
  end if;
end method;

define method temporary-properties (o :: <multiple-value-temporary>)
 => (res :: <string>)
  concatenate(next-method(), "*")
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
  exception (<error>)
  end block;
  values()
end method print-object;

define method print-computation
    (stream :: <stream>, c :: <computation>) => ()
  format(stream, "[%= computation]", c.object-class);
end method print-computation;

define method print-computation (stream :: <stream>, c :: <nop>) => ()
  format(stream, "[NOP]");
end method print-computation;

define method print-computation
    (stream :: <stream>, c :: <keyword-default>) => ()
  format(stream, "[KEYWORD-DEFAULT %=, %d]",
	 c.computation-value, c.keyword-default-value-index);
end method print-computation;

define method print-computation (s :: <stream>, c :: <make-closure>)
  let lambda = computation-closure-method(c);
  let sigval = computation-signature-value(c);
  let extent = if (closure-has-dynamic-extent?(c)) " on stack" else "" end;
  if (computation-no-free-references?(c))
    format(s, "MAKE-METHOD-WITH-SIGNATURE(%s, %=)%s", lambda, sigval, extent);
  else
    if (sigval)
      format(s, "MAKE-CLOSURE-WITH-SIGNATURE(%s, %=)%s", lambda, sigval, extent);
    else
      format(s, "MAKE-CLOSURE(%s)%s", lambda, extent)
    end if
  end if;
end method;

define method print-computation (s :: <stream>, c :: <initialize-closure>)
  format(s, "INIT-CLOSURE(%=, %s)",
         computation-closure(c), computation-closure-method(c));
end method;

define method print-computation (s :: <stream>, c :: <variable-reference>)
  format(s, "^%=", c.referenced-binding);
end method;

define method print-computation (s :: <stream>, c :: <temporary-transfer>)
  format(s, "%=", c.computation-value);
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

define method entry-point-character (c :: <function-call>, o :: <&generic-function>)
  'g'
end method entry-point-character;

define method operation-name (c :: <function-call>)
  "UNKNOWN CALL"
end method operation-name;

define method operation-name (c :: <simple-call>)
  if (call-congruent?(c))
    "CONGRUENT-CALL"
  else
    "CALL"
  end if
end method operation-name;

define method operation-name (c :: <engine-node-call>)
  "ENGINE-NODE-CALL"
end method operation-name;

define method operation-name (c :: <method-call>)
  "METHOD-CALL"
end method operation-name;

define method operation-name (c :: <apply>)
  "APPLY"
end method operation-name;

define method operation-name (c :: <engine-node-apply>)
  "ENGINE-NODE-APPLY"
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
  format(stream, "[%s%s %=(",
	 c.operation-name,
         entry-point-character(c, call-effective-function(c)),
         c.function);
  print-args(stream, c.arguments);
  format(stream, ")]");
  print-tail-call-annotation(stream, c);
end method;

define method print-computation (stream :: <stream>, c :: <slot-value>)
  format(stream, "SLOT-VALUE%s(%=, %s)",
         if (computation-guaranteed-initialized?(c)) "-INITD" else "" end,
	 computation-instance(c), ^debug-name(computation-slot-descriptor(c)));
end method;

define method print-computation (stream :: <stream>, c :: <slot-value-setter>)
  format(stream, "SLOT-VALUE(%=, %s) := %=",
	 computation-instance(c), ^debug-name(computation-slot-descriptor(c)),
	 computation-new-value(c));
end method;

define method print-computation (stream :: <stream>, c :: <repeated-slot-value>)
  format(stream, "REPEATED-SLOT-VALUE(%=, %s, %=)",
	 computation-instance(c), ^debug-name(computation-slot-descriptor(c)),
	 computation-index(c));
end method;

define method print-computation
    (stream :: <stream>, c :: <repeated-slot-value-setter>)
  format(stream, "REPEATED-SLOT-VALUE(%=, %s, %=) := %=",
	 computation-instance(c), ^debug-name(computation-slot-descriptor(c)),
	 computation-index(c),
	 computation-new-value(c));
end method;

define method print-computation (stream :: <stream>, c :: <stack-vector>)
  format(stream, "[STACK-VECTOR (");
  print-args(stream, c.arguments);
  format(stream, ")]");
end method;

define method print-computation (stream :: <stream>, c :: <loop>)
  format(stream, "[LOOP %=]", loop-parameters(c));
end method;

define method print-computation (stream :: <stream>, c :: <loop-call>)
  format(stream, "[CONTINUE %=]", loop-call-arguments(c));
end method;

define method print-computation (stream :: <stream>, c :: <primitive-call>)
  format(stream, "[PRIMOP %s(", primitive-name(c.primitive));
  print-args(stream, c.arguments);
  format(stream, ")]");
  print-tail-call-annotation(stream, c);
end method;

define method print-computation
    (stream :: <stream>, c :: <primitive-indirect-call>)
  format(stream, "[INDIRECT-PRIMOP (");
  print-args(stream, c.arguments);
  format(stream, ")]");
  print-tail-call-annotation(stream, c);
end method;

define method print-computation
    (stream :: <stream>, c :: <c-variable-pointer-call>)
  format(stream, "[C-VARIABLE-PRIMOP %=", c.c-variable);
end method;

define method print-computation (stream :: <stream>, c :: <if>)
  format(stream, "if (%s) ... else ... end", c.test);
end method;

define method print-computation (stream :: <stream>, c :: <if-merge>)
  format(stream, "[IF-MERGE %= %=]",
	 merge-left-value(c), merge-right-value(c));
end method;

define method print-computation (stream :: <stream>, c :: <loop-merge>)
  format(stream, "[LOOP-MERGE%s %= %=]",
	 if (loop-merge-initial?(c)) "i" else "" end,
         merge-left-value(c), merge-right-value(c));
end method;

define method print-computation (stream :: <stream>, c :: <bind-exit-merge>)
  format(stream, "[BIND-EXIT-MERGE %= %=]",
	 merge-left-value(c), merge-right-value(c));
end method;

define method print-computation (stream :: <stream>, c :: <return>)
  format(stream, "return %s", c.computation-value);
end method;

define method print-computation (stream :: <stream>, c :: <bind>)
  format(stream, "[BIND]");
end method;

define method print-computation (stream :: <stream>, c :: <definition>)
  format(stream, "define %s = %=", c.assigned-binding, c.computation-value);
end method;

define method print-computation (stream :: <stream>, c :: <redefinition>)
  format(stream, "redefine %s = %=", c.assigned-binding, c.computation-value);
end method;

define method print-computation (stream :: <stream>, c :: <type-definition>)
  format(stream, "define-type %s = %=",
         c.typed-binding, c.computation-value);
end method;

define method print-computation
    (stream :: <stream>, c :: <type-redefinition>)
  format(stream, "redefine-type %s = %=",
         c.typed-binding, c.computation-value);
end method;

define method print-computation (stream :: <stream>, c :: <set!>)
  format(stream, "%s := %=", c.assigned-binding, c.computation-value);
end method;

define method print-computation (stream :: <stream>, c :: <bind-exit>)
  format(stream, "[BIND-EXIT entry-state: %= ...]", c.entry-state);
end method;

define method print-computation (stream :: <stream>, c :: <unwind-protect>)
  format(stream, "[UNWIND-PROTECT entry-state: %= ...]", c.entry-state);
end method;

define method print-computation (stream :: <stream>, c :: <exit>)
  format(stream, "exit entry-state: %= value: %=", c.entry-state, c.computation-value);
end method;

define method print-computation (stream :: <stream>, c :: <end-loop>)
  format(stream, "BREAK");
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
  format(stream, "[VALUES", c.fixed-values);
  for (v in c.fixed-values)
    format(stream, " %=", v);
  end for;
  if (c.rest-value)
    format(stream, " #rest %=", c.rest-value)
  end if;
  format(stream, "]")
end method;

define method print-computation
    (stream :: <stream>, c :: <extract-single-value>)
  format(stream, "%= [%d]", c.computation-value, c.index);
end method;

define method print-computation
    (stream :: <stream>, c :: <extract-rest-value>)
  format(stream, "#rest %= [%d]", c.computation-value, c.index);
end method;

define method print-computation
    (stream :: <stream>, c :: <multiple-value-spill>)
  format(stream, "[MV-SPILL %=]", c.computation-value);
end method;

define method print-computation
    (stream :: <stream>, c :: <multiple-value-unspill>)
  format(stream, "[MV-UNSPILL %=]", c.computation-value);
end method;

define method print-computation
    (stream :: <stream>, c :: <adjust-multiple-values>)
  format(stream, "[ADJUST-MV %= %d]",
	 c.computation-value, c.number-of-required-values);
end method;

define method print-computation
    (stream :: <stream>, c :: <adjust-multiple-values-rest>)
  format(stream, "[ADJUST-MV-REST %= %d]",
	 c.computation-value, c.number-of-required-values);
end method;

// types

define method print-computation
    (stream :: <stream>, c :: <single-value-check-type-computation>)
  format(stream, "check-type %s :: %s", c.computation-value, c.type)
end method print-computation;

define method print-computation
    (stream :: <stream>, c :: <keyword-check-type>)
  format(stream, "keyword-check-type %s :: %s", c.computation-value, c.type)
end method print-computation;

define method print-computation
    (stream :: <stream>, c :: <constrain-type>)
  format(stream, "constrain-type %s :: %s", c.computation-value, c.type)
end method print-computation;

define method print-computation
    (stream :: <stream>, c :: <assignment-check-type>)
  format(stream, "assignment-check-type %s = %s :: %s", c.lhs-variable-name, c.computation-value, c.type)
end method print-computation;

define method print-computation
    (stream :: <stream>, c :: <multiple-value-check-type-computation>)
  format(stream, "multiple-value-check-type %s :: ", c.computation-value);
  for (first? = #t then #f, type in c.types)
    unless (first?)
      format(stream, ", ");
    end unless;
    format(stream, "%=", type);
  end;
end method print-computation;

define method print-computation
    (stream :: <stream>, c :: <multiple-value-check-type-rest>)
  next-method();		// print fixed part
  unless (empty?(c.types))
    format(stream, ", ");
  end;
  format(stream, "#rest %=", c.rest-type);
end method print-computation;

define method print-computation
    (stream :: <stream>, c :: <guarantee-type>)
  format(stream, "guarantee-type %s :: %s",
	 c.computation-value, c.static-guaranteed-type | c.guaranteed-type)
end method print-computation;

// cells

define method print-computation (stream :: <stream>, c :: <make-cell>)
  format(stream, "make-cell(%s)", c.computation-value)
end method print-computation;

define method print-computation (stream :: <stream>, c :: <get-cell-value>);
  format(stream, "cell-value(%s)", c.computation-cell)
end method print-computation;

define method print-computation (stream :: <stream>, c :: <set-cell-value!>);
  format(stream, "cell-value(%s) := %s", c.computation-cell, c.computation-value)
end method print-computation;
