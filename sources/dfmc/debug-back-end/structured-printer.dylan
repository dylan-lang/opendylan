Module:   dfmc-debug-back-end
Author:   Hannes Mehnert
Synopsis: Definition structured object printing.
Copyright:    Dylan Hackers 2014
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method structured-output
    (c :: <computation>) => (res :: <string>)
  c.get-structured-output
end method;

define method structured-output
    (c :: type-union(<temporary>, <object-reference>)) => (res :: <string>)
  c.get-structured-output
end method;

define method structured-output (c :: <loop>) => (res :: <string>)
  "LOOP"
end method;

define method structured-output (c :: <loop-call>) => (res :: <string>)
  "LOOP-CALL"
end method;

define method structured-output (c :: <if>) => (res :: <string>)
  concatenate("IF ", format-to-string("%=", c.test))
end method;

define method structured-output (c :: <bind-exit>) => (res :: <string>)
  "bind-exit"
end method;

define method structured-output (c :: <unwind-protect>) => (res :: <string>)
  "UNWIND-PROTECT"
end method;

//helper function which does the actual work!
define method get-structured-output (o :: <object>) => (res :: <string>)
  format-to-string("%=", o)
end;

define method get-structured-output (o :: <variable-defining-form>)
 => (res :: <string>)
  format-to-string("%s %s",
                   o.object-class,
                   map(fragment-identifier, form-variable-names(o)))
end;

define method get-structured-output (o :: <lexical-specialized-variable>)
 => (res :: <string>)
  if (slot-initialized?(o, specializer)
        & o.specializer
        & o.specializer ~= dylan-value(#"<object>"))
    format-to-string("var %s(%=)", o.name, o.specializer)
  else
    format-to-string("var %s", o.name)
  end;
end;

define method get-structured-output (o :: <cell>) => (res :: <string>)
  block ()
    format-to-string("cell %s(%=)", o.name, o.cell-type)
  exception (c :: <condition>)
    format-to-string("cell %s", o.name)
  end;
end;

define method get-structured-output (o :: <temporary>) => (res :: <string>)
  let prop = o.temporary-properties;
  if (named?(o))
    format-to-string("%s%s/%=", prop, o.name, o.frame-offset)
  elseif (instance?(o, <multiple-value-temporary>))
    format-to-string("%sMVT (%d%s)", prop, o.required-values,
                     if (rest-values?(o)) ", #rest" else "" end)
  else
    format-to-string("%sT", prop);
  end if;
end method;

define method get-structured-output (o :: <defined-constant-reference>)
 => (res :: <string>)
  let stream = make(<string-stream>, direction: #"output");
  print-referenced-object(o.referenced-binding, stream);
  concatenate("$", stream.stream-contents)
end method;

// Computations!
define method get-structured-output (c :: <computation>)
 => (res :: <string>)
  let str = format-to-string("%=", c.object-class);
  //this is a bit awful, but need to get rid of "class <" and ">"
  //(foo instead of class <foo>)
  copy-sequence(str, start: 8, end: str.size - 2)
end;


define method get-structured-output
    (c :: <keyword-default>) => (res :: <string>)
  format-to-string("keyword-default %d", c.keyword-default-value-index)
end method;

define method get-structured-output (c :: <make-closure>) => (res :: <string>)
  let lambda = c.computation-closure-method;
  let sigval = c.computation-signature-value;
  let extent = if (c.closure-has-dynamic-extent?) " on stack" else "" end;
  if (c.computation-no-free-references?)
    format-to-string("make-method-with-signature (%s, %=)%s",
                     lambda, sigval, extent)
  else
    if (sigval)
      format-to-string("make-closure-with-signature (%s, %=)%s",
                       lambda, sigval, extent)
    else
      format-to-string("make-closure (%s)%s", lambda, extent)
    end if
  end if;
end method;

define method get-structured-output (c :: <initialize-closure>)
 => (res :: <string>)
  format-to-string("init-closure(%=, %s)",
                   c.computation-closure,
                   c.computation-closure-method)
end method;

define method get-structured-output (c :: <variable-reference>)
 => (res :: <string>)
  format-to-string("^%=", c.referenced-binding);
end method;

define method get-structured-output (c :: <function-call>)
 => (res :: <string>)
  "unknown-call"
end method;

define method get-structured-output (c :: <simple-call>)
 => (res :: <string>)
  "call"
end method;

define method get-structured-output (c :: <method-call>)
 => (res :: <string>)
  "method-call"
end method;

define method get-structured-output (c :: <apply>)
 => (res :: <string>)
  "apply"
end method;

define method get-structured-output (c :: <slot-value>) => (res :: <string>)
  format-to-string("slot-value%s (%s)",
                   if (c.computation-guaranteed-initialized?)
                     "-initd"
                   else
                     ""
                   end,
                   ^debug-name(c.computation-slot-descriptor))
end method;

define method get-structured-output (c :: <slot-value-setter>)
 => (res :: <string>)
  format-to-string("slot-value-setter (%s)",
                   ^debug-name(c.computation-slot-descriptor))
end method;

define method get-structured-output (c :: <repeated-slot-value>)
 => (res :: <string>)
  format-to-string("repeated-slot-value (%s, %=)",
                   ^debug-name(c.computation-slot-descriptor),
                   c.computation-index)
end method;

define method get-structured-output (c :: <repeated-slot-value-setter>)
 => (res :: <string>)
  format-to-string("repeated-slot-value-setter (%s, %=)",
                   ^debug-name(c.computation-slot-descriptor),
                   c.computation-index);
end method;

define method get-structured-output (c :: <primitive-call>) => (res :: <string>)
  concatenate("primitive ", primitive-name(c.primitive))
end method;

define method get-structured-output (c :: <loop-merge>) => (res :: <string>)
  if (loop-merge-initial?(c))
    "loop-mergei"
  else
    "loop-merge"
  end
end method;

define method get-structured-output (c :: <definition>) => (res :: <string>)
  concatenate("define ", c.assigned-binding.get-structured-output)
end method;

define method get-structured-output (c :: <redefinition>) => (res :: <string>)
  concatenate("redefine ", c.assigned-binding.get-structured-output)
end method;

define method get-structured-output (c :: <type-definition>) => (res :: <string>)
  concatenate("define-type ", c.typed-binding.get-structured-output)
end method;

define method get-structured-output (c :: <type-redefinition>)
 => (res :: <string>)
  concatenate("redefine-type ", c.typed-binding.get-structured-output)
end method;

define method get-structured-output (c :: <set!>) => (res :: <string>)
  concatenate("set! ", c.assigned-binding.get-structured-output)
end method;

define method get-structured-output (c :: <exit>) => (res :: <string>)
  format-to-string("exit entry-state: %= value: %=",
                   c.entry-state,
                   c.computation-value)
end method;

define method get-structured-output (c :: <end-loop>) => (res :: <string>)
  "break"
end method;

define method get-structured-output (c :: <end-exit-block>) => (res :: <string>)
  format-to-string("end-exit-block entry-state: %=", c.entry-state)
end method;

define method get-structured-output (c :: <end-protected-block>)
 => (res :: <string>)
  format-to-string("end-protected-block entry-state: %=", c.entry-state)
end method;

define method get-structured-output (c :: <end-cleanup-block>)
 => (res :: <string>)
  format-to-string("end-cleanup-block entry-state: %=", c.entry-state)
end method;

// multiple values

define method get-structured-output (c :: <values>) => (res :: <string>)
  format-to-string("values (#%d%s)",
                   c.fixed-values.size,
                   if (c.rest-value) " #rest" else "" end)
end method;

define method get-structured-output (c :: <extract-single-value>)
 => (res :: <string>)
  format-to-string("extract [%d]", c.index)
end method;

define method get-structured-output (c :: <extract-rest-value>)
 => (res :: <string>)
  format-to-string("extract #rest [%d]", c.index)
end method;

define method get-structured-output (c :: <adjust-multiple-values>)
 => (res :: <string>)
  format-to-string("adjust-mv %d", c.number-of-required-values)
end method;

define method get-structured-output (c :: <adjust-multiple-values-rest>)
 => (res :: <string>)
  format-to-string("adjust-mv-rest %d", c.number-of-required-values)
end method;

// fragments
define method get-structured-output (o :: <variable-name-fragment>)
 => (res :: <string>)
  as(<string>, o.fragment-identifier)
end method;

define method get-structured-output (o :: <named-object>) => (res :: <string>)
  if (o.named?)
    if (*verbose-objects?*)
      format-to-string("%s %s", o.object-class, o.name)
    else
      format-to-string("%s", o.name)
    end if;
  else
    next-method();
  end if;
end method;

define method get-structured-output (s :: <&singleton>) => (res :: <string>)
  format-to-string("{ %= }", s.^singleton-object)
end;

define method get-structured-output (o :: <&object>) => (res :: <string>)
  let ld = current-library-description() | model-original-library(o);
  if (ld)
    with-library-context (ld)
      if (o.name-if-named)
        format-to-string("%s :: %s",
                         o.name-if-named,
                         o.&object-class.debug-string)
      else
        o.&object-class.debug-string
      end;
    end;
  else // not enough info to do &object-class...
    format-to-string("interactive model-object :: %s", o.object-class)
  end;
end method;

define method get-structured-output (o :: <&signature>) => (res :: <string>)
  "<&signature>"
end method;

define method get-structured-output (o :: <&generic-function>)
 => (res :: <string>)
  let head = format-to-string("<&generic-function> %s", o.debug-string);
  let sig = model-signature(o);
  let str = make(<string-stream>, direction: #"output");
  if (sig)
    print-contents(sig, str);
  end;
  concatenate(head, str.stream-contents)
end method;

define method get-structured-output (o :: <&top-type>) => (res :: <string>)
  $top-string
end method;

define method get-structured-output (o :: <&bottom-type>) => (res :: <string>)
  $bottom-string
end method;

define method get-structured-output (o :: <&class>) => (res :: <string>)
  o.debug-string
end method;

define method get-structured-output (o :: <&slot-descriptor>) => (res :: <string>)
  concatenate("slot ", o.^slot-getter.^debug-name)
end method;

define method get-structured-output (o :: <&raw-type>) => (res :: <string>)
  concatenate("raw ", o.debug-string)
end method;

define method get-structured-output (o :: <&primitive>) => (res :: <string>)
  concatenate("primitive ", primitive-name(o))
end method;

define method get-structured-output (o :: <library-description>)
 => (res :: <string>)
  let vers = o.library-description-change-count;
  format-to-string("%s%slibrary-description of %s.%s",
                   if (vers) "" else "CLOSED " end,
                   if (instance?(o, <interactive-library-description>))
                     "interactive "
                   else
                     ""
                   end,
                   library-description-emit-name(o),
                   if (instance?(vers, <integer>)) vers else "???" end)
end method;

define method get-structured-output (o :: <model-heap>) => (res :: <string>)
  concatenate("model-heap of ", heap-compilation-record(o))
end;



