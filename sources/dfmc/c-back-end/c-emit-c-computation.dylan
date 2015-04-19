Module: dfmc-c-back-end
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method emit-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>,
     c :: <primitive-call>, f :: <&c-function>)
  emit-primitive-call(b, s, d, c, f);
end method;

define method emit-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>,
     c :: <c-variable-pointer-call>, f)
  format-emit(b, s, d, "&~;", c.c-variable.name);
end method;


define method emit-primitive-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>,
     c :: <primitive-call>, f :: <&c-function>)
  format-emit(b, s, d, "~", f.c-function-name);
  emit-c-function-arguments(b, s, d, f, c.arguments);
  write(s, ";");
end method;

define method emit-primitive-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>,
     c :: <primitive-indirect-call>, f :: <&c-function>)
  format-emit(b, s, d, "((*((^*)%))", f, c.arguments.first);
  emit-c-function-arguments(b, s, d, f, copy-sequence(c.arguments, start: 1));
  format-emit(b, s, d, ");");
end method;


define method emit-c-function-arguments
    (back-end :: <c-back-end>, s :: <stream>, d :: <integer>,
     c :: <&c-function>, arguments)
 => ()
  write(s, "(");
  for (arg in arguments,
       parm in c.c-signature.^signature-required,
       first? = #t then #f)
    unless (first?) write(s, ", "); end;
    emit-c-function-argument-out(back-end, s, d, arg, parm);
  end;
  write(s, ")");
end;

define method emit-primitive-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>,
     c :: <primitive-call>, f :: <&objc-msgsend>)
  let sig-values = f.primitive-signature.^signature-values;
  let return-type = first(sig-values, default: dylan-value(#"<object>"));
  format-emit(b, s, d, "((^(*)(", return-type);
  for (type in f.c-signature.^signature-required,
       first? = #t then #f)
    unless (first?)
      format(s, ", ");
    end unless;
    format-emit(b, s, d, "^", type);
  end for;
  format(s, "))%s)", f.c-function-name);
  emit-c-function-arguments(b, s, d, f, c.arguments);
  write(s, ";");
end method;

// general method
define method emit-c-function-argument-out
    (back-end :: <c-back-end>,
     s :: <stream>, d :: <integer>, arg, type)
  format-emit(back-end, s, d, "@", arg);
end;

// method for structs by value.  dereference pointer before passing it on
define method emit-c-function-argument-out
    (back-end :: <c-back-end>,
     s :: <stream>, d :: <integer>, arg, type :: <&raw-aggregate-type>)
  format-emit*(back-end, s, "*(^ *)", type);
  format-emit(back-end, s, d, "@", arg);
end;

// WITH-STACK-STRUCTURE

/* [gts, 2/98, wait until harp backend ready]
define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>,
     c :: <begin-with-stack-structure>)
  let type = type-estimate(c.wss-var);

  gts-debug("wss", "emit-comp, begin-wss, c=%=.\n", c);
  gts-debug("wss", "\ttype of %= = %=(%=); specializer=%=.\n",
            c.wss-var, type, object-class(type), specializer(c.wss-var));

  format-emit(b, s, d, "\t#(~)malloc((size_t) @);\n", $dylan-type-string, wss-var(c), wss-size-temp(c));
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>,
     c :: <end-with-stack-structure>)
  gts-debug("wss", "emit-comp, end-wss, c=%=.\n", c);
  format-emit(b, s, d, "\tfree(@);\n", wss-var(begin-wss(c)));
end method;
*/
