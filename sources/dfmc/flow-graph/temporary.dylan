Module: dfmc-flow-graph
Author: Jonathan Bachrach and Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <value-reference> (<referenced-object>)
end class;

define method generator (ref :: <value-reference>) => (object)
  #f
end method;

// Like a value continuation

define dood-class <temporary> (<value-reference>)
  weak slot %generator :: false-or(<computation>) = #f, 
    reinit-expression: #f,
    init-keyword: generator:;
  weak slot environment :: false-or(<lambda-lexical-environment>), 
    reinit-expression: #f,
    required-init-keyword: environment:;
  slot temporary-properties :: <integer> = 0;
  weak slot %temporary-id :: false-or(<integer>),
    init-function: next-computation-id,
    reinit-expression: #f;
end dood-class;

define method generator (t :: <temporary>) => (res :: false-or(<computation>))
  t.%generator;
end;

define method generator-setter (g :: false-or(<computation>), t :: <temporary>) => (res :: false-or(<computation>))
  if (g & *computation-tracer*)
    let old-id = if (t.%generator) t.%generator else 0 end;
    *computation-tracer*(#"temporary-generator", t, g, old-id);
  end;
  t.%generator := g;
end;
define method remove-user! (t :: <temporary>, c :: <computation>)
  next-method();
  if (*computation-tracer*)
    *computation-tracer*(#"remove-temporary-user", t, c, 0);
  end;
end;

define method add-user! (t :: <temporary>, c :: <computation>)
  if (*computation-tracer* & t.environment ~== c.environment)
    *computation-tracer*(#"add-temporary", t, c, 0);
  end;
  next-method();
  if (*computation-tracer*)
    *computation-tracer*(#"add-temporary-user", t, c, 0);
  end;
end;

define method temporary-id (t :: <temporary>) => (id :: <integer>)
  if (instance?(t.%temporary-id, <integer>))
    t.%temporary-id;
  else
    t.%temporary-id := next-computation-id();
    if (*computation-tracer*)
      if (t.users.size > 0)
        let gen = if (t.generator) t.generator else 0 end;
        *computation-tracer*(#"add-temporary", t, gen, 0);
        if (t.generator)
          *computation-tracer*(#"temporary-generator", t, gen, 0);
          let new = t.generator.computation-type;
          *computation-tracer*(#"change-type", t, new, 0);
        end;
        do(rcurry(curry(*computation-tracer*, #"add-temporary-user", t), 0),
           t.users);
      end;
    end;
    t.%temporary-id;
  end;
end;

// Seal construction over the <temporary> world.

define sealed domain make (subclass(<value-reference>));
define sealed domain initialize (<value-reference>);
define sealed domain initialize-packed-slots (<value-reference>);

define method assignments (t :: <temporary>) => (res)
  #()
end method;

define method name (o :: <temporary>) => (res :: <boolean>)
  #f
end method;

define inline function pack-false-or-field
    (x :: false-or(<integer>)) => (z :: <integer>)
  if (x) x else 0 end
end function;

define inline function unpack-false-or-field
    (x :: <integer>) => (z :: false-or(<integer>))
  if (x = 0) #f else x end
end function;

define constant $max-frame-offset-field-size = 12;
define constant $max-frame-offset = ash(1, $max-frame-offset-field-size) - 1;

define packed-slots temporary-properties (<temporary>, <object>)
  eval slot closed-over? = #f, 
    field-size:      2, 
    pack-function:   pack-false-or-field,
    unpack-function: unpack-false-or-field;
  field slot frame-offset = 0, field-size: $max-frame-offset-field-size;
  quadstate slot dynamic-extent? = #"unknown";
end packed-slots;

define method initialize
    (temporary :: <temporary>, #rest all-keys, #key environment, generator)
  next-method();
  apply(initialize-packed-slots, temporary, all-keys);
  add-temporary!(environment, temporary);
  if (*computation-tracer*)
    let gen = if (generator) generator else 0 end;
    *computation-tracer*(#"add-temporary", temporary, gen, 0);
    unless (gen == 0)
      *computation-tracer*(#"temporary-generator", temporary, gen, 0);
    end;
  end;
  temporary.frame-offset
    := min(next-frame-offset(environment), $max-frame-offset);
  temporary
end method;

define class <named-temporary-mixin> 
    (<named-object>, <emitted-object>)
end class;

define class <named-temporary> 
    (<named-temporary-mixin>, <temporary>)
end class;

define class <cell> (<named-temporary>)
  slot assignments :: <list> = #();
  slot %cell-type :: <&type>,
    init-keyword: cell-type:;
end class;

define method cell-type (c :: <cell>) => (t :: <&type>)
  c.%cell-type;
end;

define method cell-type-setter (new :: <&type>, c :: <cell>) => (t :: <&type>)
  c.%cell-type := new;
  if (c.users.size > 0 & *computation-tracer*)
    *computation-tracer*(#"change-type", c.temporary-id, new, 0);
  end;
  new;
end;

define method remove-user! (t :: <cell>, c :: <computation>)
  dynamic-bind(*computation-tracer* = #f)
    next-method();
  end;
  if (*computation-tracer*)
    *computation-tracer*(#"remove-temporary-user", t, c, 0);
  end;
end;

define method add-user! (t :: <cell>, c :: <computation>)
  dynamic-bind(*computation-tracer* = #f)
    next-method();
  end;
  if (*computation-tracer*)
    *computation-tracer*(#"add-temporary-user", t, c, 0);
  end;
end;
define method cell? (t :: <temporary>) #f end;
define method cell? (t :: <cell>) #t end;

define class <entry-state> (<named-temporary>)
  slot me-block :: false-or(<block>), init-keyword: block:;
  slot exits :: <list> = #();
end class;

define leaf packed-slots temporary-properties (<entry-state>, <temporary>)
  boolean slot local-entry-state? = #f;
end packed-slots;

// a temporary for %stack-vector computation

define class <stack-vector-temporary> (<temporary>)
  slot number-values :: <integer> = 0, init-keyword: number-values:;
end class;

// define constant <lazy-boolean-value> = type-union(<boolean>, singleton(#"not-computed"));

// a temporary for computations that produce multiple values.

define constant $max-number-values-field-size 
  = 8;
define constant $max-number-values            
  = ash(1, $max-number-values-field-size);

define class <multiple-value-temporary> (<temporary>)
end class <multiple-value-temporary>;

// The required-values and rest-values? slots of mv-temps are intended to be 
// set during conversion.  They record the <value-context> in which the 
// mv-temp was created.  If set, we can assume that the dfm is doing its best 
// to satisfy the signature of the mv-temp.  For ex., if the mv-temp has a 
// required-values = 2 and is generated by a function call with a #rest values
// signature, there will be an ADJ-MV done if necessary to guarantee that at
// least two values are available from the MV area.  Of course, the back-end may 
// ignore ADJ-MV computations, if it has other ways of insuring that accesses 
// to the MV area will yield correct results.

define leaf packed-slots temporary-properties 
    (<multiple-value-temporary>, <temporary>)
  boolean  slot mvt-required-initialized? = #f;
  field    slot %required-values = 0, field-size: $max-number-values-field-size;
  tristate slot %rest-values?    = #"unknown";
end packed-slots;

// handle initialization, where required-vals and rest-vals? are being set.

define method initialize
    (tmp :: <multiple-value-temporary>, #rest all-keys, #key environment, 
     required-values, rest-values? = #"unknown")
  next-method();
  if (required-values)
    tmp.required-values := required-values;
  end if;
  if (rest-values? ~== #"unknown")
    tmp.rest-values? := rest-values?;
  end if;
end method;

/*
define function mvt-debug-name (o :: <multiple-value-temporary>) 
    if (named?(o))
      format-to-string("%s/%=", o.name, o.frame-offset);
    else
      if (o.frame-offset)
        format-to-string("*t%d",
          o.frame-offset - o.environment.lambda.parameters.size);
      else
        "*t?"
      end if;
    end if;
end function;
*/

define method required-values (tmp :: <multiple-value-temporary>) 
    => (n :: <integer>)
  if (mvt-required-initialized?(tmp))
    %required-values(tmp);
  else
    0;
  end if;
end method;

define method required-values-setter (new-val :: <integer>, tmp :: <multiple-value-temporary>) 
    => (n :: <integer>)
  mvt-required-initialized?(tmp) := #t;
  tmp.%required-values := new-val
end method;

define method rest-values? (tmp :: <multiple-value-temporary>)
    => (b :: <boolean>)
  if (%rest-values?(tmp) ~== #"unknown")
    %rest-values?(tmp);
  else
    #t;
  end if;
end method;

define method rest-values?-setter (new-val :: <boolean>, tmp :: <multiple-value-temporary>) 
    => (n :: <boolean>)
  tmp.%rest-values? := new-val
end method;

define method mvt-transfer-values! (from, to) => ()
  if (instance?(from, <multiple-value-temporary>)
      & instance?(to, <multiple-value-temporary>)
      & mvt-required-initialized?(from))
    required-values(to) := required-values(from);
    rest-values?(to) := rest-values?(from);
  end if;
end method;

define generic multiple-values? 
    (t :: <value-reference>) => (boolean :: <boolean>);

define method multiple-values? 
    (t :: <value-reference>) => (boolean :: <boolean>);
  #f
end method multiple-values?;

define method multiple-values? (t :: <multiple-value-temporary>)
 => (boolean :: <boolean>);
  #t
end method multiple-values?;
