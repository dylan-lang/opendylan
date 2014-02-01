Module:   dfmc-flow-graph
Author:   Hannes Mehnert
Synopsis: tracing for computations for live visualizations
Copyright:    Dylan Hackers 2014
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define thread variable *current-node-id* :: <integer> = 0;
define thread variable *computation-tracer* :: false-or(<function>) = #f;

define constant <node> =
  type-union(<temporary>, <object-reference>, <computation>);

define function next-node-id () => (result :: <integer>)
  *current-node-id* := *current-node-id* + 1;
end;

define method node-id (c :: <computation>) => (res :: <integer>)
  unless (instance?(c.%node-id, <integer>))
    c.%node-id := next-node-id();
    maybe-trace-connection(#"new-computation", c, #f);
    if (c.next-computation)
      c.next-computation := c.next-computation; //side effect: trace!
    end;
    if (c.temporary)
      c.temporary.node-id; //side effect: adding temporary
    end;
  end;
  c.%node-id;
end;

define method node-id (t :: <temporary>)
 => (res :: false-or(<integer>))
  unless (instance?(t.%node-id, <integer>))
    t.%node-id := next-node-id();
    let gen = block ()
                t.generator
              exception (e :: <condition>)
                #f
              end;
    maybe-trace-connection(#"new-temporary", t, gen);
    if (t.users.size > 0)
      if (t.generator)
        let new = t.generator.computation-type;
        *computation-tracer*
          & *computation-tracer*(#"type-setter", t, new, t.generator);
      end;
      do(curry(maybe-trace-connection, #"add-temporary-user", t),
         t.users);
    end;
  end;
  t.%node-id
end;

define method node-id (t :: <object-reference>)
 => (res :: false-or(<integer>))
  unless (instance?(t.%node-id, <integer>))
    if (t.users.size > 0)
      t.%node-id := next-node-id();
      let user = t.users.first;
      maybe-trace-connection(#"new-object-reference", t, user);
      do(curry(maybe-trace-connection, #"add-temporary-user", t),
         t.users);
    end;
  end;
  t.%node-id
end;

define method remove-user! (t :: <object-reference>, c :: <computation>)
  next-method();
  maybe-trace-connection(#"remove-temporary-user", t, c);
  if (t.users.size == 0)
    maybe-trace-connection(#"remove-temporary", t, c);
  end;
end;

define method add-user! (t :: <object-reference>, c :: <computation>)
  let add? = (t.users.size == 0);
  next-method();
  maybe-trace-connection(#"add-temporary-user", t, c);
end;

define method remove-user! (t :: <temporary>, c :: <computation>)
  next-method();
  maybe-trace-connection(#"remove-temporary-user", t, c);
end;

define method add-user! (t :: <temporary>, c :: <computation>)
  if (t.environment ~== c.environment)
    maybe-trace-connection(#"new-temporary", t, c);
  end;
  next-method();
  maybe-trace-connection(#"add-temporary-user", t, c);
end;

define method remove-user! (t :: <cell>, c :: <computation>)
  dynamic-bind(*computation-tracer* = #f)
    next-method();
  end;
  maybe-trace-connection(#"remove-temporary-user", t, c);
end;

define method add-user! (t :: <cell>, c :: <computation>)
  dynamic-bind(*computation-tracer* = #f)
    next-method();
  end;
  maybe-trace-connection(#"add-temporary-user", t, c);
end;

define function maybe-trace-change
    (key :: <symbol>, c :: <node>, getter :: <function>, new)
  if (*computation-tracer*)
    let old =
      block ()
        c.getter
      exception (e :: <condition>)
        #f
      end;
    *computation-tracer*(key, c, new, old);
  end;
end;

define function maybe-trace-connection
    (key :: <symbol>, c :: <node>, other)
  if (*computation-tracer*)
    *computation-tracer*(key, c, other, #f);
  end;
end;
