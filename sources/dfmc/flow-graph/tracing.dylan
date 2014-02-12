Module:   dfmc-flow-graph
Author:   Hannes Mehnert
Synopsis: tracing for computations for live visualizations
Copyright:    Dylan Hackers 2014
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//callback function called for trace events:
// (key-value-pairs :: <plist> => ()
define variable *trace-dfm-callback* :: false-or(<function>) = #f;
//outputter function called on control and data flow node
define variable *trace-dfm-outputter* :: false-or(<function>) = #f;
//method uniquifier.. - use dfmc-debug-back-end and:
// compose(print-specializers, signature-spec)
define variable *trace-dfm-method-printer* :: false-or(<function>) = #f;



//library filter:
//  #f                         -- trace everything
//  #t                         -- trace top level library
//  library-name (as <symbol>) -- only specific library
define variable *trace-dfm-library* :: false-or(<symbol>) = #f;
//file filter
// #f       -- no filter
// filename -- only trace top level defintions with this locator-base
define variable *trace-dfm-file* :: false-or(<symbol>) = #f;
//method filter
// #f                   -- no filter
// method (as <symbol>) -- filter by method name
//   -- NOTE: top-level-initializer is special
// these are automatically generated for class/method initialization
// multiple different DFM thus are named #"top-level-initializer"
// we extend their names with an upwards counting integer.
define variable *trace-dfm-method* :: false-or(<symbol>) = #f;

// HACK: SHOULD BE ELSEWHERE

define inline function debug-string (object) => (res :: false-or(<symbol>))
  let debug-name = object.debug-name;
  if (instance?(debug-name, <variable-name-fragment>))
    debug-name.fragment-identifier
  elseif (debug-name)
    as(<symbol>, debug-name)
  end
end function debug-string;

define inline function trace (#rest args) => (res)
  *trace-dfm-callback*
    & *trace-dfm-callback*(reduce1(concatenate, args))
end;

define inline function output (object) => (res)
  *trace-dfm-outputter*
    & *trace-dfm-outputter*(object)
end;

define inline function tracing-library?
    (library :: false-or(<symbol>))
 => (res :: <boolean>)
  if (~library)
    #t
  else
    library == debug-name(language-definition(current-library-description()))
  end if
end function;

define inline function tracing-file?
    (filename :: false-or(<symbol>), lam :: <&lambda>)
 => (res :: <boolean>)
  if (~filename)
    #t
  else
    let cr = model-compilation-record(lam);
    let sr = compilation-record-source-record(cr);
    let lc = source-record-location(sr);
    as(<symbol>, locator-base(lc)) == filename
  end if
end function;

define inline function tracing-method?
    (methodname :: false-or(<symbol>), lam :: <&lambda>)
 => (res :: <boolean>)
  if (~methodname)
    #t
  else
    debug-string(lam) == methodname
  end if
end function;

define inline function tracing-dfm?
    (code :: <&lambda>) => (well? :: <boolean>)
  (*trace-dfm-callback* & #t)
    & tracing-library?(*trace-dfm-library*)
    & tracing-file?(*trace-dfm-file*, code)
    & tracing-method?(*trace-dfm-method*, code)
end function;

//these are of interest for us!
define constant <node> =
  type-union(<temporary>, <object-reference>, <computation>);

define constant <real-node> =
  type-union(<temporary>, <computation>);

define inline function tracing-node?
    (node :: <real-node>) => (well? :: <boolean>)
  if (node.environment & node.environment.lambda)
    tracing-dfm?(node.environment.lambda)
  end;
end;

//top-level-initializer VOODOO
define variable *lambda-string-table* :: <table> = make(<table>);

define function init-flow-graph ()
  *lambda-string-table* := make(<table>);
end;

define function safe-name (x :: <&lambda>) => (res :: <symbol>)
  let debug-str = x.debug-string;
  let uniquify
    = if (*trace-dfm-method-printer*)
        *trace-dfm-method-printer*(x.signature-spec)
      end;
  let debug-name = as(<symbol>, format-to-string("%s%s", debug-str, uniquify | ""));
  let unique-name = element(*lambda-string-table*, debug-name, default: #f);
  if (unique-name)
    let debug-string = as(<string>, debug-name);
    let name-suffix = find-key(unique-name, curry(\=, x));
    if (name-suffix)
      if (name-suffix == 0)
        as(<symbol>, output(debug-string))
      else
        as(<symbol>, output(concatenate(debug-string, "-", name-suffix.integer-to-string)))
      end
    else
      *lambda-string-table*[debug-name] := pair(x, unique-name);
      as(<symbol>, output(concatenate(debug-string, "-", unique-name.size.integer-to-string)))
    end
  else
    *lambda-string-table*[debug-name] := list(x);
    as(<symbol>, output(debug-string))
  end;
end;

//helpers used in flow-graph and optimization
define inline function trace-dfm-phase
    (code :: <&lambda>, key :: <symbol>, description :: <string>) => ()
  if (tracing-dfm?(code))
    let props = code.source-properties;
    let data = list(type:, key, description:, description);
    trace(data, props);
  end;
end;

define inline function node-properties
    (c :: <real-node>) => (res :: <list>)
  concatenate(list(nodeid:, c.node-id),
              source-properties(c.environment.lambda))
end;

define inline function source-properties
    (lam :: <&lambda>) => (res :: <list>)
  let ld = current-library-description();
  let library = ld.language-definition.debug-name;
  let cr = model-compilation-record(lam);
  let sr = compilation-record-source-record(cr);
  let lc = source-record-location(sr);
  let filename = locator-base(lc);
  let debug = safe-name(lam);
  list(method:, debug, file:, filename, library:, library)
end;

define method trace-dfm-node
    (key :: <symbol>, c :: <real-node>, description :: false-or(<string>))
  if (tracing-node?(c))
    let props = c.node-properties;
    let data = list(type:, key,
                    description:, description);
    trace(data, props);
  end;
end;

define method trace-dfm-node
    (key :: <symbol>, c :: <real-node>, description :: <object>)
  if (tracing-node?(c))
    let props = c.node-properties;
    let data = list(type:, key,
                    description:, output(description));
    trace(data, props);
  end;
end;

define inline method trace-dfm-object-reference
  (key :: <symbol>,
   o :: <object-reference>,
   c :: <real-node>,
   description :: false-or(<string>))
  if (tracing-node?(c))
    let props = c.environment.lambda.source-properties;
    let data = list(type:, key,
                    description:, description,
                    nodeid:, o.node-id);
    trace(data, props);
  end;
end;

define inline method trace-dfm-object-reference
  (key :: <symbol>,
   o :: <object-reference>,
   c :: <real-node>,
   description :: <object>)
  if (tracing-node?(c))
    let props = c.environment.lambda.source-properties;
    let data = list(type:, key,
                    description:, output(description),
                    nodeid:, o.node-id);
    trace(data, props);
  end;
end;

define inline function trace-dfm-nodes
    (cs :: <vector>, key :: <symbol>)
  if (cs.size > 0 & tracing-node?(cs[0]))
    let props = cs[0].environment.lambda.source-properties;
    let ids = map(node-id, cs);
    let data = list(type:, key,
                    nodeids:, ids);
    trace(data, props);
  end;
end;

define generic trace-dfm-connection
    (key :: <symbol>, source :: false-or(<node>), destination :: false-or(<node>));

define method trace-dfm-connection
    (key :: <symbol>, source == #f, destination :: <node>)
end;

define method trace-dfm-connection
    (key :: <symbol>, source :: <node>, destination == #f)
end;

define method trace-dfm-connection
    (key :: <symbol>, source :: <real-node>, destination :: <node>)
  if (tracing-node?(source))
    let props = source.node-properties;
    let data = list(type:, key, other:, destination.node-id);
    trace(data, props);
  end;
end;

define method trace-dfm-connection
    (key :: <symbol>, source :: <object-reference>, destination :: <node>)
  if (tracing-node?(destination))
    let props = destination.node-properties;
    let data = list(type:, key, other:, destination.node-id);
    trace(data, props);
  end
end;

define inline function trace-dfm-reconnection
    (key :: <symbol>, source :: <computation>, getter :: <function>, new :: false-or(<node>))
  if (tracing-node?(source))
    let props = source.node-properties;
    let old = block ()
                getter(source)
              exception (e :: <condition>)
                #f
              end;
    let data = list(type:, key,
                    old: old & old.maybe-node-id,
                    other: new & new.maybe-node-id);
    trace(data, props);
  end;
end;

// Node identifiers and helpers
define variable *current-node-id* :: <integer> = 0;
define inline function next-node-id () => (result :: <integer>)
  *current-node-id* := *current-node-id* + 1;
end;

define method maybe-node-id (o) => (res)
  o
end;

define method maybe-node-id (n :: <node>) => (res)
  n.node-id
end;

define method node-id (c :: <computation>) => (res :: <integer>)
  unless (instance?(c.%node-id, <integer>))
    if (c.environment & c.environment.lambda)
      c.%node-id := next-node-id();
      trace-dfm-node(#"new-computation", c, c);
      if (c.temporary)
        c.temporary.node-id; //side effect: adding temporary
      end;
    else
      error("computation without environment or lambda %=", c)
    end;
  end;
  c.%node-id;
end;

define method node-id (t :: <temporary>)
 => (res :: false-or(<integer>))
  unless (instance?(t.%node-id, <integer>))
    if (t.environment & t.environment.lambda)
      t.%node-id := next-node-id();
      let gen = block ()
                  t.generator
                exception (e :: <condition>)
                  #f
                end;
      trace-dfm-node(#"new-temporary", t, t);
      if (t.users.size > 0)
        do(curry(trace-dfm-connection, #"add-temporary-user", t),
           t.users);
      end;
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
      trace-dfm-object-reference(#"new-object-reference", t, user, t);
      do(curry(trace-dfm-connection, #"add-temporary-user", t),
         t.users);
    end;
  end;
  t.%node-id
end;

//temporary user tracking
define method remove-user! (t :: <object-reference>, c :: <computation>)
  next-method();
  trace-dfm-connection(#"remove-temporary-user", t, c);
  if (t.users.size == 0)
    trace-dfm-object-reference(#"remove-temporary", t, c, #f);
  end;
end;

define method add-user! (t :: <object-reference>, c :: <computation>)
  let add? = (t.users.size == 0);
  next-method();
  trace-dfm-connection(#"add-temporary-user", t, c);
end;

define method remove-user! (t :: <temporary>, c :: <computation>)
  next-method();
  trace-dfm-connection(#"remove-temporary-user", t, c);
end;

define method add-user! (t :: <temporary>, c :: <computation>)
  if (t.environment ~== c.environment)
    trace-dfm-node(#"new-temporary", t, t);
  end;
  next-method();
  trace-dfm-connection(#"add-temporary-user", t, c);
end;

define method remove-user! (t :: <cell>, c :: <computation>)
  dynamic-bind(*trace-dfm-callback* = #f)
    next-method();
  end;
  trace-dfm-connection(#"remove-temporary-user", t, c);
end;

define method add-user! (t :: <cell>, c :: <computation>)
  dynamic-bind(*trace-dfm-callback* = #f)
    next-method();
  end;
  trace-dfm-connection(#"add-temporary-user", t, c);
end;
