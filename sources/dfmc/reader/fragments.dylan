Module: dfmc-reader
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define thread variable *fragment-context* = #f;

define constant $nowhere = #f;

define macro nowhere-or
  { nowhere-or(?:expression) } => { false-or(?expression) }
end macro;

define macro with-fragment-info
  { with-fragment-info (?frag:expression) ?:body end }
    => { ?body }
end macro;

//// Fragment classes.

define constant $literal-token = $number-token;

define abstract class <fragment> (<object>)
  slot fragment-record :: false-or(<compilation-record>) = #f,
    init-keyword: record:;
  slot fragment-source-position = $nowhere,
    init-keyword: source-position:;
end class;

define generic fragment-source-location
    (f :: <object>) => (loc :: nowhere-or(<source-location>));

define method fragment-source-location
    (f :: <fragment>) => (loc :: nowhere-or(<source-location>));
  record-position-as-location(fragment-record(f), fragment-source-position(f))
end method;

// TODO: Is this hack still used? If not, tighten the arg decl above.
define method fragment-source-location
    (f :: <sequence>) => (loc :: nowhere-or(<source-location>))
  // between(f.first, f.last);
  fragment-source-location(f.first);
end method;

define sealed domain make (subclass(<fragment>));
define sealed domain initialize (<fragment>);

define compiler-open class <fragment-copier> (<copier>) end;

define dont-copy-object <compilation-record> using <fragment-copier>;
define dont-copy-object <source-record>      using <fragment-copier>;

define inline method compute-position-between* (p1, p2)
  let start-offset = p1.source-position-start-offset;
  let end-offset   = p2.source-position-end-offset;
  if (range-source-offset-greater-than?(start-offset, end-offset))
    make-range-position(p2.source-position-start-offset,
                        p1.source-position-end-offset);
  else
    make-range-position(start-offset, end-offset);
  end if
end method;

define method compute-position-between (p1, p2)
  if (~p1) p2
  elseif (~p2) p1
  else
    compute-position-between*(p1, p2)
  end;
end method;

define method position-between (f1 == #f, f2 == #f) #f end;

define method position-between (f1 == #f, f2)
  position-between(#(), f2);
end;

define method position-between (f1, f2 == #f)
  position-between(f1, #());
end method;

define method position-between (f1 :: <fragment>, f2 :: <fragment>)
  compute-position-between
    (f1.fragment-source-position, f2.fragment-source-position)
end method;

define method position-between (f1 :: <fragment>, f2 :: <list>)
  if (empty?(f2))
    f1.fragment-source-position;
  else
    position-between(f1, f2.last)
  end;
end method;

define method position-between (f1 :: <list>, f2 :: <list>)
  position-between(if (empty?(f1)) #f else f1.first end,
                   if (empty?(f2)) #f else f2.last end)
end method;

define method position-between (f2 :: <list>, f1 :: <fragment>)
  if (empty?(f2))
    f1.fragment-source-position
  else
    position-between(f2.first, f1)
  end;
end method;

define method position-spanning (f*)
  if (empty?(f*))
    $nowhere
  else
    // format-out("First loc: %=\n", fragment-source-location(f*.first));
    // format-out("Last loc: %=\n", fragment-source-location(f*.last));
    position-between(f*.first, f*.last);
  end;
end method;

define method spanning (f*)
  let pos = position-spanning(f*);
  if (pos)
    record-position-as-location(f*.first.fragment-record, pos)
  else
    $nowhere
  end;
end method;

define abstract class <compound-fragment> (<fragment>) end;
define abstract class <elementary-fragment> (<fragment>) end;

define generic fragment-kind (fragment :: <fragment>) => kind;

// Hygiene mixins.

define thread variable *expansion-identifier* = #f;

define class <expansion-identifier> (<object>) end;

define inline function do-with-new-hygiene-context (f, mac)
  ignore(mac);
  dynamic-bind
      (*expansion-identifier* = make(<expansion-identifier>))
    f();
  end;
end function;

define macro with-new-hygiene-context
  { with-new-hygiene-context (?mac:expression) ?:body end }
    => { do-with-new-hygiene-context
           (method () ?body end, ?mac) }
end macro;

define function make-unique-local-variable-name-fragment (name)
  with-new-hygiene-context (#"unknown")
    make(<variable-name-fragment>,
         name: as(<symbol>, concatenate("_unique-", as(<string>, name))),
         record: #f,
         source-position: #f);
  end;
end function;

define abstract class <hygienic-fragment> (<fragment>)
end class;

define generic hygienic-fragment-info (f :: <hygienic-fragment>) => info;

define generic hygienic-fragment-context (info) => context;

define method hygienic-fragment-context (context) => context;
  context
end method;

define method fragment-context (f :: <hygienic-fragment>)
  hygienic-fragment-context(f.hygienic-fragment-info)
end method;

define generic hygienic-fragment-origin (info) => origin;

define method hygienic-fragment-origin (context) => origin;
  #f
end method;

define method fragment-origin (f :: <hygienic-fragment>)
  hygienic-fragment-origin(f.hygienic-fragment-info)
end method;

// This is only used when there is a non-#f origin (less than 5% of the cases)
define made-inline class <hygienic-fragment-info> (<object>)
  constant slot hygienic-fragment-context,
    required-init-keyword: context:;
  constant slot hygienic-fragment-origin,
    required-init-keyword: origin:;
end class;

define sealed domain make (subclass(<hygienic-fragment-info>));
define sealed domain initialize (<hygienic-fragment-info>);

define inline method initialize
    (f :: <hygienic-fragment>, #key context = *fragment-context*,
                                    origin = *expansion-identifier*)
 => ()
  next-method();
  if (origin)
    hygienic-fragment-info(f)
      := make(<hygienic-fragment-info>,
              context: context, origin: origin);
  else
    hygienic-fragment-info(f)
      := context
  end;
end method;

define abstract class <literal-fragment> (<fragment>)
  slot fragment-value,
    init-keyword: value:;
end class;

define constant <literal-constant-fragment> = <literal-fragment>;

// TODO: CORRECTNESS: Provide a subclass of <elementary-literal-fragment>
// for use for "other values", rather than instantiating this one.

define /* abstract */ class <elementary-literal-fragment>
    (<literal-fragment>, <elementary-fragment>)
end class;

define method fragment-kind (f :: <elementary-literal-fragment>) => kind;
  $literal-token
end method;

define /* abstract */ class <boolean-fragment> (<elementary-literal-fragment>)
end class;

define class <true-fragment> (<boolean-fragment>)
  inherited slot fragment-value = #t;
  // keyword value: = #t;
end class;

define class <false-fragment> (<boolean-fragment>)
  inherited slot fragment-value = #f;
  // keyword value: = #f;
end class;

define /* abstract */ class <character-fragment>
    (<elementary-literal-fragment>)
end class;

define method fragment-kind (f :: <character-fragment>) => kind;
  $character-literal-token
end method;

define /* abstract */ class <string-fragment> (<elementary-literal-fragment>)
end class;

define method fragment-kind (f :: <string-fragment>) => kind;
  $string-token
end method;


define /* abstract */ class <symbol-fragment> (<elementary-literal-fragment>)
end class;

define method fragment-kind (f :: <symbol-fragment>) => kind;
  $symbol-token
end method;


define method as
    (class == <symbol>, name :: <symbol-fragment>) => (symbol :: <symbol>)
  fragment-value(name);
end method;

// This distinction is only maintained for displaying parsed code as
// it was read.

define class <symbol-syntax-symbol-fragment> (<symbol-fragment>) end;
define class <keyword-syntax-symbol-fragment> (<symbol-fragment>) end;

define /* abstract */ class <number-fragment>
    (<literal-fragment>, <elementary-fragment>)
end class;

define method fragment-kind (f :: <number-fragment>) => kind;
  $literal-token
end method;


// TODO: CORRECTNESS: Decide how to represent big integers.

define /* abstract */ class <abstract-integer-fragment> (<number-fragment>) end;
define class <integer-fragment> (<abstract-integer-fragment>) end;
define class <big-integer-fragment> (<abstract-integer-fragment>) end;

define abstract class <literal-sequence-fragment>
    (<literal-fragment>, <compound-fragment>)
  constant slot fragment-elements,
    required-init-keyword: elements:;
  inherited slot fragment-value = #f;
  // keyword value: = #f;
end class;

// TODO: CORRECTNESS: Get this representation right. We need to be able
// to represent both fragments from the parser and literal objects
// made given just a list value.

define class <list-fragment> (<literal-sequence-fragment>) end;

define method fragment-kind (f :: <list-fragment>) => kind;
  $parsed-list-constant-token;
end method;

define class <proper-list-fragment> (<list-fragment>) end;

define inline method initialize (f :: <proper-list-fragment>, #key)
  if (~fragment-value(f))
    fragment-value(f)
      := map-as(<list>, fragment-value, fragment-elements(f));
  end;
end method;

define class <improper-list-fragment> (<list-fragment>)
  constant slot fragment-improper-tail,
    required-init-keyword: improper-tail:;
end class;

define inline method initialize (f :: <improper-list-fragment>, #key)
  if (~fragment-value(f))
    for (val = fragment-value(fragment-improper-tail(f))
           then pair(fragment-value(next), val),
         next in reverse(fragment-elements(f)))
    finally
      fragment-value(f) := val;
    end;
  end;
end method;

define class <vector-fragment> (<literal-sequence-fragment>) end;

define inline method initialize (f :: <vector-fragment>, #key)
  if (~fragment-value(f))
    fragment-value(f)
      := map-as(<vector>, fragment-value, fragment-elements(f));
  end;
end method;

define method fragment-kind (f :: <vector-fragment>) => kind;
  $parsed-vector-constant-token;
end method;

define abstract class <punctuation-fragment> (<elementary-fragment>) end;

define class <dot-fragment> (<punctuation-fragment>) end;

define method fragment-kind (f :: <dot-fragment>) => kind;
  $dot-token
end method;

define abstract class <separator-fragment> (<punctuation-fragment>) end;

define class <comma-fragment> (<separator-fragment>) end;

define method fragment-kind (f :: <comma-fragment>) => kind;
  $comma-token
end method;

define class <semicolon-fragment> (<separator-fragment>) end;

define method fragment-kind (f :: <semicolon-fragment>) => kind;
  $semicolon-token
end method;

define class <colon-colon-fragment> (<punctuation-fragment>) end;

define method fragment-kind (f :: <colon-colon-fragment>) => kind;
  $colon-colon-token
end method;

define class <equal-greater-fragment> (<punctuation-fragment>) end;

define method fragment-kind (f ::  <equal-greater-fragment>) => kind;
  $equal-greater-token
end method;

define abstract class <hash-word-fragment> (<punctuation-fragment>) end;

define class <hash-next-fragment> (<hash-word-fragment>) end;

define method fragment-kind (f ::  <hash-next-fragment>) => kind;
  $hash-next-token
end method;

define class <hash-rest-fragment> (<hash-word-fragment>) end;

define method fragment-kind (f ::  <hash-rest-fragment>) => kind;
  $hash-rest-token
end method;

define class <hash-key-fragment> (<hash-word-fragment>) end;

define method fragment-kind (f ::  <hash-key-fragment>) => kind;
  $hash-key-token
end method;

define class <hash-all-keys-fragment> (<hash-word-fragment>) end;

define method fragment-kind (f ::  <hash-all-keys-fragment>) => kind;
  $hash-all-keys-token
end method;

define abstract class <open-paren-fragment> (<punctuation-fragment>) end;
define abstract class <close-paren-fragment> (<punctuation-fragment>) end;

define class <lparen-fragment> (<open-paren-fragment>) end;

define method fragment-kind (f ::  <lparen-fragment>) => kind;
  $lparen-token
end method;

define class <rparen-fragment> (<close-paren-fragment>) end;

define method fragment-kind (f ::  <rparen-fragment>) => kind;
  $rparen-token
end method;

define class <lbracket-fragment> (<open-paren-fragment>, <hygienic-fragment>)
  // constant slot hygienic-fragment-info, required-init-keyword: info:;
  slot hygienic-fragment-info = #f, init-keyword: info:;
end class;

define method fragment-kind (f :: <lbracket-fragment>) => kind;
  $lbracket-token
end method;

define class <rbracket-fragment> (<close-paren-fragment>) end;

define method fragment-kind (f :: <rbracket-fragment>) => kind;
  $rbracket-token
end method;

define class <lbrace-fragment> (<open-paren-fragment>) end;

define method fragment-kind (f :: <lbrace-fragment>) => kind;
  $lbrace-token
end method;

define class <rbrace-fragment> (<close-paren-fragment>) end;

define method fragment-kind (f ::  <rbrace-fragment>) => kind;
  $rbrace-token
end method;

define class <hash-lparen-fragment> (<open-paren-fragment>) end;

define method fragment-kind (f :: <hash-lparen-fragment>) => kind;
  $hash-lparen-token
end method;

define class <hash-lbracket-fragment> (<open-paren-fragment>) end;

define method fragment-kind (f ::  <hash-lbracket-fragment>) => kind;
  $hash-lbracket-token
end method;

define class <hash-lbrace-fragment>
    (<open-paren-fragment>, <hygienic-fragment>)
  // constant slot hygienic-fragment-info, required-init-keyword: info:;
  slot hygienic-fragment-info = #f, init-keyword: info:;
end class;

define method fragment-kind (f ::  <hash-lbrace-fragment>) => kind;
  $hash-lbrace-token
end method;


// Macro system punctuation.

define class <query-fragment> (<punctuation-fragment>) end;

define method fragment-kind (f ::  <query-fragment>) => kind;
  $query-token
end method;


define class <query-query-fragment> (<punctuation-fragment>) end;

define method fragment-kind (f ::  <query-query-fragment>) => kind;
  $query-query-token
end method;

define class <query-equal-fragment> (<punctuation-fragment>) end;

define method fragment-kind (f ::  <query-equal-fragment>) => kind;
  $query-equal-token
end method;

define class <query-at-fragment> (<punctuation-fragment>) end;

define method fragment-kind (f ::  <query-at-fragment>) => kind;
  $query-at-token
end method;

define class <ellipsis-fragment> (<punctuation-fragment>, <hygienic-fragment>)
  // constant slot hygienic-fragment-info, required-init-keyword: info:;
  slot hygienic-fragment-info = #f, init-keyword: info:;
end class;

define method fragment-kind (f :: <ellipsis-fragment>) => kind;
  $ellipsis-token
end method;


define class <constrained-name-fragment>
    (<elementary-fragment>, <hygienic-fragment>)
  constant slot fragment-kind = $constrained-name-token,
    init-keyword: kind:;
  // constant slot hygienic-fragment-info, required-init-keyword: info:;
  slot hygienic-fragment-info = #f, init-keyword: info:;
  constant slot fragment-name,
    required-init-keyword: name:;
  constant slot fragment-constraint,
    required-init-keyword: constraint:;
end class;

define class <hash-hash-fragment> (<punctuation-fragment>) end;

define method fragment-kind (f ::  <hash-hash-fragment>) => kind;
  $hash-hash-token
end method;



define class <escaped-substitution-fragment> (<punctuation-fragment>)
  constant slot fragment-escaped-fragment,
    required-init-keyword: escaped-fragment:;
end class;

define method fragment-kind (f ::  <escaped-substitution-fragment>) => kind;
  $escaped-substitution-token
end method;


define inline function make-escaped-punctuation
    (class, source-location :: <lexer-source-location>)
  let rec = source-location.source-location-record;
  let pos = source-location.source-location-source-position;
  make(<escaped-substitution-fragment>,
       record: rec,
       source-position: pos,
       escaped-fragment: make(class, record: rec, source-position: pos));
end function;

define function make-escaped-query (lexer, source-location)
  make-escaped-punctuation(<query-fragment>, source-location);
end function;

define function make-escaped-query-query (lexer, source-location)
  make-escaped-punctuation(<query-query-fragment>, source-location);
end function;

define function make-escaped-query-equal (lexer, source-location)
  make-escaped-punctuation(<query-equal-fragment>, source-location);
end function;

define function make-escaped-ellipsis (lexer, source-location)
  make-escaped-punctuation(<ellipsis-fragment>, source-location);
end function;

define function make-escaped-colon-colon (lexer, source-location)
  make-escaped-punctuation(<colon-colon-fragment>, source-location);
end function;

define function make-escaped-hash-next-fragment (lexer, source-location)
  make-escaped-punctuation(<hash-next-fragment>, source-location);
end function;

define function make-escaped-hash-rest-fragment (lexer, source-location)
  make-escaped-punctuation(<hash-rest-fragment>, source-location);
end function;

define function make-escaped-hash-key-fragment (lexer, source-location)
  make-escaped-punctuation(<hash-key-fragment>, source-location);
end function;

define function make-escaped-hash-all-keys-fragment (lexer, source-location)
  make-escaped-punctuation(<hash-all-keys-fragment>, source-location);
end function;

//// Names and stuff.

define class <name-fragment> (<elementary-fragment>, <hygienic-fragment>)
  constant slot fragment-name :: <symbol>,
    init-keyword: name:;
end class;

define method same-name-when-local?
    (name1 :: <name-fragment>, name2 :: <name-fragment>)
 => (same? :: <boolean>)
  fragment-name(name1) == fragment-name(name2)
    & fragment-origin(name1) == fragment-origin(name2)
end method;

define method as
    (class == <symbol>, name :: <name-fragment>) => (symbol :: <symbol>)
  fragment-name(name);
end method;

define method as
    (class == <string>, name :: <name-fragment>) => (symbol :: <string>)
  as(<string>, fragment-name(name));
end method;

define method fragment-name-string
    (name :: <name-fragment>) => (name-string :: <string>)
  as(<string>, fragment-name(name));
end method;

// TODO: Turn all this into a sensible inheritance hierarchy again!

define class <variable-name-fragment> (<name-fragment>, <variable-name>) end;

define method hygienic-fragment-info-setter
    (info, var :: <variable-name-fragment>) => (info)
  // Should assert.
  info
end method;

define constant fragment-identifier = fragment-name;

// A fragment with everything on it.
define made-inline class <special-variable-name-fragment>
    (<variable-name-fragment>)
  // constant slot hygienic-fragment-info,
  //  required-init-keyword: info:;
  slot hygienic-fragment-info = #f, init-keyword: info:;
  constant slot fragment-kind = $unreserved-name-token,
    init-keyword: kind:;
end;

// A hygiene-less Dylan variable name.
define made-inline class <dylan-variable-name-fragment>
    (<variable-name-fragment>)
  constant slot fragment-kind = $unreserved-name-token,
    init-keyword: kind:;
end;

define method hygienic-fragment-info
    (var :: <dylan-variable-name-fragment>) => (info);
  #f // indicates the Dylan module
end method;

define made-inline class <simple-variable-name-fragment>
    (<variable-name-fragment>)
end class;

define method fragment-kind (var :: <simple-variable-name-fragment>) => kind;
  $unreserved-name-token
end method;

define method hygienic-fragment-info
    (var :: <simple-variable-name-fragment>) => (info);
  let rec = fragment-record(var);
  rec & compilation-record-module(rec)
end method;

// As above, but always in the Dylan library. This allows the record/source
// position not to have to match the module context.
define made-inline class <simple-dylan-variable-name-fragment>
    (<simple-variable-name-fragment>)
end class;

define method hygienic-fragment-info
    (var :: <simple-dylan-variable-name-fragment>) => (info);
  #f // indicates the Dylan module
end method;

// A fragment without a hygiene context and with a derivable module context.
define made-inline class <simple-classified-variable-name-fragment>
    (<variable-name-fragment>)
  constant slot fragment-kind = $unreserved-name-token,
    init-keyword: kind:;
end;

define method hygienic-fragment-info
    (var :: <simple-classified-variable-name-fragment>) => (info);
  let rec = fragment-record(var);
  rec & compilation-record-module(rec)
end method;

define inline method make
    (class == <variable-name-fragment>, #rest initargs,
       #key kind = $unreserved-name-token,
            context = *fragment-context*,
            origin = *expansion-identifier*,
            record,
            source-position,
            name,
       #all-keys) => (var :: <variable-name-fragment>);
  if (origin)
    // If we have a hygiene context, we just go the whole hog.
    // format-out("Origin special: %s, %=\n", name, context);
    apply(make, <special-variable-name-fragment>, initargs)
  elseif (~context)
    // If we're in the Dylan library, choose the most compact rep we
    // have.
    if (kind == $unreserved-name-token)
      make(<simple-dylan-variable-name-fragment>,
           name: name,
           record: record,
           source-position: source-position);
    else
      make(<dylan-variable-name-fragment>,
           name: name,
           kind: kind,
           record: record,
           source-position: source-position);
    end;
  else
    // We're outside the Dylan library, but we can still have a more compact
    // rep if the source location matches the expansion module context.
    let simple? = record & context;
    let cr = simple? & instance?(record, <compilation-record>) & record;
    let m = cr & compilation-record-module(cr);
    if (simple? & (m == context))
      if (kind == $unreserved-name-token)
        make(<simple-variable-name-fragment>,
                  name: name,
             record: cr,
             source-position: source-position);
      else
        make(<simple-classified-variable-name-fragment>,
                  name: name,
             kind: kind,
             record: cr,
             source-position: source-position);
      end;
    else
      // format-out("Special: %s, %=\n", name, context);
      apply(make, <special-variable-name-fragment>, initargs)
    end;
  end;
end method;

// Implemented in dfmc-flow-graph
define compiler-open generic fragment-module
  (fragment :: <variable-name-fragment>) => (module);

define function dylan-variable-name (name)
  make(<simple-variable-name-fragment>,
       name: name,
       record: #f, source-position: #f);
end function;

define sideways method make-variable-name-fragment (name) => (variable-name)
  dylan-variable-name(name)
end method;

define method make-variable-name-fragment-in-module
    (name, module) => (new-name :: <name-fragment>)
  make(<variable-name-fragment>,
       name: name,
       context: module,
       record: #f,
       source-position: #f)
end method;

define method make-variable-name-like
    (name :: <hygienic-fragment>, #rest keys)
 => (new-name :: <name-fragment>)
  apply(make, <variable-name-fragment>,
        context: fragment-context(name),
        origin:  fragment-origin(name),
        keys);
end method;

define method splice-name-hygienically
    (name :: <name-fragment>, prefix :: <string>, suffix :: <string>)
 => (new-name :: <name-fragment>)
  let spliced-name
    = as(<symbol>, concatenate(prefix, fragment-name-string(name), suffix));
  make-variable-name-like(name,
       record: fragment-record(name),
       source-position: fragment-source-position(name),
       kind: classify-expansion-word-in(fragment-context(name), spliced-name),
       name: spliced-name);
end method;

define method suffix-name-hygienically
    (name :: <name-fragment>, suffix :: <string>)
 => (new-name :: <name-fragment>)
  make-variable-name-like(name,
                          record: fragment-record(name),
                          source-position: fragment-source-position(name),
                          name: as(<symbol>,
                                   concatenate(fragment-name-string(name),
                                               suffix)));
end method;

define class <escaped-name-fragment> (<special-variable-name-fragment>) end;

define class <operator-fragment> (<special-variable-name-fragment>) end;

define class <binary-operator-fragment>
    (<operator-fragment>, <separator-fragment>)
  inherited slot fragment-kind = $binary-operator-only-token;
  // keyword kind: = $binary-operator-only-token;
end class;

define class <unary-operator-fragment> (<operator-fragment>)
  inherited slot fragment-kind = $unary-operator-only-token;
  // keyword kind: = $unary-operator-only-token;
end class;

define class <unary-and-binary-operator-fragment>
    (<unary-operator-fragment>, <binary-operator-fragment>)
  inherited slot fragment-kind = $unary-and-binary-operator-token;
  // keyword kind: = $unary-and-binary-operator-token;
end class;

define class <equal-fragment>
    (<operator-fragment>, <punctuation-fragment>)
  inherited slot fragment-kind = $equal-token;
  inherited slot fragment-name = #"=";
  // keyword kind: = $equal-token;
  // keyword name: = #"=";
end class;

define class <sequence-fragment> (<compound-fragment>)
  constant slot fragment-fragments,
    required-init-keyword: fragments:;
end class;

// We compute sequence fragment locations lazily on the grounds that they're
// only usually needed for error reporting, or only accessed once if they
// are used for anything else.

define method fragment-source-location
    (f :: <sequence-fragment>) => (loc :: nowhere-or(<source-location>));
  // format-out(">>>> Sequence source location of %=\n", f);
  let f* = fragment-fragments(f);
  if (empty?(f*))
    next-method();
  else
    let f1 = f*.first;
    let f2 = f*.last;
    if (fragment-record(f1) == fragment-record(f2))
      record-position-as-location
        (fragment-record(f1), position-between(f1, f2));
    else
      // Our final fallback is to prune out everything macro-generated, and
      // try again.
      let user-f* = choose(fragment-record, f*);
      let f1 = user-f*.first;
      let f2 = user-f*.last;
      if (fragment-record(f1) == fragment-record(f2))
        record-position-as-location
          (fragment-record(f1), position-between(f1, f2));
      else
        #f
      end;
    end;
  end;
end method;

// TODO: Remove - hack. This because of the canonicalisation of a
// wildcard match to a single element if of length one.

define method fragment-fragments (f :: <fragment>) list(f) end;

define abstract class <function-call-fragment> (<compound-fragment>)
  slot fragment-function,
    required-init-keyword: function:;
  constant slot fragment-arguments,
    required-init-keyword: arguments:;
end class;

// TODO: CORRECTNESS: Generating a macro call for an array syntax call
// where appropriate. Same thing for -setter calls, if they don't simply
// always get reparsed.

define class <prefix-call-fragment> (<function-call-fragment>) end;
define class <dot-call-fragment> (<function-call-fragment>) end;
define class <array-call-fragment> (<function-call-fragment>) end;

define class <binary-operator-call-fragment> (<function-call-fragment>) end;
define class <unary-operator-call-fragment> (<function-call-fragment>) end;

// TODO: CORRECTNESS: Need to reclassify the call as a macro if necessary.
// Perhaps we should do this transformation elsewhere? See also how
// element is handled.

define inline method initialize (call :: <unary-operator-call-fragment>, #key)
  next-method();
  let func = fragment-function(call);
  if (fragment-name(func) == #"-")
    fragment-function(call)
      := make-variable-name-like(func,
                                 record: fragment-record(func),
                                 source-position: fragment-source-position(func),
                                 name:    #"negative",
                                 kind:    $unreserved-name-token);
  end;
end method;

define class <body-fragment> (<compound-fragment>)
  constant slot fragment-constituents,
    required-init-keyword: constituents:;
end class;

define method fragment-kind (f :: <body-fragment>) => kind;
  $parsed-macro-call-token;
end method;

define function body-fragment (f*)
  let rec = if (f* ~== #()) f*.head.fragment-record end;
  collecting (folded)
    iterate walk (cursor = f*)
      if (cursor == #())
        make(<body-fragment>,
             record: rec,
             source-position: position-spanning(f*),
             constituents: f*);
      else
        let lead = cursor.head;
        if (instance?(lead, <local-declaration-fragment>))
          let sub-body = body-fragment(cursor.tail);
          collect-into
            (folded,
             make(<local-declaration-call-fragment>,
                  record: rec,
                  source-position: position-between(f*.head, sub-body),
                  declaration-fragment: lead,
                  body-fragment: sub-body));
          let folded = collected(folded);
          make(<body-fragment>,
               record: rec,
               source-position: position-spanning(folded),
               constituents: folded);
        else
          collect-into(folded, lead);
          walk(cursor.tail);
        end;
      end;
    end;
  end;
end function;

define function empty-body-fragment ()
  make(<body-fragment>, record: #f, source-position: $nowhere, constituents: #());
end function;

define abstract class <expression-fragment> (<compound-fragment>) end;
define abstract class <macro-call-fragment> (<compound-fragment>) end;

define class <macro-definition-fragment> (<compound-fragment>) end;

define abstract class <nested-fragment> (<compound-fragment>)
  constant slot fragment-left-delimiter,
    required-init-keyword: left-delimiter:;
  constant slot fragment-nested-fragments,
    required-init-keyword: nested-fragments:;
  constant slot fragment-right-delimiter,
    required-init-keyword: right-delimiter:;

  inherited slot fragment-record = #f;
  inherited slot fragment-source-position = $nowhere;
  // keyword record: = #f;
  // keyword source-position: = $nowhere;
end class;

define method nested-fragment?
    (f) => (well? :: <boolean>, left, right)
  values(#f, #f, #f);
end method;

define method nested-fragment?
    (f :: <nested-fragment>)
 => (well? :: <boolean>,
       left :: <punctuation-fragment>, right :: <punctuation-fragment>)
  values(#t, fragment-left-delimiter(f), fragment-right-delimiter(f))
end method;

define class <parens-fragment> (<nested-fragment>) end;
define class <brackets-fragment> (<nested-fragment>) end;
define class <braces-fragment> (<nested-fragment>) end;

//// Conditional compilation fragments.

/* CMU only
define abstract class <hash-directive-fragment> (<elementary-fragment>)
  constant slot fragment-kind,
    required-init-keyword: kind:;
end;

define class <hash-if-fragment> (<hash-directive-fragment>) end;
define class <hash-else-fragment> (<hash-directive-fragment>) end;
define class <hash-elseif-fragment> (<hash-directive-fragment>) end;
define class <hash-endif-fragment> (<hash-directive-fragment>) end;
*/

//// Pseudo-fragments used to delimit the reparsing of macro constraints.

define class <pseudo-fragment> (<elementary-fragment>)
  constant slot fragment-kind,
    required-init-keyword: kind:;
  inherited slot fragment-record = #f;
  inherited slot fragment-source-position = $nowhere;
  // keyword record: = #f;
  // keyword source-position: = $nowhere;
end class;

define constant $start-token-constraint
  = make(<pseudo-fragment>, kind: $token-constraint-token);

define constant $start-name-constraint
  = make(<pseudo-fragment>, kind: $name-constraint-token);

define constant $start-expression-constraint
  = make(<pseudo-fragment>, kind: $expression-constraint-token);

define constant $start-variable-constraint
  = make(<pseudo-fragment>, kind: $variable-constraint-token);

define constant $start-body-constraint
  = make(<pseudo-fragment>, kind: $body-constraint-token);

define constant $start-case-body-constraint
  = make(<pseudo-fragment>, kind: $case-body-constraint-token);

define constant $start-property-list-constraint
  = make(<pseudo-fragment>, kind: $property-list-constraint-token);

define constant $start-fragment-constraint
  = make(<pseudo-fragment>, kind: $fragment-constraint-token);

define constant $end-constraint
  = make(<pseudo-fragment>, kind: $end-constraint-token);

//// Special fragments.

define abstract class <special-fragment> (<elementary-fragment>)
  constant slot fragment-kind,
    init-keyword: kind:;
end;

define class <eof-marker> (<special-fragment>)
  inherited slot fragment-kind = $eof-token;
  // keyword kind: = $eof-token;
end class;

define class <end-of-modifiers-marker> (<special-fragment>) end;

define method end-of-modifiers-marker? (fragment) => (well? :: <boolean>)
  // Temporarily, for bootstrapping purposes, any defining word matches
  // as an end modifier. TODO: Remove this bootstrapping hack.
  instance?(fragment, <end-of-modifiers-marker>)
    /*
    | (instance?(fragment, <variable-name-fragment>)
         & definer-or-merged-token-class?(fragment-kind(fragment)))
    */
end method;

// define constant $eof-marker = make(<eof-marker>,
//                                    record: #f,
//                                    source-position: $nowhere);

//// Create a fragment value from a Dylan value.

define method as-fragment-value (object) object end;
define method as-fragment-float-value (class, object) object end;

//// A model object fed back into the compiler as syntax.

define class <model-fragment> (<literal-fragment>) end;

define method parsed-literal (o)
  make(<model-fragment>, record: #f, source-position: $nowhere, value: o);
end method;

define method fragment-kind (f :: <model-fragment>) => kind;
  $literal-token;
end method;

// define method as-fragment (o)
//   make-in-expansion(<model-fragment>, value: o);
// end method;

//// The unbound object.

// The unbound class is defined as a compiler space object and mapped to
// to make it consistent with the unique literals #t, #f, etc. It's also
// allowed to appear as a literal in code.

define class <mapped-unbound> (<object>) end;
define constant $unbound = make(<mapped-unbound>);

define dont-copy-object <mapped-unbound> using <fragment-copier>;

//// Coercion.

define compiler-open generic as-fragment (object);

define method as-fragment (o :: <mapped-unbound>)
  parsed-literal(o)
end method;

//// Reparsing.

define method fragment-kind (f :: <function-call-fragment>) => kind;
  $parsed-function-call-token;
end method;

//// Shallow parsing.

define class <statement-fragment> (<macro-call-fragment>)
  slot fragment-macro,
    required-init-keyword: macro:;
  constant slot fragment-body-fragment,
    required-init-keyword: body-fragment:;
  constant slot fragment-end-word = #f,
    init-keyword: end-word:;
end class;

define method fragment-kind (f :: <statement-fragment>) => kind;
  $parsed-macro-call-token;
end method;

define method fragment-argument (f :: <statement-fragment>)
  fragment-body-fragment(f);
end method;

define program-warning <statement-tail-mismatch>
  slot condition-statement-name,
    init-keyword: statement-name:;
  format-string "Mismatched end clause in %s statement.";
  format-arguments statement-name;
end program-warning;

define function verify-statement-tail
    (macro-word :: <name-fragment>, tail :: false-or(<name-fragment>))
 => ()
  if (tail & fragment-name(tail) ~== #"end")
    if (fragment-name(tail) ~== fragment-name(macro-word))
      note(<statement-tail-mismatch>,
           source-location:
             record-position-as-location
               (fragment-record(macro-word),
                position-between(macro-word, tail)),
           statement-name: macro-word);
    end;
  end;
end function;

define class <function-macro-fragment> (<macro-call-fragment>)
  slot fragment-macro,
    required-init-keyword: macro:;
  constant slot fragment-body-fragment,
    required-init-keyword: body-fragment:;
end class;

define method fragment-kind (f :: <function-macro-fragment>) => kind;
  $parsed-macro-call-token;
end method;

define method fragment-argument (f :: <function-macro-fragment>)
  fragment-body-fragment(f);
end method;

define class <local-declaration-fragment> (<macro-call-fragment>)
  slot fragment-macro,
    required-init-keyword: macro:;
  constant slot fragment-list-fragment,
    required-init-keyword: list-fragment:;
end class;

define method fragment-kind (f :: <local-declaration-fragment>) => kind;
  $parsed-local-declaration-token;
end method;

define class <local-declaration-call-fragment> (<macro-call-fragment>)
  constant slot fragment-declaration-fragment,
    required-init-keyword: declaration-fragment:;
  constant slot fragment-body-fragment,
    required-init-keyword: body-fragment:;
end class;

define method fragment-macro (f :: <local-declaration-call-fragment>)
  fragment-macro(fragment-declaration-fragment(f));
end method;

define method fragment-kind (f :: <local-declaration-call-fragment>) => kind;
  $parsed-macro-call-token;
end method;

define abstract class <definition-fragment> (<macro-call-fragment>)
  slot fragment-macro;
  constant slot fragment-modifiers,
    required-init-keyword: modifiers:;
  constant slot fragment-define-word,
    required-init-keyword: define-word:;
end class;

define /* abstract */ class <definition-tail-fragment> (<compound-fragment>)
  constant slot fragment-end,
    required-init-keyword: end:;
  constant slot fragment-tail-name-1 = #f,
    init-keyword: tail-name-1:;
  constant slot fragment-tail-name-2 = #f,
    init-keyword: tail-name-2:;
end class;

ignore(fragment-end);

define program-warning <definition-tail-mismatch>
  slot condition-definition-name,
    init-keyword: definition-name:;
  format-string "Mismatched end clause in %s definition.";
  format-arguments definition-name;
end program-warning;

define function verify-definition-tail
    (lead-word :: <name-fragment>, macro-word :: <name-fragment>,
       maybe-name :: false-or(<name-fragment>),
       tail :: <definition-tail-fragment>)
 => ()
  let name-1 = fragment-tail-name-1(tail);
  let name-2 = fragment-tail-name-2(tail);
  if (name-2)
    // We know we have both a macro word and a name.
    if (fragment-name(name-1) ~== fragment-name(macro-word)
          | (maybe-name
               & fragment-name(name-2) ~== fragment-name(maybe-name)))
      note(<definition-tail-mismatch>,
           source-location:
             record-position-as-location
               (fragment-record(lead-word),
                position-between(lead-word, tail)),
           definition-name: macro-word);
    end;
  elseif (name-1)
    // We have one name that could validly be either the macro word or
    // the name of the thing being defined.
    if ((fragment-name(name-1) ~== fragment-name(macro-word)
            & (definer-or-merged-token-class?(fragment-kind(name-1))
                | (maybe-name
                     & fragment-name(name-1) ~== fragment-name(maybe-name)))))
      note(<definition-tail-mismatch>,
           source-location:
             record-position-as-location
               (fragment-record(lead-word),
                position-between(lead-word, tail)),
           definition-name: macro-word);
    end;
  end;
end function;

define function maybe-defined-name
    (fragments :: <list>) => (maybe-name :: false-or(<name-fragment>))
  let name = first(fragments, default: #f);
  if (name & instance?(name, <name-fragment>))
    name
  else
    #f
  end;
end function;

define inline method initialize (f :: <definition-fragment>, #key)
  next-method();
  fragment-macro(f)
    := suffix-name-hygienically(fragment-define-word(f), "-definer");
end method;

define method fragment-kind (f :: <definition-fragment>) => kind;
  $parsed-macro-call-token;
end method;

define class <body-definition-fragment> (<definition-fragment>)
  constant slot fragment-body-fragment,
    required-init-keyword: body-fragment:;
  constant slot fragment-end-word = #f,
    init-keyword: end-word:;
end class;

/*
define method initialize (f :: <list-definition-fragment>, #key)
  next-method();
  // If it's all potential modifiers in the body until a definition,
  // then warn.
  block (return)
    for (fragment in fragment-list-fragment(f))
      if (~instance?(fragment, <variable-name-fragment>))
        return();
      end;
      select (fragment-kind(fragment))
        $define-body-word-only-token,
        $define-list-word-only-token,
        $begin-and-define-body-word-token,
        $begin-and-define-list-word-token,
        $function-and-define-body-word-token,
        $function-and-define-list-word-token
          => signal("Ambiguous stuff - %s vs %s",
                    fragment-define-word(f), fragment);
        otherwise
          => #t;
      end;
    end;
  end;
end method;
*/

define class <list-definition-fragment> (<definition-fragment>)
  constant slot fragment-list-fragment,
    required-init-keyword: list-fragment:;
end class;

// For reference macros.
define method fragment-macro
    (f :: <variable-name-fragment>) => (f :: <variable-name-fragment>)
  f
end method;

//// Extra surface syntax for the macro system.

define class <macro-body-definition-fragment> (<definition-fragment>)
  constant slot fragment-macro-body-fragment,
    required-init-keyword: macro-body-fragment:;
  constant slot fragment-end-word = #f,
    init-keyword: end-word:;
end class;

define abstract class <compound-macro-fragment> (<compound-fragment>)
end class;

define method fragment-kind (f :: <compound-macro-fragment>) => kind;
  $escaped-substitution-token
end method;

define class <pattern-variable-fragment> (<compound-macro-fragment>)
  constant slot fragment-name :: type-union
                          (<name-fragment>, <constrained-name-fragment>),
    required-init-keyword: name:;
  constant slot fragment-constraint :: false-or(<symbol>),
    required-init-keyword: constraint:;
end class;

define class <sequence-pattern-variable-fragment> (<compound-macro-fragment>)
  constant slot fragment-name :: type-union
                          (<name-fragment>, <constrained-name-fragment>),
    required-init-keyword: name:;
//  slot fragment-constraint :: false-or(<symbol>),
//    required-init-keyword: constraint:;
  constant slot fragment-separator :: false-or(<separator-fragment>),
    required-init-keyword: separator:;
end class;

define class <unhygienic-name-fragment> (<compound-macro-fragment>)
  constant slot fragment-name :: <name-fragment>,
    required-init-keyword: name:;
end class;

define class <spliced-pattern-variable-fragment> (<compound-macro-fragment>)
  constant slot fragment-prefix,
    required-init-keyword: prefix:;
  constant slot fragment-pattern-variable,
    required-init-keyword: pattern-variable:;
  constant slot fragment-suffix,
    required-init-keyword: suffix:;
end class;

/*
define class <pattern-expression-fragment> (<compound-macro-fragment>)
  constant slot fragment-expression :: <fragment>,
    required-init-keyword: name:;
end class;

define class <sequence-pattern-expression-fragment>
    (<compound-macro-fragment>)
  slot fragment-expression :: <fragment>,
    required-init-keyword: name:;
  constant slot fragment-separator :: false-or(<separator-fragment>),
    required-init-keyword: separator:;
end class;
*/

define abstract class <template-call-fragment> (<compound-macro-fragment>)
  constant slot fragment-template,
    required-init-keyword: template:;
end class;

define class <template-aux-rule-call-fragment> (<template-call-fragment>)
  constant slot fragment-rule-name,
    required-init-keyword: rule-name:;
end class;

define class <template-macro-call-fragment> (<template-call-fragment>)
end class;

//// Macro template objects.

define class <template> (<object>)
  constant slot template-fragments,
    required-init-keyword: fragments:;
end class;

define method nested-fragment?
    (f :: <template>) => (well? :: <boolean>, left, right)
  values(#t, #f, #f);
end method;

// TODO: Remove. This for backward compatibility only.
define constant <template-closure> = <template>;

define sealed domain make (subclass(<template>));
define sealed domain initialize (<template>);

// Stubs.

define compiler-open generic classify-word-in (context, word);

//// Dynamic source location contexts.

define thread variable *parent-source-location* = #f;

define inline function parent-source-location () *parent-source-location* end;

// HACK: THIS MUST BE INLINED SO ENSURE BY FUZZING OUT SPECIALIZER

define inline function do-with-parent-source-location
    (f :: <function>, location /* :: false-or(<source-location>) */ )
  dynamic-bind (*parent-source-location* = location)
    f()
  end;
end function;

define macro with-parent-source-location
  { with-parent-source-location (?location:expression) ?:body end }
    => { do-with-parent-source-location(method () ?body end, ?location) }
end macro;

define inline function do-with-parent-fragment
    (f :: <function>, fragment :: <fragment>)
  dynamic-bind (*parent-source-location* = fragment-source-location(fragment))
    f()
  end;
end function;

define macro with-parent-fragment
  { with-parent-fragment (?f:expression) ?:body end }
    => { do-with-parent-fragment(method () ?body end, ?f) }
end macro;

//// Unused token classes

ignore($token-token);
ignore($hash-t-token);
ignore($hash-f-token);
