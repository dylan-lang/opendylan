Module:    DFMC-Typist
Author:    Steve Rowley
Synopsis:  Hashing of <type-estimate>s.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic type-estimate-hash (thing, hash-state) 
  // A hash function on <type-estimate>s with equality type-estimate-match?.
  // Hashes anything else with object-hash.
  => (hash-id :: <integer>, hash-state :: <object>);

define macro type-estimate-hash-rules-definer
  // Expand a bunch of rules into methods for type-estimate-hash.
  { define type-estimate-hash-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates a type-estimate-hash method.
  { ?tname:name :: ?typ:*, ?hash-state:name <- ?expr:* }
  => { define method type-estimate-hash(?tname :: ?typ, ?hash-state)
        => (?=hash-id :: <integer>, ?=hash-state :: <object>);
         ?expr
       end }
end;

// Don't care if these move; we're just gonna canonicalize all these.
define variable *top-hash*          = #f;
define variable *bottom-hash*       = #f;
//define constant $empty-list-hash$   :: <integer>           = object-hash(#());
//define constant $empty-vector-hash$ :: <integer>           = object-hash(#[]);

define type-estimate-hash-rules
  o :: <object>, hash-state   <- object-hash(o, hash-state);
  p :: <pair>, hash-state     <- type-estimate-hash-reduce(#t, hash-state, head(p), tail(p));
  l :: <empty-list>, hash-state  <- object-hash(#(), hash-state);
  k :: <symbol>, hash-state      <- type-estimate-hash(as(<string>, k), hash-state);
  s :: <sequence>, hash-state    <- case
                                   empty?(s) => object-hash(#[], hash-state);
                                   otherwise => apply(type-estimate-hash-reduce,
                                                      #t, hash-state, s);
                                 end;
  o :: <serial-numbered-object>, hash-state
    <- values(object-serial-number(o), hash-state);
  t :: <type-estimate-top>, hash-state    
    <- begin 
         unless (*top-hash*)
           // Not yet computed, so exploit opportunity.
           *top-hash* := object-hash(t, hash-state)
         end;
         values(*top-hash*, hash-state)
       end;
  b :: <type-estimate-bottom>, hash-state
    <- begin 
         unless (*bottom-hash*)
           // Not yet computed, so exploit opportunity.
           *bottom-hash* := object-hash(b, hash-state)
         end;
         values(*bottom-hash*, hash-state)
       end;
  r :: <type-estimate-raw>, hash-state
    <- type-estimate-hash(type-estimate-raw(r), hash-state);
  c :: <type-estimate-class>, hash-state
    <- type-estimate-hash(type-estimate-class(c), hash-state);
  // The rest are recursive rules, which reduce over their component parts.
  mv :: <type-estimate-values>, hash-state
    <- apply(type-estimate-hash-reduce,
             #t, hash-state,
             type-estimate-rest-values(mv),
             type-estimate-fixed-values(mv));
  li :: <type-estimate-limited-integer>, hash-state
    <- type-estimate-hash-reduce(#t, hash-state, 
                                 type-estimate-class(li),
                                 type-estimate-min(li),
                                 type-estimate-max(li));
  subcl :: <type-estimate-limited-class>, hash-state 
    <- type-estimate-hash-reduce(#t, hash-state, 
                                 type-estimate-class(subcl),
                                 type-estimate-subclass(subcl));
  sing :: <type-estimate-limited-instance>, hash-state 
    <- type-estimate-hash-reduce(#t, hash-state,
                                 type-estimate-class(sing),
                                 type-estimate-singleton(sing));
  coll :: <type-estimate-limited-collection>, hash-state
    <- type-estimate-hash-reduce(#t, hash-state, 
                                 type-estimate-class(coll), 
                                 type-estimate-of(coll),
                                 type-estimate-size(coll),
                                 type-estimate-dimensions(coll));
  un :: <type-estimate-union>, hash-state // NB: NOT ordered!
    <- apply(type-estimate-hash-reduce, #f, hash-state, type-estimate-unionees(un));
  fn :: <type-estimate-limited-function>, hash-state
    <- type-estimate-hash(type-estimate-class(fn), hash-state);
end;

// Actually the te here is sometimes a bare <&class> and god knows what
// else.

define inline function type-estimate-hash-reduce 
  (ordered?, hash-state, te :: <object>, #rest te*)
    => (hash-id :: <integer>, hash-state :: <object>)
  // Like reduce w/type-estimate-hash & merge-hash-codes, but cope w/mult values
  // Assume destructive update of hash-state
  let hash-id = type-estimate-hash(te, hash-state); // Hash 1st guy
  for (next-te in te*)
    let next-id = type-estimate-hash(next-te, hash-state); // Hash next guy
    let merge-id =                            // Merge next guy
      merge-hash-ids(hash-id, next-id, ordered: ordered?);
    hash-id    := merge-id;                                  // Write back
  finally values(hash-id, hash-state)                        // Return net hash
  end
end;

define inline function type-estimate-hash-reduce-sequence1
  (ordered? :: <boolean>, hash-state, te* :: <simple-object-vector>)
    => (hash-id :: <integer>, hash-state :: <object>)
  // Like reduce w/type-estimate-hash & merge-hash-codes, but cope w/mult values
  // Assume destructive update of hash-state
  let te = te*[0];
  let hash-id = type-estimate-hash(te, hash-state); // Hash 1st guy
  for (next-te in te*)
    let next-id = type-estimate-hash(next-te, hash-state); // Hash next guy
    let merge-id =                            // Merge next guy
      merge-hash-ids(hash-id, next-id, ordered: ordered?);
    hash-id    := merge-id;                                  // Write back
  finally values(hash-id, hash-state)                        // Return net hash
  end
end;

define class <type-estimate-match-table> (<table>)
  // <table> w/equality pred type-estimate-match? & hash fn type-estimate-hash.
end;

define sealed domain make (subclass(<type-estimate-match-table>));
define sealed domain initialize (<type-estimate-match-table>);

define method table-protocol (temt :: <type-estimate-match-table>) 
 => (test :: <function>, hash :: <function>)
  values(type-estimate-match?, type-estimate-hash)
end;

define class <type-estimate-pair-match-table> (<table>)
  // <table> w/equality pred type-estimate-match? on head & tail of <pair>,
  // and hash fn that hashes head & tail & merges results.
end;

define sealed domain make (subclass(<type-estimate-pair-match-table>));
define sealed domain initialize (<type-estimate-pair-match-table>);

define method table-protocol (tepmt :: <type-estimate-pair-match-table>)
 => (test :: <function>, hash :: <function>)
  values(method (x :: <pair>, y :: <pair>) => (pair-match? :: <boolean>)
           // Match head & tail
           type-estimate-match?(head(x), head(y)) & 
           type-estimate-match?(tail(x), tail(y))
         end,
         method (x :: <pair>, hash-state :: <object>) => (hash-id :: <integer>, hash-state :: <object>)
           type-estimate-hash-reduce(#t, hash-state, head(x), tail(x))
         end)
end;

define class <type-estimate-sequence-match-table> (<table>)
  // <table> w/equality pred type-estimate-match? on sequences of types
  // for dispatching.
end;

define sealed domain make (subclass(<type-estimate-sequence-match-table>));
define sealed domain initialize (<type-estimate-sequence-match-table>);

define method table-protocol (tepmt :: <type-estimate-sequence-match-table>)
 => (test :: <function>, hash :: <function>)
  values(method (x :: <simple-object-vector>, y :: <simple-object-vector>)
             => (match? :: <boolean>)
           block (return)
             for (x-type in x, y-type in y)
               if (~type-estimate-match?(x-type, y-type))
                 return(#f);
               end;
             finally
               #t
             end;
           end;
         end,
         method (x :: <simple-object-vector>, hash-state :: <object>)
              => (hash-id :: <integer>, hash-state :: <object>)
           if (empty?(x))
             values(0, hash-state);
           else
             // Assumes at least one type.
             type-estimate-hash-reduce-sequence1(#t, hash-state, x);
           end;
         end)
end;

// eof
