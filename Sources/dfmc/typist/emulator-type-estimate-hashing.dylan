Module:    DFMC-Typist
Author:    Steve Rowley
Synopsis:  Hashing of <type-estimate>s.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro type-estimate-hash-rules-definer
  // Expand a bunch of rules into methods for type-estimate-hash.
  { define type-estimate-hash-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates a type-estimate-hash method.
  { ?tname:name :: ?typ:* <- ?expr:* }
  => { define method type-estimate-hash(?tname :: ?typ)
        => (hash-id :: <integer>, hash-state :: <object>);
         values(?expr, $permanent-hash-state)
       end }
end;

// Don't care if these move; we're just gonna canonicalize all these.
define variable *top-hash*          :: false-or(<integer>) = #f;
define variable *bottom-hash*       :: false-or(<integer>) = #f;
define constant $empty-list-hash$   :: <integer>           = object-hash(#());
define constant $empty-vector-hash$ :: <integer>           = object-hash(#[]);

define type-estimate-hash-rules
  // NB: #f is the permanent-hash-state, used for gc-invariant hashes.
  // Object-hash is gc-invariant for integers, booleans, characters, floats.
  //   Other stuff will cause rehashes on gc-flips.
  o :: <object>               <- object-hash(o);
  p :: <pair>                 <- type-estimate-hash-reduce(#t, head(p), tail(p));
  l :: <empty-list>           <- values($empty-list-hash$, #f);
  k :: <symbol>               <- type-estimate-hash(as(<string>, k));
  s :: <sequence>             <- case
                                   empty?(s) => $empty-vector-hash$;
                                   otherwise => apply(type-estimate-hash-reduce,
                                                      #t, s);
                                 end;
  o :: <serial-numbered-object>
    <- values(object-serial-number(o), #f);
  t :: <type-estimate-top>    
    <- begin 
         unless (*top-hash*)
           // Not yet computed, so exploit opportunity.
           *top-hash* := object-hash(t)
         end;
         values(*top-hash*, #f)
       end;
  b :: <type-estimate-bottom>
    <- begin 
         unless (*bottom-hash*)
           // Not yet computed, so exploit opportunity.
           *bottom-hash* := object-hash(b)
         end;
         values(*bottom-hash*)
       end;
  r :: <type-estimate-raw>    <- type-estimate-hash(type-estimate-raw(r));
  c :: <type-estimate-class>  <- type-estimate-hash(type-estimate-class(c));
  // The rest are recursive rules, which reduce over their component parts.
  mv :: <type-estimate-values>
    <- apply(type-estimate-hash-reduce,
             #t,
             type-estimate-rest-values(mv),
             type-estimate-fixed-values(mv));
  li :: <type-estimate-limited-integer>
    <- type-estimate-hash-reduce(#t,
                                 type-estimate-class(li),
                                 type-estimate-min(li),
                                 type-estimate-max(li));
  subcl :: <type-estimate-limited-class> 
    <- type-estimate-hash-reduce(#t,
                                 type-estimate-class(subcl),
                                 type-estimate-subclass(subcl));
  sing :: <type-estimate-limited-instance> 
    <- type-estimate-hash-reduce(#t,
                                 type-estimate-class(sing),
                                 type-estimate-singleton(sing));
  coll :: <type-estimate-limited-collection>
    <- type-estimate-hash-reduce(#t,
                                 type-estimate-class(coll), 
                                 type-estimate-of(coll),
                                 type-estimate-size(coll),
                                 type-estimate-dimensions(coll));
  un :: <type-estimate-union> // NB: NOT ordered!
    <- apply(type-estimate-hash-reduce, #f, type-estimate-unionees(un));
  fn :: <type-estimate-limited-function>
    <- type-estimate-hash(type-estimate-class(fn)); // Punt the complexity below
       /* begin
         // 2 sequences in these things, so need 2 reductions + merge.
         // First is everything but keys.
         let (id1, state1) = apply(type-estimate-hash-reduce,
                                   #t,
                                   type-estimate-class(fn),
                                   type-estimate-rest?(fn),
                                   type-estimate-all-keys?(fn),
                                   type-estimate-values(fn),
                                   type-estimate-requireds(fn));
         // Second is the keys, if present.
         let (id2, state2) = if (type-estimate-keys(fn))
                               // Loop over the table, hashing key/type pairs,
                               // (in an order-INsensitive way).  (<table> is
                               // not a <sequence>, so type-estimate-hash-reduce
                               // isn't the answer here.)
                               let keys-id    = #f; // Accumulators of the 
                               let keys-state = #f; //   net key hash code.
                               let first?     = #t;
                               // *** Emulator's for doesn't support keyed-by?
                               map-table(type-estimate-keys(fn),
                                         method (the-key, the-type)
                                           // Compute ordered hash of key/type
                                           let (this-id, this-state) = 
                                             type-estimate-hash-reduce(#t,
                                                                       the-key,
                                                                       the-type);
                                           if (first?)
                                             // First iteration: set accs
                                             keys-id    := this-id;
                                             keys-state := this-state;
                                             first?     := #f
                                           else
                                             // Later iteration: merge accs, 
                                             //  but not ordered among keys.
                                             let (merge-id, merge-state) = 
                                               merge-hash-codes(keys-id, 
                                                                keys-state, 
                                                                this-id, 
                                                                this-state,
                                                                ordered: #f);
                                             keys-id    := merge-id;
                                             keys-state := merge-state
                                           end
                                         end);
                               values(keys-id, keys-state)
                             else
                               // No keys present, hash some empty sequence.
                               type-estimate-hash(#())
                             end;
         // Result is the merge of keys and everything else.
         merge-hash-codes(id1, state1, id2, state2, ordered: #t)
       end; */
end;


define generic type-estimate-hash (thing) 
  // A hash function on <type-estimate>s with equality type-estimate-match?.
  // Hashes anything else with object-hash.
  => (hash-id :: <integer>, hash-state :: <object>);

define inline function type-estimate-hash-reduce 
  (ordered?, te :: <type-estimate>, #rest te*)
    => (hash-id :: <integer>, hash-state :: <object>)
  // Like reduce w/type-estimate-hash & merge-hash-codes, but cope w/mult values
  let (hash-id, hash-state) = type-estimate-hash(te);        // Hash 1st guy
  for (next-te in te*)
    let (next-id, next-state) = type-estimate-hash(next-te); // Hash next guy
    let (merge-id, merge-state) =                            // Merge next guy
      merge-hash-codes(hash-id, hash-state, next-id, next-state,
                       ordered: ordered?);
    hash-id    := merge-id;                                  // Write back
    hash-state := merge-state                                // (mult-val-setq)
  finally values(hash-id, hash-state)                        // Return net hash
  end
end;

define class <type-estimate-match-table> (<table>)
  // <table> w/equality pred type-estimate-match? & hash fn type-estimate-hash.
end;

define method table-protocol (temt :: <type-estimate-match-table>) 
 => (test :: <function>, hash :: <function>)
  values(type-estimate-match?, type-estimate-hash)
end;

define class <type-estimate-pair-match-table> (<table>)
  // <table> w/equality pred type-estimate-match? on head & tail of <pair>,
  // and hash fn that hashes head & tail & merges results.
end;

define method table-protocol (tepmt :: <type-estimate-pair-match-table>)
 => (test :: <function>, hash :: <function>)
  values(method (x :: <pair>, y :: <pair>) => (pair-match? :: <boolean>)
           // Match head & tail
           type-estimate-match?(head(x), head(y)) & 
           type-estimate-match?(tail(x), tail(y))
         end,
         method (x :: <pair>) => (hash-id :: <integer>, hash-state :: <object>)
           type-estimate-hash-reduce(#t, head(x), tail(x))
         end)
end;

