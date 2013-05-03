language: infix-dylan
module: dispatch-engine-internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *engine-node-classes* :: <simple-object-vector>
  = system-allocate-repeated-object-instance
      (<simple-object-vector>, #f, ash(1, properties$s-entry-type), #f);


// **** Every constant defined below is duplicated in both the compiler (dfmc/modeling)
// **** and in harp (core-harp/function-offsets).


define constant engine-node$k-absent = 0;

define constant engine-node$k-inapplicable = 1;

define constant engine-node$k-unkeyed-single-method = 2;

define constant engine-node$k-implicit-keyed-single-method = 3;

define constant engine-node$k-explicit-keyed-single-method = 4;

define constant engine-node$k-unrestricted-keyed-single-method = 5;

define constant engine-node$k-reserved-terminal-n-a = 6;

define constant engine-node$k-reserved-terminal-n-b = 7;

define constant engine-node$k-reserved-terminal-n-c = 8;

define constant engine-node$k-reserved-terminal-n-d = 9;

define constant engine-node$k-reserved-terminal-n-e = 10;

define constant engine-node$k-reserved-terminal-n-f = 11;

define constant engine-node$k-reserved-terminal-n-g = 12;

define constant engine-node$k-profiling-cache-header = 13;

define constant engine-node$k-cache-header = 14;

define constant engine-node$k-ambiguous-methods = 15;


/* ************************************************************************
                      Slot Accessor Assignments
   ************************************************************************

Slot accessors codes are allocated in a block, in getter/setter pairs;
a setter code has the low bit set, a getter clear.  We can use the code,
adjusted by subtracting the lowest one, to index into a table to manage
engine-node-specific data.

*/


define constant engine-node$k-first-slot-engine-node = 16;

define constant engine-node$k-boxed-instance-slot-getter = 16;
define constant engine-node$k-boxed-instance-slot-setter = 17;

define constant engine-node$k-boxed-repeated-instance-slot-getter = 18;
define constant engine-node$k-boxed-repeated-instance-slot-setter = 19;

define constant engine-node$k-boxed-class-slot-getter = 20;
define constant engine-node$k-boxed-class-slot-setter = 21;

define constant engine-node$k-raw-byte-repeated-instance-slot-getter = 22;
define constant engine-node$k-raw-byte-repeated-instance-slot-setter = 23;

define constant engine-node$k-reserved-slot-a-getter = 24;
define constant engine-node$k-reserved-slot-a-setter = 25;

define constant engine-node$k-reserved-slot-b-getter = 26;
define constant engine-node$k-reserved-slot-b-setter = 27;

define constant engine-node$k-reserved-repeated-slot-a-getter = 28;
define constant engine-node$k-reserved-repeated-slot-a-setter = 29;

define constant engine-node$k-reserved-repeated-slot-b-getter = 30;
define constant engine-node$k-reserved-repeated-slot-b-setter = 31;


define constant engine-node$k-slot-engine-node-count = 16;



define constant engine-node$k-typecheck = 32;

define constant engine-node$k-if-type = 33;

define constant engine-node$k-linear-by-class = 34;

define constant engine-node$k-hashed-by-class = 35;

define constant engine-node$k-linear-by-singleton-class = 36;

define constant engine-node$k-hashed-by-singleton-class = 37;

define constant engine-node$k-immediate-linear-singleton = 38;

define constant engine-node$k-immediate-hashed-noreloc-singleton = 39;

define constant engine-node$k-immediate-hashed-singleton = 40;

define constant engine-node$k-value-object-linear-singleton = 41;

define constant engine-node$k-monomorphic-by-class = 42;

define constant engine-node$k-reserved-discriminator-a = 43;

define constant engine-node$k-reserved-discriminator-b = 44;

define constant engine-node$k-reserved-discriminator-c = 45;

define constant engine-node$k-reserved-discriminator-d = 46;

define constant engine-node$k-reserved-discriminator-e = 47;

define constant engine-node$k-reserved-discriminator-f = 48;

define constant engine-node$k-reserved-discriminator-g = 49;

define constant engine-node$k-reserved-discriminator-h = 50;

define constant engine-node$k-reserved-discriminator-i = 51;

define constant engine-node$k-reserved-discriminator-j = 52;

define constant engine-node$k-reserved-discriminator-k = 53;

define constant engine-node$k-reserved-discriminator-l = 54;

define constant engine-node$k-reserved-discriminator-m = 55;

define constant engine-node$k-reserved-discriminator-n = 56;

define constant engine-node$k-reserved-discriminator-o = 57;

define constant engine-node$k-reserved-discriminator-p = 58;

define constant engine-node$k-reserved-discriminator-q = 59;

define constant engine-node$k-reserved-discriminator-r = 60;

define constant engine-node$k-reserved-discriminator-s = 61;

define constant engine-node$k-reserved-discriminator-t = 62;

define constant engine-node$k-reserved-discriminator-u = 63;


/*
This little bit of cruft is factored out because it's not clear what
extension to make to keep it general and generic-operation-free.  The
basic problem is to keep a record of which arguments we have discriminated
on while setting up the discrimination program.  We also need to be able
to generate ones remaining to be done.

What comes to mind is a bit mask, but the range limitations of that would
be a problem (not much, considering, but well smaller than what we would be
supporting as maximum number of specialized arguments).  So this will do
for now, and it does have the benefit that its waste is proportional to
the number of specialized arguments.
*/

define constant <argnum-set> = <pair>;

define function make-argnum-set () => (argnum-set :: <pair>)
  pair(0, #())
end function;

// define inline function argnum-set-empty? (argnum-set :: <pair>) => (well? :: <boolean>)
//   tail(argnum-set) == #()
// end function;


define function argnum-considered? (argnum :: <integer>, argnum-set :: <pair>) => (B :: <boolean>)
  let args :: <list> = tail(argnum-set);
  local method loop (l :: <list>)
          if (l == #())
            #f
          else
            let oargnum :: <integer> = head(l);
            if (oargnum == argnum)
              #t
            elseif (oargnum > argnum)
              #f
            else
              let l :: <list> = tail(l);
              loop(l)
            end if
          end if
        end method;
  loop(args)
end function;


define function add-argnum
    (argnum :: <integer>, argnum-set :: <pair>) => (argnum-set :: <pair>)
  local method loop (prev :: <pair>, l :: <list>)
          if (l == #())
            tail(prev) := pair(argnum, #());
            let oldcount :: <integer> = head(argnum-set);
            head(argnum-set) := oldcount + 1
          else
            let oargnum :: <integer> = head(l);
            unless (oargnum == argnum)
              if (argnum < oargnum)
                tail(prev) := pair(argnum, tail(prev));
                let oldcount :: <integer> = head(argnum-set);
                head(argnum-set) := oldcount + 1
              else
                let nxt :: <list> = tail(l);
                loop(l, nxt)
              end if
            end unless
          end if
        end method;
  let firstone :: <list> = tail(argnum-set);
  loop(argnum-set, firstone);
  argnum-set
end function;

//define function argnum-set-full?
//    (argnum-set :: <pair>, n-elements :: <integer>) => (B :: <boolean>)
//  n-elements == head(argnum-set)
//end function;

// Returns the next free argument number from the set, after previous, which should be -1 if there
// is no previous one.  You can iterate over the free argument numbers by calling next-free-argnum
// with the previous result.  Don't go off the end.
define function next-free-argnum
    (previous :: <integer>, argnum-set :: <pair>) => (argnum :: <integer>)
  local method loop1 (l :: <list>)
          if (l == #())
            loop2(previous, l)
          else
            let n :: <integer> = head(l);
            if (previous < n)
              loop2(previous, l)
            else
              let nextl :: <list> = tail(l);
              loop1(nextl)
            end if
          end if
        end method,
        method loop2 (prev :: <integer>, l :: <list>)
          let next :: <integer> = prev + 1;
          if (l == #() | head(l) ~== next)
            next
          else
            let nextl :: <list> = tail(l);
            loop2(next, nextl)
          end if
        end method;
  let argnums :: <list> = tail(argnum-set);
  loop1(argnums)
end function;


// Is the argnum-set a (possibly improper) subset of the set represented by the integer?
// I.e., are there no args in the argnum-set that aren't in the integer set?
// Currently not used.
//define function argnum-set-subset? (argnum-set :: <pair>, set :: <integer>) => (v :: <boolean>)
//  let cnt :: <integer> = head(argnum-set);
//  local method loop (mtest :: <integer>, i :: <integer>, cnt :: <integer>)
//          if (cnt == 0)
//            #t
//          elseif (mtest == 0)
//            #f
//          elseif (argnum-considered?(i, argnum-set))
//            if (logbit?(0, mtest))
//              loop(ash(mtest, -1), i + 1, cnt - 1)
//            else
//              #f
//            end if
//          else
//            loop(ash(mtest, -1), i + 1, cnt)
//          end if
//        end method;
//  loop(set, 0, cnt)
//end function;


// Currently not used, save...
//define function argnum-set-to-mask (argnum-set :: <pair>) => (mask :: <integer>)
//  local method loop (l :: <list>, m :: <integer>)
//          if (l == #())
//            m
//          else
//            let i :: <integer> = head(l);
//            let l :: <list> = tail(l);
//            loop(l, logior(m, ash(1, i)))
//          end if
//        end method;
//  let bitlist :: <list> = tail(argnum-set);
//  loop(bitlist, 0)
//end function;



//define function collect-methods-to-consider
//    (methods :: <list>, args :: <simple-object-vector>, argnum-set :: <argnum-set>)
//  let headed-methods :: <pair> = pair(#f, #());
//  local method collect-methods (last-pair :: <pair>, methods :: <list>)
//          unless (methods == #())
//            let meth :: <method> = head(methods);
//            local method loop (l :: <list>)
//                    if (l == #())
//                      collect-methods(tail(last-pair) := pair(meth, #()), tail(methods))
//                    else
//                      let argnum :: <integer> = head(l);
//                      if (grounded-instance?(vector-element(args, argnum),
//                                             %method-specializer(meth, argnum)))
//                        loop(tail(l))
//                      else
//                        collect-methods(last-pair, tail(methods))
//                      end if
//                    end if
//                  end method;
//            loop(tail(argnum-set))
//          end unless
//        end method;
//  collect-methods(headed-methods, methods);
//  headed-methods
//end function;


define inline function dbg (str, #rest args) => ();
  // apply(format-out, str, args);
  // format-out("\n")
end function;



define constant %make-simple-vector
    = method (n :: <integer>, v) => (vec :: <simple-object-vector>);
        make(<simple-object-vector>, size: n, fill: v)
      end method;


// define inline-only constant $integer-tag-value = as(<machine-word>, 1);
// define inline-only constant $integer-tag-width = 2;
// define inline-only constant $integer-tag-mask  = as(<machine-word>, ash(-1, $integer-tag-width));
// define inline-only constant $machine-word-integer-one = as(<machine-word>, 5);
// define inline-only constant $machine-word-integer-one-value = as(<machine-word>, 4);


define inline function %load-byte (p :: <integer>, s :: <integer>, n :: <integer>)
 => (b :: <integer>);
  let ps :: <integer> = - p;
  let n2 :: <integer> = ash(n, ps);
  let p :: <integer> = ash(1, s);
  let m :: <integer> = p - 1;
  logand(n2, m)
end function;

//define function %load-byte1 (p :: <integer>, s :: <integer>, n :: <integer>)
// => (b :: <integer>);
//  let shift = primitive-unwrap-machine-word(coerce-integer-to-machine-word(p));
//  primitive-cast-integer-as-raw
//    (primitive-machine-word-logior
//       (primitive-machine-word-logand
//          (primitive-machine-word-shift-right
//             (primitive-cast-integer-as-raw(n),
//              primitive-unwrap-machine-word(coerce-integer-to-machine-word(p))),
//           primitive-machine-word-subtract
//             (primitive-machine-word-shift-left-low
//                (primitive-unwrap-machine-word($machine-word-integer-one-value),
//                 primitive-unwrap-machine-word(coerce-integer-to-machine-word(s))),
//              primitive-unwrap-machine-word($machine-word-integer-one-value))),
//        primitive-unwrap-machine-word($integer-tag-value)))
//end function;


// -- unused after all.
//define inline function %maskify (n :: <integer>) => (m :: <integer>);
//  primitive-cast-raw-as-integer
//    (primitive-machine-word-logior
//       (primitive-machine-word-subtract
//          (primitive-machine-word-shift-left-low
//             (primitive-unwrap-machine-word($machine-word-integer-one-value),
//              primitive-unwrap-machine-word(coerce-integer-to-machine-word(n))),
//           primitive-unwrap-machine-word($machine-word-integer-one-value)),
//        primitive-unwrap-machine-word($integer-tag-value)))
////  ash(1, n) - 1
//end function;

define inline function %twopower (n :: <integer>) => (m :: <integer>);
//  primitive-cast-raw-as-integer
//    (primitive-machine-word-logior
//       (primitive-machine-word-shift-left-low
//          (primitive-unwrap-machine-word($machine-word-integer-one-value),
//           primitive-unwrap-machine-word(coerce-integer-to-machine-word(n))),
//        primitive-unwrap-machine-word($integer-tag-value)))
  ash(1, n)
end function;


define inline function %scale-down (n :: <integer>, s :: <integer>) => (m :: <integer>);
//  primitive-cast-raw-as-integer
//    (primitive-machine-word-logior
//       (primitive-machine-word-logand
//          (primitive-machine-word-shift-right
//             (primitive-cast-integer-as-raw(n),
//              primitive-unwrap-machine-word(coerce-integer-to-machine-word(s))),
//           primitive-unwrap-machine-word($integer-tag-mask)),
//        primitive-unwrap-machine-word($integer-tag-value)))
  ash(n, - s)
end function;


define function %method-specializer (m :: <method>, i :: <integer>) => (spec :: <type>);
  select (m by instance?)
    <accessor-method> =>
      let m :: <accessor-method> = m;
      let sd :: <slot-descriptor> = method-slot-descriptor(m);
      if (instance?(m, <single-accessor-method>))
        if (instance?(m, <getter-accessor-method>))
          select (i) 0 => slot-owner(sd); end
        else
          select (i) 0 => slot-type(sd); 1 => slot-owner(sd); end
        end if
      else
        if (instance?(m, <getter-accessor-method>))
          select (i) 0 => slot-owner(sd); 1 => <integer>; end
        else
          select (i) 0 => slot-type(sd); 1 => slot-owner(sd); 2 => <integer>; end
        end if
      end if;
    otherwise =>
      let m :: <lambda> = m;
      let sig :: <signature> = function-signature(m);
      let v :: <simple-object-vector> = signature-required(sig);
      vector-element(v, i)        // v[i]
  end select
end function;


define function %method-number-required (m :: <method>) => (nreq :: <integer>)
  select (m by instance?)
    <accessor-method> =>
      if (instance?(m, <repeated-accessor-method>))
        if (instance?(m, <setter-accessor-method>)) 3 else 2 end;
      else
        if (instance?(m, <setter-accessor-method>)) 2 else 1 end;
      end if;
    otherwise =>
      let m :: <lambda> = m;
      signature-number-required(function-signature(m))
  end select
end function;


define inline function %gf-number-required (g :: <generic-function>) => (nreq :: <integer>)
  signature-number-required(function-signature(g))
end function;


//// UNIQUE CLASS KEYS

define constant $dispatch-key-lock
  = make-simple-lock();

define variable *next-unique-dispatch-key* :: <integer> = 0;

// @@@@@ This should be a weak something-or-other.
define variable *implementation-classes-by-key* :: <simple-object-vector>
  = #[];

/// JB START

/// UNIQUE-KEY AS WRAPPER

/// TODO: JB TAG REP -- SHOULD GO TO CONVERSION FILES

define constant $xxx-integer-tag-value = as(<machine-word>, 1);

define inline function force-integer-to-address-tag
    (x :: <machine-word>) => (res :: <machine-word>)
  machine-word-logxor(x, $xxx-integer-tag-value)
  // machine-word-logand(x, as(<machine-word>, lognot($address-tag-mask)))
end function;

/// HACK: DUP FROM CONVERSION-TAGGED-INTEGER

define inline-only function xxx-interpret-integer-as-machine-word
    (x :: <integer>) => (result :: <machine-word>)
  primitive-wrap-machine-word(primitive-cast-integer-as-raw(x))
end function;

define inline function integer-as-address (n :: <integer>) => (res :: <machine-word>)
  force-integer-to-address-tag(xxx-interpret-integer-as-machine-word(n))
end function;

define inline function pointer-as-integer (x) => (res :: <integer>)
  primitive-cast-raw-as-integer
    (primitive-unwrap-machine-word
       (force-integer-tag(primitive-wrap-machine-word(primitive-cast-pointer-as-raw(x)))))
end function;

define inline function interpret-mm-wrapper-as-integer (w) => (key :: <integer>);
  pointer-as-integer(w)
end function;

define inline function iclass-unique-key (ic :: <implementation-class>) => (key :: <integer>);
  interpret-mm-wrapper-as-integer(class-mm-wrapper(ic));
end function;

define inline function class-unique-key (c :: <class>) => (key :: <integer>);
  iclass-unique-key(class-implementation-class(c))
end function;


define inline function object-class-unique-key (x) => (key :: <integer>);
  interpret-mm-wrapper-as-integer(object-mm-wrapper(x))
end function;

define function implementation-class-from-key (n :: <integer>)
 => (v :: <implementation-class>)
  let mm-wrapper = primitive-cast-raw-as-pointer
                     (primitive-unwrap-machine-word(integer-as-address(n)));
  %mm-wrapper-implementation-class(mm-wrapper);
end function;

/// JB END

/// UNIQUE-KEY FROM ICLASS


/*
define inline function iclass-unique-key (ic :: <implementation-class>) => (key :: <integer>);
  iclass-dispatch-key(ic)
end function;

define inline function class-unique-key (c :: <class>) => (key :: <integer>);
  iclass-unique-key(class-implementation-class(c))
end function;

define inline function object-class-unique-key (x)
 => (key :: <integer>);
  iclass-unique-key(object-implementation-class(x))
end function;

// ---- Unsure whether this should be more error resistant or what.  See
// how it really gets used outside of debugging.
define function implementation-class-from-key (n :: <integer>)
 => (v :: false-or(<implementation-class>))
  let classes :: <simple-object-vector> = *implementation-classes-by-key*;
  vector-element(classes, iclass-key-to-number(n))
end function;
*/


define inline function iclass-number-to-key (n :: <integer>) => (k :: <integer>)
  let scaled :: <integer> = ash(n, 1);
  scaled + 1000
end function;

define inline function iclass-key-to-number (k :: <integer>) => (n :: <integer>)
  let scaled :: <integer> = k - 1000;
  ash(scaled, -1)
end function;


define function ensure-key-to-iclass-storage (ninc :: <integer>) => (v :: <simple-object-vector>)
  let next-key :: <integer> = *next-unique-dispatch-key*;
  let n-needed :: <integer> = next-key + ninc;
  let keyed :: <simple-object-vector> = *implementation-classes-by-key*;
  let nkeyed :: <integer> = size(keyed);
  if (n-needed >= nkeyed)
    local method groan (new-nkeyed :: <integer>)
            if (n-needed >= new-nkeyed)
              groan(ash(new-nkeyed, 1))
            else
              let new-keyed :: <simple-object-vector>
                = make(<simple-object-vector>, size: new-nkeyed, fill: #f);
              local method loop (i :: <integer>)
                      if (i ~== nkeyed)
                        vector-element(new-keyed, i) := vector-element(keyed, i);
                        loop(i + 1)
                      end
                    end;
              loop(0);
              *implementation-classes-by-key* := new-keyed
            end if;
          end method;
    groan(ash(nkeyed, 1));
  else
    keyed
  end;
end function;


define function initialize-class-dispatch-keys (#rest v) => ()
  initialize-class-dispatch-keys-vectored(v)
end function;


define function initialize-class-dispatch-keys-vectored (v :: <simple-object-vector>) => ()
  let lk :: <simple-lock> = $dispatch-key-lock;
  with-lock(lk)
    let ninc :: <integer> = size(v);
    ensure-key-to-iclass-storage(ninc);
    let classes :: <simple-object-vector> = *implementation-classes-by-key*;
    let next-key :: <integer> = *next-unique-dispatch-key*;
    *next-unique-dispatch-key* := next-key + ninc;
    for (c in v, k :: <integer> from next-key)
      let ic :: <implementation-class> = select (c by instance?)
                                           <class> =>
                                             let c :: <class> = c;
                                             class-implementation-class(c);
                                           <implementation-class> =>
                                             c;
                                         end select;
      vector-element(classes, k) := ic;
      iclass-dispatch-key(ic) := iclass-number-to-key(k)
    end for;
  end with-lock;
end function;




/* <function>
    debug-name
    function-signature :: <signature>
    xep
    iep
<lambda>
    environment
<method>
    mep
    keyword-specifiers
  */

//define sealed generic engine-node-callback(e :: <engine-node>)
// => (v :: <raw-pointer>);
//define sealed generic engine-node-callback-setter (v :: <raw-pointer>, e :: <engine-node>)
// => (v :: <raw-pointer>);
//define sealed generic engine-node-entry-point (e :: <engine-node>)
// => (v :: <raw-pointer>);
//define sealed generic engine-node-entry-point-setter (v :: <raw-pointer>, e :: <engine-node>)
// => (v :: <raw-pointer>);

define inline function engine-node-raw-integer (e :: <engine-node>)
 => (n :: <integer>);
  // primitive-pointer-as-small-integer(engine-node-callback(e))
  engine-node-callback(e)
end function;

define inline function engine-node-raw-integer-setter (v :: <integer>, e :: <engine-node>)
 => (n :: <integer>);
  // engine-node-callback(e) := primitive-small-integer-as-pointer(v);
  engine-node-callback(e) := v;
  v
end function;


//define macro engine-node-slot-definer
//  { define engine-node-slot ?name:name ?type:name ?rettype:name ?which:name ;}
//    => {
//          define inline function ?name (e :: ?type) => (value :: ?rettype);
//            ?which(e)
//          end function;
//          define inline function ?name ## "-setter" (value :: ?rettype, e :: ?type) => (value :: ?rettype);
//            ?which(e) := value
//          end function;
//        }
//end macro;


/*   **** PROPERTIES ****

There used to be two other bits having to do with permanency and precomputation or
something (I've forgotten!) which want to be in all engine-nodes at some point in
the future.  Adding them will be more convenient when the properties word is made
raw, and we can use the tag bits.


Engine Node
Entry-type is contained in low byte, shifted 2:  mask or shift out low 2 bits.

_31_________________________________________________________8_7____________2_1___________0_
|                             other                          |  entry type  |  fixnum tag  |
-------------------------------------------------------------------------------------------

Single-method engine node

_31___________________17______16_____15_____________________8_7____________2_1___________0_
|         other          |  restp   |      nrequired         |  entry type  |  fixnum tag  |
-------------------------------------------------------------------------------------------


Discriminator
Argument number to discriminate on is contained in second byte.
Third byte is the number of required arguments, and the following bit indicates whether
there are any optionals.  The sum of that byte and the bit give the number of MEP-style
arguments, which may be of use to primitive-initialize-discriminator.


_31_____25____24_____23___________16_15____________________8_7____________2_1___________0_
|  other  |  restp  |   nrequired   |  discriminator argnum  |  entry type  |  fixnum tag  |
-------------------------------------------------------------------------------------------


*/

// **** Every constant defined below is duplicated in both the compiler (dfmc/modeling)
// **** and in harp (core-harp/function-offsets).

define constant properties$m-entry-type = 63;
define constant properties$s-entry-type = 6;
define constant properties$v-entry-type = 0;
define constant properties$v-data = properties$s-entry-type;

define constant engine-node$v-data-start = 14;

define constant smen$v-nrequired = 6;
define constant smen$s-nrequired = 8;
define constant smen$m-nrequired = ash(ash(1, smen$s-nrequired) - 1, smen$v-nrequired);
define constant smen$v-restp = 14;
define constant smen$m-restp = ash(1, smen$v-restp);
define constant smen$v-data-start = 15;


define constant $simple-typechecked-cache-arguments-limit = 8;

define constant stchen$v-checkedmask = engine-node$v-data-start;
define constant stchen$s-checkedmask = $simple-typechecked-cache-arguments-limit;
define constant stchen$m-checkedmask = ash(ash(1, stchen$s-checkedmask) - 1, stchen$v-checkedmask);

define constant $partial-dispatch-arguments-limit = 8;
define constant pdisp$v-typemask = engine-node$v-data-start;
define constant pdisp$s-typemask = $partial-dispatch-arguments-limit;
define constant pdisp$m-typemask = ash(ash(1, pdisp$s-typemask) - 1, pdisp$v-typemask);


define function compress-mask (argmask :: <integer>, checkedmask :: <integer>)
 => (idx :: <integer>)
  local method loop (amask :: <integer>, cmask :: <integer>,
                     ans :: <integer>, shiftbit :: <integer>)
          if (amask == 0)
            ans
          else
            let anext :: <integer> = ash(amask, -1);
            let cnext :: <integer> = ash(cmask, -1);
            if (logbit?(0, cmask))
              loop(anext, cnext, ans + shiftbit, ash(shiftbit, 1))
            elseif (logbit?(0, amask))
              loop(anext, cnext, ans, ash(shiftbit, 1))
            else
              loop(anext, cnext, ans, shiftbit)
            end if
          end if
        end method;
  loop(argmask, checkedmask, 0, 1)
end function;


define function expand-mask (argmask :: <integer>, idx :: <integer>)
 => (mask :: <integer>)
  local method loop (amask :: <integer>, idx :: <integer>, ans :: <integer>, shiftbit :: <integer>)
          if (amask == 0 | idx == 0)
            ans
          else
            let anext :: <integer> = ash(amask, -1);
            let snext :: <integer> = shiftbit + 1;
            if (logbit?(0, amask))
              loop(anext, ash(idx, -1), ans + ash(logand(idx, 1), shiftbit), snext)
            else
              loop(anext, idx, ans, snext)
            end if
          end if
        end method;
  loop(argmask, idx, 0, 0)
end function;


define constant discriminator$v-argnum = 6;

define constant discriminator$s-argnum = 8;

define constant discriminator$m-argnum
  = ash(ash(1, discriminator$s-argnum) - 1, discriminator$v-argnum);

define constant discriminator$v-nrequired = 14;
define constant discriminator$s-nrequired = 8;
define constant discriminator$m-nrequired
  = ash(ash(1, discriminator$s-nrequired) - 1, discriminator$v-nrequired);

define constant discriminator$v-restp = 22;
define constant discriminator$m-restp = ash(1, discriminator$v-restp);

define constant discriminator$v-data-start = 23;


define inline function discriminator-argnum (d :: <discriminator>) => (argnum :: <integer>);
  %load-byte(discriminator$v-argnum, discriminator$s-argnum, properties(d))
end function;


define variable *engine-node-callbacks* :: <simple-object-vector>
  = make(<simple-object-vector>,
         size: ash(1, properties$s-entry-type));


define inline function engine-node-function-code (d :: <engine-node>)
  %load-byte(properties$v-entry-type,
             properties$s-entry-type,
             properties(d))
end function;


define inline constant <dispatch-starter>
  = type-union(<generic-function>, <cache-header-engine-node>);


define inline function %invoke-engine-node (d :: <object> /* <engine-node> */,
                                            gf :: <generic-function>,
                                            args :: <simple-object-vector>)
  primitive-engine-node-apply-with-optionals(d, gf, args)
end function;


define inline function %invoke-generic-function-mepized
    (gf :: <generic-function>, args :: <simple-object-vector>)
  %invoke-engine-node(discriminator(gf), gf, args)
end function;


define inline function %invoke-generic-function (gf :: <generic-function>, args :: <simple-object-vector>)
  apply(gf, args)
end function;


define function %restart-dispatch (from :: <dispatch-starter>, mepized-args :: <simple-object-vector>)

  %invoke-engine-node(if (instance?(from, <generic-function>))
                        let from :: <generic-function> = from;
                        discriminator(from)
                      else
                        from
                      end if,
                      from,
                      mepized-args)
end function;


begin
  let classes :: <simple-object-vector> = *engine-node-classes*;
  let callbacks :: <simple-object-vector> = *engine-node-callbacks*;
  local method eassign(i :: <integer>, c :: <class>, f)
          classes[i] := c;
          callbacks[i] := f;
        end;
  local method dassign(i :: <integer>, c :: <class>, f)
          eassign(i, c, f)
        end;
  eassign(engine-node$k-absent,
          <absent-engine-node>,
          %gf-dispatch-absent);
  eassign(engine-node$k-inapplicable,
          <inapplicable-engine-node>,
          %gf-dispatch-inapplicable);
  eassign(engine-node$k-unkeyed-single-method,
          <unkeyed-single-method-engine-node>,
          #f);
  eassign(engine-node$k-implicit-keyed-single-method,
          <implicit-keyed-single-method-engine-node>,
          #f);
  eassign(engine-node$k-explicit-keyed-single-method,
          <explicit-keyed-single-method-engine-node>,
          #f);
  eassign(engine-node$k-unrestricted-keyed-single-method,
          <unrestricted-keyed-single-method-engine-node>,
          #f);
  eassign(engine-node$k-ambiguous-methods,
          <ambiguous-methods-engine-node>,
          %gf-dispatch-ambiguous-methods);

  eassign(engine-node$k-profiling-cache-header,
          <profiling-call-site-cache-header-engine-node>,
          #f);

  eassign(engine-node$k-cache-header,
          <simple-typechecked-cache-header-engine-node>,
          #f);


  dassign(engine-node$k-linear-by-class,
          <linear-by-class-discriminator>,
          %gf-dispatch-linear-by-class);
  dassign(engine-node$k-hashed-by-class,
          <hashed-by-class-discriminator>,
          %gf-dispatch-hashed-by-class);
  dassign(engine-node$k-linear-by-singleton-class,
          <linear-by-singleton-class-discriminator>,
          %gf-dispatch-linear-by-singleton-class);
  dassign(engine-node$k-hashed-by-singleton-class,
          <hashed-by-singleton-class-discriminator>,
          %gf-dispatch-hashed-by-singleton-class);
  dassign(engine-node$k-typecheck,
          <typecheck-discriminator>,
          %gf-dispatch-typecheck);
  dassign(engine-node$k-if-type,
          <if-type-discriminator>,
          %gf-dispatch-if-type);
  dassign(engine-node$k-immediate-linear-singleton,
          <immediate-linear-singleton-discriminator>,
          %gf-dispatch-immediate-linear-singleton);
  dassign(engine-node$k-value-object-linear-singleton,
          <value-object-linear-singleton-discriminator>,
          %gf-dispatch-value-object-linear-singleton);

  dassign(engine-node$k-monomorphic-by-class,
          <monomorphic-by-class-discriminator>,
          #f);

  eassign(engine-node$k-boxed-instance-slot-getter,
          <boxed-instance-slot-getter-engine-node>,
          #f);
  eassign(engine-node$k-boxed-instance-slot-setter,
          <boxed-instance-slot-setter-engine-node>,
          #f);
  eassign(engine-node$k-boxed-class-slot-getter,
          <boxed-class-slot-getter-engine-node>,
          %gf-dispatch-boxed-class-slot-getter);
  eassign(engine-node$k-boxed-class-slot-setter,
          <boxed-class-slot-setter-engine-node>,
          %gf-dispatch-boxed-class-slot-setter);
  eassign(engine-node$k-boxed-repeated-instance-slot-getter,
          <boxed-repeated-instance-slot-getter-engine-node>,
          #f);
  eassign(engine-node$k-boxed-repeated-instance-slot-setter,
          <boxed-repeated-instance-slot-setter-engine-node>,
          #f);
  eassign(engine-node$k-raw-byte-repeated-instance-slot-getter,
          <repeated-byte-slot-getter-engine-node>,
          #f);
  eassign(engine-node$k-raw-byte-repeated-instance-slot-setter,
          <repeated-byte-slot-setter-engine-node>,
          #f);

end;

// define constant $inapplicable-engine-node =
//      bootstrap-allocate-and-initialize-engine-node(engine-node$k-inapplicable, 0);

// define constant $absent-engine-node =
//  bootstrap-allocate-and-initialize-engine-node(engine-node$k-absent, 0);


// define constant get-absent-engine-node = method () #f end;

define variable *dispatch-profiling-enabled?* :: <boolean> = #f;
