language: infix-dylan
module: dispatch-engine-internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline function fixnum-dammit (x :: <integer>) => (x :: <integer>) x end;


define constant <callback>   = <simple-method>;
define constant callback-iep = mep;


define function parent-gf (parent :: <dispatch-starter>) => (g :: <generic-function>)
  select (parent by instance?)
    <generic-function> =>
      let g :: <generic-function> = parent;
      g;
    <cache-header-engine-node> =>
      let c :: <cache-header-engine-node> = parent;
      parent-gf(cache-header-engine-node-parent(c));
  end select
end function;


define function dispatch-start (dispatch-starter :: <dispatch-starter>) => (engine)
  select (dispatch-starter by instance?)
    <generic-function> =>
      let gf :: <generic-function> = dispatch-starter;
      discriminator(gf);
    <cache-header-engine-node> =>
      let e :: <cache-header-engine-node> = dispatch-starter;
      cache-header-engine-node-next(e) | $absent-engine-node;
  end select
end function;


define function dispatch-start-setter (v, dispatch-starter :: <dispatch-starter>) => (engine)
  select (dispatch-starter by instance?)
    <generic-function> =>
      let gf :: <generic-function> = dispatch-starter;
      discriminator(gf) := v;
    <cache-header-engine-node> =>
      let e :: <cache-header-engine-node> = dispatch-starter;
      cache-header-engine-node-next(e) := v;
  end select
end function;


define function bootstrap-allocate-engine-node (entry-type :: <integer>, root-bits :: <integer>)
 => (e :: <engine-node>)
  let classes :: <simple-object-vector> = *engine-node-classes*;
  let c :: <class> = vector-element(classes, entry-type);
  bootstrap-typed-allocate-engine-node(c, entry-type, root-bits)
end function;

define function bootstrap-typed-allocate-engine-node (c :: <class>,
                                                      entry-type :: <integer>,
                                                      root-bits :: <integer>)
  let callbacks :: <simple-object-vector> = *engine-node-callbacks*;
  let e :: <engine-node> = system-allocate-simple-instance(c);
  let extra-bits :: <integer> = logand(root-bits, lognot(properties$m-entry-type));
  properties(e) := logior(extra-bits, entry-type);
  let callback? = vector-element(callbacks, entry-type);
  if (callback?)
    let callback :: <callback> = callback?;
    engine-node-callback(e) := callback-iep(callback);
  end if;
  e
end function;

define function bootstrap-allocate-and-initialize-engine-node
    (entry-type :: <integer>, root-bits :: <integer>)
 => (e :: <engine-node>);
  let e :: <engine-node> = bootstrap-allocate-engine-node(entry-type, root-bits);
  primitive-initialize-engine-node(e);
  e
end function;


define function standard-discriminator-bits
    (gf :: <generic-function>) => (bits :: <integer>);
  let sig :: <signature> = function-signature(gf);
  let props :: <integer> = ash(signature-number-required(sig),
                               discriminator$v-nrequired);
  if (signature-optionals?(sig)) logior(props, discriminator$m-restp) else props end
end function;

define constant $standard-discriminator-bit-mask
  = logior(discriminator$m-nrequired, discriminator$m-restp);



define inline function bootstrap-shared-allocate-discriminator
    (entry-type :: <integer>, argnum :: <integer>, root-bits :: <integer>, d :: <discriminator>)
 => (d :: <discriminator>)
  let callbacks :: <simple-object-vector> = *engine-node-callbacks*;
  let props :: <integer>
    = logand(root-bits, lognot(logior(discriminator$m-argnum, properties$m-entry-type)));
  props := logior(props, entry-type);
  props := logior(props, fixnum-dammit(ash(argnum, discriminator$v-argnum)));
  let nreq :: <integer>
    = ash(logand(root-bits, discriminator$m-nrequired),
          - discriminator$v-nrequired);
  if (~(argnum < nreq))        // @@@@ (argnum >= nreq)
    error("Discriminator being created with conflicting nrequired %= and argnum %=.", nreq, argnum)
  end if;
  properties(d) := props;
  let callback? = vector-element(callbacks, entry-type);
  if (callback?)
    let callback :: <callback> = callback?;
    engine-node-callback(d) := callback-iep(callback);
  end if;
  d
end function;

define function bootstrap-allocate-discriminator
    (entry-type :: <integer>, argnum :: <integer>, root-bits :: <integer>)
 => (d :: <discriminator>)
  let classes :: <simple-object-vector> = *engine-node-classes*;
  let c :: <class> = vector-element(classes, entry-type);
  let d :: <discriminator> = system-allocate-simple-instance(c);
  bootstrap-shared-allocate-discriminator(entry-type, argnum, root-bits, d);
end function;

define function bootstrap-allocate-repeated-discriminator
    (entry-type :: <integer>, argnum :: <integer>, root-bits :: <integer>, size :: <integer>, default)
 => (d :: <discriminator>)
      let classes :: <simple-object-vector> = *engine-node-classes*;
  let c :: <class> = vector-element(classes, entry-type);
  let d :: <discriminator> = system-allocate-repeated-object-instance(c, #f, size, default);
  bootstrap-shared-allocate-discriminator(entry-type, argnum, root-bits, d);
end function;



define open generic bletch (x :: <condition>) => ();


define open method bletch (x :: <condition>) => ()
  signal(x)
end method;


define open method bletch (x :: <error>) => ()
  error(x)
end method;


define function bletch-stack (l :: <list>)
  if (l == #())
    #f
  else
    bletch-stack(tail(l));
    bletch(head(l));
  end if
end function;


/* *******************************************************
   Object locking
   ******************************************************* */

// This is the general mechanism we use for locking individual objects
// against writing.  It doesn't need extra space in the object, nor a high
// number of locks (two total), and hopefully it will be fast enough.  I
// see it being used for generic functions, maybe dispatch tables (I'm
// undecided as to how generic function vs. dispatch table locking will be
// done during discrimination fulfillment).  I suspect class redefinition
// may require a bigger and more exclusive club (not per-object).  Maybe
// this lock will revert to being a generic function only lock, and maybe
// as a consequence it will implicitly lock out class redefinition, or
// something like that.  Anyway, it seems to be a reasonable paradigm for
// being able to write-lock individual objects.


define constant $object-lock-notification-lock :: <simple-lock> =
  make-simple-lock();

define constant $object-lock-notification :: <notification> =
  make-notification($object-lock-notification-lock);


define variable *object-lock-data* :: <list>  = #();


define inline-only function token-for-current-thread () => (a-token)
  if (*dylan-library-initialized?*)
    current-thread()
  else #t; // This token could be anything
  end if;
end function;


// define function object-locked-p (obj) => (ans :: <list>);
//   iterate loop (l :: <list> = *object-lock-data*)
//     if (l == #())
//       #()
//     else
//       let couple :: <pair> = head(l);
//       if (pointer-id?(head(couple), obj))
//         couple
//       else
//         let nxt :: <list> = tail(l);
//         loop(nxt)
//       end if
//     end if
//   end iterate
// end function;


define function multiple-objects-locked-p (cells :: <list>, tokin) => (ans);
  if (*object-lock-data* == #())
    #f
  else
    local method peruse (cells :: <list>, recursive-losers :: <list>)
            if (cells == #())
              if (recursive-losers == #()) #f else recursive-losers end
            else
              let cell :: <pair> = head(cells);
              let obj = head(cell);
              let nxt :: <list> = tail(cells);
              local method checklock (l :: <list>)
                      if (l == #())
                        peruse(nxt, recursive-losers)
                      else
                        let this :: <pair> = head(l);
                        if (pointer-id?(obj, this))
                          if (pointer-id?(tokin, tail(this)))
                            peruse(nxt, pair(obj, recursive-losers))
                          else
                            #t
                          end if
                        else
                          checklock(tail(l))
                        end if
                      end if
                    end method;
              checklock(*object-lock-data*)
            end if
          end method;
    peruse(cells, #())
  end if
end function;


//define function begin-locking-object (cell :: <pair>)
//  let cell2 :: <pair> = head(cell);
//  let obj = head(cell2);
//  let lock :: <simple-lock> = $object-lock-notification-lock;
//  let notif :: <notification> = $object-lock-notification;
//  with-lock (lock)
//    iterate try-again ()
//      let lockedp :: <list> = object-locked-p(obj);
//      if (lockedp == #())
//        tail(cell) := *object-lock-data*;
//        *object-lock-data* := cell;
//        tail(cell2) := token-for-current-thread();
//        release(notif);
//        cell
//      elseif (tail(lockedp) == token-for-current-thread())
//        error("Attempt to recursively lock object %=", obj)
//      else
//        wait-for(notif);
//        try-again()
//      end if
//    end iterate
//  end with-lock
//end function;

define function begin-locking-multiple-objects (hd :: <pair>, tl :: <pair>)
  let lock :: <simple-lock> = $object-lock-notification-lock;
  let notif :: <notification> = $object-lock-notification;
  let tokin = token-for-current-thread();
  with-lock (lock)
    iterate try-again ()
      let stuff = multiple-objects-locked-p(hd, tokin);
      if (stuff == #f)
        for (x :: <pair> in hd) tail(x) := tokin end;
        tail(tl) := *object-lock-data*;
        *object-lock-data* := hd;
        release(notif);
      elseif (stuff == #t)
        wait-for(notif);
        try-again()
      else
        error("Attempt to recursively lock objects %=", stuff)
      end if
    end iterate
  end with-lock
end function;


define function end-locking-object-cell (cell :: <pair>)
  let data :: <list> = *object-lock-data*;
  let first-l :: <list> = tail(data);
  if (cell == data)
    *object-lock-data* := tail(data)
  else
    iterate sigh (prev :: <list> = data, l :: <list> = first-l)
      if (l == #())
        #f                    // This means we aborted before getting the lock.
      else
        let nxt :: <list> = tail(l);
        if (l == cell)
          tail(prev) := nxt;
        else sigh(l, nxt)
        end if
      end if
    end iterate
  end if;
end function;

//define function end-locking-object (cell :: <pair>)
//  let lock :: <simple-lock> = $object-lock-notification-lock;
//  let notif :: <notification> = $object-lock-notification;
//  with-lock (lock)
//    end-locking-object-cell(cell);
//    release(notif);
//  end with-lock;
//  values()
//end function;


define function end-locking-multiple-objects (hd :: <pair>, tl :: <pair>) => ()
  let lock :: <simple-lock> = $object-lock-notification-lock;
  let notif :: <notification> = $object-lock-notification;
  with-lock (lock)
    block(done)
      for (x :: <pair> = hd then tail(x))
        end-locking-object-cell(x);
        if (x == tl) done() end;
      end for;
    end;
    release(notif);
  end with-lock;
  values()
end function;


define macro with-object-lock

  { with-object-lock (?object:expression)
      ?body:body
    end
  }
  =>
//  { begin
//      let $cell$ = pair(pair(?object, #f), #());
//      block ()
//        begin-locking-object($cell$);
//        ?body;
//      cleanup
//        end-locking-object($cell$);
//      end block
//    end
//  }

    { begin
        let _objlist = pair(pair(?object, #f), #());
        block ()
          begin-locking-multiple-objects(_objlist, _objlist);
          ?body;
        cleanup
          end-locking-multiple-objects(_objlist, _objlist);
        end block
      end
   }

  { with-object-lock (?object:expression, ?punter:name)
      ?body:body
    end
  }
  =>
    { begin
        let _objlist = pair(pair(?object, #f), #());
        block (?punter)
          begin-locking-multiple-objects(_objlist, _objlist);
          ?body;
        cleanup
          end-locking-multiple-objects(_objlist, _objlist);
        end block
      end
   }
end macro;


define inline-only function make-multiple-object-lock-cells (seq :: <sequence>)
 => (h :: <pair>, t :: <pair>)
  let t = #();
  for (x in seq,
       ans :: <list> = #() then pair(pair(x, #f), ans))
    if (t == #()) t := ans end;
    finally values(ans, if (t == #()) ans else t end)
  end for;
end function;


define macro with-multiple-object-lock

  { with-multiple-object-lock (?objectsequence:expression)
      ?body:body
    end
  }
  =>
  { begin
      let _objseq = ?objectsequence;
      if (~empty?(_objseq))
        let (_head :: <pair>, _tail :: <pair>) = make-multiple-object-lock-cells(_objseq);
        block ()
          begin-locking-multiple-objects(_head, _tail);
          ?body;
        cleanup
          end-locking-multiple-objects(_head, _tail);
        end block
      end if
    end
  }

end macro;




/* *******************************
   Simple, terminal discriminators
   ******************************* */

define function %gf-dispatch-absent (mepargs :: <simple-object-vector>,
                                     e :: <engine-node>,
                                     parent :: <dispatch-starter>)
  handle-missed-dispatch(e, parent, mepargs)
end function;

define function %gf-dispatch-inapplicable (spreadargs :: <simple-object-vector>,
                                           e :: <inapplicable-engine-node>,
                                           parent :: <dispatch-starter>)
  e;
  no-applicable-method-error(parent-gf(parent), copy-sequence(spreadargs))
end function;



define function %gf-dispatch-ambiguous-methods (spreadargs :: <simple-object-vector>,
                                                e :: <ambiguous-methods-engine-node>,
                                                parent :: <dispatch-starter>)
  ambiguous-method-error(parent-gf(parent), copy-sequence(spreadargs),
                         ambiguous-methods-engine-node-ordered(e),
                         ambiguous-methods-engine-node-ambig(e))
end function;

define function make-ambiguous-methods-engine-node
    (ordered :: <sequence>, ambig :: <sequence>)
 => (e :: <ambiguous-methods-engine-node>);
  let e :: <ambiguous-methods-engine-node>
    = bootstrap-allocate-engine-node(engine-node$k-ambiguous-methods, 0);
  ambiguous-methods-engine-node-ordered(e) := ordered;
  ambiguous-methods-engine-node-ambig(e) := ambig;
  primitive-initialize-engine-node(e);
  e
end function;


define inline function make-ambiguous-methods-next-method
    (ordered :: <sequence>, ambig :: <sequence>, gf :: <generic-function>)
 => (p :: <pair>);
  pair(make-ambiguous-methods-engine-node(ordered, ambig), gf)
end function;


define sealed inline method make (c == <single-method-engine-node>, #key meth :: <method>, data, keys)
 => (e :: <single-method-engine-node>);
  c;
  make-single-method-engine-node(meth, data: data, keys: keys)
end method;

define function make-single-method-engine-node (meth :: <method>, #key data, keys)
  // @@@@@ The method here is known to not be an <accessor-method>, so can
  // use a more specialized version of function-signature when one is available.
  let sig :: <signature> = function-signature(meth);
  let bits :: <integer> = ash(signature-number-required(sig), smen$v-nrequired);
  let bits :: <integer> = if (signature-optionals?(sig)) logior(smen$m-restp, bits) else bits end;
  let sme :: <single-method-engine-node>
    = if (keys == #f)
        bootstrap-allocate-engine-node(engine-node$k-unkeyed-single-method, bits)
      elseif (keys == #t)
        bootstrap-allocate-engine-node(engine-node$k-unrestricted-keyed-single-method, bits)
      else
        let keys :: <simple-object-vector> = keys;
        let mkeys :: <simple-object-vector> = keyword-specifiers(meth);
        let nkeys :: <integer> = size(keys);
        let nmkeys :: <integer> = size(mkeys);
        if (nkeys = ash(nmkeys, -1)
              & begin
                  local method outer (i :: <integer>)
                          if (i = nkeys)
                            #t
                          else
                            let k :: <symbol> = vector-element(keys, i);
                            local method inner (j :: <integer>)
                                    if (j = nmkeys)
                                      #f
                                    elseif (pointer-id?(k, vector-element(mkeys, j)))
                                      outer(i + 1)
                                    else
                                      inner(j + 2)
                                    end if
                                  end method;
                            inner(0)
                          end if
                        end method;
                  outer(0)
                end)
          bootstrap-allocate-engine-node(engine-node$k-implicit-keyed-single-method, bits)
        else
          let e :: <explicit-keyed-single-method-engine-node>
            = bootstrap-allocate-engine-node(engine-node$k-explicit-keyed-single-method, bits);
          single-method-engine-node-keys(e) := keys;
          e
        end if
      end if;
  single-method-engine-node-method(sme) := meth;
  single-method-engine-node-data(sme) := data;
  primitive-initialize-engine-node(sme);
  sme
end function;



/* *****************
   Dispatch by Class
   ***************** */

define constant ckd$v-log2size = discriminator$v-data-start;

define constant ckd$s-log2size = 5;

define macro with-lckd-dispatch
  { with-lckd-dispatch (?d:name)
      ?:body
    end }
    => { if (instance?(?d, <linear-by-singleton-class-discriminator>))
           let ?d :: <linear-by-singleton-class-discriminator> = ?d;
           ?body
         else
           let ?d :: <linear-by-class-discriminator> = ?d;
           ?body
         end if }
end macro;

define macro with-hckd-dispatch
  { with-hckd-dispatch (?d:name)
      ?:body
    end }
    => { if (instance?(?d, <hashed-by-singleton-class-discriminator>))
           let ?d :: <hashed-by-singleton-class-discriminator> = ?d;
           ?body
         else
           let ?d :: <hashed-by-class-discriminator> = ?d;
           ?body
         end if }
end macro;

define macro with-ckd-dispatch
  { with-ckd-dispatch (?d:name)
      ?:body
    end }
    => { if (instance?(?d, <linear-class-keyed-discriminator>))
           let ?d :: <linear-class-keyed-discriminator> = ?d;
           with-lckd-dispatch (?d)
             ?body
           end
         else
           let ?d :: <hashed-class-keyed-discriminator> = ?d;
           with-hckd-dispatch (?d)
             ?body
           end
         end if }
end macro;

define inline function %ckd-ref
    (ckd :: <class-keyed-discriminator>, idx :: <integer>)
  class-keyed-discriminator-table-element(ckd, idx)
end function;

define inline function %ckd-ref-setter
    (value, ckd :: <class-keyed-discriminator>, idx :: <integer>)
  class-keyed-discriminator-table-element(ckd, idx) := value
end function;

define inline function %ckd-size (ckd :: <class-keyed-discriminator>)
 => (key-and-value-vector-size :: <integer>)
  class-keyed-discriminator-table-size(ckd)
end function;

define inline function %ckd-mask (ckd :: <class-keyed-discriminator>) => (mask :: <integer>)
  class-keyed-discriminator-table-size(ckd) - 2
end function;

// @@@@ Variable because Apple Dylan compiler copies value and loses eqness!
// @@@@ Keep it that way - better safe than sorry.
define variable $ckd-empty
  = #(#"*empty*");


// How big we let a linear discrimination table get.  Units are total number
// of data entries, twice the number of key/value pairs.
define constant $linear-discriminator-table-limit :: <integer> = 2 * 5;
define constant $linear-discriminator-table-growth-increment :: <integer> = 2 * 2;


define function hashed-class-keyed-discriminator-log2size
    (storage-size :: <integer>) => (table-size :: <integer>)
  local method f (i :: <integer>)
          let nxt :: <integer> = i + 1;
          let siz :: <integer> = %twopower(i);
          // @@@@ if (siz > storage-size) nxt else f(nxt) end
          if (storage-size < siz) nxt else f(nxt) end
        end method;
  f(4)
end function;

define inline function grow-linear-class-keyed-discriminator
    (d :: <linear-class-keyed-discriminator>)
 => (nd :: <class-keyed-discriminator>)
  let stgsiz :: <integer> = %ckd-size(d);
  let nstgsiz :: <integer> = stgsiz + $linear-discriminator-table-growth-increment;
  let initbits :: <integer> = logand(properties(d), $standard-discriminator-bit-mask);
  dbg("grow-linear-class-keyed-discriminator %= - stgsiz=%=, nstgsiz=%=",
      d, stgsiz, nstgsiz);
  let nd :: <class-keyed-discriminator>
    = if (~(nstgsiz < $linear-discriminator-table-limit))  // @@@@ (nstgsiz >= $linear-discriminator-table-limit)
        make-hashed-class-keyed-discriminator(logior(engine-node-function-code(d), 1),
                                              discriminator-argnum(d),
                                              hashed-class-keyed-discriminator-log2size(nstgsiz),
                                              initbits)
      else
        make-linear-class-keyed-discriminator(logand(engine-node-function-code(d), -2),
                                              discriminator-argnum(d), nstgsiz, initbits)
      end if;
  copy-class-keyed-discriminator-attributes(d, nd);
  local method loop (i :: <integer>, nd :: <class-keyed-discriminator>)
          if (i = stgsiz)
            nd
          else
            loop(i + 2, ckd-add!(nd, %ckd-ref(d, i), %ckd-ref(d, i + 1)))
          end if
        end method;
  loop(0, nd);
end function;

define inline function grow-hashed-class-keyed-discriminator
    (d :: <hashed-class-keyed-discriminator>)
 => (nd :: <hashed-class-keyed-discriminator>)
  let stgsiz :: <integer> = %ckd-size(d);
  let log2stgsize = %load-byte(ckd$v-log2size, ckd$s-log2size, properties(d));
  let nstgsiz :: <integer> = ash(stgsiz, 1);
  let initbits :: <integer> = logand(properties(d), $standard-discriminator-bit-mask);
  dbg("grow-hashedclass-keyed-discriminator %= - stgsiz=%=, log2=%=, nstgsiz=%=",
      d, stgsiz, log2stgsize, nstgsiz);
  let nd :: <hashed-class-keyed-discriminator>
       = make-hashed-class-keyed-discriminator(engine-node-function-code(d), discriminator-argnum(d),
                                               log2stgsize + 1, initbits);
  copy-class-keyed-discriminator-attributes(d, nd);
  local method loop (i :: <integer>, nd :: <class-keyed-discriminator>)
          if (i = stgsiz)
            nd
          else
            let k = %ckd-ref(d, i);
            if (pointer-id?(k, $ckd-empty))
              loop (i + 2, nd)
            else
              loop(i + 2, ckd-add!(nd, k, %ckd-ref(d, i + 1)))
            end if
          end if
        end method;
  loop (0, nd)
end function;

define function copy-class-keyed-discriminator-attributes
    (d :: <class-keyed-discriminator>, nd :: <class-keyed-discriminator>)
 => ()
  if (instance?(d, <by-singleton-class-discriminator>))
    grounded-class-keyed-discriminator-default(nd) := grounded-class-keyed-discriminator-default(d);
  end if;
end function;

define function grounded-class-keyed-discriminator-default
    (d :: <class-keyed-discriminator>)
 => (nd :: <object>)
  select (d by instance?)
    <monomorphic-by-class-discriminator>, <linear-by-class-discriminator>, <hashed-by-class-discriminator> =>
      $absent-engine-node;
    <linear-by-singleton-class-discriminator> =>
      let d :: <linear-by-singleton-class-discriminator> = d;
      class-keyed-discriminator-default(d);
    <hashed-by-singleton-class-discriminator> =>
      let d :: <hashed-by-singleton-class-discriminator> = d;
      class-keyed-discriminator-default(d);
  end select
end function;

define function grounded-class-keyed-discriminator-default-setter
    (value :: <object>, d :: <class-keyed-discriminator>)
 => (nd :: <object>)
  select (d by instance?)
    <linear-by-singleton-class-discriminator> =>
      let d :: <linear-by-singleton-class-discriminator> = d;
      class-keyed-discriminator-default(d) := value;
    <hashed-by-singleton-class-discriminator> =>
      let d :: <hashed-by-singleton-class-discriminator> = d;
      class-keyed-discriminator-default(d) := value;
  end select
end function;

define function make-linear-class-keyed-discriminator
    (code :: <integer>, argnum :: <integer>,
     table-size :: <integer>, extra-bits :: <integer>)
 => (discriminator :: <linear-class-keyed-discriminator>)
  dbg("make-linear-class-keyed-discriminator %= %= %=", code, argnum, table-size);
  let d :: <linear-class-keyed-discriminator>
    = bootstrap-allocate-repeated-discriminator(code, argnum, extra-bits, table-size, $ckd-empty);
  lckd-index(d) := 0;
  primitive-initialize-discriminator(d);
  d
end function;

define function make-hashed-class-keyed-discriminator
    (code :: <integer>, argnum :: <integer>,
     log2size :: <integer>, extra-bits :: <integer>)
 => (discriminator :: <class-keyed-discriminator>);
  dbg("make-hashed-class-keyed-discriminator %= %= %=", code, argnum, log2size);
  let bitz :: <integer> = logior(extra-bits, ash(log2size, ckd$v-log2size));
  let table-size :: <integer> = %twopower(log2size);
  let d :: <hashed-class-keyed-discriminator>
    = bootstrap-allocate-repeated-discriminator(code, argnum, bitz, table-size, $ckd-empty);
  primitive-initialize-discriminator(d);
  d
end function;

define function make-initial-class-keyed-discriminator
    (code :: <integer>, argnum :: <integer>,
     gf :: <generic-function>, number-of-keys :: <integer>)
  dbg("make-initial-class-keyed-discriminator %= %= %=", code, argnum, number-of-keys);
  let stgsiz :: <integer> = ash(number-of-keys, 1);
  let bitz :: <integer> = standard-discriminator-bits(gf);
  if (~(stgsiz < $linear-discriminator-table-limit)) // @@@@ (stgsiz >= $linear-discriminator-table-limit)
    make-hashed-class-keyed-discriminator(logior(code, 1), argnum,
                                          hashed-class-keyed-discriminator-log2size(stgsiz),
                                          bitz)
  else
    make-linear-class-keyed-discriminator(logand(code, -2), argnum, logand(stgsiz + 3, -8),
                                          bitz)
  end if
end function;

define function make-by-class-discriminator
    (argnum :: <integer>, gf :: <generic-function>, number-of-keys :: <integer>)
 => (d :: <by-class-discriminator>);
  if (number-of-keys == 1)
    make-monomorphic-by-class-discriminator(argnum, gf)
  else
    make-initial-class-keyed-discriminator(engine-node$k-linear-by-class, argnum, gf, number-of-keys)
  end if
end function;

define function make-by-singleton-class-discriminator
    (argnum :: <integer>, gf :: <generic-function>, number-of-keys :: <integer>, default)
 => (d :: <by-singleton-class-discriminator>);
  let d :: <by-singleton-class-discriminator>
    = make-initial-class-keyed-discriminator(engine-node$k-linear-by-singleton-class, argnum, gf, number-of-keys);
  grounded-class-keyed-discriminator-default(d) := default;
  d
end function;

define inline function linear-class-key-lookup
    (key :: <integer>, d :: <linear-class-keyed-discriminator>, default)
  let n :: <integer> = %ckd-size(d);
  local method loop (i :: <integer>)
          if (i = n)
            default
          else
            let otherkey = %ckd-ref(d, i);
            if (pointer-id?(otherkey, key))
              %ckd-ref(d, i + 1)
            // elseif (pointer-id?(otherkey, $ckd-empty))
            //   default
            else
              loop(i + 2)
            end if
          end if
        end method;
  loop(0)
end function;

/*
define inline function linear-class-key-lookup
    (key :: <integer>, d :: <linear-class-keyed-discriminator>, default)
 => (ans)

  let firsti :: <integer> = lckd-index(d);

  let otherkey = %ckd-ref(d, firsti);
  if (pointer-id?(otherkey, key))
    %ckd-ref(d, firsti + 1)
  else
    let n :: <integer> = %ckd-size(d);
    local method loop (i :: <integer>)
            let i :: <integer> = i + 2;
            let i :: <integer> = if (i == n) 0 else i end;
            if (i == firsti)
              default
            else
              let otherkey = %ckd-ref(d, i);
              if (pointer-id?(otherkey, key))
                lckd-index(d) := i;
                %ckd-ref(d, i + 1)
              else
                loop(i)
              end if
            end if
          end method;
    loop(firsti)
  end if
end function;
*/

define function grounded-linear-class-key-lookup
    (key :: <integer>, d :: <linear-class-keyed-discriminator>, default)
  with-lckd-dispatch (d)
    linear-class-key-lookup(key, d, default);
  end with-lckd-dispatch;
end function;

define function %gf-dispatch-linear-by-class
    (arg, parent :: <dispatch-starter>, d :: <linear-by-class-discriminator>)
  parent;
  linear-class-key-lookup(object-class-unique-key(arg), d, $absent-engine-node)
end function;

define function %gf-dispatch-linear-by-singleton-class
    (arg :: <class>, parent :: <dispatch-starter>, d :: <linear-by-singleton-class-discriminator>)
  parent;
  linear-class-key-lookup(class-unique-key(arg), d, class-keyed-discriminator-default(d))
end function;


/* ******  Hashing.  ******
   *  We use a power of 2 hash table size - avoid division in computing the hash index.
   *  The hash index is computed by masking something of the wrapper.  It may be an incrementally
      assigned index, or it may come from the address, but we assume it is unchanging
      over time  (we don't want to have to rehash, at least not commonly).  It does need to be
      a per-wrapper (maybe per-implementation-class), not per-class, key, if we want to allow
      any kind of lazy invalidation.  The number used to compute the first hash probe index is
      simply masked by the bitmask for the size of the table.  Were it coming from an address, a
      small right-shift might be appropriate.
   *  We compute a step for successive probes in the event of a hash conflict.  To do this,
      we start with the same number which was used to compute the initial probe, but select the
      bits just to the left of the ones used to compute the first index.  This maximizes the chance
      that they are different when the lower set of bits are the same.  Some set of these bits are
      used to index into a table of prime numbers.  The result only has to be odd
      in order step through all positions in the table, so this masking will decrease the optimality of
      the second hash in smaller tables, but not remove it entirely.
   *  The table is referenced as alternating keys and values at successive indices, so all numbers
      indicating positions/probes/steps are returned scaled appropriately by the routines
      which generate them.
*/

define inline function %hckd-first-index
    (key :: <integer>, d :: <hashed-class-keyed-discriminator>)
 => (index :: <integer>)
  logand(key, %ckd-mask(d))
end function;

// See %hckd-hash-step.
define function %second-hash-values () => (v :: <simple-object-vector>)
  #[2,                                // index = 0,  step = 1
    6,                                // index = 1,  Step = 3
    10,                               // index = 2,  Step = 5
    14,                               // index = 3,  Step = 7
    22,                               // index = 4,  step = 11
    26,                               // index = 5,  step = 13
    34,                               // index = 6,  step = 17
    38,                               // index = 7,  step = 19
    46,                               // index = 8,  step = 23
    58,                               // index = 9,  step = 29
    62,                               // index = 10, step = 31
    74,                               // index = 11, step = 37
    82,                               // index = 12, step = 41
    86,                               // index = 13, step = 43
    94,                               // index = 14, step = 47
    106                               // index = 15, step = 53
      ]
end function;

// Mask to compute index mod the size of $second-hash-values.
define constant $second-hash-mask :: <integer> = 15;

define inline function %hckd-hash-step
    (key :: <integer>, d :: <hashed-class-keyed-discriminator>) => (step :: <integer>, mask :: <integer>)
  let log2size :: <integer> = %load-byte(ckd$v-log2size, ckd$s-log2size, properties(d));
  values(vector-element(%second-hash-values(),
                        logand(%scale-down(key, log2size), $second-hash-mask)),
         %twopower(log2size) - 2)
end function;



define inline function %hckd-next-index
    (i :: <integer>, step :: <integer>, mask :: <integer>) => (idx :: <integer>)
  let next :: <integer> = i + step;
  logand(next, mask)
end function;

define inline function hashed-class-key-lookup
    (key :: <integer>, d :: <hashed-class-keyed-discriminator>, default)
 => (discriminator :: <object>);
  let i :: <integer> = %hckd-first-index(key, d);
  let otherkey = %ckd-ref(d, i);
  if (pointer-id?(otherkey, key))
    %ckd-ref(d, i + 1)
  elseif (pointer-id?(otherkey, $ckd-empty))
    default
  else
    let (step :: <integer>, mask :: <integer>)
      = %hckd-hash-step(key, d);
    local method loop (i :: <integer>)
            let i :: <integer> = %hckd-next-index(i, step, mask);
            let otherkey = %ckd-ref(d, i);
            if (pointer-id?(otherkey, key))
              %ckd-ref(d, i + 1)
            elseif (pointer-id?(otherkey, $ckd-empty))
              default
            else
              loop(i)
            end if
          end method;
    loop(i)
  end if
end function;

define function grounded-hashed-class-key-lookup
    (key :: <integer>, d :: <hashed-class-keyed-discriminator>, default)
  with-hckd-dispatch (d)
    hashed-class-key-lookup(key, d, default);
  end with-hckd-dispatch;
end function;

define function %gf-dispatch-hashed-by-class
    (arg, parent :: <dispatch-starter>, d :: <hashed-by-class-discriminator>)
  parent;
  hashed-class-key-lookup(object-class-unique-key(arg), d, $absent-engine-node)
end function;

define function %gf-dispatch-hashed-by-singleton-class
    (arg :: <class>, parent :: <dispatch-starter>, d :: <hashed-by-singleton-class-discriminator>)
  parent;
  hashed-class-key-lookup(class-unique-key(arg), d, class-keyed-discriminator-default(d))
end function;

define function ckd-lookup
    (key, d :: <class-keyed-discriminator>) => (ans :: <object>)
  let default = grounded-class-keyed-discriminator-default(d);
  if (instance?(d, <monomorphic-by-class-discriminator>))
    let d :: <monomorphic-by-class-discriminator> = d;
    if (monomorphic-by-class-discriminator-key(d) == key)
      monomorphic-by-class-discriminator-next(d)
    else
      default
    end if
  else
    if (instance?(d, <linear-class-keyed-discriminator>))
      let d :: <linear-class-keyed-discriminator> = d;
      grounded-linear-class-key-lookup(key, d, default)
    else
      let d :: <hashed-class-keyed-discriminator> = d;
      grounded-hashed-class-key-lookup(key, d, default)
    end if
  end if
end function;

define function ckd-ref
    (d :: <class-keyed-discriminator>, index :: <integer>) => (value);
  with-ckd-dispatch (d)
    %ckd-ref(d, index)
  end with-ckd-dispatch;
end function;

define function ckd-ref-setter
    (value, d :: <class-keyed-discriminator>, index :: <integer>)
  with-ckd-dispatch (d)
    %ckd-ref(d, index) := value;
  end with-ckd-dispatch;
end function;

define function ckd-size
    (d :: <class-keyed-discriminator>) => (value);
  with-ckd-dispatch (d)
    %ckd-size(d)
  end with-ckd-dispatch;
end function;

define function ckd-add!
    (d :: <class-keyed-discriminator>, key, value)
 => (discriminator :: <class-keyed-discriminator>);
  dbg("ckd-add(%=, %=, %=)", d, key, value);
  let ans :: <class-keyed-discriminator>
    = if (instance?(d, <linear-class-keyed-discriminator>))
        let d :: <linear-class-keyed-discriminator> = d;
        grounded-lckd-add!(d, key, value)
      elseif (instance?(d, <monomorphic-by-class-discriminator>))
        let d :: <monomorphic-by-class-discriminator> = d;
        grounded-mckd-add!(d, key, value);
      else
        let d :: <hashed-class-keyed-discriminator> = d;
        grounded-hckd-add!(d, key, value)
      end if;
  dbg(if (ans == d) "ckd-add! same discriminator" else "ckd-add! new discriminator" end);
  ans
end function;

// define constant $initial-linear-size = 4;

define function mckd-add!
    (d :: <monomorphic-by-class-discriminator>, key, value)
 => (discriminator :: <object>);
  let old-key = monomorphic-by-class-discriminator-key(d);
  if (pointer-id?(old-key, $ckd-empty))
    monomorphic-by-class-discriminator-key(d)  := key;
    monomorphic-by-class-discriminator-next(d) := value;
    d
  elseif (pointer-id?(old-key, key))
    monomorphic-by-class-discriminator-next(d) := value;
    d
  else
    let new-d :: <linear-by-class-discriminator>
      = make-linear-class-keyed-discriminator
          (engine-node$k-linear-by-class, discriminator-argnum(d), 2,
           logand(properties(d), $standard-discriminator-bit-mask));
    ckd-add!(lckd-add!(new-d, monomorphic-by-class-discriminator-key(d),
                       monomorphic-by-class-discriminator-next(d)),
             key, value)
  end if;
end function;

define constant grounded-mckd-add! = mckd-add!;

define inline function hckd-add!
    (d :: <hashed-class-keyed-discriminator>, key, value)
 => (discriminator :: <object>);
  let (step :: <integer>, mask :: <integer>) = %hckd-hash-step(key, d);
  let enlarge?
       = method (d :: <hashed-class-keyed-discriminator>, nprobes :: <integer>)
           let siz :: <integer> = %ckd-size(d);         // Note siz is number of entries * 2.
           local method count (i :: <integer>, cnt :: <integer>)
                   if (i == siz)
                     let quarterfull :: <integer> = ash(siz, -3);
                     if (~(quarterfull < cnt))  // @@@@ (cnt <= quarterfull)
                       #f           // The table is less than 1/4 full.
                     else
                       let halffull :: <integer> = ash(siz, -2);
                       if (cnt <= halffull)
                         // Table less than 1/2 full, tolerate 7 collisions.
                         nprobes > 7
                       else
                         // Table is more than 1/2 full.  Grow if the hash chain will be
                         // more than 4 long, or if the table gets 3/4 full.
                         let threequarters :: <integer> = quarterfull + halffull;
                         (cnt >= threequarters) | (nprobes > 4)
                       end if
                     end if
                   else
                     count(i + 2, if (%ckd-ref(d, i) == $ckd-empty) cnt else cnt + 1 end if)
                   end if
                 end method;
           count(0, 0)
         end method;
  local method loop (i :: <integer>, nprobes :: <integer>)
          let otherkey = %ckd-ref(d, i);
          if (pointer-id?(otherkey, key))
            %ckd-ref(d, i + 1) := value;
            d
          elseif (~pointer-id?(otherkey, $ckd-empty))
            loop(%hckd-next-index(i, step, mask), nprobes + 1)
          elseif (enlarge?(d, nprobes))
            let nd :: <hashed-class-keyed-discriminator>
              = grow-hashed-class-keyed-discriminator(d);
            grounded-hckd-add!(nd, key, value)
          else
            %ckd-ref(d, i + 1) := value;
            sequence-point();
            %ckd-ref(d, i) := key;
            d
          end if
        end method;
  loop(%hckd-first-index(key, d), 0)
end function;

define function grounded-hckd-add!
    (d :: <hashed-class-keyed-discriminator>, key, value)
 => (discriminator :: <object>);
  with-hckd-dispatch (d)
    hckd-add!(d, key, value);
  end with-hckd-dispatch;
end function;

define inline function lckd-add!
    (d :: <linear-class-keyed-discriminator>, key, value)
 => (possibly-new-discriminator :: <class-keyed-discriminator>);
  // *** The g.f. is assumed to be locked; no one else is permitted
  // *** to be mucking around with the table.
  let n :: <integer> = %ckd-size(d);
  local method loop (i :: <integer>)
          if (i = n)
            let nd :: <class-keyed-discriminator> = grow-linear-class-keyed-discriminator(d);
            ckd-add!(nd, key, value)
          elseif (pointer-id?(%ckd-ref(d, i), $ckd-empty))
            %ckd-ref(d, i + 1) := value;
            sequence-point();
            %ckd-ref(d, i) := key;
            d
          elseif (pointer-id?(%ckd-ref(d, i), key))
            %ckd-ref(d, i + 1) := value;
            d
          else
            loop (i + 2)
          end if
        end method;
  loop(0)
end function;

define function grounded-lckd-add!
    (d :: <linear-class-keyed-discriminator>, key, value)
 => (discriminator :: <object>);
  with-lckd-dispatch (d)
    lckd-add!(d, key, value);
  end with-lckd-dispatch;
end function;

// define engine-node-slot typecheck-discriminator-type <typecheck-discriminator> <type> engine-node-data-1;
// define engine-node-slot typecheck-discriminator-next <typecheck-discriminator> <object> engine-node-data-2;


define function %gf-dispatch-typecheck
    (arg, parent :: <dispatch-starter>, d :: <typecheck-discriminator>)
  parent;
  if (primitive-instance?(arg, typecheck-discriminator-type(d)))
    typecheck-discriminator-next(d)
  else
    $inapplicable-engine-node
  end if
end function;

define function make-typecheck-discriminator
    (argnum :: <integer>, gf :: <generic-function>, t :: <type>, next :: <object>)
 => (d :: <discriminator>)
//  let d :: <typecheck-discriminator>
//    = bootstrap-allocate-discriminator(engine-node$k-typecheck, argnum,
//                                       standard-discriminator-bits(gf));
//  typecheck-discriminator-type(d) := type;
//  typecheck-discriminator-next(d) := next;
//  primitive-initialize-discriminator(d);
//  d
  make-if-type-discriminator(argnum, gf, t, next, $inapplicable-engine-node)
end function;

define function make-monomorphic-by-class-discriminator
    (argnum :: <integer>, gf :: <generic-function> /* , ic :: <implementation-class>, next :: <object> */)
 => (d :: <discriminator>)
  let d :: <monomorphic-by-class-discriminator>
    = bootstrap-allocate-discriminator(engine-node$k-monomorphic-by-class, argnum,
                                       standard-discriminator-bits(gf));
  monomorphic-by-class-discriminator-key(d)  := $ckd-empty;
  // let key = iclass-unique-key(ic);
  // monomorphic-by-class-discriminator-key(d)  := key;
  // monomorphic-by-class-discriminator-next(d) := next;
  primitive-initialize-discriminator(d);
  d
end function;


// define engine-node-slot if-type-discriminator-type <if-type-discriminator> <type> engine-node-data-1;
// define engine-node-slot if-type-discriminator-then <if-type-discriminator> <object> engine-node-data-2;
// define engine-node-slot if-type-discriminator-else <if-type-discriminator> <object> engine-node-data-3;

define function %gf-dispatch-if-type
    (arg, parent :: <dispatch-starter>, disp :: <if-type-discriminator>)
  parent;
  if (primitive-instance?(arg, if-type-discriminator-type(disp)))
    if-type-discriminator-then(disp)
  else
    if-type-discriminator-else(disp)
  end if
end function;

define function make-if-type-discriminator
    (argnum :: <integer>, gf :: <generic-function>, type :: <type>, thend :: <object>, elsed :: <object>)
  let d :: <if-type-discriminator>
       = bootstrap-allocate-discriminator(engine-node$k-if-type, argnum,
                                          standard-discriminator-bits(gf));
  if-type-discriminator-type(d) := type;
  if-type-discriminator-then(d) := thend;
  if-type-discriminator-else(d) := elsed;
  primitive-initialize-discriminator(d);
  d
end function;

/* Singleton Dispatch */

//define engine-node-slot singleton-discriminator-table <singleton-discriminator>
//  <simple-object-vector>
//  engine-node-data-1;
//define engine-node-slot singleton-discriminator-default <singleton-discriminator>
//  <object>
//  engine-node-data-2;


define function make-linear-singleton-discriminator
    (entry-type :: <integer>, argnum :: <integer>, gf :: <generic-function>, keys :: <list>, nkeys :: <integer>)
 => (d :: <linear-singleton-discriminator>)
  let len :: <integer> = ash(nkeys, 1);
  let v :: <simple-object-vector>
       = make(<simple-object-vector>, size: len, fill: $absent-engine-node);
  let d :: <linear-singleton-discriminator>
       = bootstrap-allocate-discriminator(entry-type, argnum, standard-discriminator-bits(gf));
  singleton-discriminator-table(d) := v;
  singleton-discriminator-default(d) := $absent-engine-node;
  lsd-index(d)    := 0;
  local method loop(i :: <integer>, l :: <list>)
          unless (l == #())
            if (~(i < len)) // @@@@ (i >= len)
              error("fmh")
            else
              vector-element(v, i) := head(l);
              let nxt :: <list> = tail(l);
              loop(i + 2, nxt)
            end if
          end unless
        end method;
  loop(0, keys);
  primitive-initialize-discriminator(d);
  d
end function;


/*
define inline function immediate-linear-singleton-discriminator-element
    (d :: <linear-singleton-discriminator>, key, default) => (val :: <object>)
  let table :: <simple-object-vector> = singleton-discriminator-table(d);
  let n :: <integer> = size(table);
  local method loop (i :: <integer>)
          if (i = n)
            default
          else
            let k = vector-element(table, i);
            if (pointer-id?(k, key))
              vector-element(table, i + 1);
            else
              loop(i + 2)
            end if
          end if
        end method;
  loop(0)
end function;
*/

define inline function immediate-linear-singleton-discriminator-element
    (d :: <linear-singleton-discriminator>, key, default) => (val :: <object>)
  let table :: <simple-object-vector> = singleton-discriminator-table(d);
  let n :: <integer> = size(table);
  let firsti :: <integer> = lsd-index(d);
  let k = vector-element(table, firsti);
  if (pointer-id?(k, key))
    vector-element(table, firsti + 1)
  else
    local method loop (i :: <integer>)
            let i :: <integer> = i + 2;
            let i :: <integer> = if (i == n) 0 else i end;
            if (i == firsti)
              default
            else
              let k = vector-element(table, i);
              if (pointer-id?(k, key))
                lsd-index(d) := i;
                vector-element(table, i + 1)
              else
                loop(i)
              end if
            end if
          end method;
    loop(firsti)
  end if
end function;


/*
define inline function value-object-linear-singleton-discriminator-element
    (d :: <linear-singleton-discriminator>, key, default) => (val :: <object>)
  let table :: <simple-object-vector> = singleton-discriminator-table(d);
  let n :: <integer> = size(table);
  local method loop (i :: <integer>)
          if (i = n)
            default
          else
            let k = vector-element(table, i);
            if (k ~== $absent-engine-node & k = key)
              vector-element(table, i + 1);
            else
              loop(i + 2)
            end if
          end if
        end method;
  loop(0)
end function;
*/

define inline function value-object-linear-singleton-discriminator-element
    (d :: <linear-singleton-discriminator>, key, default) => (val :: <object>)
  let table :: <simple-object-vector> = singleton-discriminator-table(d);
  let n :: <integer> = size(table);
  let firsti :: <integer> = lsd-index(d);
  let k = vector-element(table, firsti);
  if (k = key)
    vector-element(table, firsti + 1)
  else
    local method loop (i :: <integer>)
            let i :: <integer> = i + 2;
            let i :: <integer> = if (i == n) 0 else i end;
            if (i == firsti)
              default
            else
              let k = vector-element(table, i);
              if (k ~== $absent-engine-node & k = key)
                lsd-index(d) := i;
                vector-element(table, i + 1)
              else
                loop(i)
              end if
            end if
          end method;
    loop(firsti)
  end if
end function;


define function linear-singleton-discriminator-element-setter
    (value, d :: <linear-singleton-discriminator>, key)
  let table :: <simple-object-vector> = singleton-discriminator-table(d);
  let n :: <integer> = size(table);
  local method loop (i :: <integer>)
          if (i == n)
            error("key not found")
          else
            let k = vector-element(table, i);
            if (k == key)
              lsd-index(d) := i;
              vector-element(table, i + 1) := value
            else
              loop(i + 2)
            end if
          end if
        end method;
  loop(0)
end function;

define function %gf-dispatch-immediate-linear-singleton
    (arg, parent :: <dispatch-starter>, d :: <immediate-linear-singleton-discriminator>)
  parent;
  immediate-linear-singleton-discriminator-element(d, arg, singleton-discriminator-default(d))
end function;


define function %gf-dispatch-value-object-linear-singleton
    (arg, parent :: <dispatch-starter>, d :: <immediate-linear-singleton-discriminator>)
  parent;
  value-object-linear-singleton-discriminator-element(d, arg, singleton-discriminator-default(d))
end function;


define function singleton-discriminator-element
    (d :: <singleton-discriminator>, key, default) => (val :: <object>)
  select (d by instance?)
    <immediate-linear-singleton-discriminator> =>
      immediate-linear-singleton-discriminator-element(d, key, default);
    <value-object-linear-singleton-discriminator> =>
      value-object-linear-singleton-discriminator-element(d, key, default);
  end select
end function;

define function singleton-discriminator-element-setter
    (value, d :: <singleton-discriminator>, key)
  select (d by instance?)
    <linear-singleton-discriminator> =>
      linear-singleton-discriminator-element-setter(value, d, key);
  end select
end function;


define function make-single-class-singleton-discriminator
    (keys :: <list>, argnum :: <integer>, gf :: <generic-function>)
 => (d :: <singleton-discriminator>);
  let n :: <integer> = size(keys);
  make-linear-singleton-discriminator
    (if (value-object?(head(keys)))
       engine-node$k-value-object-linear-singleton
     else
       engine-node$k-immediate-linear-singleton
     end if,
     argnum, gf, keys, n)
end function;


//// Handle-missed-dispatch


define primary class <dispatch-state> (<object>)
  slot %ds-gf :: <generic-function>;
  slot %ds-parent :: <dispatch-starter>;
  slot %ds-args :: <simple-object-vector>;
  slot %ds-argnum-set :: <argnum-set>;
  slot %ds-args-to-check-first :: <list> = #();
  slot %ds-headed-methods :: <pair>;
  slot %ds-cache = #f;
  slot %ds-result = #f;
  slot %ds-conditions = #();
  slot %ds-argtypes :: <simple-object-vector> = #[];
end class;


define inline function %ds-add-argnum (argnum :: <integer>, ds :: <dispatch-state>)
  add-argnum(argnum, %ds-argnum-set(ds))
end function;

define inline-only function %ds-argtype (ds :: <dispatch-state>, i :: <integer>)
  element(%ds-argtypes(ds), i, default: <object>)
end function;


define function dispinapplicable (ds :: <dispatch-state>)
  dispresult($inapplicable-engine-node, ds);
  $absent-engine-node
end function;



define function dispwarn (c :: <condition>, ds :: <dispatch-state>) => ()
  %ds-conditions(ds) := pair(c, %ds-conditions(ds))
end function;

define function dispresult (r, ds :: <dispatch-state>) => ()
  if (%ds-result(ds))
    error("Bug!  Multiple dispatch results?")
  else
    %ds-result(ds) := r
  end if;
end function;



define function handle-missed-dispatch (d :: <engine-node>,
                                        parent :: <dispatch-starter>,
                                        args :: <simple-object-vector>)
  /*
  iterate redefinition-check (i :: <integer> = size(args) - 1,
                              updated-p :: <boolean> = #f)
    if (i < 0)
      if (updated-p)
        apply(gf, args)
      else handle-missed-dispatch-1(d, gf, args)
      end if
    else redefinition-check(i - 1,
                            if (obsolete-instance?(element(args, i)))
                              element(args, i)
                                := update-obsolete-instance(element(args, i));
                              #t
                            else updated-p
                            end if)
    end if
  end iterate
   */
  handle-missed-dispatch-1(d, parent, args)
end function;


define variable *dispatch-miss-count* = 0;
define variable *dispatch-computation-count* = 0;

define function handle-missed-dispatch-1 (d :: <engine-node>,
                                          parent :: <dispatch-starter>,
                                          args :: <simple-object-vector>)
  let ds :: <dispatch-state> = system-allocate-simple-instance(<dispatch-state>);
  let parent :: <dispatch-starter> = if (d == $absent-engine-node) parent else d end;
  let gf :: <generic-function> = parent-gf(parent);
  %ds-parent(ds) := parent;
  %ds-gf(ds) := gf;
  %ds-args(ds) := args;
  %ds-argnum-set(ds) := make-argnum-set();
  %ds-args-to-check-first(ds) := #();
  %ds-conditions(ds) := #();
  %ds-result(ds) := #f;
  %ds-argtypes(ds) := #[];

//  *dispatch-miss-count* := wrapped-incr(*dispatch-miss-count*);
  let new-start-engine = (with-object-lock (gf)
                            let cache = %gf-cache(gf);
                            %ds-cache(ds) := cache | #f;
                            (~type-complete?(gf) & call-to-type-incomplete-generic(gf, args))
                             |
                             compute-dispatch-engine(ds)
                          end with-object-lock);

  bletch-stack(%ds-conditions(ds));
  let what = %ds-result(ds) | new-start-engine;
  if (what)
    if (instance?(what, <condition>))
      bletch(what)
    else
      %invoke-engine-node(what, parent, args)
    end if
  else
    dbg("Handle-missed-dispatch: reinvoking %=", gf);
    // show(gf);
    %invoke-generic-function-mepized(gf, args)
  end if
end function;
