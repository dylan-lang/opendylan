Module: dfmc-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///////////////////////////////////////////////////////////////////////
///
///                 COMPILE TIME TYPES
///
/// We add some additional types for the compiler. These don`t exit in 
/// the runtime.
///
///////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////
///
///                                  CONSTANTS
///
/// It would be nice if these could actually be constants - so it would 
/// make sense to in line the functions.
///


define variable *false-type* :: false-or(<&type>) = #f;

define inline function typist-<false-type>() => (t :: <&type>)
  *false-type* | (*false-type* := ^singleton(#f));
end;

define variable *true-type* :: false-or(<&type>) = #f;

define inline function typist-<true-type>() => (t :: <&type>)
  *true-type* | (*true-type* := ^singleton(#t));
end;

define class <unknown-type> (<&type>) end;

define method print-object(o :: <unknown-type>, s :: <stream>) => ()
  format(s, "** unknown type **");
end;

define variable *unknown-type* :: false-or(<unknown-type>) = #f;

define inline function typist-<unknown-type>() =>(t :: <unknown-type>)
  *unknown-type* | 
  ( without-dependency-tracking
      *unknown-type* := make(<unknown-type>)
    end)
end;

define method ^subtype? 
  (t1 :: <unknown-type>, t2 :: <&type>) => (r :: <boolean>)
  #t;
end;

define method ^subtype? 
  (t1 :: <&type>, t2 :: <unknown-type>) => (r :: <boolean>)
  #f;
end;

define method ^subtype? 
  (t1 :: <unknown-type>, t2 :: <unknown-type>) => (r :: <boolean>)
  #t;
end;

define method ^instance? (x :: <model-value>, c :: <unknown-type>) => (r :: <boolean>)
  #f;
end;

define method ^instance? (x :: <unknown-type>, c :: <unknown-type>) => (r :: <boolean>)
  #t;
end;

define method ^instance? (x :: <unknown-type>, c :: <&type>) => (r :: <boolean>)
  #f;
end;

define variable *object-type* :: false-or(<&type>) = #f;

define inline function typist-<object-type>() => (t :: <&type>)
  *object-type* | (*object-type* := dylan-value(#"<object>"));
end;

define variable *sov-type* = #f;

define inline function typist-sov-type() => (t :: <&type>)
  *sov-type* 
  | (*sov-type* := dylan-value(#"<simple-object-vector>"));
end;

define variable *rest-variable-type* :: false-or(<&type>) = #f;

define inline function typist-rest-variable-type() => (t :: <&type>)
  *rest-variable-type* |
   (*rest-variable-type* := typist-sov-type());
end;

define variable *values-rest-type* :: false-or(<&type>) = #f;

define inline function typist-values-rest-type() => (t :: <&type>)
  *values-rest-type* |
   (*values-rest-type* :=
     cached-type-union(typist-<false-type>(), typist-<object-type>()))
end; 

define variable *bottom-type* :: false-or(<&bottom-type>) = #f;

define inline function typist-<bottom-type>() => (b :: <&bottom-type>)
  *bottom-type* | (*bottom-type* := dylan-value(#"<bottom>"));
end;

define method ^type-equivalent?
  (t1 :: <&bottom-type>,  t2 :: <&type>) => (r :: <boolean>)
  #f;
end;

define method ^type-equivalent?
  (t1 :: <&type>, t2 :: <&bottom-type>) => (r :: <boolean>)
  #f;
end;

define method ^type-equivalent?
  (t1 :: <&bottom-type>, t2 :: <&bottom-type>) => (r :: <boolean>)
  #t;
end;
 
define method ^instance?
  (obj :: <model-value>, t2 :: <&bottom-type>) => (r :: <boolean>)
  #f;
end;

define method ^instance?
  (obj :: <&bottom-type> , t2 :: <&bottom-type>) => (r :: <boolean>)
  #t;
end;

define method ^instance?
  (obj :: <&bottom-type> , obj :: <&type>) => (r :: <boolean>)
  #f;
end;

// deal with VALUES types

//TODO: if we keep this give the slot a type
define class <values-type> (<&type>)
  slot fixed-types,
    required-init-keyword: types:;
  slot values-rest-type = #f,
    init-keyword: rest-type:;
end;

define method print-object (v :: <values-type> , s :: <stream>) => ()
  format(s, "#v[");
  for (x in v.fixed-types,
       first? = #t then #f)
    unless(first?)
      format(s, ", ");
    end;
    format(s, "%=", x);
  finally
    if (v.values-rest-type)
      if (v.fixed-types.size > 0)
        format(s, ", ");
      end;
      format(s, "#rest %=", v.values-rest-type);
    end;
  end;
  format(s, "]");
end;

define class <values-type-table> (<table>)
end class;

define function values-type-table-test (p1 :: <pair>, p2 :: <pair>)
  if (p1.tail)
    if (p2.tail)
      ^type-equivalent?(p1.tail, p2.tail) &
	simple-vector-type-key-test(p1.head, p2.head)
    else
      #f;
    end;
  elseif (p2.tail)
    #f;
  else
    simple-vector-type-key-test(p1.head, p2.head);
  end;
end;

define method table-protocol (t :: <values-type-table>) => (test :: <function>, hash :: <function>)
  values(values-type-table-test, values-type-table-hash);
end method;

define variable *cache-values-types?* = #f;

// need to watch the size of this cache!!!
define constant $values-type-cache = make(<values-type-table>);

define method make(vt :: subclass(<values-type>), 
		   #rest r, #key types, rest-type)
    => (v :: <values-type>)
  if (*cache-values-types?*)
    let key = pair(types, rest-type);
    (element($values-type-cache, key, default: #f)) |
      ($values-type-cache[key] := next-method())
  else
    next-method();
  end;
end;

// Deal with the algebra stuff

// It might actually be worth introducing a cache for this (and ^subtype?)
define method ^type-equivalent?
  (t1 :: <values-type>, t2 :: <values-type>) => (r :: <boolean>)
  values-type-equivalent?(t1, t2)
end;

define inline function values-type-equivalent?
  (t1 :: <values-type>, t2 :: <values-type>) => (r :: <boolean>)
  let rt1 = t1.values-rest-type;
  let rt2 = t2.values-rest-type;
  if (rt1)
    if (rt2)
      if (^type-equivalent?(rt1, rt2))
        let ft1 = t1.fixed-types;
        let ft2 = t2.fixed-types;
        if (ft1.size = ft2.size)
          block (exit)
            for (i from 0 below ft1.size)
              unless (^type-equivalent?(ft1[i], ft2[i]))
                exit(#f);
              end unless;
            end for;
            #t;
          end block;
        end if;
      end if;
    end if;
  elseif (rt2)
    #f;
  else
    let ft1 = t1.fixed-types;
    let ft2 = t2.fixed-types;
    if (ft1.size = ft2.size)
      block (exit)
        for (i from 0 below ft1.size)
          unless (^type-equivalent?(ft1[i], ft2[i]))
            exit(#f);
          end unless;
        end for;
        #t;
      end block;
    end if;
  end if;
end function;

define method ^type-equivalent?
  (t1 :: <values-type>, t2 :: <&type>) => (r :: <boolean>)
  #f;
end method;

define method ^type-equivalent?
  (t1 :: <&type>, t2 :: <values-type>) => (r :: <boolean>)
  #f;
end method;

// a <values-type> can't be a subtype of anything other than a <values-type>
define method ^subtype? 
  (t1 :: <values-type>, t2 :: <values-type>) => (r :: <boolean>)
  values-subtype?(t1, t2)
end;

define inline function values-subtype? 
  (t1 :: <values-type>, t2 :: <values-type>) => (r :: <boolean>)
  let ft1 = t1.fixed-types;
  let ft2 = t2.fixed-types;
  let rt1 = t1.values-rest-type;
  let rt2 = t2.values-rest-type;
  let limit = min(ft1.size, ft2.size);
  block (exit)
    for (i from 0 below limit)
      unless (^subtype?(ft1[i], ft2[i]))
        exit(#f);
      end unless;
    end for;
    if (ft1.size = ft2.size)
      if (rt1)
        if (rt2)
          ^subtype?(rt1, rt2);
        else
          // the remaining values are discarded in all operations where subtype-ness matters
          #t;
        end;
      elseif (rt2)
        ^subtype?(typist-<false-type>(), rt2);
      else
        #t;
      end;
    elseif (ft2.size > limit)
      let rt1 = (rt1 | typist-<false-type>());
      for (i from limit below ft2.size)
        unless (^subtype?(rt1, ft2[i]))
          exit(#f);
        end unless;
      end for;
      if (rt2)
        ^subtype?(rt1, rt2);
      else
        #t;
      end;
    else
      if (rt2)
        for (i from limit below ft1.size)
          unless (^subtype?(ft1[i], rt2))
            exit(#f);
          end unless;
        end for;
        if (rt1)
          ^subtype?(rt1, rt2);
        else
          #t;
        end;
      else
        let ot = typist-<object-type>(); 
        // Damn raw types ...
        for (i from limit below ft1.size)
          unless (^subtype?(ft1[i], ot))
            exit(#f);
          end unless;
	end;
        #t;
      end;
    end if;
  end block;
end;

define method ^subtype?
  (t1 :: <values-type>, t2 :: <&type>) => (r :: <boolean>)
  #f;
end method;

define method ^subtype?
  (t1 :: <&type>, t2 :: <values-type>) => (r :: <boolean>)
  #f;
end method;

define method ^subtype?
  (t1 :: <&bottom-type>, t2 :: <values-type>) => (r :: <boolean>)
  #t;
end method;

define method ^subtype?
  (t1 :: <unknown-type>, t2 :: <values-type>) => (r :: <boolean>)
  #t;
end method;


// We want <rest-values-type> to behave like typist-sov-type() to the
// unsuspecting...

define class <rest-values-type> (<values-type>)
end;

define method print-object (v :: <rest-values-type> , s :: <stream>) => ()
  format(s, "vector[");
  for (x in v.fixed-types,
       first? = #t then #f)
    unless(first?)
      format(s, ", ");
    end;
    format(s, "%=", x);
  finally
    if (v.values-rest-type)
      if (v.fixed-types.size > 0)
        format(s, ", ");
      end;
      format(s, "#rest %=", v.values-rest-type);
    end;
  end;
  format(s, "]");
end;


define method ^type-equivalent?
  (t1 :: <rest-values-type>, t2 :: <&type>) => (r :: <boolean>)
  ^type-equivalent?(typist-sov-type(), t2)
end method;

define method ^type-equivalent?
  (t1 :: <&type>, t2 :: <rest-values-type>) => (r :: <boolean>)
  ^type-equivalent?(t1, typist-sov-type())
end method;

define method ^type-equivalent?
  (t1 :: <rest-values-type>, t2 :: <rest-values-type>) => (r :: <boolean>)
  values-type-equivalent?(t1, t2)
end method;


define method ^subtype?
  (t1 :: <rest-values-type>, t2 :: <&type>) => (r :: <boolean>)
  ^subtype?(typist-sov-type(), t2)
end method;

define method ^subtype?
  (t1 :: <&type>, t2 :: <rest-values-type>) => (r :: <boolean>)
  ^subtype?(t1, typist-sov-type())
end method;

define method ^subtype? 
  (t1 :: <rest-values-type>, t2 :: <rest-values-type>) => (r :: <boolean>)
  values-subtype?(t1, t2)
end;

define method ^instance? (x :: <model-value>, c :: <rest-values-type>) => (r :: <boolean>)
  ^instance?(x, typist-sov-type())
end;







define generic maybe-make-inferred-union 
    (c, t1 :: <&type>, t2 :: <&type>) => (r :: <&type>);

define method maybe-make-inferred-union 
  (c, t1 :: <unknown-type> , t2 :: <&type>) 
 => (r :: <&type>)
  t2;
end;

define method maybe-make-inferred-union 
  (c, t1 :: <&type>  , t2 :: <unknown-type>) => (r :: <&type>)
  t1;
end;

define method maybe-make-inferred-union 
  (c, t1 :: <unknown-type>  , t2 :: <unknown-type>) => (r :: <&type>)
  t1;
end;

define method maybe-make-inferred-union 
  (c, t1 :: <&bottom-type> , t2 :: <&type>) => (r :: <&type>)
  t2;
end;

define method maybe-make-inferred-union 
  (c, t1 :: <&type>  , t2 :: <&bottom-type>) => (r :: <&type>)
  t1;
end;

define method maybe-make-inferred-union 
  (c, t1 :: <&bottom-type> , t2 :: <values-type>) => (r :: <&type>)
  t2;
end;

define method maybe-make-inferred-union 
  (c, t1 :: <values-type>  , t2 :: <&bottom-type>) => (r :: <&type>)
  t1;
end;

define method maybe-make-inferred-union 
  (c, t1 :: <&bottom-type>  , t2 :: <&bottom-type>) => (r :: <&type>)
  t1;
end;

define method maybe-make-inferred-union 
  (c, t1 :: <values-type>  , t2 :: <&type>) => (r :: <&type>)
  let ft1 = unless (empty?(t1.fixed-types)) t1.fixed-types[0] end;
  let fixed = if (ft1)
                local method make-false-or (t)
                        cached-type-union(t, typist-<false-type>());
                      end;
                let f = map-as(<simple-object-vector>,
                               make-false-or,
                               t1.fixed-types);
                f[0] := cached-type-union(ft1, t2);
                f;
              elseif (t1.values-rest-type)
                vector(cached-type-union(t1.values-rest-type, t2));
              else
                vector(t2);
              end;
  make(<values-type>, 
       types: fixed,
       rest-type: t1.values-rest-type);
end;

define method maybe-make-inferred-union 
  (c, t1 :: <&type>  , t2 :: <values-type>) => (r :: <&type>)
  maybe-make-inferred-union(c, t2, t1);
end;

define method maybe-make-inferred-union 
  (c, t1 :: <values-type>  , t2 :: <values-type>) => (r :: <&type>)
  make-inferred-values-union(c, t2, t1);
end;

define method maybe-make-inferred-union 
  (c, t1 :: <&type>  , t2 :: <&type>) => (r :: <&type>)
  if (^subtype?(t1, t2))
    t2;
  elseif (^subtype?(t2, t1))
    t1;
  else
    cached-type-union(t1, t2);
  end;
end;

define method make-inferred-union
  (t1 :: <&type>, t2 :: <&type>) => (r :: <&union>)
  cached-type-union(t1, t2);
end;

define method make-inferred-union
  (t1 :: <&union>, t2 :: <&type>) => (r :: <&union>)
  cached-type-union(t1, t2);
end;

define method make-inferred-union
  (t1 :: <&type>, t2 :: <&union>) => (r :: <&union>)
  cached-type-union(t1, t2);
end;

define method make-inferred-union
  (t1 :: <&union>, t2 :: <&union>) => (r :: <&union>)
  cached-type-union(t1, t2);
end;

define method make-inferred-union
  (v1 :: <values-type>, v2 :: <values-type>) => (v3 :: <values-type>)
  error("shouldn`t");
end;

define method make-inferred-values-union
  (c, v1 :: <values-type>, v2 :: <values-type>) => (v3 :: <values-type>)
  let size1 = v1.fixed-types.size;
  let size2 = v2.fixed-types.size;
  let max-size = max(size1, size2);
  let fixed = make(<simple-object-vector>, size: max-size);
  local method get-type(i, max, which)
	  if (i < max)
	    which.fixed-types[i];
	  else
	    which.values-rest-type | typist-<false-type>();
	  end;
	end;
  for (i from 0 below max-size)
    fixed[i] :=  cached-type-union(get-type(i, size1, v1), 
                                   get-type(i, size2, v2));
  end;
  let rest-type = if (v1.values-rest-type & v2.values-rest-type)
                    cached-type-union(v1.values-rest-type, v2.values-rest-type);
                  else 
                    (v1.values-rest-type | v2.values-rest-type);
                  end;
  make(<values-type>, types: fixed, rest-type: rest-type);
end;

define variable *singleton-cache* = make(<table>);

define method cached-singleton (object :: <model-value>)
  make(<&singleton>, object: object)
end;

define method cached-singleton (obj :: <boolean>)
  element(*singleton-cache*, obj, default: #f)
  | (element(*singleton-cache*, obj) := next-method())
end;

define method cached-singleton (obj :: <list>)
  element(*singleton-cache*, obj, default: #f)
  | (element(*singleton-cache*, obj) := next-method())
end;

define constant $%%%an-empty-list = #();

define method cached-singleton (obj :: <empty-list>)
  element(*singleton-cache*, $%%%an-empty-list, default: #f)
  | (element(*singleton-cache*, $%%%an-empty-list) := next-method())
end;

define method cached-singleton (obj :: <&function>)
  element(*singleton-cache*, obj, default: #f)
  | (element(*singleton-cache*, obj) := next-method())
end;

define method cached-singleton (obj :: <integer>)
  element(*singleton-cache*, obj, default: #f)
  | (element(*singleton-cache*, obj) := next-method())
end;

define method cached-singleton (obj :: <symbol>)
  element(*singleton-cache*, obj, default: #f)
  | (element(*singleton-cache*, obj) := next-method())
end;

define method cached-singleton (obj :: <character>)
  element(*singleton-cache*, obj, default: #f)
  | (element(*singleton-cache*, obj) := next-method())
end;

define variable *canonical-unbound-value* = #f;

define method cached-singleton (obj :: <&unbound>)
  unless (*canonical-unbound-value*)
    *canonical-unbound-value* := obj;
  end;
  element(*singleton-cache*, *canonical-unbound-value*, default: #f)
  | (element(*singleton-cache*, *canonical-unbound-value*) := next-method())
end;

define variable *canonical-mapped-unbound-value* = #f;

define method cached-singleton (obj :: <mapped-unbound>)
  unless (*canonical-mapped-unbound-value*)
    *canonical-mapped-unbound-value* := obj;
  end;
  element(*singleton-cache*, *canonical-mapped-unbound-value*, default: #f)
  | (element(*singleton-cache*, *canonical-mapped-unbound-value*) := next-method())
end;

define constant $%%%an-empty-vector = #[];

define method cached-singleton (obj :: <vector>)
  if (obj.size = 0)
    element(*singleton-cache*, $%%%an-empty-vector, default: #f)
    | (element(*singleton-cache*, $%%%an-empty-vector) := next-method())
  else
    next-method();
  end;
end;

define variable *raw-singleton-cache* = make(<table>);

define method cached-singleton (obj :: <&raw-integer>)
  element(*raw-singleton-cache*, obj.^raw-object-value, default: #f)
  | (element(*raw-singleton-cache*, obj.^raw-object-value) := next-method())
end;

define method cached-singleton (obj :: <&raw-boolean>)
  element(*raw-singleton-cache*, obj.^raw-object-value, default: #f)
  | (element(*raw-singleton-cache*, obj.^raw-object-value) := next-method())
end;

define method cached-singleton (obj :: <&class>)
  element(*singleton-cache*, obj, default: #f)
  | (element(*singleton-cache*, obj) := next-method())
end;

define method cached-singleton (obj :: <&union>)
  element(*singleton-cache*, obj, default: #f)
  | (element(*singleton-cache*, obj) := next-method())
end;


define method reset-type-caches()
  format-out("\nReset type caches [%=, %=, %=]....\n", 
    *singleton-cache*.size, *raw-singleton-cache*.size,
    *union-cache*.size);
  *singleton-cache* := make(<table>);
  *union-cache* := make(<simple-vector-type-key-table>);
  *raw-singleton-cache* := make(<table>);
end;

define method reset-basic-types()
  *object-type* := #f;
  *values-rest-type* := #f;
  *false-type* := #f;
  *true-type* := #f;
  *sov-type* := #f;
  *rest-variable-type* := #f;
end;
