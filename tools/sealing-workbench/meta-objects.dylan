module:   sealing-workbench
author:   Paul Haahr
synopsis: Object system and types for the sealing test world.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

////
//// the meta-classes for the meta-type system
////

define abstract class <&object> (<object>)
  slot object-&class, required-init-keyword: &class:;
  slot name, init-keyword: name:, init-value: #f;
  slot abstract-instance?, init-value: #f, init-keyword: abstract-instance?:;
  slot library,
    init-keyword: library:,
    init-function: method () *library* end;
end class <&object>;

define abstract class <&type> (<&object>)
end class <&type>;

define abstract class <&class> (<&type>)
  slot meta-class, init-keyword: meta-class:, init-value: <&object>;
  slot &all-superclasses;
  slot &direct-superclasses, required-init-keyword: superclasses:;
  slot &direct-subclasses, init-value: #();
  slot open?, init-keyword: open?:, init-value: #f;
  slot abstract?, init-keyword: abstract?:, init-value: #f;
  // slot primary?, init-keyword: primary?:;
  slot disjoint-from :: <object-table>,
    init-function: curry(make, <object-table>);
end class <&class>;


////
//// disjointness enforcement for sealing
////

define method disjointness-required?
    (class-1 :: <&class>, class-2 :: <&class>) => false-or-reason;
  // any?(rcurry(curry(element, class-1.disjoint-from), default: #f),
  //     class-2.&all-superclasses)
  element(class-1.disjoint-from, class-2, default: #f)
end method disjointness-required?;

define method require-disjointness
    (class-1 :: <&class>, class-2 :: <&class>, reason) => ()
  local method disjointify (from, to)
	  to.disjoint-from[from] :=
	    add!(element(to.disjoint-from, from, default: #()), reason);
	end method disjointify;
  disjointify(class-1, class-2);
  disjointify(class-2, class-1)
end method require-disjointness;


////
//// non-specific class precedence list computation
////

define method class-precedence-list (class)
  select (class.&direct-superclasses.size)
    0 =>
      unless (class.meta-class == <&object>)
	error("%= must have at least one superclass (try &<object>)", class)
      end unless;
      list(class);
    1 =>
      let parent-cpl = class.&direct-superclasses.first.&all-superclasses;
      if (member?(class, parent-cpl))
	error("%= can't be its own superclass", class)
      end if;
      pair(class, parent-cpl);
    otherwise =>
      compute-class-precedence-list(class);
  end select
end method class-precedence-list;

define method all-superclasses-unordered(class :: <&class>)
  add(reduce1(union,
	      map(&all-superclasses, class.&direct-superclasses)),
      class);
end method all-superclasses-unordered;

////
//// CLOS-derived class precedence list computation
////

define class <ordering-pair> (<object>)
  slot before, required-init-keyword: before:;
  slot after,  required-init-keyword: after:;
end class <ordering-pair>;

define method ordering-pair (x, y)
  make(<ordering-pair>, before: x, after: y)
end method ordering-pair;

define method \= (op-1 :: <ordering-pair>, op-2 :: <ordering-pair>)
  op-1.before == op-2.before
  & op-1.after == op-2.after
end method \=;

define method clos-class-precedence-list (class)
  let direct-superclasses = class.&direct-superclasses;
  let all-superclasses = class.all-superclasses-unordered;
  let orderings
    = remove-duplicates!
        (apply(concatenate,
	       map(method (super)
		     map(ordering-pair,
			 pair(super, super.&direct-superclasses),
			 super.&direct-superclasses)
		   end method,
		   all-superclasses)),
	 test: \=);
  local method first-unprecedented-member (s, r, reverse-partial-cpl)
	  let unprecedented-classes
	    = choose(method (c)
		       ~any?(method (op) op.after == c end, r)
		     end method,
		     s);
	  select (unprecedented-classes.size)
	    0 =>
	      #f;
	    1 =>
	      unprecedented-classes.first;
	    otherwise =>
	      block(return)
		for (c in reverse-partial-cpl)
		  for (n in unprecedented-classes)
		    if (member?(n, c.&direct-superclasses))
		      return(n)
		    end if
		  end for
		end for;
		error("couldn't find rightmost unprecedented class")
	      end block
	  end select
	end method first-unprecedented-member;
  local method ordering-before? (op :: <ordering-pair>, x)
	  op.before == x
	end method ordering-before?;
  block (exit)
    let n = #f;
    for (p = #() then pair(n, p),
	 s = all-superclasses then remove!(s, n),
	 r = orderings then remove!(r, n, test: ordering-before?))
      n := first-unprecedented-member(s, r, p);
      if (~n)
	if (s.empty?)
	  exit(reverse!(p))
	else
	  error("no partial ordering in superclasses %= of %=", s, class)
	end if
      end unless
    end for
  end block
end method clos-class-precedence-list;


////
//// L*Loops class precedence list computation
////

define class <graph> (<object>)
  slot vertices, required-init-keyword: vertices:;
  slot reversed-successors-table;
end class <graph>;

define method initialize (graph :: <graph>, #key, #all-keys) => ();
  graph.reversed-successors-table := make(<table>)
end method initialize;

define method successors (graph :: <graph>, vertex)
  reverse(element(graph.reversed-successors-table, vertex, default: #()))
end method succcessors;

define method successor? (graph :: <graph>, vertex, successor) => boolean;
  member?(successor, graph.reversed-successors-table[vertex])
end method successor?;

define method add-successor (graph :: <graph>, vertex, successor) => ();
  let rs = element(graph.reversed-successors-table, vertex, default: #());
  unless (member?(successor, rs))
    graph.reversed-successors-table[vertex] := pair(successor, rs)
  end unless;
end method add-successor;

define method print-graph (graph :: <graph>) => ();
  for (vertex in graph.vertices)
    format-out("successors(%=) : %=\n", vertex, successors(graph, vertex))
  end for
end method print-graph;

define method linearization-graph (class :: <&class>) => (graph :: <graph>);
  let direct-superclasses = class.&direct-superclasses;
  let all-superclasses = class.all-superclasses-unordered;
  let graph = make(<graph>, vertices: all-superclasses);
  for (Ci in direct-superclasses)
    for (current-class in Ci.&all-superclasses,
	 previous-class = class then current-class)
      add-successor(graph, previous-class, current-class)
    end for;
  end for;
  graph
end method linearization-graph;

define method step (graph :: <graph>, class :: <&class>, subs) => cpl;
  for (y in successors(graph, class))
    // format-out("; from %= decrementing subs[%=]\n", class, y);
    subs[y] := subs[y] - 1
  end for;
  for (y in successors(graph, class),
       cpl = list(class)
	 then if (zero?(subs[y]))
		concatenate(cpl, step(graph, y, subs))
	      else
		cpl
	      end if)
  finally
    cpl
  end for
end method step;

/* needed because lispworks trace doesn't get the right print methods
define variable *depth* = 0;
define method debug-step (graph :: <graph>, class :: <&class>, subs) => cpl;
  for (i from 0 below *depth*)
    format-out("  ")
  end for;
  format-out("step %d <- %= %=\n", *depth*, class, subs);

  *depth* := *depth* + 1;
  let cpl = step(graph, class, subs);
  *depth* := *depth* - 1;

  for (i from 0 below *depth*)
    format-out("  ")
  end for;
  format-out("step %d -> %=\n", *depth*, cpl);

  cpl
end;
*/

define method loops (class :: <&class>, graph :: <graph>) => cpl;
  let subs = begin
	       let subs = make(<table>);
               for (class in graph.vertices)
                 subs[class] := 0;
               end for;
	       for (successor-list in graph.reversed-successors-table)
		 for (succ in successor-list)
		   subs[succ] := subs[succ] + 1
		 end for;
	       finally
		 subs
	       end for
	     end;
  step(graph, class, subs)
end method loops;

define method lloops-class-precedence-list (class :: <&class>) => cpl;
  let graph = linearization-graph(class);
  // print-graph(graph);
  let cpl = loops(class, graph);
  let circularities = choose(complement(rcurry(member?, cpl)),
                             class.all-superclasses-unordered);
  unless (empty?(circularities))
    error("circular or non-monotonic definition of class %= "
          "among superclasses %=", class, circularities)
  end unless;
  cpl
end method lloops-class-precedence-list;


////
//// class initialization
////

define variable compute-class-precedence-list :: <function>
  = clos-class-precedence-list;

define method initialize(&class :: <&class>, #rest keys, #key, #all-keys)
  next-method();

  local method sealing-violation? (super)
          ~super.open? & super.library ~== &class.library
        end method sealing-violation?;
  if (any?(sealing-violation?, &class.&direct-superclasses))
    error("%= violates sealing on %=", &class,
          choose(sealing-violation?, &class.&direct-superclasses))
  end if;

  local method disjointness-violation? (super)
          any?(curry(disjointness-required?, super),
               &class.&direct-superclasses)
        end method disjointness-violation?;
  if (any?(disjointness-violation?, &class.&direct-superclasses))
    let reasons = map(disjointness-violation?, &class.&direct-superclasses);
    let messages = #f;
    for (reason in reasons, problem in &class.&direct-superclasses)
      if (reason)
        let message = format-to-string("%= because of %=", problem, reason);
        messages := if (messages)
                      concatenate(messages, ", ", message)
                    else
                      message
                    end if
      end if
    end for;
    error("%= violates required disjointness: %s", &class, messages)
  end if;

  // require disjointness against all classes the parents are disjoint from
  for (super in &class.&direct-superclasses)
    for (disjoint in super.disjoint-from.key-sequence)
      require-disjointness(&class, disjoint, super.disjoint-from[disjoint])
    end for
  end for;

  if (subtype?(&class.meta-class, <&object>))
    let proper-meta-class? = curry(subtype?, &class.meta-class);
    unless (every?(proper-meta-class?,
                   map(meta-class, &class.&direct-superclasses)))
      error("meta-class %= not a supertype of %=",
            &class.meta-class,
            choose(complement(proper-meta-class?),
                   map(meta-class, &class.&direct-superclasses)));
    end unless;
  end if;

  &class.&all-superclasses
    := class-precedence-list(&class);

  for (super in &class.&direct-superclasses)
    super.&direct-subclasses := add!(super.&direct-subclasses, &class)
  end for;
end method initialize;


////
//// the instance object protocol
////

define generic &make(&class :: <&type>, #rest args, #key, #all-keys)
 => (object :: <&object>);

define method &make(&class :: <&class>, #rest args,
		    #key abstract-instance?, #all-keys)
  if (&class.abstract? & ~abstract-instance?)
    error("&make on abstract class %=", &class)
  end if;
  apply(make, &class.meta-class, &class: &class, args)
end method &make;

define generic &subtype? (type-1 :: <&type>, type-2 :: <&type>) => boolean;

define method &subtype? (type-1 :: <&type>, type-2 :: <&type>)
  type-1 == type-2
end method &subtype?;

define method &subtype? (class-1 :: <&class>, class-2 :: <&class>)
  member?(class-2, class-1.&all-superclasses)
end method &subtype?;

define generic &instance? (&object, type :: <&type>) => boolean;

define method &instance? (&object, &class :: <&class>)
  &subtype?(&object.object-&class, &class)
end method &instance?;

define method \= (type-1 :: <&type>, type-2 :: <&type>)
  subtype?(type-1, type-2) & subtype?(type-2, type-1)
end method \=;

define method &all-subclasses (class :: <&class>)
  let direct-subclasses = class.&direct-subclasses;
  if (direct-subclasses.empty?)
    list(class)
  else
    add(reduce1(union, map(&all-subclasses, direct-subclasses)), class)
  end if
end method;


////
//// booting
////

define constant (&<object>, &<type>, &<class>)
  = begin
      local method boot-class(meta-class, name, open?, superclasses)
              make(<&class>, meta-class: meta-class, &class: #f,
                   name: name, superclasses: superclasses,
                   open?: open?, library: $dylan-library)
            end method boot-class;
      let object = boot-class(<&object>, #"&<object>", #t, #());
      let type   = boot-class(<&type>,   #"&<type>",   #f, list(object));
      let class  = boot-class(<&class>,  #"&<class>",  #f, list(type));
      object.object-&class := class;
      type  .object-&class := class;
      class .object-&class := class;
      values(object, type, class)
    end;


////
//// type meta-objects
////

/// singletons

define class <&singleton> (<&type>)
  slot singleton-&object, required-init-keyword: &object:;
end class <&singleton>;

define &class &<singleton> = <&singleton> (&<type>);

define method &singleton (&object)
  &make(&<singleton>, &object: &object)
end method &singleton;

define method &subtype? (type-1 :: <&singleton>, type-2 :: <&type>)
  &instance?(type-1.singleton-&object, type-2)
end method subtype?;

define method &instance? (object, singleton :: <&singleton>)
  object == singleton.singleton-&object
end method instance?;

define method &all-superclasses (singleton :: <&singleton>)
  singleton.singleton-&object.object-&class.&all-superclasses
end method &all-superclasses;

// TODO: union
// TODO: limited integers
// TODO: limited collection
// TODO: subtype
