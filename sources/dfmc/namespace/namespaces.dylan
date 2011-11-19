Module:   dfmc-namespace
Synopsis: Namespaces.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Namespace clauses.

// Instances of these classes are constructed by the converters. They
// provide convenient parsed representations for each clause of a
// namespace.

define class <use-clause> (<dood-dfmc-object>)
  constant slot used-name,
    required-init-keyword: use:;
  constant slot options = #(),
    init-keyword: options:;
end class <use-clause>;

define abstract class <export-or-create-clause> (<dood-dfmc-object>)
  constant slot names,
    required-init-keyword: names:;
end class;

define class <create-clause> (<export-or-create-clause>)
end class <create-clause>;

define class <export-clause> (<export-or-create-clause>)
end class <export-clause>;

//// Namespaces.

// <Namespace> is an abstract class providing a partial implementation
// of namespaces, capturing the commonality in this area between Dylan's
// libraries and modules. Namespaces are considered to be environments
// which inherit names through filtering and have a distingushed
// interface for clients, extending the environment protocol. The
// environment protocol itself is used to lookup names from "within"
// the namespace.

// When a namespace is created, all its imports are collected and
// consistency checking is performed. This information is then discarded
// and bindings looked up and cached lazily from then on. The only
// motivation behind this is to try to keep the size of binding tables
// down - duplicating the binding table for all the exports of some
// framework plus Dylan in every module could prove expensive. If this
// turns out to be a false economy we can simply keep around all the
// information we compute at checking time.

//// Caching.

// One big lose in the way things are done structurally, particularly
// during the checking phases, is that an exported name collection is
// returned for a used namespace, and then each of those names is
// looked up for its value in turn. Using a once-computed table or
// sequence with the name/value relationship intact so that you can
// iterate over both together would save a lot of time.

// The <namespace> class is subclassed, but only within this library.

define abstract dood-class <namespace>
    (<dood-mapped-and-owned-object>, <environment>)
  slot debug-name = #f,
    init-keyword: debug-name:;
  // Names defined locally.
  lazy constant slot namespace-local-bindings :: <dood-lazy-symbol-table>
    = make(<dood-lazy-symbol-table>);
  // Referenced names imported from used namespaces.  This is used by
  // namespace redefinition to know which names were referenced, so it's
  // not really a cache in the sense that it can't be arbitrarily cleared.
  lazy constant slot imported-name-cache :: <table> = make(<table>),
    init-keyword: internal-imports:;
  // Set of all external names (exported or created)
  lazy slot exported-names :: <set> = make(<set>),
    init-keyword: exported-names:;
  // runtime model of this namespace
  lazy slot namespace-model = #f,
    init-keyword: model:;
  // Mangling Cache: used in back-end
  slot emitted-name = #f;
end;

define abstract dood-class <full-namespace> (<namespace>)
  lazy slot namespace-definition,
    init-keyword: definition:;
  // Information duplicated in the definition.
  lazy slot use-clauses = #[],
    init-keyword: use-clauses:;
  lazy slot create-clauses = #[],
    init-keyword: create-clauses:;
  // Set of those external names which were created.
  lazy slot created-names :: <set> = make(<set>);
  lazy slot export-clauses = #[],
    init-keyword: export-clauses:;
  // Derived information: use filters, the set of exported names.
  weak slot cached-uses = #f,
    reinit-expression: #f;
  // The collection of imported exported values is computed on demand,
  // which occurs whenever this namespace is used by another namespace.
  lazy slot cached-exported-imports-table :: false-or(<table>) = #f;
  weak slot all-used-namespaces-cache = #f,
    reinit-expression: #f;
end dood-class <full-namespace>;

// Virtual slots
define inline method namespace-original-library
    (space :: <full-namespace>) => (ld :: false-or(<library-description>))
  form-original-library(namespace-definition(space));
end method;

define inline method namespace-library-description
    (space :: <full-namespace>) => (ld :: <library-description>)
  form-library(namespace-definition(space));
end method;

define compiler-open generic namespace-name (n :: <namespace>)
  => name :: <symbol>;

define method strip-incremental-slots (x :: <namespace>)
  remove-all-keys!(imported-name-cache(x));
end method;

// These protocl functions support module-level macro hygiene.

// Lookup the value of an implicitly-exported name. An error is signaled
// if the name is not implicitly exported (<name-not-implicitly-exported>)
// unless a default value is supplied, in which case that value is
// returned.

define generic lookup-implicitly-exported-name
    (space :: <namespace>, name :: <name>, #key) => (value);

// Return a collection containing all the names implicitly exported by
// the namespace (i.e. accessible through lookup-implicitly-exported-name).

define generic implicitly-exported-name-collection
    (space :: <namespace>) => (name-sequence);

// Return a collection containing all the values of the names implicitly
// exported by the namespace (i.e. accessible through
// lookup-implicitly-exported-name).

define generic implicitly-exported-value-collection
    (space :: <namespace>) => (name-sequence);

//// Methods on these generic functions should be filled-in by
//// superclasses.

define generic created-name-value
    (space :: <namespace>, name :: <name>) => (value);

define generic exported-name-value
    (space :: <namespace>, name :: <name>) => (value);

define generic resolve-used-namespace
    (space :: <namespace>, space-designator, #key) => (namespace);

// Called when a namespace is defined, with a table of all imported
// names and their bindings.
define generic note-namespace-imports
    (space :: <namespace>, imports :: <table>);

define method note-namespace-imports (space :: <namespace>, imports :: <table>)
  // The default method does nothing.
end method;

//// Utility functions.

define generic all-used-namespaces
    (space :: <namespace>) => (namespaces);

//// Implementation.

define function uses (space :: <full-namespace>)
  space.cached-uses |
    (space.cached-uses
       := map(method(clause)
                apply(make, <filter>,
                      namespace: clause.used-name,
                      clause: clause,
                      clause.options)
              end,
              space.use-clauses))
end function;

define function compute-imports (space :: <full-namespace>)
  let imports :: <table> = make(<table>);
  collecting (resolved-uses)
    for (filter in space.uses)
      let resolved? = update-imports(space, filter, imports);
      if (resolved?)
        collect-into(resolved-uses, filter);
      end;
    end;
    // Zap anything that isn't really there to prevent breakages later.
    space.cached-uses := collected(resolved-uses);
  end;
  imports
end function;

define function update-exports! (space :: <full-namespace>, imports :: <table>)
  local method do-clause (clause :: <export-or-create-clause>, kind)
          for (name in clause.names)
            let value = element(imports, name, default: not-found());
            if (found?(value))
              note(select (kind)
                     #"created" =>  <create-import-conflict>;
                     #"exported" => <export-import-conflict>;
                   end,
                   source-location:
                     form-source-location(namespace-definition(space)),
                   namespace: space,
                   clause: clause,
                   name: name,
                   import: value);
              // Note that name has been referenced.
              define-name-in-cache(space.imported-name-cache, name, value);
              // TODO: Should re-exported instead, but there's no easy way to
              // do that (it's hidden in filters).
            else
              if (~defined-name?(space, name))
                let value = select (kind)
                              #"created" =>  created-name-value(space, name);
                              #"exported" => exported-name-value(space, name);
                            end;
                define-name(space, name, value);
              end;
              export-name(space, name, kind);
            end if;
          end for;
        end method;
  do(method (clause) do-clause(clause, #"exported") end, space.export-clauses);
  do(method (clause) do-clause(clause, #"created") end, space.create-clauses);
end function;

define method make-namespace (class :: subclass(<full-namespace>), #rest initargs,
                              #key, #all-keys)
 => (space :: <full-namespace>)
  let space :: <full-namespace> = apply(make, class, initargs);
  let imports = compute-imports(space);
  note-namespace-imports(space, imports);
  update-exports!(space, imports);
  space
end method;

define function export-name (space :: <full-namespace>, name, kind) => ()
  add!(space.exported-names, name);
  if (kind == #"created")
    add!(space.created-names, name);
  end;
end function;

define function created-name? (space :: <full-namespace>, name) => (well? :: <boolean>)
  member?(name, space.created-names)
end;

define function exported-name? (space :: <namespace>, name) => (value :: <boolean>)
  member?(name, space.exported-names)
end function;

define method name-definition (space :: <namespace>,
                               name, #key default = unsupplied())
  if (supplied?(default))
    element(space.namespace-local-bindings, name, default: default)
  else
    element(space.namespace-local-bindings, name)
  end
end method;

define function defined-name? (space :: <namespace>, name) => (value)
  found?(name-definition(space, name, default: not-found()))
end function;

// Caller is responsible for checking for imported names.
define method define-name (space :: <full-namespace>, name :: <name>, value) => ()
  debug-assert(~defined-name?(space, name),
               "Unexpected define of defined name");
  undefine-name-in-caches(space, name);
  space.namespace-local-bindings[name] := value;
end method;

define method undefine-name (space :: <full-namespace>, name :: <name>) => ()
  debug-assert(defined-name?(space, name),
               "Unexpected undefine of undefined name");
  undefine-name-in-caches(space, name);
  remove-key!(space.namespace-local-bindings, name);
end method;

define inline function lookup-name-in-cache
    (cache :: <mutable-explicit-key-collection>, name) => (cached-value)
  element(cache, name, default: not-found());
end function;

define inline function define-name-in-cache
    (cache :: <mutable-explicit-key-collection>, name, value) => (value)
  element(cache, name) := value;
end function;

define function undefine-name-in-caches (space :: <namespace>, name) => ()
  remove-key!(space.imported-name-cache, name);
end function;

define method lookup-imported-name (space :: <full-namespace>, name :: <name>)
  block (return)
    for (use in space.uses)
      let used-name = unfilter-name(use, name);
      if (used-name)
        let used-space
          = resolve-used-namespace(space, use.namespace, default: #f);
        if (used-space)
          let value = lookup-exported-name(used-space, used-name,
                                           default: not-found());
          if (found?(value)) return(value) end;
        end if;
      end;
    end for;
    not-found()
  end block;
end;

define function lookup-exported-name
    (space :: <namespace>, name :: <name>, #key default = unsupplied())
      => (value)
  if (exported-name?(space, name))
    name-definition(space, name)
  elseif (supplied?(default))
    element(space.exported-imports-table, name, default: default);
  else
    element(space.exported-imports-table, name)
  end;
end function;

define constant $name-not-imported = #"name-not-imported";

define method lookup-name
    (space :: <full-namespace>, name :: <name>,
     #key default = unsupplied(), exported? = #f) => (value)
  block (return)
    if (exported?)
      return(lookup-exported-name(space, name, default: default))
    end;
    let defined-value = name-definition(space, name, default: not-found());
    if (found?(defined-value)) return(defined-value) end;
    if (space.cached-exported-imports-table)
      let reexported-value
        = lookup-name-in-cache(space.cached-exported-imports-table, name);
      if (found?(reexported-value)) return(reexported-value) end;
    end;
    let cached-value = lookup-name-in-cache(space.imported-name-cache, name);
    if (found?(cached-value))
      if (cached-value == $name-not-imported)
        if (supplied?(default)) return(default) end;
      else
        return(cached-value)
      end;
    end;
    let imported-value = lookup-imported-name(space, name);
    define-name-in-cache(space.imported-name-cache, name,
                         if (found?(imported-value)) imported-value
                         else $name-not-imported end);
    case
      found?(imported-value) => imported-value;
      supplied?(default)     => default;
      otherwise              => error("The name %= is not found in %=.", name, space);
    end;
  end block;
end method;

define function exported-imports-table
    (space :: <namespace>) => (table :: <table>);
  space.cached-exported-imports-table | make-exported-imports-table(space)
end function;

define function make-exported-imports-table
    (space :: <full-namespace>) => (table :: <table>)
  let table = make(<table>);
  space.cached-exported-imports-table := table;
  let cache = space.imported-name-cache;
  local method do-exported-names (exported-names, value)
	  for (local-name in exported-names)
	    table[local-name] := value;
	    remove-key!(cache, local-name);
	  end;
	end method;
  for (use in space.uses)
    let used-space = resolve-used-namespace(space, use.namespace);
    for (original-name in used-space.exported-names)
      let exported-names = filter-exported-name(use, original-name);
      unless (empty?(exported-names))
	do-exported-names(exported-names, name-definition(used-space, original-name));
      end unless;
    end;
    for (value keyed-by original-name in used-space.exported-imports-table)
      let exported-names = filter-exported-name(use, original-name);
      unless (empty?(exported-names))
	do-exported-names(exported-names, value);
      end unless;
    end;
  end;
  table
end function;

// For use by browser support
define method do-imported-names (action :: <function>,
				 space :: <full-namespace>,
				 #key internal?)
  let seen :: <object-set> = make(<object-set>);
  local method do-names (local-names, kind)
	  for (local-name in local-names)
	    unless (member?(local-name, seen))
	      add!(seen, local-name);
	      action(local-name, kind);
	    end;
	  end;
	end method;
  // Get all the exported names first, so they don't get shadowed by
  // hitting any internal uses first.
  for (use in space.uses)
    let used-space = resolve-used-namespace(space, use.namespace);
    for (original-name in used-space.exported-names)
      do-names(filter-exported-name(use, original-name), #"exported");
    end;
    for (value keyed-by original-name in used-space.exported-imports-table)
      do-names(filter-exported-name(use, original-name), #"exported");
    end;
  end;
  // Now can get internal uses.
  when (internal?)
    for (use in space.uses)
      let used-space = resolve-used-namespace(space, use.namespace);
      for (original-name in used-space.exported-names)
	do-names(filter-name(use, original-name), #"internal");
      end;
      for (value keyed-by original-name in used-space.exported-imports-table)
	do-names(filter-name(use, original-name), #"internal");
      end;
    end;
  end;
end method;

//// Consistency checking.

// We can grossly categorise consistency problems as follows:
//
//   Import name clashes - one imported name with more than one origin.
//   Import/export clashes - imported names are also exported or created.
//   Imports not found - an explicitly-requested name isn't provided.

// Something explicitly-named as imported or excluded is not exported
// from the used namespace. Warning?

define program-warning <namespace-warning>
  slot condition-namespace = #f,
    init-keyword: namespace:;
  slot condition-clause = #f,
    init-keyword: clause:;
  slot condition-option = #f,
    init-keyword: option:;
end program-warning;

define program-warning <used-namespace-not-found> (<namespace-warning>)
  slot condition-used-namespace, init-keyword: used-namespace:;
  format-string
    "The namespace %= uses %s which is not defined.";
  format-arguments
    namespace, used-namespace;
end program-warning;

define program-warning <imported-names-not-exported> (<namespace-warning>)
  slot condition-names, init-keyword: names:;
  format-string
    "The names %= are specified as imported in %= of %= but cannot be "
    "because they are not exported.";
  format-arguments
    names, clause, namespace;
end program-warning;

define program-warning <excluded-names-not-exported> (<namespace-warning>)
  slot condition-names, init-keyword: names:;
  format-string
    "The names %= are specified as excluded in %= of %= but they are not "
    "exported.";
  format-arguments
    names, clause, namespace;
end program-warning;

// Two imports collide.
define program-warning <imported-name-clash> (<namespace-warning>)
  slot condition-name, init-keyword: name:;
  slot condition-first-binding, init-keyword: first:;
  slot condition-other-binding, init-keyword: other:;
  format-string
    "The origin of the name %= in %= is ambiguous, could be "
    "%= or %=.  Will use %=.";
  format-arguments
    name, namespace, first, other, first again;
end program-warning;

define program-warning <export-import-conflict> (<namespace-warning>)
  slot condition-name, init-keyword: name:;
  slot condition-imported-binding, init-keyword: import:;
  format-string
    "The name %= is specified as exported directly from %=, but cannot be "
    "because it is imported from %=";
  format-arguments
    name, namespace, import;
end program-warning;

define program-warning <create-import-conflict> (<namespace-warning>)
  slot condition-name, init-keyword: name:;
  slot condition-imported-binding, init-keyword: import:;
  format-string
    "The name %= is specified as created in %=, but cannot be "
    "because it is imported from %=";
  format-arguments
    name, namespace, import;
end program-warning;

define method update-imports
    (space :: <namespace>, filter :: <filter>, imports :: <table>)
 => (resolved? :: <boolean>)
  let clause = filter.clause;
  let used-space = resolve-used-namespace(space, clause.used-name,
					  default: #f);
  if (used-space == #f)
    note(<used-namespace-not-found>,
         source-location: form-source-location(namespace-definition(space)),
	 namespace: space,
	 clause: clause,
	 used-namespace: clause.used-name);
    #f
  else
    local method update-name (original-name, new)
	    let local-names = filter-name(filter, original-name);
	    for (local-name in local-names)
	      let old = element(imports, local-name, default: not-found());
	      if (not-found?(old))
		imports[local-name] := new;
	      elseif (old ~== new)
		note(<imported-name-clash>,
                     source-location:
                       form-source-location(namespace-definition(space)),
		     namespace: space, clause: clause,
		     name: local-name, first: old, other: new);
	      end;
	    end for;
	  end;
    for (original-name in used-space.exported-names)
      update-name(original-name, name-definition(used-space, original-name));
    end;
    for (binding keyed-by original-name in used-space.exported-imports-table)
      update-name(original-name, binding)
    end;
    #t
  end if;
end method;


//// Utility functions.

// Returns all used namespaces (including the argument) in order such that
// each space precedes any spaces that it uses.
define method all-used-namespaces (outer-space :: <full-namespace>) => (spaces)
  all-used-namespaces-cache(outer-space) |
    (all-used-namespaces-cache(outer-space) := compute-all-used-namespaces(outer-space))
end method;

define function compute-all-used-namespaces (outer-space :: <full-namespace>) => (spaces)
  collecting (used)
    let visited = make(<set>);
    local method visit (space)
      add!(visited, space);
      collect-first-into(used, space);
    end;
    local method visited? (space)
      member?(space, visited);
    end;
    local method walk (space)
      if (space & ~visited?(space))
	for (clause in space.use-clauses)
	  // The default is to allow for referenced but not found
	  // spaces.
	  walk(resolve-used-namespace
		 (space, clause.used-name, default: #f));
	end;
	visit(space);
      end;
    end;
    walk(outer-space);
    collected(used);
  end
end;

define method namespace-uses? (space :: <full-namespace>, name :: <name>)
  any?(method(clause) clause.used-name == name end, space.use-clauses)
end method;

define method directly-used-namespaces (space :: <full-namespace>) => (spaces)
  remove-duplicates!(map(compose(curry(resolve-used-namespace, space),
				 used-name),
			 space.use-clauses))
end method;
