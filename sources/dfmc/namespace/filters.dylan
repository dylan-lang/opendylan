Module:   dfmc-namespace
Synopsis: Namespace filters.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Filters.

// This file implements the class <filter> whose instances encapsulate
// some combination of importation, exclusion and renaming directives
// to be applied to a set of names. The directives supported are
// those available to modules and libraries for controlling their
// respective namespaces.
//
// Example:
//
//      make(<filter>
//           exclude: #(#"joe"),
//           rename:  #(#(#"jim" . #"sue")
//                      #(#"sue" . #"jim")),
//           prefix:  "person/")
//
// Two main filter functions are constructed, one for mapping names in 
// each direction through the filter. "Pushing" a client-side name through 
// the filter results in the name it must have on the other side in 
// order to be visible (or no name if it couldn't be visible through 
// the filter). "Pulling" a used-side name though the filter results
// in the (potentially empty) set of names under which it is visible on
// the client side.
//
// Pushing is used for demand-driven lookup, while pulling tends to be
// used for the bulk verification of the consistency of a namespace.
//
// Two further filter functions are constructed that filter through
// any re-export clause present.

// Filters are implemented using function composition to construct
// the two mapping functions from names to names. The complexity of
// these functions is related to the complexity of the directives 
// specified. If no directive is specified, both these functions 
// become the identity and no computation is performed when mapping 
// keys.
//
// These functions need to be "recompiled" should the directives ever
// be destructively modified. Because they can be recomputed from the
// clauses at any time, the closures need not be stored in a database.

// Issues: Can you rename an imported name to multiple names?
//         For example: 
//
//           use foo, rename: { a => alias-1, a => alias-2 }
//
//         => DRM p. 373 seems to suggest this is OK.
//
//         Can multiple import: etc. options be specified in a single use.
//
//         => DRM p. 373 yes.

// Issue: Should we use hash-tables rather than plists for name sets for
// speed? Perhaps only when the number of names to search passes some
// threshold.

// Issue: Currently uses == to compare names and assumes names are symbols.
// Must be fixed.

define constant old-name = head;
define constant new-name = tail;

define class <filter> (<object>)
  constant slot namespace,
    required-init-keyword: namespace:;
  constant slot clause,
    required-init-keyword: clause:;
  slot import-spec, 
    init-value: #"all", 
    init-keyword: import:;
  slot exclude-spec, 
    init-value: #(), 
    init-keyword: exclude:;
  constant slot prefix-spec, 
    init-value: "", 
    init-keyword: prefix:;
  slot rename-spec, 
    init-value: #(), 
    init-keyword: rename:;
  constant slot export-spec, 
    init-value: #(), 
    init-keyword: export:;
  slot filtered-key->key :: <function>;
  slot key->filtered-key :: <function>;
//  slot exported-filtered-key->key :: <function>;
  slot key->exported-filtered-key :: <function>;
end class <filter>;

define method initialize (f :: <filter>, #key)
  next-method();
  ensure-valid-filter(f);
  compute-filter-maps(f);
  f
end method initialize;

// Take a used-side name and pass it though the filter to produce the
// set of names by which it may be accessed within the client.

define generic filter-name (f :: <filter>, name) => (collection);

// Take a client-side name and pass it though the filter to produce the
// name by which it should be looked up in the exports of the used 
// namespace.

define generic unfilter-name (f :: <filter>, name) => (name-or-false);

// Take a used-side name and pass it though the filter to produce the
// set of names by which it may be accessed by users of the client.

define generic filter-exported-name (f :: <filter>, name) => (collection);

// Take a re-exported client-side name and pass it though the filter to 
// produce the name by which it should be looked up in the exports of the 
// used namespace.

// define generic unfilter-exported-name (f :: <filter>, name) => (name-or-false);

// Validity checks on the filter. The following conditions can arise:

define program-warning <import-and-exclude-specified>
  slot condition-namespace,
    required-init-keyword: namespace:;
  slot condition-clause,
    required-init-keyword: clause:;
  format-string 
    "An import and an exclude option appear together in %= of %=. Ignoring the exclude.";
  format-arguments 
    clause, namespace;
end;

define program-warning <rename-exclude-conflict>
  slot condition-namespace,
    required-init-keyword: namespace:;
  slot condition-clause,
    required-init-keyword: clause:;
  slot condition-names,
    required-init-keyword: names:;
  format-string 
    "The names %= renamed in %= of %= are excluded.";
  format-arguments 
    names, namespace, clause;
end program-warning <rename-exclude-conflict>;

define method ensure-valid-filter (f :: <filter>) => ()
  // Are both an import and an exclude option specified?
  if (~empty?(f.exclude-spec) & ~(f.import-spec == #"all"))
    note(<import-and-exclude-specified>,
	 namespace: f.namespace,
	 clause: f.clause);
    f.exclude-spec := #();
  end if;
  begin
    let renamed-names = map(old-name, f.rename-spec);
    let conflicts = intersection(renamed-names, f.exclude-spec);
    if (~empty?(conflicts))
      note(<rename-exclude-conflict>,
           names: conflicts,
           clause: f.clause,
           namespace: f.namespace);
    end if
  end;
  begin
    let (renames, imports)
      = begin
          let spec = f.import-spec;
          if (spec == #"all")
            values(#(), spec)
          else
            values
              (choose(rcurry(instance?, <pair>), spec),
               choose(complement(rcurry(instance?, <pair>)), spec))
          end if
        end;
    f.import-spec := imports;
    f.rename-spec := concatenate(f.rename-spec, renames)
  end;
  unless (f.import-spec == #"all")
    f.import-spec
      := remove-duplicates
           (concatenate(f.import-spec, map(old-name, f.rename-spec)))
  end unless
end method ensure-valid-filter;

define method compute-filter-maps (f :: <filter>)
  let fk->k = identity;
  let k->fk = identity;
  begin
    let imported = f.import-spec;
    unless (imported == #"all")
      let filter
        = method (keys)
            choose(rcurry(member?, imported), keys)
          end method;
      k->fk := compose(filter, k->fk);
      fk->k := compose(filter, fk->k);
    end unless
  end;
  begin
    let excluded = f.exclude-spec;
    unless (empty?(excluded))
      let filter
        = method (keys)
            choose(complement(rcurry(member?, excluded)), keys)
          end method;
      k->fk := compose(filter, k->fk);
      fk->k := compose(filter, fk->k)
    end unless
  end;
  begin
    let renamed = f.rename-spec;
    let prefix = as(<string>, as(<symbol>, f.prefix-spec));
    unless (empty?(renamed) & prefix = "")
      k->fk := compose(renaming-expander(renamed, prefix), k->fk);
      fk->k
        := compose(fk->k, unrenaming-expander(renamed, prefix, f.exclude-spec))
    end unless
  end;
  f.filtered-key->key := compose(fk->k, list);
  f.key->filtered-key := compose(k->fk, list);
  begin
    let exported = f.export-spec;
    unless (exported == #"all")
      let filter
        = method (keys)
            choose(rcurry(member?, exported), keys)
          end method;
      k->fk := compose(filter, k->fk);
      fk->k := compose(fk->k, filter)
    end unless
  end;
//  f.exported-filtered-key->key := compose(fk->k, list);
  f.key->exported-filtered-key := compose(k->fk, list);
  values()
end method compute-filter-maps;

define method renaming-expander (renaming, prefix)
  method (keys)
    let aliases = #();
    for (key in keys)
      let local-aliases = #();
      for (rename in renaming)
        if (key == old-name(rename))
          local-aliases := pair(new-name(rename), local-aliases)
        end if;
      end for;
      if (empty?(local-aliases))
        local-aliases
          := pair(as(<symbol>, concatenate(prefix, as(<string>, key))),
                  local-aliases)
      end if;
      aliases := concatenate(local-aliases, aliases);
    end for;
    aliases
  end method
end method renaming-expander;

define method unrenaming-expander (renaming, prefix, excluded)
  method (keys)
    let unaliases = #();
    for (key in keys)
      let local-unaliases = #();
      for (rename in renaming)
        if (key == new-name(rename))
          local-unaliases := pair(old-name(rename), local-unaliases)
        end if;
      end for;
      if (empty?(local-unaliases))
        let string = as(<string>, key);
        let i = subsequence-position(string, prefix);
        if (i & i = 0)
          let symbol
            = as(<symbol>, copy-sequence(string, start: size(prefix)));
          unless (member?(symbol, excluded)
                  | member?
                      (symbol, renaming,
                       test: method (symbol, rename)
                               symbol == old-name(rename)
                             end method))
            local-unaliases := pair(symbol, local-unaliases)
          end unless
        end if
      end if;
      unaliases := concatenate(local-unaliases, unaliases);
    end for;
    unaliases
  end method
end method unrenaming-expander;

define method filter-name (f :: <filter>, name) => (res :: <list>)
  f.key->filtered-key(name)
end method;

define method unfilter-name (f :: <filter>, name) => (name-or-false)
  let names = f.filtered-key->key(name);
  if (empty?(names)) #f else names.first end
end method;

define method filter-exported-name (f :: <filter>, name) => (res :: <list>)
  f.key->exported-filtered-key(name)
end method;

// define method unfilter-exported-name (f :: <filter>, name) => (name-or-false)
//   let names = f.exported-filtered-key->key(name);
//   if (empty?(names)) #f else names.first end
// end method;

// eof
