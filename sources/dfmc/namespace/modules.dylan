Module:   dfmc-namespace
Synopsis: Modules.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Modules.

// In addition to pure namespace issues, modules have to manage syntax
// tables. The direct contents of a module entry is a binding object.
// Properties of these binding objects are collected at various stages
// during compilation.

// As with binding consistency, syntax consistency is checked at creation
// time and the information discarded to be recomputed on demand. If this
// has a significant impact on lexer speed we may have to abandon this
// approach.

define abstract dood-class <module> (<namespace>)
  lazy constant slot module-definer-bindings :: <dood-lazy-symbol-table>
    = make(<dood-lazy-symbol-table>), init-keyword: definer-bindings:;
end;

define dood-class <full-module> (<full-namespace>, <module>)
end;


define dont-copy-object <module>         using <fragment-copier>;
define dont-copy-object <module-binding> using <fragment-copier>;

define sideways method dood-disk-object
    (dood :: <dood>, object :: <string>) => (object)
  object
end method;

define sideways method dood-disk-object
    (dood :: <dood>, object :: <float>) => (object)
  object
end method;

define class <dood-cross-module-proxy> (<dood-proxy>)
  constant slot dood-proxy-library,     required-init-keyword: library:;
  constant slot dood-proxy-module-name, required-init-keyword: module-name:;
end class;

define sealed domain make (subclass(<dood-cross-module-proxy>));
define sealed domain initialize (<dood-cross-module-proxy>);

define method dood-make-cross-module-proxy
    (dood :: <dood>, object :: <module>) => (proxy)
  make(<dood-cross-module-proxy>,
       module-name: namespace-name(object),
       library:     home-library(object))
end method;

define method dood-disk-object
    (dood :: <dood>, object :: <module>)
 => (proxy :: type-union(<dood-cross-module-proxy>, <module>))
  if (language-definition(dood-root(dood)) == home-library(object))
    next-method();
  else
    dood-as-proxy(dood, object, dood-make-cross-module-proxy)
  end if
end method;

define thread variable *cross-module-access-abort* = #f;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-cross-module-proxy>) => (object)
  let lib = dood-proxy-library(proxy);
  lookup-module-in(lib, dood-proxy-module-name(proxy), default: #f)
    | begin
        // if library is all parsed, then it's a bug that we can't find
        // the module (or at least it's a different sort of error).  If
        // the library is not all parsed, then we might just be doing a
        // premature access during source updating.
        debug-assert(~lib.namespace-original-library.compiled-to-definitions?,
                     "Cross-database reference to missing module %s in %s",
                     dood-proxy-module-name(proxy), lib);
        debug-assert(*cross-module-access-abort*,
                     "used library %s of %s unexpectedly not parsed!",
                     lib, dood-root(dood));
        *cross-module-access-abort*();
      end;
end method;

define method home-library (module :: <module>)
  language-definition(namespace-library-description(module))
end method;

// Lookup a binding.

define generic lookup-binding-in
    (m :: <module>, name, #key reference?) => (binding);

//// Implementation.

define method created-name-value
    (space :: <module>, name :: <name>) => (value)
  make(<canonical-module-binding>, name: name, home: space);
end method;

define method exported-name-value
    (space :: <module>, name :: <name>) => (value)
  make(<canonical-module-binding>, name: name, home: space);
end method;

define method resolve-used-namespace
    (space :: <module>, space-designator, #key default = unsupplied())
      => (space)
  let library = space.home-library;
  let module = lookup-module-in(library, space-designator, default: default);
  module
end method;

// module '-definer' table

define constant $definer-suffix = "-definer";
define constant $definer-suffix-size = size($definer-suffix);

define function name-definer-word (name :: <name>) => word :: false-or(<name>);
  let string = as(<string>, name);
  let len = size(string);
  let prefix-len = len - $definer-suffix-size;
  unless (prefix-len <= 0)
    for (i from 0 below $definer-suffix-size, j from prefix-len by 1,
         while: $definer-suffix[i] == as-lowercase(string[j]))
    finally
      if (i == $definer-suffix-size)
        coerce-name(copy-sequence(string, end: prefix-len))
      end
    end for
  end unless
end function;

define method define-name (m :: <full-module>, name :: <name>, binding) => ()
  next-method();
  let word = name-definer-word(name);
  if (word) register-definer-word(word, m, binding) end;
end method;

define method note-namespace-imports (m :: <full-module>, imports :: <table>)
  for (binding keyed-by name in imports)
    let word = name-definer-word(name);
    if (word)
      define-name-in-cache(m.imported-name-cache, name, binding); // mark used
      register-definer-word(word, m, binding)
    end;
  end;
end method;

define method register-definer-word
    (word, module :: <full-module>, definer-binding :: <canonical-module-binding>)
  let library = home-library(module);
  let local-definer-binding = local-binding-in(library, definer-binding);
  module.module-definer-bindings[word] := local-definer-binding;
  let table = library.library-definer-references;
  let refs = element(table, local-definer-binding, default: #());
  let word-binding = untracked-lookup-binding-in(module, word);
  let local-word-binding = local-binding-in(library, word-binding);
  table[local-definer-binding] := add!(refs, local-word-binding);
end method;

/*
define function unregister-definer-word
    (word, module :: <full-module>, definer-binding, word-binding)
  remove-key!(module.module-definer-bindings, word);
  let library = home-library(module);
  let table = library.library-definer-references;
  let refs = remove!(table[definer-binding], word-binding);
  if (refs == #())
    remove-key!(table, definer-binding)
  else
    table[definer-binding] := refs
  end;
end function;
*/

define method untracked-definer-binding-in (m :: <module>, name :: <name>)
  let binding = element(module-definer-bindings(m), name, default: #f);
  binding & local-binding-in-requesting-library(binding)
end;

define method lookup-binding-in
    (m :: <module>, name, #key reference? = #t) => (binding :: <module-binding>)
  let binding = untracked-lookup-binding-in(m, name);
  let kind = if (reference?) dep$name-binding-ref else dep$name-binding end;
  note-name-dependency(binding, kind, name, m);
  binding
end method;

define method lookup-imported-binding
    (space :: <module>, binding :: <canonical-module-binding>)
  lookup-imported-binding(home-library(space), binding)
end method;

define method install-imported-binding
    (space :: <module>, binding :: <canonical-module-binding>)
  install-imported-binding(home-library(space), binding)
end method;

define function local-binding-in (library :: <library>,
                                  binding :: <module-binding>,
                                  #key default = unsupplied())
  let canonical-binding = binding-canonical-binding(binding);
  if (home-library(binding-home(canonical-binding)) == library)
    canonical-binding
  else
    lookup-imported-binding(library, canonical-binding)
      | if (supplied?(default)) default
        else install-imported-binding(library, canonical-binding) end
  end;
end function;

define function local-binding-in-requesting-library
    (binding :: <module-binding>)
  let requesting-library-description = current-library-description();
  if (~requesting-library-description)
    signal("No library context selected to act as the requesting library "
            "in binding lookup for %s.", binding);
    binding-canonical-binding(binding);
  else
    let requesting-library
      = requesting-library-description.language-definition;
    local-binding-in(requesting-library, binding);
  end;
end function;

define method untracked-lookup-canonical-binding-in
    (module :: <module>, name) => (value)
  lookup-name(module, name, default: #f)
end method;

define function untracked-lookup-binding-in
    (m :: <module>, name, #key default = unsupplied()) => (binding)
  let canonical-binding = lookup-name(m, name, default: $unfound);
  if (found?(canonical-binding))
    local-binding-in-requesting-library(canonical-binding)
  elseif (supplied?(default))
    default
  else
    // format-out("CREATING CANONICAL BINDING %=\n", name);
    let new-binding
      = new-binding-in(m, name);
    local-binding-in-requesting-library(new-binding)
  end if;
end function;

define method new-binding-in (m :: <module>, name :: <symbol>)
  let new-binding = make(<canonical-module-binding>, name: name, home: m);
  define-name(m, name, new-binding);
  new-binding
end method;

// TODO: add cache?  (Note that won't be able to use cache for any references
// before the macro definition, but that's ok since those are error cases and
// can be slower).
define method classify-word-in (m :: <module>, word)
  local method binding-class (binding)
          if (binding-macro-class?(binding))
            let definition  = untracked-binding-definition(binding, default: #f);
            let macro-class = definition & form-macro-word-class(definition);
            if (macro-class & (*current-dependent* == $no-dependent |
                               ~defined-after?(*current-dependent*, definition)))
              binding-macro-class?(binding) := #t;
              macro-class
            elseif (definition & ~macro-class)
              binding-macro-class?(binding) := #f;
            end if;
          else
            #f
          end if
        end method;
  // We need a binding so we can store the dependency, but this does mean that
  // module bindings get created for all words, even local variables.  If this
  // becomes a problem we can look into either making bindings more lightweight
  // or storing syntax dependencies differently.
  let non-definer-binding = untracked-lookup-binding-in(m, word);
  note-name-dependency(non-definer-binding, dep$name-syntax, word, m);
  let non-definer-class = begin
                            let class = binding-class(non-definer-binding);
                            class & ~definer-token-class?(class) & class
                          end;
  let definer-binding = untracked-definer-binding-in(m, word);
  let definer-class = if (definer-binding)
                        let class = binding-class(definer-binding);
                        class & definer-token-class?(class) & class
                      end;
  if (definer-class & non-definer-class)
    merge-token-classes(definer-class, non-definer-class, word)
  else
    definer-class | non-definer-class
  end
end method;

// fragment-module treats context == #f as dylan-implementation-module,
// so we follow its lead here.
define sideways method classify-word-in (context == #f, word)
  classify-word-in(dylan-implementation-module(), word);
end method;


define method macro-definition (name)
  macro-definition-in(fragment-module(name), fragment-identifier(name))
end method;

define method macro-definition-in (m, name)
  let binding = untracked-lookup-binding-in(m, name);
  note-name-dependency(binding, dep$name-macro-ref, name, m);
  let definition = untracked-binding-definition(binding, default: #f);
  let macro-class = definition & form-macro-word-class(definition);
  if (macro-class & (*current-dependent* == $no-dependent |
                       ~defined-after?(*current-dependent*, definition)))
    definition
  end
end method;

define serious-program-warning <invalid-source-module>
  slot condition-module-name,
    required-init-keyword: module:;
  slot condition-using-source-record,
    required-init-keyword: record:;
  format-string
    "Module %s of source record %s is not defined at this point,"
      " ignoring the source record.";
  format-arguments
    module, record;
end;

define serious-program-warning <external-source-module> (<invalid-source-module>)
  format-string
    "Source module %s of %s is defined in a different library,"
      " ignoring the source record.";
  format-arguments
    module, record;
end;

define method lookup-compilation-record-module
    (cr :: <compilation-record>, #key warn? = #t)
 => (maybe-module)
  debug-assert(current-library-description?(compilation-record-library(cr)));
  let name = source-record-module-name(compilation-record-source-record(cr));
  let module = lookup-module(name, default: #f);
  let definition = module & module.namespace-definition;
  if (instance?(module, <dylan-user-module>))
    module
  elseif (~definition | ~defined-before?(cr, definition))
    if (warn?)
      note(<invalid-source-module>, module: name, record: cr);
    end;
    #f
  elseif (~current-library-description?(form-library(definition)))
    if (warn?)
      note(<external-source-module>, module: name, record: cr);
    end;
    #f
  else
    module
  end;
end;

// Module registration

define program-warning <duplicate-module-definition>
  slot condition-definition, required-init-keyword: definition:;
  format-string "Duplicate module definition %= ignored";
  format-arguments definition;
end;

define function binding-for-module (library, name);
  // TODO: should only look at bindings defined in this library
  let binding = lookup-name(library, name, default: #f) |
                 begin
                   let new = make(<library-binding>,
                                  name: name, home: library);
                   define-name(library, name, new);
                   new
                 end;
  binding
end function;

define method copy-table-into
    (destination :: <table>, source :: <table>)
  for (value keyed-by key in source) destination[key] := value end;
end method;

define method copy-table-into
    (destination :: <set>, source :: <set>)
  for (value in source) add!(destination, value) end;
end method;

define method copy-table-into
    (destination :: <dood-lazy-table>, source :: <dood-lazy-table>)
  dood-lazy-table-source(destination) := source;
end method;

define function redefine-module! (module :: <full-module>,
                                  #key definition,
                                       name,
                                       use-clauses,
                                       create-clauses,
                                       export-clauses,
                                       model)
  let library = home-library(module);
  debug-assert(library = current-library-model());
  module.namespace-definition := definition;
  module.use-clauses    := use-clauses;
  module.create-clauses := create-clauses;
  module.export-clauses := export-clauses;
  module.namespace-model := model;
  module.cached-uses    := #f;
  module.all-used-namespaces-cache := #f;

  let imports = compute-imports(module);

  let local-bindings = module.namespace-local-bindings;
  let imported-bindings = module.imported-name-cache;
  let definer-bindings = module.module-definer-bindings;

  // Since have no easy way to verify that exported names are still
  // exported, move exported name cache to base cache.
  if (module.cached-exported-imports-table)
    copy-table-into(imported-bindings, module.cached-exported-imports-table);
    module.cached-exported-imports-table := #f;
  end;

  let local-keys-to-remove = #();
  for (old-binding keyed-by name in local-bindings)
    let new-binding = element(imports, name, default: #f);
    if (new-binding) // Was local, now is imported.
      let new-binding = local-binding-in(library, new-binding);
      note-changing-binding(old-binding, name, module, new-binding);
      local-keys-to-remove := pair(name, local-keys-to-remove);
    end;
  end;

  let imported-keys-to-remove = #();
  for (old-binding keyed-by name in imported-bindings)
    debug-assert(element(local-bindings, name, default: #f) == #f);
    unless (old-binding == $name-not-imported)
      let new-binding = element(imports, name, default: #f);
      if (new-binding ~== old-binding)
        // was imported, now is local or is imported from somewhere else
        let old-binding = local-binding-in(library, old-binding);
        let new-binding = new-binding & local-binding-in(library, new-binding);
        note-changing-binding(old-binding, name, module, new-binding);
        imported-keys-to-remove := pair(name, imported-keys-to-remove);
      end;
    end;
  end;

  // It's too hard to surgically try to update this, so we'll just redo it
  let definer-refs = library.library-definer-references;
  for (definer-binding keyed-by word in definer-bindings)
    // registering a definer binding always looks up the word binding,
    // so word is guaranteed to be in either the local or imported cache.
    let word-binding = element(local-bindings, word, default: #f)
                        | local-binding-in(library, imported-bindings[word]);
    let refs :: <list> = definer-refs[definer-binding];
    let new-refs = remove!(refs, word-binding, count: 1);
    if (new-refs == #())
      remove-key!(definer-refs, definer-binding);
    else
      definer-refs[definer-binding] := new-refs;
    end;
  end;
  remove-all-keys!(definer-bindings);

  for (name in local-keys-to-remove) remove-key!(local-bindings, name) end;
  for (name in imported-keys-to-remove) remove-key!(imported-bindings, name) end;

  // Ok, re-register definer bindings
  for (binding keyed-by name in local-bindings)
    let word = name-definer-word(name);
    if (word) register-definer-word(word, module, binding) end;
  end;
  note-namespace-imports(module, imports);

  // TODO: we currently don't track dependencies on bindings being exported
  // or not.  Maybe we should... In particular sealing support sometimes
  // seems to care about this.
  // let old-exports = module.exported-names;
  // But for now, the main uses are: using modules, which recompute everything
  // anyway, and check-bindings which also recomputes everything.
  // TODO: remove-all-keys doesn't work on set's!
  // remove-all-keys!(module.exported-names);
  // remove-all-keys!(module.created-names);
  module.exported-names := make(<set>);
  module.created-names := make(<set>);
  update-exports!(module, imports);

  module
end function redefine-module!;

define open generic ^make-<&module> (name, library-model) => (model);

define function define-and-install-module (#key definition,
                                                name,
                                                use-clauses = #(),
                                                create-clauses = #(),
                                                export-clauses = #())
 => (module-if-valid :: false-or(<module>));
  let library = current-library-model();
  let old-modules = library.library-deleted-modules;
  let old-module = any?(method (m) (m.namespace-name == name) & m end,
                        old-modules);
  let prior-binding = lookup-name(library, name, default: #f);
  let already-defined? = prior-binding & defined?(prior-binding);
  let library-model = namespace-model(library);
  let model = ^make-<&module>(name, library-model);
  let module
    = if (already-defined?)
        debug-assert(~old-module);
        note(<duplicate-module-definition>, definition: definition);
        #f
      elseif (old-module)
        library.library-deleted-modules := remove!(old-modules, old-module);
        redefine-module!(old-module,
                         definition: definition,
                         name: name,
                         use-clauses: use-clauses,
                         create-clauses: create-clauses,
                         export-clauses: export-clauses,
                         model: model);
        old-module
      else
        make-namespace(<full-module>,
                       definition: definition,
                       debug-name: name,
                       use-clauses: use-clauses,
                       create-clauses: create-clauses,
                       export-clauses: export-clauses,
                       model: model)
      end;
  if (module) define-module!(module) end;
  module
end;


define method define-module! (module :: <module>) => ()
  let library = home-library(module);
  let name = namespace-name(module);
  let binding = binding-for-module(library, name);
  debug-assert(~defined?(binding), "Missed duplicate module def");
  binding.library-binding-value := module;
end method;

define method undefine-module! (module :: <full-module>) => ()
  let library :: <full-library> = home-library(module);
  let name = namespace-name(module);
  let binding = lookup-name(library, name, default: $unfound);
  if (found?(binding) & defined?(binding)
        & (binding.library-binding-value == module))
    binding.library-binding-value := #f;
    library.library-deleted-modules := pair(module, library.library-deleted-modules);
    unless (binding.exported?)
      undefine-name(library, name);
    end;
  end;
end method;

define method module-defined? (module :: <module>) => (res :: <boolean>)
  module == lookup-module-in(home-library(module), namespace-name(module),
                             default: #f)
end method;

// Hygienic lookup functions

define inline method lookup-binding
    (name :: <variable-name-fragment>, #rest options) => (value)
  apply(lookup-binding-in,
        fragment-module(name), fragment-identifier(name), options)
end method;

define inline method untracked-lookup-binding
    (name :: <variable-name-fragment>, #rest options) => (value)
  apply(untracked-lookup-binding-in,
        fragment-module(name), fragment-identifier(name), options)
end method;

define method untracked-lookup-canonical-binding
    (name :: <variable-name-fragment>) => (value)
  untracked-lookup-canonical-binding-in
    (fragment-module(name), fragment-identifier(name))
end method;

define method form-variable-binding (form :: <variable-defining-form>)
  untracked-lookup-binding(form.form-variable-name, default: #f);
end;

define method model-variable-binding (model)
  let var = model.model-variable-name;
  var & untracked-lookup-binding(var, default: #f)
end;

define compiler-open generic form-defined-bindings
  (form :: <top-level-form>) => (seq :: <sequence>);

define method form-defined-bindings (form :: <top-level-form>)
 => (seq :: <list>)
  #()
end;

// needs to be overridden by multi-variable forms
define method form-defined-bindings (form :: <variable-defining-form>)
 => (seq :: <list>)
  if (form-ignored?(form))
    #()
  else
    list(form-variable-binding(form))
  end;
end;

//// Convenient access to the Dylan library and module.

define function dylan-library-description () => (library-description)
  current-library-description().library-description-dylan-library
end function;

define function dylan-library () => (library)
  let library = dylan-library-description().language-definition;
  if (~library | instance?(library, <boot-library>))
    error("The dylan library is not defined in the current world.")
  end;
  library
end function;

define function dylan-module () => (module, library)
  let library = dylan-library();
  let module = lookup-module-in(library, #"dylan", default: #f);
  if (module)
    values(module, library)
  else
    error("The dylan module is not currently defined in the dylan library.");
  end;
end function;

define function dylan-implementation-module () => (module, library)
  let library = dylan-library();
  let module = lookup-module-in(library, #"internal", default: #f);
  if (module)
    values(module, library)
  else
    error
      ("The internal module is not currently defined in the dylan library.");
  end;
end function;

//// The dylan-xxx forms are untracked, partly for efficiency,
//// partly because they're used a lot in out-of-context tests, etc.

define method dylan-binding (name :: <name>) => (model)
  untracked-lookup-binding-in(dylan-implementation-module(), name);
end method;

define method dylan-canonical-binding (name :: <name>) => (model)
  untracked-lookup-canonical-binding-in(dylan-implementation-module(), name);
end method;

define method dylan-binding (name :: <string>) => (model)
  dylan-binding(coerce-name(name));
end method;

define method dylan-definition (name) => (model)
  untracked-binding-definition(dylan-binding(name));
end method;

define function dylan-value (name) => (model)
  let cache
    = library-description-dylan-value-cache(dylan-library-description());
  let value = element(cache, name, default: $unfound);
  if (found?(value))
    value
  else
    // format-out("Cache miss on %=\n", name);
    let binding = dylan-binding(name);
    let model = binding-model-object(binding, default: $unfound);
    debug-assert(found?(model), "No model for dylan-value(%=)!", name);
    element(cache, name) := model;
  end;
end function;

define abstract class <dylan-user-module> (<module>)
end;

define class <full-dylan-user-module> (<full-module>, <dylan-user-module>)
  slot original-home-library,
    required-init-keyword: library:;
end class;

// Default method uses namespace definition, which we may not have.
define method namespace-library-description
    (m :: <full-dylan-user-module>) => (desc :: <library-description>)
  namespace-library-description(original-home-library(m));
end method;

define method namespace-original-library
    (m :: <full-dylan-user-module>) => (desc :: false-or(<library-description>))
  namespace-original-library(original-home-library(m))
end method;

define method namespace-name (m :: <dylan-user-module>) => (name :: <symbol>)
  #"dylan-user"
end method;

// Default method uses home-library, which might have renamed #"dylan"...
define method resolve-used-namespace (m :: <dylan-user-module>, name,
                                      #key default = unsupplied()) => (module)
  // debug-assert(name == #"dylan");
  lookup-module-in(dylan-library(), name, default: default)
end method;


define function dylan-user-module () => (module :: <dylan-user-module>)
  let library :: <library> = current-library-description().language-definition;
  lookup-module-in(library, #"dylan-user")
end function;

define function dylan-user-module-variable (name :: <symbol>)
  make-variable-name-fragment-in-module(name, dylan-user-module())
end;

define method namespace-model-variable (namespace :: <module>)
  let name = namespace-name(namespace);
  let var-name = as(<symbol>, concatenate(as(<string>, name), "-module"));
  dylan-user-module-variable(var-name);
end;

define method namespace-model-variable (namespace :: <library>)
  let name = namespace-name(namespace);
  let var-name = as(<symbol>, concatenate(as(<string>, name), "-library"));
  dylan-user-module-variable(var-name);
end;


define compiler-open generic make-module-definition
  (#key name, source-location, parent-form,
        use-clauses, create-clauses, export-clauses);

define serious-program-warning
    <failed-to-find-library-for-qualified-name> (<manual-parser-error>)
  slot condition-name,
    required-init-keyword: name:;
  slot condition-module-name,
    required-init-keyword: module-name:;
  slot condition-library-name,
    required-init-keyword: library-name:;
  format-string
    "Failed to find the library %s for the qualified name %s:%s:%s.";
  format-arguments
    library-name, name, module-name, library-name again;
end serious-program-warning;

define serious-program-warning
    <failed-to-find-module-for-qualified-name> (<manual-parser-error>)
  slot condition-name,
    required-init-keyword: name:;
  slot condition-module-name,
    required-init-keyword: module-name:;
  slot condition-library-name,
    required-init-keyword: library-name:;
  format-string
    "Failed to find the module %s for the qualified name %s:%s:%s.";
  format-arguments
    module-name, name, module-name again, library-name;
end serious-program-warning;

define serious-program-warning <non-interactive-qualified-name>
  slot condition-name,
    required-init-keyword: name:;
  slot condition-module-name,
    required-init-keyword: module-name:;
  slot condition-library-name,
    required-init-keyword: library-name:;
  format-string
    "Qualified name %s:%s:%s encountered outside the interactor.";
  format-arguments
    name, module-name, library-name;
end serious-program-warning;

define sideways method resolve-qualified-variable-name-module
    (name, module-name, library-name, source-location) => (module)
  let this-ld = current-library-description();
  let this-library = this-ld.language-definition;
  let used-lds = all-used-library-descriptions(this-ld);
  local method resolve-library (name)
    block (return)
      for (used-ld in used-lds)
        let used-library = language-definition(used-ld);
        if (namespace-name(used-library) == name)
          return(used-library);
        end;
      finally
        #f
      end;
    end;
  end method;
  let library
    = if (library-name)
        resolve-library(library-name)
          | note(<failed-to-find-library-for-qualified-name>,
                 source-location: source-location,
                 name: name,
                 module-name: module-name,
                 library-name: library-name);
      else
        this-library
      end;
  let module
    = lookup-module-in(library, module-name, default: #f)
        | note(<failed-to-find-module-for-qualified-name>,
               source-location: source-location,
               name: name,
               module-name: module-name,
               library-name: library-name | namespace-name(library));
  if (~*interactive-compilation-layer*)
    note(<non-interactive-qualified-name>,
         source-location: source-location,
         name: name,
         module-name: module-name,
         library-name: library-name | namespace-name(library));

  end;
  module
end method;
