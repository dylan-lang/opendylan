module: dfmc-management
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <interface-identifier> = <byte-string>;
define constant <symbol-vector> = <simple-object-vector>;
define constant <identifier-vector> = <simple-object-vector>;

define inline method interface-identifier (binding :: <module-binding>)
 => (id :: <interface-identifier>)
  binding-mangled-name(current-back-end(), binding)
end;

define method interface-identifier (f == #f)
  ""
end;

define method identifier-less-than? (b1 :: <module-binding>,
				     b2 :: <module-binding>)
  interface-identifier(b1) < interface-identifier(b2)
end;

define inline method interface-identifier (module :: <module>)
 => (id :: <interface-identifier>)
  module-mangled-name(current-back-end(), module)
end;

define method identifier-less-than? (b1 :: <module>,
				     b2 :: <module>)
  interface-identifier(b1) < interface-identifier(b2)
end;

// This records mapping of names (symbols) to their unique identifiers.
define class <name-mapping> (<object>)
  constant slot name-mapping-keys :: <symbol-vector>,
    required-init-keyword: keys:;
  constant slot name-mapping-values :: <identifier-vector>,
    required-init-keyword: values:;
end;

define sealed-constructor <name-mapping>;

define sealed method \= 
    (x :: <name-mapping>, y :: <name-mapping>) => (well? :: <boolean>)
  x.name-mapping-keys = y.name-mapping-keys
    & x.name-mapping-values = y.name-mapping-values
end method;

define function as-name-mapping (table :: <object-table>)
  let keys = as(<symbol-vector>, key-sequence-vector(table));
  let keys = sort!(keys, test: symbol-less-than?);
  let vals = map-as(<identifier-vector>,
		    method (key) interface-identifier(table[key]) end,
		    keys);
  make(<name-mapping>, keys: keys, values: vals)
end;

// This records mapping of identifiers (for bindings or modules) to
// their values.
define class <identifier-mapping> (<object>)
  constant slot identifier-mapping-keys :: <identifier-vector>,
    required-init-keyword: keys:;
  constant slot identifier-mapping-values :: <simple-object-vector>,
    required-init-keyword: values:;
end;
define sealed-constructor <identifier-mapping>;

define sealed method \= 
    (x :: <identifier-mapping>, y :: <identifier-mapping>) => (well? :: <boolean>)
  x.identifier-mapping-keys = y.identifier-mapping-keys
    & x.identifier-mapping-values = y.identifier-mapping-values
end method;

define function as-identifier-mapping (table :: <object-table>)
  let keys = sort!(key-sequence-vector(table), test: identifier-less-than?);
  let vals = map-as(<simple-object-vector>, method (key) table[key] end, keys);
  for (i from 0 below keys.size) keys[i] := interface-identifier(keys[i]) end;
  let keys = as(<identifier-vector>, keys);
  make(<identifier-mapping>, keys: keys, values: vals);
end;

define class <library-interface> (<object>)
  // exported name -> module identifier
  constant slot interface-exports :: <name-mapping>,
    required-init-keyword: exports:;
  // visible module identifier -> name mapping for visible name -> binding
  constant slot interface-modules :: <identifier-mapping>,
    required-init-keyword: modules:;
  //  binding -> macro identifier
  constant slot interface-models :: <identifier-mapping>,
    required-init-keyword: models:;
end class;

define sealed-constructor <library-interface>;

define sealed method \= 
    (x :: <library-interface>, y :: <library-interface>) => (well? :: <boolean>)
  x.interface-exports = y.interface-exports
    & x.interface-modules = y.interface-modules
    & x.interface-models = y.interface-models
end method;

define class <module-interface-walk> (<object>)
  constant slot interface-walk-module :: <module>,
    required-init-keyword: module:;
  constant slot interface-walk-walks :: <object-table>,
    required-init-keyword: walks:;
  constant slot interface-walk-values :: <object-table>,
    required-init-keyword: values:;
  // This is what the walk actually computes: name -> binding mapping
  constant slot interface-walk-names :: <object-table> = make(<object-table>);
end;

define sealed-constructor <module-interface-walk>;

define method library-interface-spec (library :: <library>,
				      #key policy = #"loose")
  debug-assert(policy == #"loose", "Interface policy not implemented");
  let exported-name->module = make(<object-table>);
  let module->walk = make(<object-table>);
  let binding->data = make(<object-table>);
  local method init-walk (name, module)
	  when (module)
	    debug-assert(~element(exported-name->module, name, default: #f));
	    exported-name->module[name] := module;
	    // Could be re-exporting the same module twice under different
	    // names so check if already init'ed.
	    unless (element(module->walk, module, default: #f))
	      module->walk[module] := make(<module-interface-walk>,
					   module: module,
					   values: binding->data,
					   walks: module->walk);
	    end unless;
	  end when;
	end method;
  for (name in library.exported-names)
    init-walk(name, lookup-module-in(library, name, default: #f))
  end;
  for (lb keyed-by name in library.exported-imports-table)
    init-walk(name, library-binding-value(lb))
  end;
  for (module in exported-name->module)
    let walk = module->walk[module];
    for (name in module.exported-names)
      interface-walk-name(walk, name, lookup-name(module, name));
    end;
    for (binding keyed-by name in module.exported-imports-table)
      interface-walk-name(walk, name, binding);
    end;
  end;
  let module->name-mapping
    = map(method (walk :: <module-interface-walk>)
	    as-name-mapping(walk.interface-walk-names)
	  end,
	  module->walk);
  make(<library-interface>,
       exports: as-name-mapping(exported-name->module),
       modules: as-identifier-mapping(module->name-mapping),
       models: as-identifier-mapping(binding->data))
end;

define function interface-walk-name
    (walk :: <module-interface-walk>, word :: <symbol>,
     binding :: false-or(<module-binding>))
  let table = walk.interface-walk-names;
  unless (element(table, word, default: #f))
    table[word] :=
      if (~binding | (binding.name == word &
			binding.binding-home == walk.interface-walk-module))
	#f
      else
	binding
      end;
    let form = binding & untracked-binding-definition(binding, default: #f);
    when (form & form.form-macro-word-class)
      let vals = walk.interface-walk-values;
      let walks = walk.interface-walk-walks;
      unless (element(vals, binding, default: #f))
	let loc = form.form-source-location;
	// TODO: check this out.  Possibly it can come up when re-export
	// system macros.  If there is no source loc, maybe that means can't
	// change?  What about the case where macro is from another library
	// whose source is currently unavailable (has changed).
	debug-assert(loc, "Not implemented -- interface spec for %s", form);
	vals[binding] := source-location-string(loc);
      end;
      let mac = form.form-macro-object;
      debug-assert(instance?(mac, <macro-descriptor>), "Unknown macro type %s", mac);
      for (var :: <variable-name-fragment> in macro-referenced-names(mac))
	let module = var.fragment-module;
	let word = var.fragment-identifier;
	let w = element(walks, module, default: #f)
	          | (walks[module] := make(<module-interface-walk>,
					   module: module,
					   values: vals,
					   walks: walks));
	interface-walk-name(w, word, lookup-name(module, word, default: #f));
      end for;
    end when;
  end unless;
end;
