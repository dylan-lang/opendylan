Module: dfmc-namespace
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract dood-class <library> (<namespace>) 
  lazy slot imported-bindings-tables :: <dood-lazy-table> 
    = make(<dood-lazy-table>),
    init-keyword: imported-bindings:;
end;

define dood-class <full-library> (<full-namespace>, <library>)
  constant slot used-libraries :: <ordered-object-table>,
    required-init-keyword: used-libraries:;
  weak slot cached-all-library-descriptions :: false-or(<list>) = #f,
    reinit-expression: #f;
  // Not strictly speaking a cache - for tightly bound libraries,
  // contains record of last model change count, which shouldn't be cleared
  // out arbitrarily.  It should only be cleared if all pointers to models
  // are flushed, usually if compilation-from-definitions-started? is #f.
  weak slot cached-all-inter-library-bindings :: false-or(<table>) = #f,
    reinit-expression: #f;
  weak slot library-deleted-modules :: <list> = #(),
    reinit-expression: #();
  // Duplicate primary definitions
  lazy constant slot library-duplicate-definitions :: <table> = make(<table>);
  // All other ignored definitions (modifying, or primary which are ignored
  // for reasons other than being duplicate).
  lazy constant slot library-ignored-definitions :: <table> = make(<table>);
  // xxx-definer backpointers
  lazy slot library-definer-references :: <table> = make(<table>);
  // "Wildcard" subclasses whose superclass class definitions can't be
  // determined at compile-time.
  lazy slot library-wildcard-subclass-definitions :: <list> = #();
end;

define method make-namespace (class == <library>, #rest initargs, #key, #all-keys)
  apply(make-namespace, <full-library>, initargs)
end;

define constant <imported-bindings-table> = <dood-lazy-symbol-table>;

define macro with-library-description
  { with-library-description (?:expression) ?:body end }
    => { do-with-library-description(method () ?body end, ?expression) }
end macro;

define macro with-top-level-library-description
  { with-top-level-library-description (?:expression) ?:body end }
    => { do-with-top-level-library-description(method () ?body end, ?expression) }
end macro;

define function binding-local-duplicate-definitions (binding)
  debug-assert(valid-binding-home-library?(binding));
  let lib :: <full-library> = current-library-model();
  let table = lib.library-duplicate-definitions;
  element(table, binding, default: #())
end function;

define function add-local-duplicate-definition (binding, definition)
  debug-assert(valid-binding-home-library?(binding));
  let lib :: <full-library> = current-library-model();
  let table = lib.library-duplicate-definitions;
  table[binding] := pair(definition, element(table, binding, default: #()));
end;

define function remove-local-duplicate-definition (binding, definition)
  debug-assert(valid-binding-home-library?(binding));
  let lib :: <full-library> = current-library-model();
  let table = lib.library-duplicate-definitions;
  let remain = remove!(element(table, binding, default: #()), definition);
  if (empty?(remain))
    remove-key!(table, binding);
  else
    table[binding] := remain
  end;
end;  

define function binding-local-ignored-definitions (binding)
  debug-assert(valid-binding-home-library?(binding));
  let lib :: <full-library> = current-library-model();
  let table = lib.library-ignored-definitions;
  element(table, binding, default: #())
end function;

define function add-local-ignored-definition (binding, definition)
  debug-assert(valid-binding-home-library?(binding));
  let lib :: <full-library> = current-library-model();
  let table = lib.library-ignored-definitions;
  table[binding] := pair(definition, element(table, binding, default: #()));
end;

define function remove-local-ignored-definition (binding, definition)
 => (removed? :: <boolean>)
  debug-assert(valid-binding-home-library?(binding));
  let lib :: <full-library> = current-library-model();
  let table = lib.library-ignored-definitions;
  let old = element(table, binding, default: #());
  if (member?(definition, old))
    let remain = remove!(old, definition);
    if (empty?(remain))
      remove-key!(table, binding);
    else
      table[binding] := remain
    end;
    #t
  end;
end;  

define method used-library-table (ld :: <project-library-description>)
 => (tab :: <ordered-object-table>)
  ld.language-definition.used-libraries
end;

define method dood-reinitialize (dood :: <dood>, object :: <library>) => ()
  next-method();
  let ld :: <project-library-description> = dood.dood-root;
  debug-assert(ld.language-definition == object);
  reinitialize-used-library-table(ld);
end method;

define method dood-reinitialize (dood :: <dood>, object :: <full-library>) => ()
  next-method();
  let ld :: <project-library-description> = dood.dood-root;
  debug-assert(ld.language-definition == object);
  if (ld.compilation-from-definitions-started?)
    record-inter-library-model-use(ld);
  end;
end method;



define function reinitialize-used-library-table (ld :: <project-library-description>)
  // This calls back to the project manager to find each used
  // project, and thus can easily err out and abort if the
  // project is not found or otherwise messed up.
  let finished? = #f;
  block ()
    for (ul keyed-by name in ld.used-library-table)
      let uld = find-used-library-description(ld, name, canonicalize?: #f);
      with-library-description (uld)
	ensure-language-definition(uld);
      end;
      ul.used-library-description := uld;
      ul.used-library-model-change-count
        := uld.language-definition-change-count;
    end for;
    finished? := #t;
  cleanup
    unless (finished?)
      ld.compilation-definitions-inconsistent? := #t;
      remove-all-keys!(ld.used-library-table);
    end;
  end;
end function;

define method strip-incremental-slots (lib :: <full-library>)
  next-method();
  // This is only used for redefinition
  remove-all-keys!(lib.library-definer-references);
end method;

// Just like variables, modules can be marked as exported before their
// definition is seen. The library binding object captures this 
// information about a module name's binding within a library.

define class <library-binding> (<dood-dfmc-object>, <named-object>)
  constant slot library-binding-home :: <library>,
    required-init-keyword: home:;
  constant slot exported? = #f,
    init-keyword: exported?:;
  slot library-binding-value = #f,
    init-keyword: value:;
end class;

define method defined? (mb :: <library-binding>)
  library-binding-value(mb) ~== #f
end method;

define method lookup-imported-binding 
    (space :: <library>, binding :: <canonical-module-binding>)
  let tables = space.imported-bindings-tables;
  let table = element(tables, binding.binding-home, default: #f);
  table & element(table, binding.name, default: #f)
end method;

define inline function do-imported-bindings
    (space :: <library>, function :: <function>)
  for (table in space.imported-bindings-tables)
    do(function, table)
  end
end function;

define method install-imported-binding 
    (space :: <library>, binding :: <canonical-module-binding>)
  let imported-binding
    = make(<imported-module-binding>, canonical-binding: binding);
  // format-out("INSTALLING IMPORTED BINDING %=\n", imported-binding);
  debug-assert(~valid-binding-home-library-in?(space, binding));
  let tables = space.imported-bindings-tables;
  let table = element(tables, binding.binding-home, default: #f)
              | (tables[binding.binding-home] := make(<imported-bindings-table>));
  element(table, binding.name) := imported-binding;
end method;

define method retract-imported-bindings (space :: <library>) => ()
  do-imported-bindings(space, retract-imported-binding)
end method;

// For debugging only
define function valid-binding-home-library? (binding :: <module-binding>)
  valid-binding-home-library-in?(current-library-model(), binding)
end function;
define method valid-binding-home-library-in?
    (space :: <library>, binding :: <canonical-module-binding>)
  space == binding.binding-home.home-library
end method;
define method valid-binding-home-library-in?
    (space :: <library>, binding :: <imported-module-binding>)
  binding == lookup-imported-binding(space, binding.binding-canonical-binding)
end method;
define method valid-binding-home-library-in?
    (ld :: <library-description>, binding :: <module-binding>)
  valid-binding-home-library-in?(ld.language-definition, binding)
end method;


define method created-name-value 
    (space :: <library>, name :: <name>) => (value)
  error("Attempted to \"create\" a module %= in %= - the create clause is " 
        "not supported in libraries.",
        space);
end method;

define method exported-name-value 
    (space :: <library>, name :: <name>) => (value)
  make(<library-binding>, name: name, home: space, exported?: #t)
end method;

// These tables are relatively small, so might as well cache it all.
define method note-namespace-imports (space :: <full-library>, imports :: <table>)
  copy-table-into(space.imported-name-cache, imports)
end method;

///  Used libraries

define dood-class <used-library> (<object>)
  weak slot used-library-description :: <library-description>,
    required-init-keyword: description:;
  slot used-library-model-change-count :: <integer>,
    required-init-keyword: model-change-count:;
  constant slot used-library-binding,
    required-init-keyword: binding:;
  // These get filled in when real parsing starts, not before then.
  slot used-library-major-version = #f;
  slot used-library-minor-version = #f;
  slot used-library-change-count = #f;
end;

define method used-library-library (ul :: <used-library>, parent :: <library>)
  let ld = used-library-description(ul);
  // These are bugs because it means the env. somehow didn't interlock
  // compilations.
  // debug-assert(~ld.library-description-closed?,
  // 	          "Used library %s closed unexpectedly during compilation", ld);
  unless (ld.language-definition-change-count
	    == ul.used-library-model-change-count)
    let parent-ld = parent.namespace-library-description;
    debug-assert(#f,
		 if (~parent-ld.library-description-personal?)
		   "System library %s uses a library %s whose database "
		     "has been modified since the system library was originally compiled."
		 else
		   "Library %s uses a library %s whose namespace was reset "
		     "unexpectedly during compilation"
		 end,
		 parent-ld, ld);
  end;
  ld.language-definition
end;

// Project manager call-back
define open generic used-library-context
    (context, used-name, #key canonicalize?)
 => (used-context /* :: <library-description> */);

define function find-used-library-description (ld, used-name,
					       #key canonicalize? = #t)
  used-library-context(ld, used-name, canonicalize?: canonicalize?)
end function;

define function make-used-library-table (use-clauses, description)
 => (table :: <ordered-object-table>)
  let table = make(<ordered-object-table>);
  unless (empty?(use-clauses))
    let project = description.library-description-project;
    for (clause in use-clauses)
      let used-name = clause.used-name;
      unless (element(table, used-name, default: #f))
	let ld = find-used-library-description(description, used-name,
					       canonicalize?: #t);
	debug-assert(ld.library-description-defined?
		       | ld.compiled-to-definitions?,
		     "Dev env didn't ensure %s library definition parsed!", used-name);
	debug-assert(ld.library-description-dylan-library
		       == description.library-description-dylan-library,
		     "%s uses a different dylan library than %s",
		     project, ld.library-description-project);
	let binding = project-inter-library-binding
	                (project, ld.library-description-project);
	table[used-name]
          := make(<used-library>,
                  description: ld,
		  model-change-count: ld.language-definition-change-count,
		  binding: binding);
      end;
    end for;
  end unless;
  table
end function;

define compiler-open generic ^make-<&library> (name) => (model);
define compiler-open generic ^used-libraries-setter (value :: <simple-object-vector>, library);
define compiler-open generic ^used-libraries (library) => (used :: <simple-object-vector>);
define compiler-open generic ^all-used-libraries-setter (value :: <simple-object-vector>, library);
define compiler-open generic ^all-used-libraries (library) => (used :: <simple-object-vector>);
define compiler-open generic ^library-description-setter (library-description, model);
define compiler-open generic ^library-description (model) => (library-description);

define method make-namespace (class :: subclass(<full-library>), #rest initargs,
			      #key use-clauses = #[],
			           definition,
			           description = form-library(definition),
			      #all-keys)
  apply(next-method, class,
	used-libraries: make-used-library-table(use-clauses, description),
	initargs)
end method;

define method resolve-used-namespace 
    (library :: <library>, used-name, #key default = unsupplied()) 
      => (library);
  let ul = element(library.used-libraries, used-name, default: #f);
  if (ul)
    used-library-library(ul, library)
  else
    debug-assert(supplied?(default),
		 "Failed to resolve used library %= in the library %s.",
		 used-name, library);
    default
  end;
end;    

// Returns true if any directly-used library definition changed, or a directly
// used project got closed/reopened.  Calls back to the project manager to
// lookup directly-used projects (without canonicalizing) in order to check
// the latter possibility.
define function invalidated-library-definition?
    (ld :: <project-library-description>)
  block (return)
    debug-out(#"driver", "Verify definition of %s\n", ld);
    for (ul keyed-by used-name in ld.used-library-table)
      let uld = find-used-library-description(ld, used-name,
					      canonicalize?: #t);
      // Since we have pointers into the used <library>, we need to retract
      // if the used library got retracted (or if used project was closed
      // and re-opened).
      unless (uld == ul.used-library-description)
	debug-out(#"driver", "%s invalidated because %s reopened\n", ld, uld);
	return(#t);
      end;
      unless (uld.language-definition-change-count
		== ul.used-library-model-change-count)
	debug-out(#"driver",
		  "%s invalidated because %s library model changed\n",
		  ld, uld);
	return(#t);
      end;
      debug-assert(uld.library-description-defined? |
		     uld.compiled-to-definitions?,
		   "Dev env didn't ensure %s library definition parsed!",
		   uld.library-description-project);
      debug-assert(uld.library-description-dylan-library
		     == ld.library-description-dylan-library,
		   "%s uses a different dylan library than %s", ld, uld);
    end for;
    debug-out(#"driver", "DONE Verify definition of %s - no change\n", ld);
    #f
  end block;
end function invalidated-library-definition?;

// Retract current library definition if any directly used library definition
// got retracted/changed (or used project got closed).
define function verify-library-definition (ld :: <project-library-description>)
  if (ld.library-description-personal? & invalidated-library-definition?(ld))
    retract-library-parsing(ld);
  end;
end function verify-library-definition;

define thread variable *verify-used-libraries-strictly?* = #t;

define function any-changed-used-library?
    (ld :: <project-library-description>) => (changed? :: <boolean>)
  ld.library-description-personal? &
  any?(method (ul :: <used-library>)
	 let uld :: <project-library-description>
	   =  ul.used-library-description;
	 uld.library-description-closed?
	   | ~uld.compiled-to-definitions?
	   | uld.compilation-definitions-inconsistent?
	   | when (ul.used-library-change-count)
	       let major = uld.library-description-major-version;
	       let minor = uld.library-description-minor-version;
	       let count = uld.library-description-change-count;
	       debug-assert(count ~== ul.used-library-change-count |
			      (major == ul.used-library-major-version &
				 minor == ul.used-library-minor-version),
			    "Change count didn't change but version did?");
	       when (if (~*verify-used-libraries-strictly?*
			   | library-dynamically-bound-in?(ld, uld))
		       major ~== ul.used-library-major-version |
			 // Recompile if reverted to earlier version
			 minor < ul.used-library-minor-version;
		     else
		       count ~== ul.used-library-change-count;
		     end)
		 debug-out(#"driver", "retracting %s because %s changed:"
			     "dynamic binding?: %s,"
			     "count: %s-%s, major: %s-%s, minor: %s-%s\n",
			   ld.library-description-project,
			   uld.library-description-project,
			   library-dynamically-bound-in?(ld, uld),
			   ul.used-library-change-count, count,
			   ul.used-library-major-version, major,
			   ul.used-library-minor-version, minor);
		 #t
	       end when;
	     end when;
       end method,
       ld.used-library-table);
end function any-changed-used-library?;

// This is only called just before parsing, so it not only verifies,
// but also records used library versions if this is the first use.
define function verify-used-libraries (ld :: <project-library-description>)
  local method record-first-use (ul :: <used-library>)
	  when (ul.used-library-change-count == #f) // first time?
	    let uld :: <project-library-description>
	      = ul.used-library-description;
	    ul.used-library-major-version
	      := uld.library-description-major-version;
	    ul.used-library-minor-version
	      := uld.library-description-minor-version;
	    ul.used-library-change-count
	      := uld.library-description-change-count;
	  end when;
	end method;
  local method any-invalidated-used-version? (ld)
	  compilation-definitions-inconsistent?(ld) |
	    invalidated-library-definition?(ld) |
	    any-changed-used-library?(ld)
	end;
  debug-assert(~any?(any-invalidated-used-version?,
		     ld.all-used-library-descriptions),
	       "Dev env didn't make used libraries %s consistent!",
	       choose(any-invalidated-used-version?,
		      ld.all-used-library-descriptions));
  if (any-invalidated-used-version?(ld))
    debug-assert(ld.library-description-personal?,
		 "System library %s definition invalidated", ld);
    retract-library-parsing(ld);
  else
    do(record-first-use, ld.used-library-table);
  end;
end function verify-used-libraries;  

define inline function note-compilation-from-definitions-started
    (ld :: <compilation-context>)
  unless (ld.compilation-from-definitions-started?)
    // When first start using models, record the used library model versions.
    // This means that the earliest used model versions are what's recorded,
    // even if the used library changes in the mean-time.
    record-inter-library-model-use(ld);
    ld.compilation-from-definitions-started? := #t;
  end;
end function;

define method record-inter-library-model-use (ld :: <compilation-context>)
end;

define function library-references-retracted-models?
    (ld :: <project-library-description>)
  ld.compilation-from-definitions-started?
   & block (return)
       for (vers keyed-by uld in ld.all-inter-library-bindings)
	 unless (vers == #"loose" |
		   vers == uld.library-description-models-change-count)
	   debug-assert(instance?(vers, <integer>), "uninited model use info");
	   return(#t);
	 end unless;
       end for;
       #f
     end block;
end function;

define method library-description-used-descriptions
    (ld :: <library-description>)
  let used = ld.used-library-table;
  let descs = make(<vector>, size: size(used));
  for (ul in used, index from 0 by 1)
    descs[index] := ul.used-library-description;
  end;
  descs
end method;

define method all-inter-library-bindings (ld :: <library-description>)
 => (inter-library-bindings :: <table>)
  let lib :: <full-library> = ld.language-definition;
  lib.cached-all-inter-library-bindings
    | (lib.cached-all-inter-library-bindings
	 := compute-inter-library-bindings(ld))
end method;

define function compute-inter-library-bindings (ld :: <library-description>)
  let table = make(<table>);	      
  // A (possibly indirectly) used library is tightly bound
  // iff there is at least one use-path to it in which all the
  // inter-library bindings are tight.
  for (ul in ld.used-library-table)
    let uld = ul.used-library-description;
    let link = if (element(table, uld, default: #f) == #"tight" |  
		     (ul.used-library-binding == #"tight"
			// It doesn't make sense to tightly bind
			// to a loosely-compiled library.
			& ~uld.library-forms-dynamic?))
		 #"tight"
	       else
		 #"loose"
	       end;
    table[uld] := link;
    for (sub-uld in uld.all-used-library-descriptions)
      unless (element(table, sub-uld, default: #f) == #"tight")
	table[sub-uld] := if (link == #"loose" |
				library-dynamically-bound-in?(uld, sub-uld))
			    #"loose"
			  else
			    #"tight"
			  end;
      end;
    end;
  end;
  table
end function;
  

// Returns all library descriptions used by ld, plus ld itself, sorted
// so that a library preceeds all the libraries it uses.  ld itself is
// first in the list.  The return value is explicitly a list so you can
// use .tail to get just the used library descriptions.
define method all-library-descriptions (ld :: <project-library-description>)
 => (l :: <list>)
  let lib = ld.language-definition;
  lib.cached-all-library-descriptions
    | (lib.cached-all-library-descriptions
	 := compute-all-library-descriptions(ld))
end method;

define method compute-all-library-descriptions (ld :: <project-library-description>)
 => (l :: <list>)
  iterate collect-ld (ld :: <library-description> = ld, used = #())
    if (member?(ld, used))
      used
    else
      pair(ld,
	   reduce(method (used, ul :: <used-library>)
		    collect-ld (ul.used-library-description, used)
		  end,
		  used,
		  ld.used-library-table));
    end
  end iterate
end method;  

define inline function all-used-library-descriptions (ld :: <library-description>)
 => (l :: <list>)
  ld.all-library-descriptions.tail
end function;

define function directly-used-library-descriptions (ld :: <library-description>)
 => (l :: <vector>)
// Fails in the emulator at least.
//  map-as(<vector>, used-library-description, ld.used-library-table);
  let uls = ld.used-library-table;
  let v = make(<vector>, size: size(uls));
  for (ul in uls, n from 0 by 1) v[n] := used-library-description(ul) end;
  v
end function;

define method lookup-module-in 
    (library :: <library>, name, #key default = unsupplied()) => (module)
  let binding = lookup-name(library, name, default: not-found());
  if (found?(binding) & defined?(binding))
    binding.library-binding-value
  elseif (supplied?(default))
    default
  else
    error("Module %= not defined in %= and no default was supplied.",
          name, library);
  end
end method;


define method undefined-module-bindings-in 
    (library :: <library>) => (bindings :: <sequence>)
  collecting ()
    for (binding in library.namespace-local-bindings)
      if (~defined?(binding))
        collect(binding);
      end;
    end;
  end;
end method;

define method defined-modules-in
    (library :: <library>) => (modules :: <sequence>)
  collecting ()
    for (binding in library.namespace-local-bindings)
      if (defined?(binding))
        collect(binding.library-binding-value);
      end;
    end;
  end;
end method;

define method definer-references (definer-binding :: <canonical-module-binding>)
  debug-assert(valid-binding-home-library?(definer-binding));
  let lib :: <full-library> = current-library-model();
  element(lib.library-definer-references, definer-binding, default: #())
end;

// This is never used, because 'definer-references' is only used when triggering
// dependencies on redefinition, and we can't redefine imported bindings.
// define method definer-references (definer-binding :: <imported-module-binding>)
//   let library = current-library-model();
//   let definer-binding = binding-canonical-binding(definer-binding);
//   reduce(method (refs, lib)
// 	   let db = local-binding-in(lib, definer-binding, default: #f);
// 	   let new-refs = if (db) element(lib.library-definer-references, db,
// 					  default: #())
// 			  else #() end;
// 	   if (new-refs == #())
// 	     refs
// 	   else
// 	     reduce(method (refs, binding)
// 		      let b = local-binding-in(library, binding, default: #f);
// 		      if (b) add-new!(refs, b) else refs end
// 		    end,
// 		    refs,
// 		    new-refs)
// 	   end;
// 	 end,
// 	 #(),
// 	 all-used-namespaces(library))
// end;

// TODO: What do I have to do with dependency tracking here?

define function add-library-wildcard-subclass-definition
    (library :: <full-library>, def :: <defining-form>) => ()
  library-wildcard-subclass-definitions(library)
    := pair(def, library-wildcard-subclass-definitions(library));
end function;

define function remove-library-wildcard-subclass-definition
    (library :: <full-library>, def :: <defining-form>) => ()
  library-wildcard-subclass-definitions(library)
    := remove!(library-wildcard-subclass-definitions(library), def);
end function;

define method library-contains-wildcard-subclasses? 
    (library :: <full-library>) => (well? :: <boolean>)
  ~empty?(library-wildcard-subclass-definitions(library))
end;

//// Library space.

// The following definitions allow a "current library" to be specified 
// during compilation. Convenient functions are defined for defining modules
// and looking up their definitions in the current module.

// Note: Working hygiene may make thisobsolete since program
// fragments will carry with them the module context from which they
// originate, and through that the library may be determined. Some code,
// though, perhaps sealing?, may find it convenient to work in terms of
// the current library.

define thread variable *library-description* :: false-or(<library-description>) = #f;

define inline sideways method current-library-description () => (false-or-ld)
  *library-description* 
end method;

define inline sideways method current-library-description?
  (ld :: <library-description>) => (well? :: <boolean>)
  ld == *library-description*
end method;

define method current-library-in-context?
   (ld :: <library-description>) => (well? :: <boolean>)
  current-library-description?(ld)
   | begin
       let cx = *library-description*;
       instance?(cx, <interactive-library-description>)
	 & ld == cx.interactive-library-project-library
     end
end method;

// Note that this is false if we're just interactively compiling something
// in some dylan library module.
define sideways method compiling-dylan-library? () => (well? :: <boolean>)
  ~*interactive-compilation-layer* &
    dylan-library-library-description?(current-library-description())
end method;

define sideways method current-back-end-name () => (name :: false-or(<symbol>))
  let ld = current-library-description();
  if (ld)
    library-description-compiler-back-end-name(ld);
  end;
end method;

define sideways method current-compilation-mode () => (mode :: <symbol>)
  library-description-compilation-mode(current-library-description())
end method;

define sideways method current-processor-name () => (name :: <symbol>)
  library-description-processor-name(current-library-description())
end method;

define sideways method current-os-name () => (name :: <symbol>)
  library-description-os-name(current-library-description())
end method;

define sideways method form-dynamic? (form :: <top-level-form>)
 => (well? :: <boolean>)
  form-interactive?(form) |
  form-evaluation-tried-and-failed?(form) | 
  begin
    let ld = form-library(form);
    if (current-library-description?(ld))
      library-forms-dynamic?(ld)
    else
      // TODO: in interactive compiles, we need to loosely bind to any library
      // that allows interactive redefinition.
      library-dynamically-bound?(ld);
    end if
  end;
end method;

define inline function library-dynamically-bound? (ld :: <library-description>)
  library-dynamically-bound-in?(current-library-description(), ld)
end;

define method library-dynamically-bound-in?
    (ld :: <project-library-description>, uld :: <project-library-description>)
  ld.all-inter-library-bindings[uld] == #"loose"
end;

define method record-inter-library-model-use
    (ld :: <project-library-description>)
  let table = ld.all-inter-library-bindings;
  for (binding keyed-by uld in table)
    unless (binding == #"loose")
      table[uld] := uld.library-description-models-change-count;
    end;
  end for;
end method;

define thread variable 
  *top-level-library-description* :: false-or(<library-description>) = #f;

define sealed sideways inline method current-top-level-library-description
    () => (false-or-library)
  *top-level-library-description* 
end method;

define sealed sideways inline method current-top-level-library-description?
  (ld :: <library-description>) => (well? :: <boolean>)
  ld == *top-level-library-description*
end method;

define inline function do-with-top-level-library-description
    (code :: <function>, library :: <library-description>)
  dynamic-bind (*top-level-library-description* = library)
    code()
  end
end function;

define inline function do-with-library-description
    (code :: <function>, library :: <library-description>)
  dynamic-bind (*library-description* = library)
    code()
  end
end function;

define inline function current-library-model ()
  let ld = current-library-description();
  if (~ld)
    error("No current library context selected");
  end;
  ld.language-definition
end function;

define inline function current-library-defined? () => (well? :: <boolean>)
  let ld = current-library-description();
  ld & library-description-defined?(ld)
end function;

define inline function current-library-stripable? () => (well?)
  // This is now encoded in the stripped? slot.
  current-library-description().library-description-stripped?
end function;

define method lookup-module (name, #rest options) => (value)
  apply(lookup-module-in, current-library-model(), name, options);
end method;

//// Boot libraries.

// TODO: need to arrange to disallow module definitions in boot library
// (i.e. module definitions can't precede library definition).
define abstract class <boot-library> (<library>)
end;

define class <full-boot-library> (<full-library>, <boot-library>)
  constant slot namespace-original-library :: false-or(<library-description>),
    required-init-keyword: description:;
end;

define method namespace-library-description (lib :: <full-boot-library>)
  let lld = lib.namespace-original-library;
  library-description-in-context(current-library-description(), lld)
end method;

define method namespace-name (library :: <full-boot-library>) => (name :: <symbol>)
  #"undefined-library"
end method;

define function library-description-defined? (ld :: <library-description>)
  let lib = ld.language-definition;
  lib & ~instance?(lib, <boot-library>)
end function;

define function ensure-language-definition (ld :: <library-description>)
  unless (ld.language-definition)
    unless (ld.library-description-dylan-library)
      ld.library-description-dylan-library
	:= if (dylan-library-library-description?(ld))
	     ld
	   else
	     // If this isn't the Dylan library, and the Dylan library isn't
	     // loaded into the compiler yet, load it up so that we can
	     // generate dylan-user for the library being compiled and can
	     // convert "define library" in order to determine its other
	     // dependencies.
	     let dld = find-used-library-description(ld, #"dylan",
						     canonicalize?: #f);
	     ensure-language-definition(dld);
	     dld
	   end;
    end;
    with-inconsistent-definitions (ld)
      let use-dylan = if (dylan-library-library-description?(ld))
			#[]
		      else
			vector(make(<use-clause>, use: #"dylan"))
		      end;
      let library = make-namespace(<full-boot-library>,
				   description: ld,
				   definition: #f,
				   use-clauses: use-dylan);
      ld.language-definition := library;
      let module
	= make-namespace(<full-dylan-user-module>,
			 library: library,
			 debug-name: #"dylan-user",
			 definition: #f,
			 use-clauses: use-dylan,
			 create-clauses: #[],
			 export-clauses: #[]);
      define-module!(module);
    end with-inconsistent-definitions;
  end unless;
end function;

//// Library contexts.  This is the exported interface.

/// SEE LIBRARY-DESCRIPTION FOR WITH-LIBRARY-CONTEXT

define method do-with-library-context (f, desc)
  let curr-desc = *library-description*;
  if (desc == curr-desc)
    f()
  else
    debug-assert(~curr-desc | 
		   if (instance?(curr-desc, <interactive-library-description>))
		     // It's legit so switch to a project library for the
		     // purposes of looking up a dood lazy slot.
		     // instance?(desc, <interactive-library-description>)
		     #t
		   else
		     instance?(desc, <project-library-description>)
		   end,
		 "Invalid context switch from %s to %s", curr-desc, desc);
    debug-assert(~*interactive-compilation-layer*,
		 "Changing libraries during interactive compilation?");
    with-library-description (desc)
      ensure-language-definition(desc);
      // Previous dependent can't be valid anymore so revert to
      // unintialized state, unless without dependency tracking, then
      // stay that way.
      if (*current-dependent* == #"no-dependent")
	f()
      else
	dynamic-bind(*current-stage* = #f, *current-dependent* = #f)
	  f();
        end;
      end if;
    end;
  end;
end method;

define macro with-dependent-context
  { with-dependent-context (?stage:expression of ?dependent:expression)
     ?:body end }
    => { do-with-dependent-context (?stage, ?dependent, method () ?body end) }
end macro;

define function do-with-dependent-context (stage, dependent, body)
  if (*current-stage* == stage & *current-dependent* == dependent)
    body()
  else
    let cr = compilation-record-of(dependent);
    if (*interactive-compilation-layer* & cr.compilation-record-downloaded?)
      // format-out("Changing to downloaded dependent %s"
      //            " during interactive compilation\n", dependent);
      dynamic-bind (*interactive-compilation-layer* = #f)
	do-with-dependent-context(stage, dependent, body);
      end;
    else
      let ld = compilation-record-library(cr);
      // Trust only switch to an unshadowed library for safe recomputations,
      // i.e. nothing that creates new/invalid properties.
      let ld = if (instance?(ld, <interactive-library-description>)
		     & ~ld.interactive-library-shadowed?)
		 compilation-record-original-library(cr);
	       else
		 ld
	       end;
      with-library-context (ld)
	with-dependent (stage of dependent)
	  body()
	end with-dependent
      end with-library-context
    end
  end
end;


define function define-library! (library :: <full-library>)
  let ld = namespace-library-description(library);
  replace-library(ld.language-definition, library);
end;

define method replace-library (boot :: <full-boot-library>, new :: <full-library>)
  new.imported-bindings-tables := boot.imported-bindings-tables;
  new.library-definer-references := boot.library-definer-references;
  let dylan-user = lookup-module-in(boot, #"dylan-user");
  undefine-module!(dylan-user);
  let library-definition = new.namespace-definition;
  debug-assert(*current-dependent* == library-definition);
  // Make a fake definition for browsers to use.
  with-form-creation
    dylan-user.namespace-definition
      := make-module-definition(name: #"dylan-user",
				parent-form: library-definition,
				use-clauses: dylan-user.use-clauses);
  end;
  dylan-user.original-home-library := new;
  dylan-user.namespace-model
    := ^make-<&module>(#"dylan-user", new.namespace-model);
  let ld = namespace-library-description(new);
  reset-language-definition(ld, new);
  define-module!(dylan-user);
  // // TODO:  HACK: Since we currently don't support dynamism in tight mode,
  // // tight libraries which use loosely compiled or loosely bound libraries
  // // are bound to lose... So we force the using library to be loosely compiled.
  // // debug-assert(ld == current-library-description()) - follows from assert above
  // if (~ld.library-forms-dynamic? &
  // 	any?(method (ul)
  // 	       let uld = ul.used-library-description;
  // 	       uld.library-dynamically-bound? | uld.library-forms-dynamic?
  // 	     end, new.used-libraries))
  //   // TODO: Remove this when loose binding between tight mode libraries
  //   // is up and running, when we don't want loose mode to cascade.
  //   ld.library-forms-dynamic? := #t;
  // end;
end method;

//// LIBRARY SPECIFIC LOOKUPS

define method untracked-lookup-local-modifying-definitions 
    (binding :: <module-binding>) => (definitions :: <definitions>)
  binding-local-modifying-definitions(binding)
end method;

define method untracked-lookup-local-modifying-definitions 
    (name) => (definitions :: <definitions>)
  untracked-lookup-local-modifying-definitions(untracked-lookup-binding(name))
end method;

define method untracked-lookup-certain-local-modifying-models
    (binding :: <module-binding>, form-predicate :: <function>)
 => (models :: <models>)
  binding-certain-local-modifying-models(binding, form-predicate)
end method;

define method untracked-lookup-certain-local-modifying-models
    (name, form-predicate :: <function>) => (models :: <models>)
  untracked-lookup-certain-local-modifying-models
    (untracked-lookup-binding(name), form-predicate)
end method;

//// DOOD-CROSS-LIBRARY-PROXY

define class <dood-cross-library-proxy> (<dood-proxy>)
  constant slot dood-proxy-library-name,  required-init-keyword: library-name:;
end class;

define sealed domain make (subclass(<dood-cross-library-proxy>));
define sealed domain initialize (<dood-cross-library-proxy>);

define method dood-make-cross-library-proxy
    (dood :: <dood>, object :: <boot-library>) => (proxy)
  // Doesn't have a name, so won't be able to find it by name.
  // Can this ever legitimately happen?  How did we find it in the first place?
  error("Can't dump reference to undefined library %s", object);
end method;

define method dood-make-cross-library-proxy
    (dood :: <dood>, object :: <library>) => (proxy)
  make(<dood-cross-library-proxy>,
       // Actually, should use whatever name *we* used to find it, in case
       // the env. supports rename-on-import.  For now, I don't think the
       // env. does any renaming.
       library-name:  namespace-name(object));
end method;

define method dood-disk-object 
    (dood :: <dood>, object :: <library>)
 => (proxy :: type-union(<dood-cross-library-proxy>, <library>))
  let ld = dood-root(dood);
  if (language-definition(ld) == object)
    next-method();
  else
    dood-as-proxy(dood, object, dood-make-cross-library-proxy)
  end if
end method;

define method dood-lookup-used-library-by-name (dood :: <dood>, name)
  let ld = dood-root(dood);
  let lib = ld.language-definition;
  debug-out(#"gz", "Restore xlib proxy %s -> %s, direct=%s",
	    ld, name, resolve-used-namespace(lib, name, default: #f));
  let ulib = any?(method (ulib) ulib.namespace-name == name & ulib end,
		  lib.all-used-namespaces);
  debug-assert(ulib, "Can't find library model for %s", name);
  ulib
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-cross-library-proxy>) => (object)
  dood-lookup-used-library-by-name(dood, dood-proxy-library-name(proxy))
end method;

//// DOOD-CROSS-LIBRARY-BINDING-PROXY

define class <dood-cross-library-binding-proxy> (<dood-proxy>)
  constant slot dood-proxy-library,     required-init-keyword: library:;
  constant slot dood-proxy-module-name, required-init-keyword: module-name:;
end class;

define sealed domain make (subclass(<dood-cross-library-binding-proxy>));
define sealed domain initialize (<dood-cross-library-binding-proxy>);

define method dood-make-cross-library-binding-proxy
    (dood :: <dood>, object :: <library-binding>) => (proxy)
  make(<dood-cross-library-binding-proxy>, 
       library:     library-binding-home(object),
       module-name: name(object))
end method;

define method dood-disk-object 
    (dood :: <dood>, object :: <library-binding>)
 => (proxy :: type-union(<dood-cross-library-binding-proxy>, 
                         <library-binding>))
  if (language-definition(dood-root(dood)) == library-binding-home(object))
    next-method();
  else
    // This can come up in the imported-name-cache
    dood-as-proxy(dood, object, dood-make-cross-library-binding-proxy)
  end if
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-cross-library-binding-proxy>) => (object)
  lookup-name(dood-proxy-library(proxy), dood-proxy-module-name(proxy))
end method;

//// DOOD-CROSS-MODULE-BINDING-PROXY
//// HACK: HERE BECAUSE OF WITH-LIBRARY-CONTEXT

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-cross-module-binding-proxy>) => (object)
  let module = dood-proxy-module(proxy);
  let ld = namespace-library-description(module);
  with-dood-context (ld)
    untracked-lookup-binding-in
      (module, dood-proxy-variable-name(proxy))
  end with-dood-context;
end method;

//// HACK: THIS HANDLES FRAGMENTS THAT ARE LEAKED ACROSS LIBRARIES BY
////       THE C-FFI.  DESTRUCTIVELY CLEAR OUT CROSS LIBRARY INFO.
////       COULD INSTEAD COPY THE FRAGMENT WITHOUT INFO.

define sideways method dood-disk-object
    (dood :: <dood>, object :: <fragment>) => (object)
  block (special-return)
    let cr = fragment-record(object);
    if (cr)
      let cr-ld = compilation-record-original-library(cr);
      // EXTERNALLY REFERENCED FRAGMENT?
      if (cr-ld & cr-ld ~== dood-root(dood))
	without-dependency-tracking 
	   with-expansion-module (compilation-record-module(cr))
	     special-return(default-in-expansion(object));
	   end;
        end without-dependency-tracking;
      end if;
    end if;
    dood-disk-object-default(dood, object)
  end;
end method;
