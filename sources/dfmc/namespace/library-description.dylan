Module: dfmc-namespace
Author: Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $dfmc-dood-version = 240;

//// Abstract library descriptions.

define abstract dood-class <library-description> (<compilation-context>)
  lazy slot language-definition = #f;
  // This is a timestamp for changes in the (pointer) value of language-
  // definition, i.e. it tracks retractions of the library definition.
  weak slot language-definition-change-count :: <integer> = 0,
    reinit-expression: 0;
  // Table mapping source records to compilation records.
  weak slot cached-library-description-record-table :: false-or(<table>) = #f,
    reinit-expression: #f;
  // Type inference results 
  weak slot library-type-cache = #f, 
    reinit-expression: #f;
  weak slot library-type-estimate-disjoint?-cache = #f,
    reinit-expression: #f;
  weak slot library-type-estimate-cons-cache = #f,
    reinit-expression: #f;
  weak slot library-type-estimate-dispatch-cache = #f,
    reinit-expression: #f;
  // Cached inter-library model lookup.
  weak slot library-external-model-cache :: <table> = make(<table>),
    reinit-expression: make(<table>);

  weak slot library-description-system-class-init-code = #f,
    reinit-expression: #f;
  weak slot library-description-system-gf-init-code = #f, 
    reinit-expression: #f;

  weak slot library-owned-model-properties :: <table> = make(<table>),
    reinit-expression: make(<table>);
  lazy slot library-conditions-table :: <table> = make(<table>);
  // The Dylan library description used by this library (different
  // libraries may use different ones, e.g. for different targets)
  weak slot library-description-dylan-library = #f;
  // True if this library is loosely compiled.
  slot library-forms-dynamic? :: <boolean> = #f;

  // call-site unique id counter
  slot library-description-current-call-site-id :: <integer> = 0;

  weak slot library-externally-visible-models :: false-or(<object-set>) = #f,
    reinit-expression: #f;
end;

define function library-generate-call-site-id 
    (ld :: <library-description>) => (res :: <integer>)
  let uid = library-description-current-call-site-id(ld);
  library-description-current-call-site-id(ld) := uid + 1;
  uid
end function;

define inline function library-description-compilation-records
    (ld :: <library-description>) => (cr* :: <vector>)
  compilation-context-records(ld)
end function;

define function library-description-model (ld :: <library-description>)
 => (model)
  let library :: <library> = language-definition(ld);
  namespace-model(library)
end function;

define method library-description-record-table
    (ld :: <library-description>) => (table :: <table>);
  ld.cached-library-description-record-table |
    begin
      let table = make(<table>);
      for (cr in ld.library-description-compilation-records)
	table[cr.compilation-record-source-record] := cr
      end;
      ld.cached-library-description-record-table := table
    end
end method;

define generic library-combined-back-end-data (ld :: <library-description>);

define dood-class <project-library-description> 
    (<dood-mapped-and-owned-object>, <library-description>)
  lazy slot library-description-interface-spec = #f;
  slot library-description-interface-version = #f;
  // A pseudo database version number, incremented any time the sources
  // change.  Really only meaningful if definitions-installed? is true.
  slot library-description-change-count :: <integer> =  0;
  slot library-description-saved-change-count :: <integer> = 0;
  // Change count for detecting stale cross-library model pointers.
  // Incremented any time models are retracted.  Should only be compared
  // if library-description-change-count is the same.
  slot library-description-models-change-count = 0;
  // library version
  // has to be eq to be compatible
  slot library-description-major-version = 0, init-keyword: major-version:;
  // backwards compatible has to be 'greater then or equal'
  slot library-description-minor-version = 0, init-keyword: minor-version:;
  // If non-zero, the indicated library pack must be present in order
  // to use this library.  (We check when opening the database...)
  slot library-description-library-pack = 0, init-keyword: library-pack:;
  // Flag used to notice when compilation is aborted, so know to start over
  // next time.  This is in place of doing the cleanup at the time of the
  // abort, because library cleanup can be time consuming so that it has a
  // non-negligible chance of being interrupted itself!
  slot library-description-compilation-aborted? :: <boolean> = #f;
  weak slot library-description-database-location,
    required-init-keyword: location:,
    reinit-expression: #f;
  lazy slot library-description-compiler-back-end-slot :: false-or(<symbol>) = #f;
  lazy slot library-description-os-name-slot :: <symbol> = #"unknown";
  lazy slot library-description-architecture-name-slot :: <symbol> = #"unknown";
  lazy slot library-description-compilation-mode-slot :: <symbol> = #"tight";
  lazy slot library-description-build-location-slot = #f;
  lazy slot library-description-profile-location = #f,
    init-keyword: profile-location:;
  lazy constant slot library-description-build-settings :: <sequence> = #(),
    init-keyword: build-settings:;
  lazy slot library-description-last-build-info = #f;
  weak slot library-description-project,
    required-init-keyword: project:,
    reinit-expression: #f;
  weak slot library-description-dood = #f, init-keyword: dood:;
  // heap and back end data for combined compilation...
  lazy slot library-description-combined-record :: false-or(<library-compilation-record>) = #f;
  // True if incremental slots have been stripped from the library
  // .see strip-incremental-slots
  // Value is #"pending" if being compiled for stripping but haven't gotten
  // to actual stripping yet.
  slot library-description-stripped? = #f;
  weak slot project-library-interactive-contexts = #(),
    reinit-expression: #();
end dood-class <project-library-description>;

define method library-combined-back-end-data (ld :: <project-library-description>)
  let cr = ld.library-description-combined-record;
  cr & cr.compilation-record-back-end-data;
end;

define method compilation-context-object-names (ld :: <compilation-context>)
  map-as(<vector>, compilation-record-name, compilation-context-records(ld))
end;

define method compilation-context-object-names (ld :: <project-library-description>)
  if (ld.library-description-combined-record)
    vector(ld.library-description-combined-record.compilation-record-name)
  else
    map-as(<vector>, compilation-record-name, compilation-context-records(ld))
  end;
end;

define thread variable *dood-current-project* = #f;

define compiler-open generic dood-boot-mapped-objects 
    (dood :: <dood>, proxies, ld :: <project-library-description>) 
 => ();

define method dood-boot-mapped-objects 
    (dood :: <dood>,
     proxies :: type-union(<list>, <project-library-description>),
     ld :: <project-library-description>) 
 => ()
end method;

define sealed class <dfmc-dood> (<dood>) end;

define open generic dood-dfmc-initial-segments // method in management/world
    (class :: <class>) => (segments, default-segment);

define method dood-reinitialize 
    (dood :: <dfmc-dood>, object :: <project-library-description>) => ()
  next-method();
  // mapped object proxies needing booting are stashed in dood-root
  let mapped-object-proxies = dood-root(dood) | #();
  // first install ld
  dood-root(dood) := object;
  library-description-project(object) := *dood-current-project*;
  library-description-saved-change-count(object) 
    := library-description-change-count(object);
  library-description-dylan-library(object)
    := if (dylan-library-library-description?(object))
	 object
       else
	 find-used-library-description(object, #"dylan", canonicalize?: #f);
       end;
  initialize-typist-library-caches(object);
  // then boot mapped-object-proxies
  library-description-dood(object) := dood;
  dood-boot-mapped-objects(dood, mapped-object-proxies, object);
end method;

// A description of the dylan library
define abstract dood-class <dylan-library-description> (<library-description>)
  weak constant slot library-description-dylan-value-cache :: <object-table> 
      = make(<object-table>),
    reinit-expression: make(<object-table>);
  weak slot library-description-dfm-copier :: false-or(<copier>) = #f,
    reinit-expression: #f;
  weak slot library-description-value-model-copier :: false-or(<copier>) = #f,
    reinit-expression: #f;
  weak slot %library-description-object-expression = #f,
    reinit-expression: #f;
  weak slot %library-description-false-expression = #f,
    reinit-expression: #f;
  weak slot %library-description-default-value-rest-spec = #f,
    reinit-expression: #f;
  constant slot library-booted-model-properties :: <object-table>
    = make(<object-table>);
end dood-class;

define dood-class <dylan-project-library-description>
    (<dylan-library-description>, <project-library-description>)
end dood-class;

define method dylan-library-library-description? (ld :: <library-description>)
  #f
end;

define method dylan-library-library-description? (ld :: <dylan-library-description>)
  #t
end;


define variable *write-databases?* = #t;
define variable *read-databases?*  = #t;

define function read-databases? () => (res :: <boolean>)
  *read-databases?*
end function;

define function write-databases? () => (res :: <boolean>)
  *write-databases?*
end function;

define function use-databases? () => (res :: <boolean>)
  *read-databases?* | *write-databases?*
end function;

define function read-databases?-setter (setting :: <boolean>) => ()
  *read-databases?* := setting;
end function;

define function write-databases?-setter (setting :: <boolean>) => ()
  *write-databases?* := setting;
end function;

define function use-databases?-setter (setting :: <boolean>) => ()
  write-databases?() := setting;  
  read-databases?()  := setting;  
end function;

define compiler-open generic install-dylan-boot-constants 
  (ld :: <dylan-library-description>, #key force?);

/// FOUR STATES:
///        OFF WRITE READ ON
/// READ   NO  NO    YES  YES
/// WRITE  NO  YES   NO   YES
/// 

define method copy-in-booted-model-properties (ld :: <dylan-library-description>)
  for (properties keyed-by model in library-booted-model-properties(ld))
    install-owned-model-properties-in(ld, model, properties);
  end for;
end method;

define function make-library-description
    (project, #key database-location, profile-location, build-settings, read-only?, load-namespace?)
  let dood-location = database-location;
  let dood-exists? = dood-location & file-exists?(dood-location);
  if (~use-databases?() | ~dood-location
       | (~write-databases?() & ~dood-exists?))
    debug-out(#"driver", "Creating new LD for %s, dood-loc=%s\n", project, dood-location);
    values(make(if (dood-location & dylan-library-database?(dood-location))
		  <dylan-project-library-description>
		else
		  <project-library-description>
		end,
		project: project,
		location: dood-location,
		profile-location: profile-location,
		build-settings: build-settings),
	   #f)
  else
    debug-out(#"driver", "Loading LD (%s) for %s from %s\n",
	      if (dood-exists? & read-only?) "input" else "input-output" end,
	      project, dood-location);
    dynamic-bind (*dood-current-project* = project)
      let (segments, default-segment)
        = dood-dfmc-initial-segments(<dfmc-dood>);
      let dood 
        = make(<dfmc-dood>, 
	       name:      as(<symbol>, locator-base(as(<file-locator>, dood-location))),
	       locator:   dood-location,
	       version:   $dfmc-dood-version,
	       if-exists: if (dood-exists?) #f else #"replace" end,
	       direction: if (dood-exists? & read-only?)
			    #"input"
			  else
			    #"input-output"
			  end,
	       segments:        segments,
	       default-segment: default-segment);
      let ld = dood-root(dood);
      if (ld)
	debug-out(#"driver", " [Got vers %s of %s]\n",
		  ld.library-description-change-count, ld);
	debug-assert((dylan-library-database?(dood-location) & #t)
		       == (dylan-library-library-description?(ld) & #t));
	ld.library-description-database-location := dood-location;
	ld.library-description-profile-location  := profile-location;
	when (read-only?)
	  ld.library-description-build-location := #f;
	end;
	ld.library-description-project := project;
	ld.library-description-dood := dood;
	when (load-namespace? &
		ld.language-definition &
		~ld.compilation-definitions-inconsistent?)
	  // Force loading of used-library tables all the way down, to
	  // establish all name -> ld mappings now so never have to ask the
	  // project manager later.
	  all-library-descriptions(ld);
	  // Invalidate if anything changed with used libraries.
	  // TODO: We don't actually retract since we don't really have
	  // permission to do compilation-like stuff at this point.  But
	  // maybe it doesn't matter, and we should retract?
	  when (any-changed-used-library?(ld))
	    ld.compilation-definitions-inconsistent? := #t;
	  end;
	end;
	if (dylan-library-library-description?(ld))
	  install-dylan-boot-constants(ld, force?: #f);
	  copy-in-booted-model-properties(ld);
	end;
	values(ld, dood-exists?)
      else
	debug-out(#"driver", " [No ld in DB, making new one]\n");
	// TODO: this needs to be a user warning, and needs to fix 
	// itself up.
	// if (dood-exists?) error("Bogus database in %s", dood-location) end;
	values(dood-root(dood)
		 := make(// TODO: this check should not happen for users, just for
			 // developers!  Users shouldn't compile the dylan library...
			 if (dylan-library-database?(dood-location))
			   <dylan-project-library-description>
			 else
			   <project-library-description>
			 end,
			 project: project,
			 location: dood-location,
			 profile-location: profile-location,
			 build-settings: build-settings,
			 dood: dood),
	       dood-exists?)
      end if
    end dynamic-bind
  end
end function;


define method close-library-description (ld :: <project-library-description>)
  if (~empty?(ld.project-library-interactive-contexts))
    error("Bug: cannot close a compilation context "
	    "with interactive contexts open")
  end;
  ld.library-description-built? := #f;
  ld.library-description-build-location := #f;
  ld.library-description-database-location := #f;
  ld.library-description-profile-location := #f;
  ld.library-description-change-count := -1;
  if (library-description-dood(ld))
    dood-close(library-description-dood(ld), abort?: #t)
    // close(library-description-dood(ld), abort?: #t)
  end if;
end method;

define function library-description-closed? (ld :: <project-library-description>)
  ld.library-description-change-count == #f;
end function;


// TODO: In the future will be able to look in the actual database,
//  so this is a temp. kludge.
define function dylan-library-database? (location :: <locator>)
  locator-base(location) = "dylan"
end function;

// Method is over in the typist.  Done this way because of module lossage, 
// i.e. typist classes are not available here so can't do as slot initializers.
define compiler-open generic initialize-typist-library-caches (ld :: <compilation-context>)
  => ();

define method initialize
    (ld :: <library-description>, #rest initargs, #key, #all-keys)
  next-method();
  initialize-typist-library-caches(ld);
end;

define function clear-library-warnings (ld)
  ld.library-conditions-table := make(<table>);
end function clear-library-warnings;

define function retract-library-warnings (ld :: <project-library-description>, stages)
  remove-dependent-program-conditions(ld, stages);
  for (cr in ld.library-description-compilation-records)
    remove-dependent-program-conditions(cr, stages);
    do(rcurry(remove-dependent-program-conditions, stages),
       cr.compilation-record-top-level-forms | #());
  end;
end function;

define function retract-compilation-timings 
    (ld :: <compilation-context>) => ()
  ld.compilation-timings := #();
end function;

define function record-compilation-timing-property
    (ld :: <compilation-context>, timing-info) => ()
  ld.compilation-timings
    := pair(timing-info, ld.compilation-timings);
end function;

define function compilation-timing-properties 
    (ld :: <compilation-context>) => (properties :: <sequence>)
  reverse(ld.compilation-timings)
end function;

define function compilation-timing-property?
    (ld :: <compilation-context>, matching-property? :: <function>)
  => (property)
  block(return)
    for (prop in ld.compilation-timings)
      if (matching-property?(prop))
	return(prop);
      end if;
    end for;
  end block;
end function;

define function retract-library-imported-bindings 
    (ld :: <library-description>)
  retract-imported-bindings(language-definition(ld));
end function;

define function retract-library-copiers
    (ld :: <library-description>)
  let dylan-ld = library-description-dylan-library(ld);
  if (dylan-ld)
    library-description-dfm-copier(dylan-ld) := #f;
    library-description-value-model-copier(dylan-ld) := #f;
  end if;
end function;

define function reset-language-definition (ld :: <library-description>,
					   false-or-library)
  let change-count = ld.language-definition-change-count;
  ld.language-definition-change-count
    := if (change-count == $maximum-integer) 1 else 1 + change-count end;
  ld.language-definition := false-or-library;
end;

define function retract-library-parsing (ld :: <project-library-description>)
  detach-interactive-namespaces(ld);
  with-inconsistent-definitions (ld)
    ld.compilation-from-definitions-started? := #f;
    ld.compiled-to-definitions? := #f;
    initialize-typist-library-caches(ld);
    clear-library-warnings(ld);
    remove-all-keys!(ld.library-owned-model-properties);
    ld.library-description-system-class-init-code := #f;
    ld.library-description-system-gf-init-code := #f;
    ld.library-description-combined-record := #f;
    ld.library-externally-visible-models := #f;
    retract-compilation-timings(ld);
    retract-library-copiers(ld);
    for (cr in ld.library-description-compilation-records)
      retract-compilation-record-heap(cr);
      cr.compilation-record-module := #f;
      cr.compilation-record-definitions-installed? := #f;
      cr.compilation-record-top-level-forms := #f;
      cr.compilation-record-dependency-table := make(<table>);
      cr.compilation-record-model-properties := #();
      cr.compilation-record-dispatch-decisions := #();
      cr.compilation-record-source-line-count := #f;
      cr.compilation-record-inline-only-table := make(<table>);
      cr.compilation-record-back-end-data := #f;
    end;
    reset-language-definition(ld, #f);
    // TODO: This is necessary because library-forms-dynamic? gets upgraded based
    // on looseness of used libraries.  So we reset it to the basemark since
    // we just retracted all used library info.
    ld.library-forms-dynamic? := (ld.library-description-compilation-mode == #"loose");
    ld.library-description-compilation-aborted? := #f;
    ld.library-description-stripped? := #f;
  end with-inconsistent-definitions;
  ld.compilation-definitions-inconsistent? := #f;
  ensure-language-definition(ld);
end;

define compiler-open generic retract-library-compilation
  (ld :: <project-library-description>);

// The name used by back-ends to mangle library-specific files
// Note that be the time the back end runs, the library definition
// has been processed, so its given name is known.
define method library-description-emit-name (ld :: <library-description>)
  let library = ld.language-definition;
  library & namespace-name(library)
end method;

define method library-description-compiler-back-end-name
    (project :: <project-library-description>) => (back-end :: false-or(<symbol>))
  project.library-description-compiler-back-end-slot
end method;

define method library-description-compiler-back-end-name-setter
    (back-end :: false-or(<symbol>), project :: <project-library-description>)
  unless (back-end == project.library-description-compiler-back-end-name)
    retract-library-compilation(project);
    project.library-description-compiler-back-end-slot := back-end;
  end;
end method;

define method library-description-os-name
    (project :: <project-library-description>) => (os-name :: <symbol>)
  project.library-description-os-name-slot
end method library-description-os-name;

define function library-description-os-name-setter
    (os :: <symbol>, ld :: <project-library-description>)
  unless (os == ld.library-description-os-name)
    retract-library-compilation(ld);
    ld.library-description-os-name-slot := os;
  end;
end;

define method library-description-architecture-name
    (project :: <project-library-description>) => (architecture-name :: <symbol>)
  project.library-description-architecture-name-slot
end method library-description-architecture-name;

define function library-description-architecture-name-setter
    (architecture :: <symbol>, ld :: <project-library-description>)
  unless (architecture == ld.library-description-architecture-name)
    retract-library-compilation(ld);
    ld.library-description-architecture-name-slot := architecture;
  end;
end;

define method library-description-compilation-mode
    (project :: <project-library-description>)
 => (compilation-mode :: <symbol>)
  project.library-description-compilation-mode-slot
end method library-description-compilation-mode;

define function library-description-compilation-mode-setter
    (mode :: <symbol>, ld :: <project-library-description>)
  unless (mode == ld.library-description-compilation-mode)
    // If "officially" changing to incremental mode, but actually already
    // compiled in incremental (e.g. because of a loose used library),
    // don't need to retract anything.
    unless (mode == #"loose" & ld.library-forms-dynamic?)
      retract-library-compilation(ld);
    end;
    ld.library-description-compilation-mode-slot := mode;
    ld.library-forms-dynamic? := (mode == #"loose");
  end;
end;

define method library-description-build-location
    (ld :: <project-library-description>)
  ld.library-description-build-location-slot
end method;

define function library-description-build-location-setter
    (loc, ld :: <library-description>)
  if (loc)
    ld.library-description-last-build-info := #f;
    ld.library-description-build-location-slot := loc;
    ld.library-description-last-build-info := read-build-srv-file(ld);
  else
    ld.library-description-build-location-slot := #f;
  end;
end function;

define method source-record-compilation-record (ld :: <library-description>,
						sr :: <source-record>,
						#key default = unsupplied())
 => (cr-or-default)
  if (supplied?(default))
    element(ld.library-description-record-table, sr, default: default)
  else
    element(ld.library-description-record-table, sr)
  end;
end method;

define class <mapped-model-properties> (<model-properties>) end;

define inline function lookup-owned-model-properties-in-table
    (model-table :: <table>, model)
 => (p :: false-or(<mapped-model-properties>));
  let value = element(model-table, model, default: #f);
  if (value)
    let (real-value, new?) = dood-maybe-force-slot-value-proxy(value);
    if (new?) // WRITE-BACK
      element(model-table, model) := real-value;
    end if;
    real-value
  end if
end function;

define function lookup-owned-model-properties-in
    (ld :: <library-description>, model)
 => (p :: false-or(<mapped-model-properties>));
  lookup-owned-model-properties-in-table(ld.library-owned-model-properties, model)
end function;

define method find-model-properties-in
    (ld :: <library-description>, model, settable?, #key create? = #t)
 => (p :: false-or(<mapped-model-properties>));
  ignore(settable?);
  lookup-owned-model-properties-in(ld, model) |
    element(ld.library-external-model-cache, model, default: #f) |
    begin
      local method do-find (ld)
	      lookup-owned-model-properties-in(ld, model)
	    end method;
      let p = any?(do-find, all-used-library-descriptions(ld));
      if (p)
	ld.library-external-model-cache[model] := p
      elseif (create?)
	new-mapped-model(model)
      end;
    end;
end method;

define function install-owned-model-properties-in 
    (ld :: <library-description>, model, 
     properties :: type-union(<model-properties>, <dood-address-proxy>))
  let model-table = ld.library-owned-model-properties;
  // DONT OVERRIDE FRESHY WITH STALE DATABASE PROPERTIES
  unless (instance?(properties, <dood-address-proxy>) 
	    & element(model-table, model, default: #f))
    model-table[model] := properties;
  end unless;
end function;

define function new-mapped-model (model)
 => (p :: <mapped-model-properties>)
  debug-assert(~instance?(model, <module-binding>) &
		 ~instance?(model, <byte-character>) &
		 ~instance?(model, <integer>),
	       "Bug: making mapped model properties for %s", model);
  let m = make(<mapped-model-properties>);
  let cr = model-compilation-record(m);
  cr.compilation-record-model-properties
    := pair(model, cr.compilation-record-model-properties);
  let il = *interactive-compilation-layer*;
  if (il)
    debug-assert(~cr.compilation-record-downloaded?);
    il.mapped-model-properties-layer[model] := m
  else
    let ld = compilation-record-library(cr);
    debug-assert(current-library-description?(ld), "model not in current lib?");
    install-owned-model-properties-in(ld, model, m);
  end;
  m
end;

define method clear-library-model-properties 
    (ld :: <project-library-description>)
  do (method(cr) cr.compilation-record-model-properties := #() end,
      ld.library-description-compilation-records);
  let ccr = ld.library-description-combined-record;
  when (ccr) ccr.compilation-record-model-properties := #() end;
  ld.library-externally-visible-models := #f;
  remove-all-keys!(ld.library-owned-model-properties);
  remove-all-keys!(ld.library-external-model-cache);
end method;

define function clear-dependent-model-properties (dep)
  let cr = compilation-record-of(dep);
  let cr-properties = cr.compilation-record-model-properties;
  unless (empty?(cr-properties))
    let il = *interactive-compilation-layer*;
    let model-table = if (il)
			debug-assert(~cr.compilation-record-downloaded?);
			il.mapped-model-properties-layer
		      else
			let ld = compilation-record-library(cr);
			ld.library-owned-model-properties;
		      end;
    cr.compilation-record-model-properties
      := remove!(cr-properties, dep,
		 test: method (object, dep)
			 let props 
			   = lookup-owned-model-properties-in-table(model-table, object);
			 if (~props | props.model-creator == dep)
			   remove-key!(model-table, object);
			   #t
			 end if
		       end method);
  end;
end function;

define method clear-stale-model-properties (ld :: <library-description>)
  // AVOID UNRESOLVED MODEL-PROPERTIES
  // REALLY WANT TO REMOVE ALL PROPERTIES CORRESPONDING
  // TO OBJECTS THAT WEREN'T REACHABLE FROM LD COMMIT
  let model-table = ld.library-owned-model-properties;
  let stale-keys =
    collecting ()
      for (properties keyed-by object in model-table)
	when (instance?(properties, <dood-address-proxy>))
	  collect(object);
	end when;
      end for;
    end collecting;
  for (key in stale-keys)
    remove-key!(model-table, key);
  end for;
end method;

define function strip-library-model-properties
    (ld :: <project-library-description>)
  let properties-table = make(<table>);
  for (properties keyed-by model in library-owned-model-properties(ld))
    let properties = dood-maybe-force-address-proxy(properties);
    when (model-has-definition?(properties))
      properties-table[model] := properties;
    end when;
  end for;
  library-owned-model-properties(ld) := properties-table;
end;

define method form-properties-in-context
    (ld :: <project-library-description>,
     form :: <top-level-form>,
     create?) => (p :: <top-level-form>)
  ignore(create?);
  form
end method;

define method binding-properties-in-context
    (ld :: <project-library-description>,
     b :: <imported-module-binding>,
     create?) => (p :: <imported-module-binding>)
  ignore(create?);
  b
end method;

define method binding-properties-in-context
    (ld :: <project-library-description>,
     b :: <canonical-module-binding>,
     create?) => (p :: false-or(<canonical-module-binding-properties>))
  b.canonical-binding-properties
    | if (create?)
	b.canonical-binding-properties
	  := make(<canonical-module-binding-properties>)
      end
end method;

define method compute-cached-binding-model-object-in
    (ld :: <project-library-description>,
     binding :: <module-binding>,
     definition :: <variable-defining-form>) => (model);
  maybe-compute-and-install-form-model-objects(definition);
  binding-cached-model-object(binding)
end method;

define method compute-cached-form-model-in
    (ld :: <project-library-description>, form :: <modifying-form>)
  maybe-compute-and-install-form-model-objects(form);
  form-model(form)
end method;


define sideways method compilation-record-library
    (cr :: <compilation-record>) => (ld :: <library-description>)
  library-description-in-context(current-library-description(),
				 cr.compilation-record-original-library)
end method;

define method library-description-in-context
    (cx :: <project-library-description>,
     ld :: <project-library-description>)
 => (ld :: <project-library-description>)
  ld
end method;

// HACK: NEEDED FOR DOOD BECAUSE current-library-description() 
//       WON'T YET BE ESTABLISHED

define method library-description-in-context
    (cx == #f,
     ld :: <project-library-description>)
 => (ld :: <project-library-description>)
  ld
end method;

// Provided by the project manager.
define compiler-open generic project-source-record-id(project, sr) => id;
define compiler-open generic project-record-id-source-record(project, id) => sr;
define compiler-open generic project-source-record-name(project, sr)
 => (name :: false-or(<string>));

define function library-record-id-source-record (ld :: <library-description>,
						 id :: <string>) => sr;
  project-record-id-source-record(ld.library-description-project, id)
end function;

define function library-source-record-id (ld :: <library-description>,
					  sr :: <source-record>) => id;
  project-source-record-id(ld.library-description-project, sr)
end function;

define sideways method compilation-record-name
    (cr :: <compilation-record>) => (name :: <string>)
  // interactive compilation records have their own method, this one only
  // applies to project compilation records.
  let ld :: <project-library-description>
    = cr.compilation-record-original-library;
  project-source-record-name(ld.library-description-project,
			     cr.compilation-record-source-record)
    | concatenate("SR",
		  integer-to-string(cr.compilation-record-sequence-number))
end method;

//// Library external visibility predicates.

// Whether a binding or object may be directly referenced from code 
// outside its defining library, a superset of the declared exports
// because of inlining/macros. It is assumed that the objects in
// question have already been determined to have definitions and 
// hence a name to export.

// On Windows, these predicates determine the set of dll exports for
// a library.

define function enable-library-externally-visible-elements
    (ld :: <library-description>) => ()
  library-externally-visible-models(ld) := make(<object-set>);
end function;

define function model-externally-visible? 
    (model :: <object>) => (well? :: <boolean>)
  let model = standard-model-object(model);
  let ld = current-library-description();
  let set = library-externally-visible-models(ld);
  if (set)
    member?(model, set)
  else
    #t // be conservative
  end;
end function;

define function model-externally-visible?-setter
    (well? :: <boolean>, model :: <object>) => (well? :: <boolean>)
  let model = standard-model-object(model);
  let ld = current-library-description();
  let set = library-externally-visible-models(ld);
  if (set)
    if (well?) add!(set, model) else remove!(set, model) end;
  end;
  well?
end function;

//// DOOD stuff.

define class <dood-source-record-proxy> (<dood-proxy>)
  constant slot dood-proxy-source-record-id, required-init-keyword: source-record-id:;
end class;

define method dood-make-source-record-proxy
    (dood :: <dood>, object :: <source-record>) => (proxy)
  make(<dood-source-record-proxy>, 
       source-record-id: library-source-record-id(dood.dood-root, object))
end method;

define sideways method dood-disk-object 
    (dood :: <dood>, object :: <source-record>)
 => (disk-object)
  dood-as-proxy(dood, object, dood-make-source-record-proxy)
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-source-record-proxy>) => (object)
  // May be invoked during dood initialization, when ld project not set yet.
  // In that case, *dood-current-project* has the project.
  let ld = dood-root(dood);
  let project
    = (ld & library-description-project(ld)) | *dood-current-project*;
  project-record-id-source-record
    (project, dood-proxy-source-record-id(proxy))
end method;

define class <dood-compilation-record-proxy> (<dood-source-record-proxy>)
  constant slot dood-proxy-library-name,  
    required-init-keyword: library-name:;
end class;

define method dood-make-compilation-record-proxy
    (dood :: <dood>, object :: <compilation-record>) => (proxy)
  let ld = compilation-record-original-library(object);
  let sr = compilation-record-source-record(object);
  make(<dood-compilation-record-proxy>, 
       source-record-id: library-source-record-id(ld, sr),
       library-name:     namespace-name(language-definition(ld)));
end method;

define sideways method dood-disk-object 
    (dood :: <dood>, object :: <compilation-record>)
 => (disk-object)
  if (dood-root(dood) == compilation-record-original-library(object))
    next-method()
  else
    dood-as-proxy(dood, object, dood-make-compilation-record-proxy)
  end if
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-compilation-record-proxy>) => (object)
  let lib = dood-lookup-used-library-by-name(dood, dood-proxy-library-name(proxy));
  let ld  = namespace-library-description(lib);
  let sr  = project-record-id-source-record
              (library-description-project(ld), dood-proxy-source-record-id(proxy));
  source-record-compilation-record(ld, sr);
end method;

// Back-end support.

define macro with-build-area-output
  { with-build-area-output (?stream:variable = ?ld:expression,
			    #rest ?keys:expression)
      ?body:body
    end }
    => { call-with-build-area-output(method (?stream) ?body end, ?ld, ?keys); }
end macro with-build-area-output;

define method build-area-output-locator (ld :: <library-description>,
					 #key base, name, type,
					 #all-keys)
  let directory = ld.library-description-build-location;
  when (directory)
    ensure-directories-exist(directory);
  end;
  let directory = directory & as(<directory-locator>, directory);
  if (name)
    make(<file-locator>, directory: directory, name: name)
  else
    make(<file-locator>, directory: directory, base: base, extension: type)
  end
end method build-area-output-locator;

define function call-with-build-area-output (fn :: <function>,
					     ld :: <library-description>,
					     #rest keys,
					     #key base, name, type)
  let locator = apply(build-area-output-locator, ld, keys);
  if (locator)
    with-open-file (stream = locator, direction: #"output", stream-lock: #f)
      fn(stream)
    end;
  else
//     // Record the output in memory, partly so could write it out to
//     // files once do get a build area location, partly so testing can
//     // examine results.
//     let key = name | pair(base, type);
//     let db-alist = ld.library-description-build-output;
//     let db-slot = any?(method(s) s.head = key & s end, db-alist) |
//		     begin
//		       let s = pair(key, #f);
//		       ld.library-description-build-output := pair(s, db-alist);
//		       s
//		     end;
//     block ()
//       let stream = make(<string-output-stream>, direction: #"output");
//       fn(stream);
//     cleanup
//       force-output(stream);
//       db-slot.tail := stream-contents(stream);
//       close(stream);
//     end block;
  end if;
end function call-with-build-area-output;

define macro with-profile-area-output
  { with-profile-area-output (?stream:variable = ?ld:expression,
			    #rest ?keys:expression)
      ?body:body
    end }
    => { call-with-profile-area-output(method (?stream) ?body end, ?ld, ?keys); }
end macro with-profile-area-output;

define method profile-area-output-locator (ld :: <library-description>,
					   #key base, name, type,
					   #all-keys)
  let directory = ld.library-description-profile-location;
  when (directory)
    ensure-directories-exist(directory);
  end;
  let directory = directory & as(<directory-locator>, directory);
  if (name)
    make(<file-locator>, directory: directory, name: name)
  elseif (base)
    make(<file-locator>, directory: directory, base: base, extension: type)
  else 
    make(<file-locator>, directory: directory, 
	                 base: as(<string>, namespace-name(language-definition(ld))),
	                 extension: type)
  end 
end method profile-area-output-locator;

define function call-with-profile-area-output (fn :: <function>,
					       ld :: <library-description>,
					       #rest keys,
					       #key base, name, type)
  let locator = apply(profile-area-output-locator, ld, keys);
  if (locator)
    with-open-file (stream = locator, direction: #"output", stream-lock: #f)
      fn(stream)
    end;
  else
  end if;
end function call-with-profile-area-output;


// Provided by project manager

// library version for the current compile
define compiler-open generic project-library-version
  (project) => (major-ver, minor-ver);

// binding between two libraries
define compiler-open generic project-inter-library-binding
  (project, used-project) => (mode :: one-of(#"tight", #"loose"));

// This is provided by the compiler
// Return version of used context for which context was compiled
define function project-used-library-version (ld, uld)
 =>  (major-ver :: <integer>, minor-ver :: <integer>, 
      time-stamp :: <machine-word>);
  let ul = any?(method (ul) ul.used-library-description == uld & ul end,
		 ld.language-definition.used-libraries);
  debug-assert(ul, "%s not used, in project-used-library-version", uld);
  values(ul.used-library-major-version,
	 ul.used-library-minor-version,
	 as(<machine-word>, ul.used-library-change-count))
end;

define sealed class <build-info> (<object>)
  constant slot build-os, required-init-keyword: os:;
  constant slot build-architecture, required-init-keyword: architecture:;
  constant slot build-source-records, required-init-keyword: source-records:;
  constant slot build-definitions-version, required-init-keyword: version:;
end class;

// Save a file listing the Source Record Versions on which compiled code in
// the "compiled code database" (i.e. in the various backend output files)
// is based.
define method record-library-build (ld :: <library-description>)
  let os = ld.library-description-os-name;
  let architecture = ld.library-description-architecture-name;
  let version = ld.library-description-change-count;
  let cr* = ld.library-description-compilation-records;
  let sr* = map-as(<vector>, compilation-record-source-record, cr*);
  ld.library-description-last-build-info := make(<build-info>,
						 os: os,
						 architecture: architecture,
						 version: version,
						 source-records: sr*);
  with-build-area-output(stream = ld, name: "_SRV")
    format(stream, "%s\n", os);
    format(stream, "%s\n", architecture);
    format(stream, "%d\n", version);
    format(stream, "%d\n", size(sr*));
    for (sr in sr*)
      format(stream, "%s\n", as(<string>, library-source-record-id(ld, sr)));
    end;
  end with-build-area-output;
end method;

define function read-build-srv-file (ld :: <library-description>)
  let location = ld.library-description-build-location;
  if (location)
    local method read-int-line (stream)
            let line = read-line(stream);
	    for (char in line,
		 result = 0 then (result * 10) + as(<integer>, char)
					       - as(<integer>, '0'))
	    finally result
	    end;
	  end method;
    let srv-location = merge-locators(as(<file-locator>, "_SRV"),
				      as(<directory-locator>, location));
    if (file-exists?(srv-location))
      with-open-file (stream = srv-location, stream-lock: #f)
	let os = as(<symbol>, read-line(stream));
	let architecture = as(<symbol>, read-line(stream));
	let version = read-int-line(stream);
	let id-count = read-int-line(stream);
	let sr* = make(<vector>, size: id-count);
	for (index from 0 below id-count)
	  let id = read-line(stream);
	  sr*[index] := library-record-id-source-record(ld, id);
	end;
        make(<build-info>,
	     os: os,
	     architecture: architecture,
	     version: version,
	     source-records: sr*);
      end with-open-file;
    end;
  end if;
end function;

define function current-build-info (ld :: <library-description>)
  let build = ld.library-description-last-build-info;
  if (build &
      build.build-os == ld.library-description-os-name &
      build.build-architecture == ld.library-description-architecture-name)
    let sr* = build.build-source-records;
    let cr* = ld.library-description-compilation-records;
    size(sr*) == size(cr*) &
      every?(method (cr, sr) cr.compilation-record-source-record == sr end,
	     cr*, sr*) &
      build
  end
end function;

define function library-needs-linking? (ld :: <project-library-description>)
  if (ld.library-description-combined-record)
    compilation-record-needs-linking?(ld.library-description-combined-record)
  else
    any?(compilation-record-needs-linking?,
	 ld.library-description-compilation-records)
  end;
end;

  
define function library-description-built? (ld :: <project-library-description>)
  if (~library-description-personal?(ld))
    // KLUDGE: work around problem with premature reading of _SRV file..
    #t
  elseif (~ld.compiled-to-definitions? |
	    ld.library-description-compilation-aborted? |
	    ld.library-references-retracted-models? |
	    ld.library-needs-linking?)

    #f
  else
    let build = current-build-info(ld);
    build &
      build.build-definitions-version == ld.library-description-change-count &
      begin
	// Force recompilation if user deletes the _SRV file, just to give
	// them some way to control this...
	let location = ld.library-description-build-location;
	~location | file-exists?(merge-locators(as(<file-locator>, "_SRV"),
						as(<directory-locator>, location)))
      end;
  end
end function;

define function library-description-built?-setter (val == #f,
						   ld :: <library-description>)
  ld.library-description-last-build-info := #f;
end function;

define compiler-open generic install-library-description-sources (ld, sr*);

define function save-definition-database 
    (ld :: <compilation-context>, #rest keys)
  with-program-conditions
    apply(ensure-database-saved, ld, keys);
  end;
end function;

define generic ensure-database-saved (ld :: <compilation-context>, #key, #all-keys);

define method ensure-database-saved (ld :: <compilation-context>, #key, #all-keys)
  // Do nothing by default
end;

define method flush-database (ld :: <compilation-context>)
  // format-out("FLUSHING %=\n", ld);
  dynamic-bind (*dood-current-project* = library-description-project(ld))
  with-library-context (ld)
    let dood = library-description-dood(ld);
    dood-flush-all(dood, identity);
  end with-library-context;
  end dynamic-bind;
end method;

define compiler-open generic record-all-booted-model-properties
    (ld :: <dylan-library-description>);

define method record-booted-model-properties 
     (ld :: <dylan-library-description>, model, properties)
  when (properties)
    let booted-properties = library-booted-model-properties(ld);
    booted-properties[model] := properties;
  end when;
end method;

define variable $heap-to-database-size-factor = 6;

define method library-approximate-model-heap-size
    (ld :: <library-description>) => (res :: <integer>)
  reduce(\+, 0, map-as(<vector>, compilation-record-approximate-model-heap-size,
		       library-description-compilation-records(ld)))
end method;

define method ensure-database-saved
    (ld :: <project-library-description>, 
     #key export-only?, flush?, stats?)
  for (ld in reverse(all-library-descriptions(ld)))
    with-library-context (ld)
      //  check if save needed
      let count = ld.library-description-change-count;
      let saved-count = ld.library-description-saved-change-count;
      let flush? = ~dylan-library-library-description?(ld) & flush?;
      debug-assert(every?(compilation-record-definitions-installed?,
      			  ld.library-description-compilation-records));
      let dood = library-description-dood(ld);
      if (write-databases?() 
	    & count ~== saved-count & dood & ~dood-read-only?(dood))
	if (export-only?)
	  unless (ld.library-description-stripped? == #t)
	    strip-incremental-slots(ld);
	  end unless;
	end if;
	if (write-databases?())
	  when (dylan-library-library-description?(ld))
	    record-all-booted-model-properties(ld);
	  end when;
	  let number-objects = library-approximate-model-heap-size(ld);
	  let size-estimate  = number-objects * $heap-to-database-size-factor;
	  // format-out("number heap objects = %=\n", number-objects);
	  dood-commit(dood, stats?: stats?, size: size-estimate);
	  clear-stale-model-properties(ld);
	end if;
	if (flush?)
          flush-database(ld);
	end if;
	ld.library-description-saved-change-count := count;
      elseif (flush? & saved-count > 0)
	flush-database(ld);
      end if;
    end with-library-context;
  end for;
end method;

define open generic dood-statistics-filter-set 
    (dood :: <dood>) => (res :: <simple-object-vector>);

define open generic dood-statistics-aggregate-set 
    (dood :: <dood>) => (res :: <simple-object-vector>);

define variable *filter-set* = vector(<collection>);

define sideways method dood-statistics-filter-set 
    (dood :: <dood>) => (res :: <simple-object-vector>)
  *filter-set*
end method;

define variable *aggregate-set* = vector();

define sideways method dood-statistics-aggregate-set 
    (dood :: <dood>) => (res :: <simple-object-vector>);
  *aggregate-set*
end method;

define method report-definition-database-statistics
    (ld :: <project-library-description>, 
     #rest all-keys, #key force?)
  with-library-context (ld)
    let dood = library-description-dood(ld);
    format-out("\nSTATISTICS FOR %=\n", ld);
    dood-walk (dood, identity, force?: force?, parents?: #t);
    block ()
      dood-statistics 
	(dood, 
	 filter-set:    dood-statistics-filter-set(dood),
	 aggregate-set: dood-statistics-aggregate-set(dood));
    afterwards
      dood-reset-walker! (dood);
    end block;
  end with-library-context;
end method;

define method report-recursive-definition-database-statistics
    (ld :: <project-library-description>, 
     #rest all-keys, #key force?)

  let total-count = 0;
  let total-size  = 0;
  for (ld in reverse(all-library-descriptions(ld)))
    let (count, size)
      = apply(report-definition-database-statistics, ld, all-keys);
    total-count := total-count + count;
    total-size  := total-size  + size;
  end for;
  format-out("TOTAL SUMMARY\n");
  format-out("%d count, size = %d words\n", total-count, total-size);
end method;

define method report-diff-definition-database-statistics
    (ld :: <project-library-description>, #rest all-keys, #key force?)
  with-library-context (ld)
    let dood = library-description-dood(ld);
    dood-diff-last-two-statistics(dood);
  end with-library-context;
end method;

// True if library is in the "personal" build area.
define method library-description-personal? (ld :: <library-description>)
  ld.library-description-build-location ~== #f
end method;

define method binding-local-dependents-in-context
    (ld :: <project-library-description>, binding :: <module-binding>)
 => (deps :: <sequence>)
  binding.shadowable-binding-local-dependents
end method;

define method register-binding-dependent-in-context
    (ld :: <project-library-description>, b :: <module-binding>, dep)
  /*
   debug-assert(~member?(dep, b.shadowable-binding-local-dependents),
		"Dup registered dependent", dep, b);
   */
  b.shadowable-binding-local-dependents
    := pair(dep, as(<list>, b.shadowable-binding-local-dependents));
end method;

define method unregister-binding-dependent-in-context
    (ld :: <project-library-description>, b :: <module-binding>, dep)
  b.shadowable-binding-local-dependents
    := remove!(as(<list>, b.shadowable-binding-local-dependents), dep);
end method;


//// NAMESPACE DOOD POLICY

define sealed class <dfmc-namespace-dood> (<dfmc-dood>) 
  slot binding-model-not-computed-proxy = #f;
end class;

define open generic ensure-export-only (ld :: <library-description>);

define method ensure-namespace-database-saved
    (ld :: <project-library-description>, 
     #key export-only?, flush?, stats?)
  for (ld in reverse(all-library-descriptions(ld)))
    with-library-context (ld)
      let dfmc-dood
	= library-description-dood(ld);
      let dfmc-dood-location
	= dood-locator(dfmc-dood);
      let dood-location
	= make(<file-locator>, 
	       directory: locator-directory(dfmc-dood-location),
	       base: locator-base(dfmc-dood-location),
	       extension: "ndb");
      unless (dylan-library-library-description?(ld))
	if (~file-exists?(dood-location) | 
	      file-property(dood-location, #"creation-date")
		> file-property(dfmc-dood-location, #"creation-date"))
	  let (segments, default-segment)
	    = dood-dfmc-initial-segments(<dfmc-namespace-dood>);
	  let dood 
	    = make(<dfmc-namespace-dood>, 
		   name:            as(<symbol>, locator-base(as(<file-locator>, dood-location))),
		   locator:         dood-location,
		   version:         $dfmc-dood-version,
		   if-exists:       #"replace",
		   direction:       #"input-output",
		   segments:        segments,
		   default-segment: default-segment);
	  dood-root(dood) := ld;

	  ensure-export-only(ld);

	  dood-commit(dood);
	end if;
      end unless;
    end with-library-context;
  end for;
end method;

define function save-namespace-database 
    (ld :: <compilation-context>, #rest keys)
  with-program-conditions
    apply(ensure-namespace-database-saved, ld, keys);
  end;
end function;
