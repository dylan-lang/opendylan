Module: dfmc-debug
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline function element-or (table, key)
  element(table, key, default: #f)
end;

define class <grouper> (<object>)
  // ld -> group mapping.  Maps to #f if ld is not in any group.
  constant slot grouper-library-groups :: <table> = make(<table>);
  // name -> group mapping
  constant slot grouper-name-groups :: <table> = make(<table>);
  // name -> ld
  constant slot grouper-name-libraries :: <table> = make(<table>);
  // True if want sealing info
  constant slot grouper-seal? :: <boolean>,
    required-init-keyword: seal?:;
end;

define thread variable *grouper* :: false-or(<grouper>) = #f;

define inline function library-description-group (ld :: <library-description>)
 => (group :: false-or(<library-group>))
  element(*grouper*.grouper-library-groups, ld);
end;

define class <library-group> (<object>)
  // Name of #f means an auto-generated group.
  constant slot group-name :: false-or(<symbol>),
    required-init-keyword: name:;
  constant slot group-base-address :: false-or(<string>) = #f,
    init-keyword: base-address:;
  constant slot group-executable :: false-or(<string>) = #f,
    init-keyword: executable:;
  constant slot group-export-specs = #f,
    init-keyword: export-specs:;
  // modules exported from this group, either because user requested,
  // or because they are used by other groups
  constant slot group-exports :: <set> = make(<set>);
  // included libraries, deepest first (needed for combining sources)
  slot group-libraries :: <list> = #(), init-keyword: libraries:;
  // modules defined in this group
  constant slot group-defined-modules :: <set> = make(<set>);
  // groups used
  constant slot group-directly-used-groups :: <set> = make(<set>);
  constant slot group-all-used-groups :: <set> = make(<set>);
  // ungrouped (leaf) libraries used
  constant slot group-directly-used-libraries :: <set> = make(<set>);
  // group sealing information: module -> set of names to seal in module.
  constant slot group-sealing :: <table> = make(<table>);
end class;

define function group-library-name (group :: <library-group>)
 => (name :: <symbol>)
  group.group-name |
    // anonymous group
    group.group-libraries.first.library-description-emit-name
end;

define function unify-project (#key config,
				  directory = unsupplied(),
				  groups = unsupplied(),
				  registry = unsupplied(),
			          seal? = unsupplied(),
			          seal-all = unsupplied())
  debug-out(#"unify", "entered unify, config = %=, dir: %=, grps: %=, reg: %=, seal?: %=, seal-all: %=\n",
	    config, directory, groups, registry, seal?, seal-all);
  when (config)
    apply(method (#key directory: c-dir = unsupplied(),
		       groups: c-groups = unsupplied(),
		       registry: c-registry = unsupplied(),
		       seal?: c-seal? = unsupplied(),
		       seal-all: c-seal-all = unsupplied())
	    debug-out(#"unify", " CONFIG dir: %=, grps: %=, reg: %=, seal?: %=, seal-all: %=\n", c-dir, c-groups, c-registry, c-seal?, c-seal-all);
	    // The explicit keywords take precedence over config.
	    unless (supplied?(directory)) directory := c-dir end;
	    unless (supplied?(groups)) groups := c-groups end;
	    unless (supplied?(registry)) registry := c-registry end;
	    unless (supplied?(seal?)) seal? := c-seal? end;
	    unless (supplied?(seal-all)) seal-all := c-seal-all end;
	  end,
	  read-grouper-config(config));
    let base-dir = locator-directory(as(<file-locator>, config));
    directory := if (supplied?(directory))
		   merge-locators(as(<directory-locator>, directory), base-dir)
		 else
		   base-dir;
		 end;
  end;
  let registry = supplied?(registry) & registry;
  let seal? = supplied?(seal?) & seal? & #t;
  let seal-all = if (supplied?(seal-all))
		   debug-out(#"unify", "Mapping seal-all: %s\n", seal-all);
		   map(curry(as, <symbol>), seal-all)
		 else
		   #[]
		 end;
  assert(supplied?(groups), "Groups: must be supplied");
  assert(supplied?(directory), "Directory: must be supplied");
  dynamic-bind (*grouper* = make(<grouper>, seal?: seal?))
    record-groups(groups);
    record-group-exports();
    record-group-library-use();
    compute-group-modules();
    compute-all-used-groups();
    when (seal?)
      compute-group-sealing(seal-all);
    end;
    output-groups(directory, registry);
  end;
end;

define function output-groups (dir :: <pathname>,
			       registry :: false-or(<pathname>))
  let dir = as(<directory-locator>, dir);
  ensure-directories-exist(dir);
  for (group in *grouper*.grouper-name-groups)
    let lib-file = write-group-library(group, dir);
    let lid-file = write-group-lid(group, dir, lib-file);
    when (registry)
      let registry-dir = merge-locators(as(<directory-locator>, registry),
					dir);
      write-group-registry(group, registry-dir, lid-file);
    end;
  end;
end function;

define function write-group-registry (group :: <library-group>,
				      registry :: <locator>,
				      lid-file :: <pathname>)
  debug-out(#"unify", "write-group-registry %s %s [%s]\n",
	    group.group-library-name, registry, lid-file);
  let lib-name = as-lowercase(as(<string>, group.group-library-name));
  let root = locator-directory(registry);
  let platform = target-platform-name();
  let platform-registry = subdirectory-locator(registry, list(platform));
  ensure-directories-exist(platform-registry);
  with-open-file(s = merge-locators(as(<file-locator>, lib-name), platform-registry),
		 direction: #"output")
    format(s, "abstract://dylan/%s\n", relative-locator(lid-file, root));
  end with-open-file;
end;

define function write-group-lid (group :: <library-group>,
				 dir :: <locator>,
				 lib-file :: false-or(<pathname>))
  debug-out(#"unify", "write-group-lid %s %s [%s]\n",
	    group.group-library-name, dir, lib-file);
  let lib-name = as-lowercase(as(<string>, group.group-library-name));
  let lid-file = merge-locators(make(<file-locator>, 
				     base: concatenate("unified-", lib-name),
				     extension: "lid"),
				dir);
  let props :: <list> = combine-lid-properties(group, dir);
  with-open-file (stream = lid-file, direction: #"output")
    format(stream, "Library: %s\n", lib-name);
    for (props = props then props.tail.tail, until: props == #())
      let key = props.head;
      let values = props.tail.head;
      if (key == Files: & lib-file)
	let lib-file = begin
			 let relative-file = relative-locator(lib-file, dir);
			 make(<file-locator>, 
			      directory: locator-directory(relative-file),
			      name: locator-base(relative-file))
		       end;
	values := pair(as(<string>, lib-file), values);
      end;
      unless (empty?(values))
	format(stream, "%s:", key);
	for (value in values) format(stream, "\t%s\n", value) end;
      end;
    end;
  end with-open-file;
  lid-file
end function;

define function combine-lid-properties (group :: <library-group>, dir)
  let dir = as(<directory-locator>, dir);
  let included = group.group-libraries;
  local method reloc-from (loc, file)
	  as(<string>, relative-locator(merge-locators(as(<file-locator>, file), loc), dir))
	end;
  local method new? (table, string)
	  let sym = as(<symbol>, string);
	  when (~member?(sym, table)) add!(table, sym); #t end
	end;
  local method new-files (files, loc, table)
	  choose(curry(new?, table),
		 map-as(<list>, curry(reloc-from, as(<directory-locator>, loc)), files));
	end;
  local method new-names (names, table)
	  copy-sequence(choose(curry(new?, table), as(<list>, names)));
	end;
  let c-source-files = #(); let c-source-files-seen = make(<set>);
  let c-header-files = #(); let c-header-files-seen = make(<set>);
  let c-object-files = #(); let c-object-files-seen = make(<set>);
  let rc-files = #(); let rc-files-seen = make(<set>);
  let c-libraries = #(); let c-libraries-seen = make(<set>);
  let linker-options = #(); let linker-options-seen = make(<set>);
  let source-files = #();
  do(method (ld)
       let project = ld.library-description-project;
       let properties = project.project-lid-file-info;
       let loc = project.project-source-location;
       let opts = element-or(properties, #"linker-options");
       when (opts)
	 linker-options := concatenate!(linker-options,
					new-names(opts, linker-options-seen));
       end;
       let files = element-or(properties, #"c-source-files")
	                       | element-or(properties, #"c-files");
       when (files)
	 c-source-files := concatenate!(c-source-files,
					new-files(files, loc, c-source-files-seen));
       end;
       let files = element-or(properties, #"c-header-files");
       when (files)
	 c-header-files := concatenate!(c-header-files,
					new-files(files, loc, c-header-files-seen));
       end;
       let files = element-or(properties, #"c-object-files");
       when (files)
	 c-object-files := concatenate!(c-object-files,
					new-files(files, loc, c-object-files-seen));
       end;
       let files = element-or(properties, #"rc-files");
       when (files)
	 rc-files := concatenate!(rc-files, new-files(files, loc, rc-files-seen));
       end;
       let names = element-or(properties, #"c-libraries");
       when (names)
	 c-libraries := concatenate!(c-libraries,
				     new-names(names, c-libraries-seen));
       end;
       let names
	 = map-as(<list>,
		  method (cr)
		    if (every?(method (form)
				 form.form-parent-form |
				   instance?(form, <library-definition>)
			       end,
			       cr.compilation-record-top-level-forms))
		      #f
		    else
		      let sr = cr.compilation-record-source-record;
		      source-record-relative-name(sr, dir)
		    end
		  end,
		  ld.library-description-compilation-records);
       source-files := concatenate!(source-files, remove!(names, #f));
     end method,
     included);
  list(c-source-files: c-source-files,
       c-header-files: c-header-files,
       c-object-files: c-object-files,
       rc-files: rc-files,
       c-libraries: c-libraries,
       linker-options: linker-options,
       files: source-files,
       base-address: if (group.group-base-address)
		       list(group.group-base-address)
		     else
		       #()
		     end,
       executable: if (group.group-executable)
		     list(group.group-executable)
		   else
		     #()
		   end,
       seal: group-sealing-property(group))
end function combine-lid-properties;

define function group-sealing-property (group :: <library-group>)
  let prop = #();
  for (set keyed-by module in group.group-sealing)
    prop := concatenate(list(concatenate("module ",
					 as(<string>, module.namespace-name))),
			set.key-sequence,
			prop);
  end;
  when (empty?(prop) & *grouper*.grouper-seal?)
    prop := list("")
  end;
  prop
end;


define function write-group-library (group :: <library-group>,
				     dir :: <locator>)
 => (lib-file :: false-or(<pathname>))
  debug-out(#"unify", "write-group-library %s %s\n",
	    group.group-library-name, dir);
  let name = group.group-library-name;
  unless (name == #"dylan") // no library file for dylan
    let lib-name = as-lowercase(as(<string>, name));
    let lib-file = merge-locators(make(<file-locator>,
				       base: concatenate("unified-",
							 lib-name,
							 "-library"),
				       extension: "dylan"),
				  dir);
    with-open-file (stream = lib-file, direction: #"output")
      write(stream, "Module: dylan-user\n\n");
      format(stream, "define library %s\n", lib-name);
      write-group-clauses(stream, group);
      format(stream, "end library;\n");
    end;
    lib-file
  end;
end function;

define function write-group-clauses (stream :: <stream>,
				     group :: <library-group>)
  debug-out(#"unify", "write-group-clauses\n");
  let uses = make(<table>);
  let exports = make(<set>);
  for (uld in group.group-directly-used-libraries)
    uses[uld] := list(uld.library-description-emit-name);
  end;
  for (ugroup in group.group-directly-used-groups)
    uses[ugroup] := list(ugroup.group-library-name);
  end;
  for (module in group.group-exports)
    let name = module.namespace-name;
    let mld = module.namespace-original-library;
    let msource = mld.library-description-group | mld;
    if (msource == group)
      add!(exports, name)
    else
      let use-info = uses[msource]; // must be there!
      use-info.tail := pair(name, use-info.tail);
    end;
  end;
  for (use-info in uses)
    format(stream, "  use %s", use-info.head);
    unless (empty?(use-info.tail))
      for (reexport in use-info.tail, prefix = ", export: { " then ", ")
	format(stream, "%s%s", prefix, reexport)
      end;
      format(stream, "}");
    end;
    format(stream, ";\n");
  end;
  unless (empty?(exports))
    for (export in exports, prefix = "  export " then ", ")
      format(stream, "%s%s", prefix, export)
    end;
    format(stream, ";\n");
  end;
end function;

define function compute-group-sealing (seal-all)
  debug-out(#"unify", "compute-group-sealing\n");
  for (group :: <library-group> in *grouper*.grouper-name-groups)
    debug-out(#"unify", "   Group: %s\n", group.group-library-name);
    let xseal = group.group-sealing;
    let seal-all? = member?(group.group-library-name, seal-all);
    for (module in group.group-defined-modules)
      with-library-context (module.namespace-original-library)
	for (name in module.exported-names)
	  let b = lookup-name(module, name);
	  if (unified-form-compiler-open?(untracked-binding-definition(b, default: #f),
					  seal-all?)
		& definitions-group-local?(group, b))
	    debug-out(#"unify", "       Sealing %s:%s\n",
		      name, module.namespace-name);
	    add!(element-or(xseal, module) | (xseal[module] := make(<set>)),
		 name);
	  end;
	end for;
      end with-library-context;
    end for;
  end for;
end;

define method unified-form-compiler-open? (f :: <object>, all?) // #f or top-level-form
  #f
end;

define method unified-form-compiler-open? (f :: <generic-definition>, all?)
  if (all?)
    ~f.form-declared-sealed?
  else
    f.form-compiler-open?
  end;
end;

define method unified-form-compiler-open? (f :: <class-definition>, all?)
  if (all?)
    ~f.form-declared-sealed?
  else
    f.form-compiler-open?
  end;
end;

define function definitions-group-local? (group :: <library-group>,
					  // actually canonical
					  binding :: <module-binding>)
  // Return #f if binding has any modifying definitions in another group.
  block (return)
    for (xgroup keyed-by xld in *grouper*.grouper-library-groups)
      when (xgroup & member?(group, xgroup.group-all-used-groups))
	for (xld :: <library-description> in xgroup.group-libraries)
	  let xlib = xld.language-definition;
	  let xb = lookup-imported-binding(xlib, binding);
	  when (xb & ~empty?(xb.binding-local-modifying-definitions))
	    return(#f)
	  end;
	end;
      end;
    end;
    #t
  end;
end function;



// TODO: make this a condition class with a print function.
define function signal-circular-use-error (group-path :: <list>)
  let s = make(<string-stream>, direction: #"output");
  format(s, "Circular use:\n");
  for (path = group-path then path.tail, until: path.tail == #())
    let g1 :: <library-group> = path.head;
    let g2 :: <library-group> = path.tail.head;
    format(s, " Group %s uses %s because:\n",
	   g1.group-library-name, g2.group-library-name);
    for (ld in g1.group-libraries)
      for (uld in ld.directly-used-library-descriptions)
	if (uld.library-description-group == g2)
	  format(s, "     %s uses %s\n",
		 ld.library-description-emit-name,
		 uld.library-description-emit-name)
	end
      end;
    end;
  end;
  error("%s", stream-contents(s));
end function;

define function circular-use-error (group :: <library-group>)
  local method find-paths (group :: <library-group>, parents :: <list>)
	  let parents = pair(group, parents);
	  for (g in group.group-directly-used-groups)
	    if (member?(g, parents))
	      local method loop (path)
		      if (path.head == g) path else loop(path.tail) end
		    end;
	      signal-circular-use-error(loop(reverse(pair(g, parents))));
	    else
	      find-paths(g, parents)
	    end
	  end for;
	end method;
  find-paths(group, #());
  error("Didn't find circular path for circular error in %s?", group);
end;

define function compute-all-used-groups () // and check for circularities
  debug-out(#"unify", "compute-all-used-groups\n");
  let done = make(<set>);
  local method all-used (group :: <library-group>, stack :: <list>)
	  let used = group.group-all-used-groups;
	  unless (member?(group, done))
	    let stack = pair(group, stack);
	    for (ugroup in group.group-directly-used-groups)
	      if (member?(ugroup, stack)) circular-use-error(ugroup) end;
	      unless (member?(ugroup, used))
		do(curry(add!, used), all-used(ugroup, stack));
		add!(used, ugroup);
	      end;
	    end;
	    add!(done, group);
	  end;
	  used
	end method;
  do(rcurry(all-used, #()), *grouper*.grouper-name-groups);
end function;

define function compute-group-modules ()
  debug-out(#"unify", "compute-group-modules\n");
  local method access (group, modules)
	  for (module in modules)
	    let mld = module.namespace-original-library;
	    let mgroup = mld.library-description-group;
	    case
	      ~mgroup =>
		add!(group.group-directly-used-libraries, mld);
	      mgroup ~== group =>
		add!(group.group-directly-used-groups, mgroup);
		add!(mgroup.group-exports, module);
	    end;
	  end for;
	end method;
  for (group keyed-by ld in *grouper*.grouper-library-groups)
    when (group)
      let modules = group.group-defined-modules;
      do-library-modules(ld,
			 method (name :: <symbol>, ignore)
			   with-library-context (ld)
			     let m = find-module-in(ld, name);
			     unless (member?(m, modules))
			       add!(modules, m);
			       access(group, m.directly-used-namespaces);
			     end unless;
			   end with-library-context;
			 end method,
			 internal?: #t,
			 inherited?: #f);
    end when;
  end for;
  for (group in *grouper*.grouper-name-groups)
    access(group, group.group-exports);
  end;
end function;

define function record-group-library-use ()
  debug-out(#"unify", "Record library use\n");
  for (group keyed-by ld in *grouper*.grouper-library-groups)
    when (group)
      let used-libraries = group.group-directly-used-libraries;
      let used-groups = group.group-directly-used-groups;
      for (uld in ld.directly-used-library-descriptions)
	let ugroup = uld.library-description-group;
	case
	  ~ugroup => add!(used-libraries, uld);
	  ugroup ~== group => add!(used-groups, ugroup);
	end
      end
    end;
  end
end function;

define function record-groups (groups)
  debug-out(#"unify", "entered record-groups\n");
  let library-table = *grouper*.grouper-library-groups;
  let library-name-table = *grouper*.grouper-name-libraries;
  let name-table = *grouper*.grouper-name-groups;
  for (group-spec in groups)
    debug-out(#"unify", "Parsing group %s\n", group-spec);
    let (name, export, base-address, executable, libraries)
      = apply(method (name, #key export, base-address, executable, libraries)
		values(as(<symbol>, name),
		       export,
		       base-address & as(<string>, base-address),
		       executable & as(<string>, executable),
		       libraries)
	      end,
	      group-spec);
    assert(~element-or(name-table, name), "Duplicate spec for %s", name);
    let group = make(<library-group>, name: name,
		     base-address: base-address,
		     executable: executable,
		     export-specs: export);
    name-table[name] := group;
    for (lib-spec in libraries | #())
      let lib-name = as(<symbol>, lib-spec);
      let ld = lookup-library-description(lib-name);
      let lgroup = element-or(library-table, ld);
      if (~lgroup)
	library-table[ld] := group;
      elseif (lgroup ~== group)
	error("%s is in both %s and %s", lib-name, name, lgroup.group-name);
      end;
      library-name-table[lib-name] := ld;
    end for;
  end for;
  let ld-set = make(<set>);
  for (ld in library-table.key-sequence)
    for (ld in ld.all-library-descriptions)
      add!(ld-set, ld);
      library-name-table[ld.library-description-emit-name] := ld;
    end;
  end;
  let all-lds = sort(ld-set.key-sequence,
		     test: method (ld1, ld2)
			     ld1.all-library-descriptions.size
			       > ld2.all-library-descriptions.size
			   end);
  for (ld in all-lds)
    let group = element-or(library-table, ld);
    if (group)
      // This puts them in order deepest first, reverse of all-lds.
      group.group-libraries := pair(ld, group.group-libraries);
    else
      let name = ld.library-description-emit-name;
      when (element-or(name-table, name))
	error("Name conflict: there is a group named %s, and a non-group"
		" library named %s in %s",
	      name, name, ld.library-description-project);
      end;
      if (any?(curry(element-or, library-table),
	       all-used-library-descriptions(ld)))
	// Make fake groups for non-leaf ungrouped libraries.
	debug-out(#"unify", "Singleton library %s uses %s\n",
		  name,
		  remove-duplicates!
		    (remove!(map(method (uld)
				   let g = element-or(library-table, uld);
				   g & g.group-library-name
				 end,
				 all-used-library-descriptions(ld)),
			     #f)));
	let group = make(<library-group>, name: #f, libraries: list(ld));
	name-table[name] := group;
	library-table[ld] := group;
	export-module-spec(group, ld, #"all-exported");
      else // No groups for leaf libraries.
	debug-out(#"unify", "Ungrouped leaf library: %s\n", name);
	library-table[ld] := #f;
      end;
    end;
  end for;
end function record-groups;

define constant $export-kinds = #[#"all", #"all-defined", #"all-exported"];

define function record-group-exports ()
  debug-out(#"unify", "Recording group exports\n");
  do(record-group-exports-from, *grouper*.grouper-name-groups);
end;

define function record-group-exports-from (group :: <library-group>)
  local method tokenize (obj)
	  if (instance?(obj, <string>)) as(<symbol>, obj) else obj end;
	end;
  local method module-spec? (obj)
	  /* One of: module-name | (module: module-name) |
	             (<export-kind> in: library) | (module-name in: library) |
                     (module: module-name in: library) */
	  let obj = tokenize(obj);
	  ~instance?(obj, <sequence>)
	    | select (obj.size)
		2 => tokenize(obj[0]) == module:;
		3 => tokenize(obj[1]) == in:;
		4 => tokenize(obj[0]) == module: & tokenize(obj[2]) == in:;
		otherwise => #f;
	      end;
	end;
  let specs = tokenize(group.group-export-specs) | #();
  // Either <export-kind>, or module-spec or a sequence of module-spec's.
  if (member?(specs, $export-kinds))
    debug-out(#"unify", "Export %s from %s in %s\n", specs, group, group);
    export-module-spec(group, group, specs)
  else
    for (spec in if (module-spec?(specs)) list(specs) else specs end)
      assert(module-spec?(spec), "Invalid module spec: %s", spec);
      let (src, module-spec, maybe-kind?)
	= if (~instance?(spec, <sequence>))
	    values(group, spec, #f)
	  elseif (spec.size == 2)
	    values(group, spec[1], #f)
	  else
	    let count = spec.size;
	    let src-spec = tokenize(spec[count - 1]);
	    let src = element-or(*grouper*.grouper-name-libraries, src-spec)
	              | element-or(*grouper*.grouper-name-groups, src-spec);
	    assert(src, "Unknown library/group %s", src-spec);
	    let module-spec = tokenize(spec[count - 3]);
	    values(src, module-spec, count == 3);
	  end;
      if (maybe-kind? & member?(module-spec, $export-kinds))
	debug-out(#"unify", "Export %s from %s\n", module-spec, src);
	export-module-spec(group, src, module-spec);
      else
	debug-out(#"unify", "Export Module %s from %s\n", module-spec, src);
	export-module-from(group, src, module-spec)
      end;
    end for;
  end if;
end function;

define function export-module-from (group :: <library-group>,
				    source,
				    module-name)
  add!(group.group-exports, find-module-in(source, module-name))
end;


define method export-module-spec (group :: <library-group>,
				  source :: <library-description>,
				  kind)
  do-library-modules(source,
		     method (name :: <symbol>, ignore)
		       export-module-from(group, source, name);
		     end,
		     internal?: kind ~== #"all-exported",
		     inherited?: kind ~== #"all-defined")
end method;

define method export-module-spec (group :: <library-group>,
				  source :: <library-group>,
				  kind)
  for (ld :: <library-description> in source.group-libraries)
    export-module-spec(group, ld, kind)
  end;
end method;

define generic find-module-in (source, name) => (m :: <module>);

define method choose-modules (group :: <library-group>, name :: <symbol>)
 => (modules :: <list>);
   reduce(method (so-far, ld :: <library-description>)
	    let module = lookup-module-in(ld.language-definition, name,
					  default: #f);
	    if (module)
	      add-new!(so-far, module)
	    else
	      so-far
	    end
	  end,
	  #(),
	  group.group-libraries);
end;

define method choose-modules (groups :: <collection>, name :: <symbol>)
 => (modules :: <list>);
  reduce(method (so-far, group :: <library-group>)
	   reduce(add-new!, so-far, choose-modules(group, name))
	 end,
	 #(),
	 groups)
end method;

define method choose-one-module (debug-name, modules) => (m :: <module>)
  if (modules.size == 1)
    modules.first
  elseif (empty?(modules))
    error("No module %s visible anywhere", debug-name);
  else
    error("Ambiguous module %s", debug-name)
  end;
end method;

define method find-module-in (groups :: <collection>, name :: <symbol>)
 => (m :: <module>)
  choose-one-module(name, choose-modules(groups, name));
end;

define method find-module-in (group :: <library-group>, name :: <symbol>)
 => (m :: <module>)
  choose-one-module(name, choose-modules(group, name));
end;

define method find-module-in (ld :: <library-description>, name :: <symbol>)
 => (m :: <module>)
  lookup-module-in(ld.language-definition, name)
end;

define method find-module-in (source, name :: <string>)
 => (m :: <module>)
  find-module-in(source, as(<symbol>, name))
end method;

define function read-grouper-config (filename) => (plist :: <list>)
  debug-out(#"unify", "entered read-grouper-config: %s\n", filename);
  local method read-token (s, first-ch)
	  local method token-loop (so-far)
		  let ch = read-element(s, on-end-of-stream: #f);
		  if (~ch | member?(ch, "\f\n\r\t ,#([)]\""))
		    if (ch) unread-element(s, ch) end;
		    so-far
		  else
		    token-loop(pair(ch, so-far))
		  end;
		end method;
	  let rev-chars = token-loop(list(first-ch));
	  if (rev-chars.head == ':')
	    as(<symbol>, as(<string>, reverse!(rev-chars.tail)))
	  else
	    as(<string>, reverse!(rev-chars))
	  end;
	end method;
  local method read-string (s)
	  local method string-loop (so-far)
		  let ch = read-element(s);
		  if (ch == '"') as(<string>, reverse!(so-far))
		  elseif (ch == '\\') string-loop(pair(read-element(s), so-far))
		  else string-loop(pair(ch, so-far)) end;
		end method;
	  string-loop(#())
	end method;
  local method read-line-comment (s) // => (not-eof?)
	  let ch = read-element(s, on-end-of-stream: #f);
	  ch & (ch == '\n' | ch == '\r' | read-line-comment(s))
	end method;
  local method read-comment (s, level)
	  unless (level == 0)
	    let ch = read-element(s);
	    read-comment(s,
			 case
			   ch == '*' & peek(s, on-end-of-stream: #f) == '/'
			     => read-element(s); level - 1;
			   ch == '/' & peek(s, on-end-of-stream: #f) == '*'
			     => read-element(s); level + 1;
			   otherwise
			     => level;
			 end)
	  end unless
	end method;
  local method read-list (s, end-char)
	  local method loop (so-far)
		  let one = read-any(s);
		  case
		    one == end-char         => reverse!(so-far);
		    one == not-found()      => error("Premature end of file");
		    one == ')' | one == ']' => error("Stray %=", one);
		    otherwise               => loop(pair(one, so-far));
		  end;
		end method;
	  loop(#())
	end method,
        method read-any (s)
	  let ch = read-element(s, on-end-of-stream: #f);
	  select (ch)
	    '\f', '\n', '\r', '\t', ' ', ',' => read-any(s);
	    '/' => select (peek(s, on-end-of-stream: #f))
		     '/' => read-line-comment(s) & read-any(s);
		     '*' => read-element(s); read-comment(s, 1); read-any(s);
		     otherwise => read-token(s, ch);
		   end;
	    '#' => let token = read-token(s, ch);
	           select (token by \=)
		     "#" => read-any(s);
		     "#f", "#F" => #f;
		     "#t", "#T" => #t;
		     otherwise => token;
		   end;
	    #f        => not-found();
	    ')', ']'  => ch;
	    '(', '['  => read-list(s, if (ch == '(') ')' else ']' end);
	    '"'       => read-string(s);
	    otherwise => read-token(s, ch);
	  end select;
	end method;
  with-open-file (stream = filename)
    let ans = read-list(stream, not-found());
    if (ans.size == 1 & instance?(ans.first, <list>))
      ans.first
    else
      ans
    end;
  end;
end function;

