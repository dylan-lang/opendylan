Module:    scepter-dylan-back-end
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define scepter-back-end <dylan-back-end>
  command-line-syntax: "dylan",
  filename-extension: "dylan"

  constant slot dbe-protocol-exports :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot dbe-stubs-exports :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot dbe-skeletons-exports :: <stretchy-vector> = make(<stretchy-vector>);

  slot dbe-project-base-name :: <string> = "";

  slot dbe-protocol-stream :: <stream>;
  slot dbe-stubs-stream :: <stream>;
  slot dbe-skeletons-stream :: <stream>;

  slot dbe-emit-protocol-project? :: <boolean> = #t;
  slot dbe-emit-stubs-project? :: <boolean> = #t;
  slot dbe-emit-skeletons-project? :: <boolean> = #t;
end scepter-back-end;

define constant dbe-shared-stream = dbe-protocol-stream;
define constant dbe-emit-shared-project? = dbe-emit-protocol-project?;

define method dbe-add-protocol-export (back-end :: <dylan-back-end>, name :: <string>)
 => ()
  add!(dbe-protocol-exports(back-end), name);
end method;

define constant dbe-add-shared-export = dbe-add-protocol-export;

define method dbe-add-stubs-export (back-end :: <dylan-back-end>, name :: <string>)
 => ()
  add!(dbe-stubs-exports(back-end), name);
end method;

define method dbe-add-skeletons-export (back-end :: <dylan-back-end>, name :: <string>)
 => ()
  add!(dbe-skeletons-exports(back-end), name);
end method;

define method dbe-set-emitted-projects (back-end :: <dylan-back-end>)
 => ()
  let scepter = get-scepter();
  if (scepter.scepter-protocol-option?)
    dbe-emit-protocol-project?(back-end) := #t;
    dbe-emit-stubs-project?(back-end) := #f;
    dbe-emit-skeletons-project?(back-end) := #f;
  end if;

  if (scepter.scepter-stubs-option?)
    dbe-emit-protocol-project?(back-end) := #t;
    dbe-emit-stubs-project?(back-end) := #t;
    dbe-emit-skeletons-project?(back-end) := #f;
  end if;

  if (scepter.scepter-skeletons-option?)
    dbe-emit-protocol-project?(back-end) := #t;
    dbe-emit-stubs-project?(back-end) := #t;
    dbe-emit-skeletons-project?(back-end) := #t;
  end if;
end method;

define constant $dylan-back-end-version = "1.1";

define method scepter-back-end-banner (back-end :: subclass(<dylan-back-end>), stream :: <stream>)
 => ()
  let scepter = get-scepter();
  format(stream, "\n%s, dylan IDL binding version %s",
	 scepter.scepter-program-name, $dylan-back-end-version);
end method;


define sealed class <dylan-back-end-result> (<scepter-back-end-result>)
  constant slot scepter-back-end-dylan-subprojects :: <sequence>, required-init-keyword: subprojects:;
  constant slot scepter-back-end-modified-dylan-projects :: <sequence>, required-init-keyword: modified-projects:;
end class;


/*
define method scepter-back-end-emitter? (back-end :: <dylan-back-end>, errors :: <collection>)
 => (run? :: <boolean>)
  empty?(errors);
end method;
*/

define method scepter-back-end-emit (be :: <dylan-back-end>, root :: <ast-root>, front-end :: <scepter-front-end>, source :: <scepter-source>)
 => (result :: <dylan-back-end-result>)

  let scepter = get-scepter();
 
  be.dbe-project-base-name := scepter-source-base-name(source);
  debug-message("Scepter Dylan back-end: emitting %=", be.dbe-project-base-name);

  let protocol-project = make-protocol-project(dbe-project-base-name(be), dbe-protocol-exports(be));
  let stubs-project = make-stubs-project(dbe-project-base-name(be), dbe-stubs-exports(be));
  let skeletons-project = make-skeletons-project(dbe-project-base-name(be), dbe-skeletons-exports(be));
  dbe-set-emitted-projects(be);

  let parent-directory = scepter-source-output-directory(source, scepter);
  let protocol-dir = protocol-project-directory(parent-directory);
  let stubs-dir = stubs-project-directory(parent-directory);
  let skeletons-dir = skeletons-project-directory(parent-directory);

  let modified = scepter-source-modified(source);
  if (dbe-emit-protocol-project?(be) & ~scepter.scepter-clean-build?)
    dbe-emit-protocol-project?(be) := ~project-up-to-date?(protocol-project, protocol-dir, modified);
  end if;
  if (dbe-emit-stubs-project?(be) & ~scepter.scepter-clean-build?)
    dbe-emit-stubs-project?(be) := ~project-up-to-date?(stubs-project, stubs-dir, modified);
  end if;
  if (dbe-emit-skeletons-project?(be) & ~scepter.scepter-clean-build?)
    dbe-emit-skeletons-project?(be) := ~project-up-to-date?(skeletons-project, skeletons-dir, modified);
  end if;

  if (dbe-emit-protocol-project?(be))
    ensure-directories-exist(protocol-dir);
    let project-name = protocol-project-name(dbe-project-base-name(be));
    let filename = project-source-file(project-name, protocol-dir);
    let stream = make(<file-stream>, locator: filename, direction: #"output");
    dbe-protocol-stream(be) := stream;
    let keys = vector(pair(#"module", project-name));
    write-interchange-file-header(stream, keys);
  end if;

  if (dbe-emit-stubs-project?(be))
    ensure-directories-exist(stubs-dir);
    let project-name = stubs-project-name(dbe-project-base-name(be));
    let filename = project-source-file(project-name, stubs-dir);
    let stream = make(<file-stream>, locator: filename, direction: #"output");
    dbe-stubs-stream(be) := stream;
    let keys = vector(pair(#"module", project-name));
    write-interchange-file-header(stream, keys);
  end if;

  if (dbe-emit-skeletons-project?(be))
    ensure-directories-exist(skeletons-dir);
    let project-name = skeletons-project-name(dbe-project-base-name(be));
    let filename = project-source-file(project-name, skeletons-dir);
    let stream = make(<file-stream>, locator: filename, direction: #"output");
    dbe-skeletons-stream(be) := stream;
    let keys = vector(pair(#"module", project-name));
    write-interchange-file-header(stream, keys);
  end if;

  do(curry(emit-ast-node, be), scepter.scepter-nodes);

  let emitted-projects = #();
  if (dbe-emit-protocol-project?(be))
    format(dbe-protocol-stream(be), "\n/* eof */\n");
    close(dbe-protocol-stream(be));
    generate-hdp-project(protocol-project, protocol-dir);
    emitted-projects := pair(project-HDP-file(protocol-project, protocol-dir), emitted-projects);
    debug-message("Scepter modified protocol project");
  end if;

  if (dbe-emit-stubs-project?(be))
    format(dbe-stubs-stream(be), "\n/* eof */\n");
    close(dbe-stubs-stream(be));
    generate-hdp-project(stubs-project, stubs-dir);
    emitted-projects := pair(project-HDP-file(stubs-project, stubs-dir), emitted-projects);
    debug-message("Scepter modified stubs project");
  end if;

  if (dbe-emit-skeletons-project?(be))
    format(dbe-skeletons-stream(be), "\n/* eof */\n");
    close(dbe-skeletons-stream(be));
    generate-hdp-project(skeletons-project, skeletons-dir);
    emitted-projects := pair(project-HDP-file(skeletons-project, skeletons-dir), emitted-projects);
    debug-message("Scepter modified skeletons project");
  end if;

  let subprojects = #();
  if (scepter.scepter-protocol-option?)
    subprojects := list(project-HDP-file(protocol-project, protocol-dir));
  end if;
  if (scepter.scepter-stubs-option?)
    subprojects := pair(project-HDP-file(stubs-project, stubs-dir), subprojects);
  end if;
  if (scepter.scepter-skeletons-option?)
    subprojects := pair(project-HDP-file(skeletons-project, skeletons-dir), subprojects);
  end if;
  if (empty?(subprojects))
    subprojects := list(project-HDP-file(protocol-project, protocol-dir),
			project-HDP-file(stubs-project, stubs-dir),
			project-HDP-file(skeletons-project, skeletons-dir));
  end if;

  make(<dylan-back-end-result>,
       success?: #t,
       subprojects: subprojects,
       modified-projects: emitted-projects);
end method;

define method protocol-project-name (base :: <string>) => (name :: <string>)
  concatenate(base, "-protocol");
end method;

define method stubs-project-name (base :: <string>) => (name :: <string>)
  concatenate(base, "-stubs");
end method;

define method skeletons-project-name (base :: <string>) => (name :: <string>)
  concatenate(base, "-skeletons");
end method;

define method protocol-project-directory (parent-directory :: <directory-locator>)
 => (directory :: <directory-locator>)
  let scepter = get-scepter();
  subdirectory-locator(parent-directory,
		       concatenate(scepter.scepter-dylan-subdir-prefix, "protocol"))
end method;

define method stubs-project-directory (parent-directory :: <directory-locator>)
 => (directory :: <directory-locator>)
  let scepter = get-scepter();
  subdirectory-locator(parent-directory,
		       concatenate(scepter.scepter-dylan-subdir-prefix, "stubs"))
end method;

define method skeletons-project-directory (parent-directory :: <directory-locator>)
 => (directory :: <directory-locator>)
  let scepter = get-scepter();
  subdirectory-locator(parent-directory,
		       concatenate(scepter.scepter-dylan-subdir-prefix, "skeletons"))
end method;

/*
define method project-library-file (project :: <project-description>, directory :: <directory-locator>)
 => (library-file :: <locator>)
  merge-locators(make(<file-locator>, base: concatenate(project-name(project), "-library"), 
		                      extension: "dylan"),
		 directory)
end method;

define method project-module-file (project :: <project-description>, directory :: <directory-locator>)
 => (module-file :: <locator>)
  merge-locators(make(<file-locator>, base: concatenate(project-name(project), "-module"),
		                      extension: "dylan"),
		 directory)
end method;
*/

define method project-source-file (project-name :: <string>, directory :: <directory-locator>)
 => (source-file :: <locator>)
  merge-locators(make(<file-locator>, base: project-name, extension: "dylan"),
		 directory)
end method;

define method make-protocol-project (base-name :: <string>, exports :: <sequence>) => (project :: <project-description>)
  let scepter = get-scepter();
  let name = protocol-project-name(base-name);

  let used-libraries = scepter.scepter-dylan-orb-runtime-libraries;
  let library = make(<library-description>, name: name, uses: used-libraries, exports: vector(name));

  let used-modules = scepter.scepter-dylan-orb-runtime-modules;
  let module = make(<module-description>, name: name, uses: used-modules, exports: exports);

  let subprojects = if (scepter.scepter-orb-project-file) vector(scepter.scepter-orb-project-file) else #[] end;

  let project = make(<project-description>, name: name);
  project-add-property(project, $project-library, library);
  project-add-property(project, $project-modules, vector(module));
  let files = vector(project-library-file-base-name(project), project-module-file-base-name(project), name);
  project-add-property(project, $project-files, files);
  project-add-property(project, $project-target-type, "dll");
  project-add-property(project, $project-subprojects, subprojects);
  project;
end method;

define method make-stubs-project (base-name :: <string>, exports :: <sequence>) => (project :: <project-description>)
  let scepter = get-scepter();
  let name = stubs-project-name(base-name);
  let protocol-name = protocol-project-name(base-name);

  let used-libraries = concatenate(scepter.scepter-dylan-orb-runtime-libraries, vector(protocol-name));
  let library = make(<library-description>, name: name, uses: used-libraries, exports: vector(name));

  let used-modules = concatenate(scepter.scepter-dylan-orb-runtime-modules, vector(concatenate(protocol-name, ", export: all")));
  let module = make(<module-description>, name: name, uses: used-modules, exports: exports);

  let parent-dir = make(<directory-locator>, path: vector(#"parent"));
  let protocol-dir = subdirectory-locator(parent-dir, concatenate(scepter.scepter-dylan-subdir-prefix, "protocol"));
  let protocol-project-file = make(<file-locator>,
				   directory: protocol-dir,
				   base: protocol-name,
				   extension: $user-project-suffix);
  let subprojects = if (scepter.scepter-orb-project-file)
		      vector(scepter.scepter-orb-project-file, protocol-project-file);
		    else
		      vector(protocol-project-file);
		    end if;

  let project = make(<project-description>, name: name);
  project-add-property(project, $project-library, library);
  project-add-property(project, $project-modules, vector(module));
  let files = vector(project-library-file-base-name(project), project-module-file-base-name(project), name);
  project-add-property(project, $project-files, files);
  project-add-property(project, $project-target-type, "dll");
  project-add-property(project, $project-subprojects, subprojects);
  project;
end method;

define method make-skeletons-project (base-name :: <string>, exports :: <sequence>) => (project :: <project-description>)
  let scepter = get-scepter();
  let name = skeletons-project-name(base-name);
  let protocol-name = protocol-project-name(base-name);
  let stubs-name = stubs-project-name(base-name);

  let used-libraries = concatenate(scepter.scepter-dylan-orb-runtime-libraries, vector(protocol-name, stubs-name));
  let library = make(<library-description>, name: name, uses: used-libraries, exports: vector(name));

  let used-modules = concatenate(scepter.scepter-dylan-orb-runtime-modules,
				 vector(protocol-name, concatenate(stubs-name, ", export: all")));
  let module = make(<module-description>, name: name, uses: used-modules, exports: exports);

  let parent-dir = make(<directory-locator>, path: vector(#"parent"));
  let protocol-dir = subdirectory-locator(parent-dir, concatenate(scepter.scepter-dylan-subdir-prefix, "protocol"));
  let protocol-project-file = make(<file-locator>,
				   directory: protocol-dir,
				   base: protocol-name,
				   extension: $user-project-suffix);
  let stubs-dir = subdirectory-locator(parent-dir, concatenate(scepter.scepter-dylan-subdir-prefix, "stubs"));
  let stubs-project-file = make(<file-locator>,
				directory: stubs-dir,
				base: stubs-name,
				extension: $user-project-suffix);
  let subprojects = if (scepter.scepter-orb-project-file)
		      vector(scepter.scepter-orb-project-file, protocol-project-file, stubs-project-file);
		    else
		      vector(protocol-project-file, stubs-project-file);
		    end if;

  let project = make(<project-description>, name: name);
  project-add-property(project, $project-library, library);
  project-add-property(project, $project-modules, vector(module));
  let files = vector(project-library-file-base-name(project), project-module-file-base-name(project), name);
  project-add-property(project, $project-files, files);
  project-add-property(project, $project-target-type, "dll");
  project-add-property(project, $project-subprojects, subprojects);
  project;
end method;


define method project-up-to-date? (project :: <project-description>, target-dir :: <directory-locator>, idl-modification-date :: <date>)
 => (well? :: <boolean>)

  local method file-up-to-date? (file :: <locator>) => (well? :: <boolean>)
          if (file-exists?(file))
	    let modification-date = file-property(file, #"modification-date");
            modification-date > idl-modification-date;
	  else
	    #f;
	  end if;
	end method;

  let files = project-files(project, target-dir);
  let HDP-file = project-HDP-file(project, target-dir);
  files := add!(files, HDP-file);
  let up-to-date? = every?(file-up-to-date?, files);
  debug-message("Project %s up to date? %=", as(<string>, HDP-file), up-to-date?);
  up-to-date?;
end method;



//
// EMIT-AST-NODE
//
define generic emit-ast-node (back-end :: <dylan-back-end>, node :: <ast-declarator>, #key last? :: <boolean>)
 => ();

define method emit-ast-node (back-end :: <dylan-back-end>, node :: <ast-declarator>, #key last? :: <boolean>)
 => ()
  do-emit-ast-node(back-end, node, last?: last?);
end method;

define method emit-ast-node (back-end :: <dylan-back-end>, node :: <ast-root>, #key last? :: <boolean>)
 => ()
end method;

define method emit-ast-node (back-end :: <dylan-back-end>, node :: <ast-predefined-type>, #key last? :: <boolean>)
 => ()
end method;

define method emit-ast-node (back-end :: <dylan-back-end>, node :: <ast-enum-value>, #key last? :: <boolean>)
 => ()
end method;

define method emit-ast-node (back-end :: <dylan-back-end>, node :: <ast-field>, #key last? :: <boolean>)
 => ()
end method;

define method emit-ast-node (back-end :: <dylan-back-end>, node :: <ast-attribute>, #key last? :: <boolean>)
 => ()
  do-emit-ast-node(back-end, node, last?: last?)
end method;

define method emit-ast-node (back-end :: <dylan-back-end>, node :: <ast-sequence>, #key last? :: <boolean>)
 => ()
end method;

define method do-emit-ast-node (back-end :: <dylan-back-end>, node :: <ast-declarator>, #key last? :: <boolean>)
 => ()
  when (emit-declarator?(back-end, node))
    let model = make(<dim>, node: node);
    before-code-emission(model);
    emit-code(back-end, model);
    after-code-emission(model);
  end when;
end method;

