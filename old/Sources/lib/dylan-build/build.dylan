Module:    dylan-build
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Person-to-blame: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant dbg-msg = method(#rest args) #f end;

define function %dbg-msg(string, #rest args)
  apply(format-out, string, args);
  force-output(*standard-output*);
end;

define constant $dylanmakefile = "dylanmakefile.mkf";

// The Build Data Structure

define primary class <build>(<object>)
  slot targets :: <vector> = #[];
  slot more-targets? :: <boolean> = #f;
  slot target :: <integer> = 0;
  slot dylanapp :: false-or(<string>), required-init-keyword: dylanapp:;
  slot dylanlib :: <symbol>, required-init-keyword: library:;
  slot base :: <string>, required-init-keyword: base:;
  slot linkopts :: <list>, required-init-keyword: linkopts:;
  slot objs :: <list>, required-init-keyword: objs:;
  slot used-libs :: <table>, required-init-keyword: used-libs:;
  slot c-headers :: <list>, required-init-keyword: c-headers:;
  slot c-objs :: <list>, required-init-keyword: c-objs:;
  slot c-src-objs :: <list>, required-init-keyword: c-src-objs:;
  slot rc-objs :: <list>, required-init-keyword: rc-objs:;
  slot c-libs :: <list>, required-init-keyword: c-libs:;
  slot rtlibs :: <list>, required-init-keyword: rtlibs:;
  slot libfile :: <string>, required-init-keyword: libfile:;
  slot dll :: <string>, required-init-keyword: dll:;
  slot app :: <string>, required-init-keyword: app:;
  slot indentation :: <string>, required-init-keyword: indentation:;
  slot dylanecho? :: <boolean>, required-init-keyword: echo:;
  slot force? :: <boolean>, required-init-keyword: force:;
  slot test? :: <boolean>, required-init-keyword: test:;
  slot build-dll? :: <boolean> = #f;
  slot build-exe? :: <boolean> = #f;
  slot top-level? :: <boolean>;
end class;


define thread variable *build* = #f;


define class <build-error>(<condition>)
  
end class;

// Wrap this around anything which constitutes a single "build
// transaction".
define macro with-used-project-cache
  { with-used-project-cache ?:body end }
    => { do-with-used-project-cache(method () ?body end) }
end macro;

// A cache used to avoid checking for dependencies of projects multple times
// for multiply-used libraries during project building
define thread variable *used-project-cache* = #f;

define function do-with-used-project-cache (fn)
  if (*used-project-cache*)
    fn()
  else
    dynamic-bind (*used-project-cache* = make(<table>))
      fn()
    end
  end if
end function;

// Toplevel function invoked from Shell

define method build() => ()
  block()
    build-system(application-arguments());
  exception(condition :: <serious-condition>)
    format-out("%s", condition);
  end block;
end method;

// Toplevel internal function that can be invoked by Dylan Clients

define method build-system(build-targets :: <sequence>,
			   #key 
			     toplevel? = #t,
			   test? = #f,
			   directory = working-directory()) => ()

  block()
    environment-variable("LIB") :=
      concatenate($personal-lib, ";", $system-lib, ";",
		  environment-variable("LIB"));
    with-used-project-cache
      build-system-internal(build-targets,
			    toplevel?: toplevel?,
			    directory: directory)
    end;
  exception(b :: <build-error>)
  end block;

end method;

define method build-system-internal(build-targets :: <sequence>,
				    #key toplevel? = #t,
				         directory) => (build-dll?, build-exe?)

  with-directory(directory)
    dynamic-bind(*build* = read-buildfile(toplevel?: toplevel?))
      *build*.top-level? := toplevel?;
      *build*.targets := build-targets;
       dbg-msg("\nSpecified Targets: %=\n", *build*.targets);
       dbg-msg("\n APP: %= OBJS: %= LIBS: %= \n", *build*.dylanapp, *build*.objs, *build*.used-libs.key-sequence);
      do-targets();
      values(*build*.build-dll?, *build*.build-exe?);
    end dynamic-bind;
  end;

end method;

// Read the makefile and set up the <build> Data Structure

define method read-buildfile(#key toplevel? = #t) => (build :: <build>)

  if (~ file-exists?($dylanmakefile))
    build-error("Invalid Build Directory: dylanmakefile not found");
  end if;

  let variables = read-file-header(as(<locator>, $dylanmakefile));
  let dylan-lib = element(variables, #"library", default: #f);
  unless(dylan-lib)
    build-error("library entry is missing in the dylanmakefile")
  end;
  let dylan-app = first(element(variables, #"executable", default: #f) | dylan-lib);
  
  //This will give us build locations of used projects
  let used-libs  = make(<table>);
  let build-list = element(variables, #"used-projects", default: #());
  ~empty?(build-list) // this shouldn't be necessary
    &
    for(i from 0 below size(build-list) by 3)
      used-libs[as(<symbol>, build-list[i])] := build-list[i + 2];
    end;

  let objects = 
    pair("_glue.obj", 
	 map(rcurry(filename-with-extension, "obj"), 
	     element(variables, #"files", default: #())));

  let base-number  = string-to-machine-word(first(element(variables, #"base-address", default: #("0x669E0000"))));
  let base = format-to-string("/base:%s", machine-word-to-string(base-number, prefix: "0x"));
 
  make(<build>,
       dylanapp:    dylan-app,
       library:     as(<symbol>, first(dylan-lib)),
       objs:        objects,
       used-libs:   used-libs,
       base:        base,
       linkopts:    element(variables, #"linkopts", default: #()),
       c-headers:   element(variables, #"c-headers", default: #()),
       c-objs:      element(variables, #"c-objs", default: #()),
       c-src-objs:  element(variables, #"c-src-objs", default: #()),
       rc-objs:     element(variables, #"rc-objs", default: #()),
       c-libs:      substitute-environment-variables(element(variables, #"all-c-libraries", default: #())),
       rtlibs:      element(variables, #"rtlibs", default: #()),
       libfile:     filename-with-extension(dylan-app, "lib"),
       dll:         filename-with-extension(dylan-app, "dll"),
       app:         filename-with-extension(dylan-app, "exe"),
       indentation: if (toplevel?) "  "
		    else concatenate("  ", *build*.indentation)
		    end if,
       force:       *build* & *build*.force?,
       echo:        *build* & *build*.dylanecho?,
       test:        *build* & *build*.test?)

end method;



