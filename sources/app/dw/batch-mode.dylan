Module:    dw
Synopsis:  Batch mode compilation handling
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define function run-batch-compile 
    (#rest arguments :: <string>) => ()
  let debugger?      :: <boolean> = #f;
  let compile?       :: <boolean> = #t;
  let save?          :: <boolean> = #f;
  let link?          :: <boolean> = #t;
  let clean?         :: <boolean> = #f;
  let harp?          :: <boolean> = #f;
  let assemble?                   = unsupplied();
  let dfm?           :: <boolean> = #f;
  let debug-info?    :: <boolean> = #t;
  let linker                      = unsupplied();
  let dll?           :: <boolean> = #f;
  let exe?           :: <boolean> = #f;
  let banner?        :: <boolean> = #t;
  let exports?       :: <boolean> = #f;
  let gnu-exports?   :: <boolean> = #f;
  let not-recursive? :: <boolean> = #f;
  let combine-dlls?  :: <boolean> = #f;
  let interpret?                  = #f;
  let messages :: false-or(<symbol>) = #f;
  let gc? = #f;
  let gc-stats? = #f;
  let mode = #f;
  let platform = #f;
  let root = #f;
  let libraries :: <sequence> = make(<stretchy-vector>);

  for (argument in arguments)
    let option? = size(argument) > 0 & is-option?(argument);
    if (option?)
      let (option :: <string>, param :: <string>) =
	parse-option(copy-sequence(argument, start: 1));

      select (option by \=)
	"logo"            => banner?      := #t;
	"nologo"          => banner?      := #f;
	"debugger"        => debugger?    := #t;
	"quiet"           => messages     := #"none";
	"save"            => save?        := #t;
	"compile"         => compile?     := #t;
	"nocompile"       => compile?     := #f;
	"loose"           => mode         := #"loose";
	"tight"           => mode         := #"tight";
	"link"            => link?        := #t;
	"nolink"          => link?        := #f;
	"elf"             => link?        := #t; elf-linker-options();
	"gnu"             => link?        := #t; linker := #"gnu";
	"microsoft"       => link?        := #t; linker := #"microsoft";
	"link-dll", "dll" => link?        := #t; dll? := #t;
	"link-exe", "exe" => link?        := #t; exe? := #t;
	"clean", "force"  => clean?       := #t;
	"harp"            => harp?        := #t;
	"interpret"       => interpret?   := #t;
	  case
	    param.empty?        => interpret? := #"library";
	    param = "library"   => interpret? := #"library";
	    param = "top-level" => interpret? := #"top-level";
	    otherwise      =>
	      invalid-option-error(argument);
	  end case;
	"platform"        =>
	  case
	    param.empty?   => invalid-option-error(argument);
	    otherwise      => platform := param;
	  end case;
	"root"             =>
	  case
	    param.empty?   => invalid-option-error(argument);
	    otherwise      => root := param;
	  end case;
	"assemble"           =>
	  case
	    param.empty?   =>
	      select (os/$os-name)
		#"win32"  => assemble? := #"mnemonic-assembler";
		#"linux"  => assemble? := #"linux-outputter";
		otherwise => assemble? := #"gnu-outputter";
	      end;
	    param = "masm" =>
	      assemble? := #"mnemonic-assembler";
	    param = "gnu"  =>
	      assemble? := #"gnu-outputter";
	    param = "elf"  =>
	      assemble? := #"linux-outputter";
	    otherwise      =>
	      invalid-option-error(argument);
	  end case;
	  select (assemble?)
	    #"gnu-outputter"      => current-native-back-end() := $gnu-backend-class;
	    #"linux-outputter"    => current-native-back-end() := $elf-backend-class;
	    #"mnemonic-assembler" => current-native-back-end() := $masm-backend-class;
	  end;
	"dfm"             => dfm?     := #t;
	"debug"           =>
	  case
	    param.empty?   => #f;
	    param = "full" => #f;
	    param = "min"  => debug-info? := #f;
	    param = "none" => debug-info? := #f;
	    otherwise      =>
	      invalid-option-error(argument);
	  end case;
	"messages"         =>
	  case
	    param.empty?       => messages := #f;
	    param = "internal" => messages := #"internal";
	    param = "none"     => messages := #"none";
	    otherwise      =>
	      invalid-option-error(argument);
	  end case;
	"exports"         => exports? := #t;
	"gnu-exports"     => gnu-exports? := #t;
	"not-recursive"   => not-recursive? := #t;
	"gc"              => 
	  if (param.empty?)
	    gc? := #"default";
	  else
	    gc? := string-to-integer(param);
	  end if;
	  gc-stats? := #t;
	"gc-stats"        => gc-stats? := #t;
	"combine"         => *combine-object-files?* := #t;
	"nocombine"       => *combine-object-files?* := #f;
	"combine-dlls"    => combine-dlls? := #t;
	"profile"         =>
	  case
	    param.empty?   =>
	      format-out("No profiling activated -- please supply an option\n");
	    param = "allocation" => *dfmc-profile-allocation?* := #t;
	    otherwise      =>
	      invalid-option-error(argument);
	  end case;
	"help", "?"       => display-help-and-exit();
	otherwise         => invalid-option-error(argument);
      end;
    else
      add!(libraries, argument);
    end
  end;
  if (empty?(libraries))
    format-out("No libraries were specified to be compiled\n");
    format-out("Exiting with return code %d\n", 
	       $compiler-error-return-code);
    os/exit-application($compiler-error-return-code)
  end;
  banner? & display-banner(*top-level-loop*);
  let handler (<serious-condition>)
    = method (condition :: <serious-condition>, handler :: <function>)
	if (debugger?)
	  handler()
	else
	  display-condition(condition);
	  format-out("Exiting with return code %d\n", 
		     $compiler-error-return-code);
	  os/exit-application($compiler-error-return-code)
	end
      end;
  let handler (<project-fatal-error>)
    = method (condition :: <project-fatal-error>, handler :: <function>)
	display-condition(condition, prefix: "Fatal error");
	format-out("Exiting with return code %d\n", $compiler-error-return-code);
	os/exit-application($compiler-error-return-code)
      end;
  //---*** It would be nice to have more control than this...
  if (messages)
    show-internal-compiler-messages?() := (messages == #"internal")
  end;
  if (interpret?)
    for (library in libraries)
      interpret-library-command(library, clean?, save?);
      if (interpret? == #"top-level")
	interpret-top-level-command(library);
      end if;
    end for;
  else
    batch-compile
      (libraries, 
       not-recursive?: not-recursive?,
       compile?: compile?, clean?: clean?, save?: save?, 
       harp?: harp?, assemble?: assemble?, dfm?: dfm?,
       debug-info?: debug-info?, gc?: gc?, gc-stats?: gc-stats?,
       combine-dlls?: combine-dlls?,
       link?: link?, dll?: dll?, linker: linker, mode: mode,
       exports?: exports?, gnu-exports?: gnu-exports?, root: root, platform: platform);
  end if;
end function run-batch-compile;

define function invalid-option-error (option :: <string>) => ()
  format-out("Invalid option %s supplied, use /help for usage\n", option);
  os/exit-application($compiler-error-return-code)
end function invalid-option-error;

define function display-options () => ()
  let cl = *top-level-loop*;
  display-banner(cl);
  let locator = as(<file-locator>, os/application-filename());
  let application-name = as-uppercase(as(<string>, locator-base(locator)));
  format-out
    ("\nUsage: %s"
     "\n  Enter interactive compilation session"
     "\n"
     "\nUsage: %s [options] [libraries]"
     "\n"
     "\n  Compile the specified libraries"
     "\n"
     "\n  Options:"
     "\n"
     "\n  /not-recursive   - don't process the subprojects of the project"
     "\n  /clean           - force a clean build of the project"
     "\n  /save            - save compilation results into databases"
     "\n  /nocompile       - don't compile the project"
     "\n"
     "\n  /dll             - force the project to be linked as a DLL"
     "\n  /exe             - force the project to be linked as an executable"
     "\n  /microsoft       - use the Microsoft linker"
     "\n  /gnu             - use the GNU linker"
     "\n  /elf             - use the ELF linker"
     "\n  /nolink          - don't link the project"
     "\n"
     "\n  /nologo          - suppress the copyright banner"
     "\n  /debugger        - enter the debugger if the compiler crashes"
     "\n",
     application-name,
     application-name);
   display-internal-options();
end function display-options;

// Describe all internal options here

define function display-internal-options () => ()
  let locator = as(<file-locator>, os/application-filename());
  let application-name = as-uppercase(as(<string>, locator-base(locator)));

  format-out
    ("\n  /loose               - compile a project in interactive development mode"
     "\n  /tight               - compile a project in production mode"
     "\n"
     "\n  /root:pathname       - sets the system and personal root directories"
     "\n  /platform:name       - targets the specified platform (e.g., x86-win32)"
     "\n"
     "\n  /harp                - generate readable form of object files (.harp files)"
     "\n  /dfm                 - generate DFM files"
     "\n  /assemble            - generate Microsoft assembler files"
     "\n  /assemble:masm       - generate Microsoft assembler files"
     "\n  /assemble:gnu        - generate GNU assembler files"
     "\n  /assemble:elf        - generate ELF assembler files"
     "\n"
     "\n  /gnu-exports         - generate exports files for GNU Linker"
     "\n"
     "\n  /debug[:full]        - generate full native debug info for DLLs"
     "\n  /debug:min           - generate minimal native debug info for DLLs"
     "\n  /debug:none          - generate minimal native debug info for DLLs"
     "\n"
     "\n  /gc[:Megs]           - activate full GC after project compilation"
     "\n                         if Memory Usage exceeds Megs (default: 50 Megs)"
     "\n"
     "\n  /nocombine           - create multiple object files per library"
     "\n  /combine-dlls        - link all DLLs into one executable"
     "\n"
     "\n  /profile:allocation  - profile heap allocation"
     "\n"
     "\n  /messages            - display internal progress messages in internal edition"
     "\n  /messages:internal   - display internal progress messages in any edition"
     "\n  /messages:none       - display external progress messages in any edition"
     "\n",
     application-name,
     application-name);
end function display-internal-options;

define function display-help-and-exit () => ()
  display-options();
  os/exit-application(0)
end function display-help-and-exit;


define function batch-compile
    (projects :: <sequence>,
     #rest keys,
     #key not-recursive?, target-type, compile? = #t, mode, clean?, save?,
          harp?, assemble?, dfm?, debug-info? = #t, gc?, gc-stats?, combine-dlls?,
          link?, dll?, exe?, linker, exports?, gnu-exports?, root, platform)
 => ()
  if (root)
    do-set-system-root(root);
    do-set-personal-root(root)
  end;
  if (platform) 
    unless (do-set-target-platform(platform)) 
      error("Unable to set target platform to %s", platform)
    end
  end;
  if (save?) write-databases?() := #t end;
  let cl = *top-level-loop*;
  local method compile-project
	    (project-name, 
	     #key not-recursive?, target-type, compile?, mode, clean?, save?,
	          harp?, assemble?, dfm?, debug-info?, gc?, gc-stats?, recursive? = #t,
	          combine-dlls?,
	          link?, dll?, exe?, linker, exports?, gnu-exports?)
    with-compiler-transaction
      let project = open-project-from-name(project-name);
      if (project)
	if (mode)
	  project-compilation-mode(project) := mode
	end;
	let target-type
	  = case
	      dll?      => #"dll";
	      exe?      => #"executable";
	      otherwise => project-target-type(project)
	    end;
	project-target-type(project) := target-type;
	if (compile?)
	  format-out("\nCompiling project %s\n\n", project-name);
	  if (not-recursive?)
	    compile-library(project, 
			    abort-on-serious-warnings?: #f,
			    abort-on-all-warnings?: #f,
			    save?: save?, 
			    force-parse?: clean?, force-compile?: clean?,
			    harp-output?: harp?, assembler-output?: assemble?, 
			    dfm-output?: dfm?,
			    debug-info?: debug-info?, gc-stats?: gc-stats?)
	  else
	    // if we are compiling the toplevel project at GC time,
	    // decline to signal a GC
	    let handler (<garbage-collection>) =
	      method (gc :: <garbage-collection>, next-handler)
		unless (gc.garbage-collection-info = project-name)
		  next-handler();
		end unless;
	      end method;
	      update-libraries(project, 
			       abort-on-serious-warnings?: #f,
			       abort-on-all-warnings?: #f,
			       save?: save?, force?: clean?,
			       harp-output?: harp?, assembler-output?: assemble?,
			       dfm-output?: dfm?,
			       debug-info?: debug-info?,
			       gc?: gc?, gc-stats?: gc-stats?, recursive?: recursive?);
	  end
	end;
	if (link?)
	  format-out("\nLinking project %s\n", project-name);
	  unless (link-library-with-options
		    (project,
		     linker: linker,
		     dll?: dll?, exe?: exe?, combine?: combine-dlls?,
		     exports?: exports?, force?: clean?,
		     not-recursive?: not-recursive?))
	    os/exit-application($compiler-error-return-code)
	  end
	end;
	if (gnu-exports?)
	  format-out("\nGenerating GNU exports for project %s\n", project-name);
	  unless (link-library-with-options(project, linker: #"gnu", exports?: #t))
	    os/exit-application($compiler-error-return-code)
	  end
	end;
	format-out("\nSaving project %s with new settings\n", project-name);
	save-project(project)
      else
	format-out("\nProject %s not found\n\n", project-name);
	os/exit-application($compiler-error-return-code)
      end;

    end with-compiler-transaction;
  end method;

  local method compile-subproject
	    (project, 
	     #key clean?, save?, harp?, assemble?, dfm?, debug-info?, gc?, gc-stats?,
	     #all-keys)
	  with-compiler-transaction
	    update-libraries(project, 
			     abort-on-serious-warnings?: #f,
			     abort-on-all-warnings?: #f,
			     save?: save?, force?: clean?,
			     harp-output?: harp?, assembler-output?: assemble?, 
			     dfm-output?: dfm?,
			     debug-info?: debug-info?,
			     gc?: gc?, gc-stats?: gc-stats?, recursive?: #f);
	  end with-compiler-transaction;
  end method;

  for (project in projects)
    if (gc?)
      apply(compile-project-with-gc, project,
	    compile-project, compile-subproject, keys);
    else
      apply(compile-project, project, keys)
    end if;
  end for;
end function batch-compile;
