Module:    console-environment
Synopsis:  The command line version of the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Main command
///
/// This is the version used by external editions.

define class <main-command> (<basic-main-command>)
end class <main-command>;

define command-line main => <main-command>
    (summary:       "command-line version of Open Dylan",
     documentation: "Command-line version of Open Dylan.")
  optional project :: <file-locator>  = "the project to be built";

  keyword build-script :: <file-locator> = "the (Jam) build script to use";
  keyword target :: <symbol> = "the target";
  keyword arch :: <symbol> = "the architecture to build (e.g. i386 or x86_64)";

  flag help        = "show this help summary";
  flag logo        = "displays the copyright banner";
  flag debugger    = "enter the debugger if this program crashes";
  flag echo-input  = "echoes all input to the console";

  flag import      = "import the project";
  flag build       = "build the project";
  flag compile     = "compile the project";
  flag link        = "link the project";
  flag clean       = "force a clean build of the project";
  flag release     = "build a release for the project";
  flag subprojects = "build subprojects as well if necesssary";
  flag force       = "force relink the executable";
end command-line main;


/// Internal main command
///
/// This is the version used by the internal edition.

define class <internal-main-command> (<basic-main-command>)
end class <internal-main-command>;

define command-line internal-main => <internal-main-command>
    (summary:       "command-line version of Open Dylan",
     documentation: "Command-line version of Open Dylan.")
  optional project :: <file-locator> = "the project to be built";

  keyword build-script :: <file-locator> = "the (Jam) build script to use";
  keyword target :: <symbol> = "the type of executable to generate";
  keyword arch :: <symbol> = "the architecture to build (e.g. i386 or x86_64)";
  keyword debug-info :: <symbol>  = "control the debug info generated";

  flag help             = "show this help summary";
  flag logo             = "displays the copyright banner";
  flag debugger         = "enter the debugger if this program crashes";
  flag echo-input       = "echoes all input to the console";

  flag import           = "import the project";
  flag build            = "build the project";
  flag compile          = "compile the project";
  flag link             = "link the project";
  flag clean            = "force a clean build of the project";
  flag release          = "build a release for the project";
  flag subprojects      = "build subprojects as well if necessary";
  flag force            = "force relink the executable";

  // Internal-only options
  keyword personal-root :: <directory-locator> = "personal area root";
  keyword system-root   :: <directory-locator> = "system area root";
  keyword internal-debug :: $keyword-list-type
                        = "show debug messages (e.g. for linker,project-manager)";
  flag unify            = "combine the libraries into a single executable";
  flag profile-commands = "profile the execution of each command";
  flag harp             = "generate HARP output";
  flag assemble         = "generate assembly-language output";
  flag dfm              = "generate Dylan Flow Machine output";

  // Backwards-compatibility options for pentium-dw users
  flag not-recursive    = "don't build subprojects as well";
  flag microsoft        = "use the Microsoft linker";
  flag gnu              = "use the GNU linker";
  flag save             = "save compiler databases";
  flag link-dll         = "link as a DLL";
  flag link-exe         = "link as an EXE";
  flag gnu-exports      = "link the GNU exports";
  keyword messages :: <symbol>  = "control the progress messages generated";
end command-line internal-main;
