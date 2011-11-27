Module:    console-environment
Synopsis:  The command line version of the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Main command

define class <main-command> (<basic-main-command>)
end class <main-command>;

define command-line main => <main-command>
    (summary:       "command-line version of Open Dylan",
     documentation: "Command-line version of Open Dylan.")
  optional project :: <file-locator> = "the project to be built";

  keyword build-script :: <file-locator> = "the (Jam) build script";
  keyword target :: <symbol> = "the type of executable";
  keyword arch :: <symbol> = "the architecture (e.g. i386 or x86_64)";

  flag help             = "show this help summary";
  flag logo             = "displays the copyright banner";
  flag version          = "displays the version";
  flag shortversion     = "displays the shortversion";
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

  keyword personal-root :: <directory-locator> = "personal area root";
  keyword system-root   :: <directory-locator> = "system area root";
  keyword internal-debug :: $keyword-list-type
                        = "show debug messages (e.g. for linker,project-manager)";
  flag unify            = "combine libraries into a single executable";
  flag profile-commands = "profile the execution of each command";
  flag harp             = "generate HARP output";
  flag assemble         = "generate assembly-language output";
  flag dfm              = "generate Dylan Flow Machine output";

  // Backwards-compatibility options for pentium-dw users
  flag save             = "save compiler databases";
end command-line main;
