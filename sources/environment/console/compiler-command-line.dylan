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

  keyword back-end :: <symbol>           = "the compiler back-end to use";
  keyword build-script :: <file-locator> = "the (Jam) build script";
  keyword target :: <symbol>             = "the type of binary to create (executable or dll)";
  keyword dispatch-coloring :: <symbol>  = "the dispatch coloring output type";

  flag help             = "show this help summary";
  flag logo             = "display the copyright banner";
  flag version          = "display the version";
  flag shortversion     = "display the short version";
  flag debugger         = "enter the debugger if this program crashes";
  flag echo-input       = "echo all input to the console";
  flag verbose          = "show verbose output";

  keyword show :: <command-property> = "show a compiler setting";

  flag import           = "import the project";
  flag build            = "build the project";
  flag compile          = "compile the project";
  flag link             = "link the project";
  flag clean            = "force a clean build of the project";
  flag release          = "build a release for the project";
  flag subprojects      = "build subprojects as well if necessary";
  flag force            = "force relink the executable";

  keyword jobs :: <integer> = "number of concurrent build jobs";

  keyword personal-root :: <directory-locator> = "personal area root";
  keyword system-root   :: <directory-locator> = "system area root";
  keyword internal-debug :: $keyword-list-type
    = "show debug messages (comma separated, e.g. linker,project-manager)";
  flag unify            = "combine libraries into a single executable";
  flag profile-commands = "profile the execution of each command";
  flag harp             = "generate HARP output";
  flag assemble         = "generate assembly-language output";
  flag dfm              = "generate Dylan Flow Machine output";
  flag allow-serious-warnings
    = "exit with success status if there are serious warnings [off by default]";
end command-line main;
