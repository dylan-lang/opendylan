Module:       database-stress-tool
Author:       Andy Armstrong
Synopsis:     A simple database stress tool
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Main

define method main (arguments :: <sequence>) => ()
  let database   = $database;
  let table      = $table;
  let statement  = $statement;
  let phases     = $phases;
  let iterations = $iterations;
  let share-connections? = #f;
  let quiet? = #f;
  let truncate? = #f;
  let transactions? = #f;
  let commit? = #t;
  let debug? = #f;
  let arguments-size = size(arguments);
  let i :: <integer> = 0;
  while (i < arguments-size)
    let argument = arguments[i];
    let next-argument = if (i < arguments-size - 1) arguments[i + 1] end;
    select (argument by \=)
      "/help"         => display-help();
      "/database"     => database := next-argument; i := i + 1;
      "/table"        => table := next-argument; i := i + 1;
      "/statement"    => statement := next-argument; i := i + 1;
      "/iterations"   => iterations := string-to-integer(next-argument); i := i + 1;
      "/phases"       => phases := string-to-integer(next-argument); i := i + 1;
      "/update"       => statement := $update-statement;
      "/truncate"     => truncate? := #t;
      "/commit"       => commit? := #t;
      "/rollback"     => commit? := #f;
      "/transactions" => transactions? := #t;
      "/share"        => share-connections? := #t;
      "/quiet"        => quiet? := #t;
      "/debug"        => debug? := #t;
      otherwise       => format-out("Ignoring unrecognized argument %s\n", argument);
    end;
    i := i + 1;
  end;
  let test
    = make(<database-stress-test>,
           database: database,
           table: table,
           statement: statement,
           phases: phases,
           iterations: iterations,
           truncate?: truncate?,
           transactions?: transactions?,
           commit?: commit?,
           share-connections?: share-connections?,
           quiet?: quiet?);
  if (debug?)
    perform-test(test)
  else
    block ()
      perform-test(test)
    exception (error :: <diagnostic>)
      format-out("\n\nSQL Error: %s", error);
      exit-application(-1)
    end
  end
end method main;

define method display-help
    () => ()
  format-out("database-stress-tool\n\n");
  format-out("Usage: database-stress-tool [options *]\n\n");
  format-out("Options:\n");
  format-out("  /help         - show this help");
  format-out("  /database     - the database to use [default: %s]\n", $database);
  format-out("  /table        - the table to use [default: %s]\n", $table);
  format-out("  /update       - update the table [default: just query]\n");
  format-out("  /statement    - the SQL statement to use [default: %s]\n", $statement);
  format-out("  /iterations   - how many iterations per phase [default: %s]\n", $iterations);
  format-out("  /phases       - how many phases to run [default: %s]\n", $phases);
  format-out("  /truncate     - truncate the table before starting [default: off]\n");
  format-out("  /share        - share a connection for each phase [default: off]\n");
  format-out("  /transactions - wrap each phase in a transaction [default: off]\n");
  format-out("  /rollback     - don't commit the updates during a transaction [default: commit]\n");
  format-out("  /quiet        - don't show progress information [default: show progress]\n");
  format-out("  /debug        - don't catch errors [default: off]\n");
  exit-application(0);
end method display-help;

begin
  main(application-arguments());
end;
