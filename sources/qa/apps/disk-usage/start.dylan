Module:    disk-usage
Synopsis:  Command line argument parsing
Author:    Carl Gay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $keyword-prefixes = #['-', '/'];

define method process-argument
    (argument :: <string>)
 => (text :: <string>, keyword? :: <boolean>)
  if (keyword-argument?(argument))
    values(copy-sequence(argument, start: 1), #t)
  else
    values(argument, #f)
  end
end method process-argument;

define method keyword-argument? 
    (argument :: <string>) => (keyword? :: <boolean>)
  member?(argument[0], $keyword-prefixes)
end method keyword-argument?;

define method command-line-error
    (description :: <string>, option :: <string>)
  format-out("Invalid %s '%s'.\n"
	     "Use -help for available options.\n",
	     description, option);
  exit-application(0);
end method command-line-error;

define function display-usage
    () => ()
  format-out("Application: %s\n\n"
             "Arguments: old-directory-or-log-file\n"
             "           [new-directory-or-log-file]\n"
             "           [-tolerance <integer>]\n"
             "           [-report {brief | detailed | log}]\n"
             "           [-top <integer>]\n"
             "           [-hd {yes | no }]\n"
             "           [-sort-by {name | size | none}]\n"
             "           [-help]\n",
             application-name());
end function display-usage;

// There must be a better way...
//
define method main () => ()
  let args = as(<deque>, application-arguments());
  let dir1 = #f;
  let dir2 = #f;
  let report-format = #"detailed";
  let tolerance = $default-tolerance;
  let top-n = $default-top-n;
  let hd? = *hd-directory?*;
  let sort-by = #"size";
  while (~empty?(args))
    let arg = pop(args);
    let (option, keyword?) = process-argument(arg);
    select (option by string-equal?)
      "help" =>
        begin
          display-usage();
          exit-application(0);
        end;
      "tolerance" =>
        tolerance := string-to-integer(pop(args));
      "report" =>
	let rep = as(<symbol>, pop(args));
	if (~member?(rep, #(#"log", #"brief", #"detailed")))
	  command-line-error("'report' option", option);
	else
	  report-format := rep;
	end if;
      "top" =>
        top-n := string-to-integer(pop(args));
      "hd" =>
        hd? := char-equal?(pop(args)[0], 'y');
      "sort-by" =>
	let sort = as(<symbol>, pop(args));
        select (sort)
          #"name", #"size", #"none" =>
            sort-by := sort;
          otherwise =>
            command-line-error("'sort-by' option", sort);
        end select;
      otherwise =>
        case
	  dir1 & dir2 =>
	    command-line-error("command line keyword", option);
          dir1 =>
            dir2 := option;
          otherwise =>
            dir1 := option;
        end;
    end select;
  end while;
  ~dir1 & command-line-error("arguments", "No directory specified");
  let info1 :: <dir-info> = if(line-ends-with(dir1, ".log"))
                              read-disk-usage-file(dir1)
                            else
                              dir-infoify(as(<directory-locator>, dir1))
                            end if;
  if (dir2)
    let info2 :: <dir-info> = if(line-ends-with(dir2, ".log"))
                                read-disk-usage-file(dir2)
                              else
                                dir-infoify(as(<directory-locator>, dir2))
                              end if;
    compare-directories(info1, info2,
                        report: report-format,
                        tolerance: tolerance,
                        hd?: hd?,
                        top: top-n,
			sort-by: sort-by);
  else
    report-disk-usage(info1, report: report-format, sort-by: sort-by);
  end if;
end method main;

begin
  main();
end;

