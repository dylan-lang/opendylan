Module:    map-statistics
Synopsis:  Library for handling Win32 map files
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// constants for various application exit values

define constant $HELP                     = 1;
define constant $INVALID-SORT-OPTION      = 2;
define constant $INVALID-COUNT            = 3;
define constant $INVALID-COMMAND-LINE-ARG = 4;
define constant $MISSING-FILENAME         = 5;
define constant $NO-SUCH-FILE             = 6;


/// Handle the command line arguments

define method application-error
    (return-code :: <integer>, format-string :: <string>, #rest args)
  apply(format-out, format-string, args);
  exit-application(return-code);
end method application-error;

define method help-function (application-name :: <string>) => ()
  format-out
    ("Application: %s\n"
     "\n"
     "  Arguments: filename\n"
     "             [-summary -nosummary]\n"
     "             [-count <integer>]\n"
     "             [-sort [size name]]\n",
     application-name);
  exit-application($HELP)
end method help-function;

define function compute-application-options
    (application-name :: <string>, arguments :: <sequence>)
 => (filename :: <string>,
     summary? :: <boolean>,
     count :: false-or(<integer>),
     sort-function :: <function>,
     group-sort-function :: <function>)
  let arguments = as(<deque>, arguments);
  let summary? = #f;
  let count = #f;
  let sort-function = library-size-greater-than;
  let group-sort-function = group-size-greater-than;
  let filename = #f;

  // Parse through the arguments
  while (~empty?(arguments))
    let keyword = pop(arguments);
    select (keyword by \=)
      "-summary" => 
	summary? := #t;
      "-nosummary" => 
	summary? := #f;
      "-count" =>
	let count-name = pop(arguments);
	count 
	  := block ()
	       string-to-integer(count-name)
	     exception (error :: <condition>)
	       application-error($INVALID-COUNT,
				 "Count '%s' not an integer.\n"
				 "Use -help for available options\n",
				 count-name)
	     end;
      "-sort" =>
	let function-name = pop(arguments);
	let by-size?
	  = select (function-name by \=)
	      "size" => #t;
	      "name" => #f;
	      otherwise =>
		application-error($INVALID-SORT-OPTION,
				  "Sort option '%s' not supported.\n"
				  "Use -help for available options\n",
				  function-name);
	    end;
	if (by-size?)
	  sort-function := library-size-greater-than;
	  group-sort-function := group-size-greater-than;
	else
	  sort-function := library-title-less-than;
	  group-sort-function := group-title-less-than;
	end;
      otherwise =>
	if (filename)
	  application-error($INVALID-COMMAND-LINE-ARG,
			    "Invalid command line keyword '%s'.\n"
			    "Use -help for available options\n",
			    keyword)
	else
	  filename := keyword
	end
    end
  end;

  unless (filename)
    application-error($MISSING-FILENAME,
		      "No filename supplied for map-statistics\n"
		      "Use -help for available options\n")
  end;

  values(filename, summary?, count, sort-function, group-sort-function)
end function compute-application-options;

define function main 
    (application-name :: <string>, arguments :: <sequence>)
 => (status-code :: <integer>)
  if (member?("-help", arguments, test:\=))
    help-function(application-name)
  end;
  let (filename, summary?, count, sort-function, group-sort-function)
    = compute-application-options(application-name, arguments);

  format-out("\n"
	     "Running map-statistics with arguments:\n"
	     "  filename: %s\n"
	     "  summary?: %s\n"
	     "  count:    %s\n"
	     "  sort:     %s\n\n",
	     filename,
	     if (summary?) "yes" else "no" end,
	     count | "no",
	     if (sort-function == library-size-greater-than)
	       "size"
	     else
	       "name"
	     end);

  let results
    = map-library-breakdown(filename, count: count);

  format-out("\nGenerating report...\n");

  if (results)
    print-group-results
      (results,
       sort-function: sort-function,
       group-sort-function: group-sort-function,
       summary?: summary?);
    0
  else
    application-error($NO-SUCH-FILE,
		      "Map file '%s' does not exist.\n",
		      filename)
  end;
end function main;

main(application-name(), application-arguments());
