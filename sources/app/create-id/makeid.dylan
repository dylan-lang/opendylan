Module:    create-id
Synopsis:  Program to generate GUID numbers.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define function make-id () => ( id :: <REFGUID> );
  let id-ptr :: <REFGUID> = make(<REFGUID>);
  let status = CoCreateGuid(id-ptr);
  check-ole-status(status, "CoCreateGuid", $null-interface);
  id-ptr
end make-id;

define function make-id-string () => ( id :: <byte-string> );
  let id = make-id();
  let result = as(<string>, id);
  destroy(id);
  result
end make-id-string;

define function show-ids ( n :: <integer> ) => ();
  for ( i from 0 below n )
    let id = make-id-string();
    format-out(" \"%s\"\n", id);
  end for;
end show-ids;

define function main () => ();
  let args = application-arguments();
  let n :: <integer> = 1;
  if ( size(args) > 0 )
    let arg = args[0];
    let start = if ( arg[0] <= '/' ) 1 else 0 end if;
    if ( as-uppercase(arg[start]) = 'H' ) // help
      format-out("Usage:  %s [<number>]\n"
		   "Writes to standard output the indicated number "
		   "of newly generated GUIDs.\n",
		 application-name());
      n := 0;
    else
      n := string-to-integer(arg, start: start, default: 1);
    end if;
  end if;
  show-ids(n);
end main;

main();

