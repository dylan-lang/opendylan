Module:    naming-service
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function usage ()
  format-out("Usage: %s [/ior filename]", application-name());
  abort();
end function;

define function process-arguments ()
  let arguments = application-arguments();
  for (i from 0 below size(arguments))
    if (case-insensitive-equal(arguments[i], "/ior") | case-insensitive-equal(arguments[i], "-ior"))
      if (i + 1 < size(arguments))
        i := i + 1;
        *naming-ior-file* := arguments[i];
      else
        usage();
      end if;
    end if;
  end for;
end function;

define function main ()
  block ()
    process-arguments();
    start-naming-service();
  exception (<abort>)
  end block;
end function;

main();

