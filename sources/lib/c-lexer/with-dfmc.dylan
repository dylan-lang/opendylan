Module: cpp-internal
Author: Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro with-emulator
  { with-emulator
      ?emulator:*
    end }
  => { }
end macro with-emulator;

define macro with-dfmc
  { with-dfmc
      ?dfmc:*
    end }
  => { ?dfmc }
end macro with-dfmc;

/*
 * Below is my original version.
 * Ironically enough, the emulator has problems with the below.
 * I'm not sure if using function macros at the top-level is legal, but it
 * seems to work.
 *

define macro with-emulator
  { with-emulator
      ?emulator:*
    else
      ?dfmc:*
    end
  } => { ?dfmc }
  { with-emulator
      ?emulator:*
    end
  } => { }
end macro with-emulator;

define macro with-dfmc
  { with-dfmc
      ?dfmc:*
    else
      ?emulator:*
    end
  } => { ?dfmc }
  { with-dfmc
      ?dfmc:*
    end
  } => { ?dfmc }
end macro with-dfmc;
*/
