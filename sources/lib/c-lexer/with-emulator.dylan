Module: cpp-internal
Author: Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro with-emulator
  { with-emulator
      ?emulator:*
    end } 
  => { ?emulator }
end macro with-emulator;

define macro with-dfmc
  { with-dfmc
      ?dfmc:*
    end } 
  => { }
end macro with-dfmc;
