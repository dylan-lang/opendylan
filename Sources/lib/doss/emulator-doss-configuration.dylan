Module:    doss-internals
Author:    Eliot Miranda
Synopsis:  Emulator DOSS configuration (for loading)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method doss-allocate-instance (class, #rest args, #key size) => (object)
  if (size)
    apply(make, class, args)
  else
    apply(allocate-instance, class, args)
  end if  
end method doss-allocate-instance;

define method doss-fill-repeated-slot(obj, class, rpt) => ()
 #f
end method doss-fill-repeated-slot;
