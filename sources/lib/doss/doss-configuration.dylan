Module:    doss-internals
Author:    Eliot Miranda
Synopsis:  DOSS configuration (for loading)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// put platform(emulator, native)-specific doss code here;
/// this is the native version

define method doss-allocate-instance (class, #rest args) => (object)
  allocate-instance(class, args)
end method doss-allocate-instance;

define method doss-fill-repeated-slot (obj, class, rpt) => ()
  slot-element(obj, class.instance-storage-size - 1) := rpt
end method doss-fill-repeated-slot;
