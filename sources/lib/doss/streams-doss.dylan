Module:   streams-doss
Language: infix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Arrange that read-end & write-end are dumped (and hence on loading, bound) before
// stream-position.

define method doss-dumpable-slots(class  :: subtype(<readable-positionable-stream>),
                                  policy :: <basic-doss-policy>)
              => (slots :: <sequence>);
  let slots       = next-method();
  let read-end-sd = any?(method (sd) sd.slot-getter == read-end & sd end,
                         slots);

  read-end-sd & concatenate(vector(read-end-sd),remove(slots,read-end-sd))
  | slots
end method;

define method doss-dumpable-slots(class  :: subtype(<writable-positionable-stream>),
                                  policy :: <basic-doss-policy>)
              => (slots :: <sequence>);
  let slots        = next-method();
  let write-end-sd = any?(method (sd) sd.slot-getter == write-end & sd end,
                          slots);

  write-end-sd & concatenate(vector(write-end-sd),remove(slots,write-end-sd))
  | slots
end method;


define method doss-slot-value (slot-getter == file-descriptor,
                               stream :: <external-file-accessor>,
                               dd :: <doss-dumper>) => (closed :: singleton(#f));
  #f
end method;

