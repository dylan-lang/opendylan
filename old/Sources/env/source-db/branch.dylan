Module: source-db
Language: infix-dylan
Author: Roman Budzianowski
Credits: John Dunning
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ref-object> (<db-object>)
	slot ref-count, init-value: 0;
end class;

define method up-reference (object :: <ref-object>)
	object.ref-count := object.ref-count + 1;
end;

define method down-reference (object :: <ref-object>)
	object.ref-count := object.ref-count - 1;
end;

define class <branch> (<ref-object>) 
	slot name, required-init-keyword: name:;
end class;

define method describe (b :: <branch>, #key verbose? = #f)
  format(*standard-output*, "[branch : %=]\n", b.name);
end; 

define method print (b :: <branch>, #key stream = *standard-output*, verbose? = #f)
  format(stream, "[branch : %=]", b.name);
end; 

define class <author> (<db-object>)
	slot name, required-init-keyword: name:;
end class;

define class <branch-search-list> (<list>) end;

define class <branched-container> (<db-object>)
	slot name, required-init-keyword: name:;
// active branch specs
// newest-in-branch containers
//	table
	slot branches, init-function: method () make(<table>) end;
end class;

define method describe (container :: <branched-container>, #key verbose? = #f)
  format(*standard-output*, "Branches:\n");
  for(branch in container.branches)
    describe(branch: verbose?);
  end;
end;

