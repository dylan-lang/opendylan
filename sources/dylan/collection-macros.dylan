Module:    internal
Culprit:   Kevin Mitchell 
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define macro with-fip-of
  { with-fip-of ?coll:name with prefix ?pref:name 
      ?:body end }
    => { begin
           let (?pref ## "initial-state", ?pref ## "limit", 
                ?pref ## "next-state", ?pref ## "finished-state?", 
                ?pref ## "current-key", ?pref ## "current-element", 
                ?pref ## "current-element-setter", ?pref ## "copy-state") 
             = ?coll.forward-iteration-protocol;
           ?body
         end }
  { with-fip-of ?coll:name 
      ?:body end }
    => { begin
           let (?=initial-state, ?=limit, ?=next-state, ?=finished-state?, 
                ?=current-key, ?=current-element, ?=current-element-setter, 
                ?=copy-state) =  ?coll.forward-iteration-protocol;
           ?body
         end }
end macro with-fip-of;


// TODO: OBSOLETE?

/*
define macro with-pinned-objects
  { with-pinned-objects(?objects:*) ?:body end }
  => { ?body }
end macro with-pinned-objects;
*/

define macro without-bounds-checks
  { without-bounds-checks ?:body end }
    => { begin
           let ?=element = element-no-bounds-check;
           let ?=element-setter = element-no-bounds-check-setter;
           ?body
         end }
end macro without-bounds-checks;
