language: infix-dylan
module: dispatch-engine-internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant *call-site-caches-enabled?* = #f;

define constant *profile-all-terminal-engine-nodes?* = #f;

define function %profile-count-low-setter
    (new-low :: <machine-word>, di :: <profiling-call-site-cache-header-engine-node>)
end function;

define function %profile-count-high-setter
    (new-low :: <machine-word>, di :: <profiling-call-site-cache-header-engine-node>)
end function;

define function install-cache-header-engine-node-next
    (old :: <cache-header-engine-node>,
     next :: type-union(<method>, <engine-node>),
     gf :: <generic-function>)
 => ()
end function;

define function handle-profiling-call-site-cache-head
    (ds :: <dispatch-state>, cache, old :: <cache-header-engine-node>)
 => (root-engine);
end function;

define function handle-simple-typechecked-cache-head 
  (ds :: <dispatch-state>, cache, old :: <simple-typechecked-cache-header-engine-node>)
  => ();
end function;

define function compute-argument-precheck-mask (ds :: <dispatch-state>, cache)
 => ();
end function;

define function handle-partial-dispatch-cache-head
    (ds :: <dispatch-state>, cache, old :: <partial-dispatch-cache-header-engine-node>)
 => (root-engine);
end function;

define function handle-simple-call-site-cache-head
    (ds :: <dispatch-state>, cache, old :: <cache-header-engine-node>)
 => (root-engine);
end function;

define function handle-unknown-cache-head
  (ds :: <dispatch-state>, cache, old :: <cache-header-engine-node>)
  => ();
end function;

define function cache-header-punt (ds :: <dispatch-state>, cache, e :: <cache-header-engine-node>)
 => ()
end function;

define function partial-dispatch-megamorphic-punt? ()
  #f
end function;

define constant *partial-dispatch?* = #f;

