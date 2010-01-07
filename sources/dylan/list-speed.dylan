Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed copy-down-method choose
  (test :: <function>, sequence :: <pair>) => (result :: <list>);
define sealed copy-down-method choose
  (test :: <function>, sequence :: <empty-list>) => (result :: <empty-list>);

define sealed copy-down-method find-key
  (coll :: <list>, test :: <function>, #key skip, failure) => (key :: <object>);
define sealed copy-down-method replace-elements! 
  (coll :: <list>, predicate :: <function>, new-val-fn :: <function>, #key count) => (coll :: <list>);

define sealed copy-down-method shallow-copy (coll :: <list>) => (copy :: <list>) ;
