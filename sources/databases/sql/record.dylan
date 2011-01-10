Module: result-set-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: !record.dylan(D-kan.1) $


define open abstract class <record> (<database-collection>)
  constant slot indicator-policy :: <indicator-policy> = $null-value,
    init-keyword: indicator-policy:;
end class;


define method type-for-copy(object :: <record>)
  => (type :: <class>)
  <simple-object-vector>
end method;


define open abstract class <coercion-record> (<record>)
  constant slot record-coercion-policy :: <coercion-policy> = $default-coercion,
    init-keyword: record-coercion-policy:;
end class;


define generic is-null?(record :: <record>, key :: <integer>)
 => (null-state :: <boolean>);


define constant $no-indicator = #"no-indicator";

// Primarly for documentation.
define constant <indicator-policy> 
  = type-union(singleton($no-indicator), <sequence>, <object>);
