Module:    dfmc-environment-database
Synopsis:  DFM compiler type information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Type objects

//---*** cpage: 1997.03.13  Currently, type-expressions returned from
//       functional-parameter-types() can include <class-definition>,
//       though the documentation says it will return <variable> or #t.
//       So, we check the object type and only provide <name-object>
//       for those that are <class-definition> (any <source-form>,
//       in fact), otherwise, g-e-o-p-n() will be called to get the
//       name directly from the <variable>.

//---*** cpage: 1997.12.18 I think these methods are no longer necessary,
//              now that specializers and types are modeled using
//              <environment-object> instead of <type-object>.
/*
define sealed method environment-object-name
    (server :: <dfmc-database>, object :: <type-object>,
     namespace :: <namespace-object>)
 => (name :: false-or(<name-object>))
  instance?(object.compiler-object-proxy, <source-form>) & next-method()
end method environment-object-name;

define sealed method environment-object-home-name
    (server :: <dfmc-database>, object :: <type-object>)
 => (name :: false-or(<name-object>))
  instance?(object.compiler-object-proxy, <source-form>) & next-method()
end method environment-object-home-name;
*/
