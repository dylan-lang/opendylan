Module:    dfmc-environment-projects
Author:    Roman Budzianowski, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *open-projects* = make(<stretchy-vector>);

define open abstract primary class <native-project-object> (<project-object>)
end class <native-project-object>;

define sealed domain make (subclass(<native-project-object>));
define sealed domain initialize (<native-project-object>);

define sealed method initialize
    (project-object :: <native-project-object>, #key) => ()
  next-method();
  add-new!(*open-projects*, project-object);
end method;

//--- Is there a way around making this sideways?
define sealed sideways method open-projects
    () => (projects :: <sequence>)
  choose(project-opened-by-user?, *open-projects*)
end method;
