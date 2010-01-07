module: dfmc-common
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DEFAULTS

define compiler-open generic add-user! (o, user);
define compiler-open generic remove-user! (o, user);

define method add-user! (o, user)
end method;

define method remove-user! (o, user)
end method;

define method users (o) => (res :: <empty-list>)
  #()
end method;

/// REFERENCED-OBJECT

define abstract compiler-open dood-class <referenced-object> (<object>)
  /* JB weak */ slot users :: <list> = #(), 
    // reinit-expression: #(),
    init-keyword: users:;
end;

define method used? (o :: <referenced-object>) => (res :: <boolean>)
  ~empty?(o.users)
end method;

define method used-once? (o :: <referenced-object>)
  o.users.size = 1
end method;

define method references (o :: <referenced-object>) => (res :: <list>)
  o.users
end method;

define method add-user! (o :: <referenced-object>, user)
  o.users := add!(o.users, user);
end method;

define method remove-user! (o :: <referenced-object>, user)
  o.users := remove!(o.users, user, count: 1);
end method;
