Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Application object General property types

define sideways method frame-property-types
    (frame :: <environment-frame>, class :: subclass(<application-object>))
 => (types :: <list>)
  if (low-level-debugging?() & ~subtype?(class, <immediate-application-object>))
    concatenate(next-method(),
		#[#"memory"])
  else
    next-method()
  end
end method frame-property-types;

define method general-property-types
    (class :: subclass(<application-object>))
 => (types :: <list>)
  if (subtype?(class, <immediate-application-object>))
    next-method()
  else
    concatenate(next-method(),
		#[#"address"])
  end
end method general-property-types;


/// Number objects

define method general-property-types
    (class :: subclass(<integer-object>))
 => (types :: <list>)
  concatenate(next-method(),
	      #[#"decimal", #"hex", #"octal", #"binary"])
end method general-property-types;


/// Application object memory page

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, class :: subclass(<application-object>),
     type == #"memory")
 => (label :: <string>, displayer :: <memory-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<memory-displayer>,
	   information-available?-function: curry(application-tethered?, project),
	   transaction-function: curry(perform-application-transaction, project),
	   address-generator: curry(application-object-address, project));
  values("Memory", displayer)
end method make-frame-property-page-displayer;
