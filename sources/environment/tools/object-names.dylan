Module:    environment-tools
Synopsis:  Environment tools library
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some useful constants

define constant $maximum-object-name-length = 2000;


/// General printing functions

define open generic frame-qualify-names?
    (frame :: <frame>) => (qualify? :: <boolean>);

define method frame-qualify-names?
    (frame :: <frame>) => (qualify? :: <boolean>)
  environment-qualify-names?()
end method frame-qualify-names?;

//---*** andrewa: I'd like to remove this unspecific method,
//---*** but it is still used...
define method frame-default-object-name
    (frame :: <frame>, object :: <object>,
     #key default :: <string> = $unknown-name)
 => (name :: <string>)
  format-to-string("%s", object)
end method frame-default-object-name;

define method frame-default-object-name
    (frame :: <frame>, object :: <string>,
     #key default :: <string> = $unknown-name)
 => (name :: <string>)
  object
end method frame-default-object-name;

define method frame-default-object-name
    (frame :: <frame>, locator :: <physical-locator>,
     #key default :: <string> = $unknown-name)
 => (name :: <string>)
  as(<string>, locator)
end method frame-default-object-name;

define method frame-default-object-name
    (frame :: <frame>, object :: <number>,
     #key default :: <string> = $unknown-name)
 => (name :: <string>)
  number-to-string(object)
end method frame-default-object-name;

define method frame-default-object-name
    (frame :: <frame>, object :: <percentage>,
     #key default :: <string> = $unknown-name)
 => (name :: <string>)
  percentage-label(object, decimal-points: 0)
end method frame-default-object-name;

define method frame-default-object-name
    (frame :: <frame>, object :: <boolean>,
     #key default :: <string> = $unknown-name)
 => (name :: <string>)
  select (object)
    #f => "";
    #t => "Yes";
  end
end method frame-default-object-name;

define method frame-default-object-name
    (frame :: <environment-frame>, object :: <environment-object>,
     #key default :: <string> = $unknown-name)
 => (name :: <string>)
  let project = frame.ensure-frame-project;
  let module = frame-current-module(frame);
  let qualify-names? = frame-qualify-names?(frame);
  let name
    = environment-object-display-name
        (project, object, module, qualify-names?: qualify-names?);
  name | default
end method frame-default-object-name;

define method frame-object-unique-name
    (frame :: <environment-frame>, object :: <environment-object>,
     #key default :: <string> = $unknown-name)
 => (name :: <string>)
  let project = frame.ensure-frame-project;
  let module = frame-current-module(frame);
  let qualify-names? = frame-qualify-names?(frame);
  let name
    = environment-object-unique-name
        (project, object, module, qualify-names?: qualify-names?);
  name | default
end method frame-object-unique-name;

define method frame-print-environment-object
    (frame :: <environment-frame>, object :: <environment-object>,
     #key default :: <string> = $unknown-name)
 => (name :: <string>)
  let project = frame.ensure-frame-project;
  let module = frame-current-module(frame);
  let qualify-names? = frame-qualify-names?(frame);
  let name
    = print-environment-object-to-string
        (project, object, namespace: module, qualify-names?: qualify-names?);
  name | default
end method frame-print-environment-object;


/// Find named objects

define method find-named-object
    (frame :: <environment-frame>, name :: <string>, #key type)
 => (object :: false-or(<environment-object>))
  let project = frame.frame-current-project;
  when (project)
    let module  = frame.frame-current-module;
    let object
      = if (project.application-tethered?
	      & name.size > 2
	      & (name[0] == '#' | name[0] == '0')
	      & name[1] == 'x')
	  let address = string-to-address(project, copy-sequence(name, start: 2));
	  unless (address == $invalid-address-object)
	    let object = address-application-object(project, address);
	    unless (instance?(object, <address-object>))
	      object
	    end
	  end
	else
	  find-environment-object(project, name, module: module)
	end;
    if (~type | instance?(object, type))
      object
    end
  end
end method find-named-object;
