Module:    environment-tools
Synopsis:  Environment tools library
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some useful constants

define constant $maximum-object-name-length = 2000;

define constant $n/a                        = "n/a";
define constant $type-n/a                   = "<?>";
define constant $no-information-available   = "No information available";
define constant $unknown-name               = "{unknown-name}";
define constant $unknown                    = "unknown";
define constant $project-not-built          = "[project not built]";
define constant $interactive-definition     = "interactive definition";


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


/// Function names

define constant $show-keyword-function-names = #f;

// Create a user-visible string to describe function parameters
define function print-function-parameters
    (server :: <server>, function-object :: <function-object>,
     namespace :: false-or(<namespace-object>))
 => (name :: <string>)
  with-output-to-string (stream)
    let <object>-class = find-environment-object(server, $<object>-id);
    let (required, rest, key, all-keys?, next) // ... values, rest-value)
      = function-parameters(server, function-object);
    format(stream, "(");
    local method do-parameter (parameter :: <parameter>) => ()
	    let keyword 
	      = instance?(parameter, <optional-parameter>)
		  & parameter.parameter-keyword;
	    let type = parameter.parameter-type;
	    if ($show-keyword-function-names)
	      if (keyword)
		format(stream, "%s: ", keyword)
	      end;
	      format(stream, "%s", parameter.parameter-name);
	    else
	      format(stream, "%s", keyword | parameter.parameter-name)
	    end;
	    unless (type == <object>-class)
	      format(stream, " :: %s",
		     environment-object-display-name(server, type, namespace)
		       | $type-n/a)
	    end
	  end method do-parameter;
    local method do-parameters (parameters :: <parameters>) => ()
	    for (parameter :: <parameter> in parameters,
		 separator = "" then ", ")
	      format(stream, separator);
	      do-parameter(parameter)
	    end for;
	  end method do-parameters;
    do-parameters(required);
    let printed-something = size(required) > 0;
    local method print-separator () => ()
	    if (printed-something)
	      format(stream, ", ");
	    else
	      printed-something := #t;
	    end;
	  end method print-separator;
    if (next)
      print-separator();
      format(stream, "#next ");
      do-parameter(next);
    end;
    if (rest)
      print-separator();
      format(stream, "#rest ");
      do-parameter(rest);
    end;
    case
      key & size(key) > 0 =>
	print-separator();
	format(stream, "#key ");
	do-parameters(key);
	if (all-keys?) 
	  format(stream, ", #all-keys")
	end;
      all-keys? =>
	print-separator();
	format(stream, "#key, #all-keys");
      otherwise =>
	#f;
    end;
    format(stream, ")");
  end
end function print-function-parameters;

// Create a user-visible string to describe function values
define function print-function-values
    (server :: <server>, function-object :: <function-object>,
     namespace :: false-or(<namespace-object>))
 => (name :: <string>)
  with-output-to-string (stream)
    let <object>-class = find-environment-object(server, $<object>-id);
    let (required-params, rest-param, key-params, all-keys?, next-param, 
	 required-values, rest-value)
      = function-parameters(server, function-object);
    ignore(required-params, rest-param, key-params, all-keys?, next-param);
    format(stream, "(");
    local method do-value (parameter :: <parameter>) => ()
	    let type = parameter.parameter-type;
	    format(stream, "%s", parameter.parameter-name);
	    unless (type == <object>-class)
	      format(stream, " :: %s",
		     environment-object-display-name(server, type, namespace)
		       | $type-n/a)
	    end
	  end method do-value;
    local method do-values (_values :: <parameters>) => ()
	    for (value in _values,
	      count from size(_values) - 1 by -1)
	      do-value(value);
	      if (count > 0)
		format(stream, ", ")
	      end;
	    end for;
	  end method do-values;
    do-values(required-values);
    if (rest-value)
      if (size(required-values) > 0)
	format(stream, ", ");
      end;
      format(stream, "#rest ");
      do-value(rest-value);
    end;
    format(stream, ")")
  end
end function print-function-values;


/// Source location display

define open generic print-environment-object-location
    (server :: <server>, object :: <environment-object>, 
     namespace :: false-or(<namespace-object>))
 => (location :: <string>);

define method print-environment-object-location
    (project :: <project-object>, object :: <environment-object>,
     namespace :: false-or(<namespace-object>))
 => (location :: <string>)
  ignore(project, object, namespace);
  let source-location = environment-object-source-location(project, object);
  if (source-location)
    let source-record = source-location.source-location-source-record;
    select (source-record by instance?)
      <interactive-source-record> =>
	$interactive-definition;
      <file-source-record> =>
	let location = source-record.source-record-location;
	file-exists?(location) & location.locator-name;
      otherwise =>
	source-record.source-record-name;
    end
  end
    | $n/a
end method print-environment-object-location;

define method print-environment-object-location
    (project :: <project-object>,
     project-object :: <project-object>,
     namespace :: false-or(<namespace-object>))
 => (location :: <string>)
  ignore(project, namespace);
  as(<string>,
     project-object.project-filename
       | project-object.project-debug-filename
       | $n/a)
end method print-environment-object-location;


/// Find named objects

define method find-named-object
    (frame :: <environment-frame>, name :: <string>, #key type)
 => (object :: false-or(<environment-object>))
  let project = frame.frame-current-project;
  when (project)
    let module  = frame.frame-current-module;
    let object
      = if (low-level-debugging?()
	      & project.application-tethered?
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
