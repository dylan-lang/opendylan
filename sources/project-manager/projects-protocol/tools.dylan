Module:    projects-protocol-internals
Synopsis:  Project tool protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Tools

define class <specification-file-information> (<file-information>)
  constant slot file-input-files :: <simple-object-vector>,
    required-init-keyword: input-files:;
end class <specification-file-information>;

define open generic read-input-files
    (target :: <build-target>, name :: <symbol>, 
     location :: <file-locator>, properties :: <table>)
 => (input-files :: false-or(<simple-object-vector>));

define method read-input-files
    (target :: <build-target>, name :: <symbol>, 
     location :: <file-locator>, properties :: <table>)
 => (files == #f)
  #f
end method read-input-files;

define method read-file-information
    (target :: <build-target>, type == #"spec", file :: <file-locator>)
 => (information :: <specification-file-information>)
  let timestamp = current-date();
  let (name, properties) = read-specification-file(target, file);
  let input-files = read-input-files(target, name, file, properties);
  if (input-files)
    make(<specification-file-information>,
	 location:    file,
	 properties:  properties,
	 timestamp:   timestamp,
	 input-files: input-files)
  else
    project-file-error
      (target.target-project, file,
       "unrecognized specification origin '%s'",
       as-lowercase(as(<string>, name)))
  end
end method read-file-information;

define method read-specification-file
    (target :: <build-target>, file :: <file-locator>)
 => (name :: <symbol>, properties :: <table>)
  let properties :: <table> = read-file-header(file);
  let name = get-symbol-property(file, properties, #"origin"); 
  unless (name)
    project-file-error
      (target.target-project, file,
       "specification missing mandatory :origin keyword")
  end;
  values(name, properties)
end method read-specification-file;


/// Lid file parsing

define inline function get-list-property
    (location :: <file-locator>, properties :: <table>, key :: <symbol>)
 => (value :: false-or(<list>))
  ignore(location);
  element(properties, #"compilation-mode", default: #f);
end function get-list-property;

define function get-single-property
    (location :: <file-locator>, properties :: <table>, key :: <symbol>)
 => (value :: false-or(<string>))
  let list = get-list-property(location, properties, #"compilation-mode");
  if (list)
    if (list.size == 1)
      list[0]
    else
      signal(make(<badly-formed-file-header>, 
		  format-string:
		    "The library description file %= contains multiple "
		    "entries for the '%s' option.",
		  format-arguments: vector(location, key)));
    end
  end
end function get-single-property;

define function get-integer-property
    (location :: <file-locator>, properties :: <table>, key :: <symbol>)
 => (value :: false-or(<integer>))
  let string = get-single-property(location, properties, key);
  string & string-to-integer(string)
end function get-integer-property;

define function get-symbol-property
    (location :: <file-locator>, properties :: <table>, key :: <symbol>)
 => (value :: false-or(<symbol>))
  let string = get-single-property(location, properties, key);
  string & as(<symbol>, string)
end function get-symbol-property;

define function get-symbol-list-property
    (location :: <file-locator>, properties :: <table>, key :: <symbol>)
 => (value :: false-or(<list>))
  let list = get-list-property(location, properties, #"compilation-mode");
  list & map(curry(as, <symbol>), list)
end function get-symbol-list-property;

/*---*** Would seem to be useful...
define function get-locator-property
    (location :: <file-locator>, properties :: <table>, key :: <symbol>)
 => (value :: false-or(<file-locator>))
  let string = get-single-property(location, properties, key);
  string & as(<file-locator>, string)
end function get-locator-property;
*/
