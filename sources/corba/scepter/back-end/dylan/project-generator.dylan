Module:    scepter-dylan-back-end
Author:    Keith Dennison, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// INTERCHANGE

define inline function string-capitalize (string :: <string>)
 => (string :: <string>)
  string;
end function;

define inline function write-single-value-header-pair
    (stream :: <stream>, format-string :: <string>,
     key :: <string>, value :: <object>)
  format(stream, format-string, key, value);
end function;

define method write-header-pair
    (stream :: <stream>, format-string :: <string>,
     key :: <string>, value :: <object>)
  when (value)
    write-single-value-header-pair(stream, format-string, key, value);
  end;
end method;

define method write-header-pair
    (stream :: <stream>, format-string :: <string>,
     key :: <string>, values :: <sequence>)
  unless (empty?(values))
    format(stream, format-string, key, values[0]);
    for (index from 1 below size(values))
      format(stream, "\t%s\n", values[index]);
    end;
  end;
end method;

// This method is here to override the one on "values :: <sequence>".
define method write-header-pair
    (stream :: <stream>, format-string :: <string>,
     key :: <string>, value :: <string>)
  write-single-value-header-pair(stream, format-string, key, value);
end method;

define function write-interchange-file-header (stream :: <stream>, keys :: <sequence>)
 => ()
  // We measure the longest key for pretty-printing, and collect the key-value
  // pairs into a vector after stringifying and capitalizing the keys.
  let key-size = 0;
  for (header-pair :: <pair> in keys)
    let string-key
      = concatenate(string-capitalize(as(<string>, head(header-pair))), ":");
    key-size := max(key-size, size(string-key));
    head(header-pair) := string-key;
  end;
  let format-string = format-to-string("%%-%ds %%s\n", key-size);
  for (header-pair :: <pair> in keys)
    write-header-pair(stream, format-string,
		      head(header-pair), tail(header-pair));
  end;
  new-line(stream);
end function;

define macro with-open-interchange-file
  { with-open-interchange-file
	(?stream:variable = ?locator:expression,
	 ?headers:variable = ?headers-val:expression,
	 #rest ?keys:*, #key, #all-keys)
      ?:body
    end }
 => { begin
	with-open-file (?stream = ?locator, ?keys)
	  let ?headers = ?headers-val;
          write-interchange-file-header(?stream, ?headers);
          ?body
        end
      end }
end macro;


// PROPERTY

define class <property> (<object>)
//  constant slot property-name :: <string>, required-init-keyword: name:;
end class;

define constant $project-library = make(<property>); //, name: "Library");
define constant $project-modules = make(<property>); //, name: "Modules");
define constant $project-files = make(<property>); //, name: "Files");
define constant $project-target-type = make(<property>); //, name: "Target-type");
define constant $project-subprojects = make(<property>); //, name: "Subprojects");


// NAMESPACE

define abstract class <namespace-description> (<object>)
  constant slot namespace-name :: false-or(<string>) = #f, init-keyword: name:;
  constant slot namespace-uses :: <sequence> = #[], init-keyword: uses:;
  constant slot namespace-exports :: <sequence> = #[], init-keyword: exports:;
end class;

define class <library-description> (<namespace-description>)
end class;

define class <module-description> (<namespace-description>)
end class;

define method generate-namespace-definition
    (namespace :: <namespace-description>, stream :: <stream>)
 => ()
  generate-namespace-definition-header(namespace, stream);
  generate-namespace-definition-used-clauses(namespace, stream);
  generate-namespace-definition-export-clause(namespace, stream);
  generate-namespace-definition-end(namespace, stream);
end method;

define method generate-namespace-definition-header
    (library :: <library-description>, stream :: <stream>)
 => ()
  format(stream, "define library %s\n", namespace-name(library));
end method;

define method generate-namespace-definition-header
    (module :: <module-description>, stream :: <stream>)
 => ()
  format(stream, "define module %s\n", namespace-name(module));
end method;

define method generate-namespace-definition-used-clauses
    (namespace :: <namespace-description>, stream :: <stream>)
 => ()
  let uses = namespace-uses(namespace);
  if (~empty?(uses))
    for (use in uses)
      format(stream, "  use %s;\n", use);
    end for;
  end if;
end method;

define method generate-namespace-definition-export-clause
    (namespace :: <namespace-description>, stream :: <stream>)
 => ()
  let exports = namespace-exports(namespace);
  if (~empty?(exports))
    format(stream, "  export\n");
    for (i from 0 below size(exports))
      format(stream, "    %s", exports[i]);
      if (i = size(exports) - 1)
        format(stream, ";\n");
      else
        format(stream, ",\n");
      end if;
    end for;
  end if;
end method;

define method generate-namespace-definition-end
    (namespace :: <namespace-description>, stream :: <stream>)
 => ()
  format(stream, "end;\n\n");
end method;


// PROJECT

define class <project-description> (<object>)
  constant slot project-name :: false-or(<string>) = #f, init-keyword: name:;
  slot project-library-file-base-name :: <string> = "library";
  slot project-module-file-base-name :: <string> = "module";
  slot project-properties :: <table>, init-keyword: properties:;
end class;

define method initialize (project :: <project-description>, #key name :: false-or(<string>))
  next-method();
  if (name)
    project.project-library-file-base-name := concatenate(name, "-library");
    project.project-module-file-base-name := concatenate(name, "-module");
  end if;
  project.project-properties := make(<table>);
end method;

define method project-add-property (project :: <project-description>,
                                    property :: <property>, value :: <object>)
 => ()
  project.project-properties[property] := value;
end method;

/* --- Not currently used
define method project-remove-property (project :: <project-description>,
                                       property :: <property>)
 => ()
  remove-key!(project.project-properties, property);
end method;
*/

define method project-property-value (project :: <project-description>,
                                      property :: <property>,
                                      #key default = #f)
 => (value :: <object>)
  project.project-properties[property];
end method;

define method project-files (project :: <project-description>, directory :: <directory-locator>)
 => (locators :: <sequence> /*---***limited(<sequence>, of: <locator>)***---*/ )
  local method expand-filenames (file :: <string>) => (locator :: <locator>)
	  merge-locators(make(<file-locator>, base: file, extension: $dylan-source-suffix),
			 directory)
	end method;
  map(expand-filenames, project-property-value(project, $project-files));
end method;

define method project-HDP-file (project :: <project-description>, directory :: <directory-locator>)
 => (locator :: <locator>)
  merge-locators(make(<file-locator>, base: project-name(project), extension: $user-project-suffix),
		 directory)
end method;

/* --- Not currently used
define method project-LID-file (project :: <project-description>, directory :: <directory-locator>)
 => (locator :: <locator>)
  merge-locators(make(<file-locator>, base: project-name(project), extension: $lid-project-suffix),
		 directory)
end method;
*/


// PROJECT-GENERATOR

define constant $user-project-suffix = "hdp";
//define constant $lid-project-suffix = "lid";
define constant $dylan-source-suffix = "dylan";

define method extract-common-keys (project :: <project-description>)
 => (keys :: <sequence>)
  #[];
end method;

/* --- Not currently used
define method extract-LID-keys (project :: <project-description>)
 => (keys :: <sequence>)
  let library = project-property-value(project, $project-library);
  let files = project-property-value(project, $project-files);
  vector(pair(#"library", namespace-name(library)),
         pair(#"files", files));
end method;
*/

define method extract-HDP-keys (project :: <project-description>)
  let library = project-property-value(project, $project-library);
  let files = project-property-value(project, $project-files);
  let target-type = project-property-value(project, $project-target-type);
  let subprojects = project-property-value(project, $project-subprojects);
  vector(pair("comment", "This file is generated, please don't edit"),
	 pair(#"format-version", 2),
	 pair(#"library", namespace-name(library)),
         pair(#"files", files),
         pair(#"target-type", target-type),
	 pair(#"subprojects", subprojects));
end method;

define method generate-library-file
    (project :: <project-description>, location :: <locator>)
 => ()
  let file = merge-locators(make(<file-locator>, 
				 base: project-library-file-base-name(project),
				 extension: $dylan-source-suffix),
			    location);
  let module = vector(pair(#"module", "dylan-user"));
  let keys = extract-common-keys(project);
  with-open-interchange-file (stream = file,
                              header-keys = concatenate(module, keys),
                              direction: #"output")
    let library = project-property-value(project, $project-library, default: #f);
    if (library)
      generate-namespace-definition(library, stream);
    end if;
  end;
end method;

define method generate-module-file
    (project :: <project-description>, location :: <locator>)
 => ()
  let file = merge-locators(make(<file-locator>,
				 base: project-module-file-base-name(project),
				 extension: $dylan-source-suffix),
			    location);
  let module = vector(pair(#"module", "dylan-user"));
  let keys = extract-common-keys(project);
  with-open-interchange-file (stream = file,
                              header-keys = concatenate(module, keys),
                              direction: #"output")
    let modules = project-property-value(project, $project-modules, default: #f);
    if (modules)
      for (module in modules)
        generate-namespace-definition(module, stream);
      end for;
    end if;
  end;
end method;

/* --- Not currently used
define method generate-LID-file
    (project :: <project-description>, location :: <locator>)
 => ()
  let file = merge-locators(make(<file-locator>,
				 base: project-name(project),
				 extension: $lid-project-suffix),
			    location);
  with-open-interchange-file (stream = file,
                              header-keys = extract-LID-keys(project),
                              direction: #"output")
  end;
end method;
*/

define method generate-HDP-file
    (project :: <project-description>, location :: <locator>)
 => ()
  let file = merge-locators(make(<file-locator>,
				 base: project-name(project),
				 extension: $user-project-suffix),
			    location);
  with-open-interchange-file (stream = file,
                              header-keys = extract-HDP-keys(project),
                              direction: #"output")
  end;
end method;

/* --- Not currently used
define method generate-lid-project
    (project :: <project-description>, directory :: <directory-locator>)
 => ()
  let location = merge-locators(make(<file-locator>, name: "foo"), directory);
  generate-library-file(project, location);
  generate-module-file(project, location);
  generate-LID-file(project, location);
end method;
*/

define method generate-hdp-project
    (project :: <project-description>, directory :: <directory-locator>)
 => ()
  let location = merge-locators(make(<file-locator>, name: "foo"), directory);
  generate-library-file(project, location);
  generate-module-file(project, location);
  generate-HDP-file(project, location);
end method;

