Module: make-dylan-app
Synopsis: A tool to create the initial boilerplate for new Dylan libraries.
Copyright: Original Code is Copyright (c) 2012 Dylan Hackers. All rights reserved.
License: See License.txt in this distribution for details.
Warranty: Distributed WITHOUT WARRANTY OF ANY KIND

// The <template> class is used to encapsulate constant format strings
// ("templates") and its arguments.

define class <template> (<object>)
  constant slot constant-string, required-init-keyword: constant-string:;
  constant slot arguments, init-keyword: arguments:, init-value: #();
  constant slot output-path, required-init-keyword: output-path:;
end class <template>;

define method as
    (class == <string>, template :: <template>) => (output :: <string>)
  apply(format-to-string, template.constant-string, template.arguments);
end method as;

define method print-object
    (template :: <template>, stream :: <stream>) => ()
  format(stream, "%s", as(<string>, template));
end method print-object;

define function write-templates
    (#rest templates :: <template>) => ();
  for (template in templates)
    with-open-file (stream = output-path(template), direction: #"output",
                    if-does-not-exist: #"create")
      print-object(template, stream);
    end with-open-file;
  end for;
end function write-templates;

define function make-dylan-app
    (app-name :: <string>) => ()
  let project-dir = create-directory(working-directory(), app-name);

  local method to-target-path (#rest args) => (target)
          merge-locators(as(<file-locator>, apply(concatenate, args)),
                         project-dir);
        end method to-target-path;

  let main :: <template>
    = make(<template>,
           output-path: to-target-path(app-name, ".dylan"),
           constant-string: $main-template-simple,
           arguments: list(app-name));
  let lib :: <template>
    = make(<template>,
           output-path: to-target-path("library.dylan"),
           constant-string: $library-template-simple,
           arguments: list(app-name, app-name, app-name, app-name));
  let lid :: <template>
    = make(<template>,
           output-path: to-target-path(app-name, ".lid"),
           constant-string: $lid-template-simple,
           arguments: list(app-name, "library", app-name));
  write-templates(main, lib, lid);
  write-registry(project-dir, app-name);
end function make-dylan-app;

define function write-registry
    (directory :: <directory-locator>, name :: <string>)
  let registry = create-directory(directory, "registry");
  let generic = create-directory(registry, "generic");
  with-open-file (stream = merge-locators(as(<file-locator>, name), generic),
                  direction: #"output",
                  if-does-not-exist: #"create")
    format(stream, "abstract://dylan/%s.lid\n", name)
  end with-open-file;
end function write-registry;

// While technically any Dylan name is valid, we prefer to restrict the names
// to the style that is in common use since this tool is most likely to be used
// by beginners.
define constant $library-name-regex = compile-regex("^[a-z][a-z0-9-]*$");

define function main
    (app-name :: <string>, arguments :: <vector>) => ()
  if (arguments.size < 1)
    format-err("Usage: make-dylan-app library-name\n");
    exit-application(1);
  else
    let library-name :: <string> = arguments[0];
    if (regex-search($library-name-regex, library-name))
      block ()
        make-dylan-app(library-name);
        exit-application(0);
      exception (condition :: <condition>)
        format-err("error: %=\n", condition);
      end block;
    else
      format-err("Error: %= is not a valid Dylan library name.\n\n"
                   "Libraries are named with one or more words separated by\n"
                   "dashes, for example 'cool-stuff'. Names must match the\n"
                   "regular expression %=.",
                 library-name, regex-pattern($library-name-regex));
      exit-application(1);
    end if;
  end if;
end function main;

main(application-name(), application-arguments());
