module: make-dylan-app
synopsis: make-dylan-app is a tool to create new Dylan projects.
copyright: Original Code is Copyright (c) 1995-2004 Functional Objects, Inc. All rights reserved.
license: See License.txt in this distribution for details.
warranty: Distributed WITHOUT WARRANTY OF ANY KIND

// The <template> class is used to encapuslate constant format strings
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

define function write-templates(#rest templates :: <template>) => ();
  for (template in templates)
    with-open-file (stream = output-path(template), direction: #"output",
                    if-does-not-exist: #"create")
      print-object(template, stream);
    end with-open-file;
  end for;
end function write-templates;

define function make-dylan-app (app-name :: <string>) => ()
  let project-dir
    = create-directory(working-directory(), app-name);

  local method to-target-path (#rest args) => (target)
          merge-locators(as(<file-locator>, apply(concatenate, args)),
                         project-dir);
        end method to-target-path;
  
  let main :: <template>
    = make(<template>, output-path: to-target-path(app-name, ".dylan"),
           constant-string: $main-template-simple, arguments: list(app-name));
  let lib :: <template>
    = make(<template>, output-path: to-target-path("library.dylan"),
           constant-string: $lib-template-simple,
           arguments: list(app-name, app-name));
  let lid :: <template>
    = make(<template>, output-path: to-target-path(app-name, ".lid"), 
           constant-string: $lid-template-simple,
           arguments: list(app-name, app-name, "library.dylan",
                           concatenate(app-name, ".dylan")));
  
  write-templates(main, lib, lid);
end function make-dylan-app;

define function main(app-name :: <string>, arguments :: <vector>) => ()
  if (arguments.size < 1)
    format(*standard-error*, "usage: make-dylan-app [project name]\n");
    exit-application(1);
  else
    let pathname :: <string> = arguments[0];

    if(is-valid-dylan-name?(pathname))
      block()
        make-dylan-app(pathname);
        exit-application(0);
      exception (condition :: <condition>)
        format(*standard-error*, "error: %=\n", condition);
      end block;
    else
      format(*standard-error*,
             "error: Invalid name! Please use a valid Dylan library name.\n");
      exit-application(1);
    end if;

  end if;
end function main;

main(application-name(), application-arguments());
