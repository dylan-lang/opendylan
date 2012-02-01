module: make-dylan-app
synopsis: make-dylan-app is a tool to create new Dylan projects.
author: Rosario Raulin
copyright: (C) 2012 Rosario Raulin, see License.txt file

define argument-parser <make-dylan-app-argparser> ()
  synopsis print-synopsis,
    usage: "make-dylan-app [options] [project name]",
    description: "make-dylan-app is a tool to create new Dylan projects.";
  
  regular-arguments file-paths;
end argument-parser <make-dylan-app-argparser>;

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

define function make-dylan-app (app-name :: <string>, options) => ()
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

define constant $invalid-pathname-chars = #('\\', '/', ':');

define function valid-pathname? (pathname :: <string>) => (valid? :: <boolean>)
  local method invalid-char? (x :: <character>) => (invalid? :: <boolean>)
          any?(curry(\=, x), $invalid-pathname-chars)
        end method invalid-char?;
  
  ~ any?(invalid-char?, pathname)
end function valid-pathname?;

define function main(app-name :: <string>, arguments :: <vector>) => ()
  let options = make(<make-dylan-app-argparser>);

  if (arguments.size < 1)
    print-synopsis(options, *standard-error*);
    exit-application(1);
  else
    parse-arguments(options, arguments);
    let pathname :: <string> = options.file-paths[0];

    if(valid-pathname?(pathname))
      block()
          make-dylan-app(pathname, options);
          exit-application(0);
      exception (condition :: <condition>)
        format(*standard-error*, "error: %=\n", condition);
      end block;
    else
      format(*standard-error*, "error: malicious project name!\n");
      exit-application(1);
    end if;

  end if;
end function main;

main(application-name(), application-arguments());
