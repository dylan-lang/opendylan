module: remake-dylan-app

define function make-dylan-app (app-name :: <string>) => ()
  let project-dir
    = create-directory(working-directory(), app-name);

  local method to-target-path (#rest args) => (target)
	  merge-locators(as(<file-locator>, apply(concatenate, args)),
			 project-dir);
	end method to-target-path;


  let main :: <writable-template>
    = make-writable-template(to-target-path(app-name, ".dylan"),
			     $main-template-simple, app-name);
  let lib :: <writable-template>
    = make-writable-template(to-target-path("library.dylan"),
			     $lib-template-simple, app-name, app-name);
  let lid :: <writable-template>
    = make-writable-template(to-target-path(app-name, ".lid"),
			     $lid-template-simple, app-name, app-name,
			     "library.dylan", concatenate(app-name, ".dylan"));

  write-templates(main, lib, lid);
end function make-dylan-app;
