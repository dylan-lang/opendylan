Module:    dfmc-shell
Synopsis:  Shared project handling code
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Project handling

define function choose-project-location
    (context, name :: <symbol>)
 => (location :: false-or(<string>))
  user-message(context,
	       "Project %s was not found, please specify its location:\n"
	       "(or leave empty to abort)",
	       as-lowercase(as(<string>, name)));
  ask-user(context,
	   prompt: " project => ",
	   test: method (location :: <string>)
		   if (file-exists?(location))
		     #t
		   else
		     user-message(context,
				  "File %s does not exist, please try again",
				  location)
		   end
		 end)
end function choose-project-location;

define function project-not-found-handler
    (context, name :: <symbol>, next-handler :: <function>)
 => ()
  let filename = choose-project-location(context, name);
  if (filename)
    let location = as(<locator>, filename);
    signal(make(<find-project-location-restart>, location: location));
  end;
  next-handler()
end function project-not-found-handler;

define macro with-project-location-handler
  { with-project-location-handler (?context:name)
      ?body:body
    end }
 => { let handler (<project-not-found>)
        = method (condition :: <project-not-found>, next-handler :: <function>)
	    project-not-found-handler
	      (?context, condition.condition-project-name, next-handler)
	  end;
      ?body }
end macro with-project-location-handler;

define constant $lid-file-type = #".lid";
define constant $hdp-file-type = #".hdp";

//---*** Maybe this should go into the projects library
define function open-project-from-name
    (name :: <string>) => (project :: false-or(<project>))
  let locator = as(<locator>, name);
  let pathname = merge-locators(locator, working-directory());
  let extension = locator-extension(pathname);
  let file-type
    = case
	~extension | empty?(extension) =>
	  #f;
	extension[0] ~== '.' =>
	  as(<symbol>, concatenate(".", extension));
	otherwise =>
	  as(<symbol>, extension);
      end;
  select (file-type)
    $lid-file-type => import-lid-project(pathname);
    $hdp-file-type => open-project(pathname);
    otherwise =>
      block ()
	lookup-named-project(as(<symbol>, name))
      exception (<project-not-found>)
	#f
      end
  end
end function open-project-from-name;

define function ensure-project-open
    (context, name :: <string>)
 => (project :: false-or(<project>))
  with-project-location-handler (context)
    let project = open-project-from-name(name);
    if (project)
      user-message(context, "Opened project %s\n", name);
      project
    else
      user-message(context, "Project %s not found", name)
    end
  end
end function ensure-project-open;

define function link-library-with-options
    (project :: <project>,
     #key linker = unsupplied(),
          dll?, exe?, exports?,
          force?, not-recursive?, combine?)
 => (linked? :: <boolean>)
  let extent
    = case
	not-recursive? => #"not-recursive";
	force?         => #"all";
	otherwise      => #"changes";
      end;
  projects/link-library(project,
			linker: linker,
			target-type:
			  case
			    exports?  => #"exports";
			    dll?      => #"dll";
			    exe?      => #"executable";
			    otherwise => project-target-type(project);
			  end case,
			extent: extent,
			mode:   combine? & #"combine");
end function link-library-with-options;
