Module:    environment-internal-commands
Synopsis:  The internal-only commands provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Debugging properties

// Personal-root property

define class <personal-root-property> (<environment-property>)
end class <personal-root-property>;

define command-property personal-root => <personal-root-property>
  (summary:       "Personal root directory",
   documentation: "The root directory of the personal area.",
   type:          <directory-locator>)
end command-property personal-root;

define method show-property
    (context :: <environment-context>, property :: <personal-root-property>)
 => ()
  message(context, "Personal root: %s",
          environment-variable("OPEN_DYLAN_USER_ROOT"))
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <personal-root-property>,
     root :: <directory-locator>,
     #key save?)
 => ()
  maybe-set-roots(context, personal-root: root)
end method set-property;


// System-root property

define class <system-root-property> (<environment-property>)
end class <system-root-property>;

define command-property system-root => <system-root-property>
  (summary:       "System root directory",
   documentation: "The root directory of the system area.",
   type:          <directory-locator>)
end command-property system-root;

define method show-property
    (context :: <environment-context>, property :: <system-root-property>)
 => ()
  message(context, "System root: %s",
          environment-variable("OPEN_DYLAN_RELEASE_ROOT"))
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <system-root-property>,
     root :: <directory-locator>,
     #key save?)
 => ()
  maybe-set-roots(context, system-root: root)
end method set-property;


// Registries property

define class <registries-property> (<environment-property>)
end class <registries-property>;

define command-property registries => <registries-property>
  (summary:       "Project manager registries",
   documentation: "The set of project manager registries.")
end command-property registries;

define method show-property
    (context :: <environment-context>, property :: <registries-property>)
 => ()
  let (processor, os) = default-platform-info(*default-project-class*);
  let registries = find-registries(processor, os);
  for (registry in registries)
    message(context, "  %s", registry)
  end
end method show-property;


/// Utilities

define constant $personal-directories
  = #(#("OPEN_DYLAN_USER_ROOT"),
      #("OPEN_DYLAN_USER_BUILD",      "build"),
      #("OPEN_DYLAN_USER_INSTALL"),
      #("OPEN_DYLAN_USER_SOURCES",    "sources"),
      #("OPEN_DYLAN_USER_REGISTRIES", "sources", "registry"));

define constant $system-directories
  = #(#("OPEN_DYLAN_RELEASE_ROOT"),
      #("OPEN_DYLAN_RELEASE_BUILD",      "build"),
      #("OPEN_DYLAN_RELEASE_INSTALL"),
      #("OPEN_DYLAN_RELEASE_SOURCES",    "sources"),
      #("OPEN_DYLAN_RELEASE_REGISTRIES", "sources", "registry"));

define method maybe-set-roots
    (context :: <server-context>,
     #key personal-root :: false-or(<directory-locator>),
          system-root :: false-or(<directory-locator>))
 => ()
  local method set-variable
            (variable :: <string>, directory :: <directory-locator>, 
             subdirectories :: <sequence>)
          let subdirectory = apply(subdirectory-locator, directory, subdirectories);
          environment-variable(variable) := as(<string>, subdirectory)
        end method set-variable;
  if (personal-root)
    for (directory-info :: <list> in $personal-directories)
      let variable       = directory-info.head;
      let subdirectories = directory-info.tail;
      set-variable(variable, personal-root, subdirectories)
    end
  end;
  if (system-root)
    for (directory-info :: <list> in $system-directories)
      let variable       = directory-info.head;
      let subdirectories = directory-info.tail;
      set-variable(variable, system-root, subdirectories)
    end
  end
end method maybe-set-roots;


/// Registry commands

define command-group registry into environment
    (summary: "registry commands",
     documentation: "Registry commands.")
  property personal-root;
  property system-root;
  property registries;
end command-group registry;
