Module: user-projects
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <duplicate-file-name-error> (<simple-error>) end;

define generic project-other-sources (project :: <project>)
 => (sources :: <sequence>);

define method project-other-sources (project :: <lid-project>)
 => (sources :: <sequence>)
  let sources = make(<stretchy-vector>);
  local method add-sources
            (new-sources :: <sequence>) => ()
          for (source in new-sources)
            add!(sources, source)
          end
        end method add-sources;
  local method add-project-filenames
            (keyword :: <symbol>) => ()
          let filenames = project-build-property(project, keyword) | #[];
          add-sources(filenames)
        end method add-project-filenames;
  for (key in #[#"c-source-files", #"c-header-files", #"jam-includes", #"c-object-files",
                  #"c-libraries", #"rc-files", #"other-files", #"broken-files"])
    add-project-filenames(key)
  end;
  sources
end;

define method project-add-file (p :: <user-project>, file :: <string>, #key save? = #t)
  let file-locator = as(<file-locator>, file);
  project-add-file(p, file-locator, save?: save?);
end;

define method project-add-file (p :: <system-project>, file, #key save? = #t)
  error("Cannot add file to lid project");
end;

define method project-add-file
    (p :: <user-project>, file-locator :: <file-locator>, #key save? = #t)
  // check now if it exists ?
  //
  let extension = locator-extension(file-locator);
  let file-type = file-type & as(<symbol>, extension);
  project-add-file-of-type(file-type, p, file-locator);
  save? & save-project(p);
end;

// protocol for adding various types of files to a project
//
define method project-add-file-of-type
    (type == $dylan-file-type, project :: <user-project>, file-locator :: <file-locator>)
  if (~empty?(choose(method (source-file)
                       locator-base(source-file) = locator-base(file-locator)
                     end,
                     project.project-source-files)))
    signal(make(<duplicate-file-name-error>,
                format-string: "File named %s is already included in the project",
                format-arguments: list(locator-base(file-locator))))
  end;
  project.project-source-files :=
    concatenate(project.project-source-files, list(file-locator));
end;

define function project-add-list-property
    (p :: <user-project>, property :: <symbol>, value :: <string>,
     #key property-function = project-build-property,
          property-function-setter = project-build-property-setter)
  let properties = property-function(p, property);
  if (properties)
    property-function(p, property) := add-new(properties, value, test: \=)
  else
    property-function(p, property) := list(value)
  end
end;

define method project-add-file-of-type
    (type == $C-source-file-type, p :: <user-project>,
     file-locator :: <file-locator>)
  project-add-list-property(p, #"c-source-files", as(<string>, file-locator))
end;

define method project-add-file-of-type
    (type :: $include-file-types, p :: <user-project>,
     file-locator :: <file-locator>)
  project-add-list-property(p, #"c-header-files", as(<string>, file-locator))
end;

define method project-add-file-of-type
    (type == $C-object-file-type, p :: <user-project>,
     file-locator :: <file-locator>)
  project-add-list-property(p, #"c-object-files", as(<string>, shorten-pathname(file-locator)))
end;

define method project-add-file-of-type
    (type == $C-libraries-file-type, p :: <user-project>,
     file-locator :: <file-locator>)
  project-add-list-property(p, #"c-libraries", as(<string>, shorten-pathname(file-locator)))
end;

define method project-add-file-of-type
    (type == $rc-file-type, p :: <user-project>,
     file-locator :: <file-locator>)
  project-add-list-property(p, #"rc-files", as(<string>, file-locator))
end;

define method project-add-file-of-type
    (type == $jam-file-type, p :: <user-project>,
     file-locator :: <file-locator>)
  project-add-list-property(p, #"jam-includes", as(<string>, file-locator))
end;

define method project-add-file-of-type
    (type :: $project-file-types, project :: <user-project>,
     file-locator :: <file-locator>)
  let project-directory = project.user-disk-project-file.locator-directory;
  let project-locator = merge-locators(file-locator, project-directory);
  add-new!(project.%subproject-files, project-locator, test: \=);

  debug-out(#"project-manager",
            "Adding file %s to project %s",
            as(<string>, file-locator),
            project.project-name);
  block ()
    let subproject =
      open-project-from-file(project-locator,
                             parent: project,
                             always-replace-system?: #t);

    if (subproject)
      note-used-project(project, subproject);
    else
      project-add-list-property(project, #"broken-files", as(<string>,project-locator));
      user-warning("Cannot open project in %s", project-locator);
    end;

  exception (e :: <system-project-not-usable>)
    project-add-list-property(project, #"broken-files", as(<string>, project-locator));
    let bad-project = condition-unusable-project(e);
    user-warning("Read-only project in %s is unusable",
                 bad-project.project-location);
  end

end;

define method project-add-file-of-type
    (type, p :: <user-project>, file-locator :: <file-locator>)
  project-add-list-property(p, #"other-files",
                            project-relative-file(p, file-locator))
end;

//
// removing files
//

define method project-remove-file (p :: <user-project>, file :: <string>)
  project-remove-file(p, as(<file-locator>, file));
end;

define method project-remove-file (p :: <user-project>, file-locator :: <file-locator>)
  debug-out(#"project-manager", "Removing file: %s\n", as(<string>, file-locator));
  let extension = locator-extension(file-locator);
  let file-type =
    if (extension)
      as(<symbol>, extension)
    else
      $dylan-file-type
    end;

  project-remove-file-of-type(file-type, p, file-locator);
  save-project(p); // this is necessary for now

end;

define method project-remove-file-of-type
    (type == $dylan-file-type, p :: <user-project>,
     file-locator :: <file-locator>)
  let base-name = locator-base(file-locator);
  p.project-source-files := remove!(p.project-source-files, base-name,
                                    test: method (f, name)
                                            debug-out(#"project-manager",
                                                      "? %s(%s) = %s",
                                                      as(<string>, f),
                                                      locator-base(f),
                                                      name);
                                            locator-base(f) = name
                                          end,
                                    count: 1);
end;

define function project-remove-list-property
    (p :: <user-project>, property :: <symbol>, value :: <string>)
  let properties = project-build-property(p, property);
  if (properties)
    project-build-property(p, property) := remove(properties, value, test: \=)
  end
end;

define method project-remove-file-of-type
    (type == $C-source-file-type, p :: <user-project>,
     file-locator :: <file-locator>)
  project-remove-list-property(p, #"c-source-files",
                               project-relative-file(p, file-locator))
end;

define method project-remove-file-of-type
    (type :: $include-file-types, p :: <user-project>,
     file-locator :: <file-locator>)
  project-remove-list-property(p, #"c-header-files",
                               project-relative-file(p, file-locator))
end;

define method project-remove-file-of-type
    (type == $C-object-file-type, p :: <user-project>,
     file-locator :: <file-locator>)
  project-remove-list-property(p, #"c-object-files", as(<string>, file-locator))
end;

define method project-remove-file-of-type
    (type == $C-libraries-file-type, p :: <user-project>,
     file-locator :: <file-locator>)
  project-remove-list-property(p, #"c-libraries", as(<string>, file-locator))
end;

define method project-remove-file-of-type
    (type == $rc-file-type, p :: <user-project>,
     file-locator :: <file-locator>)
  project-remove-list-property(p, #"rc-files",
                               project-relative-file(p, file-locator))
end;

define method project-remove-file-of-type
    (type == $jam-file-type, p :: <user-project>,
     file-locator :: <file-locator>)
  project-remove-list-property(p, #"jam-includes",
                               project-relative-file(p, file-locator))
end;

define method project-remove-file-of-type
    (type, p :: <user-project>, file-locator :: <file-locator>)
  project-remove-list-property(p, #"other-files",
                               project-relative-file(p, file-locator));

  debug-out(#"project-manager", "New list: %s", project-build-property(p, #"other-files"));
end;

///
define function %project-close-user-subproject
    (project :: <user-project>, subproject :: <project>)
 => ()
  remove-key!(project.%user-project-used-projects,
              subproject.project-library-name);
  %close-project(subproject)
end;

define method project-remove-file-of-type
    (type :: $project-file-types, project :: <user-project>,
     file-locator :: <file-locator>)
  let project-directory = project.user-disk-project-file.locator-directory;
  let project-location = merge-locators(file-locator, project-directory);

  remove!(project.%subproject-files, project-location, test: \=);

  // check if it's in the broken-files category
  //
  let broken-files = project-build-property(project, #"broken-files") | #[];
  let project-file = as(<string>, project-location);
  let found? = member?(project-file, broken-files, test: \=);
  if (found?)
    project-remove-list-property(project, #"broken-files", project-file);
  else
    let library-name = library-name-from-file(project-location);
    let subproject =
      with-compiler-transaction
        lookup-named-project(library-name, create?: #f);
      end;
    subproject & %project-close-user-subproject(project, subproject);
  end;
end;

define method project-sort-files (p :: <user-project>, test-function)
  p.project-source-files := sort!(p.project-source-files,
                                  test: method (a, b)
                                          test-function(locator-base(a),
                                                        locator-base(b))
                                        end);
end;
