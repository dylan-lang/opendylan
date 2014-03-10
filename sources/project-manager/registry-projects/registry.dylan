Module: registry-projects-internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $abstract-host-prefix = "abstract://dylan/";

define function find-library-locator
    (library-name, registries)
 => (lid-locator :: false-or(<file-locator>),
     registry :: false-or(<registry>));
  let name = as-lowercase(as(<string>, library-name));
  let prefix = $abstract-host-prefix;
  let prefix-size = prefix.size;
  block (return)
    for (registry :: <registry> in registries)
      let locator
        = make(<file-locator>,
               directory: registry.registry-location,
               name:      name);
      if (file-exists?(locator))
        let location
          = with-open-file (stream = locator)
              read-line(stream)
            end;
        let filename
          = if (location.size > prefix-size
                  & copy-sequence(location, end: prefix-size) = prefix)
              let root = as(<directory-locator>, registry.registry-root);
              let file = as(<posix-file-locator>,
                            copy-sequence(location, start: prefix-size));
              // Can't just merge them as that would produce a POSIX locator...
              let file-parent = locator-directory(file);
              let file-parent-path = file-parent & locator-path(file-parent);
              make(<file-locator>,
                   directory: make(<directory-locator>,
                                   server: locator-server(root),
                                   path: concatenate(locator-path(root) | #[],
                                                     file-parent-path | #[])),
                   name: locator-name(file))
            else
              as(<file-locator>, location)
            end;
        return(filename, registry)
      else
        debug-out(#"project-manager",
                  "Failed to find %s [%s under %s]",
                  as(<string>, locator),
                  name,
                  as(<string>, registry.registry-location))
      end
    end;
    values(#f, #f)
  end
end function find-library-locator;
