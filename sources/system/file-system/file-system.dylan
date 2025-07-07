Module:       system-internals
Author:       Gary Palter
Synopsis:     A platform independent file system API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Types

// Needs a better name, I think ...
//
// I'm guessing the author of the above comment was thinking of the potential confusion
// between "file type" and "file extension", which in turn is related to the confusion
// between "pathname" or "locator" and "file". Locators uses the "extension" terminology
// consistently so I think <file-type> is okay.  <file-system-entity-type>? ;) --cgay
define constant <file-type> = one-of(#"file", #"directory", #"link");

define constant <copy/rename-disposition> = one-of(#"signal", #"replace");

define open abstract class <file-system-locator> (<physical-locator>)
end class <file-system-locator>;

define class <file-system-directory-locator> (<file-system-locator>, <directory-locator>)
end class <file-system-directory-locator>;

define class <file-system-file-locator> (<file-system-locator>, <file-locator>)
end class <file-system-file-locator>;

define sealed class <file-system-error> (<simple-error>)
end class <file-system-error>;

define sealed class <file-error> (<file-system-error>)
  constant slot file-error-locator :: <file-system-file-locator>,
    required-init-keyword: locator:;
end class <file-error>;

define sealed class <file-exists-error> (<file-error>)
end class <file-exists-error>;

define sealed class <file-does-not-exist-error> (<file-error>)
end class <file-does-not-exist-error>;

define sealed class <invalid-file-permissions-error> (<file-error>)
end class <invalid-file-permissions-error>;


/// Locators

define constant <pathname> = type-union(<string>, <file-system-locator>);

define sealed method as
    (class == <file-system-locator>, string :: <string>)
 => (locator :: <file-system-locator>)
  as(<native-file-system-locator>, string)
end method as;

define sealed method as
    (class == <file-system-directory-locator>, string :: <string>)
 => (locator :: <file-system-directory-locator>)
  as(<native-directory-locator>, string)
end method as;

define sealed method as
    (class == <file-system-file-locator>, string :: <string>)
 => (locator :: <file-system-file-locator>)
  as(<native-file-locator>, string)
end method as;


/// Condition reporting

define method condition-to-string
    (error :: <file-exists-error>) => (string :: <string>)
  format-to-string("File %s exists", file-error-locator(error))
end method condition-to-string;

define method condition-to-string
    (error :: <file-does-not-exist-error>) => (string :: <string>)
  format-to-string("File %s does not exist", file-error-locator(error))
end method condition-to-string;

define method condition-to-string
    (error :: <invalid-file-permissions-error>) => (string :: <string>)
  format-to-string("Invalid file permissions for file %s",
                   file-error-locator(error))
end method condition-to-string;


/// And now, the functions ...

// Expand ~ and ~user in the given pathname.
//
// TODO(cgay): Seems like this should have "user" in its name. The fact that it doesn't
// could explain why win32-file-system.dylan was apparently using it as a way to convert
// to an absolute pathname.
//   Racket: expand-user-path
//   Python: os.path.expanduser
define generic expand-pathname (path :: <pathname>) => (expanded-path :: <pathname>);

define method expand-pathname (path :: <file-system-locator>) => (expanded-path :: <pathname>)
  %expand-pathname(path)
end method expand-pathname;

define method expand-pathname (path :: <string>) => (expanded-path :: <pathname>)
  expand-pathname(as(<file-system-locator>, path))
end method expand-pathname;


/// Given a pathname, returns the shortest equivalent form (e.g., a DOS pathname on Windows)
define generic shorten-pathname (path :: <pathname>) => (shortened-path :: <pathname>);

define method shorten-pathname (path :: <file-system-locator>) => (shortened-path :: <pathname>)
  %shorten-pathname(path)
end method shorten-pathname;

define method shorten-pathname (path :: <string>) => (shortened-path :: <pathname>)
  shorten-pathname(as(<file-system-locator>, path))
end method shorten-pathname;


///
define generic file-exists? (file :: <pathname>, #key follow-links?)
 => (exists? :: <boolean>);

define method file-exists?
    (file :: <file-system-locator>, #key follow-links? = #t)
 => (exists? :: <boolean>)
  %file-exists?(file, follow-links?)
end method;

define method file-exists?
    (file :: <string>, #key follow-links? = #t)
 => (exists? :: <boolean>)
  file-exists?(as(<file-system-locator>, file), follow-links?: follow-links?)
end method;


///
define generic file-type (file :: <pathname>) => (file-type :: <file-type>);

define method file-type (file :: <file-system-locator>) => (file-type :: <file-type>)
  %file-type(file)
end method file-type;

define method file-type (file :: <string>) => (file-type :: <file-type>)
  file-type(as(<file-system-locator>, file))
end method file-type;


///
define generic link-target
    (link :: <pathname>, #key follow-links?)
 => (target :: false-or(<file-system-locator>));

define method link-target
    (link :: <file-system-locator>, #key follow-links? :: <boolean> = #t)
 => (target :: false-or(<file-system-locator>))
  %link-target(link, follow-links?)
end method link-target;

define method link-target
    (link :: <string>, #key follow-links? :: <boolean> = #t)
 => (target :: false-or(<file-system-locator>))
  link-target(as(<file-system-locator>, link), follow-links?: follow-links?)
end method link-target;


///
define generic delete-file (file :: <pathname>) => ();

define method delete-file (file :: <file-system-locator>) => ()
  %delete-file(file)
end method delete-file;

define method delete-file (file :: <string>) => ()
  delete-file(as(<file-system-locator>, file))
end method delete-file;


///
define generic copy-file
    (source :: <pathname>, destination :: <pathname>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ();

define method copy-file
    (source :: <file-system-locator>, destination :: <file-system-locator>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  %copy-file(source, destination, if-exists: if-exists)
end method copy-file;

define method copy-file
    (source :: <file-system-locator>, destination :: <string>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  copy-file(source, as(<file-system-locator>, destination), if-exists: if-exists)
end method copy-file;

define method copy-file
    (source :: <string>, destination :: <file-system-locator>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  copy-file(as(<file-system-locator>, source), destination, if-exists: if-exists)
end method copy-file;

define method copy-file
    (source :: <string>, destination :: <string>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  copy-file(as(<file-system-locator>, source), as(<file-system-locator>, destination),
            if-exists: if-exists)
end method copy-file;


///
define generic rename-file
    (source :: <pathname>, destination :: <pathname>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ();

define method rename-file
    (source :: <file-system-locator>, destination :: <file-system-locator>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  %rename-file(source, destination, if-exists: if-exists)
end method rename-file;

define method rename-file
    (source :: <file-system-locator>, destination :: <string>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  rename-file(source, as(<file-system-locator>, destination),
              if-exists: if-exists)
end method rename-file;

define method rename-file
    (source :: <string>, destination :: <file-system-locator>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  rename-file(as(<file-system-locator>, source), destination,
              if-exists: if-exists)
end method rename-file;

define method rename-file
    (source :: <string>, destination :: <string>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  rename-file(as(<file-system-locator>, source), as(<file-system-locator>, destination),
              if-exists: if-exists)
end method rename-file;


///
define generic file-properties
    (file :: <pathname>) => (properties :: <explicit-key-collection>);

define method file-properties
    (file :: <file-system-locator>) => (properties :: <explicit-key-collection>)
  let properties = %file-properties(file);
  properties
end method file-properties;

define method file-properties
    (file :: <string>) => (properties :: <explicit-key-collection>)
  file-properties(as(<file-system-locator>, file))
end method file-properties;


/// "Standard" properties are:
///    author, size, creation-date, access-date, modification-date, readable?, executable?
///    Other properties may be defined by the platform.
define generic file-property (file :: <pathname>, key :: <symbol>) => (value);

define method file-property (file :: <file-system-locator>, key :: <symbol>) => (value)
  %file-property(file, key)
end method file-property;

define method file-property (file :: <string>, key :: <symbol>) => (value)
  file-property(as(<file-system-locator>, file), key)
end method file-property;

define generic %file-property (file :: <file-system-locator>, key :: <symbol>) => (value);

define method %file-property (file :: <file-system-locator>, key :: <symbol>) => (value)
  error(make(<file-system-error>,
             format-string: "Native file system does not implement the %s property",
             format-arguments: list(key)))
end method %file-property;


/// Not all properties are settable:
///    See the platform's implementation for details
define generic file-property-setter
    (new-value, file :: <pathname>, key :: <symbol>) => (new-value);

define method file-property-setter
    (new-value, file :: <file-system-locator>, key :: <symbol>) => (new-value)
  %file-property-setter(new-value, file, key)
end method file-property-setter;

define method file-property-setter
    (new-value, file :: <string>, key :: <symbol>) => (new-value)
  file-property-setter(new-value, as(<file-system-locator>, file), key)
end method file-property-setter;

define generic %file-property-setter
    (new-value, file :: <pathname>, key :: <symbol>) => (new-value);

define method %file-property-setter
    (new-value, file :: <file-system-locator>, key :: <symbol>) => (new-value)
  error(make(<file-system-error>,
             format-string: "Native file system cannot set the %s property",
             format-arguments: list(key)))
end method %file-property-setter;


///
define generic do-directory (f :: <function>, directory :: <pathname>) => ();

define method do-directory (f :: <function>, directory :: <file-system-directory-locator>) => ()
  %do-directory(f, directory)
end method do-directory;

define method do-directory (f :: <function>, directory :: <file-system-file-locator>) => ()
  do-directory(f, locator-directory(directory))
end method do-directory;

define method do-directory (f :: <function>, directory :: <string>) => ()
  do-directory(f, as(<file-system-locator>, directory))
end method do-directory;


define generic directory-contents
    (directory :: <pathname>, #key resolve-links?)
 => (locators :: <sequence>);

define method directory-contents
    (directory :: <string>, #key resolve-links? :: <boolean>)
 => (locators :: <sequence>)
  directory-contents(as(<file-system-directory-locator>, directory),
                     resolve-links?: resolve-links?)
end;

// Return a sequence of locators describing the files in `directory`.  If
// `resolve-links?` is false then symbolic links are returned as instances of
// <file-locator>.  If true, then symbolic links are resolved and the correct class of
// locator is guaranteed.  Note that if a symlink points to another file in `directory`
// then the resulting sequence may contain duplicates (in the sense of naming the same
// file system entity, not Dylan object equality) when `resolve-links?` is true. (This
// could be solved by resolving all the locators and checking for duplicates.)
define method directory-contents
    (directory :: <file-system-directory-locator>, #key resolve-links? :: <boolean>)
 => (locators :: <sequence>)
  collecting ()
    local method add-file (directory, name, type)
            if (name ~= "." & name ~= "..")
              select (type)
                #"directory" => collect(subdirectory-locator(directory, name));
                #"file" => collect(file-locator(directory, name));
                #"link" =>
                  let locator = file-locator(directory, name);
                  if (resolve-links?)
                    locator := resolve-locator(locator);
                  end;
                  collect(locator);
              end;
            end;
          end method;
    do-directory(add-file, directory);
  end collecting
end method directory-contents;

// Check the file system to resolve and expand links, and normalize the path.
// Returns an absolute locator, using the current process's working directory
// to resolve relative locators, or signals <file-system-error>. Note that lack
// of an error does not mean that the resolved locator names an existing file,
// but does mean the containing directory exists. In other words, this function
// inherits POSIX `realpath` semantics.
define open generic resolve-file
    (path :: <pathname>) => (resolved :: <file-system-locator>);

define method resolve-file
    (path :: <string>) => (resolved :: <file-system-locator>)
  let resolved = %resolve-file(path);
  let class = if (file-type(resolved) == #"directory")
                <file-system-directory-locator>
              else
                <file-system-file-locator>
              end;
  as(class, resolved)
end method;

define method resolve-file
    (path :: <file-system-locator>) => (resolved :: <file-system-locator>)
  resolve-file(as(<string>, path))
end method;


///
// TODO(cgay): We could compatibly change this to create-directory(dir, #rest names).
// Then if you've already cobbled together a directory locator you can just pass that.
// If you pass multiple names, it's like create-directory(subdirectory-locator(dir,
// names...)).  create-directory(dir) always was the intuitive API to me anyway.
define generic create-directory (parent :: <pathname>, name :: <string>)
 => (directory :: <pathname>);

define method create-directory (parent :: <file-system-directory-locator>, name :: <string>)
 => (directory :: <pathname>)
  let directory = subdirectory-locator(parent, name);
  %create-directory(directory)
end method create-directory;

define method create-directory (parent :: <file-system-file-locator>, name :: <string>)
 => (directory :: <pathname>)
  create-directory(locator-directory(parent), name)
end method create-directory;

define method create-directory (parent :: <string>, name :: <string>)
 => (directory :: <pathname>)
  create-directory(as(<file-system-directory-locator>, parent), name)
end method create-directory;


define generic delete-directory
    (directory :: <pathname>, #key recursive?) => ();

define method delete-directory
    (directory :: <file-system-directory-locator>, #key recursive? :: <boolean>)
 => ()
  if (recursive?)
    for (file in directory-contents(directory, resolve-links?: #f))
      if (instance?(file, <directory-locator>))
        delete-directory(file, recursive?: #t);
      else
        delete-file(file);
      end;
    end;
  end;
  %delete-directory(directory);
end method delete-directory;

define method delete-directory
    (directory :: <file-system-file-locator>,  #key recursive? :: <boolean>)
 => ()
  delete-directory(locator-directory(directory), recursive?: recursive?);
end method delete-directory;

define method delete-directory
    (directory :: <string>, #key recursive? :: <boolean>)
 => ()
  delete-directory(as(<directory-locator>, directory), recursive?: recursive?)
end method delete-directory;


///
define generic ensure-directories-exist (file :: <pathname>) => (created? :: <boolean>);

define method ensure-directories-exist (file :: <file-system-directory-locator>)
 => (created? :: <boolean>)
  local method doit (directory :: false-or(<file-system-directory-locator>)) => (created? :: <boolean>)
          if (false?(directory))
            #f                          // Presume that the root exists...
          elseif (file-exists?(directory))
            #f
          else
            let parent = locator-directory(directory);
            doit(parent);
            %create-directory(directory);
            #t
          end
        end method doit;
  doit(file)
end method ensure-directories-exist;

define method ensure-directories-exist (file :: <file-system-file-locator>) => (created? :: <boolean>)
  if (locator-directory(file))
    ensure-directories-exist(locator-directory(file))
  end if
end method ensure-directories-exist;

define method ensure-directories-exist (file :: <string>) => (created? :: <boolean>)
  ensure-directories-exist(as(<file-system-locator>, file))
end method ensure-directories-exist;


///
define generic directory-empty? (directory :: <pathname>) => (empty? :: <boolean>);

define method directory-empty? (directory :: <file-system-directory-locator>) => (empty? :: <boolean>)
  %directory-empty?(directory)
end method directory-empty?;

define method directory-empty? (directory :: <file-system-file-locator>) => (empty? :: <boolean>)
  directory-empty?(locator-directory(directory))
end method directory-empty?;

define method directory-empty? (directory :: <string>) => (empty? :: <boolean>)
  directory-empty?(as(<file-system-locator>, directory))
end method directory-empty?;


///
define function home-directory () => (home-directory :: false-or(<pathname>))
  %home-directory()
end function home-directory;


///
define function working-directory () => (working-directory :: false-or(<pathname>))
  %working-directory()
end function working-directory;


///
define generic working-directory-setter (new-working-directory :: <pathname>)
 => (new-working-directory :: <pathname>);

define method working-directory-setter (new-working-directory :: <file-system-directory-locator>)
 => (new-working-directory :: <pathname>)
  %working-directory-setter(new-working-directory)
end method working-directory-setter;

define method working-directory-setter (new-working-directory :: <file-system-file-locator>)
 => (new-working-directory :: <pathname>)
  working-directory-setter(locator-directory(new-working-directory))
end method working-directory-setter;

define method working-directory-setter (new-working-directory :: <string>)
 => (new-working-directory :: <pathname>)
  working-directory-setter(as(<file-system-locator>, new-working-directory))
end method working-directory-setter;


///
define function temp-directory () => (temp-directory :: <pathname>)
  %temp-directory()
end function temp-directory;


///
define function root-directories () => (roots :: <sequence>)
  %root-directories()
end function root-directories;


/// Finally, two functions defined as part of Common Dylan's locators-protocol module

///
define sideways method supports-list-locator?
    (directory :: <file-system-directory-locator>) => (listable? :: <boolean>)
  ~directory.locator-relative?
end method supports-list-locator?;

///
define sideways method list-locator
    (locator :: <file-system-directory-locator>) => (locators :: <sequence>)
  directory-contents(locator)
end method list-locator;

///

define function create-symbolic-link
    (target :: <pathname>, link :: <pathname>) => ()
  %create-symbolic-link(as(<file-system-locator>, target),
                        as(<file-system-locator>, link))
end function create-symbolic-link;

define function create-hard-link
    (target :: <pathname>, link :: <pathname>) => ()
  %create-hard-link(as(<file-system-locator>, target),
                    as(<file-system-locator>, link))
end function create-hard-link;
