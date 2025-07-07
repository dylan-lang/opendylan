Module:       system-internals
Synopsis:     Abstract modeling of locations
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open generic locator-server
    (locator :: <locator>) => (server :: false-or(<server-locator>));
define open generic locator-host
    (locator :: <locator>) => (host :: false-or(<string>));
define open generic locator-volume
    (locator :: <locator>) => (volume :: false-or(<string>));
define open generic locator-directory
    (locator :: <locator>) => (directory :: false-or(<directory-locator>));
define open generic locator-relative?
    (locator :: <locator>) => (relative? :: <boolean>);

// For file locator /a/b/c this returns #["a", "b"].
// For directory locator /a/b/c/ this returns #["a", "b", "c"].
define open generic locator-path
    (locator :: <locator>) => (path :: <sequence>);

// For file locator /a/b/c this returns "c".
// For directory locator /a/b/c/ this returns "c".
define open generic locator-name
  (locator :: <locator>) => (name :: false-or(<string>));

// The locator name without the extension.  e.g., for locator /a/b/c.d
// this will return "c".
define open generic locator-base
    (locator :: <locator>) => (base :: false-or(<string>));

define open generic locator-extension
    (locator :: <locator>) => (extension :: false-or(<string>));

/// Locator classes

define open abstract class <directory-locator> (<physical-locator>)
  sealed constant slot locator-relative? :: <boolean> = #f,
    init-keyword: relative?:;
  sealed constant slot locator-path :: <simple-object-vector>,
    required-init-keyword: path:;
end class;

define open abstract class <file-locator> (<physical-locator>)
end class <file-locator>;

define sealed method as
    (class == <directory-locator>, string :: <string>)
 => (locator :: <directory-locator>)
  as(<native-directory-locator>, string)
end method as;

define sealed method make
    (class == <directory-locator>,
     #key server :: false-or(<server-locator>) = #f,
          path :: <sequence> = #[],
          relative? :: <boolean> = #f,
          name :: false-or(<string>) = #f)
 => (locator :: <directory-locator>)
  make(<native-directory-locator>,
       server:    server,
       path:      path,
       relative?: relative?,
       name:      name)
end method make;

define sealed method as
    (class == <file-locator>, string :: <string>)
 => (locator :: <file-locator>)
  as(<native-file-locator>, string)
end method as;

define sealed method make
    (class == <file-locator>,
     #key directory :: false-or(<directory-locator>) = #f,
          base :: false-or(<string>) = #f,
          extension :: false-or(<string>) = #f,
          name :: false-or(<string>) = #f)
 => (locator :: <file-locator>)
  make(<native-file-locator>,
       directory: directory,
       base:      base,
       extension: extension,
       name:      name)
end method make;


/// Locator coercion

define open generic locator-as-string
    (class :: subclass(<string>), locator :: <locator>)
 => (string :: <string>);

define open generic string-as-locator
    (class :: subclass(<locator>), string :: <string>)
 => (locator :: <locator>);

define sealed sideways method as
    (class :: subclass(<string>), locator :: <locator>)
 => (string :: <string>)
  locator-as-string(class, locator)
end method as;

define sealed sideways method as
    (class :: subclass(<locator>), string :: <string>)
 => (locator :: <locator>)
  string-as-locator(class, string)
end method as;


/// Locator conditions

define class <locator-error> (<simple-error>)
end class <locator-error>;

define function locator-error
    (format-string :: <string>, #rest format-arguments)
  error(make(<locator-error>,
             format-string:    format-string,
             format-arguments: format-arguments))
end function locator-error;


/// Useful locator protocols

define open generic locator-test
    (locator :: <directory-locator>) => (test ::<function>);

define method locator-test
    (locator :: <directory-locator>) => (test :: <function>)
  \=
end method locator-test;

define method locator-relative?
    (locator :: <file-locator>) => (relative? :: <boolean>)
  let directory = locator.locator-directory;
  ~directory | directory.locator-relative?
end method locator-relative?;

define method current-directory-locator?
    (locator :: <directory-locator>) => (current-directory? :: <boolean>)
  locator.locator-relative?
    & locator.locator-path = #[#"self"]
end method current-directory-locator?;

define method locator-directory
    (locator :: <directory-locator>) => (parent :: false-or(<directory-locator>))
  let path = locator.locator-path;
  let relative? = locator.locator-relative?;
  // It is meaningless for a relative locator to have an empty path, whereas
  // for an absolute locator an empty path indicates the root directory.
  let min-path-size = if (relative?) 1 else 0 end;
  when (path.size > min-path-size)
    make(object-class(locator),
         server:    locator.locator-server,
         path:      copy-sequence(path, end: path.size - 1),
         relative?: relative?)
  end
end method locator-directory;

define method locator-path
  (locator :: <locator>) => (path :: <sequence>)
  #()
end method locator-path;


/// Simplify locator

// Simplify (or normalize) locator by collapsing redundant separators and
// parent references so that A//B, A/B/, A/./B and A/foo/../B all become
// A/B. This string manipulation may change the meaning of a path that contains
// symbolic links.
define open generic simplify-locator
    (locator :: <physical-locator>)
 => (simplified-locator :: <physical-locator>);

define method simplify-locator
    (locator :: <directory-locator>)
 => (simplified-locator :: <directory-locator>)
  let path = locator.locator-path;
  let relative? = locator.locator-relative?;
  let simplified-path = simplify-path(path, relative?: relative?);
  if (path = simplified-path)
    locator
  else
    make(object-class(locator),
         server:    locator.locator-server,
         path:      simplified-path,
         relative?: relative?)
  end
end method;

define method simplify-locator
    (locator :: <file-locator>)
 => (simplified-locator :: <file-locator>)
  let directory = locator.locator-directory;
  let simplified-directory = directory & simplify-locator(directory);
  if (directory = simplified-directory)
    locator
  else
    make(object-class(locator),
         directory: simplified-directory,
         base:      locator.locator-base,
         extension: locator.locator-extension)
  end
end method;

// Check the file system to resolve and expand links, and normalize the path.
// Returns an absolute locator, using the current process's working directory
// to resolve relative locators, or signals <file-system-error>. Note that lack
// of an error does not mean that the resolved locator names an existing file,
// but does mean the containing directory exists. In other words, this function
// inherits POSIX `realpath` semantics.
define open generic resolve-locator
    (locator :: <physical-locator>)
 => (resolved-locator :: <physical-locator>);

define method resolve-locator
    (locator :: <physical-locator>)
 => (resolved-locator :: <physical-locator>)
  let resolved = %resolve-locator(locator);
  let class = if (file-type(resolved) == #"directory")
                <file-system-directory-locator>
              else
                <file-system-file-locator>
              end;
  as(class, resolved)
end method;



/// Subdirectory locator

define open generic subdirectory-locator
    (locator :: <directory-locator>, #rest sub-path)
 => (subdirectory :: <directory-locator>);

define method subdirectory-locator
    (locator :: <directory-locator>, #rest sub-path)
 => (subdirectory :: <directory-locator>)
  let old-path = locator.locator-path;
  let new-path = concatenate-as(<simple-object-vector>, old-path, sub-path);
  make(object-class(locator),
       server:    locator.locator-server,
       path:      new-path,
       relative?: locator.locator-relative?)
end method subdirectory-locator;


/// File locator

// Make a <file-locator> that is a child of `directory`. If more than one name
// is supplied, the last name is the name of the file and earlier names are
// subdirectories.
define open generic file-locator
    (directory :: <directory-locator>, name, #rest more-names)
 => (file :: <file-locator>);

define method file-locator
    (directory :: <directory-locator>, name :: <string>, #rest more-names)
 => (file :: <file-locator>)
  let length = more-names.size;
  if (length == 0)
    make(<file-locator>, directory: directory, name: name)
  else
    make(<file-locator>,
         directory: apply(subdirectory-locator, directory, name,
                          copy-sequence(more-names, end: length - 1)),
         name: last(more-names))
  end
end method;



/// Relative locator

define open generic relative-locator
    (locator :: <physical-locator>, from-locator :: <physical-locator>)
 => (relative-locator :: <physical-locator>);

define method relative-locator
    (locator :: <directory-locator>, from-locator :: <directory-locator>)
 => (relative-locator :: <directory-locator>)
  let path = locator.locator-path;
  let from-path = from-locator.locator-path;
  case
    ~locator.locator-relative? & from-locator.locator-relative? =>
      locator-error
        ("Cannot find relative path of absolute locator %= from relative locator %=",
         locator, from-locator);
    locator.locator-server ~= from-locator.locator-server =>
      locator;
    path = from-path =>
      make(object-class(locator),
           path: vector(#"self"),
           relative?: #t);
    otherwise =>
      make(object-class(locator),
           path: relative-path(path, from-path, test: locator.locator-test),
           relative?: #t);
  end
end method relative-locator;

define method relative-locator
    (locator :: <file-locator>, from-directory :: <directory-locator>)
 => (relative-locator :: <file-locator>)
  let directory = locator.locator-directory;
  let relative-directory = directory & relative-locator(directory, from-directory);
  if (relative-directory = directory)
    locator
  else
    make(object-class(locator),
         directory: relative-directory,
         base:      locator.locator-base,
         extension: locator.locator-extension)
  end
end method relative-locator;

define method relative-locator
    (locator :: <physical-locator>, from-locator :: <file-locator>)
 => (relative-locator :: <physical-locator>)
  let from-directory = from-locator.locator-directory;
  case
    from-directory =>
      relative-locator(locator, from-directory);
    ~locator.locator-relative? =>
      locator-error
        ("Cannot find relative path of absolute locator %= from relative locator %=",
         locator, from-locator);
    otherwise =>
      locator;
  end
end method relative-locator;


/// Merge locators

// Construct a new locator from `locator` by copying missing or incomplete
// parts from `from-locator`.  Note that if `locator` is relative the resulting
// directory part is the concatenation of the directories of `from-locator` and
// `locator`, either of which may be empty.
define open generic merge-locators
    (locator :: <physical-locator>, from-locator :: <physical-locator>)
 => (merged-locator :: <physical-locator>);


/// Merge locators

define method merge-locators
    (locator :: <directory-locator>, from-locator :: <directory-locator>)
 => (merged :: <directory-locator>)
  if (locator.locator-relative?)
    let path = concatenate(from-locator.locator-path, locator.locator-path);
    make(object-class(locator),
         server:    from-locator.locator-server,
         path:      path,
         relative?: from-locator.locator-relative?)
  else
    locator
  end
end method;

define method merge-locators
    (locator :: <file-locator>, from-locator :: <directory-locator>)
 => (merged :: <file-locator>)
  let directory = locator.locator-directory;
  let merged-directory = if (directory)
                           merge-locators(directory, from-locator)
                         else
                           from-locator
                         end;
  if (merged-directory = directory)
    locator
  else
    make(object-class(locator),
         directory: merged-directory,
         base:      locator.locator-base,
         extension: locator.locator-extension)
  end
end method;

define method merge-locators
    (locator :: <directory-locator>, from-locator :: <file-locator>)
 => (merged-locator :: <file-locator>)
  let from-directory = locator-directory(from-locator);
  let directory = if (from-directory)
                    merge-locators(locator, from-directory)
                  else
                    locator
                  end;
  make(object-class(from-locator),
       directory: directory,
       base: locator-base(from-locator),
       extension: locator-extension(from-locator))
end method;

define method merge-locators
    (locator :: <file-locator>, from-locator :: <file-locator>)
 => (merged-locator :: <file-locator>)
  let from-directory = from-locator.locator-directory;
  if (from-directory)
    merge-locators(locator, from-directory)
  else
    locator
  end
end method;


/// Locator protocols

define sideways method supports-open-locator?
    (locator :: <file-locator>) => (openable? :: <boolean>)
  ~locator.locator-relative?
end method supports-open-locator?;

define sideways method open-locator
    (locator :: <file-locator>, #rest keywords, #key, #all-keys)
 => (stream :: <stream>)
  apply(open-file-stream, locator, keywords)
end method open-locator;
