Module:       system-internals
Author:       Gary Palter
Synopsis:     UNIX implementation of the File System library API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Expand "~" and "~USER" in a pathname.
//
// TODO(cgay): most file-system functions shouldn't call this, since file names
// with ~ in them are perfectly valid; the functionality should be provided
// only for user code to call explicitly.
define method %expand-pathname
    (dir :: <posix-directory-locator>) => (expanded-path :: <locator>)
  block (return)
    if (~locator-relative?(dir))
      return(dir);
    end;
    let elements = locator-path(dir);
    if (empty?(elements))
      return(dir);
    end;
    let first = elements[0];
    if (~instance?(first, <string>) | first.size = 0 | first[0] ~= '~')
      return(dir);
    end;
    let name = case
                 first = "~" => login-name();
                 otherwise   => copy-sequence(first, start: 1);
               end;
    let passwd = primitive-wrap-machine-word
                   (primitive-cast-pointer-as-raw
                      (%call-c-function ("getpwnam")
                           (name :: <raw-byte-string>) => (passwd :: <raw-c-pointer>)
                           (primitive-string-as-raw(name))
                       end));
    if (primitive-machine-word-equal?(primitive-unwrap-machine-word(passwd),
                                      integer-as-raw(0)))
      dir
    else
      let homedir = as(<native-directory-locator>, passwd-dir(passwd));
      return(merge-locators(make(<native-directory-locator>,
                                 path: copy-sequence(elements, start: 1),
                                 relative?: #t),
                            homedir))
    end
  end
end method;

define method %expand-pathname
    (file :: <posix-file-locator>) => (expanded-path :: <locator>)
  let directory = locator-directory(file);
  let expanded-directory = directory & %expand-pathname(directory);
  if (directory = expanded-directory)
    file
  else
    make(<native-file-locator>,
         directory: expanded-directory,
         base: locator-base(file),
         extension: locator-extension(file))
  end
end method;

define method %expand-pathname
    (path :: <posix-file-system-locator>) => (expanded-path :: <locator>)
  path
end method;


// No-op implementation for Windows-only feature.
define function %shorten-pathname
    (path :: <posix-file-system-locator>)
 => (shortened-path :: <posix-file-system-locator>)
  path
end function %shorten-pathname;

define function %resolve-locator
    (locator :: <posix-file-system-locator>)
 => (resolved-locator :: <posix-file-system-locator>)
  let path = as(<byte-string>, locator);
  with-storage (resolved-path, $path-max)
    let result
      = primitive-wrap-machine-word(
            primitive-cast-pointer-as-raw(
                %call-c-function ("realpath")
                      (name :: <raw-byte-string>, buffer :: <raw-byte-string>)
                   => (rpath :: <raw-byte-string>)
                  (primitive-string-as-raw(path),
                   primitive-cast-raw-as-pointer(
                       primitive-unwrap-machine-word(resolved-path)))
                end));
    if (result = resolved-path)
      let resolved = primitive-raw-as-string(
                         primitive-cast-raw-as-pointer(
                             primitive-unwrap-machine-word(resolved-path)));
      string-as-locator(object-class(locator), resolved)
    else
      unix-file-error("get realpath for", "%=", path);
    end
  end with-storage
end function;

define function %file-exists?
    (file :: <posix-file-system-locator>, follow-links? :: <boolean>)
 => (exists? :: <boolean>)
  let file = %expand-pathname(file);
  with-stack-stat (st, file)
    ~primitive-raw-as-boolean(
      if (follow-links?)
        (%call-c-function ("system_stat")
              (path :: <raw-byte-string>, st :: <raw-c-pointer>)
           => (failed? :: <raw-c-signed-int>)
           (primitive-string-as-raw(as(<byte-string>, file)),
            primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(st)))
        end)
      else
        (%call-c-function ("system_lstat")
              (path :: <raw-byte-string>, st :: <raw-c-pointer>)
           => (failed? :: <raw-c-signed-int>)
           (primitive-string-as-raw(as(<byte-string>, file)),
            primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(st)))
        end)
      end)
  end
end function;

///
define function %file-type
    (file :: <posix-file-system-locator>, #key if-not-exists = #f)
 => (file-type :: <file-type>)
  let file = %expand-pathname(file);
  with-stack-stat (st, file)
    if (primitive-raw-as-boolean
          (%call-c-function ("system_lstat") (path :: <raw-byte-string>, st :: <raw-c-pointer>)
            => (failed? :: <raw-c-signed-int>)
             (primitive-string-as-raw(as(<byte-string>, file)),
              primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(st)))
           end))
      if (unix-errno() = $ENOENT & if-not-exists)
        if-not-exists
      else
        unix-file-error("determine the type of", "%s", file)
      end
    elseif (logand(st-mode(st), $S_IFMT) = $S_IFDIR)
      #"directory"
    elseif (logand(st-mode(st), $S_IFMT) = $S_IFLNK)
      #"link"
    else
      #"file"
    end
  end
end function %file-type;


///
define function %link-target
    (link :: <posix-file-system-locator>) => (target :: <posix-file-system-locator>)
  let link = %expand-pathname(link);
  while (%file-type(link, if-not-exists: #"file") == #"link")
    let buffer = make(<byte-string>, size: 8192, fill: '\0');
    let count
      = raw-as-integer(%call-c-function ("readlink")
                           (path :: <raw-byte-string>, buffer :: <raw-byte-string>,
                            bufsize :: <raw-c-size-t>)
                        => (count :: <raw-c-ssize-t>)
                           (primitive-string-as-raw(as(<byte-string>, link)),
                            primitive-string-as-raw(buffer),
                            integer-as-raw(8192))
                       end);
    if (count = -1)
      unless (unix-errno() = $ENOENT | unix-errno() = $EINVAL)
        unix-file-error("readlink", "%s", link)
      end
    else
      let target = as(<file-system-locator>, copy-sequence(buffer, end: count));
      link := merge-locators(target, link)
    end
  end;
  link
end function %link-target;


///
define function %delete-file
    (file :: <posix-file-system-locator>) => ()
  let file = %expand-pathname(file);
  if (primitive-raw-as-boolean
        (%call-c-function ("unlink")
             (path :: <raw-byte-string>) => (failed? :: <raw-c-signed-int>)
           (primitive-string-as-raw(as(<byte-string>, file)))
         end))
    unix-file-error("delete", "%s", file)
  end
end function %delete-file;


/// Whoever heard of an OS that doesn't provide a primitive to copy files?
/// Why, the creators of UNIX, of course since it doesn't.  We have to resort
/// to invoking the cp (copy) command via RUN-APPLICATION.
define function %copy-file
    (source :: <posix-file-system-locator>, destination :: <posix-file-system-locator>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  let source = %expand-pathname(source);
  let destination = %expand-pathname(destination);
  // UNIX strikes again!  The copy command will overwrite its target if
  // the user has write access and the only way to prevent it would
  // require the user to respond to a question!  So, we have to manually
  // check beforehand.  (Just another reason I'm a member of Unix-Haters)
  if (if-exists = #"signal" & file-exists?(destination))
    error(make(<file-system-error>,
               format-string: "File exists: Can't copy %s to %s",
               format-arguments: list(as(<string>, source),
                                      as(<string>, destination))))
  end;
  run-application
    (concatenate
       ("cp -p",
        " \"",
        as(<string>, source),
        "\" \"",
        as(<string>, destination),
        "\""))
end function %copy-file;


///
define function %rename-file
    (source :: <posix-file-system-locator>, destination :: <posix-file-system-locator>,
     #Key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  let source = %expand-pathname(source);
  let destination = %expand-pathname(destination);
  // UNIX strikes again!  It's rename function always replaces the target.
  // So, if the caller doesn't want to overwrite an existing file, we have
  // to manually check beforehand.  (Sigh)
  if (if-exists = #"signal" & file-exists?(destination))
    error(make(<file-system-error>,
               format-string: "File exists: Can't rename %s to %s",
               format-arguments: list(as(<string>, source),
                                      as(<string>, destination))))
  end;
  if (primitive-raw-as-boolean
        (%call-c-function ("rename") (from :: <raw-byte-string>, to :: <raw-byte-string>)
          => (failed? :: <raw-c-signed-int>)
           (primitive-string-as-raw(as(<byte-string>, source)),
            primitive-string-as-raw(as(<byte-string>, destination)))
         end))
    unix-file-error("rename", "%s to %s", source, destination)
  end
end function %rename-file;


///
define function %file-properties
    (file :: <posix-file-system-locator>)
 => (properties :: <explicit-key-collection>)
  let file = %expand-pathname(file);
  let properties = make(<table>);
  with-stack-stat (st, file)
    if (primitive-raw-as-boolean
          (%call-c-function ("system_stat") (path :: <raw-byte-string>, st :: <raw-c-pointer>)
            => (failed? :: <raw-c-signed-int>)
             (primitive-string-as-raw(as(<byte-string>, file)),
              primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(st)))
           end))
      unix-file-error("get attributes of", "%s", file)
    else
      properties[#"size"] := st-size(st);
      properties[#"creation-date"] := make(<date>, native-clock: st-ctime(st));
      properties[#"access-date"] := make(<date>, native-clock: st-atime(st));
      properties[#"modification-date"] := make(<date>, native-clock: st-mtime(st))
    end
  end;
  properties[#"author"] := %file-property(file, #"author");
  properties[#"readable?"] := %file-property(file, #"readable?");
  properties[#"writeable?"] := %file-property(file, #"writeable?");
  properties[#"executable?"] := %file-property(file, #"executable?");
  properties
end function %file-properties;


/// "Standard" properties not implemented on this platform:
///    ?

/// "Standard" properties not settable on this platform:
///    author, size, creation-date, access-date, modification-date

define method %file-property
    (file :: <posix-file-system-locator>, key == #"author")
 => (author :: false-or(<string>))
  let file = %expand-pathname(file);
  with-stack-stat (st, file)
    if (primitive-raw-as-boolean
          (%call-c-function ("system_stat") (path :: <raw-byte-string>, st :: <raw-c-pointer>)
            => (failed? :: <raw-c-signed-int>)
             (primitive-string-as-raw(as(<byte-string>, file)),
              primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(st)))
           end))
      unix-file-error("get the author of", "%s", file)
    end;
    let passwd = primitive-wrap-machine-word
                   (primitive-cast-pointer-as-raw
                      (%call-c-function ("getpwuid")
                           (uid :: <raw-c-unsigned-int>) => (passwd :: <raw-c-pointer>)
                         (abstract-integer-as-raw(st-uid(st)))
                       end));
    if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(passwd),
                                          integer-as-raw(0)))
      passwd-name(passwd)
    else
      unix-file-error("get the author of", "%s", file)
    end
  end
end method %file-property;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"size")
 => (file-size :: <integer>)
  let file = %expand-pathname(file);
  with-stack-stat (st, file)
    if (primitive-raw-as-boolean
          (%call-c-function ("system_stat") (path :: <raw-byte-string>, st :: <raw-c-pointer>)
            => (failed? :: <raw-c-signed-int>)
             (primitive-string-as-raw(as(<byte-string>, file)),
              primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(st)))
           end))
      unix-file-error("get the size of", "%s", file)
    else
      st-size(st)
    end
  end
end method %file-property;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"creation-date")
 => (creation-date :: <date>)
  let file = %expand-pathname(file);
  with-stack-stat (st, file)
    if (primitive-raw-as-boolean
          (%call-c-function ("system_stat") (path :: <raw-byte-string>, st :: <raw-c-pointer>)
            => (failed? :: <raw-c-signed-int>)
             (primitive-string-as-raw(as(<string>, file)),
              primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(st)))
           end))
      unix-file-error("get the creation date of", "%s", file)
    else
      make(<date>, native-clock: st-ctime(st))
    end
  end
end method %file-property;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"access-date")
 => (access-date :: <date>)
  let file = %expand-pathname(file);
  with-stack-stat (st, file)
    if (primitive-raw-as-boolean
          (%call-c-function ("system_stat") (path :: <raw-byte-string>, st :: <raw-c-pointer>)
            => (failed? :: <raw-c-signed-int>)
             (primitive-string-as-raw(as(<byte-string>, file)),
              primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(st)))
           end))
      unix-file-error("get the access date of", "%s", file)
    else
      make(<date>, native-clock: st-atime(st))
    end
  end
end method %file-property;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"modification-date")
 => (modification-date :: <date>)
  let file = %expand-pathname(file);
  with-stack-stat (st, file)
    if (primitive-raw-as-boolean
          (%call-c-function ("system_stat") (path :: <raw-byte-string>, st :: <raw-c-pointer>)
            => (failed? :: <raw-c-signed-int>)
             (primitive-string-as-raw(as(<byte-string>, file)),
              primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(st)))
           end))
      unix-file-error("get the modification date of", "%s", file)
    else
      make(<date>, native-clock: st-mtime(st))
    end
  end
end method %file-property;

define function accessible?
    (file :: <posix-file-system-locator>, mode :: <integer>)
 => (accessible? :: <boolean>)
  let file = %expand-pathname(file);
  if (primitive-raw-as-boolean
        (%call-c-function ("access") (path :: <raw-byte-string>, mode :: <raw-c-signed-int>)
          => (failed? :: <raw-c-signed-int>)
           (primitive-string-as-raw(as(<byte-string>, file)), abstract-integer-as-raw(mode))
         end))
    let errno = unix-errno();
    unless (errno = $EACCES | errno = $EROFS | errno = $ETXTBSY)
      unix-file-error("determine access to", "%s (errno = %=)", file, errno)
    end;
    #f
  else
    #t
  end
end function accessible?;

define function accessible?-setter
    (new-mode :: <integer>, file :: <posix-file-system-locator>, on? :: <boolean>)
 => (new-mode :: <integer>)
  let file = %expand-pathname(file);
  with-stack-stat (st, file)
    if (primitive-raw-as-boolean
          (%call-c-function ("system_stat") (path :: <raw-byte-string>, st :: <raw-c-pointer>)
            => (failed? :: <raw-c-signed-int>)
             (primitive-string-as-raw(as(<byte-string>, file)),
              primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(st)))
           end))
      unix-file-error("get permissions for", "%s", file)
    else
      let old-mode = st-mode(st);
      let mode = if (on?)
                   logior(old-mode, new-mode)
                 else
                   logand(old-mode, lognot(new-mode))
                 end;
      if (primitive-raw-as-boolean
            (%call-c-function ("chmod")
                  (path :: <raw-byte-string>, mode :: <raw-c-unsigned-int>)
               => (failed? :: <raw-c-signed-int>)
               (primitive-string-as-raw(as(<byte-string>, file)),
                abstract-integer-as-raw(mode))
             end))
        unix-file-error("set permissions for", "%s", file)
      end
    end
  end;
  new-mode
end function accessible?-setter;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"readable?")
 => (readable? :: <boolean>)
  accessible?(file, $R_OK)
end method %file-property;

define method %file-property-setter
    (new-readable? :: <boolean>, file :: <posix-file-system-locator>,
     key == #"readable?")
 => (new-readable? :: <boolean>)
  if (new-readable? ~= %file-property(file, #"readable?"))
    accessible?(file, new-readable?) := logior($S_IRUSR, $S_IRGRP, $S_IROTH)
  end;
  new-readable?
end method %file-property-setter;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"writeable?")
 => (writeable? :: <boolean>)
  accessible?(file, $W_OK)
end method %file-property;

define method %file-property-setter
    (new-writeable? :: <boolean>, file :: <posix-file-system-locator>,
     key == #"writeable?")
 => (new-writeable? :: <boolean>)
  if (new-writeable? ~= %file-property(file, #"writeable?"))
    accessible?(file, new-writeable?) := logior($S_IWUSR, $S_IWGRP, $S_IWOTH)
  end;
  new-writeable?
end method %file-property-setter;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"executable?")
 => (executable? :: <boolean>)
  accessible?(file, $X_OK)
end method %file-property;

define method %file-property-setter
    (new-executable? :: <boolean>, file :: <posix-file-system-locator>,
     key == #"executable?")
 => (new-executable? :: <boolean>)
  if (new-executable? ~= %file-property(file, #"executable?"))
    accessible?(file, new-executable?) := logior($S_IXUSR, $S_IXGRP, $S_IXOTH)
  end;
  new-executable?
end method %file-property-setter;


///
define constant $INVALID_DIRECTORY_FD = 0;
define constant $NO_MORE_DIRENTRIES = 0;

define function %do-directory
    (f :: <function>, directory :: <posix-directory-locator>) => ()
  let directory = %expand-pathname(directory);
  let directory-fd :: <machine-word> = as(<machine-word>, $INVALID_DIRECTORY_FD);
  block ()
    directory-fd := primitive-wrap-machine-word
                      (primitive-cast-pointer-as-raw
                         (%call-c-function ("system_opendir")
                              (path :: <raw-byte-string>) => (dir-fd :: <raw-c-pointer>)
                            (primitive-string-as-raw(as(<byte-string>, directory)))
                          end));
    if (primitive-machine-word-equal?
          (primitive-unwrap-machine-word(directory-fd),
           integer-as-raw($INVALID_DIRECTORY_FD)))
      unix-file-error("start listing of", "%s", directory)
    end;
    let dirent = primitive-wrap-machine-word
                   (primitive-cast-pointer-as-raw
                      (%call-c-function ("system_readdir")
                           (dir-fd :: <raw-c-pointer>) => (dirent :: <raw-c-pointer>)
                         (primitive-cast-raw-as-pointer
                            (primitive-unwrap-machine-word(directory-fd)))
                       end));
    while (primitive-machine-word-not-equal?
             (primitive-unwrap-machine-word(dirent),
              integer-as-raw($NO_MORE_DIRENTRIES)))
      let filename :: <byte-string> = dirent-name(dirent);
      let type :: <file-type>
        = %file-type(make(<posix-file-locator>,
                          directory: directory,
                          name: filename));
      unless (type == #"directory" & (filename = "." | filename = ".."))
        f(directory,
          filename,
          type)
      end;
      dirent := primitive-wrap-machine-word
                  (primitive-cast-pointer-as-raw
                     (%call-c-function ("system_readdir")
                          (dir-fd :: <raw-c-pointer>) => (dirent :: <raw-c-pointer>)
                        (primitive-cast-raw-as-pointer
                           (primitive-unwrap-machine-word(directory-fd)))
                      end));
    end;
/*
    if (primitive-machine-word-equal?
             (primitive-unwrap-machine-word(dirent),
              integer-as-raw($NO_MORE_DIRENTRIES)) & (unix-errno() ~= 0))
      unix-file-error("continue listing of", "%s", directory)
    end;
*/
  cleanup
    if (primitive-machine-word-not-equal?
          (primitive-unwrap-machine-word(directory-fd),
           integer-as-raw($INVALID_DIRECTORY_FD)))
      %call-c-function ("system_closedir")
          (dir :: <raw-c-pointer>) => (failed? :: <raw-c-signed-int>)
        (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(directory-fd)))
      end
    end
  end
end function %do-directory;


///
define function %create-directory
    (directory :: <posix-directory-locator>)
 => (directory :: <posix-directory-locator>)
  let directory = %expand-pathname(directory);
  if (primitive-raw-as-boolean
        (%call-c-function ("mkdir")
             (path :: <raw-byte-string>, mode :: <raw-c-unsigned-int>)
          => (failed? :: <raw-c-signed-int>)
           (primitive-string-as-raw(as(<byte-string>, directory)),
            // Let the process' UMASK restrict access to the directory as desired
            abstract-integer-as-raw(logior($S_IRWXU, $S_IRWXG, $S_IRWXO)))
         end))
    unix-file-error("create the directory", "%s", directory)
  else
    directory
  end
end function %create-directory;


///
define function %delete-directory
    (directory :: <posix-directory-locator>) => ()
  let directory = %expand-pathname(directory);
  if (primitive-raw-as-boolean
        (%call-c-function ("rmdir")
            (path :: <raw-byte-string>) => (failed? :: <raw-c-signed-int>)
           (primitive-string-as-raw(as(<byte-string>, directory)))
         end))
    unix-file-error("delete", "%s", directory)
  end
end function %delete-directory;


///---*** Is there an easier way?  (Look into it ...)
define function %directory-empty?
    (directory :: <posix-directory-locator>) => (empty? :: <boolean>)
  ~%file-exists?(directory, #t)
    | block (return)
        %do-directory
          (method (directory :: <posix-directory-locator>, name :: <string>, type :: <file-type>)
             ignore(directory); ignore(name); ignore(type);
             return(#f)
           end,
           directory);
        #t
      end
end function %directory-empty?;


///
define function %home-directory
    () => (home-directory :: false-or(<posix-directory-locator>))
  let path = environment-variable("HOME");
  path
    & as(<posix-directory-locator>, path)
end function %home-directory;


///
define constant $ERANGE = 34;

define function %working-directory
    () => (working-directory :: false-or(<posix-directory-locator>))
  let bufsiz :: <integer> = 128;
  let errno :: <integer> = $ERANGE;
  block (return)
    while (errno = $ERANGE)
      let buffer = make(<byte-string>, size: bufsiz, fill: '\0');
      if (primitive-machine-word-equal?
            (primitive-cast-pointer-as-raw(primitive-string-as-raw(buffer)),
             primitive-cast-pointer-as-raw
               (%call-c-function ("getcwd")
                    (buf :: <raw-byte-string>, size :: <raw-c-size-t>)
                 => (result :: <raw-pointer>)
                  (primitive-string-as-raw(buffer), integer-as-raw(bufsiz))
                end)))
        let _end = position(buffer, '\0', test: \=);
        return(as(<directory-locator>, copy-sequence(buffer, end: _end)))
      else
        errno := unix-errno();
        bufsiz := bufsiz * 2;
      end
    end;
    // Arrive here iff we couldn't get the working directory
    unix-file-error("getcwd", #f)
  end
end function %working-directory;


///
define function %working-directory-setter
    (new-working-directory :: <posix-directory-locator>)
 => (new-working-directory :: <posix-directory-locator>)
  let directory = %expand-pathname(new-working-directory);
  if (primitive-raw-as-boolean
        (%call-c-function ("chdir")
             (path :: <raw-byte-string>) => (failed? :: <raw-c-signed-int>)
           (primitive-string-as-raw(as(<byte-string>, directory)))
         end))
    unix-file-error("chdir", "%s", directory)
  end;
  directory
end function %working-directory-setter;


///
define variable *temp-directory* = #f;

define function %temp-directory
    () => (temp-directory :: <posix-directory-locator>)
  *temp-directory*
    | (*temp-directory* :=
         as(<posix-directory-locator>,
            primitive-raw-as-string(%call-c-function ("unix_tmpdir")
                                      ()
                                      => (tmpdir :: <raw-byte-string>)
                                      ()
           end)))
end function %temp-directory;


/// A UNIX system has exactly one root directory
define function %root-directories () => (roots :: <sequence>)
  vector(as(<posix-directory-locator>, "/"))
end function %root-directories;

define function %create-symbolic-link
    (target :: <posix-file-system-locator>, link :: <posix-file-system-locator>)
 => ()
  let target = %expand-pathname(target);
  let link   = %expand-pathname(link);
  if (primitive-raw-as-boolean
        (%call-c-function("symlink") 
             (target :: <raw-byte-string>, link :: <raw-byte-string>)
          => (failed? :: <raw-c-signed-int>)
           (primitive-string-as-raw(as(<byte-string>, target)),
            primitive-string-as-raw(as(<byte-string>, link)))
        end))
    unix-file-error("symlink", "create symbolic link to %s from %s", target, link)
  end
end function %create-symbolic-link;