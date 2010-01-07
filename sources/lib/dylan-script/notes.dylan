Module: dylan-script-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Example: #notes://noteslong -> ref to the server
// Example: #notes://noteslong/basics/dylanevo.nsf -> ref to a database

define class <notes-locator> (<locator>)
  slot locator-host :: <byte-string>,
    required-init-keyword: host:;
  slot locator-database :: false-or(<byte-string>),
    init-keyword: database:;
end class;

define method notes-parser 
    (string :: <byte-string>) => (locator :: <notes-locator>)
  let (host, port, path) = split-url(string);
  make(<notes-locator>,
       host: host,
       // Need to remove the leading / from the path
       database: path & as-backslash-string(copy-sequence(path, start: 1)));
end method;

define method as-backslash-string 
    (string :: <byte-string>) => (string :: <byte-string>)
  let start = #f;
  while ((start := subsequence-position(string, "/")))
    string[start] := '\\';
  end;
  string
end method;

define method as-slash-string 
    (string :: <byte-string>) => (string :: <byte-string>)
  let start = #f;
  while ((start := subsequence-position(string, "\\")))
    string[start] := '/';
  end;
  string
end method;

define method locator-as-string
    (type == <string>, locator :: <notes-locator>)
 => (string :: <byte-string>)
  locator-as-string(<byte-string>, locator)
end method;

define method locator-as-string
    (type == <byte-string>, locator :: <notes-locator>)
 => (string :: <byte-string>)
   let database = locator-database(locator);
   if (database)
     concatenate
       ("notes://", locator-host(locator), "/", as-slash-string(database));
   else
     concatenate("notes://", locator-host(locator));
   end;
end method;

/// One, lazily-initialized session for now

define variable *notes-session* = #f;

define method ensure-notes-session () => (sesion :: <NOTESSESSION>)
  if (*notes-session*)
    *notes-session*
  else
    OLE-initialize();
    *notes-session* := make(<NOTESSESSION>, 
                            class-id: $session-id,
                            interface-id: $IID-IDispatch);
  end;
end method;

define method contents
    (locator :: <notes-locator>, #rest options, #key, #all-keys)
 => (contents)
  let database = locator-database(locator);
  ensure-notes-session();
  if (database)
    notes-database-contents(locator);
  else
    apply(notes-directory-contents, locator, options);
  end;
end method;

define method notes-database-contents
    (locator :: <notes-locator>, #key) => (contents)
  let raw-db 
    = notessession/getdatabase
        (*notes-session*, locator-host(locator), locator-database(locator));
  let db = cast-object(<NOTESDATABASE>, raw-db);
  notesdatabase/isopen(db) := #t;
  format-out("Open?: %=\n", notesdatabase/isopen(db));
  // notesdatabase/open(db, locator-host(locator), locator-database(locator));
  format-out("Title: %=\n", database-title(db));
  let raw-views = notesdatabase/views(db);
  collecting ()
    for (raw-view in raw-views)
      let view = cast-object(<NOTESVIEW>, raw-view);
      collect(notesview/name(view));
      Release(view);
    end;
    notesdatabase/close(db);
    Release(db);
  end;
end method;

define method notes-directory-contents
    (locator :: <notes-locator>, #key title-pattern) => (contents)
  collecting ()
    let dir 
      = notessession/getdbdirectory(*notes-session*, locator-host(locator));
    block ()
      let dir = cast-object(<NOTESDBDIRECTORY>, dir);
      for (db =    notesdbdirectory/getfirstdatabase(dir, $TEMPLATE-CANDIDATE)
              then notesdbdirectory/getnextdatabase(dir),
           until: null-pointer?(db))
        let db = cast-object(<NOTESDATABASE>, db);
        block ()
          let title = as(<byte-string>, notesdatabase/title(db));
          format-out
            ("DB: %s : %s : %s\n", 
             title,
             as(<byte-string>, notesdatabase/filename(db)),
             as(<byte-string>, notesdatabase/filepath(db)));
          if (~title-pattern | find(title-pattern, title))
            collect(make(<notes-locator>,
                         host: locator-host(locator),
                         database: as(<byte-string>, 
                                      notesdatabase/filepath(db))));
          end;
        cleanup
          Release(db);
        end block;
      end for;
    cleanup
      Release(dir);
    end block;
  end collecting;
end method;

define method database-title 
    (db :: <NOTESDATABASE>) => (title :: <byte-string>)
  as(<byte-string>, notesdatabase/title(db))  
end method;

define method database-filepath 
    (db :: <NOTESDATABASE>) => (title :: <byte-string>)
  as(<byte-string>, notesdatabase/filepath(db))  
end method;
