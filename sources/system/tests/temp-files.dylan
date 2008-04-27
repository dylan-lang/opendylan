Module:       system-test-suite
Synopsis:     System library test suite
Author:       Carl Gay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This lock is used  to manage access to both *temp-file-counter*
// and *temp-file-date*.
define constant *temp-file-lock* :: <simple-lock> = make(<lock>);

define variable *temp-file-counter* :: <integer> = 0;

// This variable isn't strictly necessary.  I just think the file
// names look nicer if *temp-file-counter* is reset to 1 if the date
// has changed since the last time.
define variable *temp-file-date* :: <date> = current-date();

// Returns a unique pathname.  The pathname is guaranteed to be different from
// any other pathname returned from this function within the past month, as
// long as you don't call this function more than a million times per second.
// Should be thread safe.
define function temp-file-pathname
    (#key initial-substring :: false-or(<string>) = "tmp",
          extension :: false-or(<string>) = "dat",
          directory :: false-or(<directory-locator>) = temp-directory())
 => (path :: <file-locator>)
  let now = current-date();
  let counter = #f;
  with-lock (*temp-file-lock*)
    *temp-file-counter* := *temp-file-counter* + 1;
    if (*temp-file-counter* >= 1000000 | *temp-file-date* ~= now)
      *temp-file-counter* := 1;
    end if;
    *temp-file-date* := now;
    counter := *temp-file-counter*;
  end;
  let filename = concatenate(initial-substring | "",
			     format-date("%d%H%M%S.", now),
			     integer-to-string(counter, size: 6),
			     (extension & ".") | "",
			     extension | "");
  if (size(filename) > 31)
    // If it's too long for the Mac OS, just use the last bits...
    filename := copy-sequence(filename, start: size(filename) - 31)
  end;
  let directory = directory & as(<directory-locator>, directory);
  make(<file-locator>, name: filename, directory: directory)
end function temp-file-pathname;
