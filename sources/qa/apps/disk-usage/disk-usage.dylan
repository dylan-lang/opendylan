Module:    disk-usage
Synopsis:  Report directory contents in human readable and log formats
Author:    Carl Gay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The code in this file is responsible for producing reports of disk usage
// in human readable and log formats, and for parsing the logs back into
// a usable form.  The format is very simple, but it could probably be made
// much simpler by using DOOD.

define constant <report-type>
  = one-of(#"log",         // A format suitable for parsing by test-report
           #"detailed",    // A line for each file or directory
           #"brief",       // A line for each directory
           #"none");       // No report, just return a total

define thread variable *report* :: <report-type> = #"brief";

define constant $file-name-column-width :: <integer> = 45;

define constant $log-file-header :: <byte-string>
  = "--------Disk Usage Log Report--------";

// report-disk-usage
//
define method report-disk-usage
    (dir-info :: <dir-info>,
     #key report :: <report-type> = *report*,
          sort-by :: one-of(#"name", #"size", #"none"))
 => (total-bytes :: <integer>)
  dynamic-bind(*report* = report)
    if (*report* == #"log")
      format-out("\n%s\nROOT %s\n", $log-file-header, dir-info.file-name);
      display-disk-usage-log(dir-info);
    elseif (report ~== #"none")
      let infos = if (sort-by == #"none")
                    flatten-file-info(dir-info)
                  else
                    sort!(flatten-file-info(dir-info),
                          test: select (sort-by)
                                  #"name" => info-name-less?;
                                  #"size" => info-size-greater?;
                                end)
                  end if;
      do(method (info)
	   if (instance?(info, <dir-info>) | report == #"detailed")
	     print-file-info(info);
	   end if;
	 end method,
	 infos);
    end if;
    dir-info.file-size
  end dynamic-bind
end method report-disk-usage;

define method display-disk-usage-log
    (info :: <dir-info>) => ()
  format-out("\nDIRECTORY %d %s\n", info.file-size, info.file-name);
  for (subinfo in info.directory-data)
    display-disk-usage-log(subinfo);
  end for;
  format-out("END\n");
end method display-disk-usage-log;

define method display-disk-usage-log
    (info :: <file-info>) => ()
  format-out("FILE %d %s\n", info.file-size, as(<string>, info.file-name));
end method display-disk-usage-log;

define function flatten-file-info
    (info :: <dir-info>) => (infos :: <sequence>)
  local method flatten (info :: <dir-info>, results :: <stretchy-vector>)
                    => (results :: <stretchy-vector>)
	  add!(results, info);
	  for (item in info.directory-data)
	    if (instance?(item, <dir-info>))
	      flatten(item, results);
	    else
	      add!(results, item);
	    end if;
	  end for;
	  results
	end method;
  flatten(info, make(<stretchy-vector>))
end function flatten-file-info;

define class <file-info> (<object>)
  slot file-locator :: <locator>,
    required-init-keyword: #"locator";
  slot %file-name :: false-or(<string>) = #f;
  constant slot file-size :: <integer>,
    required-init-keyword: #"size";
end class <file-info>;

define class <dir-info> (<file-info>)
  constant slot directory-data :: <sequence>,
    required-init-keyword: #"data";
end class <dir-info>;

define function file-name (info :: <file-info>) => (name :: <string>)
  info.%file-name
    | (info.%file-name := as(<string>, info.file-locator))
end function file-name;

define method print-file-info
    (info :: <file-info>) => ()
  format-out("%s %s (%s)\n",
	     pad(info.file-name, $file-name-column-width, #t),
	     pad(integer-to-string(info.file-size), 9, #f),
	     kbytes(info.file-size));
end method print-file-info;

define method do-file-info
    (f :: <function>, info :: <file-info>) => ()
  f(info)
end method do-file-info;

define method do-file-info
    (f :: <function>, info :: <dir-info>) => ()
  f(info);
  do(curry(do-file-info, f), info.directory-data);
end method do-file-info;

// The following two are sort test functions

define method info-name-less?
    (info1 :: <file-info>, info2 :: <file-info>)
 => (less? :: <boolean>)
  string-less?(info1.file-name, info2.file-name);
end method info-name-less?;

define method info-size-greater?
    (info1 :: <file-info>, info2 :: <file-info>)
 => (less? :: <boolean>)
  info1.file-size > info2.file-size;
end method info-size-greater?;

// Given a directory, return a <dir-info> object to use for comparisons.
// All stored filenames (except the top-level dir) are relative to the
// initial directory.
//
define function dir-infoify
    (root :: <directory-locator>)
 => (info :: <dir-info>, total-size :: <integer>)
  local method do-one-directory (directory)
          let data = make(<stretchy-vector>);
          let total-size :: <integer> = 0;
          local method do-one-file (dir, name, type)
		  let dir = as(<directory-locator>, dir);
                  if (type == #"directory")
		    let subdir = subdirectory-locator(dir, name);
                    if (name ~= "." & name ~= "..")
                      let (info, dsize) = do-one-directory(subdir);
                      total-size := total-size + dsize;
                      add!(data, info);
                    end if;
		  else
		    let file = merge-locators(as(<file-locator>, name), dir);
		    let fsize = file-property(file, #"size");
                    total-size := total-size + fsize;
                    add!(data, make(<file-info>,
				    locator: relative-locator(file, root),
                                    size: fsize));
                  end if;
                end method do-one-file;
            do-directory(do-one-file, directory);
            values(make(<dir-info>,
                        locator: relative-locator(directory, root),
                        size: total-size,
                        data: data),
                   total-size)
          end method do-one-directory;
  let (info, total-size) = do-one-directory(root);
  info.file-locator := root;
  values(info, total-size)
end function dir-infoify;

/* Log file format looks like this:

  <log file header>
  ROOT <complete path of root directory>
  DIRECTORY <path of dir1 relative to root>   (may be blank, if dir1 = root)
  FILE <bytes> <file name relative to dir1>
  DIRECTORY <path of dir2 relative to root>
  FILE <bytes> <file name relative to dir2>
  FILE <bytes> <file name relative to dir2>
  END DIRECTORY <bytes> <path of dir2 relative to root>
  FILE <bytes> <file name relative to dir1>
  FILE <bytes> <file name relative to dir1>
  END DIRECTORY <bytes> <path of dir1 relative to root>

  The number of bytes for the file/dir is given before the pathname to make dealing
  with spaces in pathnames easier.  Blank lines are ignored.
*/

// Top-level function for reading a disk-usage log file.
//
define function read-disk-usage-file
    (file-name :: <string>) => (info :: <dir-info>)
  with-open-file (in = file-name, direction: #"input")
    find-line-starting-with(in, $log-file-header);
    let line1 = find-line-starting-with(in, "ROOT ");
    ignore(line1);
    let line2 = find-line-starting-with(in, "DIRECTORY ");
    let (bytes, locator) = parse-dir-line(line2);
    let contents = read-directory-from-log(in);
    make(<dir-info>, size: bytes, locator: locator, data: contents)
  end
end function read-disk-usage-file;

define function line-starts-with
    (line :: <string>, s :: <string>) => (b :: <boolean>)
  block (return)
    let len = size(line);
    for (i from 0 below size(s))
      if (i >= len | line[i] ~= s[i])
        return(#f);
      end if;
    end for;
    #t
  end block
end function line-starts-with;

define method find-line-starting-with
    (in :: <file-stream>, s :: <string>) => (line :: <string>)
  block (return)
    while (#t)
      let line = read-line(in, on-end-of-stream: #f);
      if (~line)
        error("End of file reached without finding a line starting with %=.", s);
      elseif (line-starts-with(line, s))
        return(line);
      end if;
    end while;
  end block;
end method find-line-starting-with;

// When this is called the file stream is positioned following the DIRECTORY line.
//
define function read-directory-from-log
    (in :: <file-stream>) => (contents :: <sequence>)
  block (return)
    let contents = make(<stretchy-vector>);
    while (#t)
      let line = read-line(in, on-end-of-stream: #f);
      if (~line)
        error("Reached end of stream without finding end of directory.");
      elseif (line-starts-with(line, "FILE"))
        add!(contents, parse-file-line(line));
      elseif (line-starts-with(line, "DIRECTORY"))
        let (bytes, locator) = parse-dir-line(line);
        let subcontents = read-directory-from-log(in);
        add!(contents, make(<dir-info>, size: bytes, locator: locator, data: subcontents));
      elseif (line-starts-with(line, "END"))
        return(contents);
      else
        #f  // ignore unrecognized (e.g., blank) lines
      end if;
    end while;
  end block;
end function read-directory-from-log;

// Parse a line like "FILE 21345 <file-name>"
//
define function parse-file-line
    (line :: <string>) => (info :: <file-info>)
  let (file-size, file-name) = parse-file-line-internal(line, "FILE ");
  make(<file-info>,
       size: file-size,
       locator: as(<file-locator>, file-name))
end function parse-file-line;

define function parse-file-line-internal
    (line :: <string>, initial-substring :: <string>)
 => (size :: <integer>, name :: <string>)
  let (file-size, _end) = string-to-integer(line, start: size(initial-substring));
  values(file-size, copy-sequence(line, start: _end + 1))
end function parse-file-line-internal;

define function parse-dir-line
    (line :: <string>) => (size :: <integer>, locator :: <directory-locator>)
  let (size, dir-name) = parse-file-line-internal(line, "DIRECTORY ");
  values(size, as(<directory-locator>, dir-name))
end function parse-dir-line;

define function kbytes
    (bytes :: <integer>, #key sign? :: <boolean>)
 => (kbytes :: <string>)
  let sign = if (bytes < 0) "-" elseif (sign?) "+" else "" end;
  format-to-string("%s%dK", sign, ceiling/(abs(bytes), 1024));
end function kbytes;

