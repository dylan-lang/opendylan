Module:    map-statistics
Synopsis:  Library for handling Win32 map files
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method map-library-breakdown
    (filename :: <string>, #key count)
 => (results :: false-or(<sequence>))
  if (file-exists?(filename))
    with-open-file (stream = filename, direction: #"input")
      map-library-breakdown(stream, count: count)
    end
  end
end method map-library-breakdown;

// define variable *results* = #f;

define class <library-info> (<object>)
  slot library-info-name :: <symbol>,
    required-init-keyword: name:;
  slot library-info-size :: <integer>,
    required-init-keyword: size:;
  virtual slot library-info-title :: <string>;
end class <library-info>;

define method library-info-title 
    (library :: <library-info>) => (name :: <string>)
  as(<string>, library-info-name(library))
end method library-info-title;

define method map-library-breakdown
    (stream :: <stream>, #key count)
 => (results :: false-or(<sequence>))
  let results = make(<table>);
  // *results* := results;
  let last-key = #f;
  let last-address = 0;
  let count = 0;
  local method record-result 
            (line :: <string>, key :: false-or(<symbol>), 
	     address :: <integer>)
	 => ()
          if (key)
	    let old-value = element(results, key, default: #f);
            unless (old-value)
              // debug-message("  Found new key %s in %s", key, line);
	      results[key] := 0
            end;
          end;
          let size = address - last-address;
          if (size >= 0)
            if (last-key)
              results[last-key] := results[last-key] + size
            end
          else
            // debug-message("Ignoring negative size %d: %s", size, line)
          end;
          last-key := key;
          last-address := address;
        end method record-result;
  block (return)
    while (#t)
      count := count + 1;
      if (modulo(count, 10) == 0)
        format-out(".")
      end;
      let line = read-line(stream, on-end-of-stream: #f);
      unless (line) return() end;
      let (library, name, address) = parse-map-line(line);
      ignore(name);
      case
	library & address =>
	  let key = as(<symbol>, library);
	  record-result(line, key, address);
        address =>
          record-result(line, #f, address);
        otherwise =>
          // Finish once we get to the static symbols
          if (subsequence-position(line, "Static symbols"))
            return()
          end;
          // debug-message("Ignored line: %s", line);
      end;
      if (count)
	count := count - 1;
	if (count < 0)
	  return()
	end
      end
    end
  end;
  format-out("\n");
  let keys = key-sequence(results);
  map(method (key)
	make(<library-info>, name: key, size: results[key])
      end,
      keys)
end method map-library-breakdown;

define method parse-map-line
    (line :: <string>)
 => (library :: false-or(<string>), name :: false-or(<string>), 
     address :: false-or(<integer>))
  block (return)
    let current-position = 0;
    let last-position = size(line);
    local method skip-whitespace () => ()
            while (current-position < last-position & line[current-position] == ' ')
              current-position := current-position + 1
            end
          end method skip-whitespace;
    local method next-word () => (word :: <string>)
            skip-whitespace();
            if (current-position == last-position)
              return(#f, #f, #f)
            end;
            let start-position = current-position;
            while (current-position < last-position & line[current-position] ~== ' ')
              current-position := current-position + 1
            end;
            copy-sequence(line, start: start-position, end: current-position)
          end method next-word;
    local method skip-word () => ()
            skip-whitespace();
            if (current-position == last-position)
              return(#f, #f, #f)
            end;
            while (current-position < last-position & line[current-position] ~== ' ')
              current-position := current-position + 1
            end;
          end method skip-word;
    local method next-integer () => (integer :: <integer>)
            skip-whitespace();
            if (current-position == last-position)
              return(#f, #f, #f)
            end;
	    let (integer, next-key)
	      = string-to-integer(line, start: current-position, base: 16, default: -1);
	    if (integer < 0 | next-key >= last-position | line[next-key] ~= ' ')
              return(#f, #f, #f)
            end;
            current-position := next-key;
            integer
          end method next-integer;
    skip-word();
    skip-word();
    // let name = next-word();
    let full-address = next-integer();
    current-position := min(last-position, current-position + 3);
    let object-file = next-word();
    let colon-position = position(object-file, ':');
    let library
      = if (colon-position)
          copy-sequence(object-file, end: colon-position)
        end;
    values(library, #f, full-address)
  end
end method parse-map-line;

define method print-library-breakdown
    (filename :: <string>, #key count)
 => (results :: <sequence>)
  let results = map-library-breakdown(filename, count: count);
  results & print-results(results);
  results
end method print-library-breakdown;

define method library-size-greater-than
    (info1 :: <library-info>, info2 :: <library-info>)
 => (greater-than? :: <boolean>)
  library-info-size(info1) > library-info-size(info2)
end method library-size-greater-than;

define method library-title-less-than
    (info1 :: <library-info>, info2 :: <library-info>)
 => (less-than? :: <boolean>)
  library-info-title(info1) < library-info-title(info2)
end method library-title-less-than;

define method print-results
    (results :: <sequence>,
     #key sort-function = library-size-greater-than)
 => ()
  let maximum-title-length
    = begin
        let max-size = 0;
        for (library in results)
          max-size := max(max-size, size(library-info-title(library)))
        end;
        max-size
      end;
  let maximum-size-length = 12;
  let sorted-results = sort(results, test: sort-function);
  for (library-info in sorted-results)
    let library-name = library-info-title(library-info);
    let library-size = library-info-size(library-info);
    let library-size-text = integer-to-string(library-size);
    let whitespace-count
      = maximum-title-length - size(library-name)
      + maximum-size-length  - size(library-size-text);
    format-out("%s%s%s\n",
               library-name,
               make(<string>, size: whitespace-count),
               library-size-text)
  end;
  format-out("---\n");
  let total = 0;
  for (library-info in results)
    total := total + library-info-size(library-info)
  end;
  let total-size-text = integer-to-string(total);
  let whitespace-count
    = maximum-title-length - size("Total:")
    + maximum-size-length  - size(total-size-text);
  format-out("Total:%s%s",
             make(<string>, size: whitespace-count),
             total-size-text);
end method print-results;


/// Library groupings

define constant $windows-libraries
  = #(#"advapi32", #"comctl32", #"comdlg32", #"gdi32", #"kernel32", #"libcmt",
      #"mmdw", #"user32", #"win32main");

define constant $dylan-runtime-libraries
  = #(#"c-ffi", #"dylan", #"equal-table", #"functional-dylan",
      #"functional-extensions", #"machine-word", #"pentium-run-time",
      #"threads", #"run-time", #"gc");

define constant $standard-libraries
  = #(#"byte-vector", #"date", #"file-system", #"transcendentals", #"streams",
      #"standard-io-streams", #"standard-io", #"print", #"operating-system",
      #"format", #"format-out", #"set");

define constant $win32-ffi-libraries
  = #(#"win32-common", #"win32-controls", #"win32-dde", #"win32-dialog",
      #"win32-gdi", #"win32-kernel", #"win32-rich-edit", #"win32-registry",
      #"win32-user");

define constant $duim-libraries
  = #(#"duim", #"duim-core", #"duim-dcs", #"duim-extended-geometry",
      #"duim-frames", #"duim-gadget-panes", #"duim-gadgets", #"duim-geometry",
      #"duim-graphics", #"duim-layouts", #"duim-silica", #"duim-utilities", 
      #"win32-duim");

define method library-group
    (library :: <library-info>) => (group :: <string>)
  let name = library-info-name(library);
  library-group(name)
end method library-group;

define method library-group
    (name :: <symbol>) => (group :: <string>)
  case
    member?(name, $windows-libraries)       => "Windows glue";
    member?(name, $dylan-runtime-libraries) => "Dylan runtime";
    member?(name, $standard-libraries)      => "Standard libraries";
    member?(name, $win32-ffi-libraries)     => "Win32 FFI";
    member?(name, $duim-libraries)          => "DUIM";
    otherwise                               => "User";
  end
end method library-group;

define method total-memory-size
    (libraries :: <sequence>) => (total :: <integer>)
  let total = 0;
  for (library-info in libraries)
    total := total + library-info-size(library-info)
  end;
  total
end method total-memory-size;

define method group-size-greater-than
    (group1 :: <sequence>, group2 :: <sequence>)
 => (size-greater-than? :: <boolean>)
  total-memory-size(second(group1)) > total-memory-size(second(group2))
end method group-size-greater-than;

define method group-title-less-than
    (group1 :: <sequence>, group2 :: <sequence>)
 => (title-less-than? :: <boolean>)
  first(group1) < first(group2)
end method group-title-less-than;

define method print-group-results
    (results :: <sequence>,
     #key sort-function = library-size-greater-than,
          group-sort-function = group-size-greater-than,
          summary?)
 => ()
  let maximum-title-length
    = begin
        let max-size = 0;
        for (library in results)
          max-size := max(max-size, size(library-info-title(library)))
        end;
        max-size
      end;
  let maximum-size-length = 12;
  let table = make(<table>);
  for (library-info in results)
    let group = library-group(library-info);
    table[group] := element(table, group, default: make(<stretchy-vector>));
    add!(table[group], library-info)
  end;
  let ordered-keys = sort(key-sequence(table), test: \<);
  let groups
    = map(method (key)
            vector(key, table[key])
          end,
          ordered-keys);
  let grand-total = total-memory-size(results);
  let sorted-groups = sort(groups, test: group-sort-function);
  for (group in sorted-groups)
    let group-name = first(group);
    let group-entries = second(group);
    let total = total-memory-size(group-entries);
    let percentage = round/(total * 1000, grand-total);
    let total-size-text = integer-to-string(total);
    if (summary?)
      let whitespace-count
	= maximum-title-length - size(group-name)
	+ maximum-size-length  - size(total-size-text);
      format-out("%s%s  %s (%s%d.%d%%) \n",
                 group-name,
		 make(<string>, size: whitespace-count),
		 total-size-text,
                 if (percentage < 100) " " else "" end,
		 floor/(percentage, 10),
		 modulo(percentage, 10))
    else
      format-out("%s\n", group-name);
      let sorted-results = sort(group-entries, test: sort-function);
      for (library-info in sorted-results)
	let library-name = library-info-title(library-info);
	let library-size = library-info-size(library-info);
	let library-size-text = integer-to-string(library-size);
	let whitespace-count
	  = maximum-title-length - size(library-name)
	  + maximum-size-length  - size(library-size-text);
	format-out("  %s%s%s\n",
		   library-name,
		   make(<string>, size: whitespace-count),
		   library-size-text)
      end;
      let whitespace-count
	= maximum-title-length - size("Total:")
	+ maximum-size-length  - size(total-size-text);
      format-out("  ----\n");
      format-out("  Total:%s%s (%s%d.%d%%) \n\n",
		 make(<string>, size: whitespace-count),
		 total-size-text,
                 if (percentage < 100) " " else "" end,
		 floor/(percentage, 10),
		 modulo(percentage, 10))
    end;
  end;
  let total-size-text = integer-to-string(grand-total);
  let whitespace-count
    = maximum-title-length - size("Grand Total:")
    + maximum-size-length  - size(total-size-text);
  format-out("----\n");
  format-out("Grand Total:%s  %s (100%%)\n",
             make(<string>, size: whitespace-count),
             total-size-text)
end method print-group-results;
