Module:    sectionizer
Synopsis:  Dylan sectionizer
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Sectionizer for Dylan source code

define class <source-container> (<object>)
  slot source-container-locator,
    required-init-keyword: locator:;
  slot source-container-sections = make(<stretchy-vector>);
  // The modification timestamp for the whole container
  slot source-container-timestamp = make(<date>, native-clock: -1),
    init-keyword: timestamp:;
end class <source-container>;

// Mapping from locators to <source-container> objects
define variable *source-containers* = make(<equal-table>);


define class <section> (<object>)
  slot section-identifier,
    required-init-keyword: identifier:;
  slot section-source-container,
    required-init-keyword: container:;
  // Start and end line number
  slot section-start-line,
    required-init-keyword: start-line:;
  slot section-end-line,
    required-init-keyword: end-line:;
  // The actual lines of the section
  slot section-lines,
    required-init-keyword: lines:;
  // The line in which the define word is contained
  slot section-defining-line = #f,
    init-keyword: defining-line:;
  // Modification and compilation timestamps for the section
  slot section-timestamp = make(<date>, native-clock: -2),
    init-keyword: timestamp:;
  slot section-compilation-timestamp = make(<date>, native-clock: -2),
    init-keyword: compilation-timestamp:;
end class <section>;


//--- Should be part of a File System library...
define open generic file-modification-time
    (pathname :: <locator>) => (time :: <integer>);

define method file-modification-time
    (pathname :: <locator>) => (time :: <integer>);
  file-property(pathname, #"write-date")
end method file-modification-time;

define open generic sectionize-file
    (locator) => (container :: <source-container>);

define method sectionize-file
    (locator :: <string>) => (container :: <source-container>)
  sectionize-file(as(<locator>, locator))
end method sectionize-file;

define method sectionize-file
    (locator :: <locator>) => (container :: <source-container>)
  block (return)
    let container
      = element(*source-containers*, locator, default: #f)
	| begin
            let new-container = make(<source-container>, locator: locator);
            *source-containers*[locator] := new-container
          end;
    let timestamp = file-property(locator, #"modification-date");
    when (timestamp <= source-container-timestamp(container))
      return(container)
    end;
    source-container-timestamp(container) := timestamp;
    let sections = make(<stretchy-vector>);
    let lines = make(<stretchy-vector>);
    let header-length = 0;
    // Read in the file header, then all the remaining lines
    with-open-file (stream = locator, direction: #"input")
      block (break)
	let (header, line) = read-header-section(container, stream);
	section-timestamp(header) := timestamp;
	add!(sections, header);
	header-length := size(section-lines(header));
	add!(lines, line);
	while (#t)
	  let line = read-line(stream, on-end-of-stream: #f);
	  if (line)
	    add!(lines, line)
	  else
	    break()
	  end
	end
      end
    end;
    // Now sectionize the rest of the lines in the file as follows:
    //  - Set _this-start_ to the first 'define' form
    //  - Loop doing
    //    - Look for the next 'define' form
    //    - Back up from the next 'define' form to the last
    //      non-blank, non-comment line
    //    - The section runs from _prev-start_ to that line
    //    - Set _prev-start_ to the next 'define' form
    let section-number = 0;
    let prev-end = 0;
    let this-start = find-define-line(lines, 0);
    unless (this-start)
      let section = make(<section>,
			 identifier: section-number,
			 container: container,
			 start-line: header-length,
			 end-line: size(lines) + header-length ,
			 lines: lines,
			 timestamp: timestamp);
      add!(sections, section);
      return(sections)
    end;
    while (#t)
      let next-start
	= find-define-line(lines, this-start + 1);
      let this-end
	= if (next-start) find-non-blank-line(lines, next-start - 1) else size(lines) end;
      let section = make(<section>,
			 identifier: section-number,
			 container: container,
                         //--- The start line is the 'define line'
			 start-line: this-start + header-length,
			 end-line: this-end + header-length,
			 lines: copy-sequence(lines, start: prev-end, end: this-end),
			 defining-line: lines[this-start],
			 timestamp: timestamp);
      add!(sections, section);
      section-number := section-number + 1;
      if (next-start)
	this-start := next-start;
        prev-end := this-end
      else
        source-container-sections(container)
	  := merge-sections(source-container-sections(container), sections);
	return(container)
      end
    end
  end
end method sectionize-file;

define method merge-sections
    (old-sections :: <sequence>, new-sections :: <sequence>)
 => (sections :: <sequence>)
  //---*** Implement this
  new-sections
end method merge-sections;  


// Read the header of a Dylan files, returning the header in a section
// and the text of the first non-header line
define method read-header-section
    (container, stream) => (section :: <section>, next-line)
  let lines = make(<stretchy-vector>);
  let synopsis = #f;
  let copyright = #f;
  block (return)
    while (#t)
      let line = read-line(stream);
      when (size(line) > 9
            & string-equal?(line, "Synopsis:", end1: 9))
        synopsis := line
      end;
      when (size(line) > 10
            & string-equal?(line, "Copyright:", end1: 10))
        copyright := line
      end;
      if (empty?(line))
	return(make(<section>,
		    identifier: #"header",
		    container: container,
		    start-line: 0,
		    end-line: size(lines) - 1,
		    lines: lines,
                    defining-line: synopsis | copyright | "Header"),
	       line)
      else
	add!(lines, line)
      end
    end
  end
end method read-header-section;


// Find a line starting with 'define'; returns the index of that line
define method find-define-line
    (lines :: <vector>, index :: <integer>) => (line :: false-or(<integer>))
  block (return)
    while (index < size(lines))
      let line = lines[index];
      when (size(line) > 6
	    & string-equal?(line, "define", end1: 6))
	return(index)
      end;
      index := index + 1
    end;
    #f
  end
end method find-define;

// Find a non-blank, non-comment line; returns the index of the next line
define method find-non-blank-line
    (lines :: <vector>, index :: <integer>) => (line :: false-or(<integer>))
  block (return)
    while (index >= 0)
      let line = lines[index];
      let length = size(line);
      let bp = find-key(line, method (ch) ~(ch = ' ' | ch = '\t') end, failure: length);
      when (length - bp > 2
            & ~string-equal?(line, "//", start1: bp, end1: bp + 2))
	return(index + 1)
      end;
      index := index - 1
    end;
    #f
  end
end method find-non-blank-line;


/// String matching utilities

define method string-equal? 
    (string1 :: <string>, string2 :: <string>,
     #key start1 = 0, end1, start2 = 0, end2) => (equal? :: <boolean>)
  block (return)
    unless (end1)
      end1 := size(string1)
    end;
    unless (end2)
      end2 := size(string2)
    end;
    end1 - start1 = end2 - start2
    & for (i from start1 below end1,
           j from start2 below end2)
        let char1 = string1[i];
        let char2 = string2[j];
        unless (char-equal?(char1, char2))
          return(#f)
        end;
        finally return(#t)
      end
  end
end method string-equal?;

define function upper-case-code? (code :: <integer>) => (upper-case? :: <boolean>)
  as(<integer>, 'A') <= code & code <= as(<integer>, 'Z')
end function upper-case-code?;

define function lower-case-code? (code :: <integer>) => (lower-case? :: <boolean>)
  as(<integer>, 'a') <= code & code <= as(<integer>, 'z')
end function lower-case-code?;

define method char-equal?
    (char1 :: <character>, char2 :: <character>) => (equal? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  code1 = code2
  | (zero?(logand(#o337, logxor(code1, code2)))
     & (upper-case-code?(code1) | lower-case-code?(code1))
     & (upper-case-code?(code2) | lower-case-code?(code2)))
end method char-equal?;
