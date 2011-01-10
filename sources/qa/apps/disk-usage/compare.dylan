Module:    disk-usage
Synopsis:  Compare directories disk usage
Author:    Carl Gay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $default-tolerance :: <integer>
  = 10;  // i.e., 10% change from comparison benchmark

define thread variable *tolerance* :: <integer> = $default-tolerance;

// How many "most increased" or "most decreased" files to display.
//
define constant $default-top-n :: <integer> = 10;

// If true, then treat HD redistributable files specially in comparisons.
// e.g., d2dylan.{dll,lib,defs} = dxdylan.{dll,lib,defs}
//
define thread variable *hd-directory?* :: <boolean> = #t;

define class <comparison-result> (<object>)
  constant slot result-info1 :: false-or(<file-info>),
    required-init-keyword: #"info1";
  constant slot result-info2 :: false-or(<file-info>),
    required-init-keyword: #"info2";
  constant slot result-byte-diff :: false-or(<integer>),
    required-init-keyword: #"byte-diff";
  constant slot result-percent-diff :: false-or(<float>),
    required-init-keyword: #"percent-diff";
  constant slot result-subresults :: <sequence> = make(<stretchy-vector>),
    init-keyword: #"subresults";
end class <comparison-result>;

define method above-tolerance-limit?
    (result :: <comparison-result>) => (within? :: <boolean>)
  result.result-percent-diff
  & abs(result.result-percent-diff) >= *tolerance*
end method above-tolerance-limit?;

// These next two are used as sort test functions

define method result-byte-diff-greater?
    (r1 :: <comparison-result>, r2 :: <comparison-result>)
 => (greater? :: <boolean>)
  r1.result-byte-diff
  & r2.result-byte-diff
  & r1.result-byte-diff > r2.result-byte-diff
end method result-byte-diff-greater?;

define method result-name-less?
    (r1 :: <comparison-result>, r2 :: <comparison-result>)
 => (less? :: <boolean>)
  r1.result-byte-diff
  & r2.result-byte-diff
  & string-less?(r1.result-info1.file-name, r2.result-info1.file-name)
end method result-name-less?;

define class <directory-comparison> (<object>)
  // The root comparison result.  Contains a <dir-info> for both
  // directories being compared.
  constant slot dc-root :: <comparison-result>,
    required-init-keyword: #"root";
  constant slot dc-top-n :: <integer> = $default-top-n,
    init-keyword: #"top-n";

  // These four contain <comparison-result> objects.
  // These are kept sorted at all times, with smallest in magnitude last.
  slot dc-most-increased-files :: <deque> = make(<deque>);
  slot dc-most-decreased-files :: <deque> = make(<deque>);
  slot dc-most-increased-dirs :: <deque> = make(<deque>);
  slot dc-most-decreased-dirs :: <deque> = make(<deque>);

  // These two contain <file-info> objects.
  constant slot dc-missing-files1 :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot dc-missing-files2 :: <stretchy-vector> = make(<stretchy-vector>);

end class <directory-comparison>;

// Maybe add a <comparison-result> to the list of most increased or most
// decreased in size.  This goes by absolute byte size differences rather 
// than percentage change differences, since the latter can be huge for
// small (and therefore uninteresting) files.  This is only called if
// result has two valid subresults, not if one is #f.  Directory and file
// totals are kept separately.
//
define method maybe-add-to-top
    (dircomp :: <directory-comparison>, result :: <comparison-result>)
 => ()
  local method doit (tops :: <deque>) => (tops :: <deque>)
          if (zero?(result.result-byte-diff))
            tops
          else
            let greater?
              = method (a :: <comparison-result>, b :: <comparison-result>)
                  abs(a.result-byte-diff) > abs(b.result-byte-diff)
                end;
            if (size(tops) < dircomp.dc-top-n)
              push(tops, result);
              sort!(tops, test: greater?)
            else
              block (continue)
                for (i from 0 below size(tops))
                  if (greater?(result, tops[i]))
                    pop-last(tops);  // pop smallest element
                    push(tops, result);
                    continue(sort!(tops, test: greater?));
                  end if;
                end for;
                tops
              end block
            end if
          end if
        end method doit;
  let info1 = result.result-info1;
  if (info1 & result.result-info2)
    if (byte-difference(result) > 0)
      if (instance?(info1, <dir-info>))
        dircomp.dc-most-increased-dirs := doit(dircomp.dc-most-increased-dirs);
      else
        dircomp.dc-most-increased-files := doit(dircomp.dc-most-increased-files);
      end;
    elseif (byte-difference(result) < 0)
      if (instance?(info1, <dir-info>))
        dircomp.dc-most-decreased-dirs := doit(dircomp.dc-most-decreased-dirs);
      else
        dircomp.dc-most-decreased-files := doit(dircomp.dc-most-decreased-files);
      end;
    end if;
  end if;
end method maybe-add-to-top;

define method display-missing-files
    (dc :: <directory-comparison>, dir1 :: <string>, dir2 :: <string>,
     sort-function :: false-or(<function>))
 => ()
  // files missing from dir1
  let files1 = (sort-function
                & sort(dc.dc-missing-files1, test: sort-function))
               | dc.dc-missing-files1;
  // files missing from dir2
  let files2 = (sort-function
                & sort(dc.dc-missing-files2, test: sort-function))
               | dc.dc-missing-files2;
  local method sum (files)
          let total :: <integer> = 0;
          do(method (info)
               ~instance?(info, <dir-info>) & (total := total + info.file-size);
             end,
             files);
          kbytes(total)
        end method;
  if (size(files1) > 0)
    format-out("\n%s has %d new files not in %s\ntotalling %s bytes.\n",
               dir2, size(files1), dir1, sum(files1));
    if (*report* == #"detailed")
      format-out("\n");
      do(print-file-info, files1);
    end if;
  end if;
  if (size(files2) > 0)
    format-out("\n%s is missing %d old files from %s\ntotalling %s bytes.\n",
               dir2, size(files2), dir1, sum(files2));
    if (*report* == #"detailed")
      format-out("\n");
      do(print-file-info, files2);
    end if;
  end if;
end method display-missing-files;

define method display-top-level-subdirectories
    (dc :: <directory-comparison>, sort-function :: false-or(<function>))
 => ()
  let tops = choose(method (x :: <comparison-result>)
		      let info = x.result-info1 | x.result-info2;
		      instance?(info, <dir-info>)
		    end method,
		    dc.dc-root.result-subresults);
  if (size(tops) > 0)
    format-out("Top level subdirectories:\n\n");
    print-comparison-header();
    do(print-comparison-result,
       (sort-function & sort!(tops, test: sort-function)) | tops);
    format-out("\n");
  end if;
end method display-top-level-subdirectories;

define method display-top-n
    (dc :: <directory-comparison>, sort-function :: false-or(<function>))
 => ()
  let n = dc.dc-top-n;
  let tops = list(dc.dc-most-increased-dirs, dc.dc-most-decreased-dirs,
                  dc.dc-most-increased-files, dc.dc-most-decreased-files);
  let headers = list("increased", "decreased", "increased", "decreased");
  let types = list("directories", "directories", "files", "files");
  for (top in tops,
       header in headers,
       type in types)
    format-out("\nTop %d %s most %s in size:\n\n",
               n, type, header);
    if (size(top) > 0)
      print-comparison-header();
      for (result in (sort-function & sort!(top, test: sort-function)) | top)
        print-comparison-result(result);
      end for;
    else
      format-out("  None\n");
    end if;
  end for;
  format-out("\n");
end method display-top-n;  

define method display-changed-files
    (dc :: <directory-comparison>, sort-function :: false-or(<function>))
 => ()
  let results = make(<stretchy-vector>);
  local method maybe-collect-result (result, dirs?) => ()
          let info = result.result-info1 | result.result-info2;
          if ((dirs? & instance?(info, <dir-info>))
              | (~dirs? & ~instance?(info, <dir-info>)))
            if (above-tolerance-limit?(result))
              if (info.file-locator = as(<directory-locator>, "bin/"))
               // break("%=", info);
              end if;
              add!(results, result);
            end if;
          end if;
        end method;
  local method display (kind :: <string>, dirs? :: <boolean>) => ()
          format-out("\n%s changed by more than %%%s:\n\n", kind, *tolerance*);
          print-comparison-header();
          do-comparison-results(dc.dc-root, rcurry(maybe-collect-result, dirs?));
          do(print-comparison-result, (sort-function
                                       & sort!(results, test: sort-function))
                                      | results);
        end method;
  display("Directories", #t);
  size(results) := 0;
  *report* == #"detailed" & display("Files", #f);
end method display-changed-files;

// Compare two directories.  This is intended for comparison of the Functional Developer
// installation directories from release to release, so it has special knowledge of
// how to compare redistributable DLLs.  e.g., d2dylan.dll compares to d3dylan.dll.
// This is only active if hd? = #t.
// 
// If dir1 or dir2 is of type ".log" then it's assumed to be a log file created
// by report-disk-usage.  Otherwise it's treated as a directory and any trailing
// filename is stripped.  e.g., c:\foo\bar.txt becomes c:\foo\
// ---*** Fix this when switching to locators.
//
define method compare-directories
    (info1 :: <dir-info>, info2 :: <dir-info>,
     #key tolerance :: <integer> = *tolerance*,
          report :: <report-type> = #"brief",
          hd? :: <boolean>,                             // Comparing HD releases?
          top :: <integer> = $default-top-n,
          sort-by :: one-of(#"name", #"size", #"none") = #"size")
 => ()
  let dir1 = info1.file-name;
  let dir2 = info2.file-name;
  report ~== #"none"
    & format-out("Comparing %s (old) to %s (new)...\n", dir1, dir2);
  let subresults = compare-info(info1, info2);
  let result = create-comparison-result(info1, info2, subresults: subresults);
  dynamic-bind(*tolerance* = tolerance,
	       *report* = report,
	       *hd-directory?* = hd?)
    generate-report(make(<directory-comparison>, top: top, root: result),
		    dir1, dir2, sort-by);
  end;
end method compare-directories;

define method generate-report
    (dc :: <directory-comparison>, dir1 :: <string>, dir2 :: <string>,
     sort-by :: one-of(#"name", #"size", #"none"))  // size is a misnomer but...
 => ()
  local method collect-totals (result) => ()
          maybe-add-to-top(dc, result);
          if (~result.result-info1)
            add!(dc.dc-missing-files1, result.result-info2);
          elseif (~result.result-info2)
            add!(dc.dc-missing-files2, result.result-info1);
          end if;
        end method collect-totals;
  let sort-function = select (sort-by)
			#"name" => result-name-less?;
			#"size" => result-byte-diff-greater?;
                        #"none" => #f;
		      end;
  do-comparison-results(dc.dc-root, collect-totals);
  display-top-level-subdirectories(dc, sort-function);
  display-top-n(dc, select (sort-by)
                      #"name" => result-name-less?;
                      #"size", #"none" => #f;  // they're already sorted by size
                    end select);
  display-changed-files(dc, sort-function);
  display-missing-files(dc, dir1, dir2, select (sort-by)
					  #"name" => info-name-less?;
					  #"size" => info-size-greater?;
                                          #"none" => #f;
					end);
end method generate-report;

define function do-comparison-results
    (result :: <comparison-result>, f :: <function>) => ()
  f(result);
  do(method (res)
       do-comparison-results(res, f);
     end method,
     result.result-subresults);
end function do-comparison-results;

// ---*** Need locators...
//
define function line-ends-with
    (line :: <string>, str :: <string>) => (b :: <boolean>)
  block (return)
    let llen = size(line);
    let slen = size(str);
    if (slen > llen)
      return(#f)
    end if;
    for (l from llen - 1 to 0 by -1,
         s from slen - 1 to 0 by -1)
      if (~char-equal?(line[l], str[s]))
        return(#f);
      end if;
    end for;
    #t
  end block
end function line-ends-with;

define function pad
    (text :: <string>, columns :: <integer>, align-left? :: <boolean>)
 => (s :: <string>)
  let len = size(text);
  if (len > columns)
    text
  else
    let filler = make(<string>, size: columns - len, fill: ' ');
    if (align-left?)
      concatenate(text, filler)
    else
      concatenate(filler, text)
    end if
  end if
end function pad;

define method print-comparison-line
    (name :: <string>, bytes1 :: <string>, bytes2 :: <string>,
     byte-diff :: <string>, percent-change :: <string>) => ()
  format-out("  %s %s %s %s %s\n",
	     pad(name, 45, #t),
	     pad(bytes1, 10, #f),
	     pad(bytes2, 10, #f),
	     pad(percent-change, 10, #f),
	     pad(byte-diff, 10, #f));
end method print-comparison-line;

define method print-comparison-header
    () => ()
  print-comparison-line("Name", "Size1", "Size2", "Byte-diff", "%-diff");
  print-comparison-line("----", "-----", "-----", "---------", "------");
end method print-comparison-header;

define method print-comparison-result
    (result :: <comparison-result>) => ()
  let info1 = result.result-info1;
  let info2 = result.result-info2;
  assert(info1 | info2, "no info to display!");
  if (info1 & info2)
    print-comparison-line(info1.file-name,
			  kbytes(info1.file-size),
			  kbytes(info2.file-size),
			  kbytes(result.result-byte-diff, sign?: #t),
			  float->percentage(result.result-percent-diff));
  else
    let info = info1 | info2;
    print-comparison-line(info.file-name,
			  if (info1) kbytes(info1.file-size) else "N/A" end,
			  if (info2) kbytes(info2.file-size) else "N/A" end,
			  "N/A", "N/A");
  end if;
end method print-comparison-result;

define method byte-difference
    (result :: <comparison-result>) => (diff :: <integer>)
  file-size(result-info2(result)) - file-size(result-info1(result))
end method byte-difference;

// ---*** This can return "%xx.100", which is nonsensical.
define method float->percentage
    (f :: <float>) => (s :: <string>)
  let sign = if (f < 0) "-" else "+" end;
  let (int1, ff) = floor(abs(f));
  let int2 = abs(round(ff * 100));
  if (int1 = 0 & int2 = 0)
    sign := "";
  end if;
  format-to-string("%s%d.%s%%", sign, int1, integer-to-string(int2, size: 2))
end method float->percentage;

define method hd-redistributable-file?
    (file :: <locator>) => (hd? == #f)
  #f
end method hd-redistributable-file?;

define method hd-redistributable-file?
    (file :: <file-locator>) => (hd? :: <boolean>)
  let base = locator-base(file);
  let type = locator-extension(file);
  size(base) >= 3
    & (type = "dll" | type = "dbg" | type = "defs" | type = "lib")
    & char-equal?(base[0], 'd')
    & (char-equal?(base[1], 'x') | digit-char?(base[1]))
end method hd-redistributable-file?;

// Both objects must be files for this comparison to be meaningful
define method hd-file-locator-equal?
    (file1 :: <locator>, file2 :: <locator>) => (equal? == #f)
  #f
end method hd-file-locator-equal?;

// Two filenames are considered equal if they look like HD redistributable
// files and only differ in the release number
define method hd-file-locator-equal?
    (file1 :: <file-locator>, file2 :: <file-locator>) => (equal? :: <boolean>)
  if (locator-directory(file1) ~= locator-directory(file2))
    #f
  else
    hd-redistributable-file?(file1)
    & hd-redistributable-file?(file2)
    & begin
	let base1 = locator-base(file1);
	let type1 = locator-extension(file1);
	let base2 = locator-base(file2);
	let type2 = locator-extension(file2);
	type1 ~= type2
	  & (base1 = base2
	       | (size(base1) = size(base2)
		    & char-equal?(base1[0], base2[0])
		    & every?(char-equal?,
			     copy-sequence(base1, start: 2),
			     copy-sequence(base2, start: 2))))
      end
  end
end method hd-file-locator-equal?;

// Regular expressions would be nice here.  For now do it by hand.
//
define method file-info-equal?
    (info1 :: <file-info>, info2 :: <file-info>) => (equal? :: <boolean>)
  info1.file-locator = info2.file-locator       // ---*** OS-specific locator comparison
    | (*hd-directory?*                          // Comparing the HD release directories....
	 & hd-file-locator-equal?(info1.file-locator, info2.file-locator))
end method file-info-equal?;

define method file-info-less?
    (info1 :: <file-info>, info2 :: <file-info>) => (less? :: <boolean>)
  local method fixup-hd (file :: <locator>) => (fixed :: <locator>)
	  if (hd-redistributable-file?(file))
	    let base = copy-sequence(locator-base(file));
	    base[1] := '@';
	    make(<file-locator>, 
		 directory: locator-directory(file),
		 base: base,
		 extension: locator-extension(file))
	  else
	    file
	  end
	end method;
  if (*hd-directory?*)
    if (hd-redistributable-file?(info1.file-locator)
	  | hd-redistributable-file?(info2.file-locator))
      let file1 = fixup-hd(info1.file-locator);
      let file2 = fixup-hd(info2.file-locator);
      string-less?(as(<string>, file1), as(<string>, file2))
    else
      string-less?(info1.file-name, info2.file-name)
    end
  else
    string-less?(info1.file-name, info2.file-name)
  end if
end method file-info-less?;

// This and compare-directory-data just build up a tree of <comparison-result>
// objects.  The main result of this is that files/directories that don't have
// a corresponding match are paired up with #f.  Byte size differences and
// percentage change are calculated during this pass also.
//
// ---*** This should return a second value, "any-differences?", so that the
//        display code can just say "No differences encounted." if necessary.
//
define method compare-info
    (info1 :: false-or(<file-info>), info2 :: false-or(<file-info>))
 => (subresults :: <sequence>)
  let info1-dir? = instance?(info1, <dir-info>);
  let info2-dir? = instance?(info2, <dir-info>);
  case
    info1-dir? & info2-dir? =>
      compare-directory-data(info1.directory-data, info2.directory-data);
    info1-dir? =>
      compare-directory-data(info1.directory-data, #[]);
    info2-dir? =>
      compare-directory-data(#[], info2.directory-data);
    otherwise =>
      #[];
  end case
end method compare-info;

define method compare-directory-data
    (subresults1 :: <sequence>, subresults2 :: <sequence>)
 => (comp-results :: <sequence>)
  let subresults1 = sort(subresults1, test: file-info-less?);
  let subresults2 = sort(subresults2, test: file-info-less?);
  let comp-results = make(<stretchy-vector>);
  let size1 = subresults1.size;
  let size2 = subresults2.size;
  let index1 :: <integer> = 0;
  let index2 :: <integer> = 0;
  while (index1 < size1 & index2 < size2)
    let subresult1 :: <file-info> = subresults1[index1];
    let subresult2 :: <file-info> = subresults2[index2];
    case
      file-info-equal?(subresult1, subresult2) =>
	add!(comp-results, create-comparison-result(subresult1, subresult2));
	index1 := index1 + 1;
	index2 := index2 + 1;
      file-info-less?(subresult1, subresult2) =>
	add!(comp-results, create-comparison-result(subresult1, #f));
	index1 := index1 + 1;
      otherwise =>
	add!(comp-results, create-comparison-result(#f, subresult2));
	index2 := index2 + 1;
    end;
  end;
  for (index from index1 below size1)
    add!(comp-results, create-comparison-result(subresults1[index], #f))
  end;
  for (index from index2 below size2)
    add!(comp-results, create-comparison-result(#f, subresults2[index]))
  end;
  comp-results
end method compare-directory-data;

define method create-comparison-result
    (info1 :: false-or(<file-info>), info2 :: false-or(<file-info>),
     #key subresults :: false-or(<sequence>))
 => (comp-result :: <comparison-result>)
  let subresults = subresults | compare-info(info1, info2);
  let byte-diff = info1 & info2 & info2.file-size - info1.file-size;
  let percent-diff = byte-diff & (100 * (as(<float>, byte-diff) / info1.file-size));
  // The above expression can yield -0.0 if byte-diff and info1.file-size are
  // both zero.  That causes problems later, so canonicalize to +0.0   oy.
  if (percent-diff & zero?(percent-diff))
    percent-diff := 0.0;
  end if;
  make(<comparison-result>,
       info1: info1,
       info2: info2,
       byte-diff: byte-diff,
       percent-diff: percent-diff,
       subresults: subresults)
end method create-comparison-result;


/***
 *** String manipulation.
 ***
 *** The code below was nicked from D-duim-utilities!strings.dylan
 ***/

define inline function range-check
    (sequence :: <sequence>, _size :: <integer>, _start :: <integer>, _end :: <integer>) => ()
  when (_start < 0 | _start > _size)
    error("element out of range")
  end;
  when (_end < 0 | _end > _size)
    error("element out of range")
  end
end function range-check;

define inline function upper-case-code?
    (code :: <integer>) => (true? :: <boolean>)
  as(<integer>, 'A') <= code & code <= as(<integer>, 'Z')
end function upper-case-code?;

define inline function lower-case-code?
    (code :: <integer>) => (true? :: <boolean>)
  as(<integer>, 'a') <= code & code <= as(<integer>, 'z')
end function lower-case-code?;

/// Case-insensitive character comparisons

define method char-equal?
    (char1 :: <byte-character>, char2 :: <byte-character>)
 => (true? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  code1 == code2
  | (zero?(logand(#o337, logxor(code1, code2)))
     & (upper-case-code?(code1) | lower-case-code?(code1))
     & (upper-case-code?(code2) | lower-case-code?(code2)))
end method char-equal?;

define method string-equal? 
    (string1 :: <byte-string>, string2 :: <byte-string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = size(string1),
	  start2 :: <integer> = 0, end2 :: <integer> = size(string2))
 => (true? :: <boolean>)
  range-check(string1, size(string1), start1, end1);
  range-check(string2, size(string2), start2, end2);
  block (return)
    end1 - start1 = end2 - start2
    & //without-bounds-checks
	for (i :: <integer> from start1 below end1,
	     j :: <integer> from start2 below end2)
	  let char1 :: <byte-character> = string1[i];
	  let char2 :: <byte-character> = string2[j];
	  unless (char-equal?(char1, char2))
	    return(#f)
	  end;
	finally
	  return(#t);
	end for
      // end 
  end block
end method string-equal?;

define method string-less?
    (string1 :: <byte-string>, string2 :: <byte-string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = size(string1),
	  start2 :: <integer> = 0, end2 :: <integer> = size(string2))
 => (true? :: <boolean>)
  let length1 = end1 - start1;
  let length2 = end2 - start2;
  range-check(string1, size(string1), start1, end1);
  range-check(string2, size(string2), start2, end2);
  let result = string-compare(string1, start1, 
			      string2, start2, min(length1, length2));
  if (result = 0)
    length1 < length2
  else
    result < 0
  end
end method string-less?;

define method char-less?
    (char1 :: <byte-character>, char2 :: <byte-character>)
 => (true? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  when (lower-case-code?(code1))
    code1 := logxor(code1, #o40)
  end;
  when (lower-case-code?(code2))
    code2 := logxor(code2, #o40)
  end;
  code1 < code2
end method char-less?;

define method string-compare
    (string1 :: <byte-string>, start1 :: <integer>, 
     string2 :: <byte-string>, start2 :: <integer>, count :: <integer>)
 => (result :: <integer>)
  let subrange1 = size(string1) - start1;
  let subrange2 = size(string2) - start2;
  let state = 0;
  case
    count > subrange1 =>
      case
	count > subrange2 =>
	  count := min(subrange1, subrange2);
	  state := 1;
	otherwise =>
	  count := subrange1;
	  state := 2
      end;
    count > subrange2 =>
      count := subrange2;
      state := 3
  end;
  block (return)
    //without-bounds-checks
      for (i1 :: <integer> = start1 then i1 + 1,
	   i2 :: <integer> = start2 then i2 + 1,
	   until: count = 0)
	let char1 :: <byte-character> = string1[i1];
	let char2 :: <byte-character> = string2[i2];
	unless (char-equal?(char1, char2))
	  return(if (char-less?(char1, char2))
		   (start1 - i1) - 1
		 else
		   (i1 + 1) - start1
		 end)
	end;
	count := count - 1;
      finally
	select (state)
	  0 => 0;
	  1 => case
		 subrange1 = subrange2 => 0;
		 subrange1 < subrange2 => -1 - i1;
		 otherwise => i1 + 1
	       end;
	  2 => (start1 - i1) - 1;
	  otherwise => (i1 - start1) + 1
	end
      end
    //end
  end
end method string-compare;

define method digit-char?
    (char :: <byte-character>, #key radix = 10) => (true? :: <boolean>)
  let code = as(<integer>, char);
  (as(<integer>, '0') <= code & code <= as(<integer>, '9'))
  | (radix > 10 & radix < 36
     & ((code   >= as(<integer>, 'A') & code - as(<integer>, 'A') < radix - 10)
	| (code >= as(<integer>, 'a') & code - as(<integer>, 'a') < radix - 10)))
end method digit-char?;

