module: dfmc-debug
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <tagger> (<object>)
  constant slot tagger-output-stream :: <stream>,
    required-init-keyword: stream:;
  constant slot tagger-stream :: <string-stream>
    = make(<string-stream>, direction: #"output",
	   contents: make(<byte-string>, size: 10000));
  constant slot tagger-table :: <object-table>
    = make(<object-table>);
  constant slot tagger-sequence :: <simple-object-vector>
    = make(<simple-object-vector>, size: 1);
  slot tagger-record :: false-or(<source-record>) = #f;
  slot tagger-header-lines :: <integer> = 0;
  slot tagger-header-chars :: <integer> = 0;
end;

define function make-tags (lib-desc, output-file, #key personal?, recursive?)
  let ld = lookup-library-description(lib-desc);
  with-open-file (stream = output-file, direction: #"output")
    let tagger = make(<tagger>, stream: stream);
    dynamic-bind (*check-source-record-date?* = #f)
      library-tags(tagger, ld);
      when (recursive? | personal?)
	for (ld in all-used-library-descriptions(ld))
	  unless (personal? & ~library-description-personal?(ld))
	    library-tags(tagger, ld);
	  end unless;
	end for;
      end when;
    end dynamic-bind;
  end with-open-file;
end;

define function library-tags (tagger :: <tagger>, ld :: <library-description>)
  format-out("%s\n", ld.library-description-project);
  for (cr in ld.library-description-compilation-records)
    let sr :: <file-source-record> = cr.compilation-record-source-record;
    let path = sr.source-record-location;
    let pathname = as(<string>, path);
    format-out("   %s\n", pathname);
    tagger.tagger-record := cr.compilation-record-source-record;
    let (header, lines, chars) = read-file-header(path);
    ignore(header);
    tagger.tagger-header-lines := lines;
    tagger.tagger-header-chars := chars;
    file-tags(tagger, cr);
    let str = stream-contents(tagger.tagger-stream, clear-contents?: #t);
    // This writes CRLF's for \n!
    // format(tagger.tagger-output-stream, "\f\n%s,%d\n", pathname, size(str));
    begin
      format(tagger.tagger-stream, "\f\n%s,%d\n", pathname, size(str));
      write(tagger.tagger-output-stream,
	    stream-contents(tagger.tagger-stream, clear-contents?: #t));
    end;
    write(tagger.tagger-output-stream, str);
  end;
end;

define method file-tags (tagger :: <tagger>, cr :: <compilation-record>)
  for (form in cr.compilation-record-top-level-forms)
    if (~form.form-parent-form)
      form-tags(tagger, form);
    end;
  end;
end;

define method form-tags (tagger :: <tagger>, form :: <top-level-form>)
  let seq = tagger.tagger-sequence;
  seq[0] := form;
  var-tags(tagger, #f, form.form-source-location, seq);
end method;

define method form-tags (tagger :: <tagger>, form :: <macro-call-form>)
  var-tags(tagger, form, form.form-source-location, form.form-derived-forms)
end;

define method form-tags (tagger :: <tagger>, form :: <top-level-init-form>)
end;

define function var-tags (tagger :: <tagger>,
			  parent :: false-or(<macro-call-form>),
			  loc :: <source-location>,
			  forms :: <top-level-form-sequence>)
  debug-assert(loc.source-location-source-record == tagger.tagger-record,
	       "Wrong loc sr?");
  let defn? = parent == #f | form-define-word(parent);
  let locs = tagger.tagger-table;
  local method que (loc, line, name)
	  let name = name & as(<symbol>, name);
	  let col = loc & loc.source-location-end-column;
	  let old-locs = element(locs, line, default: #());
	  let old = any?(method (a) a.head == name & a end, old-locs);
	  if (old)
	    let old-col = old.tail;
            when (old-col & (col == #f | old-col < col)) old.tail := col end;
	  else
	    locs[line] := pair(pair(name, col), old-locs);
	  end;
	end;
  local method que-var (var)
	  let loc = var.fragment-source-location;
	  if (loc) que(loc, loc.source-location-end-line, var) end;
	end;
  for (form in forms)
    when (form.form-parent-form == parent)
      select (form by instance?)
	<top-level-init-form> => #f;
	<namespace-defining-form> =>
	  let name = form.form-namespace-name;
	  defn? := #t;
	  que(#f, loc.source-location-start-line, name);
	<variable-defining-form> =>
	  defn? := #t;
	  do(que-var, form.form-variable-names);
	  if (instance?(form, <class-definition>))
	    for (slotd in form.form-slot-specs)
	      que-var(slotd.form-variable-name);
	    end;
	  end;
      end;
    end;
  end;
  if (defn?)
    let sr = tagger.tagger-record;
    if (empty?(locs)) que(#f, loc.source-location-start-line, #f) end;
    let lines = sort(locs.key-sequence, test: \<);
    for (line in lines)
      let line-pos
	= source-offset-character-in(sr, make-source-offset(0, line, 0));
      for (a in locs[line])
	let name = a.head & as(<string>, a.head);
	let col = a.tail;
	tags-output-range(tagger, name, line, line-pos, col & (line-pos + col));
      end;
    end;
  end;
  remove-all-keys!(locs);
end;


define function tags-output-range (tagger :: <tagger>,
				   tag-name :: false-or(<string>),
				   start-line :: <integer>,
				   start-pos :: <integer>,
				   end-pos :: false-or(<integer>))
  let sr = tagger.tagger-record;
  let contents = sr.source-record-contents;
  let len = contents.size;
  let end-pos = if (end-pos & start-pos <= end-pos & end-pos <= len)
		  end-pos
		else
		  local method line (pos)
			  let ch = pos == len | contents[pos];
			  if (ch == #t | ch == 10 | ch == 13) pos
			  else line(pos + 1) end
			end;
		  line(start-pos);
		end;
  let end-pos = if (end-pos < len & begin
				      let ch = contents[end-pos];
				      ch == 32 | ch == 9
				    end)
		  end-pos + 1
		else
		  end-pos
		end;
  let stream = tagger.tagger-stream;
  write(stream, contents, start: start-pos, end: end-pos);
  write-element(stream, as(<character>, 127));
  when (tag-name)
    write(stream, tag-name);
    write-element(stream, as(<character>, 1));
  end;
  format(stream, "%d,%d\n",
	 tagger.tagger-header-lines + start-line,
	 tagger.tagger-header-chars + start-pos);
end;
