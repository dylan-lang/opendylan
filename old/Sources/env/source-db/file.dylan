Module: source-db
Language: infix-dylan
Author: Roman Budzianowski
Credits: John Dunning
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <db-file> (<db-object>)
  slot metafile :: <metafile> , required-init-keyword: metafile:;
  slot branch :: <branch>, required-init-keyword: branch:;
  slot version :: <integer>, init-value: 1, init-keyword: version:;
  slot author :: <author>, init-keyword: author:;
  slot comment, init-keyword: comment:;
  slot date;
  slot predecessor :: union(<db-file>, singleton(#f)), 
    required-init-keyword: predecessor:;
  slot sections :: <table>, init-function: method () make(<table>) end;
  slot section-sequence :: <list>;
end class;

define method find-file-sections(file :: <db-file>, search-string :: <string>)
  let found = #();
  let ss = as(<string>, as(<symbol>, search-string));
  for(s in file.section-sequence)
    if(subsequence-position(as(<string>, s.symbol-id), ss))
      format(*standard-output*, "found: %=\n", s.symbol-id);
      found := pair(s, found);
    end;
  end;
  found;
end;

define method print(f :: <db-file>, #key stream = #t, verbose? = #f)
  format(stream, "[db-file: %= ver: %=]", f.metafile.name, f.version);
end;

define method describe (file :: <db-file>, #key verbose? = #f)
  format(*standard-output*, "\nFile %= in %=:", file, file.branch);
  let old = file.predecessor;
  if(old)
    format(*standard-output*, "\nPredecessor: %= in %=\n", old, old.branch);
  end;

  if(verbose?)
    format(*standard-output*, "Sections:\n");
  end;
  for(key in file.sections.key-sequence)
    if(old & file.sections[key] == old.sections[key])
      if(verbose? == full:)
	format(*standard-output*, " ");
	describe(file.sections[key], verbose?: verbose?);
      end;
    else
      if(verbose?)
	format(*standard-output*, "*");
	describe(file.sections[key], verbose?: verbose?);
      end;
    end;
  end;
end;

define method dump-to-disk ( file :: <db-file> )

end;

define method setup-new-file-sections(new-file :: <db-file>, new-sections :: <list>)
  let old-file = new-file.predecessor;
  local method old-section (sid) 
	  if(old-file) old-file.sections[sid] else #f end 
	end;
  let modified = #();
  let sequence = #();
  for (sec in new-sections)
    let section-id = head(sec);
    let old = old-section(section-id);
    if(old & old.section-text = head(tail(tail(tail(sec)))))
      new-file.sections[section-id] := old;
      sequence := add(sequence, old);
      format(*standard-output*, "New and old sections identical: %=\n", section-id);
    else
      let new-sec = create-new-file-section(sec, old);
      modified := add(modified, new-sec);
      new-file.sections[section-id] := new-sec;
      sequence := add(sequence, new-sec);
      format(*standard-output*, "Updating or creating new section: %=\n", section-id);
    end;
  end;
  new-file.section-sequence := reverse(sequence);
  modified;
end;  


define class <metafile> (<branched-container>)
  slot pathname, required-init-keyword: pathname:;
  slot in-core, init-value: #f;
  slot the-sections :: <stretchy-vector>, init-function: curry(make, <stretchy-vector>);
  slot no-sections :: <integer>, init-value: 0;
// active branch specs
// newest-in-branch files
end class;

define method register-sections(meta :: <metafile>, sections :: <list>)
  for(s in sections, index from meta.no-sections by 1)
//    meta.the-sections[index] := s; bug in the emulator
  end;
  sections;
end;

define method reload-from-disk(meta :: <metafile>, branch :: <branch>)
  let f = meta.branches[branch];
  let file-stream = #f;
  format(*standard-output*, "Reloading file: %=\n", meta);
  block()
    let text = "";
    let full-filename = dylan-filename(meta.pathname);
    file-stream := read-stream-over(as(<locator>, full-filename) , element-type: #"byte");
    for(s in f.section-sequence)
      format(*standard-output*, "%=\n", s);
      s.section-text := read-n(file-stream, s.section-length);
    end;
    meta.in-core := #t;
  cleanup
    file-stream & stream-close(file-stream);
  exception(err :: <file-does-not-exist-error>)
    format(*standard-output*, "Cannot find file: %=\n", filename);
    #f
  exception(err :: <end-of-stream-error>)
    // for some files I get this error even though the file is OK.
    meta.in-core := #t;
    format(*standard-output*, "done\n");
    #t
  end;
end;

define method find-sections(meta :: <metafile>, 
			    branch :: <branch>, 
			    search-string :: <string>)
  find-file-sections(meta.branches[branch], search-string);
end;

define method all-sections(meta :: <metafile>, 
			   branch :: <branch>)
  let all = #();
  let f = meta.branches[branch];
  for(s in f.section-sequence)
//    format(*standard-output*, "%=\n", s);
    all := add(all, s);
  end;
  reverse(all);
end;


define method shallow-copy(meta :: <metafile>)
  let copy = make(<metafile>, name: meta.name, pathname: meta.pathname);
  for(key in meta.branches.key-sequence)
    copy.branches[key] := meta.branches[key];
  end;
  copy;
end;

define method print(meta :: <metafile>, #key stream = #t, verbose? = #f)
  format(*standard-output*, "[Metafile: %= located at %=]", meta.name, meta.pathname);
end;

define method describe (meta :: <metafile>, #key verbose? = #f)
  format(*standard-output*, "\nMetafile: %= located at %=\n", meta.name, meta.pathname);
  block(return)
    for(b in meta.branches)
      describe(b, verbose?: verbose?);
      unless(verbose? == full:) return(#f) end;
    end;
  end;
end;

define method update-file-section(meta :: <metafile>, branch :: <branch>, 
				  section :: <section>, text :: <string>)
  up-reference(branch);
  let new-file = create-new-file-version(meta, branch);
  let old-file = new-file.predecessor;
  let new-sec = create-updated-section(text, section);
  new-file.sections := old-file.sections;
  new-file.sections[new-sec.symbol-id] := new-sec;
  register-sections(meta, list(new-sec));
  new-sec;
end;

define method new-branch-from-disk ( meta :: <metafile>, 
				    branch :: <branch>, base :: <branch>)
  up-reference(branch);
  let new-file = create-new-file-branch(meta, branch, base);
  let new-sections = read-file-sections(meta.pathname);
  let modified = setup-new-file-sections(new-file, new-sections);
  register-sections(meta, modified);
end;

define method update-from-disk ( meta :: <metafile>, branch :: <branch> )
  up-reference(branch);
  let new-file = create-new-file-version(meta, branch);
  let new-sections = read-file-sections(meta.pathname);
  let modified = setup-new-file-sections(new-file, new-sections);
  register-sections(meta, modified);
end;

define method maybe-update-from-disk ( meta :: <metafile>, branch :: <branch> )
  let modified = update-from-disk(meta, branch);

  // if the file is not modified we back off the creation of the new version
  if(empty?(modified))
    format(*standard-output*, "Files identical: new version NOT created\n");
    let file = meta.branches[branch];
    remove-key!(meta.branches, branch);
    meta.branches[branch] := file.predecessor;
  end;
end;

define method boot-from-disk (meta :: <metafile>, branch :: <branch>)
  let new-file = create-new-file(meta, branch);
  let new-sections = read-file-sections(meta.pathname);
  let seq = #();
  for (sec in new-sections)
    let new-sec = create-new-file-section(sec, #f);
    new-file.sections[new-sec.symbol-id] := new-sec;
    seq := add(seq, new-sec);
  end;
  new-file.section-sequence := reverse(seq);
  register-sections(meta, new-file.section-sequence);
end;

define method create-new-file( meta :: <metafile>, branch :: <branch> )
  meta.branches[branch]  := make (<db-file>, metafile: meta, branch: branch,
				  version: 1, author: #f, predecessor: #f);
end;

define method create-new-file-version ( meta :: <metafile>, branch :: <branch> )
  let old-file = meta.branches[branch];
  unless (old-file)
    no-file-in-branch("File %= not found in branch %=\n", meta.name, branch.name);
  end;
  let new-file = create-new-file(meta, branch);
  new-file.predecessor := old-file;
  new-file.version := old-file.version + 1;
  new-file;
end;

define method create-new-file-branch ( meta :: <metafile>, 
				      branch :: <branch>,
				      base-branch :: <branch>)

  let new-file = create-new-file( meta, branch);
  let old-file = meta.branches[base-branch];
  unless (old-file)
    no-file-in-branch("File %= not found in branch %=\n", meta.name, branch.name);
  end;
  new-file.predecessor := old-file;
  new-file.version := 1;
  new-file;
end;

define method create-new-file-section(sec :: <list>, 
				      pre :: union(#f, <section>))
  let new-sec = make(<section>, 
		     symbol-id: head(sec),
		     start: head(tail(sec)),
		     length: head(tail(tail(sec))),
		     text: head(tail(tail(tail(sec)))),
		     code: head(tail(tail(tail(tail(sec))))),
		     predecessor: pre);

  new-sec;
end;

define method create-updated-section(sec :: <string>, 
				     pre :: <section>)
  let new-sec = make(<section>,
		     symbol-id: pre.symbol-id,
		     start: -1,
		     length: 0,
		     code: #f,
		     text: sec,
		     predecessor: pre);
  new-sec;
end;
