Module: source-db
Language: infix-dylan
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <db-source> (<db-object>)
  slot name, init-value: "unnamed", init-keyword: name:;
  slot metafiles, init-function: method() make (<table>) end;
  slot meta-sequence, init-function: method() make (<deque>) end;
  slot branches, init-function: method() make (<table>) end;
  slot authors;
  slot branch-search-list :: <list>;
end class;

define method describe (snapshot :: <db-source>, #key verbose? = #f)
  format(*standard-output*, "\n=> Snapshot Name: %=\n\n", snapshot.name);
  format(*standard-output*, "All branches: \n");

  for(b in snapshot.branches)
    describe(b, verbose?: verbose?);
  end;
  format(*standard-output*, "\nBranch search list: ");
  for(b in snapshot.branch-search-list)
    print(b);
  end;

  format(*standard-output*, "\nMetafiles: \n");
  for(m in snapshot.metafiles)
    describe(m, verbose?: verbose?);
  end;
end;

define class <db-source-system> (<db-source>)
  slot db-pointer;
  slot snapshots, init-function: method() make (<table>) end;
end class;

define method initialize (sys :: <db-source-system>, #key, #all-keys)
  let branch = make(<branch>, name: "base");
  sys.branches[#"base"] := branch;
  sys.branch-search-list := list(branch);
end;

define method print(sys :: <db-source-system>, #key stream = *standard-output*, verbose? = #f)
  format(stream, "[db-source-system: %= ]", sys.name);
end;

define method find-branch(sys :: <db-source>, s :: <string>)
  sys.branches[as(<symbol>, s)];
end;

define method find-branch(sys :: <db-source>, b :: <branch>)
  sys.branches[as(<symbol>, b.name)]; // branch b may be not in sys
end;

define method create-branch(sys :: <db-source-system>, s :: <string>)
  if(sys.branches[as(<symbol>, s)])
    source-db-error("Branch %= already exists\n", s);
  else
    sys.branches[as(<symbol>, s)] := make(<branch>, name: s);
  end;
end;

define method branch-list-setter(l :: <list>, sys :: <db-source-system>)
  let new-list = #();
  for( b in l )
    let branch = find-branch(sys, b);
    unless(branch)
      source-db-error("No such branch: %=\n", b);
    end;
    new-list := pair(branch, new-list);
  end;
  sys.branch-search-list := reverse! (new-list);
end;

define method describe (sys :: <db-source-system>, #key verbose? = #f)
  format(*standard-output*, "Database source system: %=\n", sys.name);
  next-method();
  format(*standard-output*, "Snapshots: \n");
  for(s in sys.snapshots)
    describe(s, verbose?: verbose?);
  end;
end;

define method full-file-name(file :: <locator>)
  let file-name = base-name(file);
  if(extension(file))
    file-name := concatenate(file-name, as(<string>, extension(file)));
  end;
  file-name;
end;

define method boot-file(sys :: <db-source-system>, file :: <string>)
  let branch = default-branch(sys);
  let meta = make(<metafile>, name: file, pathname: file);
  sys.metafiles[as(<symbol>, file)] := meta;
  sys.meta-sequence := push-last(sys.meta-sequence, meta);
  format(*standard-output*, "Booting file: %=\n", file);
  boot-from-disk(meta, branch);
end;

define method boot-file(sys :: <db-source-system>, file :: <locator>)
  let branch = default-branch(sys);
  let file-name = as(<symbol>, full-file-name(file));
  let meta = make(<metafile>, name: file-name, pathname: as(<string>, file));
  sys.metafiles[file-name] := meta;
  sys.meta-sequence := push-last(sys.meta-sequence, meta);
  format(*standard-output*, "Booting file(locator): %=\n", file);
  boot-from-disk(meta, branch);
  meta.in-core := #t;
end;

define method find-file(sys :: <db-source-system>, file :: <locator>)
  find-file(sys, full-file-name(file));
end;

define method find-file(sys :: <db-source-system>, file :: <string>)
  let meta = sys.metafiles[as(<symbol>, file)];
  unless(meta)
    format(*standard-output*, "No file %= in the system %=\n", file, sys.name);
    no-file-in-system("No file %= in the system %=\n", file, sys.name);
  end;
  let branch = find-file(meta, sys.branch-search-list);
  unless(branch)
    format(*standard-output*, "No file %= in the system %= branch search list\n", 
	   file, sys.name);
    no-file-in-system("No file %= in the system %= branch search list\n", 
		      file, sys.name);
  end;
  values(meta, branch);
end;


define method find-file(meta :: <metafile>, branch-list :: <list>)
  block(result)
    for(b in branch-list)
      if(meta.branches[b])
	result (b);
      end;
    end;
    #f;
  end;
end;

define method update-or-boot-file(sys :: <db-source-system>, 
				  file :: union(<string>, <locator>))
  block(return)
    update-file(sys, file);
  exception (<no-file-in-system>)
    boot-file(sys, file);
  end;
end;

define method update-file(sys :: <db-source-system>, 
			  file :: union(<string>, <locator>))
  let (meta, base-branch) = find-file(sys, file);
  format(*standard-output*, "Updating file: %=\n", file);  
  if(sys.default-branch == base-branch)
    maybe-update-from-disk(meta, sys.default-branch);
  else
    new-branch-from-disk(meta, sys.default-branch, base-branch);
  end;
end;

define method update-file-in-branch(sys :: <db-source-system>, file :: <string>)
  let branch = default-branch(sys);
  let meta = sys.metafiles[as(<symbol>, file)];
  update-from-disk(meta, branch);
end;

define method default-branch(sys :: <db-source-system>)
  head(sys.branch-search-list);
end;

// snapshot contains versions (copies) of metafiles at the time of 
// the snapshot creation 
define method create-snapshot(sys :: <db-source-system>, s-name,
			      #key branch-list = sys.branch-search-list)
  let snapshot = make(<db-source>, name: s-name);
  let search-list = #();
  for(b in branch-list)
    let branch = find-branch(sys, b);
    snapshot.branches[b] := b;
    search-list := pair(b, search-list);
  end;
  snapshot.branch-search-list := reverse!(search-list);
  for(key in sys.metafiles.key-sequence)
    let m = sys.metafiles[key];
    if(find-file(m, branch-list))
      snapshot.metafiles[as(<symbol>, m.name)] := shallow-copy(m);
    end;
  end;
  sys.snapshots[as(<symbol>, s-name)] := snapshot;
end;

define method save-source-system(sys :: <db-source-system>, locator :: <locator>)
  let db-stream = #f;
  block()
    db-stream := write-stream-over(locator, element-type: #"byte",
				   end: #f, if-exists: #"truncate");
    let policy = make(<source-db-doss-policy>);
    let dumper = make(<doss-dumper>, stream: db-stream, policy: policy);
    store-object(sys, dumper);
  cleanup
    db-stream & stream-close(db-stream);
  end;
end;

define method open-source-system(locator :: <locator>)
  let db-stream = #f;
  let sys = #f;
  block()
    db-stream := read-stream-over(locator, element-type: #"byte");
    let loader = make(<doss-loader>, stream: db-stream);
    sys := loader.fetch-object;
  cleanup
    db-stream & stream-close(db-stream);
  exception(err :: <file-does-not-exist-error>)
    format(*standard-output*, "%=\n", err);
    #f
  exception(err :: <end-of-stream-error>)
    format(*standard-output*, "done\n");
    #t
  end;
  sys;
end;

define method retrieve-one-section(sys :: <db-source-system>, 
				   search-string :: <string>)
  let section = 
    block(return)
      for(key in sys.metafiles.key-sequence)
	  let m = sys.metafiles[key];
	  let b = find-file(m, sys.branch-search-list);
	  format(*standard-output*, "M= %= in core? %= B= %= \n", m, m.in-core, b);
	  if(b)
	    let found = find-sections(m, b, search-string);
	    format(*standard-output*, "found: %= \n", found);
	    if(~empty?(found)) 
	      unless(m.in-core) reload-from-disk(m, b) end;
	      return(head(found));
	    end;
	  end;
      end;
      #f;
    end;
  section;
end;

define method retrieve-some-sections(sys :: <db-source-system>, 
				     search-string :: <string>)
  let found = #();
  for(key in sys.metafiles.key-sequence)
    let m = sys.metafiles[key];
    let b = find-file(m, sys.branch-search-list);
    format(*standard-output*, "M= %= in core? %= B= %= \n", m, m.in-core, b);
    if(b)
      let sections = find-sections(m, b, search-string);
      if(~empty?(sections))
	unless(m.in-core) reload-from-disk(m, b) end;
	found := union(sections, found);
      end;
    end;
  end;
  for(s in found)
    describe(s, verbose?: #t);
  end;
  found;
end;

define method retrieve-all-sections(sys :: <db-source-system>)
  let found = #();
  for(m in sys.meta-sequence)
    let b = find-file(m, sys.branch-search-list);
    unless(m.in-core) reload-from-disk(m, b) end;
    if(b)
      format(*standard-output*, "**%=\n", m);
      found := union(all-sections(m, b), found);
    end;
  end;
  for(s in found)
    describe(s);
  end;
  reverse(found);
end;


define method open-source-snapshot(name, path, snapshot-name)

end;

