Module: user-projects
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *copy-canonical-sources?* = #f;

define method project-compiler-source-files
    (project :: <user-project>)
 => (location :: false-or(<sequence>));
  if (*copy-canonical-sources?*)
    let build-directory = project.project-build-location;
    map(method (f :: <file-locator>)
	  make(<file-locator>,
	       directory: build-directory,
	       name:      f.locator-name)
	end,
	project.project-source-files)
  else
    project.project-source-files;
  end;
end;

define method project-compiler-source-location
    (project :: <user-project>)
 => (location :: <directory-locator>)
  if(*copy-canonical-sources?*)
    project.project-build-location;
  else
    project.project-source-location;
  end;
end;

define generic project-source-record-location(project :: <project>, 
					      sr :: <source-record>)
 => (location :: false-or(<file-locator>));

define method project-source-record-location(project :: <project>,
					     sr :: <source-record>)
 => (location :: false-or(<file-locator>));
  sr.source-record-location
end;

define method project-source-record-location(project :: <user-project>,
					     sr :: <file-source-record>)
 => (location :: false-or(<file-locator>));
  if(*copy-canonical-sources*)
    let sr-name = sr.source-record-name;
    sr-name & any?(method(sr)
		       sr.source-record-name = sr-name & sr.source-record-location
		   end,
		   compute-project-source-records(project));
  else
    sr.source-record-location
  end;  
end;

define function project-id-source-record(project :: <user-project>, id, #key create? = #t) => sr;
  let table = project.%source-record-table;
  let str = as(<string>, id);
  let record = element(table, str, default: #f);
  record | (create? &
	      (table[str] := 
		 id-as-source-record(project-source-record-class(project), 
				     project,
				     project-source-location(project), id)))
end function;

define function compute-project-source-records(project :: <user-project>)
 => sr*;
  local method id-source-record (id) => sr;
	  project-id-source-record(project, id)
	end method;

  let new-user-records = 
      project-files-to-source-records(project,
				      directory: project.project-source-location,
				      files: project.project-source-files,
				      id-source-record: id-source-record);
  new-user-records
end;

define method project-verify-source-records(project :: <user-project>)
 => (records :: <sequence>);
  if(*copy-canonical-sources?*)
    block()
      compute-project-source-records(project)
    exception(e :: <source-record-error>)
      apply(user-error, e.condition-format-string, e.condition-format-arguments);
      #()
    end;
  else
    next-method()
  end
end;

define method update-project-files (project :: <user-project>) => ();
  if(*copy-canonical-sources?*)
    let new-user-records = compute-project-source-records(project);
      
    let changed-records = choose(method(r) 
				     ~member?(r, project.%compiled-source-records)
				 end,
				 new-user-records);
    unless(empty?(changed-records))
      let build-directory = project.project-build-location;
      for(sr in changed-records)
	let source-file = source-record-location(sr);
	let target-file
	  = make(<file-locator>,
		 directory: build-directory,
		 name:      locator-name(source-file));
	debug-message("  Copying %s to build area", as(<string>, source-file));
		
	block()
	  copy-file(source-file, target-file, if-exists: #"replace")
	exception(e :: <file-system-error>) 
	  debug-assert(#f, condition-format-string(e), condition-format-arguments(e))
	end block;
      end;
      project.%compiled-source-records := new-user-records;
    end;

  end;
  // do nothing - we don't allow for changes of HDP files 
  // outside of the environment
end;

// Compute the sources records to give the compiler.
define method project-current-source-records (project :: <user-project>)
 => sr*;
  block()
    compute-compiler-source-records(project)
// TO DO: should we catch it ?
//  exception (<file-does-not-exist-error>)

  end;
end method;


// some assumptions:
//
// the environment gets hold of canonical source records only through the compiler
// i.e. it never opens the files itself. Consequently we never look at the real 
// canonical source records
//
// the meaning of compiled is similar to canonical, but if we copy canonical records
// compiled can be #t even though the record is a project record
define generic project-file-source-record(project :: <project>, file :: <file-locator>) 
 => (sr :: false-or(<source-record>), compiled? :: <boolean>);

define method project-file-source-record(project :: <project>, file :: <file-locator>) 
 => (sr :: false-or(<source-record>), compiled? :: <boolean>);
  values(#f, #f)
end;
/*
  let sr-class = project.project-source-record-class;
  let directory = project.project-compiler-source-location;
  let ids = file-source-record-ids(sr-class, directory, file);
  let id = first(ids); // ----- assume for now that we deal with one id per file
  values(project-id-canonical-source-record(project, id), #t)
*/
define method project-file-source-record
    (project :: <system-project>, file :: <file-locator>) 
 => (sr :: false-or(<file-source-record>), compiled? :: <boolean>);
  let sr-class = project.project-source-record-class;
  let directory = project.project-source-location;
  
  
end;

define function file-project-source-record
    (file :: <file-locator>)
 => (project :: <project>, sr :: false-or(<source-record>));

define generic project-canonical-source-record(project, sr :: <source-record>)
 => (sr :: false-or(<source-record>));

define generic project-project-source-record(project, sr :: <source-record>)
 => (sr :: false-or(<source-record>));

define generic project-source-record-location(project, sr :: <source-record>)
 => (loc :: <file-locator>);

define generic project-canonical-source-record-location(project, sr :: <source-record>)
 => (loc :: <file-locator>);
