Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Source containers

define protocol <<source-container>> ()
  getter container-pathname
    (container :: <source-container>) => (pathname :: <pathname>);
  setter container-pathname-setter
    (pathname :: <pathname>, container :: <source-container>, #key editor)
 => (pathname :: <pathname>);
  getter container-sections
    (container :: <source-container>) => (sections :: <stretchy-sequence>);
  setter container-sections-setter
    (sections :: <stretchy-sequence>, container :: <source-container>)
 => (sections :: <stretchy-sequence>);
  getter container-has-hard-sections?
    (container :: <source-container>) => (hard-sections? :: <boolean>);
  getter container-modified?
    (container :: <source-container>) => (modified? :: <boolean>);
  getter container-read-only?
    (container :: <source-container>) => (read-only? :: <boolean>);
  function container-home-buffer
    (container :: <source-container>, #key editor) => (buffer :: false-or(<buffer>));
  getter container-buffers
    (container :: <source-container>) => (buffers :: <sequence>);
  setter container-buffers-setter
    (buffers :: <sequence>, container :: <source-container>, #key editor)
 => (buffers :: <sequence>);
  getter container-undo-history
    (container :: <source-container>) => (undo-history :: false-or(<undo-history>));
  getter container-lock
    (container :: <source-container>) => (lock :: false-or(<recursive-lock>));
  getter container-properties
    (container :: <source-container>) => (properties :: <sequence>);
  setter container-properties-setter
    (properties :: <sequence>, container :: <source-container>) => (properties :: <sequence>);
  function note-container-changed
    (container :: <source-container>) => ();
  function read-container-contents
    (container :: <source-container>, buffer :: <buffer>) => ();
  function sectionize-container
    (container :: <source-container>) => (sectionized? :: <boolean>);
  function add-section!
    (container :: <source-container>, section :: <section>, #key after) => ();
  function remove-section!
    (container :: <source-container>, section :: <section>) => ();
end protocol <<source-container>>;

// A source container is an object that models the primary place from
// which a set of sections came from.  A simple flat file, for example,
// is a kind of source container from which sections are derived.
define open abstract primary class <basic-source-container> (<source-container>)
  sealed slot container-pathname :: <pathname>,
    setter: %pathname-setter,
    required-init-keyword: pathname:;
  sealed slot container-sections :: <stretchy-object-vector> = make(<stretchy-vector>);
  sealed slot container-modified? :: <boolean> = #f;
  // Backpointer to all buffers that use any sections in this container
  // We represent this as a list because the usual length is 1
  sealed slot container-buffers :: <list> = #(),
    setter: %buffers-setter;
  sealed slot container-undo-history :: <undo-history> = make(<undo-history>),
    init-keyword: undo-history:;
  sealed slot container-modification-date :: false-or(<date>) = #f,
    init-keyword: modification-date:;
  sealed slot container-properties :: <list> = #(),
    init-keyword: properties:;
  sealed constant slot container-lock :: false-or(<recursive-lock>) = #f,
    init-keyword: lock:;
end class <basic-source-container>;

define method find-source-container
    (editor :: <editor>, class :: subclass(<source-container>), pathname, #rest initargs)
 => (container :: <source-container>)
  let namestring = as(<string>, pathname);
  let containers = editor-source-containers(editor);
  let container
    = gethash(containers, namestring)
      | begin
	  let new-container = apply(make, class, pathname: pathname, initargs);
	  gethash(containers, namestring) := new-container;
	  new-container
	end;
  container
end method find-source-container;

define method container-home-buffer
    (container :: <basic-source-container>, #key editor = frame-editor(*editor-frame*))
 => (buffer :: false-or(<buffer>))
  let pathname = container-pathname(container);
  pathname & find-buffer-from-pathname(editor, pathname)
end method container-home-buffer;

define method container-pathname-setter
    (pathname :: <pathname>, container :: <basic-source-container>, #key editor)
 => (pathname :: <pathname>)
  let old-namestring = as(<string>, container-pathname(container));
  let new-namestring = as(<string>, pathname);
  container.%pathname := pathname;
  // Make the editor's set of source containers be keyed by the new name
  let editor = editor | (*editor-frame* & frame-editor(*editor-frame*));
  when (editor)
    remhash(editor-source-containers(editor), old-namestring);
    gethash(editor-source-containers(editor), new-namestring) := container
  end;
  pathname
end method container-pathname-setter;

define method container-buffers-setter
    (buffers :: <list>, container :: <basic-source-container>, #key editor)
 => (buffers :: <list>)
  container.%buffers := buffers;
  // If this is the last buffer using this source container,
  // get rid of the source container too
  let editor = editor | (*editor-frame* & frame-editor(*editor-frame*));
  when (editor & empty?(buffers))
    let namestring = as(<string>, container-pathname(container));
    remhash(editor-source-containers(editor), namestring)
  end;
  buffers
end method container-buffers-setter;

//--- How do we decide whether to use a database file?
define method source-container-class
    (pathname :: <pathname>) => (class :: subclass(<source-container>))
  <flat-file-source-container>
end method source-container-class;


define method note-container-changed
    (container :: <basic-source-container>) => ()
  container-modified?(container) := #t
end method note-container-changed;

define sealed method sectionize-container
    (container :: <basic-source-container>) => (sectionized? :: <boolean>)
  // If the container has hard sections, those sections are definitive,
  // so don't go sectionizing it
  unless (container-has-hard-sections?(container))
    let mode = find-mode-from-pathname(container-pathname(container));
    do-sectionize-container(mode, container)
  end
end method sectionize-container;

define method add-section!
    (container :: <basic-source-container>, section :: <basic-section>,
     #key after :: type-union(<basic-section>, one-of(#f, #"start", #"end")) = #"end") => ()
  assert(~section-container(section),
	 "The section %= is already in the container %=", section, section-container(section));
  let index = select (after)
		#f, #"start" =>
		  #"start";
		#"end" =>
		  #"end";
		otherwise =>
		  assert(section-container(after) == container,
			 "The 'after' section %= is not in the container %=", after, container);
		  position(container-sections(container), after) + 1;
	      end;
  section-container(section) := container;
  insert-at!(container-sections(container), section, index)
end method add-section!;

define method remove-section!
    (container :: <basic-source-container>, section :: <basic-section>) => ()
  assert(section-container(section) == container,
	 "The section %= is not in the container %=", section, section-container(section));
  //---*** This needs to notify all the nodes using this section
  section-container(section) := #f;
  container-sections(container) := remove!(container-sections(container), section)
end method remove-section!;


/// Flat file source containers

define sealed class <flat-file-source-container> (<basic-source-container>)
end class <flat-file-source-container>;

define sealed domain make (singleton(<flat-file-source-container>));
define sealed domain initialize (<flat-file-source-container>);

define sealed inline method container-has-hard-sections?
    (container :: <flat-file-source-container>) => (hard-sections? :: singleton(#f))
  #f
end method container-has-hard-sections?;

define sealed method container-read-only?
    (container :: <flat-file-source-container>) => (read-only? :: <boolean>)
  let pathname   = container-pathname(container);
  let writeable? = get-file-property(pathname, #"writeable?", default: #t);
  ~writeable?
end method container-read-only?;

// Read the contents of the file into a single-node buffer
define sealed method read-container-contents
    (container :: <flat-file-source-container>, buffer :: <buffer>) => ()
  let pathname = container-pathname(container);
  // Make a section to hold all the lines of the file
  let section = make(<section>,
		     container: container,
		     start-line: #f, end-line: #f);
  container-sections(container)
    := make(<stretchy-vector>, size: 1, fill: section);
  unless (member?(buffer, container-buffers(container)))
    push!(container-buffers(container), buffer)
  end;
  // Now read the file contents into the section
  let stream :: false-or(<stream>)
    = pathname & file-exists?(pathname)
      & make(<file-stream>, locator: pathname, direction: #"input");
  block ()
    read-buffer-contents-from-stream(buffer, section, stream)
  cleanup
    when (stream & stream-open?(stream))
      close(stream)
    end
  end;
  // Remember the file modification date
  container-modification-date(container)
    := get-file-property(pathname, #"modification-date", default: current-date());
  container-modified?(container) := #f
end method read-container-contents;

// Read all the lines from the stream into the single provided section
define method read-buffer-contents-from-stream
    (buffer :: <buffer>, section :: <basic-section>, stream :: false-or(<stream>)) => ()
  // Fill up the section
  read-section-contents-from-stream(section, stream);
  // Now make a node for the buffer that holds the single section
  let node = make-section-node(buffer, section);
  node-buffer(node)         := buffer;
  section-nodes(section)    := list(node);
  buffer-start-node(buffer) := node;
  buffer-end-node(buffer)   := node
end method read-buffer-contents-from-stream;

define method read-section-contents-from-stream
    (section :: <basic-section>, stream :: false-or(<stream>)) => ()
  let first-line :: false-or(<basic-line>) = #f;
  let last-line  :: false-or(<basic-line>) = #f;
  when (stream)
    block (break)
      while (#t)
	let contents = read-line(stream, on-end-of-stream: #f);
	if (contents)
	  let line = make(<text-line>,
			  contents: contents,
			  section: section);
	  unless (first-line)
	    first-line := line
	  end;
	  line-previous(line) := last-line;
	  when (last-line)
	    line-next(last-line) := line
	  end;
	  last-line := line
	else
	  break()
	end
      end
    end
  end;
  // Watch out for empty or non-existent files
  unless (first-line)
    first-line := make(<text-line>,
		       contents: "",
		       section: section);
    last-line  := first-line
  end;
  section-start-line(section) := first-line;
  section-end-line(section)   := last-line
end method read-section-contents-from-stream;


/// Database source containers

define sealed class <database-file-source-container> (<basic-source-container>)
end class <database-file-source-container>;

define sealed domain make (singleton(<database-file-source-container>));
define sealed domain initialize (<database-file-source-container>);

define sealed inline method container-has-hard-sections?
    (container :: <database-file-source-container>) => (hard-sections? :: singleton(#t))
  #t
end method container-has-hard-sections?;

define method read-container-contents
    (container :: <database-file-source-container>, buffer :: <buffer>) => ()
  //---*** Database files have hard sections, just read them in and create nodes
  container-modified?(container) := #f
end method read-container-contents;


define method add-section!
    (container :: <database-file-source-container>, section :: <basic-section>,
     #key after :: type-union(<basic-section>, one-of(#f, #"start", #"end")) = #"end") => ()
  //---*** What else does this need to do?
  next-method()
end method add-section!;

define method remove-section!
    (container :: <database-file-source-container>, section :: <basic-section>) => ()
  //---*** What else does this need to do?
  next-method()
end method remove-section!;
