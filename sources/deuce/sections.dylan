Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Sections

define protocol <<section>> ()
  getter section-container
    (section :: <section>) => (container :: false-or(<source-container>));
  getter section-home-buffer
    (section :: <section>, #key editor)
 => (buffer :: false-or(<buffer>));
  getter section-start-line
    (section :: <section>) => (line :: false-or(<line>));
  setter section-start-line-setter
    (line :: false-or(<line>), section :: <section>) => (line :: false-or(<line>));
  getter section-end-line
    (section :: <section>) => (line :: false-or(<line>));
  setter section-end-line-setter
    (line :: false-or(<line>), section :: <section>) => (line :: false-or(<line>));
  getter section-nodes
    (section :: <section>) => (nodes :: <sequence>);
  setter section-nodes-setter
    (nodes :: <sequence>, section :: <section>) => (nodes :: <sequence>);
  getter section-modification-tick
    (section :: <section>) => (tick :: <integer>);
  setter section-modification-tick-setter
    (tick :: <integer>, section :: <section>) => (tick :: <integer>);
  getter section-sectionization-tick
    (section :: <section>) => (tick :: <integer>);
  setter section-sectionization-tick-setter
    (tick :: <integer>, section :: <section>) => (tick :: <integer>);
  getter section-compilation-tick
    (section :: <section>) => (tick :: <integer>);
  setter section-compilation-tick-setter
    (tick :: <integer>, section :: <section>) => (tick :: <integer>);
  getter section-lock
    (section :: <section>) => (lock :: false-or(<exclusive-lock>));
  function add-line!
    (section :: <section>, line :: <line>, #key after) => ();
  function remove-line!
    (section :: <section>, line :: <line>) => ();
  function note-section-changed
    (section :: <section>) => ();
  function note-section-compiled
    (section :: <section>) => ();
  function resectionize-section
    (section :: <section>) => (resectionized? :: <boolean>);
  getter section-defining-line
    (section :: <section>) => (line :: false-or(<line>));
  function section-definition-signature
    (section :: <section>) => (signature);
  function section-definition-name
    (section :: <section>) => (name :: false-or(<string>));
  function section-definition-type
    (section :: <section>) => (type :: false-or(<symbol>));
end protocol <<section>>;

define constant <section-separator-style> = one-of(#"always", #"requested", #"never");

// A section is the basic unit of user data, which is represented as a sequence
// of lines.  A section can be contained by more than one node, but a section node
// can contain only one section.  This containment of sections by section nodes
// means that the same section can simultaneously appear in multiple buffers.
define open abstract primary class <basic-section> (<section>)
  sealed slot section-container :: false-or(<source-container>) = #f,
    init-keyword: container:;
  // The start and end of a linked list of lines
  // When both are #f, the section is empty
  sealed slot section-start-line :: false-or(<basic-line>),
    required-init-keyword: start-line:;
  sealed slot section-end-line :: false-or(<basic-line>),
    required-init-keyword: end-line:;
  // All of the nodes in which this section is contained.
  // We represent this as a list because the usual length is 1
  sealed slot section-nodes :: <list> = #(),
    init-keyword: nodes:;
  // The section is considered changed when the modification tick is greater
  // than the sectionization tick.  They are both initialized to the same value
  // when the container is sectionized.
  sealed slot section-modification-tick   :: <integer> = *tick*;
  sealed slot section-sectionization-tick :: <integer> = *tick*;
  sealed slot section-compilation-tick    :: <integer> = *tick*;
  // It's reasonable for every section in a container to share the
  // container's single recursive lock...
  sealed constant slot section-lock :: false-or(<exclusive-lock>) = #f,
    init-keyword: lock:;
  // Cached so that redisplay doesn't get slowed down
  sealed slot %n-lines :: false-or(<integer>) = #f;
end class <basic-section>;

define sealed method section-home-buffer
    (section :: <basic-section>, #key editor = frame-editor(*editor-frame*))
 => (buffer :: false-or(<basic-buffer>))
  let container = section-container(section);
  if (container)
    // The section came from a source container, so return the buffer
    // into which the container was originally read.
    container-home-buffer(container, editor: editor)
  else
    // The section isn't in a source container.  We could try to return
    // the section's original node, but that is often not the right thing,
    // so just return #f.  Higher-level code can always insert the section
    // into a dummy source container if this is not adequate.
    #f
  end
end method section-home-buffer;


define sealed inline method make
    (class == <section>, #rest initargs, #key, #all-keys)
 => (section :: <simple-section>)
  apply(make, <simple-section>, initargs)
end method make;

define sealed class <simple-section> (<basic-section>)
end class <simple-section>;

define sealed domain make (singleton(<simple-section>));
define sealed domain initialize (<simple-section>);


define function make-empty-section
    (#key section-class = <section>) => (section :: <section>)
  let section = make(section-class,
                     container: #f,
                     start-line: #f, end-line: #f);
  let line    = make(<text-line>,
                     contents: "",
                     section: section);
  section-start-line(section) := line;
  section-end-line(section)   := line;
  section
end function make-empty-section;


// A class upon which language-specific definition sections are built
define open abstract class <definition-section> (<basic-section>)
end class <definition-section>;

define method section-defining-line
    (section :: <basic-section>) => (line :: false-or(<basic-line>))
  #f
end method section-defining-line;

define method section-definition-signature
    (section :: <basic-section>) => (signature);
  #f
end method section-definition-signature;

define method section-definition-name
    (section :: <basic-section>) => (name :: false-or(<string>));
  #f
end method section-definition-name;

define method section-definition-type
    (section :: <basic-section>) => (type :: false-or(<symbol>));
  #f
end method section-definition-type;


define method do-lines
    (function :: <function>, section :: <basic-section>,
     #key from-end? = #f, skip-test = line-for-display-only?) => ()
  let (start-line, end-line, step :: <function>)
    = if (from-end?)
        values(section-end-line(section), section-start-line(section), line-previous)
      else
        values(section-start-line(section), section-end-line(section), line-next)
      end;
  when (start-line)        // the section might be empty
    block (break)
      for (line = start-line then step(line))
        when (line & (~skip-test | ~skip-test(line)))
          let si = 0;
          let ei = line-length(line);
          function(line, si, ei, line == end-line)
        end;
        when (~line | line == end-line)
          break()
        end
      end
    end
  end
end method do-lines;

define method count-lines
    (section :: <basic-section>,
     #key skip-test = line-for-display-only?, cache-result? = #f)
 => (nlines :: <integer>)
  (cache-result? & section.%n-lines)
  | begin
      let n :: <integer> = 0;
      do-lines(method (line :: <line>, si, ei, last?)
                 ignore(line, si, ei, last?);
                 inc!(n)
               end method, section, skip-test: skip-test);
      when (cache-result?)
        section.%n-lines := n
      end;
      n
    end
end method count-lines;

// Note that this _does_ include a '\n' character at the end of each line
define method as
    (class :: subclass(<string>), section :: <basic-section>)
 => (string :: <byte-string>)
  let bp1 = line-start(section-start-line(section));
  let bp2 = line-end(section-end-line(section));
  let interval = make-interval(bp1, bp2, in-order?: #t);
  as(<byte-string>, interval)
end method as;


define method note-section-changed
    (section :: <basic-section>) => ()
  section-modification-tick(section) := tick();
  // Notify the source container that some of its data changed
  let container = section-container(section);
  when (container)
    note-container-changed(container)
  end;
  // Then tell all the nodes using this section that they have changed, too
  for (node :: <basic-node> in section-nodes(section))
    note-node-changed(node)
  end
end method note-section-changed;

define method note-section-compiled
    (section :: <basic-section>) => ()
  section-compilation-tick(section) := section-modification-tick(section)
end method note-section-compiled;

define sealed method resectionize-section
    (section :: <basic-section>) => (resectionized? :: <boolean>)
  when (section-sectionization-tick(section) < section-modification-tick(section))
    let container = section-container(section);
    // If the container has hard sections, those sections are definitive,
    // so don't go resectionizing this section
    when (container & ~container-has-hard-sections?(container))
      let mode = find-mode-from-pathname(container-pathname(container));
      let resectionized? = do-resectionize-section(mode, section);
      section-sectionization-tick(section) := section-modification-tick(section);
      // If we resectionized this section, arrange to redisplay it
      when (resectionized?)
        do-associated-windows (window :: <basic-window> = *editor-frame*)
          let buffer = window-buffer(window);
          when (buffer & buffer-contains-section?(buffer, section))
            queue-redisplay(window, $display-all)
          end
        end
      end;
      resectionized?
    end
  end
end method resectionize-section;


/// Adding and removing lines to sections

define sealed method add-line!
    (section :: <basic-section>, line :: <basic-line>,
     #key after :: type-union(<basic-line>, one-of(#f, #"start", #"end")) = #"end") => ()
  assert(~line-section(line),
         "The line %= is already in the section %=", line, line-section(line));
  let (next, prev)
    = select (after)
        #f, #"start" =>
          values(section-start-line(section), #f);
        #"end" =>
          values(#f, section-end-line(section));
        otherwise =>
          assert(line-section(after) == section,
                 "The 'after' line %= is not in the section %=", after, section);
          values(line-next(after), after);
      end;
  line-section(line)  := section;
  line-next(line)     := next;
  line-previous(line) := prev;
  if (next)
    line-previous(next) := line
  else
    section-end-line(section) := line;
    // Update the end BPs of any nodes using this section
    for (node :: <basic-node> in section-nodes(section))
      move-bp!(interval-end-bp(node), line, line-length(line))
    end
  end;
  if (prev)
    line-next(prev) := line
  else
    section-start-line(section) := line;
    // Update the start BPs of any nodes using this section
    for (node :: <basic-node> in section-nodes(section))
      move-bp!(interval-start-bp(node), line, 0)
    end
  end;
  update-section-line-count(section, 1)
end method add-line!;

define sealed method remove-line!
    (section :: <basic-section>, line :: <basic-line>) => ()
  assert(line-section(line) == section,
         "The line %= is not in the section %=", line, section);
  let (next, prev)
    = values(line-next(line), line-previous(line));
  if (next)
    line-previous(next) := prev
  else
    section-end-line(section) := prev;
    for (node :: <basic-node> in section-nodes(section))
      move-bp!(interval-end-bp(node), prev, line-length(prev))
    end
  end;
  if (prev)
    line-next(prev) := next
  else
    section-start-line(section) := next;
    for (node :: <basic-node> in section-nodes(section))
      move-bp!(interval-start-bp(node), next, 0)
    end
  end;
  //---*** Set 'line-section' to #f when we're sure nobody is relying on it
  line-next(line)     := #f;
  line-previous(line) := #f;
  // If any windows are starting their redisplay at the line
  // we just removed, we better fix them up now
  //--- This is dubious modularity, but at least it's very centralized
  do-associated-windows (window :: <basic-window> = *editor-frame*)
    when (window-initial-line(window) == line)
      window-initial-line(window) := next
    end
  end;
  update-section-line-count(section, -1)
end method remove-line!;

define sealed method update-section-line-count
    (section :: <basic-section>, delta :: <integer>) => ()
  // Update the line count for this section
  when (section.%n-lines)
    section.%n-lines := section.%n-lines + delta
  end;
  // Update every window which has a buffer containing this section
  //--- This looping is probably not too bad, since most sections are in only one node
  for (node :: <basic-node> in section-nodes(section))
    let buffer = node-buffer(node);
    when (buffer)
      do-associated-windows (window :: <basic-window> = *editor-frame*)
        let buffer = window-buffer(window);
        when (buffer
              & window.%total-lines
              & buffer-contains-section?(buffer, section))
          window-total-lines(window) := window.%total-lines + delta
        end
      end
    end
  end
end method update-section-line-count;
