Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Composite buffers

define open abstract class <composite-buffer-mixin> (<buffer>)
end class <composite-buffer-mixin>;

define method composite-buffer?
    (buffer :: <buffer>) => (composite? :: singleton(#f))
  #f
end method composite-buffer?;

define sealed inline method composite-buffer?
    (buffer :: <composite-buffer-mixin>) => (composite? :: singleton(#t))
  #t
end method composite-buffer?;

define method buffer-section-separator-style
    (buffer :: <composite-buffer-mixin>) => (style :: <section-separator-style>)
  #"always"
end method buffer-section-separator-style;


// When we add a section node to a composite buffer, establish a two-way
// associated between the composite buffer and any other buffers that
// use the section
define sealed method add-node!
    (buffer :: <composite-buffer-mixin>, node :: <section-node>,
     #key after :: type-union(<basic-node>, one-of(#f, #"start", #"end")) = #"end") => ()
  ignore(after);
  next-method();
  let section = node-section(node);
  do-associated-buffers (other :: <basic-buffer> = *editor-frame*)
    when (buffer ~== other
	  & buffer-contains-section?(other, section))
      unless (member?(buffer, buffer-associated-buffers(other)))
	push!(buffer-associated-buffers(other), buffer)
      end;
      unless (member?(other, buffer-associated-buffers(buffer)))
	push!(buffer-associated-buffers(buffer), other)
      end
    end
  end
end method add-node!;

define sealed method remove-node!
    (buffer :: <composite-buffer-mixin>, node :: <section-node>) => ()
  let section = node-section(node);
  do-associated-buffers (other :: <basic-buffer> = *editor-frame*)
    when (buffer ~== other
	  & buffer-contains-section?(other, section))
      buffer-associated-buffers(other) := remove!(buffer-associated-buffers(other), buffer)
    end
  end;
  next-method()
end method remove-node!;

define method buffer-default-pathname
    (buffer :: <composite-buffer-mixin>) => (pathname :: <pathname>)
  "definitions"
end method buffer-default-pathname;

// Composite buffers save like file buffers, but the 'save-buffer' method
// write out all of the associated file buffers.  Cool, huh?
define method saves-like-file-buffer?
    (buffer :: <composite-buffer-mixin>) => (saves? :: <boolean>)
  #t
end method saves-like-file-buffer?;

// Saving a composite buffer causes all of the associated buffers to be saved.
// Note the "Save As" just saves the text of the buffer to the specified location,
// as it would for any other kind of buffer.
define method save-buffer
    (buffer :: <composite-buffer-mixin>,
     #key frame = *editor-frame*, editor)
 => (pathname :: false-or(<pathname>), condition)
  let editor = editor | frame-editor(frame);
  let window = frame-window(frame);
  let buffers :: <stretchy-object-vector> = make(<stretchy-vector>);
  // Do this the hard way, so that we only offer to save buffers
  // that have been modified by way of this composite buffer
  for (node = buffer-start-node(buffer) then node-next(node),
       until: ~node)
    let section :: <basic-section> = node-section(node);
    when (section-modification-tick(section) > section-sectionization-tick(section))
      let home-buffer = section-home-buffer(section, editor: editor);
      when (home-buffer & file-buffer?(home-buffer))
	add-new!(buffers, home-buffer)
      end
    end
  end;
  do-save-all-files(frame, buffers, curry(display-message, window));
  //--- A kludgy way of indicating the save succeeded...
  values("all home files", #f)
end method save-buffer;

define sealed method kill-buffer
    (buffer :: <composite-buffer-mixin>,
     #key frame = *editor-frame*, editor, no-exit-frame) => ()
  ignore(editor, no-exit-frame);
  for (node = buffer-start-node(buffer) then node-next(node),
       until: ~node)
    let section = node-section(node);
    do-associated-buffers (other :: <basic-buffer> = frame)
      when (buffer ~== other
	    & buffer-contains-section?(other, section))
	buffer-associated-buffers(other) := remove!(buffer-associated-buffers(other), buffer)
      end
    end
  end;
  next-method();
end method kill-buffer;


// A command that will get us from a section to its home buffer...
define command edit-home-definition (frame, #key section)
    "Edit this definition in its home buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  when (composite-buffer?(buffer))
    let section = section | line-section(bp-line(point()));
    let home    = section-home-buffer(section);
    when (home & home ~== buffer)
      select-buffer-in-appropriate-window(window, home,
					  line: section-start-line(section))
    end
  end;
  frame-last-command-type(frame) := #"motion"
end command edit-home-definition;


define sealed class <composite-buffer>
    (<composite-buffer-mixin>, <non-file-buffer-mixin>, <basic-buffer>)
end class <composite-buffer>;

define sealed domain make (singleton(<composite-buffer>));
define sealed domain initialize (<composite-buffer>);


/// Special-purpose buffers for browsing

define open abstract class <definition-browsing-buffer>
    (<composite-buffer-mixin>,
     <basic-special-purpose-buffer>)
  // The definition we'll browse
  // Back-end clients get to define exactly what a definition is...
  sealed constant slot browsing-buffer-definition,
    required-init-keyword: definition:;
  // The name-key applied to the definition gives a string
  sealed constant slot browsing-buffer-name-key  :: <function>,
    required-init-keyword: name-key:;
  // A format string for generating the name of the buffer, or #f
  // (in which case the subclass may supply a 'buffer-name' method)
  sealed constant slot %format-string :: false-or(<string>) = #f,
    init-keyword: format-string:;
  sealed slot %browsing-buffer-name :: false-or(<byte-string>) = #f;
  // The generator applied to the definition produces a sequence of sections
  sealed constant slot browsing-buffer-generator :: <function>,
    required-init-keyword: generator:;
  sealed constant slot %node-class :: subclass(<definition-node>) = <definition-node>,
    init-keyword: node-class:;
end class <definition-browsing-buffer>;

define method buffer-name
    (buffer :: <definition-browsing-buffer>) => (name :: <string>)
  buffer.%browsing-buffer-name
  | begin
      let definition = browsing-buffer-definition(buffer);
      let name-key   = browsing-buffer-name-key(buffer);
      let name       = format-to-string(buffer.%format-string | "%s", name-key(definition));
      buffer.%browsing-buffer-name := name;
      name
    end
end method buffer-name;

define method revert-buffer
    (buffer :: <definition-browsing-buffer>,
     #key buffer-filler :: false-or(<function>) = fill-definition-browsing-buffer, major-mode)
 => (reverted? :: <boolean>)
  ignore(major-mode);
  // Reset the timestamps on the buffer
  // Since the undo history is per-section, we don't need to reset it
  let tick = tick();
  buffer-modification-tick(buffer) := tick;
  buffer-save-tick(buffer) := tick;
  // Now go read the contents of the buffer
  when (buffer-filler)
    buffer-filler(buffer)
  end;
  #t
end method revert-buffer;

define method fill-definition-browsing-buffer
    (buffer :: <definition-browsing-buffer>) => ()
  let definition = browsing-buffer-definition(buffer);
  let generator  = browsing-buffer-generator(buffer);
  let sections :: false-or(<sequence>) = generator(definition);
  buffer-start-node(buffer) := #f;
  buffer-end-node(buffer)   := #f;
  if (sections & ~empty?(sections))
    for (section :: <basic-section> in sections)
      let node = make-section-node(buffer, section,
				   node-class: buffer.%node-class);
      add-node!(buffer, node)
    end
  else
    // No definitions, so make an empty section node for the buffer
    let node = make-empty-section-node(buffer);
    add-node!(buffer, node)
  end
end method fill-definition-browsing-buffer;


/// Class and function browsing buffer classes

define sealed class <subclasses-browsing-buffer>
    (<definition-browsing-buffer>)
  keyword format-string: = "Subclasses of %s";
end class <subclasses-browsing-buffer>;


define sealed class <superclasses-browsing-buffer>
    (<definition-browsing-buffer>)
  keyword format-string: = "Superclasses of %s";
end class <superclasses-browsing-buffer>;


define sealed class <class-methods-browsing-buffer>
    (<definition-browsing-buffer>)
  keyword format-string: = "Methods of class %s";
end class <class-methods-browsing-buffer>;


define sealed class <generic-function-methods-browsing-buffer>
    (<definition-browsing-buffer>)
  keyword format-string: = "Methods of generic function of %s";
end class <generic-function-methods-browsing-buffer>;


define sealed class <callers-browsing-buffer>
    (<definition-browsing-buffer>)
  keyword format-string: = "Callers of %s";
end class <callers-browsing-buffer>;


define sealed class <callees-browsing-buffer>
    (<definition-browsing-buffer>)
  keyword format-string: = "Callees of %s";
end class <callees-browsing-buffer>;
