Module:       progress-stream-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2020 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $default-bar-width :: <integer> = 40;
define constant $default-line-width :: <integer> = 79;

define abstract class <progress-stream> (<wrapper-stream>)
end class;

define sealed method make
    (class == <progress-stream>, #rest args,
     #key inner-stream :: <stream>, force?, #all-keys)
 => (progress-stream :: <progress-stream>);
  if (force?
        | (instance?(inner-stream, <file-stream>)
             & stream-console?(inner-stream)))
    apply(make, <console-progress-stream>, args)
  else
    apply(make, <dummy-progress-stream>, args)
  end if
end method;

define class <dummy-progress-stream> (<progress-stream>)
end class;

define class <console-progress-stream> (<progress-stream>)
  constant slot console-progress-bar-width :: <integer>,
    init-keyword: bar-width:, init-value: $default-bar-width;
  constant slot console-progress-line-width :: <integer>,
    init-keyword: line-width:, init-value: $default-line-width;
  slot console-progress-bars :: false-or(<integer>),
    init-value: #f;
  slot console-progress-label :: false-or(<string>),
    init-value: #f;
end class;

define generic stream-supports-show-progress?
    (stream :: <stream>)
 => (supports-show-progress? :: <boolean>);

define sideways method stream-supports-show-progress?
    (stream :: <stream>)
 => (supports-show-progress? :: <boolean>);
  #f
end method;

define method stream-supports-show-progress?
    (stream :: <console-progress-stream>)
 => (supports-show-progress? :: <boolean>);
  #t
end method;

define generic show-progress
    (stream :: <progress-stream>, position :: <integer>, range :: <integer>,
     #key label)
 => ();

define method show-progress
    (stream :: <progress-stream>,
     position :: <integer>, range :: <integer>,
     #key label :: false-or(<string>))
 => ();
  // Do nothing
end method;

define method show-progress
    (stream :: <console-progress-stream>,
     position :: <integer>, range :: <integer>,
     #key label :: false-or(<string>))
 => ();
  let inner = stream.inner-stream;
  let bar-width = stream.console-progress-bar-width;
  let line-width = stream.console-progress-line-width;
  let bars
    = if (zero?(range))
        bar-width
      else
        round/(position * bar-width, range);
      end if;
  if (bars ~= stream.console-progress-bars
        | label ~= stream.console-progress-label)
    // The bar itself
    write(inner, "\r[");
    write-fill(inner, '=', bars);
    write-fill(inner, ' ', bar-width - bars);
    write(inner, "]");

    // Label, and spaces to clear previous leftover label
    let max-label-width = line-width - (bar-width + 2 + 1);
    let current-post-bar-width
      = if (label)
          write-element(inner, ' ');
          if (label.size <= max-label-width)
            write(inner, label);
            label.size + 1
          else
            write(inner, label, end: max-label-width - 3);
            write(inner, "...");
            max-label-width + 1
          end if
        else
          0
        end if;
    let previous-label = stream.console-progress-label;
    let previous-post-bar-width
      = if (previous-label)
          if (previous-label.size <= max-label-width)
            previous-label.size + 1
          else
            max-label-width + 1
          end if
        else
          0;
        end if;
    write-fill(inner, ' ', previous-post-bar-width - current-post-bar-width);
    force-output(inner);
    stream.console-progress-bars := bars;
    stream.console-progress-label := label;
  end if;
end method;

define function clear-bar
    (stream :: <console-progress-stream>, available :: <integer>)
  if (stream.console-progress-bars | stream.console-progress-label)
    let max-label-width
      = stream.console-progress-line-width
      - (stream.console-progress-bar-width + 2 + 1);
    let previous-label = stream.console-progress-label;
    let previous-line-width
      = stream.console-progress-bar-width + 2
      + if (previous-label)
          if (previous-label.size <= max-label-width)
            previous-label.size + 1
          else
            max-label-width + 1
          end if
        else
          0;
        end if;
    write-element(stream.inner-stream, '\r');
    if (available < previous-line-width)
      write-fill(stream.inner-stream, ' ', previous-line-width);
      write-element(stream.inner-stream, '\r');
    else
    end if;
    stream.console-progress-bars := #f;
    stream.console-progress-label := #f;
  end if;
end function;

define method write-element
    (stream :: <console-progress-stream>, elt :: <object>) => ()
  clear-bar(stream, 1);
  write-element(stream.inner-stream, elt)
end method write-element;

define method write
    (stream :: <console-progress-stream>, elements :: <sequence>,
     #rest keys, #key start: _start, end: _end) => ()
  clear-bar(stream, (_end | elements.size) - (_start | 0));
  apply(write, stream.inner-stream, elements, keys)
end method write;

define method new-line (stream :: <console-progress-stream>) => ()
  clear-bar(stream, 0);
  new-line(stream.inner-stream);
end method new-line;
