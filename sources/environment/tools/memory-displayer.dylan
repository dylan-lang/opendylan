Module:    environment-tools
Synopsis:  Environment tools
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Memory displayer

define class <memory-displayer-state> (<displayer-state>)
end class <memory-displayer-state>;

define pane <memory-displayer> (<displayer-mixin>)
  constant slot displayer-information-available?-function :: false-or(<function>) = #f,
    init-keyword: information-available?-function:;
  constant slot displayer-address-generator :: <function> = identity,
    init-keyword: address-generator:;
  constant slot displayer-memory-size :: <data-display-size> = #"word",
    init-keyword: memory-size:;
  constant slot displayer-memory-format :: <data-display-format> = #"hexadecimal",
    init-keyword: memory-format:;
  pane displayer-editor-pane (pane)
    make(<text-editor>,
         read-only?: #t,
         text-style: make(<text-style>, family: #"fix"));
  layout (pane)
    pane.displayer-editor-pane;
end pane <memory-displayer>;

define sideways method refresh-frame-property-page
    (frame :: <environment-frame>,
     displayer :: <memory-displayer>,
     object :: <environment-object>, type == #"memory",
     #key clean?, new-thread? = #t)
 => ()
  //---*** We should really update this piecemeal
  ignore(new-thread?);
  let project = frame.ensure-frame-project;
  let state = displayer.displayer-state;
  let new-state
    = case
        state & state.displayer-state-object == object =>
          state;
        otherwise =>
          displayer.displayer-state
            := make(<memory-displayer-state>, object: object);
      end;
  if (state ~= new-state | clean?)
    let address
      = displayer.displayer-information-available?-function()
          & displayer.displayer-address-generator(object);
    let text
      = if (address)
          with-output-to-string (stream)
            print-displayer-memory-contents(stream, displayer, address)
          end
        else
          "No information available"
        end;
    gadget-text(displayer.displayer-editor-pane) := text;
    frame-status-message(frame) := ""
  end
end method refresh-frame-property-page;

define sideways method refresh-frame-property-page
    (frame :: <environment-frame>,
     displayer :: <memory-displayer>,
     environment-object == #f, type == #"memory",
     #key clean?, new-thread? = #t)
 => ()
  gadget-text(displayer.displayer-editor-pane) := ""
end method refresh-frame-property-page;

define method print-displayer-memory-contents
    (stream :: <stream>, displayer :: <memory-displayer>, object :: <address-object>)
 => ()
  with-displayer-transaction (displayer)
    let frame = displayer.sheet-frame;
    let project = frame.ensure-frame-project;
    let group-size :: <integer> = 4;
    let words-to-bytes = 4;
    let address-string = environment-object-primitive-name(project, object);
    let address = string-to-machine-word(address-string);
    let memory-size   = displayer.displayer-memory-size;
    let memory-format = displayer.displayer-memory-format;
    for (index :: <integer> from 0 below 256 by group-size)
      let strings
        = address-read-memory-contents
            (project, object,
             size:       memory-size,
             format:     memory-format,
             from-index: index,
             to-index:   index + group-size - 1);
      format(stream, "%s  ", mw/+(address, as(<machine-word>, index)));
      for (string :: <string> in strings)
        format(stream, "%s ", string)
      end;
      let ascii-characters
        = address-read-memory-contents
            (project, object,
             size:       #"byte",
             format:     #"byte-character",
             from-index: (index * words-to-bytes),
             to-index:   ((index + group-size) * words-to-bytes) - 1);
      format(stream, "    ");
      //---*** This won't work properly with Unicode!
      for (string :: <string> in ascii-characters)
        let character = string[0];
        let code = as(<integer>, character);
        if (code > 31 & code < 127)
          format(stream, "%s", string)
        else
          write(stream, ".")
        end
      end;
      new-line(stream)
    end
  end
end method print-displayer-memory-contents;
