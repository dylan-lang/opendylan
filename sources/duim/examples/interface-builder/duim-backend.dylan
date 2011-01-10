Module:       interface-builder
Author:       Andy Armstrong
Synopsis:     DUIM interface builder
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $indentation        = "  ";
define constant $double-indentation = concatenate($indentation, $indentation);

define method save-model
    (model :: <model-frame>, code == #"duim") => ()
  with-open-file (stream = model-filename(model), direction: #"output")
    print-header(stream, model);
    print-model(stream, model, code)
  end
end method save-model;

define method print-header
    (stream :: <stream>, model :: <model-frame>) => ()
  format(stream, "Module: %s\n\n", model-module-name(model))
end method print-header;

define method print-model
    (stream :: <stream>, model :: <model-frame>, code == #"duim") => ()
  local method maybe-print-section
            (model :: false-or(<model-sheet>), section :: <string>) => ()
          model & print-duim-model-section(stream, model, section)
        end method maybe-print-section;
  let name = model.model-name;
  format(stream, "define frame <%s> (%s)\n", name, model.model-superclasses);
  maybe-print-section(model.model-menu-bar,   "menu-bar");
  maybe-print-section(model.model-tool-bar,   "tool-bar");
  maybe-print-section(model.model-layout,     "layout");
  maybe-print-section(model.model-status-bar, "status-bar");
  format(stream, "%skeyword title: = %=;\n", $indentation, model.model-title);
  format(stream, "end frame <%s>;\n\n", name)
end method print-model;

define method print-duim-model-section
    (stream :: <stream>, model :: <model-sheet>, section :: <string>) => ()
  let name = model.model-name;
  print-model(stream, model, #"duim");
  format(stream, "%s%s (frame)\n%s",
         $indentation, section, $double-indentation);
  if (name)
    format(stream, "frame.%s", name)
  else
    print-duim-make-code(stream, model, indentation: $double-indentation)
  end;
  format(stream, ";\n")
end method print-duim-model-section;

define method print-duim-make-code
    (stream :: <stream>, model :: <model-sheet>, #key indentation = $indentation) => ()
  let name = model.model-name;
  let info = model.model-class;
  let children = model.model-children;
  let arguments = as(<deque>, model.model-arguments);
  format(stream, "make(<%s>", info.info-name);
  unless (empty?(children))
    format(stream, ",\n%s     children: vector(", indentation);
    let indentation = concatenate(indentation, "                      ");
    for (child in children,
         separator = "" then format-to-string(",\n%s", indentation))
      let child-name = child.model-name;
      format(stream, "%s", separator);
      if (child-name)
        format(stream, "frame.%s", child-name)
      else
        print-duim-make-code(stream, child, indentation: indentation)
      end
    end;
    format(stream, ")")
  end;
  while (~empty?(arguments))
    let keyword = pop(arguments);
    let value = pop(arguments);
    format(stream, ",\n%s     %s: %=", indentation, keyword, value)
  end;
  format(stream, ")")
end method print-duim-make-code;

define method print-duim-make-code
    (stream :: <stream>, model :: <model-layout>, #key indentation = $indentation) => ()
  let name = model.model-name;
  let info = model.model-class;
  let class = info.info-class;
  select (class)
    <row-layout>, <column-layout> =>
      let children = model.model-children;
      format(stream, "%s ()\n",
             if (class == <row-layout>) "horizontally" else "vertically" end);
      unless (empty?(children))
        let indentation = concatenate(indentation, $indentation);
        for (child in children)
          let child-name = child.model-name;
          format(stream, "%s", indentation);
          if (child-name)
            format(stream, "frame.%s", child-name)
          else
            print-duim-make-code(stream, child, indentation: indentation)
          end;
          format(stream, ";\n")
        end;
      end;
      format(stream, "%send", indentation);
    otherwise =>
      next-method();
  end
end method print-duim-make-code;

define method print-model
    (stream :: <stream>, model :: <model-sheet>, code == #"duim") => ()
  let name = model.model-name;
  let children = model.model-children;
  for (child in model.model-children)
    print-model(stream, child, code)
  end;
  if (name)
    format(stream, "%spane %s (frame)\n%s",
           $indentation, name, $double-indentation);
    print-duim-make-code(stream, model, indentation: $double-indentation);
    format(stream, ";\n")
  end
end method print-model;

