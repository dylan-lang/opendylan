Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Printers

define method print-object
    (bp :: <bp>, stream :: <stream>) => ()
  printing-object (bp, stream, type?: #t, identity?: #f)
    format(stream, "index %d in line %=", bp-index(bp), bp-line(bp))
  end
end method print-object;

define method print-object
    (line :: <text-line>, stream :: <stream>) => ()
  printing-object (line, stream, type?: #t, identity?: #f)
    let length   = line-length(line);
    let text     = copy-sequence(line-contents(line), start: 0, end: min(length, 20));
    let ellipses = if (length > 20) "..." else "" end;
    format(stream, "%s%s", text, ellipses)
  end
end method print-object;

define method print-object
    (section :: <section>, stream :: <stream>) => ()
  printing-object (section, stream, type?: #t, identity?: #f)
    format(stream, "%=", section-definition-name(section))
  end
end method print-object;

define method print-object
    (interval :: <interval>, stream :: <stream>) => ()
  printing-object (interval, stream, type?: #t, identity?: #f)
    format(stream, "start %= end %=",
	   interval-start-bp(interval), interval-end-bp(interval))
  end
end method print-object;

define method print-object
    (node :: <node>, stream :: <stream>) => ()
  printing-object (node, stream, type?: #t, identity?: #f)
    format(stream, "%=", node-definition-name(node))
  end
end method print-object;

define method print-object
    (container :: <source-container>, stream :: <stream>) => ()
  printing-object (container, stream, type?: #t, identity?: #f)
    format(stream, "%s", container-pathname(container))
  end
end method print-object;

define method print-object
    (buffer :: <buffer>, stream :: <stream>) => ()
  printing-object (buffer, stream, type?: #t, identity?: #f)
    format(stream, "%s", buffer-name(buffer))
  end
end method print-object;

define method print-object
    (dline :: <display-line>, stream :: <stream>) => ()
  printing-object (dline, stream, type?: #t, identity?: #f)
    format(stream, "%= at %=", display-line-line(dline), display-line-y(dline))
  end
end method print-object;

define method print-object
    (comtab :: <command-table>, stream :: <stream>) => ()
  printing-object (comtab, stream, type?: #t, identity?: #f)
    format(stream, "%=", command-table-name(comtab))
  end
end method print-object;

define method print-object
    (comset :: <command-set>, stream :: <stream>) => ()
  printing-object (comset, stream, type?: #t, identity?: #f)
    format(stream, "%=", command-set-name(comset))
  end
end method print-object;

define method print-object
    (presentation :: <presentation>, stream :: <stream>) => ()
  printing-object (presentation, stream, type?: #t, identity?: #f)
    format(stream, "%= %=",
	   presentation-type(presentation), presentation-object(presentation))
  end
end method print-object;


define method print-object
    (record :: <insert-change-record>, stream :: <stream>) => ()
  printing-object (record, stream, type?: #t, identity?: #f)
    format(stream, "%= %=",
           change-record-start-bp(record), change-record-end-bp(record))
  end
end method print-object;

define method print-object
    (record :: <delete-change-record>, stream :: <stream>) => ()
  printing-object (record, stream, type?: #t, identity?: #f)
    format(stream, "%= %=",
           change-record-start-bp(record), change-record-end-bp(record),
           change-record-old-text(record))
  end
end method print-object;


define method print-object
    (signature :: <dylan-signature>, stream :: <stream>) => ()
  printing-object (signature, stream, type?: #t, identity?: #f)
    format(stream, "%= %= %=",
           signature-type(signature), signature-name(signature),
	   as(<list>, signature-parameters(signature)))
  end
end method print-object;
