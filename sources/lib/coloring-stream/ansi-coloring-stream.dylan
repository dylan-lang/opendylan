Module:    coloring-stream-internals
Author:    Bruce Mitchener, Jr.
Copyright: Original Code is Copyright 2015 Dylan Hackers.
           All rights reserved.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define class <ansi-coloring-stream> (<coloring-stream>)
end class <ansi-coloring-stream>;

define sealed domain make(singleton(<ansi-coloring-stream>));
define sealed domain initialize(<ansi-coloring-stream>);

define constant $ansi-fg-base = 30;
define constant $ansi-bg-base = 40;

define method attributes-to-ansi
    (attributes :: <text-attributes>)
 => (ansi-codes :: <byte-string>)
  let fg = attributes.text-foreground-color;
  let bg = attributes.text-background-color;
  let intensity = attributes.text-intensity;
  with-output-to-string(s)
    write(s, "\<1b>[");
    let need-separator = #f;
    if (fg)
      write(s, integer-to-string($ansi-fg-base + fg.color-ansi-code));
      need-separator := #t;
    end if;
    if (bg)
      if (need-separator)
        write-element(s, ';');
      end if;
      write(s, integer-to-string($ansi-bg-base + bg.color-ansi-code));
      need-separator := #t;
    end if;
    if (intensity)
      if (need-separator)
        write-element(s, ';');
      end if;
      write(s, select (intensity)
                 $normal-intensity => "0";
                 $bright-intensity => "1";
                 $dim-intensity    => "2";
               end select);
      need-separator := #t;
    end if;
    write-element(s, 'm');
  end with-output-to-string
end method attributes-to-ansi;

define method print-object
    (attributes :: <text-attributes>,
     stream :: <ansi-coloring-stream>)
 => ()
  let ansi-codes
    = if (attributes = $reset-attributes)
        "\<1b>[0m"
      else
        attributes-to-ansi(attributes);
      end if;
  write(stream, ansi-codes);
end method print-object;
