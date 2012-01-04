Module:   dfmc-debug-back-end
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: Definition object printing.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define compiler-sideways method print-object
    (o :: <variable-defining-form>, stream :: <stream>) => ()
  format(stream,
         if (*sexp?*) "%s %s" else "{%s %s}" end, 
         o.object-class, 
         map(fragment-identifier, form-variable-names(o)));
end method;

define compiler-sideways method print-object
    (o :: <&converter-definition>, stream :: <stream>) => ()
  format(stream, "{ define &converter %s }", 
         fragment-identifier(form-variable-name(o)));
end method;

define compiler-sideways method print-object
    (o :: <&macro-definition>, stream :: <stream>) => ()
  format(stream, "{ define &macro %s }", 
         fragment-identifier(form-variable-name(o)));
end method;

define compiler-sideways method print-object
    (o :: <&definition-definition>, stream :: <stream>) => ()
  format(stream, "{ define &definition %s }", 
         fragment-identifier(form-variable-name(o)));
end method;

define compiler-sideways method print-object
    (o :: <macro-definition>, stream :: <stream>) => ()
  format(stream, "{ define macro %s }", 
         fragment-identifier(form-variable-name(o)));
end method;

define compiler-sideways method print-object
    (o :: <constant-definition>, stream :: <stream>) => ()
  format(stream, "{ define constant %s }", 
         fragment-identifier(form-variable-name(o)));
end method;

define compiler-sideways method print-object
    (o :: <variable-definition>, stream :: <stream>) => ()
  format(stream, "{ define variable %s }", 
         fragment-identifier(form-variable-name(o)));
end method;

define compiler-sideways method print-object
    (o :: <generic-definition>, stream :: <stream>) => ()
  format(stream, "{ %sdefine generic %s ",
	 if (form-implicitly-defined?(o)) "(implicit) " else "" end,
         fragment-identifier(form-variable-name(o)));
  print-contents(form-signature(o), stream);
  format(stream, " }");
end method;

define compiler-sideways method print-object
    (o :: <method-definition>, stream :: <stream>) => ()
  format(stream, "{ define method %s ",
         fragment-identifier(form-variable-name(o)));
  print-contents(form-signature(o), stream);
  format(stream, " ... end }");
end method;

define compiler-sideways method print-object
    (o :: <domain-definition>, stream :: <stream>) => ()
  format(stream, "{ define sealed domain %s (",
         fragment-identifier(form-variable-name(o)));
  print-contents-sequence(form-domain-type-expressions(o), stream);
  format(stream, ") }");
end method;

define compiler-sideways method print-object
    (o :: <class-definition>, stream :: <stream>) => ()
  format(stream, "{ define class %s }", 
         fragment-identifier(form-variable-name(o)));
end method;

define compiler-sideways method print-object
    (o :: <slot-definition>, stream :: <stream>) => ()
  format(stream, "{ ");
  print-adjectives(form-adjectives(o), stream);
  format(stream, "slot %s }", 
         fragment-identifier(form-variable-name(o)));
end method;

define method print-adjectives
    (adjectives, stream :: <stream>) => ()
  for (adjective in adjectives)
    format(stream, "%s ", adjective);
  end;
end method;

// Function signature printing.

define method print-contents (sig-spec :: <signature-spec>, stream) => ()
  let sep = #f;
  local method print-sep () 
    if (sep) format(stream, ", ") else sep := #t end;
  end;
  format(stream, "(");
  for (var-spec in spec-argument-required-variable-specs(sig-spec))
    print-sep();
    print-contents(var-spec, stream);
  end;
  if (spec-argument-next-variable-spec(sig-spec))
    print-sep();
    format(stream, "#next ");
    print-contents(spec-argument-next-variable-spec(sig-spec), stream);
  end;
  if (spec-argument-rest-variable-spec(sig-spec))
    print-sep();
    format(stream, "#rest ");
    print-contents(spec-argument-rest-variable-spec(sig-spec), stream);
  end;
  if (spec-argument-key?(sig-spec))
    print-sep();
    format(stream, "#key ...");
  end;
  if (spec-argument-all-keys?(sig-spec))
    print-sep();
    format(stream, "#all-keys");
  end;
  format(stream, ")");
  format(stream, " => ");
  sep := #f;
  format(stream, "(");
  for (var-spec in spec-value-required-variable-specs(sig-spec))
    print-sep();
    print-contents(var-spec, stream);
  end;
  if (spec-value-rest-variable-spec(sig-spec))
    print-sep();
    format(stream, "#rest ");
    print-contents(spec-value-rest-variable-spec(sig-spec), stream);
  end;
  format(stream, ")");
end method;

define method print-specializers (sig-spec :: <signature-spec>, stream) => ()
  let sep = #f;
  local method print-sep () 
    if (sep) format(stream, ", ") else sep := #t end;
  end;
  format(stream, "(");
  for (var-spec in spec-argument-required-variable-specs(sig-spec))
    print-sep();
    print-specializer(var-spec, stream);
  end;
  format(stream, ")");
end method;

define method print-specializer (var :: <variable-spec>, stream) => ()
  print-contents(spec-type-expression(var), stream);
end method;

define method print-contents (var :: <variable-spec>, stream) => ()
  print-contents(spec-variable-name(var), stream);
  when (spec-variable-typed?(var))
    format(stream, " :: ");
    print-contents(spec-type-expression(var), stream);
  end when;
end method;

define method print-contents (frag :: <variable-name-fragment>, stream) => ()
  format(stream, "%s", fragment-identifier(frag))
end method;

define method print-contents (frag, stream) => ()
  format(stream, "%=", frag)
end method;

define method print-contents-sequence
   (seq :: <sequence>, stream, #key separator = " ")
  for (object in seq, first = #t then #f)
    if (~first)
      write(stream, separator);
    end;
    print-contents(object, stream);
  end;
end method;
