module:   sealing-workbench
author:   Paul Haahr
synopsis: Print methods for meta-objects.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *print-library?* = #f;
define variable *verbose?* = #f;
define variable *terse?* = #f;

define macro print-method-definer
  { define print-method (?object:name :: ?type:name , ?stream:variable)
      ?body
    end }
  => { define method print
           (?object :: ?type, #key ?stream = *standard-output*, verbose?)
         if (*terse?* & ?object.name)
	   format(stream, "%s", ?object.name);
	 else
	   format(?stream, "{");
	   ?body;
	   format(?stream, "}");
	 end if;
         values()
       end method print }
end macro;

define print-method (library :: <library>, stream)
  format(stream, "{the %s library}", library.name)
end print-method;

define method print-library (stream :: <stream>, object)
  if (*print-library?* & object.library ~= *library*)
    format(stream, " in %s", object.library.name)
  end if;
end method print-library;  

define print-method (object :: <&object>, stream)
  if (object.name)
    format(stream, "the %s %s", object.object-&class.name, object.name);
  else
    format(stream, "an anonymous %s", object.object-&class.name)
  end if;
  print-library(stream, object);
end print-method;

define print-method (object :: <&class>, stream)
  if (object.name)
    format(stream, "class %s", object.name);
  else
    format(stream, "anonymous class", object.object-&class.name)
  end if;
  print-library(stream, object);
end print-method;

define print-method (&generic :: <&generic>, stream)
  if (&generic.open?)
    format(stream, "open ");
  end if;
  format(stream, "generic %s", &generic.name | "{anonymous}");
  if (*verbose?*)
    format(stream, " %=", &generic.&function-parameters);
  end if;
  print-library(stream, &generic);
end print-method;

define print-method (&method :: <&method>, stream)
  format(stream, "method %s", &method.name | "{anonymous}");
  if (&method.calls-next-method?)
    format(stream, " [calls next method]");
  end;
  if (*verbose?*)
    format(stream, " %=", &method.&function-parameters);
  end if;
  print-library(stream, &method);
end print-method;

define print-method (domain :: <sealed-domain>, stream)
  format(stream, "sealed domain %= %=", domain.&generic, domain.types);
  print-library(stream, domain);
end print-method;

define print-method (singleton :: <&singleton>, stream)
  format(stream, "singleton for %=", singleton.singleton-&object)
end print-method;

define print-method (op :: <ordering-pair>, stream)
  format(stream, "%s preceeds %s", op.before.name, op.after.name);
end print-method;
