Module:   dfmc-debug-back-end
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: Model object printing.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define thread variable *verbose-objects?* = #f;

define compiler-sideways method print-object
    (o :: <named-object>, stream :: <stream>) => ()
  if (o.named?)
    if (*verbose-objects?*)
      format(stream, "{%s %s}", o.object-class, o.name);
    else
      format(stream, "{%s}", o.name);
    end if;
  else
    next-method();
  end if;
end method;

define compiler-sideways method print-object (o :: <mapped-unbound>, stream :: <stream>) => ()
  format(stream, "#unbound-slot-value");
end method;

define compiler-sideways method print-object (o :: <&object>, stream :: <stream>) => ()
  let ld = current-library-description() | model-original-library(o);
  if (ld)
    with-library-context (ld)
      if (o.name-if-named)
	format(stream, "{model-object %s :: %s}",
	       o.name-if-named, o.&object-class.debug-string);
      else
	format(stream, "{model-object :: %s}", o.&object-class.debug-string);
      end;
    end;
  else // not enough info to do &object-class...
   format(stream, "{interactive model-object :: %s}", o.object-class);
  end;
end method;

define compiler-sideways method print-object (o :: <&signature>, stream :: <stream>) => ()
  format(stream, "{<&signature>}");
end method;

define function model-signature (object)
  if (model-has-definition?(object))
    let def = model-definition(object);
    if (instance?(def, <function-defining-form>))
      form-signature(def)
    end;
  end;
end;

define compiler-sideways method print-object (o :: <&generic-function>, stream :: <stream>) => ()
  format(stream, "{<&generic-function> %s", o.debug-string);
  let sig = model-signature(o);
  if (sig)
    format(stream, " ");
    print-contents(sig, stream);
  end;
  format(stream, "}");
end method;


define compiler-sideways method print-referenced-object (o :: <&generic-function>, stream :: <stream>) => ()
  format(stream, "{<&generic> %s}", o.debug-string);
end method;

define method find-top-level-lambda (o :: <&lambda>) => (res :: false-or(<&lambda>))
  iterate loop (f :: <&lambda> = o)
    if (model-has-definition?(f))
      f
    else 
      let env    = environment(f);
      let lambda = lambda(lambda-environment(outer(env)));
      lambda & loop(lambda)
    end if
  end iterate;
end method;

define compiler-sideways method print-referenced-object (o :: <&method>, stream :: <stream>) => ()
  format(stream, "{<&method> ");
  if (o.named?)
    format(stream, "%s", o.debug-string);
  end if;
  if (instance?(model-definition(o), <function-defining-form>))
    format(stream, " ");
    print-specializers(form-signature(model-definition(o)), stream);
  elseif (instance?(o, <&lambda>))
    let top-lambda = find-top-level-lambda(o);
    format(stream, " of ");
    if (top-lambda)
      print-referenced-object(top-lambda, stream);
    else 
      format(stream, "top-level");
    end if;
  end if;
  format(stream, "}");
end method;

define thread variable *print-method-bodies?* = #f;

define compiler-sideways method print-object (o :: <&method>, stream :: <stream>) => ()
  format(stream, "{<&method>");
  if (o.named?)
    format(stream, " %s", o.debug-string);
  end if;
  let sig = model-signature(o);
  if (sig)
    format(stream, " ");
    print-contents(sig, stream);
  end;
  if (*print-method-bodies?* & o.body &
	if (lambda-top-level?(o))
	  // don't print bodies of top-level lambdas referenced
	  // from inside other lambdas
	  empty?(*lambdas-in-progress*)
	else
	  ~member?(o, *lambdas-in-progress*)
	end)
    dynamic-bind (*lambdas-in-progress* = pair(o, *lambdas-in-progress*))
      label!(o);
      format(stream, "\n");
      block ()
        dynamic-bind (*offset* = *offset* + 1)
          print-computations(stream, o.body);
        end;
      exception (c :: <error>)
        format(stream, "<<<Error caught while printing code %=>>>", c);
      end;
    end;
  end;
  format(stream, "}");
end method;

define constant $top-string = "<top>";

define compiler-sideways method print-object (o :: <&top-type>, stream :: <stream>) => ()
  format(stream, "{<&top-type> %s}", $top-string);
end method;

define constant $bottom-string = "<bottom>";

define compiler-sideways method print-object (o :: <&bottom-type>, stream :: <stream>) => ()
  format(stream, "{<&bottom-type> %s}", $bottom-string);
end method;

define compiler-sideways method print-object (o :: <&class>, stream :: <stream>) => ()
  format(stream, "{<&class> %s}", o.debug-string);
end method;

define compiler-sideways method print-object (o :: <&slot-descriptor>, stream :: <stream>) => ()
  format(stream, "{<&slot-descriptor> %s}", o.^slot-getter.^debug-name);
end method;

define compiler-sideways method print-object (o :: <&raw-type>, stream :: <stream>) => ()
  format(stream, "{<&raw-type> %s}", o.debug-string);
end method;

define compiler-sideways method print-object (o :: <&raw-object>, stream :: <stream>) => ()
  format(stream, "%%%=", o.^raw-object-value);
end method;

define method primitive-name (o :: <&primitive>) => (name)
  let var = o.model-variable-name;
  if (var)
    let name = var.fragment-identifier;
    copy-sequence(as(<string>, name), start: size("primitive-"));
  else
    "unknown"
  end;
end method;

define compiler-sideways method print-object (o :: <&primitive>, stream :: <stream>) => ()
  format(stream, "&[PRIMITIVE %s]", primitive-name(o));
end method;

define compiler-sideways method print-object (o :: <&boolean>, stream :: <stream>) => ()
  format(stream, "&#%s",
	 case
	   o == &false => "f";
	   o == &true  => "t";
	   otherwise   => cerror("Ignore it.", "unknown <&boolean>"); "?";
	 end case);
end method;

define compiler-sideways method print-object (o :: <&single-float>, stream :: <stream>) => ()
  format(stream, "&%=", o.^%single-float-data.^raw-object-value)
end method;

define compiler-sideways method print-object (o :: <&double-float>, stream :: <stream>) => ()
  format(stream, "&%=", o.^%double-float-data.^raw-object-value)
end method;


// hack to get a decent debug name out of an ^debug-name

define function debug-string-using (debug-name)
  if (instance?(debug-name, <variable-name-fragment>))
    debug-name.fragment-identifier
  elseif (debug-name == #f)
    #"no-name"
  else
    as(<symbol>, debug-name)
  end
end function debug-string-using;

define method debug-string (object)
  debug-string-using(object.^debug-name)
end method debug-string;

define method debug-string (object :: <&method>)
  debug-string-using(object.debug-name)
end method debug-string;

define method name-if-named (o :: <named-object>) => (name?)
  o.name
end method;

define method name-if-named (o :: <&type>) => (name?)
  #f
end method;

define method name-if-named (o :: <&class>) => (name?)
  o.debug-string
end method;

define method name-if-named (o :: <&mm-wrapper>) => (name?)
  // name-if-named(o.^mm-wrapper-class)
  name-if-named(o.^mm-wrapper-implementation-class.^iclass-class)
end method;

define method name-if-named (o :: <&function>) => (name?)
  o.debug-string
end method;

define method name-if-named (o) => (name?)
  #f
end;

define compiler-sideways method named? (o :: <&type>) => (well? :: <boolean>)
  #f
end method;

define compiler-sideways method named? (o :: <&class>) => (well? :: <boolean>)
  o.^debug-name & #t
end method;

define compiler-sideways method named? (o :: <&function>) => (well? :: <boolean>)
  o.debug-name & #t
end method;

define compiler-sideways method named? (o) => (well? :: <boolean>)
  #f
end;

define compiler-sideways method print-object (o :: <library-description>, stream :: <stream>) => ()
  let vers = o.library-description-change-count;
  format(stream, "{%s%slibrary-description of %s.%s}",
	 if (vers) "" else "CLOSED " end,
	 if (instance?(o, <interactive-library-description>)) "interactive "
	 else "" end,
	 library-description-emit-name(o),
	 if (instance?(vers, <integer>)) vers else "???" end);
end method;


define compiler-sideways method print-object (o :: <model-heap>, stream :: <stream>) => ()
  format(stream, "{model-heap of %s}", heap-compilation-record(o));
end method;
