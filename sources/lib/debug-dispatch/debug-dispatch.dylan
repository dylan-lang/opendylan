Module:    debug-dispatch
Filename:  debug-dispatch.dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method show (g :: <generic-function>)
  format-out("\n\nDiscriminator code for %=:\n", g);
  disp(discriminator(g), 1);
  let c = %gf-cache(g);
  if (instance?(c, <gf-cache-info>))
    let entries = gf-cache-info-users(c);
    format-out("------------- Call site cache %=, %= possible entries\n", c, size(entries));
    for (x in entries, i from 0)
      if (x)
	format-out("---- user %= ----\n", i);
	disp(x, 1)
      end 
    end 
  end;
  #t
end method;

define method show (e :: <engine-node>)
  format-out("\n\nEngine node %=:\n", e);
  disp(e, 1);
  #t
end method;


define method disp-line (lev :: <integer>, line :: <string>, #rest args)
  for (i :: <integer> from 0 below lev) format-out(" | ") end;
  apply(format-out, line, args);
  format-out("\n")
end method;


define method wline (line :: <string>, #rest args)
  apply(format-out, line, args);
  format-out("\n")
end method;


define method disp (x :: <object>, lev :: <integer>)
  disp-line(lev, "Terminal: %=", x)
end method;


define method disp (x :: <class-keyed-discriminator>, lev :: <integer>)
  disp-line(lev, "** class-keyed-discriminator **");
  let base :: <table-base> = grounded-class-keyed-discriminator-table-base(x);
  let siz :: <integer> = %ckd-size(x, base);
  disp-line(lev, "%s on arg %=, size = %=", debug-name(object-class(x)), discriminator-argnum(x), siz);
  let lev :: <integer> = lev + 1;
  for (i :: <integer> from 0 below siz by 2)
    let k = %ckd-ref(x, base, i);
    // unless (k == $ckd-empty)
      disp-line(lev, "%=, %=", k, instance?(k, <integer>) & implementation-class-from-key(k));
      disp(%ckd-ref(x, base, i + 1), lev)
    // end unless
  end for;
  disp-line(lev, "--End table body");
  if (instance?(x, <by-singleton-class-discriminator>))
    disp-line(lev, "Default:");
    disp(grounded-class-keyed-discriminator-default(x), lev)
  end if
end method;


define method disp (x :: <singleton-discriminator>, lev :: <integer>)
  disp-line(lev, "** singleton-discriminator **");
  disp-line(lev, "%s on arg %=", debug-name(object-class(x)), discriminator-argnum(x));
  let table :: <simple-object-vector> = singleton-discriminator-table(x);
  let siz :: <integer> = size(table);
  let lev :: <integer> = lev + 1;
  for (i :: <integer> from 0 below siz by 2)
    let k = vector-element(table, i);
    unless (k == $absent-engine-node)
      disp-line(lev, "%=", k);
      disp(vector-element(table, i + 1), lev)
    end unless
  end for;
  disp-line(lev, "Default:");
  disp(singleton-discriminator-default(x), lev)
end method;

define method disp (x :: <if-type-discriminator>, lev :: <integer>)
  disp-line(lev, "** if-type-discriminator **");
  disp-line(lev, "If type of argument %= is %=,", discriminator-argnum(x), if-type-discriminator-type(x));
  let lev :: <integer> = lev + 1;
  disp-line(lev, "Then:");
  disp(if-type-discriminator-then(x), lev);
  disp-line(lev, "Else:");
  disp(if-type-discriminator-else(x), lev)
end method;

define method disp (x :: <typecheck-discriminator>, lev :: <integer>)
  disp-line(lev, "** typecheck-discriminator **");
  disp-line(lev, "Check argument %= of type %=, then:", 
	    discriminator-argnum(x), typecheck-discriminator-type(x));
  disp(typecheck-discriminator-next(x), lev + 1)
end method;


define method disp (x :: <cache-header-engine-node>, lev :: <integer>)
  disp-line(lev, "** Cache header, %s, %= **", 
	    if (pointer-id?(engine-node-entry-point(x), 
			    engine-node-entry-point($absent-engine-node)))
	      "set to trap"
	    else
	      "valid"
	    end if,
	    x);
  disp(cache-header-engine-node-next(x), lev + 1)
end method;




// define constant make-gf
//     = method (#rest methods) => (g :: <generic-function>);
// 	let m :: <method> = first(methods);
// 	let g :: <generic-function> 
// 	  = make(<generic-function>,
// 		 required: make(<simple-object-vector>,
// 				size: signature-number-required(function-signature(m)),
// 				fill: <object>),
// 		 values: #[],
// 		 rest-value: <object>);
// 	apply(screw-gf, g, methods)
//     end method;


define constant screw-gf
    = method (g :: <generic-function>, #rest methods) => (g :: <generic-function>);
	let l :: <list> =  as(<list>, methods);
	generic-function-methods(g) := l;
	discriminator(g) := $absent-engine-node;
	primitive-set-generic-function-entrypoints(g);
	g
      end method;



define constant reset-gf
  = method (g :: <generic-function>)
      discriminator(g) := $absent-engine-node;
      primitive-set-generic-function-entrypoints(g);
      g
    end method;
