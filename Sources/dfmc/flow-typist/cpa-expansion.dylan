Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// we punt when the cross product will contain more `elements' than this.
define variable $mega-morphic-threshold = 32;  // Let's be conservative for now...

define constant <CPA-expansion> = limited(<simple-vector>, of: <simple-vector>);

define function CPA-expand-args (args :: <arg-types>)
  => (r :: false-or(<CPA-expansion>), reason)
  block (exit)
    if (args.size = 0)
      exit(vector(args), ok:)
    end;
    local method expand-type (t)
            select(t by instance?)
              <&union> => t.^union-members;
              otherwise => vector(t);
            end;
        end;
    let types = map-as(<simple-object-vector>, expand-type, args);
    let cross-product-size = reduce1(\*,map(size, types));

    if (*KM-FLAG*)
      format-out("Expansion of %= has size %=\n", args, cross-product-size * args.size);
    end;

    if ((cross-product-size * args.size) > $mega-morphic-threshold)
      values(#f, megamorphic:);
    else
      let cross-product :: <CPA-expansion> 
        = make(<CPA-expansion>, 
    	       size: cross-product-size,
	       fill: #[]);

      for (i from 0 below cross-product-size)
        cross-product[i] := make(<simple-object-vector>, size: args.size);
      end;

    /*
      // here`s a nice alternative way to do the above - if only....
      let make-product = method (x) make(<simple-object-vector>, size: args.size) end;
      let cross-procuct :: <CPA-expansion> =
        map-as(<CPA-expansion>, 
               make-product,
               range(from: 0, below: cross-product-size));
     */

      let repeat-count = 1;
      for (i from 0 below args.size)
        let s = types[i].size;
        let next-repeat-count = repeat-count * s;
         for (j from 0 below cross-product-size by repeat-count,
              t from 0)
           let t = modulo(t, s);
           for (k from 0 below repeat-count)
             let type = types[i][t];
             if (^subtype?(type, typist-<bottom-type>()))
               exit(#f, bottom:);
             elseif (instance?(type, <unknown-type>))
               cross-product[j + k][i] := typist-<object-type>();
             else
               cross-product[j + k][i] := type;
             end;
          end;
        end;
        repeat-count := next-repeat-count;
      end;
      values(cross-product, ok:);
    end;
  end;
end;

define inline function check-product 
  (sig-required :: <simple-vector>, prod :: <arg-types>, required-size :: <integer>) 
  => (r :: false-or(<arg-types>))
  let result = copy-sequence(prod);
  for (i from 0 below required-size)
    if(^subtype?(prod[i], sig-required[i]))
      result[i] := prod[i];
    elseif (guaranteed-joint?(prod[i], sig-required[i]))
      result[i] := sig-required[i];
    else
      result[i] := prod[i];
    end;
  end;
  result;
end;


define inline function constrain-product 
  (sig-required :: <simple-vector>, prod :: <arg-types>) 
  => (r :: false-or(<arg-types>))
  let result = copy-sequence(prod);
  for (i from 0 below sig-required.size)
    unless(^subtype?(prod[i], sig-required[i]))
      result[i] := sig-required[i];
    end;
  end;
  result;
end;



