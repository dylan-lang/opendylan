Module:       quicksort
Synopsis:     Quicksort demo program
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// To see the different optimizations performed on each version of quicksort
// in this file, compile this example in "production mode" and then select
// Color Dispatch Optimizations from the editor's View menu.  See the
// Harlequin Dylan documentation for an explanation of the colors.

// No type declarations at all
define method sequence-quicksort
    (v :: <sequence>) => (sorted-v :: <sequence>)
  local method exchange (m, n) => ()
	  let t = v[m];
	  v[m] := v[n];
	  v[n] := t
	end method exchange,
        method partition (lo, hi, x) => (i, j)
	  let i = for (i from lo to hi, while: v[i] < x)
		  finally i
		  end;
	  let j = for (j from hi to lo by -1, while: x < v[j])
		  finally j
		  end;
	  if (i <= j)
	    exchange(i, j);
	    partition(i + 1, j - 1, x)
	  else
	    values(i, j)
	  end
	end method partition,
        method sort (lo, hi) => ()
	  if (lo < hi)
	    let (i, j) = partition(lo, hi, v[round/(lo + hi, 2)]);
	    sort(lo, j);
	    sort(i, hi)
	  end
	end method sort;
  sort(0, v.size - 1);
  v
end method sequence-quicksort;

// With type declarations for the sequence and its indices
define method sequence-quicksort-typed
    (v :: <sequence>) => (sorted-v :: <sequence>)
  local method exchange (m :: <integer>, n :: <integer>) => ()
	  let t = v[m];
	  v[m] := v[n];
	  v[n] := t
	end method exchange,
        method partition
            (lo :: <integer>, hi :: <integer> , x)
         => (i :: <integer>, j :: <integer>)
	  let i :: <integer>
            = for (i :: <integer> from lo to hi, while: v[i] < x)
              finally i
              end;
	  let j :: <integer>
            = for (j :: <integer> from hi to lo by -1, while: x < v[j])
              finally j
              end;
	  if (i <= j)
	    exchange(i, j);
	    partition(i + 1, j - 1, x)
	  else
	    values(i, j)
	  end
	end method partition,
        method sort (lo :: <integer>, hi :: <integer>) => ()
	  when (lo < hi)
	    let (i, j) = partition(lo, hi, v[round/(lo + hi, 2)]);
	    sort(lo, j);
	    sort(i, hi)
	  end;
	end method sort;
  sort(0, v.size - 1);
  v
end method sequence-quicksort-typed;

define constant <integer-vector> = limited(<vector>, of: <integer>);

// Non-polymorphic version -- only sorts vectors of integers
define method integer-vector-quicksort
    (v :: <integer-vector>) => (sorted-v :: <integer-vector>)
  local method exchange (m :: <integer>, n :: <integer>) => ()
	  let t = v[m];
	  v[m] := v[n];
	  v[n] := t
	end method exchange,
        method partition
	    (lo :: <integer>, hi :: <integer> , x :: <integer>)
	 => (i :: <integer>, j :: <integer>)
	  let i :: <integer>
	    = for (i :: <integer> from lo to hi, while: v[i] < x)
	      finally i
	      end;
	  let j :: <integer>
	    = for (j :: <integer> from hi to lo by -1, while: x < v[j])
	      finally j
	      end;
	  if (i <= j)
	    exchange(i, j);
	    partition(i + 1, j - 1, x)
	  else
	    values(i, j)
	  end
	end method partition,
        method sort (lo :: <integer>, hi :: <integer>) => ()
	  when (lo < hi)
	    let (i, j) = partition(lo, hi, v[round/(lo + hi, 2)]);
	    sort(lo, j);
	    sort(i, hi)
	  end;
	end method sort;
  sort(0, v.size - 1);
  v
end method integer-vector-quicksort;

// Non-polymorphic version -- only sorts vectors of integers
// It's unsafe because it defeats bounds-checking, too
define method unsafe-integer-vector-quicksort
    (v :: <integer-vector>) => (sorted-v :: <integer-vector>)
 without-bounds-checks
  local method exchange (m :: <integer>, n :: <integer>) => ()
          let t = v[m];
          v[m] := v[n];
          v[n] := t
        end method exchange,
        method partition
            (lo :: <integer>, hi :: <integer> , x :: <integer>)
         => (i :: <integer>, j :: <integer>)
          let i :: <integer>
            = for (i :: <integer> from lo to hi, while: v[i] < x)
              finally i
              end;
          let j :: <integer>
            = for (j :: <integer> from hi to lo by -1, while: x < v[j])
              finally j
              end;
          if (i <= j)
            exchange(i, j);
            partition(i + 1, j - 1, x)
          else
            values(i, j)
          end
        end method partition,
        method sort (lo :: <integer>, hi :: <integer>) => ()
          when (lo < hi)
            let (i, j) = partition(lo, hi, v[round/(lo + hi, 2)]);
            sort(lo, j);
            sort(i, hi)
          end;
        end method sort;
  sort(0, v.size - 1);
 end;
 v
end method unsafe-integer-vector-quicksort;

define method main () => ()
  let args = application-arguments();
  let data = vector("The quick brown fox jumps over the lazy dog.",
                    vector("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog."),
                    vector('t', 'q', 'b', 'f', 'j', 'o', 't', 'l', 'd', '.'),
                    vector(17, 42, 3, 2, 7, 6, 8));
  // Show off polymorphism...
  map(method (v)
        display-sequence(v);
        format-out(" sorted is ");
        display-sequence(sequence-quicksort(v));
        format-out("\n");
      end,
      data);
  // Show differences in speed...
  let default-size = 50000;
  local method warn-and-default () => (default-size)
          format-out("*** Invalid argument specified.  Using default value. ***\n");
          default-size
        end;
  let n = if (args.size > 0)
	    block ()
              let x = string-to-integer(application-arguments()[0]);
              if (x < 0)
                warn-and-default()
              else
                x
              end
            exception (<error>)
              warn-and-default()
            end block
          else
            default-size
          end;
  let orig :: <integer-vector> = make(<integer-vector>, size: n);
  let data :: <integer-vector> = make(<integer-vector>, size: n);
  for (i :: <integer> from 0 below n)
    orig[i] := random($maximum-integer);
  end;
  for (function in vector(sequence-quicksort,
                          sequence-quicksort-typed,
                          integer-vector-quicksort,
                          unsafe-integer-vector-quicksort),
       typename in #["<sequence>", "<sequence> (with type decls)",
                     "<integer-vector>", "<integer-vector> (unsafe)"])
    map-into(data, identity, orig);	// copy unsorted data
    format-out("Sorting a %d element %s... ", n, typename);
    let (seconds, microseconds) = timing ()
                                    function(data);
                                  end;
    format-out("took %d.%s seconds\n",
               seconds, integer-to-string(microseconds, size: 6));
  end;
end method main;

// Display a sequence nicely
define method display-sequence (s :: <sequence>)
  format-out("#[");
  let length :: <integer> = size(s);
  for (elem in s, i from 1)
    format-out("%s%s", elem, if (i < length) ", " else "" end);
  end for;
  format-out("]");
end;

// Strings already display nicely, so don't make them worse.
define method display-sequence (s :: <string>)
  format-out("%=", s);
end;

begin
  main();
end;


