Module: gabriel-benchmarks
Author: Carl Gay
Synopsis: PUZZLE - Converted from Common Lisp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// PUZZLE -- Forest Baskett's Puzzle benchmark, originally written in
// Pascal. Solves a combinatorial puzzle by exploring the search
// space, using arrays to represent state.

define constant $puzzle-size :: <integer> = 511;
define constant $puzzle-classmax :: <integer> = 3;
define constant $puzzle-dee :: <integer> = 8;
define constant $puzzle-typemax :: <integer> = 12;

define variable *iii* :: <integer> = 0;
define variable *kount* :: <integer> = 0;

define constant <puzzle-intvector> = limited(<vector>, of: <integer>);
define constant <puzzle-array>
  = limited(<array>, dimensions: list($puzzle-typemax + 1, $puzzle-size + 1));

define variable *piece-count* :: <puzzle-intvector>
  = make(<puzzle-intvector>, size: $puzzle-classmax + 1, fill: 0);
define variable *puzzle-class* :: <puzzle-intvector>
  = make(<puzzle-intvector>, size: $puzzle-typemax + 1, fill: 0);
define variable *piecemax* :: <puzzle-intvector>
  = make(<puzzle-intvector>, size: $puzzle-typemax + 1, fill: 0);
define variable *puzzle* :: <vector>
  = make(<vector>, size: $puzzle-size + 1);
define variable *puzzle-p* :: <puzzle-array>
  = make(<puzzle-array>,
	 dimensions: list($puzzle-typemax + 1, $puzzle-size + 1),
	 fill: #f);

define function fit (i :: <integer>, j :: <integer>)
  block (return)
    for (k from 0 to *piecemax*[i])
      *puzzle-p*[i, k] & *puzzle*[j + k] & return(#f);
    finally
      #t
    end for
  end block
end function;

define function place (i :: <integer>, j :: <integer>) => (k :: <integer>)
  for (k :: <integer> from 0 to *piecemax*[i])
    if (*puzzle-p*[i, k])
      *puzzle*[j + k] := #t;
    end;
  end for;
  *piece-count*[*puzzle-class*[i]] := *piece-count*[*puzzle-class*[i]] - 1;
  block (return)
    for (k :: <integer> from j to $puzzle-size)
      unless (*puzzle*[k])
	return(k);
      end;
    finally
      //format-out("Puzzle filled\n");
      0
    end for
  end block
end function;

define function puzzle-remove (i :: <integer>, j :: <integer>)
  for (k :: <integer> from 0 to *piecemax*[i])
    if (*puzzle-p*[i, k])
      *puzzle*[j + k] := #f;
    end;
  end for;
  *piece-count*[*puzzle-class*[i]] := *piece-count*[*puzzle-class*[i]] + 1;
end function;

define function trial (j :: <integer>)
  let k :: <integer> = 0;
  block (return)
    for (i from 0 to $puzzle-typemax)
      if (*piece-count*[*puzzle-class*[i]] ~= 0)
        if (fit(i, j))
          k := place(i, j);
          if (trial(k) | k = 0)
	    *kount* := *kount* + 1;
            return(#t);
          else
            puzzle-remove(i, j);
          end if;
        end if;
      end if;
    finally
      *kount* := *kount* + 1;
      #f
    end for
  end block
end function;

define function definepiece (iclass, ii :: <integer>, jj :: <integer>,
                             kk :: <integer>)
  let index :: <integer> = 0;
  for (i from 0 to ii)
    for (j from 0 to jj)
      for (k from 0 to kk)
        index := i + $puzzle-dee * (j + $puzzle-dee * k);
        *puzzle-p*[*iii*, index] := #t;
      end for;
    end for;
  end for;
  *puzzle-class*[*iii*] := iclass;
  *piecemax*[*iii*] := index;
  if (*iii* ~= $puzzle-typemax)
    *iii* := *iii* + 1;
  end if;
end function;

define function puzzle-start ()
  for (m from 0 to $puzzle-size)
    *puzzle*[m] := #t;
  end for;
  for (i from 1 to 5)
    for (j from 1 to 5)
      for (k from 1 to 5)
	*puzzle*[i + ($puzzle-dee * (j + ($puzzle-dee * k)))] := #f;
      end for;
    end for;
  end for;
  for (i from 0 to $puzzle-typemax)
    for (m from 0 to $puzzle-size)
      *puzzle-p*[i, m] := #f;
    end for;
  end for;
  *iii* := 0;
  definepiece(0, 3, 1, 0);
  definepiece(0, 1, 0, 3);
  definepiece(0, 0, 3, 1);
  definepiece(0, 1, 3, 0);
  definepiece(0, 3, 0, 1);
  definepiece(0, 0, 1, 3);
  
  definepiece(1, 2, 0, 0);
  definepiece(1, 0, 2, 0);
  definepiece(1, 0, 0, 2);
  
  definepiece(2, 1, 1, 0);
  definepiece(2, 1, 0, 1);
  definepiece(2, 0, 1, 1);
  
  definepiece(3, 1, 1, 1);
  
  *piece-count*[0] := 13;
  *piece-count*[1] := 3;
  *piece-count*[2] := 1;
  *piece-count*[3] := 1;

  let m :: <integer> = 1 + ($puzzle-dee * (1 + $puzzle-dee));
  let n :: <integer> = 0;
  *kount* := 0;
  if (fit(0, m))
    n := place(0, m);
  else
    format-out("Error.\n");
  end if;
  if (trial(n))
    #t //format-out("Success in %d trials.\n", *kount*);
  else
    format-out("Failure.\n");
  end if;
end function;

define benchmark puzzle-benchmark ()
  benchmark-repeat (iterations: 30)
    puzzle-start();
  end;
  assert-equal(2005, *kount*);
end benchmark;
