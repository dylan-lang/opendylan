Module: gabriel-benchmarks
Author: Carl Gay
Synopsis: PUZZLE - Converted from Common Lisp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant puzzle-size = 511;
define constant puzzle-classmax = 3;
define constant puzzle-typemax = 12;

define variable **iii** :: <integer> = 0;
define variable **kount** :: <integer> = 0;
define variable puzzle-d :: <integer> = 8;

define constant <puzzle-intvector> = limited(<vector>, of: <integer>);
define constant <puzzle-array>
  = limited(<array>, dimensions: list(puzzle-typemax + 1, puzzle-size + 1));

define variable piececount :: <puzzle-intvector>
  = make(<puzzle-intvector>, size: puzzle-classmax + 1, fill: 0);
define variable puzzle-class :: <puzzle-intvector>
  = make(<puzzle-intvector>, size: puzzle-typemax + 1, fill: 0);
define variable piecemax :: <puzzle-intvector>
  = make(<puzzle-intvector>, size: puzzle-typemax + 1, fill: 0);
define variable puzzle :: <vector>
  = make(<vector>, size: puzzle-size + 1);
define variable puzzle-p :: <puzzle-array>
  = make(<puzzle-array>,
	 dimensions: list(puzzle-typemax + 1, puzzle-size + 1),
	 fill: #f);

define function fit (i :: <integer>, j :: <integer>)
  block (return)
    let fin = piecemax[i];
    for (k from 0 to fin)
      if (puzzle-p[i, k] & puzzle[j + k])
	return(#f);
      end if;
    end for;
    #t
  end block;
end function fit;

define function place (i :: <integer>, j :: <integer>)  // => (i :: <integer>)
  let fin = piecemax[i];
  for (k from 0 to fin)
    if (puzzle-p[i, k])
      puzzle[j + k] := #t;
    end if;
  end for;
  piececount[puzzle-class[i]] := piececount[puzzle-class[i]] - 1;
  block (return)
    for (k from j to puzzle-size)
      if (~puzzle[k])
	return(k);
      end if;
    finally
      format-out("Puzzle filled\n");
      0
    end for
  end block
end function place;

define function puzzle-remove (i :: <integer>, j :: <integer>)
  let fin = piecemax[i];
  for (k from 0 to fin)
    if (puzzle-p[i, k])
      puzzle[j + k] := #f;
    end if;
    piececount[puzzle-class[i]] := piececount[puzzle-class[i]] + 1;
  end for;
  #f
end function puzzle-remove;

define function trial (j :: <integer>)   // => (b :: <boolean>)
  let k :: <integer> = 0;
  block (return)
    for (i from 0 to puzzle-typemax)
      if (piececount[puzzle-class[i]] ~= 0)
        if (fit(i, j))
          k := place(i, j);
          if (trial(k) | k = 0)
            **kount** := **kount** + 1;
            return(#t);
          else
            puzzle-remove(i, j);
          end if;
        end if;
      end if;
    finally
      **kount** := **kount** + 1;
      #f
    end for
  end block
end function trial;

define function definepiece
    (iclass :: <integer>, ii :: <integer>, jj :: <integer>, kk :: <integer>)
  let index :: <integer> = 0;
  for (i from 0 to ii)
    for (j from 0 to jj)
      for (k from 0 to kk)
	index := i + (puzzle-d * (j + (puzzle-d * k)));
	puzzle-p[**iii**, index] := #t;
      end for;
    end for;
  end for;
  puzzle-class[**iii**] := iclass;
  piecemax[**iii**] := index;
  if (**iii** ~= puzzle-typemax)
    **iii** := **iii** + 1;
  end if;
end function definepiece;

define function puzzle-start ()
  for (m from 0 to puzzle-size)
    puzzle[m] := #t;
  end for;
  for (i from 1 to 5)
    for (j from 1 to 5)
      for (k from 1 to 5)
	puzzle[i + (puzzle-d * (j + (puzzle-d * k)))] := #f;
      end for;
    end for;
  end for;
  for (i from 0 to puzzle-typemax)
    for (m from 0 to puzzle-size)
      puzzle-p[i, m] := #f;
    end for;
  end for;
  **iii** := 0;
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
  
  piececount[0] := 13;
  piececount[1] := 3;
  piececount[2] := 1;
  piececount[3] := 1;

  // In the Common Lisp version **kount** gets dynamically bound
  // to zero here, but there appears to be no point to it other
  // than to reset **kount** to its old value on exit.  There's
  // no need for this in Dylan.
  let m :: <integer> = 1 + (puzzle-d * (1 + puzzle-d));
  let n :: <integer> = 0;
  if (fit(0, m))
    n := place(0, m);
  else
    format-out("Error.\n");
  end if;
  if (trial(n))
    format-out("Success in %d trials.\n", **kount**);
  else
    format-out("Failure.\n");
  end if;
end function puzzle-start;

define function testpuzzle ()
  puzzle-start();
end function testpuzzle;

define benchmark puzzle = testpuzzle;
