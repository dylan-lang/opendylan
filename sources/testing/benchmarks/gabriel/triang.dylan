Module: gabriel-benchmarks
Author: Carl Gay
Synopsis: TRIANG - Converted from Common Lisp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// TRIANG -- Board game benchmark.

// Notes on the conversion from Common Lisp to Dylan:
//
// - I didn't use dynamically scoped variables, since they didn't seem
//   necessary.

define constant <intvec> = limited(<vector>, of: <integer>);

define constant $triang-board :: <intvec>
  = make(<intvec>, size: 16, fill: 1);

define constant $triang-sequence :: <intvec>
  = make(<intvec>, size: 14, fill: 0);

define function listtovec (l :: <list>) => (v :: <intvec>)
  let v :: <intvec> = make(<intvec>, size: size(l));
  map-into(v, identity, l)
end;

define constant $triang-a :: <intvec>
  = listtovec(#(1, 2, 4, 3, 5, 6, 1, 3, 6, 2, 5, 4,
		11, 12, 13, 7, 8, 4, 4, 7, 11,
		8, 12, 13, 6, 10, 15, 9, 14,
		13, 13, 14, 15, 9, 10, 6, 6));

define constant $triang-b :: <intvec>
  = listtovec(#(2, 4, 7, 5, 8, 9, 3, 6, 10, 5,
		9, 8, 12, 13, 14, 8, 9, 5,
		2, 4, 7, 5, 8, 9, 3, 6, 10, 5,
		9, 8, 12, 13, 14, 8, 9, 5, 5));

define constant $triang-c :: <intvec>
  = listtovec(#(4, 7, 11, 8, 12, 13, 6, 10,
		15, 9, 14, 13, 13, 14,
		15, 9, 10, 6, 1, 2, 4, 3, 5, 6,
		1, 3, 6, 2, 5, 4, 11, 12, 13,
		7, 8, 4, 4));


define thread variable *triang-answer* = #();

define thread variable *triang-final* = #();

begin
  $triang-board[5] := 0;
end;

define function triang-last-position ()
  block (return)
    for (i from 1 to 15)
      if ($triang-board[i] == 1)
	return(i);
      end if;
    end for;
    0
  end block
end function triang-last-position;

define function triang-try (i :: <integer>, depth :: <integer>)
  if (depth == 14)
    let lp = triang-last-position();
    unless (member?(lp, *triang-final*))
      *triang-final* := pair(lp, *triang-final*);
    end unless;
    *triang-answer* := pair(copy-sequence($triang-sequence, start: 1),
			    *triang-answer*);
    #t
  elseif ($triang-board[$triang-a[i]] == 1
          & $triang-board[$triang-b[i]] == 1
	  & $triang-board[$triang-c[i]] == 0)
    $triang-board[$triang-a[i]] := 0;
    $triang-board[$triang-b[i]] := 0;
    $triang-board[$triang-c[i]] := 1;
    $triang-sequence[depth] := i;
    let depth = depth + 1;
    for (j from 0 to 35,
	 until: triang-try(j, depth))
    end for;
    $triang-board[$triang-a[i]] := 1;
    $triang-board[$triang-b[i]] := 1;
    $triang-board[$triang-c[i]] := 0;
    #f
  end if
end function triang-try;

define function gogogo (i :: <integer>)
  dynamic-bind (*triang-answer* = #(),
                *triang-final* = #())
//  *triang-answer* := #();
//  *triang-final* := #();
    triang-try(i, 1);
  end;
end function gogogo;

define function testtriang ()
  gogogo(22);
end function testtriang;

define benchmark triang = testtriang;
