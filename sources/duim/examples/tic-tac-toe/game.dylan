Module:       tic-tac-toe
Author:       Jonathon Lee
Synopsis:     The classic game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *computer-player* :: <integer> = 2;
define variable *human-player1* :: <integer> = 1;
define variable *human-player2* :: <integer> = 1;

define constant <game-status>
  = type-union(singleton(#f), singleton(#"win"), singleton(#"draw"));

define class <tree> (<collection>)
  constant slot game :: false-or(<ttt-game>) = #f,
    init-keyword: game:;
  slot value :: false-or(<integer>) = #f,
    init-keyword: value:;
  slot children :: <array>;
end class <tree>;

define method initialize (tree :: <tree>, #key) => ()
  next-method();
  tree.children := make(<array>, dimensions: #(3, 3), of: false-or(<tree>));
  for (i from 0 below 3)
    for (j from 0 below 3)
      tree.children[i, j] := #f;
    end for;
  end for;
end method initialize;

define constant <ttt-board>
  = limited(<array>,  
	    dimensions: list($ttt-square-rows, $ttt-square-columns),
	    of: false-or(<ttt-square>)); 

define class <ttt-game> (<object>)
  slot ttt-board :: false-or(<ttt-board>) = #f,
    init-keyword: ttt-squares:;
  slot turn  :: limited(<integer>, min: 1, max: 2) = 1,
    init-keyword: turn:;
  slot moves :: limited(<integer>, min: 0, max: 9) = 0,
    init-keyword: moves:;
end class <ttt-game>;

define method make-ttt-board () => (squares :: <ttt-board>)
  let squares = make(<ttt-board>,
		     dimensions: list($ttt-square-rows, $ttt-square-columns),
		     fill: #f);
  for (i from 0 below $ttt-square-rows)
    for (j from 0 below $ttt-square-columns)
      squares[i, j] := make(<ttt-square>,
                            square-coordinates: vector(i, j));
    end for;
  end for;
  squares
end method make-ttt-board;

define method heuristic 
    (board :: <ttt-board>, moves :: <integer>, game-status :: <game-status>, 
     player :: <integer>)
  if (game-status = #"win") 
    if (player = *computer-player*)
      1;
    else 
      -1;
    end if
  elseif (game-status = #"draw")
    0;
  else
    debug-message("hahahaha");
  end if;
end method heuristic;
 
define method move
    (board :: <ttt-board>, position :: <vector>, player :: <integer>) => ()
  let i = position[0];
  let j = position[1];
  board[i, j].square-occupied := player;
end method move;

define method undo-move
    (board :: <ttt-board>, position :: <vector>) => ()
  let i = position[0];
  let j = position[1];
  board[i, j].square-occupied := #f;
end method undo-move;

define method copy-game
    (old :: <ttt-board>) => (new :: <ttt-board>) 
  let temp = make(<ttt-board>,
		  dimensions: list($ttt-square-rows, $ttt-square-columns),
		  fill: #f);
  temp := make-ttt-board();
  for (i from 0 below 3)
    for (j from 0 below 3)
      temp[i, j].square-occupied := old[i, j].square-occupied;
      temp[i, j].square-coordinates := old[i, j].square-coordinates;
    end for;
  end for;
  temp;
end method copy-game;

define method get-max-child-value
    (tree :: <tree>) => (value :: <integer>)
  let max = -10000000;
  for (i from 0 below 3)
    for (j from 0 below 3)
      let child = tree.children[i, j];
      if (child)
	if (child.value > max)
	  max := child.value;
	end if;
      end if;
    end for;
  end for; 
  max;
end method get-max-child-value;

define method get-min-child-value
    (tree :: <tree>) => (value :: <integer>)
  let min = 10000000;
  for (i from 0 below 3)
    for (j from 0 below 3)
      let child = tree.children[i, j];
      if (child)
	if (child.value < min)
	  min := child.value;
	end if;
      end if;
    end for;
  end for;
  min;
end method get-min-child-value;

define method find-winner
    (board :: <ttt-board>, p :: <integer>) => (winner? :: <boolean>)
  let ans :: <boolean> = #f;
  let i = 0;
  let j = 0;

  while ((i < 3) & ~ans) 
    ans := row-winner?(board, vector(i, 0), p); 
    i := i + 1;
  end while;

  while ((j < 3) & ~ans)
    ans := col-winner?(board, vector(0, j), p); 
    j := j + 1;
  end while;

  if (~ans)
    ans := up-diagonal-winner?(board, vector(2, 0), p); 
  end if;

  if (~ans)
    ans := down-diagonal-winner?(board, vector(0, 0), p); 
  end if;
  ans;
end method find-winner;  

define method end-state? 
    (board :: <ttt-board>, moves :: <integer>, player :: <integer>)
 => (game-status :: <game-status>)
  case
    find-winner(board, player)   => #"win";
    moves = $max-number-of-moves => #"draw";
    otherwise                    => #f;
  end
end method end-state?;
  
define method build-tree
    (board :: <ttt-board>, player :: <integer>, moves :: <integer>)
 => (game-tree :: <tree>)
  let root = make(<tree>, game: copy-game(board));
  let other-player = select(player)
		       1 => 2;
		       2 => 1;
		     end;
  let over = end-state?(root.game.ttt-board, moves, other-player);
  if (over)
    root.value := heuristic(root.game.ttt-board, moves, over, other-player);
  else 
    for (i from 0 below $ttt-square-rows) 
      for (j from 0 below $ttt-square-columns)
	if (~board[i, j].square-occupied)
	  move(root.game.ttt-board, vector(i, j), player);
	  root.children[i, j] := build-tree(root.game.ttt-board,
					    other-player, moves + 1);
	  undo-move(root.game.ttt-board, vector(i, j));
	end if;
      end for;
    end for;
	      
    if (player = *computer-player*)
      root.value := get-max-child-value(root);
    else
      root.value := get-min-child-value(root);
    end if;
  end if;
  values(root);
end method build-tree;

define method find-good-move
    (tree :: <tree>) => (i :: <integer>, j :: <integer>)
  let loopX = 0 ;
  let loopY = 0;
  let equal = #f;
  let my-value = tree.value;
  let good = vector(0, 0);
  while ( (loopX < 3) & ~equal ) 
    while ( (loopY < 3) & ~equal )
      let this-child = tree.children[loopX, loopY]; 
      if (this-child)
	if (this-child.value = my-value)
	  if (~equal) 
	    good[0] := loopX;
	    good[1] := loopY;
	    equal := #t;
	  end if;
	end if;
      end if;
      if (~equal)
	loopY := loopY + 1;
      end if;
    end while;
    if (~equal)
      loopY := 0;
      loopX := loopX + 1;
    end if;
  end while;
  values( good[0], good[1]);
end method find-good-move;

define method tie-game? ( moves :: <integer> ) => ( tie :: <boolean> )
  moves = $max-number-of-moves;
end method tie-game?;

define method on-down-diagonal?( move :: <vector> ) => (b :: <boolean>)
  let row = first(move);
  let col = second(move);
  row = col;
end method on-down-diagonal?;
    
define method on-up-diagonal?( move :: <vector> ) => (b :: <boolean>)
  let row = first(move);
  let col = second(move);
  row + col = 2;
end method on-up-diagonal?;

define method down-diagonal-winner?
    (board :: <ttt-board>, move :: <vector>, player :: <integer>) 
 => (winner? :: <boolean>)
  (square-occupied(board[0, 0]) = square-occupied(board[1, 1]))
    & (square-occupied(board[1, 1]) = square-occupied(board[2, 2]))
    & (square-occupied(board[2, 2]) = player);
end method down-diagonal-winner?;

define method up-diagonal-winner?
    (board :: <ttt-board>, move :: <vector>, player :: <integer>) 
 => (winner? :: <boolean>)
  (square-occupied(board[0, 2]) = square-occupied(board[1, 1]))
    & (square-occupied(board[1, 1]) = square-occupied(board[2, 0]))
    & (square-occupied(board[2, 0]) = player);
end method up-diagonal-winner?;

define method row-winner?
    (board :: <ttt-board>, move :: <vector>, player :: <integer>) 
 => (winner? :: <boolean>)
  let row = move[0];
  (square-occupied(board[row, 0]) = square-occupied(board[row, 1]))
    & (square-occupied(board[row, 1]) = square-occupied(board[row, 2]))
    & (square-occupied(board[row, 2]) = player);
end method row-winner?;

define method col-winner?
    (board :: <ttt-board>, move :: <vector>, player :: <integer>) 
 => (winner? :: <boolean>)
  let col = move[1];
  (square-occupied(board[0, col]) = square-occupied(board[1, col]))
    & (square-occupied(board[1, col]) = square-occupied(board[2, col]))
    & (square-occupied(board[2, col]) = player);
end method col-winner?;

define method winning-move?
    (b :: <ttt-board>, s :: <ttt-square>)
 => ( won :: <boolean> )
  let last-move = s.square-coordinates;
  let player = s.square-occupied; 
  if ( on-up-diagonal?( last-move ) )
    if (up-diagonal-winner?(b, last-move, player ))
      #t;
    elseif ( on-down-diagonal?(last-move) ) 
      if (down-diagonal-winner?(b, last-move, player ))
	#t;
      else
	(row-winner?(b, last-move, player ) 
	   | col-winner?(b, last-move, player ));
      end if;
    else
      (row-winner?(b, last-move, player ) 
	 | col-winner?(b, last-move, player ));
    end if;
  elseif ( on-down-diagonal?(last-move) ) 
    if (down-diagonal-winner?(b, last-move, player ))
      #t;
    else
      (row-winner?(b, last-move, player) 
	| col-winner?(b, last-move, player));
    end if;
  else
    (row-winner?(b, last-move, player) 
      | col-winner?(b, last-move, player));
  end if;
end method winning-move?;

define method over? (g :: <ttt-game>, square :: <ttt-square>)
 => (type :: one-of(#f, #"win", #"draw"))
  if (winning-move?(g.ttt-board, square))
    #"win";
  elseif (tie-game?(g.moves))
    #"draw";
  else
    #f;
  end if;
end method over?;

define method game-computers-turn
    (game :: <ttt-game>, i :: <integer>, j :: <integer>) 
 => (type :: one-of(#f, #"win", #"draw"))
  ttt-square-setter(game.ttt-board[i, j], game.turn);  
  game.moves := game.moves + 1;
  let over = over?(game, game.ttt-board[i, j]);
  over;
end method game-computers-turn;

define method game-play-computers-move
    (game :: <ttt-game>) => (row :: <integer>, col :: <integer>)
  let moves = game.moves;
  if (moves = 0)
    values(1, 1);
  elseif (moves = 1)
    if (game.ttt-board[1, 1].square-occupied)
      values(0, 0);
    else
      values(1, 1);
    end if;
  elseif (moves = 2)
    if (game.ttt-board[0, 0].square-occupied)
      values(0, 2);
    else
      values(0, 0); 
    end if;
  else
    let tree = build-tree(game.ttt-board, *computer-player*, moves);
    let (i, j) = find-good-move(tree);
    values(i, j);
  end if;
end method game-play-computers-move;

