Module:       reversi
Author:       Andy Armstrong
Synopsis:     Reversi game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Algorithms

define constant $algorithms = make(<table>);

define method install-algorithm 
    (algorithm :: <algorithm>, title :: <string>) => ()
  $algorithms[algorithm] := title
end method install-algorithm;

define constant $default-algorithm
  = #"minimize-opponents-choices-aware-of-corners";
define constant $default-white-algorithm = $default-algorithm;
define constant $default-black-algorithm = $default-algorithm;

define method default-algorithm-for-player 
    (player :: <player>) => (algorithm :: <algorithm>)
  select (player)
    #"white" => $default-white-algorithm;
    #"black" => $default-black-algorithm;
  end
end method default-algorithm-for-player;


/// Random player

install-algorithm(#"random", "Random");

define method choose-move-for-player
    (algorithm == #"random",
     board :: <reversi-board>, player :: <player>, all-moves :: <sequence>)
 => (move :: false-or(<sequence>))
  ignore(player);
  let no-of-moves = size(all-moves);
  if (no-of-moves > 0)
    all-moves[random(no-of-moves)]
  end
end method choose-move-for-player;


/// 'Most pieces' player

install-algorithm(#"most-pieces", "Most Pieces");

define method choose-move-for-player
    (algorithm == #"most-pieces",
     board :: <reversi-board>, player :: <player>, all-moves :: <sequence>)
 => (move :: false-or(<sequence>))
  let best-length = max-size(all-moves);
  let best-moves 
    = choose(method (move)
	       size(move) = best-length
	     end,
	     all-moves);
  choose-move-for-player(#"random", board, player, best-moves)
end method choose-move-for-player;

define method max-size 
    (sequence :: <sequence>) => (max-size :: <integer>)
  reduce(method(total, x) 
	   max(total, size(x))
	 end,
	 0, sequence)
end method max-size;


/// 'Least pieces' player

install-algorithm(#"least-pieces", "Least Pieces");

define method choose-move-for-player
    (algorithm == #"least-pieces",
     board :: <reversi-board>, player :: <player>, all-moves :: <sequence>)
 => (move :: false-or(<sequence>))
  let worst-length = min-size(all-moves);
  let best-moves
    = choose(method (move)
	       size(move) = worst-length
	     end,
	     all-moves);
  choose-move-for-player(#"random", board, player, best-moves)
end method choose-move-for-player;

define method min-size 
    (sequence :: <sequence>) => (min-size :: <integer>)
  reduce(method (total, x)
           if (total)
             min(total, size(x))
           else
             size(x)
           end
         end,
         #f, sequence)
end method min-size;


/// 'Minimize opponents choices' player

install-algorithm(#"minimize-opponents-choices", "Minimize Opponents Choices");

define method choose-move-for-player
    (algorithm == #"minimize-opponents-choices",
     board :: <reversi-board>, player :: <player>, all-moves :: <sequence>)
 => (move :: false-or(<sequence>))
  let temp-board = make(<reversi-board>, size: reversi-board-size(board));
  let other-player = other-player(player);
  let no-of-squares = reversi-board-no-of-squares(board);
  let minimum-choices = no-of-squares;
  let best-moves = make(<stretchy-vector>);
  for (move in all-moves)
    copy-board(temp-board, board);
    perform-move(temp-board, player, move);
    let no-of-moves
      = number-of-possible-moves-for-player(temp-board, other-player);
    if (no-of-moves < minimum-choices)
      best-moves.size := 0;
      minimum-choices := no-of-moves;
    end;
    if (no-of-moves = minimum-choices)
      best-moves := add!(best-moves, move)
    end
  end;
  choose-move-for-player(#"random", board, player, best-moves)
end method choose-move-for-player;


/// 'Corner aware algorithm' players

define method play-algorithm-aware-of-corners
    (algorithm :: <algorithm>, board :: <reversi-board>, player :: <player>,
     all-moves :: <sequence>)
 => (move :: false-or(<sequence>))
  choose-move-for-player(algorithm, board, player, 
			 all-corner-moves(board, all-moves))
    | choose-move-for-player(algorithm, board, player,
			     all-but-next-to-untaken-corner-moves
			       (board, player, all-moves))
    | choose-move-for-player(algorithm, board, player, all-moves)
end method play-algorithm-aware-of-corners;

install-algorithm(#"most-pieces-aware-of-corners",
		  "Most Pieces Aware Of Corners");
install-algorithm(#"least-pieces-aware-of-corners", 
		  "Least Pieces Aware Of Corners");
install-algorithm(#"minimize-opponents-choices-aware-of-corners",
		  "Minimize Opponents Choices Aware Of Corners");

define method choose-move-for-player
    (algorithm == #"most-pieces-aware-of-corners",
     board :: <reversi-board>, player :: <player>, all-moves :: <sequence>)
 => (move :: false-or(<sequence>))
  play-algorithm-aware-of-corners(#"most-pieces", board, player, all-moves)
end method choose-move-for-player;

define method choose-move-for-player
    (algorithm == #"least-pieces-aware-of-corners",
     board :: <reversi-board>, player :: <player>, all-moves :: <sequence>)
 => (move :: false-or(<sequence>))
  play-algorithm-aware-of-corners(#"least-pieces", board, player, all-moves)
end method choose-move-for-player;

define method choose-move-for-player
    (algorithm == #"minimize-opponents-choices-aware-of-corners",
     board :: <reversi-board>, player :: <player>, all-moves :: <sequence>)
 => (move :: false-or(<sequence>))
  play-algorithm-aware-of-corners
    (#"minimize-opponents-choices", board, player, all-moves)
end method choose-move-for-player;
