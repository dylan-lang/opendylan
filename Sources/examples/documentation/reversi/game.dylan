Module:    reversi
Author:    Andy Armstrong
Synopsis:  Reversi game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *changed?* :: <boolean> = #f;
define variable *changed2?* :: <boolean> = #f;

/// Player protocol

define constant <player> = one-of(#"black", #"white");
define constant <piece>  = false-or(<player>);

define method name-for-player 
    (player :: <player>) => (name :: <string>)
  select (player)
    #"white" => "White";
    #"black" => "Black";
  end
end method name-for-player;
  
define method other-player 
    (player :: <player>) => (other-player :: <player>)
  select (player)
    #"white" => #"black";
    #"black" => #"white";
  end
end method other-player;

/// Algorithm protocol

define constant <algorithm> = <symbol>;

define generic choose-move-for-player
    (algorithm :: <algorithm>, board :: <reversi-board>,
     player :: <player>, all-moves :: <sequence>)
 => (move :: false-or(<sequence>));

/// Reversi board

define constant $default-board-size = 8;

define class <reversi-board> (<object>)
  slot reversi-board-size :: <integer> = $default-board-size,
    init-keyword: size:;
  slot reversi-board-squares :: <sequence> = #[];
end class <reversi-board>;

define method reversi-board-no-of-squares 
    (board :: <reversi-board>) => (no-of-squares :: <integer>)
  let size = reversi-board-size(board);
  size * size
end method reversi-board-no-of-squares;

define method initialize (board :: <reversi-board>, #key) => ()
  next-method();
  reversi-board-squares(board)
    := make(<vector>, size: reversi-board-no-of-squares(board));
end method initialize;

define method initial-pieces
    (board :: <reversi-board>) => (pieces :: <sequence>)
  let last-pos = floor/(reversi-board-size(board), 2);
  let first-pos = last-pos - 1;
  vector
    (vector(coordinate-to-square-number(board, first-pos, first-pos),#"white"),
     vector(coordinate-to-square-number(board, last-pos, last-pos), #"white"),
     vector(coordinate-to-square-number(board, last-pos, first-pos), #"black"),
     vector(coordinate-to-square-number(board, first-pos, last-pos), #"black"))
end method initial-pieces;

define method initialize-board (board :: <reversi-board>) => ()
  let squares = reversi-board-squares(board);
  for (square from 0 below size(squares))
    squares[square] := #f
  end;
  for (piece in initial-pieces(board))
    let square = piece[0];
    squares[square] := piece[1]
  end;
end method initialize-board;

define method square-number-to-coordinate 
    (board :: <reversi-board>, coordinate :: <integer>)
 => (x :: <integer>, y :: <integer>)
  let (y, x) = floor/(coordinate, reversi-board-size(board));
  values(x, y)
end method square-number-to-coordinate;

define method coordinate-to-square-number
    (board :: <reversi-board>, x :: <integer>, y :: <integer>)
 => (coordinate :: <integer>)
  let size = reversi-board-size(board);
  x + size * y;
end method coordinate-to-square-number;

define method copy-board 
    (new-board :: <reversi-board>, old-board :: <reversi-board>)
 => (new-board :: <reversi-board>)
  let new-squares = reversi-board-squares(new-board);
  let old-squares = reversi-board-squares(old-board);
  for (i from 0 below size(old-squares))
    new-squares[i] := old-squares[i]
  end;
  new-board
end method copy-board;

define method perform-move 
    (board :: <reversi-board>, player :: <player>, move :: <sequence>) => ()
  let squares = reversi-board-squares(board);
  for (square in move)
    squares[square] := player
  end
end method perform-move;

define method corner-square? 
    (board :: <reversi-board>, square :: <integer>) => (corner? :: <boolean>)
  let size = reversi-board-size(board);
  let no-of-squares = size * size;
  square = 0
    | square = size - 1
    | square = no-of-squares - 1
    | square = no-of-squares - size
end method corner-square?;

define method in-corner-region? 
    (board :: <reversi-board>, square :: <integer>)
 => (corner-region? :: <boolean>)
  let (x, y) = square-number-to-coordinate(board, square);
  let size = reversi-board-size(board);
  case 
    x < 2 | x > size - 3 => #t;
    y < 2 | y > size - 3 => #t;
    otherwise            => #f;
  end
end method in-corner-region?;

define method next-to-corner-square? 
    (board :: <reversi-board>, square :: <integer>)
 => (next-to-corner? :: <boolean>)
  in-corner-region?(board, square)
   & ~corner-square?(board, square)
end method next-to-corner-square?;

define method nearest-corner
    (board :: <reversi-board>, square :: <integer>) => (square :: <integer>)
  let size = reversi-board-size(board);
  let half-size = floor/(size, 2);
  let (x, y) = square-number-to-coordinate(board, square);
  coordinate-to-square-number
    (board,
     if (x < half-size) 0 else size - 1 end,
     if (y < half-size) 0 else size - 1 end)
end method nearest-corner;

define method next-to-untaken-corner-square?
    (board :: <reversi-board>, player :: <player>, square :: <integer>)
 => (next-to-untaken-corner? :: <boolean>)
  let squares = reversi-board-squares(board);
  next-to-corner-square?(board, square) 
    & squares[nearest-corner(board, square)] ~= player
end method next-to-untaken-corner-square?;

define method all-corner-moves 
    (board :: <reversi-board>, all-moves :: <sequence>)
 => (corner-moves :: <sequence>)
  choose(method (move)
	   corner-square?(board, move[0])
	 end,
	 all-moves)
end method all-corner-moves; 

define method all-but-next-to-untaken-corner-moves
    (board :: <reversi-board>, player :: <player>, all-moves :: <sequence>)
 => (moves :: <sequence>)
  choose(method (move) 
          ~next-to-untaken-corner-square?(board, player, move[0])
         end,
         all-moves)
end method all-but-next-to-untaken-corner-moves;

define method map-over-squares-in-direction 
    (procedure :: <function>, board :: <reversi-board>, start :: <integer>,
     x-offset :: <integer>, y-offset :: <integer>)
 => ()
  let size = reversi-board-size(board);
  let (row, col) = square-number-to-coordinate(board, start);
  for (x from row + x-offset by x-offset,
       y from col + y-offset by y-offset,
       while: x >= 0 & x < size & y >= 0 & y < size)
    let square = coordinate-to-square-number(board, x, y);
    procedure(square);
  end
end method map-over-squares-in-direction;

define method squares-to-take-in-direction?
    (board :: <reversi-board>, player :: <player>, square :: <integer>,
     x-offset :: <integer>, y-offset :: <integer>)
 => (squares-to-take? :: <boolean>)
  let squares = reversi-board-squares(board);
  let found-opposing-piece? = #f;
  let other-player = other-player(player);
  block (return)
    map-over-squares-in-direction
      (method (square)
	 select (squares[square])
	   #f           => return(#f);
	   player       => return(found-opposing-piece?);
	   other-player => #f;
	 end;
	 found-opposing-piece? := #t
       end,
       board, square, x-offset, y-offset)
  end;
end method squares-to-take-in-direction?;

define method squares-to-take-in-direction 
    (board :: <reversi-board>, player :: <player>, square :: <integer>,
     x-offset :: <integer>, y-offset :: <integer>)
 => (squares-to-take :: <sequence>)
  let squares = reversi-board-squares(board);
  let squares-to-take = make(<stretchy-vector>);
  let other-player = other-player(player);
  block (return)
    map-over-squares-in-direction
      (method (square)
	 select (squares[square])
	   #f           => return(#[]);
	   player       => return(squares-to-take);
	   other-player => #f;
	 end;
	 add!(squares-to-take, square);
       end,
       board, square, x-offset, y-offset);
    #[]
  end;
end method squares-to-take-in-direction;

define constant $reversi-directions
  = #[#[-1, -1], #[0, -1], #[1, -1],
      #[-1, 0],             #[1, 0],
      #[-1, 1],  #[0, 1],   #[1, 1]];

define method squares-to-take 
    (board :: <reversi-board>, player :: <player>, square :: <integer>)
 => (squares-to-take :: <sequence>)
  let squares = reversi-board-squares(board);
  unless (squares[square])
    let squares-to-take = make(<stretchy-vector>);
    for (direction in $reversi-directions)
      let new-squares-to-take
        = squares-to-take-in-direction
	    (board, player, square, direction[0], direction[1]);
      for (square in new-squares-to-take)
        add!(squares-to-take, square)
      end;
    end;
    unless (empty?(squares-to-take))
      add!(squares-to-take, square);
      reverse!(squares-to-take)
    end
  end
  | #[]
end method squares-to-take;

define method squares-to-take? 
    (board :: <reversi-board>, player :: <player>, square :: <integer>)
 => (squares? :: <boolean>)
  let squares = reversi-board-squares(board);
  squares[square] = #f
    & any?(method (direction)
             squares-to-take-in-direction?(board, player, square,
					   direction[0], direction[1])
	   end,
	   $reversi-directions)
end method squares-to-take?;

define method all-possible-moves-for-player 
    (board :: <reversi-board>, player :: <player>) => (moves :: <sequence>)
  let all-moves = make(<stretchy-vector>);
  for (square from 0 below reversi-board-no-of-squares(board))
    let squares-to-take = squares-to-take(board, player, square);
    unless (empty?(squares-to-take))
      add!(all-moves, squares-to-take)
    end
  end;
  all-moves
end method all-possible-moves-for-player;

define method number-of-possible-moves-for-player
    (board :: <reversi-board>, player :: <player>)
 => (no-of-moves :: <integer>)
  let no-of-moves = 0;
  for (square from 0 below reversi-board-no-of-squares(board))
    if (squares-to-take?(board, player, square))
      no-of-moves := no-of-moves + 1
    end
  end;
  no-of-moves
end method number-of-possible-moves-for-player;

define method any-moves-for-player?
    (board :: <reversi-board>, player :: <player>)
 => (any-moves? :: <boolean>)
  block (return)
    for (square from 0 below reversi-board-no-of-squares(board))
      if (squares-to-take?(board, player, square))
        return(#t)
      end
    end;
    #f
  end
end method any-moves-for-player?;

define method game-over? 
    (board :: <reversi-board>) => (game-over? :: <boolean>)
  let squares = reversi-board-squares(board);
  every?(method (square)
	   squares[square]
           | (  ~squares-to-take?(board, #"white", square)
	      & ~squares-to-take?(board, #"black", square))
	 end,
	 range(from: 0, below: reversi-board-no-of-squares(board)))
end method game-over?;

define method score-for-player 
    (board :: <reversi-board>, player :: <player>) => (score :: <integer>)
  let score = 0;
  for (piece in board.reversi-board-squares)
    if (piece = player)
      score := score + 1;
    end
  end;
  score
end method score-for-player;

define method display-board (board :: <reversi-board>) => ()
  let squares = reversi-board-squares(board);
  let size = reversi-board-size(board);
  format-out("\n");
  for (y from 0 below size)
    for (x from 0 below size)
      let piece = squares[coordinate-to-square-number(board, x, y)];
      format-out(select (piece)
                   #"white"  => "x";
                   #"black"  => "o";
                   otherwise => "-"
                 end)
    end;
    format-out("\n")
  end
end method display-board;

/// Reversi game

define class <reversi-game> (<object>)
  slot reversi-game-board :: <reversi-board> = make(<reversi-board>);
  slot %player :: <player> = #"black",
    init-keyword: player:;
  slot %players :: <integer> = 1,
    init-keyword: players:;
  slot black-algorithm :: <algorithm> = default-algorithm-for-player(#"black"),
    init-keyword: black-algorithm:;
  slot white-algorithm :: <algorithm> = default-algorithm-for-player(#"white"),
    init-keyword: white-algorithm:;
  slot reversi-game-update-callback :: <function> = always(#f),
    init-keyword: update-callback:;
  slot reversi-game-message-function :: false-or(<function>) = #f,
    init-keyword: message-function:;
end class <reversi-game>;

define method reversi-game-size
    (game :: <reversi-game>) => (size :: <integer>)
  reversi-board-size(reversi-game-board(game))
end method reversi-game-size;

define method reversi-game-size-setter
    (size :: <integer>, game :: <reversi-game>)
 => (size :: <integer>)
  reversi-game-board(game) := make(<reversi-board>, size: size);
  new-game(game);
  size
end method reversi-game-size-setter;

define method new-game (game :: <reversi-game>) => ()
  let players = game.%players;
  initialize-board(reversi-game-board(game));
  note-game-updated(game);
  select (players)
    0 =>
      play-both-players(game);
    1 => 
      if (game.%player = #"white")
        play-moves-for-player(game, #"black", #"white")
      end;
    2 =>
      game.%player := #"black";
  end
end method new-game;

define method note-game-updated (game :: <reversi-game>) => ()
  game.reversi-game-update-callback(game);
  display-score(game)
end method note-game-updated;

define method game-over? 
    (game :: <reversi-game>) => (game-over? :: <boolean>)
  game-over?(reversi-game-board(game))
end method game-over?;

define method algorithm-for-player
    (game :: <reversi-game>, player :: <player>)
 => (algorithm :: <algorithm>)
  select (player)
    #"white" => game.white-algorithm;
    #"black" => game.black-algorithm;
  end
end algorithm-for-player;

define method algorithm-for-player-setter
    (algorithm :: <algorithm>, game :: <reversi-game>, player :: <player>)
 => (algorithm :: <algorithm>)
  select (player)
    #"white" => game.white-algorithm := algorithm;
    #"black" => game.black-algorithm := algorithm;
  end
end method algorithm-for-player-setter;

define method play-move-for-player 
    (game :: <reversi-game>, player :: <player>) => ()
  let board = reversi-game-board(game);
  let algorithm = algorithm-for-player(game, player);
  let all-moves = all-possible-moves-for-player(board, player);
  let squares-to-take
    = choose-move-for-player(algorithm, board, player, all-moves);
  if (squares-to-take & ~empty?(squares-to-take))
    let squares = reversi-board-squares(board);
    for (square in squares-to-take)
      squares[square] := player
    end;
    note-game-updated(game)
  end;
end method play-move-for-player;

define method play-moves-for-player 
    (game :: <reversi-game>, player :: <player>, other-player :: <player>)
 => ()
  let board = reversi-game-board(game);
  play-move-for-player(game, player);
  while (~any-moves-for-player?(board, other-player)
	   & any-moves-for-player?(board, player))
    play-move-for-player(game, player)
  end
end method play-moves-for-player;


/// Play the piece the player requests

define method reversi-game-play-square
    (game :: <reversi-game>, square :: <integer>)
 => (played-square? :: <boolean>)
  let board = reversi-game-board(game);
  let player = game.%player;
  let players = game.%players;
  let squares-to-take = squares-to-take(board, player, square);
  let other-player = other-player(player);
  if (~empty?(squares-to-take))
    let squares = reversi-board-squares(board);
    for (square in squares-to-take)
      squares[square] := player
    end;
    note-game-updated(game);
    select (players)
      2         => game.%player := other-player;
      otherwise => play-moves-for-player(game, other-player, player)
    end;
    #t
  end
end method reversi-game-play-square;

define method play-both-players (game :: <reversi-game>) => ()
  let player = game.%player;
  let other-player = other-player(player);
  until (game-over?(game))
    play-moves-for-player(game, player, other-player);
    play-moves-for-player(game, other-player, player)
  end
end method play-both-players;

define method choose-players 
    (game :: <reversi-game>, 
     type :: one-of(#"white", #"black", #"computers", #"two-players"))
 => ()
  let game-over? = game-over?(game);
  let player = game.%player;
  select (type)
    #"white", #"black" =>
      game.%players := 1;
      unless (player = type)
	game.%player := type;
	play-moves-for-player(game, other-player(type), type);
      end;
    #"computers" =>
      game.%players := 0;
      if (game-over?)
	new-game(game)
      else
	play-both-players(game)
      end;
      game-over? := #f;
    #"two-players" =>
      game.%players := 2;
  end;
  if (game-over?)
    new-game(game)
  end
end method choose-players;
     
define method display-board (game :: <reversi-game>) => ()
  display-board(reversi-game-board(game));
end method display-board;

define method display-message 
    (game :: <reversi-game>, message :: <string>, #rest message-args) => ()
  let message-function = reversi-game-message-function(game);
  case
    message-function =>
      apply(message-function, message, message-args);
    otherwise =>
      apply(format-out, message, message-args);
      format-out("\n");
  end
end method display-message;


/// Score handling

define method score-for-player 
    (game :: <reversi-game>, player :: <player>) => (score :: <integer>)
  score-for-player(reversi-game-board(game), player)
end method score-for-player;

define method display-current-score (game :: <reversi-game>) => ()
  display-message(game,
                  "%s: %d, %s: %d",
                  name-for-player(#"white"),
                  score-for-player(game, #"white"),
                  name-for-player(#"black"),
                  score-for-player(game, #"black"))
end method display-current-score;

define method display-final-score (game :: <reversi-game>) => ()
  let white-score = score-for-player(game, #"white");
  let black-score = score-for-player(game, #"black");
  case
    white-score = black-score =>
      display-message(game, "Game drawn: %d pieces each", white-score);
    otherwise =>
      let winner
	= if (white-score > black-score) 
	    #"white"
	  else
	    #"black"
	  end;
      display-message(game, "%s has won: %d to %d",
		      name-for-player(winner),
		      score-for-player(game, winner),
		      score-for-player(game, other-player(winner)))
  end
end method display-final-score;

define method display-score (game :: <reversi-game>) => ()
  case
    game-over?(game) => display-final-score(game);
    otherwise        => display-current-score(game);
  end
end method display-score;

