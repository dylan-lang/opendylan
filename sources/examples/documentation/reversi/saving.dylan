Module:    reversi
Author:    Jonathon Lee
Synopsis:  Reversi game extensions 2
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// this file contains changes to the reversi example. 
// it adds the functionality of saving/loading games.

*changed2?* := #t;

define method number-for-piece
    (piece :: <piece>) => (int :: <integer>)
  select (piece) 
    #"white"  => 1;
    #"black"  => 2;
    otherwise => 0; 
  end select;
end method number-for-piece

define method piece-for-number
  (int :: <integer>) => (piece :: <piece>)
  select (int) 
    1 => #"white";
    2 => #"black";
    otherwise => #f;
  end select;
end method piece-for-number;

define method reversi-square-write-data
    (square :: <piece>, stream :: <file-stream>)
 => ()
  write-element(stream, number-for-piece(square));
end method reversi-square-write-data;

define method reversi-board-write-data
    (board :: <reversi-board>, stream :: <file-stream>)
 => ()
  let squares = reversi-board-squares(board);
  for (square from 0 below size(squares))
    reversi-square-write-data(squares[square], stream);
  end for;
end method reversi-board-write-data;

define method reversi-board-read-data
    (board :: <reversi-board>, stream :: <file-stream>)
 => ()
  let squares = reversi-board-squares(board);
  for (square from 0 below size(squares))
    let this = read-element(stream);
    squares[square] := piece-for-number(this);
  end for;
end method reversi-board-read-data;

define method reversi-game-encode-players
    (game :: <reversi-game>, stream :: <file-stream>) => ()
  let players = game.%players;
  let player = game.%player;
  write-element(stream, players);
  write-element(stream, number-for-piece(player));
end method reversi-game-encode-players;

define method reversi-game-write-data 
    (game :: <reversi-game>, stream :: <file-stream>) 
 => ()
  let board = game.reversi-game-board;
  reversi-board-write-data(board, stream);
  reversi-game-encode-players(game, stream);
end class reversi-game-write-data;

define method reversi-game-decode-players
    (game :: <reversi-game>, stream :: <file-stream>) => ()
  game.%players := read-element(stream);
  game.%player := piece-for-number(read-element(stream));
end method reversi-game-decode-players;

define method reversi-game-read-data 
    (game :: <reversi-game>, stream :: <file-stream>)
 => ()
  let board = game.reversi-game-board;
  reversi-board-read-data(board, stream);
  reversi-game-decode-players(game, stream);
end method reversi-game-read-data;

define method reversi-game-save-game 
    (game :: <reversi-game>, file :: <pathname>) => ()
  let file-stream = make(<file-stream>, locator: file, direction: #"output",
			 element-type: <byte-character>);
  reversi-game-write-data(game, file-stream);
  close(file-stream);
end method reversi-game-save-game;

define method reversi-game-load-game 
    (game :: <reversi-game>, file :: <pathname>) => ()
  let file-stream = make(<file-stream>, locator: file, direction: #"input",
			 element-type: <byte>);
  reversi-game-read-data(game, file-stream);
  close(file-stream);
  note-game-updated(game)
end method reversi-game-load-game;