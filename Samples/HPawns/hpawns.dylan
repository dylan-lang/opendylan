Author:    Enrico Colombini
Version:   1.4
Synopsis:  A quick port of Pawns from Mindy to Harlequin's Dylan (console)
Copyright: (C) 1994-1997 Enrico Colombini, freely redistributable
Module:    hpawns


//////////////////////////////////////////////////////////////////////////
//                              Constants                               //
//////////////////////////////////////////////////////////////////////////

define constant <board> = <array>;
define constant <packed-board> = <string>;

define constant $size-x-range :: <range> = range(from: 3, to: 6);
define constant $size-y-range :: <range> = range(from: 3, to: 6);

define constant $empty :: <character> = ' ';
define constant $human :: <character> = 'o';
define constant $machine :: <character> = 'x';

define constant $command-line-help =
  "\nusage:  pawns [-d] x y\n\n"
  "        -d  deterministic play\n"
  "         x  board horizontal size (3..6)\n"
  "         y  board vertical size (3..6)\n\n";

define constant $opening-titles =
  "\n\n          ===  Pawns 1.4  ===\n\n"
  "(a game that learns from its own mistakes)\n\n"
  "(C) 1994-97 Enrico Colombini, all rights reserved\n"
  "experimental porting to Harlequin Dylan, 12/97\n"
  "distribution free as long as (C) is retained\n\n";

define constant $help-message = 
  " Valid commands:\n"
  "   save    save accumulated experience to disk\n"
  "   load    reload previously saved experience\n"
  "   reset   forget accumulated experience\n"
  "   random  toggle random play mode\n"
  "   quit    exit program\n\n";
  // "help" is automatic
  // undocumented commands: "exp", "debug", "nodebug"

define constant $human-comments = 
  #[  "but I'm learning", "I don't understand", "something's wrong here",
      "I'll be back", "just wait and see", "grumpf", "I've let you win",
      "very strange", "I'll fire my programmer", "it won't happen again",
      "practice makes perfect", "drat", "mmmmm", "just warming up",
      " :-( :-( :-( ", "Are you happy now?", "how peculiar", "###CENSORED###",
      "oh, no", "I'm sure you cheated", "beginner's luck", "not my day",
      "I did it on purpose", "I'll call my mom", "not again", "why?",
      "it was a power glitch", "probably a hardware fault", "oops"  ];
      
define constant $machine-comments =
  #[  "as usual", "take that", "don't be so unhappy", "please stop crying",
      "practice makes perfect", "well, well", " :-) :-) :-) ", "good",
      "maybe you've been badly programmed", "learn this lesson", "he, he, he",
      "I'm still the best", "put down that hammer, please", "smack",
      "your AI must be defective", "I think you're just a human", "wow!",
      "try raising your supply voltage", "it's been a pleasure",
      "eat flaming silicon", "play again with me", "no surprise here",
      "is your FDIV correct?", "don't reset me now", "I like this"  ];


//////////////////////////////////////////////////////////////////////////
//                             Global data                              //
//////////////////////////////////////////////////////////////////////////

define variable *board* :: <board> = #[];
define variable *board-size-x* :: <integer> = 0;
define variable *board-size-y* :: <integer> = 0;
define variable *last-board* :: <packed-board> = "";
define variable *experience* :: <list> = #();  // losing board configurations
define variable *random-play* :: <boolean> = #t;
define variable *debug* :: <boolean> = #f;

define method enemy (actor == $human) $machine; end;
define method enemy (actor == $machine) $human; end;




//////////////////////////////////////////////////////////////////////////
//                             Main program                             //
//////////////////////////////////////////////////////////////////////////
// 
define method main ()
  if (get-command-args() = #f)
    write-std-flush($command-line-help);
  else
    let board-size = list(*board-size-x*, *board-size-y*);
    *board* := make(<board>, dimensions: board-size);
    write-std($opening-titles);
    write-flag-state(*random-play*, "Random play mode");
    while (play-a-game() ~= #"quit") 
      press-return("Press <return> for next game");
    end;
    write-std-flush("--> bye!\n\n");
  end if;
end method main;


// Read command line args, set corresponding global vars, return #f if invalid
//
define method get-command-args () => valid :: <boolean>;
  let ok = #f;
  let args = #[];  // default: no arguments
  block()          // workaround for problem in Beta 2 version
    args := application-arguments();
  exception (condition :: <type-error>)
    // ignore error
  end block;
  let args-list = as(<list>, args);
  if (~ empty?(args-list) & head(args-list) = "-d")
    *random-play* := #f;
    args-list := tail(args-list);
  end if;
  if (size(args-list) = 2)
    let x = string-to-integer(first(args-list));
    let y = string-to-integer(second(args-list));
    if (member?(x, $size-x-range) & member?(y, $size-y-range))
      *board-size-x* := x;
      *board-size-y* := y;
      ok := #t
    end if;
  end if;
  ok;
end method get-command-args;


// Play a game until somebody wins or player quits 
// exit status: winner (#"human" | #"machine"), or #"quit"
//
define method play-a-game () => status :: <symbol>;
  reset-board();
  write-board();
  let status = #f;
  until (status)
    let (cmd, dest) = input-move();
    if (dest ~= "")
      if (valid-human-move?(cmd, dest))
	status := play-turn(cmd, dest);	
      end if;
    elseif (exec-command(cmd))
      status := #"quit";
    end if;
  end until;
  status;
end method play-a-game;


// Execute command (if valid), return #t if quit requested
//
define method exec-command (cmd :: <string>) => quit-request :: <boolean>;
  let quit-request = #f;
  select (cmd by \=)    
    "save" => 
      save-experience();
      write-board();
    "load" => 
      load-experience();
      write-board();
    "reset" => 
      forget-experience();
    "random" =>
      *random-play* := ~ *random-play*;
      write-flag-state(*random-play*, "Random play");
    "quit" => 
      quit-request := #t;
    "exp" =>  // undocumented
      print(*experience*, *standard-output*); 
      write-std ("\n\n");
    "debug" =>  // undocumented
      *debug* := ~ *debug*;
      write-flag-state(*debug*, "Debug mode");
    otherwise =>
      write-std($help-message);
      write-board();
  end select;
  quit-request;
end method exec-command;


// Write description of current state of a flag
//
define method write-flag-state (flag :: <boolean>, desc :: <string>)
  write-std(" ", desc, ": ", if (flag) "on" else "off" end, "\n\n");
end method write-flag-state;


// Input move (2 tokens) or command (1 token)
//
define method input-move ()
 => (cmd :: <string>, dest :: <string>);
  write-std-flush("Move (e.g. a1 a2): ");
  // read-line will signal at eof, no need to check for #f
  let line = as-lowercase(read-line(*standard-input*));
  write-std("\n");
  let (token-1, pos) = get-token(line, 0);
  let token-2 = get-token(line, pos);
  values(token-1, token-2);
end method input-move;


// Check if valid move for human player, #f and print message if not valid
//
define method valid-human-move? (start :: <string>, dest :: <string>)
 => valid :: <boolean>;
  let (x1, y1) = name-to-coords(start);
  let (x2, y2) = name-to-coords(dest);
  let valid = #f;
  case 
    ~ valid-square-coords?(x1, y1) =>
      write-std("- <", start, "> is not a valid square\n\n");
    *board*[x1, y1] ~= $human =>
      write-std("- You don't have a pawn in square <", start, ">\n\n");
    ~ valid-square-coords?(x2, y2) =>
      write-std("- Invalid destination square <", dest, ">\n\n");
    ~ member?(list (x2, y2), valid-moves-from(x1, y1), test: \=) =>
      write-std("- Move forward in a free square or eat diagonally\n\n");
    otherwise =>
      valid := #t;
  end case;
  unless (valid)
    write-board();
  end unless;
  valid;
end method valid-human-move?;


//////////////////////////////////////////////////////////////////////////
//                               Game                                   //
//////////////////////////////////////////////////////////////////////////

// Play a game turn starting with valid move, return winner or #f if none
//
define method play-turn (start :: <string>, dest :: <string>)
 => winner :: false-or(<symbol>);
  let winner = human-turn(start, dest) | machine-turn();
  if (winner = #"human") 
    // last board was a losing one, add it to *experience*
    add-losing-board(*last-board*);
  elseif (~ winner)
    // remember board after machine normal move
    *last-board* := pack-board(*board*);
  end if;
  winner;
end method play-turn;


// Execute valid human move, return winner or #f
//
define method human-turn (start :: <string>, dest :: <string>)
 => winner :: false-or(<symbol>);
  let winner = #f;
  let (x1, y1) = name-to-coords(start);
  let (x2, y2) = name-to-coords(dest);
  move-pawn(x1, y1, x2, y2);
  write-board();
  // if last row reached, victory
  if (y2 = *board-size-y* - 1)
    winner := victory(#"human", "You have won this game"); 
  end if;
  winner;
end method human-turn;


// Build (possibly empty) list of valid destination squares from 
// (valid, occupied) square, works for both human and machine
//
define method valid-moves-from (x :: <integer>, y :: <integer>)
 => valid-moves :: <list>;
  let actor = *board*[x, y];
  let y = y + (if (actor = $human) 1 else -1 end);
  let valid-moves = #();
  if (valid-square-coords?(x, y) & *board*[x, y] = $empty)
    valid-moves := add!(valid-moves, list(x, y));
  end if;
  for (px from x - 1 to x + 1 by 2)
    if (valid-square-coords?(px, y) & *board*[px, y] = enemy(actor))
      valid-moves := add!(valid-moves, list(px, y));
    end if;
  end for;
  valid-moves;
end method valid-moves-from;


// Build a list of all possible moves for actor's pawns, in form (x1,y1,x2,y2)
//
define method possible-moves (actor :: <character>) => moves :: <list>;
  let moves = #();
  let pawns = pawns-list(actor);
  for (i in pawns)
    let x1 = first(i);
    let y1 = second(i);
    let valid = valid-moves-from(x1, y1);
    for (m in valid)
      moves := add!(moves, list(x1, y1, first(m), second(m)));
    end for;
  end for;
  moves;
end method possible-moves;


// Show victory message with random comment, return winner arg
//
define method victory (winner :: <symbol>, message :: <string>)
 => winner :: <symbol>;
  let comments
    = if (winner = #"machine") $machine-comments else $human-comments end;
  let r = random(size(comments));
  write-std("==>  ", message, "  <==   ...", comments [r], "...\n\n");
  winner;
end method victory;


//////////////////////////////////////////////////////////////////////////
//                        Machine Play & memory                         //
//////////////////////////////////////////////////////////////////////////

// Machine player thinks and moves, return winner or #f
//
define method machine-turn () => winner :: false-or(<symbol>);
  let winner = #f;
  let choices = possible-moves($machine);
  if (empty?(choices))
    winner := victory(#"human", "I cannot move! You win");
  else
    if (*random-play*)
      choices := shuffle-list(choices);
    end if;
    let move = any?(non-losing-move, choices);
    if (move = #f)
      winner := victory(#"human", "This position is hopeless. I give up");
    else
      press-return("Press <return> for my move");
      apply(move-pawn, move);
      write-board(); 
      // if first row reached, victory
      if (last(move) = 0)
	winner := victory(#"machine", "I won!");
      elseif (possible-moves($human) = #())
	winner := victory(#"machine", "You cannot move. You have lost!");
      end if;
    end if;
  end if;
  winner;
end method machine-turn;


// Simulate move, return move if resulting board is not in losing list, else #f
//
define method non-losing-move (move :: <list>) => move :: false-or(<list>);
  apply(move-pawn, move);
  let packed = pack-board(*board*);
  if (*debug*) 
    print(move, *standard-output*); 
    write-std("   ");
    print(packed, *standard-output*); 
    write-std("\n\n");
  end if;
  apply(undo-move-pawn, move);
  if (member?(packed, *experience*, test: \=)) #f else move end;
end method non-losing-move;


// Add losing board to experience (losing list)
//
define method add-losing-board (losing :: <packed-board>)
  *experience* := add!(*experience*, losing);
end method add-losing-board;


// Forget all accumulated experience (losing board configurations)
//
define method forget-experience ()
  *experience* := #();
end method forget-experience;


//////////////////////////////////////////////////////////////////////////
//                             Save/load                                //
//////////////////////////////////////////////////////////////////////////

// Save the losing boards list from *experience* to a disk file
// (overwrite file if it already exists, no error detection)
//
define method save-experience ()
  let outfile 
    = make(<file-stream>, locator: experience-file-name(), direction: #"output");
  for (i in *experience*)
    write-line(outfile, i);
  end for;
  close(outfile);
  write-std("Experience saved into \"", experience-file-name (), "\"\n\n");
end method save-experience;


// Load experience from disk file into *experience* 
// (no error detection)
//
define method load-experience ()
  let infile
    = make(<file-stream>, locator: experience-file-name(), direction: #"input");
  let temp = #();
  let line = #f;
  while (line := read-line(infile, on-end-of-stream: #f))
    temp := add!(temp, line);
  end while;
  close(infile);
  write-std("Experience read from \"", experience-file-name (), "\"\n\n");
  *experience* := temp;
end method load-experience;


// Build experience file name according to current board size
//
define method experience-file-name () => name :: <string>;
  format-to-string("experience.%dx%d", *board-size-x*, *board-size-y*);
end method experience-file-name;


//////////////////////////////////////////////////////////////////////////
//                             Game board                               //
//////////////////////////////////////////////////////////////////////////

// Write board and its contents to standard-output
// (could be rewritten using format())
//
define method write-board ()
  // write a separation line
  local method sep-line ()
          write-std("      +");
	  for (x from 0 below *board-size-x*)
	    write-std("---+");
	  end for;
	  write-std("\n");
	end method sep-line;
  // draw by row
  for (y from *board-size-y* - 1 to 0 by -1)
    sep-line();
    write-std("   ", as(<character>, y + as(<integer>, '1')), "  | ");
    for (x from 0 below *board-size-x*)
      write-std(*board*[x, y], " | ");
    end for;
    write-std("\n");
  end for;
  // bottom border
  sep-line();
  write-std("        ");
  for (x from 0 below *board-size-x*)
    write-std(as(<character>, x + as(<integer>, 'a')));
    write-std("   ");
  end for;
  write-std-flush("\n\n");
end method write-board;


// Convert square name (e.g. "a1") to x,y coordinates, no validity check
//
define method name-to-coords (name :: <string>) 
 => (x :: <integer>, y :: <integer>);
  // return invalid coord if no possible valid name
  if (size(name) ~= 2)
    values(-1, -1);
  else
    values(as(<integer>, name[0]) - as(<integer>, 'a'),
	   as(<integer>, name[1]) - as(<integer>, '1'));
  end if;
end method name-to-coords;


// Check validity of square coordinates
//
define method valid-square-coords? (x :: <integer>, y :: <integer>)
 => valid :: <boolean>;
  x >= 0 & x < *board-size-x* & y >= 0 & y < *board-size-y*;
end method valid-square-coords?;


// Pack board into string for compact storing
//
define method pack-board (board :: <board>) => packed-board :: <string>;
  map-as(<string>, identity, board);
end method pack-board;


//////////////////////////////////////////////////////////////////////////
//                           Pawns & moves                              //
//////////////////////////////////////////////////////////////////////////

// Put all pawns in their starting positions
//
define method reset-board ()
  fill!(*board*, $empty);
  for (x from 0 below *board-size-x*)
    *board*[x, 0] := $human;
    *board*[x,*board-size-y* - 1] := $machine;
  end for;
end method reset-board;


// Build list of pawns of given player as (x,y) coordinates
//
define method pawns-list (actor :: <character>) => coords :: <list>;
  let pawns = #();
  for (y from 0 below *board-size-y*)
    for (x from 0 below *board-size-x*)
      if (*board*[x, y] = actor) 
	pawns := add!(pawns, list (x, y));
      end if;
    end for;
  end for;
  pawns;
end method pawns-list;


// Move pawn from x1,y1 to x2,y2
//
define method move-pawn (x1 :: <integer>, y1 :: <integer>, 
			 x2 :: <integer>, y2 :: <integer>)
  *board*[x2, y2] := *board*[x1, y1];
  *board*[x1, y1] := $empty;
end method move-pawn;


// Undo pawn move from x1,y1 to x2,y2
//
define method undo-move-pawn (x1 :: <integer>, y1 :: <integer>, 
			      x2 :: <integer>, y2 :: <integer>)
  let actor = *board*[x2, y2];
  *board*[x1, y1] := actor;
  *board*[x2, y2] := if (x1 = x2) $empty else enemy(actor) end;
end method undo-move-pawn;


//////////////////////////////////////////////////////////////////////////
//                             Utilities                                //
//////////////////////////////////////////////////////////////////////////

// Some of these utilities may be outdated and/or could be rewritten
// in better style. However, they work, so I see no need to fix them.

// Get token from string, use space & tab as separators if not given
//
define method get-token (str :: <string>, start :: <integer>, #key separators)
 => (token :: <string>, new-pos :: <integer>);
  unless (separators) separators := " \t"; end;
  // find start of token
  let limit = size(str);
  while (start < limit & member?(str[start], separators))
    start := start + 1;
  end while;
  // find end of token
  let pos = start;
  while (pos < limit & ~ member?(str[pos], separators))
    pos := pos + 1;
  end while;
  if (pos = start)
    values("", 0);
  else
    values(copy-sequence(str, start: start, end: pos), pos);
  end if;
end method get-token;


// Write all arguments to standard output
//
define method write-std (#rest args)
  for (i in args)
    print-message(i, *standard-output*);
  end for;
end method write-std;
 

// Write all arguments to standard output and flush output buffer
//
define method write-std-flush (#rest args)
  apply(write-std, args);
  force-output(*standard-output*);
end method write-std-flush;


// Show prompt, wait for return key, add a newline
//
define method press-return (prompt :: <string>)
  write-std-flush(prompt, ": ");
  read-line(*standard-input*);
  write-std("\n");
end method press-return;


// Shuffle list elements, return a new shuffled list
// (more or less constant time implementation)
//
define method shuffle-list (ordered :: <list>) => shuffled :: <list>;
  as(<list>, shuffle-vector!(as(<vector>, ordered)));
end method shuffle-list;


// Randomly shuffle vector elements in the passed array
//
define method shuffle-vector! (vec :: <vector>) => shuffled :: <vector>;
  for (i from 0 below size(vec))
    let j = random (size(vec));
    let temp = vec [i];
    vec [i] := vec [j];
    vec [j] := temp;  
  end for;
  vec;
end method shuffle-vector!;



main();

