

===========================================================================


                   Pawns: a small Dylan program, v 1.4

                   by Enrico Colombini (erix@mclink.it)
               Harlequin Dylan porting (hpawns), 29 Dec 1997

===  What is Pawns?  ======================================================

Pawns is a simple game that learns from its own mistakes, inspired by an 
article published by Martin Gardner in the June 1969 issue of (the Italian 
edition of) Scientific American.
I actually built the 'computer' described by Gardner (a bundle of 
matchboxes glued together): it worked, and I was quite impressed (I had
never seen a computer at the time).

In 1989 I wrote a Basic program (as an example for a programming course) 
to simulate the game. In 1994 I rewrote the same program using CMU's
Mindy (Mindy Is Not Dylan Yet) compiler/interpreter. In 1997 I ported it
to Harlequin Dylan in console mode, with surprisingly few problems.


===  Running (h)pawns  ====================================================

To run pawns in standard (3 x 3) mode, type:

  pawns 3 3

(command line arguments are detailed later in this file).


===  Rules of the game  ===================================================

The game is played by a human player against a machine opponent. There are
some pawns on a chessboard; they move as chess pawns do, i.e. can either
move straight ahead by one square (if the destination square is free) or eat 
an opponent pawn by advancing diagonally.

Whoever reaches the opponent's "home row" wins. If a player cannot move
anymore, the other player wins.


===  Not exactly AI  ======================================================

Pawns could be classified as AD (Artificial Dumbness): it uses no strategy
at all, but chooses its moves at random.

However, the machine remembers every past mistake (in the form of a 'losing'
board configuration) and avoids making any move which would result in one of
the remembered 'losing boards'.

It is important to remember that the machine *never* evaluates the relative
value of a move: it only discards moves that have *proven* themselves wrong
in past games (i.e. led to defeat).

In other words, it starts as a very bad player, then gradually improves
its 'skills' until it can no longer be beaten (at least on the 3 x 3 board).


===  Command line arguments  ==============================================

The syntax of the command line is:

  pawns [-d] x y

where x and y are the dimensions of the playing board (3..6).

(the "pawns" one-line script expands it to: mindy -f pawns.dbc [-d] x y)

A 3 x 3 board is strongly suggested to appreciate the game; other values 
can be used to experiment, but are likely to produce boring results. 
For example, a 6 x 6 board would simply have too many possible configurations,
while a 3 x 5 board could be interesting to explore.

The -d optional argument forces 'deterministic' play, i.e. no random moves.
In deterministic mode, the machine always evaluates the possible moves in
the same order. It will be easier to beat, and therefore it will learn faster.


===  Online help  =========================================================

To get help in the game, just hit <return>. 
You can also type "help", "info", "?", "what?" or your preferred word;
they will be automagically recognized as a request for help :-)

There are two undocumented commands, used for debugging: 

- "exp" will show the remembered losing boards. The compact format is not 
easy to read, because it is in the internal array order (i.e. for a 3 x 3 
board: a1 a2 a3 b1 b2 b3 c1 c2 c3).

- "debug" will toggle a debug flag; if active, it will show all the moves
that the machine considers (in x1, y1, x2, y2 format, zero based) and the 
resulting board configurations.


===  Programming  =========================================================

Pawns was the first 'real' Dylan program I wrote and it certainly is not
a good example of the best way to write Dylan programs. Some shortcomings 
are the result of deliberate choices: 

- I did not even *think* about efficiency, non-consing, and the like. 
  It seems to work fine enough even with lots of dynamic list creation, 
  linear search (!) and so on.
- No new classes are defined: I preferred to get a grasp of the syntax
  first. Good candidates for classes are the playing board (it would get
  rid of 2 global variables), the move (for a cleaner undo) and maybe the
  squares and/or pawns. A <player> class would make it possible to pit
  one machine against another, for auto-learning.
  In short, this version of Pawns is not object-oriented. For an object-
  oriented version, see my Apple Dylan porting (if you can find it).
- File names for experience saving are fixed, and there is no error 
  checking.
- Mirrored losing positions are not considered (that would not be difficult
  to fix).
- From Pawns 1.3 on, I began using more of the (yet) non-standard library 
  functions and facilities of Mindy; that makes the program a bit less 
  portable (as I said, this did not prove to be true).

Other problems are simply due to sloppy programming. For example:

- the use of $human and #"human" in different occasions is not a very bright 
  idea, though it is perhaps convenient from a programming standpoint.
- There are at least three different representations of pawn coordinates
  (symbolic, x,y list and x1,x1,x2,y2 list for the moves).
- The packed board representation is not quite human-readable.

I would also like to add portable cursor positioning control and do away
with the scrolling, or localize it for a given OS or terminal (most likely
ANSI).


===  Suggestions, improvements...  ========================================

I would not describe this program as "elegant"; I will appreciate any
suggestion on how a better use of Dylan could have made it smaller and 
cleaner.

Moreover, Pawns is written (non surprisingly) "as a C programmer would
write it". I have had very little exposure to Scheme, and almost none to
Common Lisp. So, feel free to use this program to analyse the "typical"
mistakes of a programmer not used to dynamic programming.

Please mail your *not-too-harsh* :-) criticism to the author:

 erix@mclink.it

- v 1.3 addendum: 
After more than one year of dabbling with Dylan, I feel that I should
rewrite Pawns using a more decent organization and style. However, I
already did a partial rewriting for the Apple Dylan version, and would
prefer to do something else instead, so I'll keep it as an historical
document :-) 


===  Acknowledgements  ====================================================

I would like to express my gratitude to all the people that have made it 
possible to experiment with this new langage, especially to the authors
of the Marlais interpreter (Brent Benson, Joe Wilson, Patrick Beard and
others) and the Mindy bytecode compiler (the CMU Gwydion project group).

Special thanks to Scott Fahlman and Patrick Beard for their assistance in
starting up with Dylan, and to Steve Strassmann and all of the wonderful
Apple Cambridge Team (now a victim of company (mis)management) for their
invaluable research work and patient assistance to Dylan developers.

Thanks to Robert Orenstein for telling me the name of Gardner's game (Hexapawn)
and, of course, thanks to Martin Gardner for the original idea of the game. 


===  Legalese  ============================================================

Pawns is Copyright (C) 1994-97 Enrico Colombini.
It can be freely distributed (I removed the "non-commercial use"
restriction), as long as the copyright notice is not removed.
