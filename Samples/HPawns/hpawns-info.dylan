Author:    Enrico Colombini
Version:   1.4
Synopsis:  Pawns 1.4 ported from Mindy to Harlequin's Dylan. Pawns is
           a very simple board game that plays at random but learns 
           from its own mistakes, inspired by Martin Gardner's 
           "matchbox computer" in a 1969 issue of Scientific American.
           (this is not an object-oriented program)
Copyright: (C) Enrico Colombini 1994-1997, freely redistributable
Module:    hpawns

define constant $application-name :: <byte-string> = "hpawns";
define constant $application-major-version :: <byte-string> = "1";
define constant $application-minor-version :: <byte-string> = "4";

define method application-full-name () => (full-name :: <byte-string>)
  concatenate($application-name, " Version ",
              $application-major-version, ".",
              $application-minor-version)
end method application-full-name;
