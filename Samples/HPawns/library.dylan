Module:    dylan-user
Author:    Enrico Colombini
Version:   0.1
Synopsis:  A try to port Pawns from Mindy to Harlequin's Dylan
Copyright: (C) Enrico Colombini, freely redistributable

define library hpawns
  use operating-system;
  use streams;
  use standard-io;
  use print;
  use format-out;
  use format;
  use threads;
  use table-extensions;
  use machine-word;
  use harlequin-dylan;

  // Add any more module exports here.
  export hpawns;
end library hpawns;
