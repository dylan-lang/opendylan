Module:    dylan-user
Author:    Enrico Colombini
Version:   0.1
Synopsis:  A try to port Pawns from Mindy to Harlequin's Dylan
Copyright: (C) Enrico Colombini, freely redistributable

define module hpawns
  // From system:
  use operating-system;
  use file-system;
  // From io:
  use streams;
  use standard-io;
  use print;
  use format;
  use format-out;
  // From collections:
  use table-extensions;
  // From functional-dylan:
  use threads;
  use finalization;
  use functional-dylan;
  use simple-random;

  // Add binding exports here.

end module hpawns;
