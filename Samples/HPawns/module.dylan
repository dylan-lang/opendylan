Module:    dylan-user
Author:    Enrico Colombini
Version:   0.1
Synopsis:  A try to port Pawns from Mindy to Harlequin's Dylan
Copyright: (C) Enrico Colombini, freely redistributable

define module hpawns
  // From operating-system:
  use operating-system;
  // From streams:
  use streams;
  // From standard-io:
  use standard-io;
  // From print:
  use print;
  // From format-out:
  use format-out;
  // From format:
  use format;
  // From threads:
  use threads;
  // From table-extensions:
  use table-extensions;
  // From machine-word:
  use machine-integer-user;
  // From harlequin-dylan:
  use finalization;
  use harlequin-dylan;
  use simple-random;

  // Add binding exports here.

end module hpawns;
