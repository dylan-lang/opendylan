Module:    hanoi
Synopsis:  The classic Towers of Hanoi puzzle
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method print-tower
    (tower :: <tower>) => ()
  for (disk :: <disk> in tower.disks,
       separator = "" then ", ")
    format-out("%s%d",
               separator, disk.diameter)
  end
end method print-tower;

define method print-towers
    (#rest towers :: <tower>) => ()
  for (tower :: <tower> in towers)
    format-out("  %s: ", tower.name);
    print-tower(tower);
    format-out("\n")
  end;
  format-out("\n")
end method print-towers;

define method play-hanoi (height :: <integer>) => ()
  format-out("Towers of Hanoi:\n\n");
  let disks = map(make-disk, range(from: 1, to: height));
  let left-tower   = make(<tower>, name: "Left", initial-disks: disks);
  let middle-tower = make(<tower>, name: "Middle");
  let right-tower  = make(<tower>, name: "Right");
  format-out("Initial position:\n\n");
  print-towers(left-tower, middle-tower, right-tower);
  hanoi(left-tower, middle-tower, right-tower);
  format-out("took %d operations\n\nFinal position:\n\n", *n-operations*);
  print-towers(left-tower, middle-tower, right-tower)
end method play-hanoi;

begin
  play-hanoi(5);
end;

