Module:    hanoi
Synopsis:  The classic Towers of Hanoi puzzle
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <disk> (<object>)
  constant slot diameter :: <integer>,
    required-init-keyword: diameter:;
end class <disk>;

define function make-disk
    (integer :: <integer>) => (disk :: <disk>)
  make(<disk>, diameter: integer)
end function make-disk;


define class <tower> (<object>)
  constant slot name :: <string>,
    required-init-keyword: name:;
  constant slot disks :: <deque> = make(<deque>);
end class <tower>;

define method initialize
    (tower :: <tower>,
     #key initial-disks :: <sequence> = #[])
 => ()
  next-method();
  for (disk in initial-disks)
    push(tower.disks, disk)
  end
end method initialize;

define method height
    (tower :: <tower>) => (height :: <integer>)
  size(tower.disks)
end method height;

define variable *n-operations* :: <integer> = 0;

define method move-disk
    (from-tower :: <tower>, to-tower :: <tower>)
  *n-operations* := *n-operations* + 1;
  format-out(".");
  let disk = pop(from-tower.disks);
  push(to-tower.disks, disk)
end method move-disk;


define method hanoi
    (from-tower :: <tower>, to-tower :: <tower>, with-tower :: <tower>,
     #key count :: <integer> = from-tower.height)
 => ()
  if (count >= 1)
    hanoi(from-tower, with-tower, to-tower, count: count - 1);
    move-disk(from-tower, to-tower);
    hanoi(with-tower, to-tower, from-tower, count: count - 1)
  end
end method hanoi;

