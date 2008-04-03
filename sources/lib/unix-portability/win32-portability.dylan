module: unix-portability
author: Hannes Mehnert <hannes@mehnert.org>

define function unix-lseek (fd :: <integer>, position :: <integer>, mode :: <integer>)
 => (position :: <integer>)
end;

define function unix-errno () => (res)
end;
