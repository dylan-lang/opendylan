Module: angle-implementation 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The implementation of angle, latitude and longitude for the
        angle library.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

define abstract class <angle> (<sixty-unit>)
end class <angle>; 

define method say (angle :: <angle>) => ()
  let(degrees, minutes, seconds) = decode-total-seconds(angle);
  format-out("%d degrees %d minutes %d seconds", degrees, minutes, seconds);
end method say; 

define class <relative-angle> (<angle>)
end class <relative-angle>; 

define method say (angle :: <relative-angle>) => ()
  format-out(" %d", decode-total-seconds(angle));
end method say; 

define abstract class <directed-angle> (<angle>)
  // virtual slot direction :: <symbol>;
  slot internal-direction :: <symbol>;
  keyword direction:;
end class <directed-angle>; 

define method initialize (angle :: <directed-angle>, #key direction: dir)
  next-method();
  angle.direction := dir;
end method initialize; 

define open generic direction (object :: <object>) => (dir :: <symbol>);
define open generic direction-setter (dir :: <symbol>, object :: <object>) => (dir :: <symbol>);

define method direction (angle :: <directed-angle>) => (dir :: <symbol>)
  angle.internal-direction;
end method direction; 

define method direction-setter 
    (dir :: <symbol>, angle :: <directed-angle>) => (new-dir :: <symbol>)
  angle.internal-direction := dir;
end method direction-setter; 

define method say (angle :: <directed-angle>) => ()
  next-method();
  format-out(" %s", angle.direction);
end method say; 

define class <latitude> (<directed-angle>)
end class <latitude>; 

define method say (latitude :: <latitude>) => ()
  next-method();
  format-out(" latitude\n");
end method say; 

define method direction-setter 
    (dir :: <symbol>, latitude :: <latitude>) => (new-dir :: <symbol>)
  if (dir == #"north" | dir == #"south")
    next-method();
  else
    error("%= is not North or South", dir);
  end if;
end method direction-setter; 

define class <longitude> (<directed-angle>)
end class <longitude>; 

define method say (longitude :: <longitude>) => ()
  next-method();
  format-out(" longitude\n");
end method say; 

define method direction-setter 
    (dir :: <symbol>, longitude :: <longitude>) => (new-dir :: <symbol>)
  if (dir == #"east" | dir == #"west")
    next-method();
  else
    error("%= is not East or West", dir);
  end if;
end method direction-setter; 
