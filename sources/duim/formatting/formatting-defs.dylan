Module:       DUIM-formatting-internals
Synopsis:     DUIM formatted output
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// A bummed version of 'set-sheet-position' that doesn't propagate
// geometry cache clearing
define function %set-sheet-position
    (record :: <output-record-element-mixin>, x :: <integer>, y :: <integer>) => ()
  let (old-x, old-y) = box-position(sheet-region(record));
  let transform = sheet-transform(record);
  transform-coordinates!(transform, old-x, old-y);
  let dx :: <integer> = x - old-x;
  let dy :: <integer> = y - old-y;
  sheet-transform(record) := compose-translation-into!(dx, dy, transform)
end function %set-sheet-position;


define open generic process-spacing-arg
    (sheet, spacing, axis, #key form) => (spacing :: false-or(<integer>));

define method process-spacing-arg
    (sheet :: <sheet>, spacing == #f, axis, #key form)
 => (spacing :: singleton(#f))
  ignore(sheet, axis, form);
  #f
end method process-spacing-arg;

define method process-spacing-arg
    (sheet :: <sheet>, spacing :: <integer>, axis, #key form)
 => (spacing :: <integer>)
  ignore(sheet, axis, form);
  spacing
end method process-spacing-arg;

define method process-spacing-arg
    (sheet :: <sheet>, spacing :: <string>, axis, #key form)
 => (spacing :: <integer>)
  ignore(form);
  let (width, height) = text-size(sheet, spacing);
  if (axis == #"horizontal") floor(width) else floor(height) end
end method process-spacing-arg;

define method process-spacing-arg
    (sheet :: <sheet>, spacing :: <character>, axis, #key form)
 => (spacing :: <integer>)
  ignore(form);
  let (width, height) = text-size(sheet, spacing);
  if (axis == #"horizontal") floor(width) else floor(height) end
end method process-spacing-arg;

define method process-spacing-arg
    (sheet :: <sheet>, spacing :: <function>, axis, #key form)
 => (spacing :: <integer>)
  ignore(form);
  spacing(sheet, axis)
end method process-spacing-arg;

define method process-spacing-arg
    (sheet :: <sheet>, spacing :: <sequence>, axis, #key form)
 => (spacing :: <integer>)
  ignore(form);
  let units = spacing[1];
  let spacing = spacing[0];
  let display = display(sheet);
  select (units)
    #"point"  => floor(spacing * display-pixels-per-point(display));
    #"pixel"  => spacing;
    #"device" => spacing;
    #"mm"     => if (axis == #"horizontal")
		   spacing * floor/(display-pixel-width(display), display-mm-width(display))
		 else
		   spacing * floor/(display-pixel-height(display), display-mm-height(display))
		 end;
    #"character" => begin
		      let (width, height) = text-size(sheet, ' ');
		      if (axis == #"horizontal") floor(spacing * width)
		      else floor(spacing * height) end;
		    end;
    #"line" => spacing * sheet-line-height(sheet)
	       + (spacing - 1) * sheet-line-spacing(sheet);
  end
end method process-spacing-arg;

define method process-spacing-arg
    (sheet :: <sheet>, spacing, axis, #key form)
 => (spacing :: <integer>)
  ignore(axis);
  error("The spacing specification %= to %= was invalid", spacing, form)
end method process-spacing-arg;
