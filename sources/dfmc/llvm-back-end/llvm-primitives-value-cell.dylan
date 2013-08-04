Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2013 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-box
    (object :: <object>)
 => (box :: <object>);
  let header-words = dylan-value(#"$number-header-words");
  let module = be.llvm-builder-module;

  let class :: <&class> = dylan-value(#"<traceable-value-cell>");
  let total-size = header-words + ^instance-storage-size(class);
  let byte-size = total-size * back-end-word-size(be);
  call-primitive(be, primitive-alloc-s1-descriptor,
		 byte-size,
		 emit-reference(be, module, ^class-mm-wrapper(class)),
		 object)
end;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-raw-box
    (raw-object :: <raw-pointer>)
 => (box :: <object>);
  let class :: <&class> = dylan-value(#"<untraceable-value-cell>");
  let box = op--allocate-untraced(be, class);
  let raw-ptr = op--getslotptr(be, box, class, #"value-cell-raw-object");
  ins--store(be, raw-object, raw-ptr, alignment: back-end-word-size(be));
  ins--bitcast(be, box, $llvm-object-pointer-type)
end;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-single-float-box
    (raw-single-float :: <raw-single-float>)
 => (box :: <object>);
  let class :: <&class> = dylan-value(#"<untraceable-value-cell>");
  let box = op--allocate-untraced(be, class);
  let raw-ptr = op--getslotptr(be, box, class, #"value-cell-raw-object");
  let sf-type
    = llvm-reference-type(be, dylan-value(#"<raw-single-float>"));
  let sf-ptr = ins--bitcast(be, raw-ptr, llvm-pointer-to(be, sf-type));
  ins--store(be, raw-single-float, sf-ptr, alignment: back-end-word-size(be));
  ins--bitcast(be, box, $llvm-object-pointer-type)
end;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-double-float-box
    (raw-double-float :: <raw-double-float>)
 => (box :: <object>);
  let (class :: <&class>, slot-name :: <symbol>) = double-float-box-class(be);
  let box = op--allocate-untraced(be, class);
  let raw-ptr = op--getslotptr(be, box, class, slot-name);
  let df-type
    = llvm-reference-type(be, dylan-value(#"<raw-double-float>"));
  let df-ptr = ins--bitcast(be, raw-ptr, llvm-pointer-to(be, df-type));
  ins--store(be, raw-double-float, df-ptr, alignment: back-end-word-size(be));
  ins--bitcast(be, box, $llvm-object-pointer-type)
end;

define function double-float-box-class
    (be :: <llvm-back-end>)
 => (class :: <&class>, slot-name :: <symbol>);
  let raw-df-size = dylan-value(#"<raw-double-float>").raw-type-size;
  let raw-ptr-size = dylan-value(#"<raw-pointer>").raw-type-size;
  if (raw-df-size <= raw-ptr-size)
    values(dylan-value(#"<untraceable-value-cell>"),
	   #"value-cell-raw-object")
  elseif (raw-df-size <= 2 * raw-ptr-size)
    values(dylan-value(#"<untraceable-double-value-cell>"),
	   #"value-cell-raw-object-1")
  else
    error("<raw-double-float> won't fit in a box");
  end if;
end;

define method op--make-closed-over-cell
    (back-end :: <llvm-back-end>,
     rep :: <&type>, value :: <llvm-value>)
 => (cell :: <llvm-value>);
  call-primitive(back-end, primitive-make-box-descriptor, value)
end method;

define method op--make-closed-over-cell
    (back-end :: <llvm-back-end>,
     rep :: <&raw-single-float>, value :: <llvm-value>)
 => (cell :: <llvm-value>);
  call-primitive(back-end, primitive-make-single-float-box-descriptor, value)
end method;

define method op--make-closed-over-cell
    (back-end :: <llvm-back-end>,
     rep :: <&raw-double-float>, value :: <llvm-value>)
 => (cell :: <llvm-value>);
  call-primitive(back-end, primitive-make-double-float-box-descriptor, value)
end method;

define method op--get-closed-over-cell
    (back-end :: <llvm-back-end>,
     rep :: <&type>, cell :: <llvm-value>)
 => (value :: <llvm-value>);
  let class :: <&class> = dylan-value(#"<traceable-value-cell>");
  let cell-cast = op--object-pointer-cast(back-end, cell, class);
  let value-ptr
    = op--getslotptr(back-end, cell-cast, class, #"value-cell-object");
  ins--load(back-end, value-ptr, alignment: back-end-word-size(back-end));
end method;

define method op--get-closed-over-cell
    (back-end :: <llvm-back-end>,
     rep :: <&raw-single-float>, cell :: <llvm-value>)
 => (value :: <llvm-value>);
  let class :: <&class> = dylan-value(#"<untraceable-value-cell>");
  let cell-cast = op--object-pointer-cast(back-end, cell, class);
  let raw-ptr
    = op--getslotptr(back-end, cell-cast, class, #"value-cell-raw-object");
  let sf-type
    = llvm-reference-type(back-end, dylan-value(#"<raw-single-float>"));
  let sf-ptr
    = ins--bitcast(back-end, raw-ptr, llvm-pointer-to(back-end, sf-type));
  ins--load(back-end, sf-ptr, alignment: back-end-word-size(back-end));
end method;

define method op--get-closed-over-cell
    (back-end :: <llvm-back-end>,
     rep :: <&raw-double-float>, cell :: <llvm-value>)
 => (value :: <llvm-value>);
  let (class :: <&class>, slot-name :: <symbol>)
    = double-float-box-class(back-end);
  let cell-cast = op--object-pointer-cast(back-end, cell, class);
  let raw-ptr
    = op--getslotptr(back-end, cell-cast, class, slot-name);
  let df-type
    = llvm-reference-type(back-end, dylan-value(#"<raw-double-float>"));
  let df-ptr
    = ins--bitcast(back-end, raw-ptr, llvm-pointer-to(back-end, df-type));
  ins--load(back-end, df-ptr, alignment: back-end-word-size(back-end));
end method;

define method op--set-closed-over-cell
    (back-end :: <llvm-back-end>,
     rep :: <&type>, cell :: <llvm-value>, value :: <llvm-value>)
 => ();
  let class :: <&class> = dylan-value(#"<traceable-value-cell>");
  let cell-cast = op--object-pointer-cast(back-end, cell, class);
  let value-ptr
    = op--getslotptr(back-end, cell-cast, class, #"value-cell-object");
  ins--store(back-end, value, value-ptr,
	     alignment: back-end-word-size(back-end));
end method;

define method op--set-closed-over-cell
    (back-end :: <llvm-back-end>,
     rep :: <&raw-single-float>, cell :: <llvm-value>, value :: <llvm-value>)
 => ();
  let class :: <&class> = dylan-value(#"<untraceable-value-cell>");
  let cell-cast = op--object-pointer-cast(back-end, cell, class);
  let raw-ptr
    = op--getslotptr(back-end, cell-cast, class, #"value-cell-raw-object");
  let sf-type
    = llvm-reference-type(back-end, dylan-value(#"<raw-single-float>"));
  let sf-ptr
    = ins--bitcast(back-end, raw-ptr, llvm-pointer-to(back-end, sf-type));
  ins--store(back-end, raw-single-float, sf-ptr,
	     alignment: back-end-word-size(back-end));
end method;

define method op--set-closed-over-cell
    (back-end :: <llvm-back-end>,
     rep :: <&raw-double-float>, cell :: <llvm-value>, value :: <llvm-value>)
 => ();
  let (class :: <&class>, slot-name :: <symbol>)
    = double-float-box-class(back-end);
  let cell-cast = op--object-pointer-cast(back-end, cell, class);
  let raw-ptr = op--getslotptr(back-end, cell-cast, class, slot-name);
  let df-type
    = llvm-reference-type(back-end, dylan-value(#"<raw-double-float>"));
  let df-ptr
    = ins--bitcast(back-end, raw-ptr, llvm-pointer-to(back-end, df-type));
  ins--store(back-end, value, df-ptr, alignment: back-end-word-size(back-end));
end method;
