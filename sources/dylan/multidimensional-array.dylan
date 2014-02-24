Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed inline method make
    (class == <multidimensional-array>, #rest all-keys, #key)
 => (array :: <array>)
  apply(make, <simple-object-array>, all-keys)
end method make;

define sealed inline method empty? (array :: <simple-array>) => (b :: <boolean>)
  array.size = 0
end method empty?;

define constant <dimensions>      = limited(<vector>, of: <integer>, default-fill: 0);
define constant $empty-dimensions = make(<dimensions>, size: 0);

define inline function compute-size-from-dimensions
    (dimensions :: false-or(<sequence>))
 => (size :: false-or(<integer>))
  dimensions
    & if (dimensions.size = 0)
        0
      else
        reduce(\*, 1, dimensions)
      end if
end function;

define function compute-array-dimensions-and-size
    (dimensions)
 => (dimensions :: <sequence>, size :: <integer>)
  if (supplied?(dimensions))
    let canonical-dimensions = as(<dimensions>, dimensions);
    values(canonical-dimensions, compute-size-from-dimensions(canonical-dimensions));
  else
    error(make(<missing-keyword-error>,
               format-string: "No dimensions in call to make(<array>)"));
  end if;
end function;

define macro limited-array-minus-constructor-definer
  { define limited-array-minus-constructor "<" ## ?:name ## ">" (?superclasses:*)
      (#key ?fill:expression) }
    => { define primary class "<simple-" ## ?name ## "-array>" (?superclasses)
           constant slot dimensions :: <dimensions> = $empty-dimensions,
             init-keyword: dimensions:;
           repeated slot "row-major-" ## ?name ## "-array-element" :: "<" ## ?name ## ">",
             init-keyword:      fill:,
             init-value:        ?fill,
             size-getter:       size,
             size-init-keyword: size:,
             size-init-value:   0;
         end class;

         define sealed domain initialize ("<simple-" ## ?name ## "-array>");
         define sealed domain size ("<simple-" ## ?name ## "-array>");
         define sealed domain empty? ("<simple-" ## ?name ## "-array>");
         define sealed domain rank  ("<simple-" ## ?name ## "-array>");
         define sealed domain row-major-index ("<simple-" ## ?name ## "-array>");
         define sealed domain dimensions ("<simple-" ## ?name ## "-array>");
         define sealed domain dimension ("<simple-" ## ?name ## "-array>", <integer>);
         define sealed domain aref ("<simple-" ## ?name ## "-array>");
         define sealed domain aref-setter ("<" ## ?name ## ">", "<simple-" ## ?name ## "-array>");

         define inline sealed method element
             (array :: "<simple-" ## ?name ## "-array>", index :: <integer>,
              #key default = unsupplied()) => (object)
           if (element-range-check(index, size(array)))
             "row-major-" ## ?name ## "-array-element"(array, index)
           else
             if (unsupplied?(default))
               element-range-error(array, index)
             else
               default
             end if
           end if
         end method element;

         define inline sealed method element-no-bounds-check
             (array :: "<simple-" ## ?name ## "-array>", index :: <integer>, #key default)
          => (object :: "<" ## ?name ## ">")
           "row-major-" ## ?name ## "-array-element"(array, index)
         end method element-no-bounds-check;

         define inline sealed method element-no-bounds-check-setter
             (new-value :: "<" ## ?name ## ">",
              array :: "<simple-" ## ?name ## "-array>", index :: <integer>)
          => (object :: "<" ## ?name ## ">")
           "row-major-" ## ?name ## "-array-element"(array, index) := new-value
         end method element-no-bounds-check-setter;

         define method fill!
             (target :: "<simple-" ## ?name ## "-array>", value :: "<" ## ?name ## ">",
              #key start :: <integer> = 0, end: last = unsupplied())
          => (target :: "<simple-" ## ?name ## "-array>")
           let last :: <integer> = check-start-compute-end(target, start, last);
           without-bounds-checks
             for (index :: <integer> from start below last)
               target[index] := value
             end;
           end without-bounds-checks;
           target
         end method;
         }
end macro;

define macro limited-array-minus-selector-definer
  { define limited-array-minus-selector "<" ## ?:name ## ">" (?superclasses:*)
      (#key ?fill:expression) }
    => { define limited-array-minus-constructor "<" ## ?name ## ">" (?superclasses)
           (fill: ?fill);

         define sealed inline method element-type
             (t :: "<simple-" ## ?name ## "-array>") => (type :: <type>)
           "<" ## ?name ## ">"
         end method;
         
         define sealed method element-setter
             (new-value :: "<" ## ?name ## ">",
              array :: "<simple-" ## ?name ## "-array>", index :: <integer>)
          => (object :: "<" ## ?name ## ">")
           if (index >= 0 & index < array.size)
             "row-major-" ## ?name ## "-array-element"(array, index) := new-value
           else
             element-range-error(array, index)
           end if
         end method element-setter;
         
         define sealed method make
             (class == "<simple-" ## ?name ## "-array>",
              #key dimensions = unsupplied(), fill = ?fill,
                   element-type-fill = ?fill)
          => (array :: "<simple-" ## ?name ## "-array>")
           let (dimensions, size) = compute-array-dimensions-and-size(dimensions);
           unless (size = 0)
             check-type(fill, "<" ## ?name ## ">")
           end unless;
           ?=next-method(class,
                         dimensions:        dimensions,
                         size:              size,
                         element-type-fill: element-type-fill,
                         fill:              fill)
         end method make;
         
         define sealed inline method type-for-copy
             (array :: "<simple-" ## ?name ## "-array>")
          => (type :: <limited-mutable-sequence-type>)
           limited-array(element-type(array), element-type-fill(array), #f)
         end method type-for-copy
       }
end macro;

define macro limited-array-definer
  { define limited-array "<" ## ?:name ## ">" (#key ?fill:expression) }
    => { define limited-array-minus-selector "<" ## ?name ## ">"
             (<limited-fillable-collection>, <simple-array>) (fill: ?fill);
    
         define method concrete-limited-array-class
             (of == "<" ## ?name ## ">", default-fill)
          => (res :: <class>, fully-specified? :: <boolean>)
           values("<simple-" ## ?name ## "-array>", default-fill = ?fill)
         end method;
       }
end macro;

define limited-array <object> (fill: #f);

define method limited-array
    (of :: <type>, default-fill :: <object>, dimensions :: false-or(<sequence>))
 => (type :: <type>)
  let (concrete-class, fully-specified?)
    = concrete-limited-array-class(of, default-fill);
  if (dimensions | ~fully-specified?)
    let size = compute-size-from-dimensions(dimensions);
    make(<limited-array-type>,
         class:          <array>,
         element-type:   of,
         default-fill:   default-fill,
         concrete-class: concrete-class,
         size:           size,
         dimensions:     dimensions);
  else
    concrete-class
  end if;
end method;
