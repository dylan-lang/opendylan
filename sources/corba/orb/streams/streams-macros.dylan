Module: orb-streams
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// TYPECODE ALIGNMENTS
///
/// ---*** NB avoid closure/invoker style because stack allocated
/// closures are not properly cleaned up at the moment (19 May
/// 1999). This is a problem for infinite while(#t) loops that use
/// these macros.

/// ---*** should be stream specific (slot?)

define thread variable *typecode-input-alignments* :: <deque> = make(<deque>);
define thread variable *typecode-output-alignments* :: <deque> = make(<deque>);

define macro with-typecode-output-alignment
  { with-typecode-output-alignment (?stream:expression) ?body:body end }
    => 
    { block ()
        push(*typecode-output-alignments*, marshalling-stream-output-index(?stream));
        ?body;
      cleanup
        pop(*typecode-output-alignments*);
      end;
  }

  { with-typecode-output-alignment (?stream:expression, reset?: ?reset:expression) ?body:body end }
    => 
    { block ()
        push(*typecode-output-alignments*,
             if (?reset)
               make(<deque>)
             else
               marshalling-stream-output-index(?stream);
             end if);
        ?body;
      cleanup
        pop(*typecode-output-alignments*);
      end; }
end macro;

define macro with-typecode-input-alignment
  { with-typecode-input-alignment (?stream:expression) ?body:body end }
    => 
    { block ()
        push(*typecode-input-alignments*, marshalling-stream-input-index(?stream));
        ?body;
      cleanup
        pop(*typecode-input-alignments*);
      end;
 }

  { with-typecode-input-alignment (?stream:expression, reset?: ?reset:expression) ?body:body end }
    => 
    { block ()
        push(*typecode-input-alignments*,
             if (?reset)
               make(<deque>)
             else
               marshalling-stream-input-index(?stream);
             end if);
        ?body;
      cleanup
        pop(*typecode-input-alignments*);
      end; }
end macro;

define function typecode-alignment-from-nesting (nesting :: <integer>)
  element(*typecode-output-alignments*, nesting)
end function;

define function typecode-nesting-from-alignment (alignment :: <integer>)
  find-key(*typecode-input-alignments*, method (x) x = alignment end method)
end function;

/// ALIGNMENT CONSTRAINTS

define thread variable *input-marshalling-alignment-constraint* :: <integer> = 0;
define thread variable *output-marshalling-alignment-constraint* :: <integer> = 0;

define function input-marshalling-alignment-constraint ()
  *input-marshalling-alignment-constraint*
end function;

define function output-marshalling-alignment-constraint ()
  *output-marshalling-alignment-constraint*
end function;

define macro with-output-alignment-constraint
  { with-output-alignment-constraint (?stream:expression) ?body:body end }
    =>
    { dynamic-bind (*output-marshalling-alignment-constraint* = marshalling-stream-output-index(?stream))
        ?body;
      end }
end macro;

define macro with-input-alignment-constraint
  { with-input-alignment-constraint (?stream:expression) ?body:body end }
    =>
    { dynamic-bind (*input-marshalling-alignment-constraint* = marshalling-stream-input-index(?stream))
        ?body;
      end }
end macro;

define macro with-little-endianness
  { with-little-endianness (?stream:expression, ?little-endian:expression) ?body:body end }
    =>
    { 
    let stream = ?stream;
    let old-little-endian? :: <boolean> = marshalling-stream-little-endian?(stream);
    block ()
      marshalling-stream-little-endian?(stream) := ?little-endian;
      ?body;
    cleanup
      marshalling-stream-little-endian?(stream) := old-little-endian?;
    end block;
     }
end macro;

define macro with-marshalling-octet-stream
  { with-marshalling-octet-stream (?stream:name = ?profile-data:expression) ?body:body end }
    => {
    let ?stream :: false-or(<marshalling-octet-stream>) = #f;
    block ()
      ?stream := make(<marshalling-octet-stream>, buffer: ?profile-data);
      ?body;
    cleanup
      when (?stream)
        close(?stream);
      end;
    end block;
	}
end macro;

define macro with-marshalling-stream
  { with-marshalling-stream (?stream:name, ?keys:*) ?body:body end }
    => {
    let ?stream :: false-or(<marshalling-stream>) = #f;
    block ()
      ?stream := make(class-for-make(?keys), ?keys);
      ?body;
    cleanup
      when (?stream)
        close(?stream);
      end;
    end block;
	}
end macro;

define method class-for-make (#key inner-stream, #all-keys)
 => (class :: <class>)
  if (inner-stream)
    <marshalling-wrapper-stream>
  else
    <marshalling-stream>
  end if;
end method;

