Module:    bulk-io-internal
Author:    Seth LaForge
Synopsis:  bulk-io, we haul <byte>s...
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Aaaarrrrghhhh!!!  Why isn't this exported?
define constant <invalid-index-error> = <error>;


define sealed class <mm-dood-stream> (<positionable-stream>)
  constant slot mapped-file :: <memory-mapped-file>, 
	required-init-keyword: mapped-file:;
  slot stream-position :: <integer> = 0;
  slot stream-size :: <integer>;
end class <mm-dood-stream>;


define method initialize (this :: <mm-dood-stream>, #key #all-keys) => ()
  next-method();
  this.stream-size := this.mapped-file.size;
end;


// Note - this doesn't define the entire <stream> or <positionable-stream>
// protocols, but it should do enough for dood.


// Note: closes the <mapped-memory-file> too.
define method close (this :: <mm-dood-stream>, #key #all-keys) => ()
  close-mapped-memory(this.mapped-file, final-size: this.size);
end method close;


define method read-element (this :: <mm-dood-stream>, 
			    #key on-end-of-stream = unsupplied())
			=> (elem)
  block ()
    this.mapped-file[this.stream-position]
  afterwards
    this.stream-position := this.stream-position + 1;
  exception (<invalid-index-error>)
    if (unsupplied?(on-end-of-stream))
      error(make(<end-of-stream-error>, stream: this));
    else
      on-end-of-stream
    end if;
  end block;
end method read-element;


define method read (this :: <mm-dood-stream>, n :: <integer>, 
		    #key on-end-of-stream) 
		=> (r)
  ignore(on-end-of-stream);
  let r = make(<byte-vector>, size: n);
  read-into!(this, n, r);
  r
end method read;


define method read-byte-string (this :: <mm-dood-stream>, n :: <integer>, 
				#key on-end-of-stream) 
			    => (r)
  ignore(on-end-of-stream);
  let r = make(<byte-string>, size: n);
  read-into!(this, n, r);
  r
end method read-byte-string;


define method read-into! (this :: <mm-dood-stream>, n :: <integer>, 
			  seq :: <mutable-sequence>, #key start = 0, 
			  on-end-of-stream) 
		      => (count)
  ignore(on-end-of-stream);
  block ()
    copy-bytes(this.mapped-file, this.stream-position, seq, start, n);
    this.stream-position := this.stream-position + n;
    n
  exception (<invalid-index-error>)
    error(make(<end-of-stream-error>, stream: this));
  end block;
end method read-into!;


define method write-element (this :: <mm-dood-stream>, elem :: <byte>) => ()
  this.mapped-file[this.stream-position] := elem;
  this.stream-position := this.stream-position + 1;
  if (this.stream-position > this.stream-size)
    this.stream-size := this.stream-position;
  end if;
end method write-element;


define method write (this :: <mm-dood-stream>, seq :: <sequence>, 
		     #key start = 0, end: last = unsupplied())
		 => ()
  if (unsupplied?(last)) last := seq.size; end if;
  let n = last - start;
  copy-bytes(seq, start, this.mapped-file, this.stream-position, n);
  this.stream-position := this.stream-position + n;
  if (this.stream-position > this.stream-size)
    this.stream-size := this.stream-position;
  end if;
end method write;


define method read-word-32 (this :: <mm-dood-stream>) => (r :: <machine-word>)
  block ()
    word-32-element(this.mapped-file, this.stream-position)
  afterwards
    this.stream-position := this.stream-position + 4;
  exception (<invalid-index-error>)
    error(make(<end-of-stream-error>, stream: this));
  end block;
end method read-word-32;


define method write-word-32 (this :: <mm-dood-stream>, value :: <machine-word>)
			 => ()
  word-32-element(this.mapped-file, this.stream-position) := value;
  this.stream-position := this.stream-position + 4;
  if (this.stream-position > this.stream-size)
    this.stream-size := this.stream-position;
  end if;
end method write-word-32;


define method read-word-64 (this :: <mm-dood-stream>) => (r :: <machine-word>)
  block ()
    word-64-element(this.mapped-file, this.stream-position)
  afterwards
    this.stream-position := this.stream-position + 8;
  exception (<invalid-index-error>)
    error(make(<end-of-stream-error>, stream: this));
  end block;
end method read-word-64;


define method write-word-64 (this :: <mm-dood-stream>, value :: <machine-word>)
			 => ()
  word-64-element(this.mapped-file, this.stream-position) := value;
  this.stream-position := this.stream-position + 8;
  if (this.stream-position > this.stream-size)
    this.stream-size := this.stream-position;
  end if;
end method write-word-64;


define method force-output (this :: <mm-dood-stream>) => ()
  flush-mapped-memory(this.mapped-file);
end method force-output;


define method adjust-stream-position (this :: <mm-dood-stream>, 
				      delta :: <integer>,
				      #key from = #"current")
				  => (new-position :: <integer>)
  let origin :: <integer> = select (from)
			      #"start" => 0;
			      #"current" => this.stream-position;
			      #"end" => this.stream-size;
			    end select;
  this.stream-position := origin + delta;
  if (this.stream-position > this.stream-size)
    this.stream-size := this.stream-position;
  end if;
  this.stream-position
end method adjust-stream-position;
