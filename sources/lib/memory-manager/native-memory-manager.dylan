module: memory-manager
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method room () 
  block()
    values(raw-as-integer(primitive-mps-committed()), 0)
  exception (e :: <arithmetic-error>)
    // yes, it's an ugly workaround, and I demand real integers in Dylan
    values($maximum-integer, 0)
  end
end method;

define method collect-garbage (#key print-stats?)
  block () 
    primitive-mps-collect(primitive-boolean-as-raw(print-stats?));
    room()
  afterwards
    primitive-mps-release();
  end;
end method;

define method mark-garbage () 
  collect-garbage()
end method;

define method block-promotion () 
end method;

define macro with-ramp-allocation
  { with-ramp-allocation(#key ?all?:expression = #f) ?:body end }
    => {

	block () 
	  if (?all?)
	    primitive-mps-begin-ramp-alloc-all();
	  else
	    primitive-mps-begin-ramp-alloc();
	  end;
	  ?body;
	cleanup 
	  if (?all?)
	    primitive-mps-end-ramp-alloc-all();
	  else
	    primitive-mps-end-ramp-alloc();
	  end;
	end block;

	}
end macro with-ramp-allocation;

define constant stats-buffer :: <simple-object-vector> = #[#f, #f, #f];

define method garbage-collection-stats()
 => (stats :: <stretchy-object-vector>)
  let stats-vector :: <stretchy-object-vector> =
    make(<stretchy-object-vector>);
  let i :: <integer> = 0;
  while(begin
	  drain-finalization-queue();
	  primitive-raw-as-boolean
	    (primitive-mps-collection-stats
	       (primitive-vector-as-raw(stats-buffer)))
	end)

    stats-vector[i] := stats-buffer[0];
    stats-vector[i + 1] := stats-buffer[1];
    stats-vector[i + 2] := stats-buffer[2];
    i := i + 3;
  end;
  stats-vector
end method;

define method enable-gc-messages() => ()
  primitive-mps-enable-gc-messages();
end method;
