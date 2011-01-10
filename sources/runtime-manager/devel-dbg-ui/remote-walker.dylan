module:     devel-dbg-ui
synopsis:   A remote object-walker for the console debugger.
            This traces an object graph from a selected root, and gathers
            statistics on the amount of storage being taken up by objects
            of various classes.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <REMOTE-WALKER>
//    The class that holds all information necessary for walking.

define class <remote-walker> (<object>)

  constant slot remote-walker-debug-target :: <debug-target>,
    required-init-keyword: target:;

  constant slot remote-walker-root-object :: <remote-value>,
    required-init-keyword: root-object:;

  constant slot remote-walker-class-set :: <sequence>,
    init-value: #[],
    init-keyword: classes:;

  constant slot remote-walker-class-status :: <symbol>,
    init-value: #"trace-everything",
    init-keyword: class-status:;

  constant slot remote-walker-module-set :: <sequence>,
    init-value: #[],
    init-keyword: modules:;

  constant slot remote-walker-module-status :: <symbol>,
    init-value: #"trace-everything",
    init-keyword: module-status:;

  constant slot remote-walker-pages-visited :: <table> = make(<table>);
  constant slot remote-walker-wrapper-statistics :: <table> = make(<table>);
  slot remote-walker-total-storage :: <integer> = 0;
  slot remote-walker-total-instances-walked :: <integer> = 0;
end class;


///// <WRAPPER-INFORMATION-TUPLE>
//    The class that holds information accumulated about instances of
//    one particular (runtime) class.

define class <wrapper-information-tuple> (<object>)

  constant slot wrapper-actual-address :: <remote-value>,
    required-init-keyword: actual-address:;

  constant slot wrapper-presented-name :: <byte-string>,
    required-init-keyword: presented-name:;

  constant slot wrapper-module-name :: <byte-string>,
    required-init-keyword: module-name:;

  // The following two slots accumulate the statistics.

  slot wrapper-instances-walked :: <integer> = 0;

  slot wrapper-total-storage :: <integer> = 0;

  // The rest of the slots are used to cache information about how to
  // unpick instances that have this wrapper.

  constant slot wrapper-terminal? :: <boolean> = #f,
    init-keyword: terminal?:;

  constant slot wrapper-repeat-information :: <symbol> = #"no-repeat",
    init-keyword: repeat:;

  constant slot wrapper-repeat-count-offset :: <integer> = 0,
    init-keyword: repeat-count-offset:;

  constant slot wrapper-slot-count :: <integer> = 0,
    init-keyword: slot-count:;

end class;


///// WALK-FROM
//    The recusive unit of the remote tracer. 

define method walk-from
    (walker :: <remote-walker>, instance-address :: <remote-value>)
 => ()
  let application = walker.remote-walker-debug-target;
  let path = application.debug-target-access-path;
  let wrapper = remote-instance-wrapper(application, instance-address);
  let bytes-per-slot = remote-value-byte-size(path);

  local method slot-element (i :: <integer>) => (obj :: <remote-value>)
          let addr = indexed-remote-value(instance-address, i + 1);
          let obj = read-value(path, addr);
          obj;
	end method;

  local method update-and-walk-out()
          let wrapper-condensed = as-integer-losing-precision(wrapper);
          let wrec =
            element(walker.remote-walker-wrapper-statistics,
                    wrapper-condensed,
                    default: #f) |
            add-new-wrapper-record(walker, wrapper-condensed, wrapper);
          wrec.wrapper-instances-walked :=
             wrec.wrapper-instances-walked + 1;
          walker.remote-walker-total-instances-walked :=
             walker.remote-walker-total-instances-walked + 1;
	  select (wrec.wrapper-repeat-information)

	    #"no-repeat" =>
	      wrec.wrapper-total-storage := wrec.wrapper-total-storage +
		(wrec.wrapper-slot-count + 1) * bytes-per-slot;
	      walker.remote-walker-total-storage := 
		walker.remote-walker-total-storage +
		(wrec.wrapper-slot-count + 1) * bytes-per-slot;
              unless (wrec.wrapper-terminal?)
                for (i from 0 below wrec.wrapper-slot-count)
                  walk-from(walker, slot-element(i));
	        end for;
	      end unless;

	    #"byte-repeat" =>
	      let (should-be-integer, ok) =
		dylan-object-immediate-value
                  (application, 
                   slot-element(wrec.wrapper-repeat-count-offset));
	      let number-repeats = 
		if (ok & instance?(should-be-integer, <integer>))
                  should-be-integer
		else
                  0
		end if;
	      wrec.wrapper-total-storage := wrec.wrapper-total-storage +
		(wrec.wrapper-slot-count + 1) * bytes-per-slot +
		number-repeats;
	      walker.remote-walker-total-storage := 
		walker.remote-walker-total-storage +
		(wrec.wrapper-slot-count + 1) * bytes-per-slot +
		number-repeats;
              unless (wrec.wrapper-terminal?)
                for (i from 0 below wrec.wrapper-slot-count)
                  walk-from(walker, slot-element(i));
	        end for;
	      end unless;

	    #"slot-repeat" =>
	      let (should-be-integer, ok) =
		dylan-object-immediate-value
                  (application, 
                   slot-element(wrec.wrapper-repeat-count-offset));
	      let number-repeats = 
		if (ok & instance?(should-be-integer, <integer>))
                  should-be-integer
		else
                  0
		end if;
	      wrec.wrapper-total-storage := wrec.wrapper-total-storage +
		(wrec.wrapper-slot-count + 1) * bytes-per-slot +
		number-repeats * bytes-per-slot;
	      walker.remote-walker-total-storage := 
		walker.remote-walker-total-storage +
		(wrec.wrapper-slot-count + 1) * bytes-per-slot +
		number-repeats * bytes-per-slot;
              unless (wrec.wrapper-terminal?)
                for (i from 0 below wrec.wrapper-slot-count)
                  walk-from(walker, slot-element(i));
	        end for;
                for (i from 0 below number-repeats)
                  walk-from(walker,
                            slot-element
                              (wrec.wrapper-repeat-count-offset + i + 1))
		end for;
	      end unless;

	  end select;
	end method;

  if (wrapper)
    let (page, offset) = page-relative-address(path, instance-address);
    let offsets-visited =
      element(walker.remote-walker-pages-visited, page, default: #f);
    if (offsets-visited)
      let visted-object-already? =
        element(offsets-visited, offset, default: #f);
      unless (visted-object-already?)
        offsets-visited[offset] := instance-address;
        update-and-walk-out();
      end unless;
    else
      let newpage = make(<table>);
      newpage[offset] := instance-address;
      walker.remote-walker-pages-visited[page] := newpage;
      update-and-walk-out();
    end if;
  end if;
end method;


///// ADD-NEW-WRAPPER-RECORD
//    This is called when the walker enters an object whose wrapper has
//    not yet been encountered.

define method add-new-wrapper-record
    (walker :: <remote-walker>, condensed-address :: <integer>,
     actual-address :: <remote-value>)
  => (rec :: <wrapper-information-tuple>)

  let (symbolic-name, presented-name, module-name, slot-count, 
       repeat-information, repeat-offset)
         = wrapper-trace-information(walker.remote-walker-debug-target,
                                     actual-address);
  let terminal? = #f;

  format-out("Creating wrapper record for %s:%s ", 
             presented-name, module-name);

  select (walker.remote-walker-class-status)
    #"include" =>
      terminal? :=
	~member?(presented-name, walker.remote-walker-class-set, test: \=);
    #"exclude" =>
      terminal? :=
	member?(presented-name, walker.remote-walker-class-set, test: \=);
    otherwise => terminal? := #f;
  end select;

  unless (terminal?)
    select (walker.remote-walker-module-status)
      #"include" =>
	terminal? :=
	  ~member?(module-name, walker.remote-walker-module-set, test: \=);
      #"exclude" =>
	terminal? :=
	  member?(module-name, walker.remote-walker-module-set, test: \=);
      otherwise => terminal? := #f;
    end select;
  end unless;

  if ((presented-name = "<BYTE-STRING>") | (presented-name = "<BYTE-VECTOR>"))
    terminal? := #t;
    repeat-information := #"byte-repeat";
    format-out(" (byte repeated) ");
  end if;

  if (terminal?) format-out("[terminal]") end if; format-out("\n");

  let rec = make(<wrapper-information-tuple>,
                 actual-address: actual-address,
                 symbolic-name: symbolic-name,
                 presented-name: presented-name,
                 module-name: module-name,
                 terminal?: terminal?,
                 repeat: repeat-information,
                 repeat-count-offset: repeat-offset,
                 slot-count: slot-count);

  walker.remote-walker-wrapper-statistics[condensed-address] := rec;
  ignore(rec.wrapper-actual-address);
  rec;
end method;


///// DISPLAY-WALK-RESULTS
//    Having finished a walk, display the results.

define method display-walk-results (walker :: <remote-walker>) => ()
  local method sep ()
          format-out("----------------------------------------"
                     "---------------------------------------\n");
	end method;
  let application = walker.remote-walker-debug-target;
  let path = application.debug-target-access-path;
  let root = walker.remote-walker-root-object;
  let total-instances = walker.remote-walker-total-instances-walked;
  let total-storage = walker.remote-walker-total-storage;
  let finst = as(<single-float>, total-instances);
  let fstor = as(<single-float>, total-storage);
  let hex-root = remote-value-as-string(path, root, 16);
  let printed-root = debugger-print-object(application, root, length: 10);
  sep();
  format-out("Result of heap walk for 0x%s : %s\n", hex-root, printed-root);
  sep();
  format-out("Instance    (%%)     Storage      (%%)   Class:Module\n");
  format-out("count               (bytes)\n");
  sep();
  for (entry in walker.remote-walker-wrapper-statistics)
    let this-count = entry.wrapper-instances-walked;
    let this-storage = entry.wrapper-total-storage;
    let fc = as(<single-float>, this-count);
    let fs = as(<single-float>, this-storage);
    let pc = ((fc * 100.0) / finst); 
    let ps = ((fs * 100.0) / fstor);
    let count-percent = floor(pc);
    let storage-percent = floor(ps);
    let this-name = entry.wrapper-presented-name;
    let this-module = entry.wrapper-module-name;
    format-out("%8d    (%3d)  %8d     (%3d)  %s:%s\n",
               this-count, count-percent, this-storage, storage-percent,
               this-name, this-module);
  end for;
  sep();
  format-out("Total instances examined: %d\n", total-instances);
  format-out("Number of different classes of object: %d\n", 
             size(walker.remote-walker-wrapper-statistics));
  format-out("Total memory consumption of data structure: %d bytes.\n",
             total-storage);
  sep();
end method;
