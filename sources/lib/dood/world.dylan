Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $dood-default-number-of-buffers = 128;
define constant $dood-default-buffer-size       = 16 * 1024;

define function mask-from-power-of-two
    (size :: <integer>) => (res :: <integer>)
  size - 1
end function;

define function bit-from-power-of-two
    (size :: <integer>) => (res :: <integer>)
  iterate search (bit :: <integer> = 0, x :: <integer> = size)
    if (logbit?(0, x))
      bit
    else 
      search(bit + 1, ash(x, -1))
    end if
  end iterate
end function;

define variable *dood-number-of-buffers*
  = $dood-default-number-of-buffers;
define variable *dood-buffer-size*
  = $dood-default-buffer-size;
define variable *dood-address-buffer-mask* 
  = mask-from-power-of-two(*dood-buffer-size*);
define variable *dood-address-buffer-bit-offset* 
  = bit-from-power-of-two(*dood-buffer-size*);

define inline method dood-number-of-buffers ()
  *dood-number-of-buffers*
end method;

define inline method dood-buffer-size () => (res :: <integer>)
  *dood-buffer-size*
end method;

define inline method dood-address-buffer-mask () => (res :: <integer>)
  *dood-address-buffer-mask*
end method;

define inline method dood-address-buffer-bit-offset () => (res :: <integer>)
  *dood-address-buffer-bit-offset*
end method;

define constant $auditing?     = #f;
define constant $default-audit-locator
  = "c:\\dylan\\candy-6\\logs\\dood-audit.txt";

define method open-log-stream () => (res :: false-or(<file-stream>))
  when ($auditing?)
    make(<file-stream>, 
	 locator:           $default-audit-locator,
	 element-type:      <byte-character>,
	 direction:         #"output",
	 if-does-not-exist: #"create",
	 if-exists:         #"replace");
  end when;
end method;

// define inline method audit 
//     (dood :: <dood>, format-string :: <byte-string>, #rest arguments)
//   when ($auditing?)
//     apply(format, dood-world-log-stream(dood-world(dood)), 
// 	  format-string, dood-index(dood), arguments)
//   end when;
// end method;

define class <dood-world> (<object>)
  slot dood-world-doods :: <object-table> = make(<object-table>);
//  slot dood-world-objects :: <dood-table> = make-weak-key-table();
  constant slot dood-world-classes :: <object-table> = make(<object-table>);
  slot dood-world-buffer-pool :: <buffer-vector>
    = make(<buffer-vector>, 
	   number-of-buffers: dood-number-of-buffers(),
	   buffer-size:       dood-buffer-size());
  slot dood-world-dood-next-index :: <integer> = 0;
  constant slot dood-world-dood-indices :: <object-table> = make(<table>);
  slot dood-world-log-stream :: false-or(<file-stream>) = open-log-stream();
end class;

define variable *default-dood-world* :: <dood-world> = make(<dood-world>);

define method dood-number-of-buffers-setter (new-number)
  *dood-number-of-buffers* := new-number;
  *default-dood-world* := make(<dood-world>);
end method;

define method dood-buffer-size-setter (new-size)
  *dood-buffer-size*
    := new-size;
  *dood-address-buffer-mask*
    := mask-from-power-of-two(*dood-buffer-size*);
  *dood-address-buffer-bit-offset*
    := bit-from-power-of-two(*dood-buffer-size*);
  *default-dood-world*
    := make(<dood-world>);
end method;

define method dood-world-default () => (res :: <dood-world>)
  *default-dood-world*
end method;

define method dood-world-default-setter (world :: <dood-world>)
  *default-dood-world* := world
end method;

define method dood-world-reset (world :: <dood-world>) => ()
//  dood-world-objects(world) := make-weak-key-table();
  dood-world-doods(world) := make(<object-table>);
  dood-world-buffer-pool(world)
    := make(<buffer-vector>, 
	    number-of-buffers: $dood-default-number-of-buffers, 
	    buffer-size:       $dood-default-buffer-size);
  dood-world-log-stream(world) := open-log-stream();
end method;

//// DOODS

define method dood-world-find-dood 
    (world :: <dood-world>, name :: <symbol>) => (dood :: false-or(<dood>))
  element(dood-world-doods(world), name, default: #f)
end method;

define method dood-index (dood :: <dood>) => (res :: <integer>)
  let world = dood-world(dood);
  element(dood-world-dood-indices(world), dood, default: #f)
    | begin
	let index = dood-world-dood-next-index(world);
	dood-world-dood-indices(world)[dood] := index;
	format(dood-world-log-stream(world), "%dO%s\n", 
	       index, as(<string>, dood-locator(dood)));
	dood-world-dood-next-index(world) := index + 1;
	index
      end
end method;

define method dood-world-register-dood 
    (world :: <dood-world>, dood :: <dood>) => ()
  dood-world-doods(world)[dood-name(dood)] := dood;
  $auditing? & dood-index(dood);
end method;

define method dood-world-unregister-dood 
    (world :: <dood-world>, dood :: <dood>) => ()
  remove-key!(dood-world-doods(world), dood-name(dood));
  if (empty?(dood-world-doods(world)))
    dood-world-reset(world)
  end if;
  when ($auditing?)
    audit(dood, "%dC\n");
    remove-key!(dood-world-dood-indices(world), dood);
    force-output(dood-world-log-stream(dood-world(dood)));
  end when;
end method;

/*
//// OBJECT-DOOD BOOKKEEPING

define method dood-world-object-dood
    (world :: <dood-world>, object) => (dood :: false-or(<dood>))
  element(dood-world-objects(world), object, default: #f)
end method;

define method dood-world-register-object-dood
    (world :: <dood-world>, dood :: <dood>, object) => ()
  element(dood-world-objects(world), object) := dood
end method;

define method dood-world-unregister-object-dood
    (world :: <dood-world>, dood :: <dood>, object) => ()
  remove-key!(dood-world-objects(world), object)
end method;
*/
