Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function check-not-read-only (dood :: <dood>)
  if (dood-read-only?(dood))
    // It's the caller's responsibility to check, so it's a bug if it happens.
    error("Bug: attempt to write a read-only dood");
  end;
end function;

define method dood-commit
    (dood :: <dood>, 
     #key flush?, dump?, break?, parents?, clear? = #t, stats?, size) => ()
  check-not-read-only(dood);
  dood-initialize-walker!(dood);
  dood-save-state(dood);
  dood-corrupted?(dood) := #t;
  let batch? = dood-batch-mode?(dood);
  if (batch?) // COMPLETE SAVE FROM SCRATCH
    // when (size)
    //   dood-initialize-walker!(dood, size: size);
    // end when;
    dood-addresses(dood) := dood-walked-addresses(dood);
  end if;
  let parents? = parents? | stats?;
  block ()
    local method commit-root (object, id)
	    dood-walk-from
	      (dood, identity, object, 
               batch?: batch?, parents?: parents?, commit?: #t);
	    dood-write-at(dood, walked-pointer(dood, object), id);
	  end method;
    commit-root($dood-empty-stretchy-vector, $dood-proxies-id);
    // commit-root(dood-proxies(dood), $dood-proxies-id);
    commit-root(dood-root(dood), $dood-root-id);
    dood-write-at
      (dood, disk-pointer(dood, $dood-version), 
       $dood-version-id);
    dood-write-at
      (dood, disk-pointer(dood, dood-specified-user-version(dood)), 
       $dood-user-version-id);
    dood-write-at
      (dood, disk-pointer(dood, dood-free-address(dood)), 
       $dood-free-address-id);
    dood-force-output(dood);
    unless (break?)
      dood-corrupted?(dood) := #f;
    end unless;
    dood-force-output(dood);
    if (dump?)
      dump(dood); 
    end if;
  cleanup
    if (clear?)
      dood-clear-walked-objects!(dood);
      if (batch?) // COMPLETE SAVE FROM SCRATCH
         dood-addresses(dood) := dood-walked-addresses(dood);
      end if;
    end if;
    if (dood-corrupted?(dood))
      dood-restore-state(dood)
    else
      dood-flush-backup(dood);
    end if;
    if (stats?)
      dood-statistics(dood);
    end if;
  end block;
end method;  
  
/*
define method dood-shallow-commit
    (dood :: <dood>, object) => (pointer :: <pointer>)
  // TODO: NYI
  object
end method;
*/

// eof
