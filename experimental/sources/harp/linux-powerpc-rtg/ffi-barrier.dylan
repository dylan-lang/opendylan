module:    linux-powerpc-rtg
Synopsis:  Managing the FFI barrier, allocation of TEBs & entry points
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




define c-full runtime-variable thread-local-storage = "%thread-local-storage", data: 0, 
  repeat: 6,
  section: #"ambiguous-data";


define sideways method op--initialize-teb-register
    (be :: <powerpc-back-end>) => ()
  ins--move(be, reg--teb, thread-local-storage);
end method;


define sideways method op--create-TEB-tlv-index 
    (be :: <linux-powerpc-back-end>) => ()
  with-harp (be)
    tag done;
    c-result c-result;
    stack stack;

    // op--call-c(be, raw-malloc, 4);
    ins--push(be, 0); // a dummy variable
    ins--move(be, TEB-tlv-index, stack); // address of that variable
  end with-harp;
end method;

define sideways method op--get-teb-tlv
    (be :: <linux-powerpc-back-end>, dest :: <register>) => ()
  with-harp (be)
    ins--ld(be, dest, TEB-tlv-index, 0);
  end with-harp;
end method;

define sideways method op--set-teb-tlv
    (be :: <linux-powerpc-back-end>, val) => ()
  with-harp (be)
    ins--st(be, val, TEB-tlv-index, 0);
  end with-harp;
end method;

define sideways method op--free-teb-tlv
    (be :: <linux-powerpc-back-end>) => ()
  with-harp (be)
    op--call-c(be, mm-FreeMemory, TEB-tlv-index, 4);
  end with-harp;
end method;


define sideways method op--get-module-handle(be :: <linux-powerpc-back-end>) => ()
end method;



define no-export linux-API-runtime-primitive dylan-thread-trampoline
  ("dylan_thread_trampoline")
  op--dylan-thread-trampoline(be, #f);
end linux-API-runtime-primitive;


// Is this relevant for ELF loaders?

define shared init linux-API-runtime-primitive dylan-shared-object-entry
  ("DylanSOEntry")
  c-result c-result;
  
  /*
  when-base
    // While we are using the C runtime, we must initialize it. TEMPORARY
    op--stdcall-c(be, c-crt-init);
  end when-base;
  */

  // record the module handle for DLLs
  // ins--st(be, hinstDll, module-hinstance, 0);

  when-base
    // Cold start the runtime system ...
    op--initialize-master-thread(be);
  end when-base;
  ins--call-alien(be, primitive-dylan-initialize-ref, 0);

  /*
  when-base
    op--call-c(be, check-runtime-expiration);
  end when-base;
  */

  ins--move(be, c-result, 0);
  ins--rts-and-drop(be, 0);

end linux-API-runtime-primitive;

// Is this relevant for ELF loaders?

define shared init linux-API-runtime-primitive dylan-shared-object-exit
  ("DylanSOExit")
  c-result c-result;
  
  // Uninitialize any DLL roots etc.
  op--shut-down-library(be);
  when-base
    // Do any deregistration of the MM state for the master thread here
    op--maybe-uninitialize-thread-for-p-detach(be);
    // completely close down the MM etc.
    op--shut-down-dylan-library(be);
  end when-base;

  ins--move(be, c-result, 0);
  ins--rts-and-drop(be, 0);

end linux-API-runtime-primitive;


define sideways method op--init-dylan-data (be :: <linux-powerpc-back-end>) => ()
  with-harp (be)
    let data-start  = ins--constant-ref(be, $data-start-symbol);
    let data-end    = ins--constant-ref(be, $data-end-symbol);
    let objs-start  = ins--constant-ref(be, $objs-start-symbol);
    let objs-end    = ins--constant-ref(be, $objs-end-symbol);
    let vars-start  = ins--constant-ref(be, $vars-start-symbol);
    let vars-end    = ins--constant-ref(be, $vars-end-symbol);

    op--call-iep(be, primitive-register-traced-roots-ref, 
		 data-start, data-end, %ambig-root,
		 objs-start, objs-end, %static-root,
		 vars-start, vars-end, %exact-root);
    ins--rts(be);
  end with-harp;
end method;
