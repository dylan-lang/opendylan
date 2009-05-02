module:    linux-rtg
Synopsis:  Managing the FFI barrier, allocation of TEBs & entry points
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define c-fun runtime-external tlv-create-key    = "tlv_create_key";
define c-fun runtime-external tlv-destroy-key   = "tlv_destroy_key";
define c-fun runtime-external tlv-get-value     = "tlv_get_value";
define c-fun runtime-external tlv-set-value     = "tlv_set_value";

define sideways method op--create-TEB-tlv-index 
    (be :: <native-unix-back-end>) => ()
  with-harp (be)
    tag done;
    c-result c-result;
    op--call-c(be, tlv-create-key);
    ins--move(be, TEB-tlv-index, c-result);
  end with-harp;
end method;

define sideways method op--get-teb-tlv
    (be :: <native-unix-back-end>, dest :: <register>) => ()
  with-harp (be)
    c-result c-result;
    op--call-c(be, tlv-get-value, TEB-tlv-index);
    ins--move(be, dest, c-result);
  end with-harp;
end method;

define sideways method op--set-teb-tlv
    (be :: <native-unix-back-end>, val) => ()
  with-harp (be)
    op--call-c(be, tlv-set-value, TEB-tlv-index, val);
  end with-harp;
end method;

define sideways method op--free-teb-tlv
    (be :: <native-unix-back-end>) => ()
  with-harp (be)
    op--call-c(be, tlv-destroy-key, TEB-tlv-index);
  end with-harp;
end method;


define sideways method op--get-module-handle(be :: <native-unix-back-end>) => ()
end method;



define no-export unix-API-runtime-primitive dylan-thread-trampoline
  ("dylan_thread_trampoline")
  op--dylan-thread-trampoline(be, #f);
end unix-API-runtime-primitive;




define shared init unix-API-runtime-primitive dylan-shared-object-entry
  ("DylanSOEntry")
  c-result c-result;
  
  op--initialize-thread-instructions(be);

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

  ins--move(be, c-result, 0);
  ins--rts-and-drop(be, 0);

end unix-API-runtime-primitive;

define open generic op--initialize-thread-instructions (be :: <native-unix-back-end>) => ();

define sideways method op--initialize-thread-instructions (be :: <native-unix-back-end>) => ()
end method;

define shared init unix-API-runtime-primitive dylan-shared-object-exit
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

end unix-API-runtime-primitive;


define sideways method op--init-dylan-data (be :: <native-unix-back-end>) => ()
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
