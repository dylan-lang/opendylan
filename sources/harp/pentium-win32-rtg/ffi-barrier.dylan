module:    pentium-win32-rtg
Synopsis:  Managing the FFI barrier, allocation of TEBs & entry points
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define win-fun runtime-external win-TlsAlloc        = "TlsAlloc",       data:  "0";
define win-fun runtime-external win-TlsFree         = "TlsFree",        data:  "4";
define win-fun runtime-external win-TlsGetValue     = "TlsGetValue",    data:  "4";
define win-fun runtime-external win-TlsSetValue     = "TlsSetValue",    data:  "8";

define win-fun runtime-external win-GetModuleHandle        = "GetModuleHandleA",
  data:  "4", client?: #t, base?: #t;



define sideways method op--create-TEB-tlv-index 
    (be :: <x86-windows-back-end>) => ()
  with-harp (be)
    tag done;
    c-result c-result;

    op--stdcall-c(be, win-TlsAlloc);
    ins--move(be, TEB-tlv-index, c-result);
    ins--bne(be, done, c-result, -1);
    // If TlsAlloc returns -1 then we couldn't even allocate
    // the thread local variable. Big trouble.
    op--dylan-registration-error(be);
    ins--tag(be, done);
  end with-harp;
end method;

define sideways method op--get-teb-tlv
    (be :: <x86-windows-back-end>, dest :: <register>) => ()
  with-harp (be)
    c-result c-result;
    op--stdcall-c(be, win-TlsGetValue, TEB-tlv-index);
    ins--move(be, dest, c-result);
  end with-harp;
end method;

define sideways method op--set-teb-tlv
    (be :: <x86-windows-back-end>, val) => ()
  with-harp (be)
    op--stdcall-c(be, win-TlsSetValue, TEB-tlv-index, val);
  end with-harp;
end method;

define sideways method op--free-teb-tlv
    (be :: <x86-windows-back-end>) => ()
  with-harp (be)
    op--stdcall-c(be, win-TlsFree, TEB-tlv-index);
  end with-harp;
end method;

define sideways method op--get-stack-bottom
    (be :: <x86-windows-back-end>, dest :: <register>) => ()
  with-harp (be)
    ins--get-stack-bottom(be, dest);
  end with-harp;
end method;

define sideways method op--get-module-handle(be :: <x86-windows-back-end>) => ()
  with-harp (be)
    c-result c-result;
    op--stdcall-c(be, win-GetModuleHandle, 0);
    ins--st(be, c-result, module-hinstance, 0);  // record the module handle for EXEs
  end with-harp;
end method;


define sideways method op--shut-down-dll-library
    (be :: <x86-windows-back-end>) => ()
  op--call-iep(be, primitive-deregister-traced-roots-ref, 
	       %ambig-root, %static-root, %exact-root);
end method;

define sideways method op--shut-down-exe-library
    (be :: <x86-windows-back-end>) => ()
  op--call-iep(be, primitive-deregister-traced-roots-ref, 
	       %ambig-root, %static-root, %exact-root);
end method;


define no-export win32-API-runtime-primitive dylan-thread-trampoline
  ("dylan_thread_trampoline", "4")
  op--dylan-thread-trampoline(be, #t);
end win32-API-runtime-primitive;


// define win-fun runtime-external c-crt-init          = "_CRT_INIT",       data:  "12";


// DYLAN-DLL-ENTRY
//
// This is the normal entry point for DLL files

define shared init win32-API-runtime-primitive dylan-dll-entry ("DylanDllEntry", "12")
  nreg hinstDll, fdwReason, lpReserved;
  c-result c-result;
  tag tag-p-attach, tag-t-attach, tag-t-detach, tag-p-detach, done;

  let p-attach = 1;
  let t-attach = 2;
  let t-detach = 3;
  let p-detach = 0;
  
  op--c-load-arguments(be, hinstDll, fdwReason, lpReserved);

  /*
  when-base
    // While we are using the C runtime, we must initialize it. TEMPORARY
    op--stdcall-c(be, c-crt-init, hinstDll, fdwReason, lpReserved);
  end when-base;
  */

  ins--beq(be, tag-p-attach, p-attach, fdwReason);  
  ins--beq(be, tag-t-attach, t-attach, fdwReason);  
  ins--beq(be, tag-t-detach, t-detach, fdwReason);  
  ins--beq(be, tag-p-detach, p-detach, fdwReason);  

  ins--tag(be, done);

  ins--move(be, c-result, -1);
  ins--rts-and-drop(be, 12);

  ins--tag(be, tag-p-attach);
  ins--st(be, hinstDll, module-hinstance, 0);  // record the module handle for DLLs
  when-base
    // Cold start the runtime system ...
    op--initialize-master-thread(be);
  end when-base;
  ins--call-alien(be, primitive-dylan-initialize-ref, 0);
  ins--bra(be, done);

  ins--tag(be, tag-t-attach);
  when-base
    // Do any initialization of the TLV state for this thread here
    op--set-teb-tlv(be, $uninitialized-teb);
  end when-base;
  ins--bra(be, done);

  ins--tag(be, tag-t-detach);
  when-base
    // Do any deregistration of the MM state for this thread here
    op--maybe-uninitialize-thread(be);
  end when-base;
  ins--bra(be, done);

  ins--tag(be, tag-p-detach);
  // Uninitialize any DLL roots etc.
  op--shut-down-dll-library(be);
  when-base
    // Do any deregistration of the MM state for the master thread here
    op--maybe-uninitialize-thread-for-p-detach(be);
    // completely close down the MM etc.
    op--shut-down-dylan-library(be);
  end when-base;
  ins--bra(be, done);

end win32-API-runtime-primitive;


define sideways method op--init-dylan-data (be :: <x86-windows-back-end>) => ()
  with-harp (be)
    let data-start  = ins--constant-ref(be, $data-start-symbol);
    let data-end    = ins--constant-ref(be, $data-end-symbol);
    let objs-start  = ins--constant-ref(be, $objs-start-symbol);
    let objs-end    = ins--constant-ref(be, $objs-end-symbol);
    let vars-start  = ins--constant-ref(be, $vars-start-symbol);
    let vars-end    = ins--constant-ref(be, $vars-end-symbol);
    let fixup-start = ins--constant-ref(be, $fixup-start-symbol);
    let fixup-end   = ins--constant-ref(be, $fixup-end-symbol);
    let import-start = ins--constant-ref(be, $import-start-symbol);
    let import-end   = ins--constant-ref(be, $import-end-symbol);

    op--call-iep
      (be, primitive-reference(fixup-unimported-dylan-data),
       import-start, import-end);

    op--call-iep(be, primitive-reference(fixup-imported-dylan-data),
		 fixup-start, fixup-end);

    op--call-iep(be, primitive-register-traced-roots-ref, 
		 data-start, data-end, %ambig-root,
		 objs-start, objs-end, %static-root,
		 vars-start, vars-end, %exact-root);
    ins--rts(be);
  end with-harp;
end method;


/// Fixing up imported data:
///
/// The data in the fixup-section is organized as follows:
///   NB: definitive statement is in binary-builder/builder.dylan,
///   but these two files must be in-step
///
/// 
/// $fixup-start-symbol
///     obj file-1:     address of start of data for file-1       4 bytes
///           N1:       number of locations to update             * encoded
///           import1:  address of 1st object in import table     4 bytes
///                     1st data offset to fixup                  ** encoded
///                     ... more offsets ...                      ** encoded
///                     N1 data offset to fixup
///           N2:       number of locations to update
///           import2:  address of 2nd object in import table
///                     1st data offset to fixup
///                     ... more offsets ...
///                     N2 data offset to fixup
///           ... more imported objects to fixup ...
///           0                                                   1 byte
///     obj file-2:     address of start of data for file-2
///           ... imported objects to fixup ...
///           0                                                   1 byte
///     ... more object files ...
/// $fixup-end-symbol
///
/// * encoding is as follows:-
///   num          1 byte
///   long-num     2 bytes [optional - only present if num = #xff]
///   huge-num     4 bytes [optional - only present if long-num = #xffff]
///
/// The number of fixups for the import is calculated as follows:
///   It's huge-num if supplied, else it's long-num if supplied, else it's num
///
/// ** encoding is as follows:-
///   offset       1 byte
///   long-offset  2 bytes [optional - only present of offset = #xff]
///   position     4 bytes [optional - only present of long-offset = #xffff]
///
/// The address is calculated as follows:
///   if position is supplied, then it gives the address relative to start of data
///   if long-offset is supplied then it gives the address relative to the last
///     address in multiples of 4 bytes
///   otherwise, offset gives the address relative to the last address in 
///     multiples of 4 bytes


define used-by-client init runtime-primitive fixup-imported-dylan-data
  // On entry:
  //    fixup-start, fixup-end
  // On exit:
  //    no result - imported references in static data will have been fixed up
  
  result result;
  nreg fixup-start, fixup-end;
  tag done;

  op--load-arguments(be, fixup-start, fixup-end);

  ins--bhs(be, done, fixup-start, fixup-end);
  op--process-fixups-for-file(be, fixup-start, fixup-end, #f);

  ins--tag(be, done);
  ins--move(be, result, 1); // help the colourer by returning a value
  op--rts-dropping-n-args(be, 2);
end runtime-primitive;

// Dynamic derived import fixups require a sequence of indirections
// to use when fixing up objects at enumerated positions at runtime
// 
// The binary format for these differs slightly from .dyfix sections;
// currently, each import and its accompanying indirections mask and
// position are listed separately each time, room for improvement here.
// 
// Nosa Feb 24, 1999


define used-by-client init runtime-primitive fixup-unimported-dylan-data
  // On entry:
  //    fixup-start, fixup-end
  // On exit:
  //    no result - imported references in static data will have been fixed up
  
  result result;
  nreg fixup-start, fixup-end;
  tag done;

  op--load-arguments(be, fixup-start, fixup-end);

  ins--bhs(be, done, fixup-start, fixup-end);
  op--process-fixups-for-file(be, fixup-start, fixup-end, #t);

  ins--tag(be, done);
  ins--move(be, result, 1); // help the colourer by returning a value
  op--rts-dropping-n-args(be, 2);
end runtime-primitive;

define method op--process-fixups-for-file
    (be :: <x86-windows-back-end>,
     fixup-start :: <register>, fixup-end :: <register>,
     dynamic-linking? :: <boolean>) => ()
  with-harp (be)
    nreg file-relocs, base, cursor, offset;
    tag this-file, next-file, start, continue, error;

    ins--move(be, file-relocs, fixup-start);

    ins--tag(be, this-file);
    ins--ld(be, base, file-relocs, 0);    // address of base of data area for file
    ins--add(be, cursor, file-relocs, 4); // positioned at the relocs number

    ins--tag(be, start);
    if (dynamic-linking?)
      ins--ld(be, offset, cursor, 0);
      ins--add(be, cursor, cursor, 4);
      ins--beq(be, continue, offset, #xffffffff);
      op--process-fixups-for-import(be, base, cursor, offset,
				    start, next-file, error, #t);
      ins--tag(be, continue);
    end;

    op--process-fixups-for-import(be, base, cursor, offset,
				  start, next-file, error, #f);

    ins--tag(be, error);
    op--call-xep(be, dylan-error-function, not-yet-relocated-string);

    ins--tag(be, next-file);
    ins--move(be, file-relocs, cursor);   // Start of data for next file
    ins--blo(be, this-file, file-relocs, fixup-end);

  end with-harp;
end method;

define method op--process-fixups-for-import
    (be :: <x86-windows-back-end>,
     base :: <register>, cursor :: <register>, offset :: <register>,
     start :: <tag>, done :: <tag>, error :: <tag>,
     dynamic-linking? :: <boolean>) => ()
  with-harp (be)
    nreg num, obj-addr, obj, temp;
    tag obj-loop, found-num, long-num-tag, continue, dereference;

    ins--ldb(be, num, cursor, 0);
    ins--add(be, cursor, cursor, 1);      // position cursor after num (at import)
    ins--bne(be, obj-loop, num, 0);
    ins--bra(be, done);

    ins--tag(be, long-num-tag);           // handle the more complex "* encodings"
    ins--ldh(be, num, cursor, 0);         // num := long-num
    ins--add(be, cursor, cursor, 2);      // Move cursor past long-num
    ins--bne(be, found-num, num, #xffff); // branch if the "long-num" encoding was used
    ins--ld(be, num, cursor, 0);          // num := huge-num
    ins--add(be, cursor, cursor, 4);      // Move cursor past huge-num
    ins--bra(be, found-num);

    ins--tag(be, obj-loop);  // Start of loop where we process fixups for one import
    ins--beq(be, long-num-tag, num, #xff);// Branch if awkward "*" encoding
    ins--tag(be, found-num);              // End of "*" decoding
    ins--ld(be, obj-addr, cursor, 0);     // address of table entry for the import
    ins--ld(be, obj, obj-addr, 0);        // address of the object itself;
    ins--beq(be, error, obj, $not-yet-relocated-data);
    ins--add(be, cursor, cursor, 4);      // address of first offset
    if (dynamic-linking?)
      // Follow a path of indirections to derived object
      // from imported parent object
      ins--tag(be, dereference);
      ins--and(be, temp, offset, #xff);
      ins--beq(be, continue, temp, #xff);
      ins--asl(be, temp, temp, 2);
      ins--ld(be, obj, obj, temp);
      ins--asr(be, offset, offset, 8);
      ins--bra(be, dereference);
      ins--tag(be, continue);
    end;
    op--process-fixups-for-object(be, base, obj, cursor, num);
    ins--bra(be, start);

  end with-harp;
end method;


define method op--process-fixups-for-object
    (be :: <x86-windows-back-end>, 
     base :: <register>, obj :: <register>,
     cursor :: <register>, num :: <register>) => ()
  with-harp (be)
    nreg offset;   // help the colourer?
    nreg position; // help the colourer?
    tag offset-loop, offset-loop-test, long-offset-tag, found-offset, found-position;

    // Iterate over each address to fixup, storing the new value in place
    // num is the number still to be fixed up
    // cursor is the address of the next offset

    ins--move(be, position, base);    // this is the last address for the ** encoding
    
    // Start by testing for the end
    ins--bra(be, offset-loop-test);

    ins--tag(be, long-offset-tag);              // handle the more complex ** encoding
    ins--ldh(be, offset, cursor, 0);            // offset := long-offset
    ins--add(be, cursor, cursor, 2);            // Move cursor past long-offset
    ins--bne(be, found-offset, offset, #xffff); // branch if the "long-offset" encoding was used
    ins--ld(be, offset, cursor, 0);             // offset := position 
    ins--add(be, cursor, cursor, 4);            // Move cursor past position
    ins--add(be, position, base, offset);       // allow for the base
    ins--bra(be, found-position);

    ins--tag(be, offset-loop);
    ins--ldb(be, offset, cursor, 0);  // load the offset which needs to be fixed up
    ins--add(be, cursor, cursor, 1);  // position cursor past offset
    ins--beq(be, long-offset-tag, offset, #xff);
    ins--tag(be, found-offset);
    ins--asl(be, offset, offset, 2);  // offset is in multiples of 4 bytes
    ins--add(be, position, position, offset);
    ins--tag(be, found-position);
    ins--st(be, obj, position, 0);  // and store the object in-place
    ins--sub(be, num, num, 1);      // one less to go now
    ins--tag(be, offset-loop-test);
    ins--bne(be, offset-loop, num, 0);

    // Return with cursor just past the last offset
  end with-harp;
end method;

