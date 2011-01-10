Module:        remote-nub
Synopsis:      Functional interface implementations for the CORBA Debugger Nub of the Remote Debugger
Author:        Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define inline method import-<abstract-integer>
    (x :: <ffi-integer>) => (i :: <abstract-integer>)
  if (instance?(x, <integer>)) x
  else
    make(<double-integer>,
	 high: $minimum-unsigned-machine-word,
	 low: x);
  end if;
end method;

define inline method import-<vector>
    (v :: <POINTER-VECTOR>,
     #key sz) => (vector :: Rtmgr/RemoteNub/<RTARGET-ADDRESS-SEQ>)
  
  let size :: <integer> = sz | v.size;
  let vector = make (Rtmgr/RemoteNub/<RTARGET-ADDRESS-SEQ>,
		     size: size, fill: 0);
  for (i :: <integer> from 0 below size)
    let x :: <ffi-integer> = pointer-value(v, index: i);
    vector[i] := import-<abstract-integer>(x);
  end for;

  vector

end method;

define inline method import-<object>(x) => (x)
  x
end method;

define inline method export-<abstract-integer>
    (x :: <abstract-integer>) => (i :: <ffi-integer>)
  if (instance?(x, <integer>)) x
  else
    as(<machine-word>, x);
  end if;
end method;

define inline method export-<vector>
    (vector :: Rtmgr/RemoteNub/<RTARGET-ADDRESS-SEQ>,
     #key vec, sz) => (v :: <POINTER-VECTOR>)

  let size :: <integer> = sz | vector.size;
  let v :: <POINTER-VECTOR> = vec | (make (<POINTER-VECTOR>, element-count: size));
  for (i :: <integer> from 0 below size)
    let x :: <abstract-integer> = vector[i];
    pointer-value(v, index: i) := export-<abstract-integer>(x);
  end for;

  v

end method;

define inline method export-<object>(x) => (x)
  x
end method;


define macro corba-type-definer
  {
   define corba-type ?type:name = ?alias:name
  }
    =>
  {
   define constant "import-" ## ?type = "import-" ## ?alias;

   define constant "export-" ## ?type = "export-" ## ?alias
  }
end macro;

define corba-type Rtmgr/RemoteNub/<RNUBHANDLE>            = <abstract-integer>;
define corba-type Rtmgr/RemoteNub/<RNUBTHREAD>            = <abstract-integer>;
define corba-type Rtmgr/RemoteNub/<RNUBPROCESS>           = <abstract-integer>;
define corba-type Rtmgr/RemoteNub/<RNUBLIBRARY>           = <abstract-integer>;
define corba-type Rtmgr/RemoteNub/<RNUB>                  = <abstract-integer>;
define corba-type Rtmgr/RemoteNub/<RTARGET-ADDRESS>       = <abstract-integer>;

define corba-type Rtmgr/RemoteNub/<NUBINT>                = <abstract-integer>;
define corba-type Rtmgr/RemoteNub/<NUB-INDEX>             = <abstract-integer>;
define corba-type Rtmgr/RemoteNub/<NUB-ERROR>             = <abstract-integer>;

define corba-type Rtmgr/RemoteNub/<RTARGET-ADDRESS-SEQ>   = <object>;
define corba-type Rtmgr/RemoteNub/<STRING-SEQ>            = <object>;
define corba-type Rtmgr/RemoteNub/<NUBFLOAT>              = <object>;
define corba-type Rtmgr/RemoteNub/<NUBDOUBLE>             = <object>;
define corba-type CORBA/<string>                          = <object>;


define macro corba-method-definer
  {
   define corba-method ?method-name:name (?arg-specs:*) => (?result-specs:*)
     ?:body
   end
  }
    =>
  {
   define corba-method-aux ?method-name (?arg-specs) (?result-specs) (?result-specs)
     ?body
   end
  }
end macro;

define macro corba-method-aux-definer
  {
   define corba-method-aux ?method-name:name
     (?nub:name :: ?nub-implementation:name, ?arg-specs:*) (?result-specs:*) (?specs:*)
     ?:body
   end
  }
    =>
  {
   define method ?method-name (?nub :: ?nub-implementation, ?arg-specs)
    => (?result-specs)
     export-corba-values (?arg-specs)
       let (?specs) = begin ?body end;
       import-corba-values (?result-specs)
         values(?specs);
       end;
     end
   end
  }

specs:
  { } => { }
  { ?spec:name :: ?type:name, ...} => { ?spec, ...}

end macro;

define macro import-corba-values
  {
   import-corba-values ()
     ?:body
   end
  }
    => { ?body }

  {
   import-corba-values (?spec:name :: ?type:name, ?specs:*)
     ?:body
   end
  }
    =>
  {
   let ?spec :: ?type  = "import-" ## ?type(?spec);
   import-corba-values (?specs) ?body end
  }

end macro;

define macro export-corba-values
  {
   export-corba-values ()
     ?:body
   end
  }
    => { ?body }

  {
   export-corba-values (?spec:name :: ?type:name, ?specs:*)
     ?:body
   end
  }
    =>
  {
   let ?spec = "export-" ## ?type(?spec);
   export-corba-values (?specs) ?body end
  }

end macro;


/*
define corba-method Rtmgr/RemoteNub/create-and-debug-process
    (rnub :: <RemoteNub-implementation>, command :: CORBA/<string>, arguments :: CORBA/<string>, path-count :: Rtmgr/RemoteNub/<NUBINT>, lib-count :: Rtmgr/RemoteNub/<NUBINT>, working-dir :: CORBA/<string>, create-shell :: Rtmgr/RemoteNub/<NUBINT>)
 => (result :: Rtmgr/RemoteNub/<RNUB>)
  let process = remote-process(rnub);
  nub-create-and-debug-process
  (process, command, arguments, path-count, lib-count, working-dir, create-shell);
end corba-method;

define corba-method Rtmgr/RemoteNub/debug-active-process
    (rnub :: <RemoteNub-implementation>, process-name :: CORBA/<string>, process-id :: CORBA/<string>, actual-process-id :: CORBA/<unsigned-long>, path-count :: Rtmgr/RemoteNub/<NUBINT>, jit-info :: CORBA/<string>)
 => (result :: Rtmgr/RemoteNub/<RNUB>)
  let process = remote-process(rnub);
  nub-debug-active-process
  (process, process-name, process-id, actual-process-id, path-count, jit-info);
end corba-method;
*/

define corba-method Rtmgr/RemoteNub/remote-value-byte-size
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-remote-value-byte-size
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/get-process-page-fault-count
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-get-process-page-fault-count
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/thread-os-priority
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-thread-os-priority
  (process, nubthread);
end corba-method;

define corba-method Rtmgr/RemoteNub/get-thread-cpu-time
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-get-thread-cpu-time
  (process, nubthread);
end corba-method;

define corba-method Rtmgr/RemoteNub/get-library-base-address
    (rnub :: <RemoteNub-implementation>, dll :: Rtmgr/RemoteNub/<RNUBLIBRARY>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-get-library-base-address
  (process, dll);
end corba-method;

define corba-method Rtmgr/RemoteNub/get-library-version
    (rnub :: <RemoteNub-implementation>, dll :: Rtmgr/RemoteNub/<RNUBLIBRARY>)
 => (maj :: Rtmgr/RemoteNub/<NUBINT>, min :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-get-library-version
  (process, dll);
end corba-method;

define corba-method Rtmgr/RemoteNub/get-library-filename
    (rnub :: <RemoteNub-implementation>, dll :: Rtmgr/RemoteNub/<RNUBLIBRARY>)
 => (result :: CORBA/<string>)
  let process = remote-process(rnub);
  let name-length :: <integer> =
    nub-get-library-filename-length(process, dll);
  let C-filename = make (<byte-string>, size: name-length);
  nub-get-library-filename 
    (process, dll, name-length, C-filename);
  C-filename
end corba-method;

define corba-method Rtmgr/RemoteNub/get-library-undecorated-name
    (rnub :: <RemoteNub-implementation>, dll :: Rtmgr/RemoteNub/<RNUBLIBRARY>)
 => (result :: CORBA/<string>)
  let process = remote-process(rnub);
  let name-length =
    nub-get-library-undecorated-name-length(process, dll);
  let basic-name = make(<byte-string>, size: name-length);
  nub-get-library-undecorated-name
  (process, dll, name-length, basic-name);
  basic-name
end corba-method;

define corba-method Rtmgr/RemoteNub/get-register-name
    (rnub :: <RemoteNub-implementation>, reg :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: CORBA/<string>)
  let process = remote-process(rnub);
  let name-length :: <integer> =
    nub-get-register-name-length(process, reg);
  let register-name = make (<byte-string>, size: name-length);
  nub-get-register-name
  (process, reg, name-length, register-name);
  register-name
end corba-method;

define corba-method Rtmgr/RemoteNub/get-register-enumeration-code
    (rnub :: <RemoteNub-implementation>, reg :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-get-register-enumeration-code
  (process, reg);
end corba-method;

define corba-method Rtmgr/RemoteNub/all-registers
    (rnub :: <RemoteNub-implementation>)
 => (first :: Rtmgr/RemoteNub/<NUBINT>, last :: Rtmgr/RemoteNub/<NUBINT>)
     let process = remote-process(rnub);
     nub-all-registers
     (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/general-registers
    (rnub :: <RemoteNub-implementation>)
 => (first :: Rtmgr/RemoteNub/<NUBINT>, last :: Rtmgr/RemoteNub/<NUBINT>)
     let process = remote-process(rnub);
     nub-general-registers
     (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/special-registers
    (rnub :: <RemoteNub-implementation>)
 => (first :: Rtmgr/RemoteNub/<NUBINT>, last :: Rtmgr/RemoteNub/<NUBINT>)
     let process = remote-process(rnub);
     nub-special-registers
     (process);
end corba-method;

/*
define corba-method Rtmgr/RemoteNub/floating-registers
    (rnub :: <RemoteNub-implementation>)
 => (first :: Rtmgr/RemoteNub/<NUBINT>, last :: Rtmgr/RemoteNub/<NUBINT>)
     let process = remote-process(rnub);
     nub-floating-registers
     (process);
end corba-method;
*/

define corba-method Rtmgr/RemoteNub/page-read-permission
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-page-read-permission
  (process, address);
end corba-method;

define corba-method Rtmgr/RemoteNub/page-write-permission
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-page-write-permission
  (process, address);
end corba-method;

define corba-method Rtmgr/RemoteNub/page-relative-address
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>, offset :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-page-relative-address
  (process, address);
end corba-method;

define corba-method Rtmgr/RemoteNub/virtual-page-size
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-virtual-page-size
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/read-value-from-process-memory
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-read-value-from-process-memory
    (process, address);
end corba-method;

define corba-method Rtmgr/RemoteNub/write-value-to-process-memory
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, val :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (status :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-write-value-to-process-memory
  (process, address, val);
end corba-method;

define corba-method Rtmgr/RemoteNub/calculate-stack-address
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, offset :: Rtmgr/RemoteNub/<NUBINT>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-calculate-stack-address
  (process, nubthread, offset);
end corba-method;

define corba-method Rtmgr/RemoteNub/target-address-to-string
    (rnub :: <RemoteNub-implementation>, x :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, sz :: Rtmgr/RemoteNub/<NUBINT>, radix :: Rtmgr/RemoteNub/<NUBINT>, pad :: Rtmgr/RemoteNub/<NUBINT>)
 => (result :: CORBA/<string>, truncated :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  let str = make(<byte-string>, size: sz);
  let trunc? =
    nub-target-address-to-string
    (process, x, sz, str, radix, pad);
  values(str, trunc?)
end corba-method;

define corba-method Rtmgr/RemoteNub/string-to-target-address
    (rnub :: <RemoteNub-implementation>, sz :: Rtmgr/RemoteNub/<NUBINT>, buffer :: CORBA/<string>, radix :: Rtmgr/RemoteNub/<NUBINT>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, overflow :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-string-to-target-address
  (process, sz, buffer, radix);
end corba-method;

define corba-method Rtmgr/RemoteNub/read-single-float-from-process-memory
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUBFLOAT>, status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-read-single-float-from-process-memory
  (process, address);
end corba-method;

define corba-method Rtmgr/RemoteNub/write-single-float-to-process-memory
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, value :: Rtmgr/RemoteNub/<NUBFLOAT>)
 => (status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-write-single-float-to-process-memory
  (process, address, value);
end corba-method;

define corba-method Rtmgr/RemoteNub/read-double-float-from-process-memory
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUBDOUBLE>, status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-read-double-float-from-process-memory
  (process, address);
end corba-method;

define corba-method Rtmgr/RemoteNub/write-double-float-to-process-memory
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, value :: Rtmgr/RemoteNub/<NUBDOUBLE>)
 => (status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-write-double-float-to-process-memory
  (process, address, value);
end corba-method;

define corba-method Rtmgr/RemoteNub/read-byte-string-from-process-memory
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, sz :: Rtmgr/RemoteNub/<NUBINT>)
 => (buffer :: CORBA/<string>, status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  let string-destination = make (<byte-string>, size: sz);
  values(string-destination,
	 nub-read-byte-string-from-process-memory
	   (process, address, sz, string-destination));
end corba-method;

define corba-method Rtmgr/RemoteNub/write-byte-string-to-process-memory
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, sz :: Rtmgr/RemoteNub/<NUBINT>, buffer :: CORBA/<string>)
 => (status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-write-byte-string-to-process-memory
  (process, address, sz, buffer);
end corba-method;

define corba-method Rtmgr/RemoteNub/read-value-from-process-register-in-stack-frame
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, reg :: Rtmgr/RemoteNub/<NUB-INDEX>, frame-index :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-read-value-from-process-register-in-stack-frame
  (process, nubthread, reg, frame-index);
end corba-method;

define corba-method Rtmgr/RemoteNub/read-value-from-process-register
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, reg :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-read-value-from-process-register
  (process, nubthread, reg);
end corba-method;

define corba-method Rtmgr/RemoteNub/write-value-to-process-register
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, reg :: Rtmgr/RemoteNub/<NUB-INDEX>, value :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-write-value-to-process-register
  (process, nubthread, reg, value);
end corba-method;

define corba-method Rtmgr/RemoteNub/read-single-float-from-process-register
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, reg :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<NUBFLOAT>, status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-read-single-float-from-process-register
  (process, nubthread, reg);
end corba-method;

define corba-method Rtmgr/RemoteNub/write-single-float-to-process-register
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, reg :: Rtmgr/RemoteNub/<NUB-INDEX>, value :: Rtmgr/RemoteNub/<NUBFLOAT>)
 => (status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-write-single-float-to-process-register
  (process, nubthread, reg, value);
end corba-method;

define corba-method Rtmgr/RemoteNub/read-double-float-from-process-register
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, reg :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<NUBDOUBLE>, status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-read-double-float-from-process-register
  (process, nubthread, reg);
end corba-method;

define corba-method Rtmgr/RemoteNub/write-double-float-to-process-register
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, reg :: Rtmgr/RemoteNub/<NUB-INDEX>, value :: Rtmgr/RemoteNub/<NUBDOUBLE>)
 => (status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-write-double-float-to-process-register
  (process, nubthread, reg, value);
end corba-method;

define corba-method Rtmgr/RemoteNub/application-restart
    (rnub :: <RemoteNub-implementation>)
 => ()
  let process = remote-process(rnub);
  nub-application-restart
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/application-stop
    (rnub :: <RemoteNub-implementation>)
 => ()
  let process = remote-process(rnub);
  nub-application-stop
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/application-continue
    (rnub :: <RemoteNub-implementation>)
 => ()
  let process = remote-process(rnub);
  nub-application-continue
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/application-continue-unhandled
    (rnub :: <RemoteNub-implementation>)
 => ()
  let process = remote-process(rnub);
  nub-application-continue-unhandled
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/application-step
    (rnub :: <RemoteNub-implementation>, n :: Rtmgr/RemoteNub/<NUBINT>)
 => ()
  let process = remote-process(rnub);
  nub-application-step
  (process, n);
end corba-method;

define corba-method Rtmgr/RemoteNub/application-step-over
    (rnub :: <RemoteNub-implementation>, n :: Rtmgr/RemoteNub/<NUBINT>)
 => ()
  let process = remote-process(rnub);
  nub-application-step-over
  (process, n);
end corba-method;

define corba-method Rtmgr/RemoteNub/application-step-out
    (rnub :: <RemoteNub-implementation>)
 => ()
  let process = remote-process(rnub);
  nub-application-step-out
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/set-stepping-control-on-thread
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, fp :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, calling-fp :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, location-count :: Rtmgr/RemoteNub/<NUBINT>, locs :: Rtmgr/RemoteNub/<RTARGET-ADDRESS-SEQ>, operation :: Rtmgr/RemoteNub/<NUBINT>)
 => (result :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);

  // Fill in a pre-allocated vector of addresses with those that the
  // client has calculated.

  let locations :: <REMOTE-ARG-ARRAY> =
    export-<vector>(locs, vec: rnub.stepping-locations-vector, sz: location-count);

  nub-set-stepping-control-on-thread
  (process, nubthread, fp, calling-fp, location-count, locations, operation);
end corba-method;

define corba-method Rtmgr/RemoteNub/clear-stepping-control-on-thread
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => ()
  let process = remote-process(rnub);
  nub-clear-stepping-control-on-thread
  (process, nubthread);
end corba-method;

define corba-method Rtmgr/RemoteNub/thread-stop
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => ()
  let process = remote-process(rnub);
  nub-thread-stop
  (process, nubthread);
end corba-method;

define corba-method Rtmgr/RemoteNub/thread-continue
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => ()
  let process = remote-process(rnub);
  nub-thread-continue
  (process, nubthread);
end corba-method;

define corba-method Rtmgr/RemoteNub/thread-suspendedQ
    (rnub :: <RemoteNub-implementation>, thread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  let result =
    nub-thread-suspended?(thread);
  if (result = #f) 0
  else 1
  end;
end corba-method;

define corba-method Rtmgr/RemoteNub/thread-suspended
    (rnub :: <RemoteNub-implementation>, thread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => ()
  let process = remote-process(rnub);
  nub-thread-suspended
  (thread);
end corba-method;

define corba-method Rtmgr/RemoteNub/thread-resumed
    (rnub :: <RemoteNub-implementation>, thread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => ()
  let process = remote-process(rnub);
  nub-thread-resumed
  (thread);
end corba-method;


define corba-method Rtmgr/RemoteNub/kill-application
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-kill-application
    (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/close-application
    (rnub :: <RemoteNub-implementation>)
 => ()
  let process = remote-process(rnub);
  nub-close-application
  (process);
end corba-method;

/*
define corba-method Rtmgr/RemoteNub/close-remote-tether
    (rnub :: <RemoteNub-implementation>)
 => ()
  let process = remote-process(rnub);
  nub-close-remote-tether
  (process);
end corba-method;
*/

define corba-method Rtmgr/RemoteNub/setup-function-call
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, func :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, arg-count :: Rtmgr/RemoteNub/<NUBINT>, args :: Rtmgr/RemoteNub/<RTARGET-ADDRESS-SEQ>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, cx-handle :: Rtmgr/RemoteNub/<RNUBHANDLE>)
  let process = remote-process(rnub);

  // Construct the vector of arguments

  let arg-vector :: <REMOTE-ARG-ARRAY> = export-<vector>(args, sz: arg-count);

  let (ret-addr, context-cookie) =
    nub-setup-function-call
    (process, nubthread, func, arg-count, arg-vector);

  // Since the debugger nub will now have copied the arguments onto the
  // runtime stack, we can destroy the allocated vector.

  destroy(arg-vector);

  values(ret-addr, context-cookie);

end corba-method;

define corba-method Rtmgr/RemoteNub/remote-call-spy
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, func :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, arg-count :: Rtmgr/RemoteNub/<NUBINT>, args :: Rtmgr/RemoteNub/<RTARGET-ADDRESS-SEQ>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, status :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);

  // Construct the vector of arguments

  let arg-vector :: <REMOTE-ARG-ARRAY> = export-<vector>(args, vec: rnub.spy-function-argument-vector, sz: arg-count);

  // And make the call, returning the results from the nub.

  nub-remote-call-spy
  (process, nubthread, func, arg-count, arg-vector);
end corba-method;

define corba-method Rtmgr/RemoteNub/get-function-result
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-get-function-result
  (process, nubthread);
end corba-method;

define corba-method Rtmgr/RemoteNub/restore-context
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, the-context :: Rtmgr/RemoteNub/<RNUBHANDLE>)
 => ()
  let process = remote-process(rnub);
  nub-restore-context
  (process, nubthread, the-context);
end corba-method;

define corba-method Rtmgr/RemoteNub/set-breakpoint
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-set-breakpoint
  (process, address);
end corba-method;

define corba-method Rtmgr/RemoteNub/clear-breakpoint
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUB-ERROR>)
  let process = remote-process(rnub);
  nub-clear-breakpoint
  (process, address);
end corba-method;

define corba-method Rtmgr/RemoteNub/query-breakpoint
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-query-breakpoint
  (process, address);
end corba-method;

define corba-method Rtmgr/RemoteNub/wait-for-stop-reason-with-timeout
    (rnub :: <RemoteNub-implementation>, timeout :: Rtmgr/RemoteNub/<NUBINT>)
 => (code :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-wait-for-stop-reason-with-timeout
  (process, timeout);
end corba-method;

define corba-method Rtmgr/RemoteNub/profile-wait-for-stop-reason-with-timeout
    (rnub :: <RemoteNub-implementation>, timeout :: Rtmgr/RemoteNub/<NUBINT>, profiling-interval :: Rtmgr/RemoteNub/<NUBINT>)
 => (code :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-profile-wait-for-stop-reason-with-timeout
  (process, timeout, profiling-interval);
end corba-method;

define corba-method Rtmgr/RemoteNub/inform-profiling-started
    (rnub :: <RemoteNub-implementation>)
 => ()
  let process = remote-process(rnub);
  nub-inform-profiling-started
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/inform-profiling-stopped
    (rnub :: <RemoteNub-implementation>)
 => ()
  let process = remote-process(rnub);
  nub-inform-profiling-stopped
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/can-receive-first-chance
    (rnub :: <RemoteNub-implementation>, ecode :: Rtmgr/RemoteNub/<NUBINT>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-can-receive-first-chance
  (process, ecode);
end corba-method;

define corba-method Rtmgr/RemoteNub/set-first-chance
    (rnub :: <RemoteNub-implementation>, ecode :: Rtmgr/RemoteNub/<NUBINT>)
 => ()
  let process = remote-process(rnub);
  nub-set-first-chance
  (process, ecode);
end corba-method;

define corba-method Rtmgr/RemoteNub/unset-first-chance
    (rnub :: <RemoteNub-implementation>, ecode :: Rtmgr/RemoteNub/<NUBINT>)
 => ()
  let process = remote-process(rnub);
  nub-unset-first-chance
  (process, ecode);
end corba-method;

define corba-method Rtmgr/RemoteNub/thread-stop-information
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>, fchance :: Rtmgr/RemoteNub/<NUBINT>, fstart :: Rtmgr/RemoteNub/<NUBINT>, ret-addr :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-thread-stop-information
    (process, nubthread);
end corba-method;

define corba-method Rtmgr/RemoteNub/wait-for-stop-reason-no-timeout
    (rnub :: <RemoteNub-implementation>)
 => (ecode :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-wait-for-stop-reason-no-timeout
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/profile-wait-for-stop-reason-no-timeout
    (rnub :: <RemoteNub-implementation>, profile-interval :: Rtmgr/RemoteNub/<NUBINT>)
 => (ecode :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-profile-wait-for-stop-reason-no-timeout
  (process, profile-interval);
end corba-method;

define corba-method Rtmgr/RemoteNub/stop-reason-process
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<RNUB>)
  let process = remote-process(rnub);
  nub-stop-reason-process
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/stop-reason-thread
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<RNUBTHREAD>)
  let process = remote-process(rnub);
  nub-stop-reason-thread
    (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/first-hard-coded-breakpoint
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-first-hard-coded-breakpoint
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/stop-reason-process-exit-code
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-stop-reason-process-exit-code
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/stop-reason-thread-exit-code
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-stop-reason-thread-exit-code
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/stop-reason-library
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<RNUBLIBRARY>)
  let process = remote-process(rnub);
  nub-stop-reason-library
    (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/stop-reason-violation-op
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-stop-reason-violation-op
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/exception-first-chance
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-exception-first-chance
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/stop-reason-violation-address
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-stop-reason-violation-address
    (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/stop-reason-exception-address
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-stop-reason-exception-address
    (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/stop-reason-debug-string-address
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-stop-reason-debug-string-address
    (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/stop-reason-debug-string-length
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-stop-reason-debug-string-length
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/stop-reason-debug-string-is-unicode
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-stop-reason-debug-string-is-unicode
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/initialize-stack-vectors
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-initialize-stack-vectors
  (process, nubthread);
end corba-method;

define corba-method Rtmgr/RemoteNub/read-stack-vectors
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>, frame-count :: Rtmgr/RemoteNub/<NUBINT>)
 => (frame-pointers :: Rtmgr/RemoteNub/<RTARGET-ADDRESS-SEQ>, instruction-pointers :: Rtmgr/RemoteNub/<RTARGET-ADDRESS-SEQ>, return-addresses :: Rtmgr/RemoteNub/<RTARGET-ADDRESS-SEQ>)
  let process = remote-process(rnub);

  // Construct three primitive vectors that we can pass through the
  // FFI. One for frame pointers, one for return addresses, and one for
  // instruction pointers.

  let fp-vector = make(<POINTER-VECTOR>, element-count: frame-count);
  let ip-vector = make(<POINTER-VECTOR>, element-count: frame-count);
  let ra-vector = make(<POINTER-VECTOR>, element-count: frame-count);

  // Call the debugger nub to actually fill in the required data for each
  // stack frame.

  nub-read-stack-vectors
    (process, nubthread, frame-count,
     fp-vector, ip-vector, ra-vector);

  values(import-<vector>(fp-vector, sz: frame-count),
	 import-<vector>(ip-vector, sz: frame-count),
	 import-<vector>(ra-vector, sz: frame-count));
end corba-method;

define corba-method Rtmgr/RemoteNub/all-frame-lexicals
    (rnub :: <RemoteNub-implementation>, frame :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, ip :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (first :: Rtmgr/RemoteNub/<NUB-INDEX>, last :: Rtmgr/RemoteNub/<NUB-INDEX>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>)
  let process = remote-process(rnub);
  nub-all-frame-lexicals
  (process, frame, ip);
end corba-method;

define corba-method Rtmgr/RemoteNub/register-interactive-code-segment
    (rnub :: <RemoteNub-implementation>, lo :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, hi :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => ()
  let process = remote-process(rnub);
  nub-register-interactive-code-segment
  (process, lo, hi);
end corba-method;

define corba-method Rtmgr/RemoteNub/get-lexical-variable-name
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>, variable :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: CORBA/<string>)
  let process = remote-process(rnub);
  let name-length :: <integer> =
    nub-get-lexical-variable-name-length 
    (process, table, variable);
  let variable-name = make (<byte-string>, size: name-length);
  nub-get-lexical-variable-name
  (process, table, variable, name-length, variable-name);
  variable-name
end corba-method;

define corba-method Rtmgr/RemoteNub/lexical-variable-address
    (rnub :: <RemoteNub-implementation>, fp :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>, variable :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, in-reg :: Rtmgr/RemoteNub/<NUBINT>, hireg :: Rtmgr/RemoteNub/<NUB-INDEX>, loreg :: Rtmgr/RemoteNub/<NUB-INDEX>, arg :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-lexical-variable-address
    (process, fp, table, variable);
end corba-method;

define corba-method Rtmgr/RemoteNub/lookup-symbol-name
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>, sym :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: CORBA/<string>)
  let process = remote-process(rnub);       
  let name-length =
    nub-lookup-symbol-name-length(process, table, sym);
  let name = make(<byte-string>, size: name-length); 
  nub-lookup-symbol-name
  (process, table, sym, name-length, name);
  name
end corba-method;

define corba-method Rtmgr/RemoteNub/lookup-symbol-address
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>, sym :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-lookup-symbol-address
  (process, table, sym);
end corba-method;

define corba-method Rtmgr/RemoteNub/lookup-function-debug-start
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>, sym :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-lookup-function-debug-start
  (process, table, sym);
end corba-method;

define corba-method Rtmgr/RemoteNub/lookup-function-debug-end
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>, sym :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-lookup-function-debug-end
  (process, table, sym);
end corba-method;

/*
define corba-method Rtmgr/RemoteNub/lookup-symbol-language
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>, sym :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-lookup-symbol-language
  (process, table, sym);
end corba-method;
*/

define corba-method Rtmgr/RemoteNub/lookup-function-end
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>, sym :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-lookup-function-end
  (process, table, sym);
end corba-method;

define corba-method Rtmgr/RemoteNub/symbol-is-function
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>, sym :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-symbol-is-function
  (process, table, sym);
end corba-method;

define corba-method Rtmgr/RemoteNub/nearest-symbols
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>, lib :: Rtmgr/RemoteNub/<RNUBLIBRARY>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>)
  let process = remote-process(rnub);
  nub-nearest-symbols
    (process, address);
end corba-method;

define corba-method Rtmgr/RemoteNub/closest-symbol
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>, lib :: Rtmgr/RemoteNub/<RNUBLIBRARY>, actual-address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, offset :: Rtmgr/RemoteNub/<NUBINT>, name-length :: Rtmgr/RemoteNub/<NUBINT>, type :: Rtmgr/RemoteNub/<NUBINT>, is-function :: Rtmgr/RemoteNub/<NUBINT>, debug-start :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, debug-end :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, language :: Rtmgr/RemoteNub/<NUBINT>, final-address-of-definition :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-closest-symbol
    (process, address);
end corba-method;

define corba-method Rtmgr/RemoteNub/function-bounding-addresses
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (lower :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, upper :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-function-bounding-addresses
    (process, address);
end corba-method;

define corba-method Rtmgr/RemoteNub/closest-symbol-name
    (rnub :: <RemoteNub-implementation>, sz :: Rtmgr/RemoteNub/<NUBINT>)
 => (result :: CORBA/<string>)
  let process = remote-process(rnub);
  let sym-name = make(<byte-string>, size: sz);
  nub-closest-symbol-name
  (process, sz, sym-name);
  sym-name
end corba-method;

define corba-method Rtmgr/RemoteNub/find-symbol-in-library
    (rnub :: <RemoteNub-implementation>, nublibrary :: Rtmgr/RemoteNub/<RNUBLIBRARY>, sz :: Rtmgr/RemoteNub/<NUBINT>, name :: CORBA/<string>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, type :: Rtmgr/RemoteNub/<NUBINT>, is-function :: Rtmgr/RemoteNub/<NUBINT>, debug-start :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, debug-end :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, symbol-language :: Rtmgr/RemoteNub/<NUBINT>, final-address-of-definition :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-find-symbol-in-library
    (process, nublibrary, sz, name);
end corba-method;

define corba-method Rtmgr/RemoteNub/dispose-lookups
    (rnub :: <RemoteNub-implementation>, lookups :: Rtmgr/RemoteNub/<RNUBHANDLE>)
 => ()
  let process = remote-process(rnub);
  nub-dispose-lookups
  (process, lookups);
end corba-method;

define corba-method Rtmgr/RemoteNub/resolve-source-location
    (rnub :: <RemoteNub-implementation>, nublibrary :: Rtmgr/RemoteNub/<RNUBLIBRARY>, filename :: CORBA/<string>, line-number :: Rtmgr/RemoteNub/<NUBINT>, column-number :: Rtmgr/RemoteNub/<NUBINT>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, valid :: Rtmgr/RemoteNub/<NUBINT>, path :: Rtmgr/RemoteNub/<NUBINT>, search :: Rtmgr/RemoteNub/<RNUBHANDLE>, exact :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-resolve-source-location
  (process, nublibrary, filename, line-number, column-number);
end corba-method;

define corba-method Rtmgr/RemoteNub/fetch-source-locations
    (rnub :: <RemoteNub-implementation>, start-loc :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, end-loc :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<RNUBHANDLE>)
  let process = remote-process(rnub);
  nub-fetch-source-locations
  (process, start-loc, end-loc);
end corba-method;

define corba-method Rtmgr/RemoteNub/source-location-address
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>, index :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-source-location-address
  (process, table, index);
end corba-method;

define corba-method Rtmgr/RemoteNub/source-location-linenumber
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>, index :: Rtmgr/RemoteNub/<NUB-INDEX>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-source-location-linenumber
  (process, table, index);
end corba-method;

define corba-method Rtmgr/RemoteNub/source-location-filename
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>)
 => (result :: CORBA/<string>)
  let process = remote-process(rnub);
  let fname-length :: <integer> =
    nub-source-location-filename-length(process, table);
  let fname = make(<byte-string>, size: fname-length);
  nub-source-location-filename
  (process, table, fname-length, fname);
  fname
end corba-method;

define corba-method Rtmgr/RemoteNub/number-of-source-locations
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-number-of-source-locations
  (process, table);
end corba-method;

define corba-method Rtmgr/RemoteNub/dispose-source-locations
    (rnub :: <RemoteNub-implementation>, table :: Rtmgr/RemoteNub/<RNUBHANDLE>)
 => ()
  let process = remote-process(rnub);
  nub-dispose-source-locations
  (process, table);
end corba-method;

define corba-method Rtmgr/RemoteNub/interpret-instruction-at-current-location
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => (flow :: Rtmgr/RemoteNub/<NUBINT>, destination :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, instruction-size :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-interpret-instruction-at-current-location
    (process, nubthread);
end corba-method;

define corba-method Rtmgr/RemoteNub/dylan-calculate-step-into
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, function-register-live :: Rtmgr/RemoteNub/<NUBINT>, ok :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-dylan-calculate-step-into
    (process, nubthread);
end corba-method;

define corba-method Rtmgr/RemoteNub/dylan-thread-environment-block-address
    (rnub :: <RemoteNub-implementation>, thread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, valid :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-dylan-thread-environment-block-address
    (process, thread);
end corba-method;

define corba-method Rtmgr/RemoteNub/dylan-thread-mv-buffer-live
    (rnub :: <RemoteNub-implementation>, thread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-dylan-thread-mv-buffer-live
  (process, thread);
end corba-method;

define corba-method Rtmgr/RemoteNub/older-stack-frame
    (rnub :: <RemoteNub-implementation>, this-one :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, than-this-one :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-older-stack-frame
  (process, this-one, than-this-one);
end corba-method;

define corba-method Rtmgr/RemoteNub/dylan-current-function
    (rnub :: <RemoteNub-implementation>, nubthread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => (result :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
  let process = remote-process(rnub);
  nub-dylan-current-function
  (process, nubthread);
end corba-method;

define corba-method Rtmgr/RemoteNub/perform-absolute-relocation
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, destination :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-perform-absolute-relocation
  (process, address, destination);
end corba-method;

define corba-method Rtmgr/RemoteNub/perform-relative-relocation
    (rnub :: <RemoteNub-implementation>, address :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>, destination :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-perform-relative-relocation
  (process, address, destination);
end corba-method;

define corba-method Rtmgr/RemoteNub/recover-breakpoint
    (rnub :: <RemoteNub-implementation>, thread :: Rtmgr/RemoteNub/<RNUBTHREAD>)
 => ()
  let process = remote-process(rnub);
  nub-recover-breakpoint
  (process, thread);
end corba-method;

define corba-method Rtmgr/RemoteNub/get-process-wall-clock-time
    (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<NUBINT>)
  let process = remote-process(rnub);
  nub-get-process-wall-clock-time
  (process);
end corba-method;

define corba-method Rtmgr/RemoteNub/register-exit-process-function
    (rnub :: <RemoteNub-implementation>, ExitProcess :: Rtmgr/RemoteNub/<RTARGET-ADDRESS>)
 => ()
  let process = remote-process(rnub);
  nub-register-exit-process-function
    (process, ExitProcess);
end corba-method;

define corba-method Rtmgr/RemoteNub/open-local-tether
    (rnub :: <RemoteNub-implementation>, command :: CORBA/<string>, arguments :: CORBA/<string>, paths :: Rtmgr/RemoteNub/<STRING-SEQ>, lib-paths :: Rtmgr/RemoteNub/<STRING-SEQ>, working-directory :: CORBA/<string>, create-shell :: Rtmgr/RemoteNub/<NUBINT>)
 => (result :: Rtmgr/RemoteNub/<RNUB>, success :: Rtmgr/RemoteNub/<NUBINT>)
  let symfile-c-strings = map(curry(as, <C-string>), paths);
  let symfile-dir-array = make(<C-string*>, 
                               element-count: symfile-c-strings.size);
  for (i :: <integer> from 0 below symfile-c-strings.size)
    symfile-dir-array[i] := symfile-c-strings[i];
  end for;

  let lsp-c-strings = map(curry(as, <C-string>), lib-paths);
  let lsp-dir-array = make(<C-string*>, 
                           element-count: lsp-c-strings.size);
  for (i :: <integer> from 0 below lsp-c-strings.size)
    lsp-dir-array[i] := lsp-c-strings[i];
  end for;

  let (proc, success)
    = open-local-tether(command, 
                        arguments, 
                        paths.size,
                        symfile-dir-array,
                        lib-paths.size,
                        lsp-dir-array,
                        working-directory,
                        create-shell);

  rnub.process := proc;

  destroy(symfile-dir-array);
  do(destroy, symfile-c-strings);
  destroy(lsp-dir-array);
  do(destroy, lsp-c-strings);

  values(proc, success)

end corba-method;

define corba-method Rtmgr/RemoteNub/attach-local-tether
    (rnub :: <RemoteNub-implementation>, process :: Rtmgr/RemoteNub/<RNUBPROCESS>,
     process-name :: CORBA/<string>, process-system-id :: CORBA/<string>, process-actual-id :: Rtmgr/RemoteNub/<RNUB>,
     symbol-paths :: Rtmgr/RemoteNub/<STRING-SEQ>, system-JIT-information :: CORBA/<string>)
 => (result :: Rtmgr/RemoteNub/<RNUB>, success :: Rtmgr/RemoteNub/<NUBINT>)

  let symfile-c-strings = map(curry(as, <C-string>), symbol-paths);
  let symfile-dir-array = make(<C-string*>, 
                               element-count: symfile-c-strings.size);
  for (i :: <integer> from 0 below symfile-c-strings.size)
    symfile-dir-array[i] := symfile-c-strings[i];
  end for;

  let (proc, success)
    = attach-remote-tether(process,
			   process-name.size,
			   process-name,
			   process-system-id.size,
			   process-system-id,
			   process-actual-id,
			   symbol-paths.size,
			   symfile-dir-array,
			   system-JIT-information);

  rnub.process := proc;

  destroy(symfile-dir-array);
  do(destroy, symfile-c-strings);

  values(proc, success)

end corba-method;
