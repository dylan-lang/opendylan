Module: emulator-patches-2
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Dylan locators in registries.

// Patch the emulator to allow Dylan locators to be specified in emulator
// registry entries.

define lisp-interface 
  functions resolve-dylan-locator from dylan;
end lisp-interface;

define macro with-patching-dylan-host
  { with-patching-dylan-host (?namestring:expression) ?:body end }
  => { block ()
         add-abstract-host 
           ("abstract://dylan/", 
            list(list("abstract://dylan/**/*.*;*", 
                      concatenate(?namestring, "**/*.*")),
                 list("abstract://dylan/**/*;*", 
        	      concatenate(?namestring, "**/*"))));
	 ?body
       cleanup
	 // remove-abstract-host("abstract://dylan/");
       end block }
end macro;

define method resolve-dylan-locator (registry, line)
  with-patching-dylan-host (concatenate(registry, "/../../"))
    as(<string>, as(<physical-locator>, line));
  end;
end method;

// eof
