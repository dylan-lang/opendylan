module:       interactive-downloader-internals
synopsis:     Manages allocation of interactive memory in debug targets
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
///// DOWNLOAD-COFF-FILE-RAW-DATA
//    This is responsible for actually injecting the byte-vectors of
//    raw data from the COFF file into the runtime.
//    Iterates through each concrete <coff-section> in the
//    <coff-file>. By its section name, decides which interactive region
//    should be used. Downloads the raw data into the correct region,
//    and remembers the address.

define method download-coff-file-raw-data
    (target :: <downloader-target>, coff-file :: <coff-file>) => ()
  for (this-section in coff-file.sections.ordered-data)
    download-coff-section-raw-data(target, this-section)
  end for
end method;


///// DOWNLOAD-COFF-SECTION-RAW-DATA
//    Downloads a single section.

define method download-coff-section-raw-data
    (trans :: <interactive-transaction>, coff-section :: <coff-section>)
       => ()

  // SECTION-NAMES-EQUIVALENT?
  // While the actual names of coff-file sections might contain "$" extensions
  // (such as ".dyobj$M" or ".debug$T"), we are not interested in these.
  // This utility allows us to compare section names regardless of such
  // extensions.

  local method section-names-equivalent?
                  (actual-name :: <byte-string>, basic-name :: <byte-string>)
                     => (answer :: <boolean>)
     let actual-size = size(actual-name);
     let basic-size = size(basic-name);
     if (actual-size < basic-size)
       #f
     elseif (actual-size == basic-size)
       actual-name = basic-name
     else
       let answer = #t;
       block (exit)
         for (i from 0 below basic-size)
           unless (as-lowercase(actual-name[i]) = as-lowercase(basic-name[i]))
             answer := #f;
             exit();
           end unless
         end for;
       end block;
       answer;
     end if
  end method;

  let target = trans.transaction-downloader-target;
  let name = coff-section.section-name.string-data;
  let region = #f;

  // Select the correct interactive region, based upon the name of the
  // section.

//format-out("Attempting to download a %s section\n", name);

  if (section-names-equivalent?(name, ".text"))
    region := target.dylan-compiled-code;
//  format-out("Compiled code section\n");
  elseif (section-names-equivalent?(name, ".dyvar"))
    region := target.dylan-variables;
//  format-out("Dylan variable section\n");
  elseif (section-names-equivalent?(name, ".dyobj"))
    region := target.dylan-static-objects;
//  format-out("Dylan static object section\n");
  elseif (section-names-equivalent?(name, ".dydat"))
    region := target.dylan-ambiguous-data;
//  format-out("Dylan ambiguous data section\n");
  elseif (section-names-equivalent?(name, ".data"))
    region := target.raw-initialized-data;
//  format-out("Raw data section\n");
  end if;

  // If 'region' is still #f, this is not a section we are interested in,
  // so everything else is conditional on that.

  if (region)
    let sz = coff-section.raw-data-size;

    // Download the raw-data <byte-vector>. Note that the stored
    // <byte-vector> might be larger than is necessary to hold all of the
    // raw data. Hence, we tell the downloader the delimiting indices into
    // the byte vector.

//  format-out("Attempting the raw-data download: ");
    let section-base-address =
      download-into-region(target.interactive-application,
                           region,
                           coff-section.section-data,
                           alignment: coff-section.section-alignment,
                           from-index: 0,
                           to-index: sz);

    // The final address is the base address, offset by the size of the
    // section's raw data.

    let section-final-address =
      byte-indexed-remote-value(section-base-address, sz);

    // For this <coff-section>, record the base address that it was
    // downloaded to, and the final address. These will be needed later
    // in order to calculate the addresses of new symbol definitions,
    // and also to register the section with the memory manager.

    target.coff-section-runtime-address-bounds[coff-section] := 
         pair(section-base-address, section-final-address);
  end if;
end method;

*/

///// DOWNLOAD-ALL-RAW-DATA
//    The top-level function to download the raw data.

define method download-all-raw-data (trans :: <interactive-transaction>) => ()
  for (section-class in #[#"compiled-code", 
                          #"data", 

			  // imports _must_ precede fixups
                          #"dylan-import",
                          #"dylan-fixup", 

                          #"dylan-exact",
                          #"dylan-static",
                          #"dylan-ambiguous",
                          #"dylan-untraced",
                          #"misc"])

    block (exit)
      // Locate an <interactive-region> that will be used to download the raw
      // data of all sections that fall into the current section-class
      // category.

      let destination-region = select-region-for-section(trans, section-class);

      // Now, across all COFF files, gather together the set of sections whose
      // raw data will be downloaded into this section. We don't known how
      // many there are, so we need a stretchy vector of <coff-section>s.

      let selected-sections = make(<stretchy-vector>, size: 0);

      for (coff-file in trans.transaction-coff-file-sequence)
        for (coff-section in coff-file.sections.ordered-data)
          if (trans.transaction-section-types[coff-section] == section-class)
            add!(selected-sections, coff-section)
          end if
        end for
      end for;

      // If we didn't select any sections, let's forget it.

      if (selected-sections.size == 0)
        exit()
      end if;

      // Now call the raw-data downloader.
      // This will cause all of the data in the selected sections to get
      // downloaded into contiguous regions of memory. This results in just
      // a single (at most) GC registration.

      let (base-addresses, lowest-address, highest-address)
        = download-sections-into-region
            (trans,
             select-region-for-section(trans, section-class),
             selected-sections);

      // Record the base address of each section.

      for (i from 0 below size(selected-sections))
        trans.transaction-section-addresses[selected-sections[i]]
           := base-addresses[i];
      end for;

      // Record the deferred GC registration if necessary.

      let deferred-registration =
        if (section-class == #"dylan-exact")
          make(<memory-registration>,
               lower-bound: lowest-address, upper-bound: highest-address,
               style: $registration-style-exact)
        elseif (section-class == #"dylan-static")
          make(<memory-registration>,
               lower-bound: lowest-address, upper-bound: highest-address,
               style: $registration-style-static)
        elseif (section-class == #"dylan-ambiguous")
          make(<memory-registration>,
               lower-bound: lowest-address, upper-bound: highest-address,
               style: $registration-style-ambiguous)
        elseif (section-class == #"dylan-fixup")
          make(<memory-registration>,
               lower-bound: lowest-address, upper-bound: highest-address,
               style: $registration-style-fixup)
        elseif (section-class == #"dylan-import")
          make(<memory-registration>,
               lower-bound: lowest-address, upper-bound: highest-address,
               style: $registration-style-import)
        elseif (section-class == #"compiled-code")
          make(<memory-registration>,
               lower-bound: lowest-address, upper-bound: highest-address,
               style: $registration-style-code)
        else
          #f
        end if;

      if (deferred-registration)
        add!(trans.transaction-deferred-registrations, deferred-registration);
      end if;
    end block;
  end for;
end method;
