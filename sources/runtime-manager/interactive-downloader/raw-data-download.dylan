module:       interactive-downloader-internals
synopsis:     Manages allocation of interactive memory in debug targets
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

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
