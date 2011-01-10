module:       interactive-downloader-internals
synopsis:     Actually spewing byte vectors into the interactive memory.
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DOWNLOAD-SECTIONS-INTO-REGION
//    Allocates _contiguous_ memory blocks for a number of requests, each
//    of which may potentially require some alignment padding. Calculates
//    the (aligned) addresses and returns them.
//    For convenience, also returns the lowest and highest address occupied
//    as a result of the entire operation. This is useful when registering
//    the memory with the garbage collector.

define method download-sections-into-region
    (trans :: <interactive-transaction>, region :: <interactive-region>,
     sections :: <sequence>)
       => (base-addresses :: <sequence>,
           lowest-address :: <remote-value>,
           highest-address :: <remote-value>)

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

  let application = 
    trans.transaction-downloader-target.interactive-application;
  let path = application.debug-target-access-path;
  let number-of-sections = size(sections);
  let last-section = number-of-sections - 1;
  let section-sizes = make(<vector>, size: number-of-sections, fill: 0);
  let section-alignments = make(<vector>, size: number-of-sections, fill: 0);
  let section-vectors = make(<vector>, size: number-of-sections);

  for (i from 0 to last-section)
    let sname = sections[i].section-name.string-data;
    section-sizes[i] := sections[i].raw-data-size;
    section-vectors[i] := sections[i].section-data;
    section-alignments[i] := 
       if (section-names-equivalent?(sname, ".dyfix"))
         1
       else
         sections[i].section-alignment;
       end if;
  end for;

  let sum-of-section-sizes = reduce(\+, 0, section-sizes);
  let sum-of-alignments = reduce(\+, 0, section-alignments);
  let base-addresses = make(<vector>, 
                            size: number-of-sections, 
                            fill: $illegal-address);
  let lowest-address = $illegal-address;
  let highest-address = $illegal-address;

  // The size for the block is the sum of the section sizes, plus the
  // largest potential number of bytes needed for alignment padding.

  let conservative-block-size = 
    sum-of-section-sizes + sum-of-alignments;

  // If the interactive region is being used for the first time, then we
  // need to initialize it.

  unless (region.region-searched-for-allocator?)
    initialize-interactive-region(application, region);
  end unless;

  // Find a block large enough to hold the request.

  let target-block =
    find-block-in-region(trans, region, conservative-block-size,
                         library: trans.transaction-library);

  if (target-block)
    for (section-i from 0 to last-section)
      block-align-n(target-block, section-alignments[section-i]);
      base-addresses[section-i] :=
        download-byte-vector-into
           (application.debug-target-access-path,
            target-block,
            section-vectors[section-i],
            from-index: 0,
            to-index: section-sizes[section-i] - 1);

      // Some special case code for when this is the very first, or the very
      // last section.

      if (section-i == 0)
        lowest-address := base-addresses[section-i]
      end if;
      if (section-i == last-section)
        highest-address :=
          byte-indexed-remote-value(base-addresses[section-i],
                                    section-sizes[section-i]);
      end if;
    end for;
  end if;

  // Return the sequence of addresses.
  values(base-addresses,
         lowest-address,
         highest-address);
end method;

/*
///// DOWNLOAD-INTO-REGION (Obsolete)
//    Allocates enough memory to hold the data in a byte vector, downloads
//    the vector, and returns an address.

define method download-into-region
    (application :: <debug-target>, region :: <interactive-region>,
     bytes :: <byte-vector>, 
        #key alignment = 1,
             from-index = 0,
             to-index = #f)
       => (base-address :: <remote-value>)

  let object-address = #f;

  // The size we need for the block is the size of the byte vector, even
  // if we actually only want to download a segment of it. Also add the
  // requested alignment.

  let conservative-block-size = size(bytes) + alignment;

  // If the interactive region is being used for the first time, then
  // initialize it.

  unless (region.region-searched-for-allocator?)
    initialize-interactive-region(application, region)
  end unless;

  // Find a block large enough to hold the request.

  let target-block = 
    find-block-in-region(trans, region, conservative-block-size);

  // If we found one, align it correctly, and download the bytes.

  if (target-block)
    block-align-n(target-block, alignment);
    object-address := download-byte-vector-into
                         (application.debug-target-access-path,
                          target-block,
                          bytes,
                          from-index: from-index,
                          to-index: to-index);
  end if;

  // Return the address.
  if (object-address)
    object-address
  else
    $illegal-address
  end if
end method;
*/

///// INITIALIZE-INTERACTIVE-REGION
//    If an interactive region is being used for the first time, it is
//    necessary to search for its remote allocator routine.

define method initialize-interactive-region
    (application :: <debug-target>, region :: <interactive-region>) => ()
  let allocator-sym = 
    find-symbol(application.debug-target-access-path,
                region.region-allocator-name,
                library: application.application-dylan-runtime-library);
  region.region-searched-for-allocator? := #t;
  if (allocator-sym)
    region.region-allocator-entry-point := allocator-sym.remote-symbol-address;
  else
    region.region-allocator-entry-point := #f;
  end if;
end method;


///// FIND-BLOCK-IN-REGION
//    Finds (or allocates) a static block with sufficient room to hold
//    an object of a stipulated byte size.
//    The byte-size is rounded up to a virtual memory page: the downloader
//    never allocates blocks of less than a page.
//    The second return value is a flag indicating whether the returned
//    block was actually allocated during the call to this function.
//    (Some clients have to do "extra things" with brand new blocks).

define method find-block-in-region
    (trans :: <interactive-transaction>, region :: <interactive-region>,
     request-size :: <integer>,
     #key library = #f)
       => (request-block :: false-or(<static-block>), fresh? :: <boolean>)

  let request-block = #f;
  let fresh? = #f;
  let application = 
    trans.transaction-downloader-target.interactive-application;
  let thread = trans.transaction-thread;

  local method round-up-to-page
                 (byte-count :: <integer>) => (num-pages :: <integer>)
    let path = application.debug-target-access-path;
    let page-byte-size = 
          remote-virtual-page-size(path) * remote-value-byte-size(path);
    let (pages, remainder) = truncate/(byte-count, page-byte-size);
    pages + 1;
  end method;

  block (exit)

    // If there is no allocator routine, we're screwed. We have no blocks,
    // and can't allocate any either.

    unless(region.region-allocator-entry-point)
      exit()
    end unless;

    // If there is no current static block, allocate it now, and make
    // it of sufficient size to hold the request. (This branch should be
    // taken for the very first downloading operation into this region).

    unless(region.region-current-static-block)
      request-block :=
        allocate-single-static-block
           (application.debug-target-access-path,
            region.region-allocator-entry-point,
            page-granularity: round-up-to-page(request-size),
            thread-for-spy: thread);
      region.region-current-static-block := request-block;

      // Register this block as an interactive extension of a particular
      // <remote-library>.

      if (request-block & library)
        symbol-table-register-region-for-library
           (application.debug-target-symbol-table,
            library,
            request-block.static-block-base-address,
            indexed-remote-value
               (request-block.static-block-base-address, 
                request-block.static-block-size - 1),
            region.region-classification);
        fresh? := #t;
      end if;

      exit();
    end unless;

    // If there is room for the request in the current static block, then
    // use it.

    let avail-size = 
      region.region-current-static-block.static-block-remaining-size;

    if (avail-size >= request-size)
      request-block := region.region-current-static-block;
      exit();
    end if;

    // Otherwise, search any blocks that have been previously allocated.
    // If we find one that still has enough room in it, then use it.

    for (existing-block in region.region-allocated-static-blocks)
      if (existing-block.static-block-remaining-size >= request-size)
        request-block := existing-block;
        exit();
      end if
    end for;

    // If we still have nothing, then discard the current static block
    // onto the list of allocated blocks, and allocate a new block of
    // sufficient size to hold the request, making that new block the
    // current block.

    request-block :=
      allocate-single-static-block
        (application.debug-target-access-path,
         region.region-allocator-entry-point,
         byte-granularity: request-size,
         thread-for-spy: thread);

    if (request-block)
      add!(region.region-allocated-static-blocks, 
           region.region-current-static-block);
      region.region-current-static-block := request-block;

      // Register this block as an interactive extension of the appropriate
      // <remote-library> if necessary.

      if (library)
        symbol-table-register-region-for-library
           (application.debug-target-symbol-table,
            library,
            request-block.static-block-base-address,
            indexed-remote-value
               (request-block.static-block-base-address, 
                request-block.static-block-size - 1),
            region.region-classification);
        fresh? := #t;
      end if;

    end if;

  end block;

  // Return whatever we found.
  values(request-block, fresh?);  
end method;


///// CLASSIFY-COFF-SECTION
//    Returns a symbol that abstractly describes a coff section based
//    upon the section's name.

define method classify-coff-section (coff-section :: <coff-section>)
    => (classification :: <symbol>)

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

  let name = coff-section.section-name.string-data;

  if (section-names-equivalent?(name, ".text"))
    #"compiled-code"
  elseif (section-names-equivalent?(name, ".data"))
    #"data"
  elseif (section-names-equivalent?(name, ".dyvar"))
    #"dylan-exact"
  elseif (section-names-equivalent?(name, ".dyobj"))
    #"dylan-static"
  elseif (section-names-equivalent?(name, ".dydat"))
    #"dylan-ambiguous"
  elseif (section-names-equivalent?(name, ".dyfix"))
    #"dylan-fixup"
  elseif (section-names-equivalent?(name, ".dyimp"))
    #"dylan-import"
  elseif (section-names-equivalent?(name, ".dyutr"))
    #"dylan-untraced"
  elseif (section-names-equivalent?(name, ".file"))
    #"discard"
  elseif (section-names-equivalent?(name, ".debug"))
    #"discard"
  elseif (section-names-equivalent?(name, ".drectve"))
    #"discard"
  else
    #"misc"
  end if

end method;


///// SELECT-REGION-FOR-SECTION
//    Note 1: This is guaranteed to return a region, even if the section is
//            classified as #"discard". It is upto something else to decide
//            whether or not to actually download the raw data.

define method select-region-for-section
    (trans :: <interactive-transaction>, classification :: <symbol>)
       => (region :: <interactive-region>)
  let dt = trans.transaction-downloader-target;
  let region =
    select (classification)
      #"compiled-code" => dt.compiled-code;
      #"data" => dt.foreign-initialized-data;
      #"dylan-exact" => dt.dylan-variables;
      #"dylan-static" => dt.dylan-static-objects;
      #"dylan-ambiguous" => dt.dylan-ambiguous-data;
      #"dylan-fixup" => dt.dylan-fixups;
      #"dylan-import" => dt.dylan-imports;
      #"dylan-untraced" => dt.dylan-untraced-data;
      #"misc" => dt.miscellaneous-section;
      #"discard" => dt.miscellaneous-section;
    end select;
  values(region, classification);
end method;

