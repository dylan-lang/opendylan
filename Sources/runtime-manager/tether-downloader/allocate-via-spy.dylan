module:     tether-downloader-internals
synopsis:   The allocation of static blocks using access-path spy code.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// GET-THREAD-FOR-SPY
//    Given an access-path, get a <remote-thread> object (any one) to run
//    the spy function for us.

define method get-thread-for-spy
    (access-path :: <access-path>) => (thr :: <remote-thread>)
  let gotit = #f;
  local method just-grab-the-first-one (t :: <remote-thread>)
          unless (gotit)
            gotit := t;
          end unless;
        end method;
  do-threads(just-grab-the-first-one, access-path);
  gotit;
end method;


///// ALLOCATE-SINGLE-STATIC-BLOCK
//    Builds a <static-block>, and runs the necessary spy code to allocate
//    the memory.

define method allocate-single-static-block
    (access-path :: <access-path>, malloc-entry-point :: <remote-value>,
     #key byte-granularity = #f, page-granularity = #f,
          thread-for-spy = #f)
       => (b :: false-or(<static-block>))
  let returned-block = #f;
  let spy-thread = thread-for-spy | get-thread-for-spy(access-path);

  // Find the size in bytes of a remote virtual memory page.

  let 
    page-size = 
      remote-virtual-page-size(access-path) * 
                         remote-value-byte-size(access-path);

  // By default, a block of the platform virtual page size will be
  // allocated, but clients can request blocks of specific byte sizes,
  // or of a multiple of the page size. 

  let request-size =
    if (byte-granularity)
      byte-granularity
    elseif (page-granularity)
      page-granularity * page-size
    else
      page-size
    end if;

  // Call the remote allocation routine to get the block. We assume that
  // a return value of zero indicates failure. We also assume that the
  // remote allocator adopts C calling convention, and takes a single
  // raw integer argument giving the byte-size of the request.

  let (malloc-result, aborted?) =
    remote-call-spy(access-path, 
                    spy-thread, 
                    malloc-entry-point,
                    as-remote-value(request-size));

  unless (aborted? | (malloc-result = as-remote-value(0)))
    returned-block := make(<static-block>,
                           access-path: access-path,
                           size: page-size,
                           remaining-size: request-size,
                           base-address: malloc-result);
    block-align-4(returned-block);
  end unless;
  returned-block;
end method;


///// ALLOCATE-MULTIPLE-STATIC-BLOCKS
//    Given a target number of blocks to allocate, tries to allocate as near
//    to that number as possible, and return the blocks in a sequence.

define method allocate-multiple-static-blocks
    (access-path :: <access-path>, n :: <integer>,
     malloc-entry-point :: <remote-value>) 
       => (bs :: <sequence>)
  let blocks-so-far = make(<stretchy-vector>, size: 0);
  for (i from 0 below n)
     let this-block = 
       allocate-single-static-block(access-path, malloc-entry-point);
     if (this-block)
       add!(blocks-so-far, this-block);
     end if
  end for;
  blocks-so-far;
end method;
