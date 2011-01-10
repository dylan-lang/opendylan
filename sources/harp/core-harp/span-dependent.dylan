module:    main-harp
Synopsis:  Support for span dependent instructions (SDIs)
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Support for span dependent instructions (SDIs)

//// Resolve-sdis does what you would think - it also returns the total
//// increase in the code size during extension of the sdis

define method calculate-instruction (sdi :: <new-sdi>)
  block (return)
    let vec :: <simple-object-vector> = sdi.new-sdi-method-vector;
    for (i from sdi.new-sdi-method-index)
      let fun = vec[i];
      let size = fun(sdi, sdi.new-sdi-cached-span, #f);
      if (size)
        sdi.new-sdi-method-index := i;
        return(size);
      end if;
    end for;
  end block;
end;
             



//// New version for resolve-sdis, doint it without allocation. 
//// first pass just calculate the size of each sdi, and sets the diff
//// if it is not the same as the original size. 
//// second pass check for each each sdi if any of the sids in its span
//// have increased in size, increase the sdi span accordingly, 
//// check again its size and flags it as not checked.
//// third pass loop repeatedly loop thrugh the sdis, check any
//// unchecked sdis, and if they change their size increase the span of
//// all other sdis which contain it in their span.
//// The repeated loop hopefully don't actually ever do anything,
//// because the first two passes already did everything needed.
//// maybe a good idea to keep the sdis need checking during the
//// repeated loop in a chain, but I couldn't be bothered.
//// Y 21/4/93

 
define method resolve-sdis (backend :: <harp-back-end>)
  // first lets give each sdi an initial span
  let sdis-vec :: <stretchy-vector> = backend.variables.all-the-sdis;
  let sdis-num = size(sdis-vec);
  let increase-in-code-size :: <integer> = 0;
  let changed = #f;

  //// first pass: calculate each sdi.
  loop-sdis (sdi at index below sdis-num in sdis-vec)
    let dest-block = find-bb(backend, sdi.new-sdi-dest-tag);
    if (dest-block)
      let dest-place = dest-block.bb-fixed-offset + 
                       sdi.new-sdi-dest-offset;
      let this-place = sdi.new-sdi-fixed-offset;
      let span = dest-place - this-place;
      sdi.new-sdi-cached-span := span;
      sdi.new-sdi-checked := #t;
      let new-size = calculate-instruction(sdi);
      if (new-size == sdi.new-sdi-cached-size)
	sdi.new-sdi-diff := #f;
      else
	let diff = new-size - sdi.new-sdi-cached-size;
	changed := #t;
	inc!(increase-in-code-size, diff);
	sdi.new-sdi-diff := diff;
	sdi.new-sdi-cached-size := new-size;
      end if;
    else
	harp-error("Missing block for tag %=, in resolve-sdis",
	           sdi.new-sdi-dest-tag);
    end if;
  end loop-sdis;

  if (changed)
    for (sdi-index from 0 below sdis-num)
      let sdi :: <new-sdi> = sdis-vec[sdi-index];
      let sdis-before-this :: <integer> = sdi.new-sdi-preceding-sdis;
      let dest-block :: <basic-block> = sdi.new-sdi-dest-tag.tag-bb;
      let sdis-before-dest :: <integer> = dest-block.bb-preceding-sdis;
      if (sdis-before-dest < sdis-before-this)
	loop-sdis (p-sdi at p-index 
                   from sdis-before-dest below sdis-before-this 
                   in sdis-vec) 
          let diff = p-sdi.new-sdi-diff;
          sdi.new-sdi-checked := #f;
          if (diff) dec!(sdi.new-sdi-cached-span, diff) end if;
        end loop-sdis;
      else
	loop-sdis (p-sdi at p-index 
                   from sdis-before-this below sdis-before-dest 
                   in sdis-vec)
          let diff = p-sdi.new-sdi-diff;
          sdi.new-sdi-checked := #f;
          if (diff) inc!(sdi.new-sdi-cached-span, diff) end if;
        end loop-sdis;
      end if;
    end for;
  end if;

  while (changed)
    changed := #f;
    loop-sdis (sdi at index below sdis-num in sdis-vec)
      unless (sdi.new-sdi-checked)
        let new-size = calculate-instruction(sdi);
	let cached-size = sdi.new-sdi-cached-size;
	sdi.new-sdi-checked := #t;
	unless (cached-size == new-size)
	  let diff = new-size - cached-size;
          let span = sdi.new-sdi-cached-span;
          if (span > 0)
            sdi.new-sdi-cached-span := span + diff;
            let new-new-size = calculate-instruction(sdi);
            unless (new-new-size == new-size)
	      diff := new-new-size - cached-size;
	      new-size := new-new-size;
            end unless;
          end if;

          sdi.new-sdi-cached-size := new-size;
          inc!(increase-in-code-size, diff);

          loop-sdis (p-sdi at p-index from 0 below index in sdis-vec)
            let dest-block = p-sdi.new-sdi-dest-tag.tag-bb;
	    let sdis-before-dest = dest-block.bb-preceding-sdis;
            if (sdis-before-dest > index)
	      changed := #t;
	      inc!(p-sdi.new-sdi-cached-span, diff);
              p-sdi.new-sdi-checked := #f;
            end if;
          end loop-sdis;

          let n-index = index + 1;
          loop-sdis (p-sdi at p-index from n-index below sdis-num 
                     in sdis-vec)
            let dest-block = p-sdi.new-sdi-dest-tag.tag-bb;
            let sdis-before-dest = dest-block.bb-preceding-sdis;
            if (sdis-before-dest <= index)
	      dec!(p-sdi.new-sdi-cached-span, diff);
	      p-sdi.new-sdi-checked := #f;
            end if;
          end loop-sdis;
        end unless;
      end unless;
    end loop-sdis;
  end while;

  loop-sdis (sdi at index below sdis-num in sdis-vec)
    let fun = sdi.new-sdi-method-vector[sdi.new-sdi-method-index];
    sdi.new-sdi-code-holder := fun(sdi, sdi.new-sdi-cached-span, #t);
    // yes we really want the code this time
  end loop-sdis;
  increase-in-code-size;
end;





