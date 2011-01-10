module:    walker
author:    gail zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <class-stats> (<object>)
  slot stat-count        = 0,  init-keyword: count:;
  slot stat-size         = 0,  init-keyword: size:;
  constant slot stat-parent-stats = #f, init-keyword: parent-stats:;
end class;

define method walker-merge-statistics
   (x, y, merge :: <function>) => (z :: singleton(#f))
  #f
end method;

define constant $empty-table = make(<table>);

define method walker-merge-statistics
   (x :: <class-stats>, y, merge :: <function>) => (z :: <class-stats>)
  make(<class-stats>,
       count:
	 merge(x.stat-count, 0),
       size:
	 merge(x.stat-size, 0),
       parent-stats:
         walker-merge-statistics(stat-parent-stats(x), $empty-table, merge))
end method;

define method walker-merge-statistics
    (x, y  :: <class-stats>, merge :: <function>) => (z :: <class-stats>)
  make(<class-stats>,
       count:
	 merge(0, y.stat-count),
       size:
	 merge(0, y.stat-size),
       parent-stats:
         walker-merge-statistics($empty-table, stat-parent-stats(y), merge))
end method;

define method walker-merge-statistics
    (x :: <class-stats>, y  :: <class-stats>, merge :: <function>) => (z :: false-or(<class-stats>))
  let merged-count = merge(x.stat-count, y.stat-count);
  let merged-size  = merge(x.stat-size,  y.stat-size);
  if (merged-count = 0 & merged-size = 0)
    #f
  else
    make(<class-stats>, 
	 count: 
	   merged-count,
	 size: 
	   merged-size,
	 parent-stats: 
	   walker-merge-statistics(stat-parent-stats(x), stat-parent-stats(y), merge))
  end if
end method;

define method walker-merge-statistics 
    (x :: <table>, y :: <table>, merge :: <function>) => (z :: <table>)
  let z = make(<table>);
  for (stat keyed-by class in x)
    let merged-stats
      = walker-merge-statistics(stat, element(y, class, default: #f), merge);
    when (merged-stats)
      element(z, class) := merged-stats;
    end when;
  end for; 
  for (stat keyed-by class in y)
    unless (element(x, class, default: #f))
      element(z, class) := stat;
    end unless;
  end for; 
  z
end method;

define variable *neg-one-statistics* :: <table> = $empty-table;
define variable *neg-two-statistics* :: <table> = $empty-table;

define function diff-statistics 
    (old-stats :: <table>, new-stats :: <table>)
  walker-merge-statistics(new-stats, old-stats, \-);
end function;

define function walker-diff-last-two-statistics 
    (debug-name :: <function>, required-instance-size :: <function>)
  walker-display-statistics
    (0, #t, debug-name, required-instance-size, 
     diff-statistics(*neg-two-statistics*, *neg-one-statistics*));
end function;

define function do-walker-instance-statistics
    (display? :: <boolean>, 
     real-object :: <function>, instance-class :: <function>, 
     debug-name :: <function>,
     instance-size :: <function>, required-instance-size :: <function>,
     parents-of :: <table>,
     categorize :: false-or(<function>),
     #key filter-set = #[], aggregate-set = #[])
  local method listify-parents (parents)
	  if (instance?(parents, <sequence>)) 
	    parents
	  else
	    list(parents)
	  end if;
        end method,
        method default-categorize
	    (object, parents, parents-of) => (class, parent-classes)
	  values(instance-class(object), 
		 map(instance-class, listify-parents(parents)))
	end method;
  let categorize :: <function>
    = categorize | default-categorize;
  let stats :: <object-table> = make(<table>);
  local method do-record-stat (class, parent-classes, size :: <integer>)
	  let class-stats 
	    = element(stats, class, default: #f) 
	        | (stats[class] 
		     := make(<class-stats>, parent-stats: make(<table>)));
	  class-stats.stat-count := class-stats.stat-count + 1;
	  class-stats.stat-size  := class-stats.stat-size + size;
	  let parent-stats
	    = stat-parent-stats(class-stats);
	  let parent-classes
	    = listify-parents(parent-classes);
	  for (parent-class in parent-classes)
            let parent-class-stats
              = element(parent-stats, parent-class, default: #f) 
	          | (parent-stats[parent-class] := make(<class-stats>));
     	    parent-class-stats.stat-size 
              := parent-class-stats.stat-size + size;
     	    parent-class-stats.stat-count 
              := parent-class-stats.stat-count + 1;
	  end for;
	end method,
        method record-stat (object, parents, size)
          let (canonical-class, canonical-parent-classes)
	    = categorize(object, parents, parents-of);
	  do-record-stat(canonical-class, canonical-parent-classes, size)
	end method;
  // address -> memory object
  for (parents keyed-by object in parents-of)
    record-stat(object, parents, instance-size(object))
  end for;
  let (total-count, total-size)
    = if (display?)
	walker-display-statistics
	  (0, #t, debug-name, required-instance-size, stats);
      else 
	values(0, 0)
      end if;
  *neg-two-statistics* := *neg-one-statistics*;
  *neg-one-statistics* := stats;
  values(total-count, total-size, stats)
end function;

define function walker-instance-statistics
    (display? :: <boolean>, 
     real-object :: <function>, instance-class :: <function>, 
     debug-name :: <function>,
     instance-size :: <function>, required-instance-size :: <function>,
     parents-of :: <table>,
     #key filter-set = #[], aggregate-set = #[])
   local method categorize* 
	     (object, parents, parents-of :: <table>, parents?)
	  => (classes, parent-classes)
	   if (member?(object, filter-set, test: instance?))
	     let parents = parents | element(parents-of, object, default: #f);
	     categorize*(parents, #f, parents-of, parents?);
	   else
	     local method lookup-aggregate-class 
		       (object, set :: <simple-object-vector>) => (res)
		     block (return)
		       for (class in set)
			 when (instance?(object, class))
			   return(instance-class(object))
			 end when;
		       end for;
		       instance-class(object)
		     end block
		   end method;
	     let aggregate-class 
	       = lookup-aggregate-class(object, aggregate-set);
	     let parent-classes
	       = if (parents?)
		   let parents = parents | element(parents-of, object, default: #f);
		   categorize*(parents, #f, parents-of, #f)
		 else
		   #()
		 end if;
	     values(aggregate-class, parent-classes)
	   end if;
	 end method,
         method categorize 
	     (object, parents, parents-of :: <table>) => (classes, parent-classes)
	   categorize*(object, parents, parents-of, #t)
	 end method;
  let number-misses :: <integer> = 0;
  for (parents keyed-by object in parents-of)
    unless (element(parents-of, parents, default: #f))
      number-misses := number-misses + 1;
    end unless;
  finally
    format-out("%= MISSING PARENTS OUT OF %=\n", number-misses, size(parents-of));
  end for;
  do-walker-instance-statistics
    (display?, 
     real-object, instance-class, debug-name,
     instance-size, required-instance-size,
     parents-of, categorize)
end function;

define function walker-display-statistics
    (indentation :: <integer>, summary? :: <boolean>,
     debug-name :: <function>, required-instance-size :: <function>, 
     stats :: <table>)
  let classes
    = sort(key-sequence(stats),
           test: method (c1, c2) 
                   stats[c1].stat-size > stats[c2].stat-size 
                 end);
  let total-size 
    = reduce(method (sum, stats) sum + stats.stat-size end, 0, stats);
  let total-count
    = reduce(method (sum, stats) sum + stats.stat-count end, 0, stats);
  let limit = floor/(total-size, 1000);
  let interesting-classes
    = choose(method (c) abs(stats[c].stat-size) > limit end, classes);
  if (summary?)
    format-out("%s classes, size = %d words\n", size(classes), total-size);
  end if;
  unless (total-size = 0)
    let final = "Everything else";
    let class-field-width
      = reduce(method (n, c) max(n, as(<string>, c.debug-name).size) end,
	       size(final), interesting-classes);
    let cum-size  = 0;
    let cum-count = 0;
    local method report-class (class, name, ccount, csize)
	    for (i from 0 below indentation)
	      format-out(" ");
	    end for;
	    format-out("%s", name);
	    for (i from 0 below class-field-width - size(name))
	      format-out(" ")
	    end for;
	    let (pct, rem)
	      = floor/(round/(csize * 10000.0, total-size), 100.0);
	    cum-size  := cum-size + csize;
	    cum-count := cum-count + ccount;
	    let (cpct, crem)
	      = floor/(round/(cum-size * 10000.0, total-size), 100.0);
	    let isize = required-instance-size(class);
	    format-out(" - %6d [%6d] (%2d) words (%2d.%d%%) -- %2d.%d%% so far\n",
		       csize, ccount, isize, 
		       pct, truncate(rem), cpct, truncate(crem));
	  end method;
    for (class in interesting-classes)
      let name = as(<string>, class.debug-name);
      let csize  = stats[class].stat-size;
      let ccount = stats[class].stat-count;
      report-class(class, name, ccount, csize);
      let parent-stats = stats[class].stat-parent-stats;
      if (parent-stats)
	walker-display-statistics
	  (indentation + 2, #f, debug-name, 
	   required-instance-size, parent-stats);
      end if;
    end for;
    if (summary?)
      report-class(<list>, final, total-count - cum-count, total-size - cum-size);
    end if;
  end unless;
  values(total-count, total-size)
end function;

