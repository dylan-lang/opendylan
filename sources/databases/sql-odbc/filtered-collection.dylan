Module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <filtered-collection> (<collection>)
  slot filtered-source :: type-union(<collection>, singleton(#"self")),
    required-init-keyword: filtered-source:;

  constant slot filter :: <function>,
    required-init-keyword: filter:;
end class;

define method initialize(c :: <filtered-collection>, #key)
 => ()
  next-method();
  if (c.filtered-source == #"self")
    c.filtered-source := c;
  end if;
end method;

define class <filtered-state> (<object>)
  slot source-state :: <object>,
    required-init-keyword: source-state:;
end class;


define method forward-iteration-protocol(collection :: <filtered-collection>)
  => (initial-state :: <object>,
      limit :: <object>,
      next-state :: <function>,
      finished-state? :: <function>,
      current-key :: <function>,
      current-element :: <function>,
      current-element-setter :: <function>,
      copy-state :: <function>)
  let (source-initial-state :: <object>, 
       source-limit :: <object>, 
       source-next-state :: <function>, 
       source-finished? :: <function>, 
       source-current-key :: <function>,
       source-element :: <function>, 
       source-element-setter :: <function>, 
       source-copy-state :: <function>) 
   = if (collection.filtered-source == collection)
       next-method();
     else
       forward-iteration-protocol(collection.filtered-source);
     end if;

  local method next-unfiltered-state(c :: <filtered-collection>, 
                                     state :: <filtered-state>,
                                     limit :: <object>)
         => (new-state :: <filtered-state>)
          while (~source-finished?(c.filtered-source, state.source-state,  limit) &
                 c.filter(c, source-element(c.filtered-source, state.source-state)))
            state.source-state := source-next-state(c.filtered-source, 
                                                    state.source-state);
          end while;
          state;
        end method;

  local method get-initial-state(c :: <filtered-collection>,
                                 limit :: <object>)
         => (initial-state :: <object>)
          let state = make(<filtered-state>,
                           source-state: source-initial-state);
          next-unfiltered-state(c, state, limit);
        end method;

  local method next-state(c :: <filtered-collection>,
                          state :: <filtered-state>)
         => (new-state :: <filtered-state>)
          state.source-state := source-next-state(c.filtered-source, 
                                                  state.source-state);
          next-unfiltered-state(c, state, source-limit);
        end method;

  local method finished-state?(c :: <filtered-collection>,
                               state :: <filtered-state>,
                               limit :: <object>)
         => (finished? :: <boolean>)
           source-finished?(c.filtered-source, state.source-state, limit)
        end method;

  local method current-key(c :: <filtered-collection>,
                           state :: <filtered-state>)
         => (key :: <object>)
          source-current-key(c.filtered-source, state.source-state);
        end method;

  local method current-element(c :: <filtered-collection>,
                               state :: <filtered-state>)
         => (element :: <object>)
          source-element(c.filtered-source, state.source-state);
        end method;

  local method element-setter(value :: <object>,
                              c :: <filtered-collection>,
                              state :: <filtered-state>)
         => (value :: <object>)
          source-element-setter(value, c.filtered-source, state.source-state);
        end method;

  local method copy-state(c :: <filtered-collection>,
                          state :: <filtered-state>)
         => (new-state :: <filtered-state>)
          make(<filtered-state>,
               source-state: copy-state(c.filtered-source, state.source-state));
        end method;

  values(get-initial-state(collection, source-limit),
	 source-limit,
	 next-state,
	 finished-state?,
	 current-key,
	 current-element,
	 element-setter,
	 copy-state);
end method;

