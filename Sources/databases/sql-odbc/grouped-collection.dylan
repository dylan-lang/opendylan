Module: sql-odbc-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <grouped-collection> (<collection>)
  slot grouped-source :: type-union(singleton(#"self"), <collection>),
    required-init-keyword: grouped-source:;

  constant slot new-group :: <function>,
    required-init-keyword: new-group:;
  // prototype: fn(collection :: <grouped-collection>, first-element :: <object>) 
  //             => (result :: <object>)

  constant slot group-finished? :: <function>,
    required-init-keyword: group-finished?:;
  // prototype: fn(collection :: <grouped-collection>, current-group :: <object>
  //               element :: <object>) => (result :: <boolean>)

  constant slot grouper :: <function>,
    required-init-keyword: grouper:;
  // prototype: fn(collection :: <grouped-collection>, current-group :: <object>
  //               element :: <object>) => ()
end class;

define method initialize(c :: <grouped-collection>, #key)
 => ()
  next-method();
  if (c.grouped-source == #"self")
    c.grouped-source := c;
  end if;
end method;

define class <grouped-state> (<object>)
  slot source-state :: <object>,
    required-init-keyword: source-state:;

  slot current-group :: <object>;
  slot at-end? :: <boolean> = #f;
end class;

define method forward-iteration-protocol(collection :: <grouped-collection>)
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
   = if (collection.grouped-source == collection)
       next-method();
     else
       forward-iteration-protocol(collection.grouped-source);
     end if;

  local method next-grouped-state(c :: <grouped-collection>, 
                                  state :: <grouped-state>,
                                  limit :: <object>)
         => (new-state :: <grouped-state>)
          if (source-finished?(c.grouped-source, state.source-state, limit))
            state.at-end? := #t;
          else
            state.current-group 
              := c.new-group(c, source-element(c.grouped-source, state.source-state));
            state.source-state := source-next-state(c.grouped-source, state.source-state);

            while (~source-finished?(c.grouped-source, state.source-state, limit) &
                   ~c.group-finished?(c, state.current-group, 
                                      source-element(c.grouped-source, state.source-state)))
              c.grouper(c, state.current-group, 
                        source-element(c.grouped-source, state.source-state));
              state.source-state := source-next-state(c.grouped-source, state.source-state);
            end while;
          end if;
          state;
        end method;

  local method get-initial-state(c :: <grouped-collection>,
                                 limit :: <object>)
         => (initial-state :: <object>)
          let state = make(<grouped-state>,
                           source-state: source-initial-state);
          next-grouped-state(c, state, limit);
        end method;

  local method next-state(c :: <grouped-collection>,
                          state :: <grouped-state>)
         => (new-state :: <grouped-state>)
          //state.source-state := source-next-state(c.grouped-source, 
          //                                        state.source-state);
          next-grouped-state(c, state, source-limit);
        end method;

  local method finished-state?(c :: <grouped-collection>,
                               state :: <grouped-state>,
                               limit :: <object>)
         => (finished? :: <boolean>)
           state.at-end?;
           //source-finished?(c.grouped-source, state.source-state, limit)
        end method;

  local method current-key(c :: <grouped-collection>,
                           state :: <grouped-state>)
         => (key :: <object>)
          source-current-key(c.grouped-source, state.source-state);
        end method;

  local method current-element(c :: <grouped-collection>,
                               state :: <grouped-state>)
         => (element :: <object>)
          state.current-group;
        end method;

  local method element-setter(value :: <object>,
                              c :: <grouped-collection>,
                              state :: <grouped-state>)
         => (value :: <object>)
          source-element-setter(value, c.grouped-source, state.source-state);
        end method;

  local method copy-state(c :: <grouped-collection>,
                          state :: <grouped-state>)
         => (new-state :: <grouped-state>)
          make(<grouped-state>,
               source-state: copy-state(c.grouped-source, state.source-state));
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
