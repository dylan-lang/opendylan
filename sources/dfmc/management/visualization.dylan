module: dfmc-visualization
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND
synopsis: Dylan side of graphical visualization of DFM control flow graphs


define method form (c :: type-union(<temporary>, <computation>))
  c.environment.form
end;

define method form (c :: <exit>)
  c.entry-state.form
end;

define method form (c :: type-union(<&accessor-method>, <&lambda>))
  if (instance?(c.environment, <lexical-environment>))
    c.environment.form
  else
    c
  end
end;

define method form (c :: <variable-defining-form>)
  c
end;

define method form (c :: <lambda-lexical-environment>)
  (c.outer & c.outer.form) | c.lambda
end;

define method form (c :: <local-lexical-environment>)
  c.outer.form
end;

define constant $lambda-string-table = make(<table>);
//define constant $string-lambda-table = make(<table>);

define constant $top-l-i = "top-level-initializer";

define method identifier (f) => (res :: <string>)
  let id = as(<string>, f.form-variable-name);
  if (copy-sequence(id, end: min(id.size, $top-l-i.size)) = $top-l-i)
    let name = element($lambda-string-table, f, default: #f);
    unless (name)
      $lambda-string-table[f] := concatenate(id, integer-to-string($lambda-string-table.size));
    end;
    $lambda-string-table[f]
  else
    id
  end;
end;

define method identifier (f :: <name-fragment>) => (res :: <string>)
  as(<string>, f.fragment-name)
end;

define method identifier (m :: <method-definition>) => (res :: <string>)
  let str = make(<string-stream>, direction: #"output");
  print-specializers(m.form-signature, str);
  concatenate(as(<string>, m.form-variable-name), " ", str.stream-contents)
end;

define method identifier (l :: type-union(<&accessor-method>, <&lambda>)) => (res :: <string>)
  if (instance?(l.model-creator, <compilation-record>))
//type-union(<top-level-init-form>, <compilation-record>)))
    l.debug-name.identifier
  elseif (instance?(l.model-creator, <top-level-init-form>))
    let id = l.debug-name.identifier;
    if (copy-sequence(id, end: min(id.size, $top-l-i.size)) = $top-l-i)
      let name = element($lambda-string-table, l, default: #f);
      unless (name)
        $lambda-string-table[l] := concatenate(id, integer-to-string($lambda-string-table.size));
      end;
      $lambda-string-table[l]
    else
      id
    end;
  else
    l.model-creator.identifier
  end;
end;

define method identifier (s :: <string>) => (res :: <string>)
  s
end;

define constant form-id = compose(identifier, form);

define method get-id (c :: <computation>) => (id :: <integer>)
  c.computation-id
end;

define method get-id (t /* :: <temporary> */) => (id :: <integer>)
  t.temporary-id
end;

define method get-id (i :: <integer>) => (res :: <integer>)
  i
end;

define function trace-computations (write-data :: <function>, key :: <symbol>, id, comp-or-id, comp2, #key label)
  select (key by \==)
    #"add-temporary-user", #"remove-temporary-user" =>
      write-data(key, comp-or-id, comp-or-id.form-id, id.get-id, comp-or-id.get-id);
    #"add-temporary" =>
      begin
        let str = make(<string-stream>, direction: #"output");
        print-object(id, str);
        write-data(key, if (comp-or-id == 0) id else comp-or-id end,
                   if (comp-or-id == 0) id.form-id else comp-or-id.form-id end,
                   id.get-id, str.stream-contents, 0);
      end;
    #"temporary-generator" =>
      write-data(key, comp-or-id, comp-or-id.form-id, id.get-id, comp-or-id.get-id, comp2.get-id);
    #"remove-temporary" =>
      write-data(key, if (comp-or-id == 0) id else comp-or-id end,
                 if (comp-or-id == 0) id.form-id else comp-or-id.form-id end,
                 id.get-id);
    #"remove-edge", #"insert-edge" =>
      write-data(key, id, id.form-id, id.get-id, comp-or-id.get-id, label);
    #"change-edge" =>
      write-data(key, id, id.form-id, id.get-id, comp-or-id.get-id, comp2.get-id, label);
    #"new-computation" =>
      write-data(key, comp-or-id, comp-or-id.form-id, output-computation-sexp(comp-or-id));
    #"remove-computation" =>
      write-data(key, id, id.form-id, id.get-id);
    #"change-entry-point" =>
      write-data(key, id, id.form-id, id.get-id, comp-or-id);
    #"change-function" =>
      begin
        let str = make(<string-stream>, direction: #"output");
        print-object(comp-or-id, str);
        write-data(key, id, id.form-id, id.get-id, str.stream-contents);
      end;
    #"set-loop-call-loop" =>
      write-data(key, id, id.form-id, id.get-id, comp-or-id, #"no");
    otherwise => ;
  end;
end;

define function visualize (write-data :: <function>, key :: <symbol>, object :: <object>)
  select (key by \==)
    #"dfm-header" =>
      write-data(key, object.head, object.head.form-id, object.tail);
    #"beginning" =>
      write-data(key, object.head, object.head.form-id, object.tail);
    #"relayouted" =>
      write-data(key, object.head, object.head.form-id);
    #"highlight-queue" =>
      write-data(key, object.head, object.head.form-id, object.tail);
    #"highlight" =>
      write-data(key, object, object.form-id, object.get-id);
    #"source" =>
      write-data(key, object.head, object.head.form-id, object.tail);
    otherwise => ;
  end;
end;
