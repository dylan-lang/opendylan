module: dfmc-visualization
Author:    Hannes Mehnert
License:   See License.txt in this distribution for details.
Copyright: Dylan Hackers 2014
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND
synopsis:  Dylan side of graphical visualization of DFM control flow graphs


define method form (c :: type-union(<object-reference>, <temporary>, <computation>))
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

define method form (c :: <collection>)
  c.first.form
end;

define variable *lambda-string-table* :: <table> = make(<table>);

define function init-visualization ()
  *lambda-string-table* := make(<table>);
end;

define function get-or-insert (f, id :: <string>) => (res :: <string>)
  if (copy-sequence(id, end: min(id.size, $top-l-i.size)) = $top-l-i)
    let name = element(*lambda-string-table*, f, default: #f);
    unless (name)
      *lambda-string-table*[f]
        := concatenate(id, integer-to-string(*lambda-string-table*.size));
    end;
    *lambda-string-table*[f]
  else
    id
  end;
end;

define constant $top-l-i = "top-level-initializer";

define method identifier (l :: type-union(<&accessor-method>, <&lambda>))
 => (res :: <string>)
  if (instance?(l.model-creator, <compilation-record>))
    l.debug-name.identifier
  elseif (instance?(l.model-creator, <top-level-init-form>))
    let id = as(<string>, l.debug-name.identifier);
    get-or-insert(l, id)
  else
    l.model-creator.identifier
  end;
end;

define method identifier (f) => (res :: <string>)
  let id = as(<string>, f.form-variable-name);
  get-or-insert(f, id)
end;

define method identifier (f :: <name-fragment>) => (res :: <string>)
  as(<string>, f.fragment-name)
end;

define method identifier (m :: <method-definition>) => (res :: <string>)
  let str = make(<string-stream>, direction: #"output");
  print-specializers(m.form-signature, str);
  concatenate(as(<string>, m.form-variable-name), " ", str.stream-contents)
end;


define method identifier (s :: <string>) => (res :: <string>)
  s
end;

define constant form-id = compose(identifier, form);

define function safe-structured-output (writer, o, id)
  block ()
    o.structured-output
  exception (c :: <condition>)
    writer(list(#"type", #"debug-string",
                #"nodeid", id,
                #"context", "safe-structured-output",
                #"error", format-to-string("%=", c)));
    let str = make(<string-stream>, direction: #"output");
    print-object(o, str);
    str.stream-contents
  end;
end;

define method safe-form-id (writer, o :: <collection>, id-or-false)
  unless (o.empty?)
    safe-form-id(writer, o.first, id-or-false)
  end;
end;

define method safe-form-id (writer, o, id-or-false)
  block ()
    if (o)
      o.form-id
    end
  exception (c :: <condition>)
    writer(list(#"type", #"debug-string",
                #"nodeid", id-or-false,
                #"context", "safe-form-id",
                #"error", format-to-string("%=", c)));
    #f
  end;
end;

define function safe-node-id (writer, o)
  block ()
    if (o)
      o.node-id
    end
  exception (c :: <condition>)
    writer(list(#"type", #"debug-string",
                #"context", "safe-node-id",
                #"error", format-to-string("%=", c)));
    #f
  end;
end;


define function trace-computations
    (write-data :: <function>, key :: <symbol>, node, new-or-other, old-or-false)
  let id = safe-node-id(write-data, node);
  let fo = rcurry(curry(safe-form-id, write-data), id);
  select (key by \==)
    #"add-temporary-user", #"remove-temporary-user" =>
      write-data(list(#"type", key,
                      #"nodeid", id,
                      #"formid", new-or-other.fo,
                      #"other", safe-node-id(write-data, new-or-other)));
    #"new-temporary" =>
      write-data(list(#"type", key,
                      #"nodeid", id,
                      #"formid", node.fo,
                      #"description", safe-structured-output(write-data, node, id),
                      #"other", safe-node-id(write-data, new-or-other)));
    #"new-object-reference" =>
      write-data(list(#"type", key,
                      #"nodeid", id,
                      #"formid", new-or-other.fo,
                      #"description", safe-structured-output(write-data, node, id),
                      #"other", safe-node-id(write-data, new-or-other)));
    #"remove-temporary" =>
      write-data(list(#"type", key,
                      #"nodeid", id,
                      #"formid", new-or-other.fo));
    #"new-computation" =>
      write-data(list(#"type", key,
                      #"nodeid", id,
                      #"formid", node.fo,
                      #"description", safe-structured-output(write-data, node, id)));
    #"remove-computation" =>
      write-data(list(#"type", key,
                      #"nodeid", id,
                      #"formid", node.fo));
    #"computation-type-setter", #"cell-type-setter" =>
      let str = make(<string-stream>, direction: #"output");
      let noo =
        if (new-or-other)
          print-object(new-or-other, str);
          str.stream-contents
        else
          #f
        end;
      let str2 = make(<string-stream>, direction: #"output");
      let oof =
        if (old-or-false)
          print-object(old-or-false, str2);
          str2.stream-contents
        else
          #f
        end;
      write-data(list(#"type", key,
                      #"nodeid", id,
                      #"formid", node.fo,
                      #"other", noo,
                      #"old", oof));
    #"type-setter" =>
      let str = make(<string-stream>, direction: #"output");
      let noo =
        if (new-or-other)
          print-object(new-or-other, str);
          str.stream-contents
        else
          #f
        end;
      write-data(list(#"type", key,
                      #"nodeid", id,
                      #"formid", old-or-false.fo,
                      #"other", noo));
    #"call-iep?-setter" =>
      write-data(list(#"type", key,
                      #"nodeid", id,
                      #"formid", node.fo,
                      #"other", new-or-other,
                      #"old", old-or-false));
    otherwise =>
      let form = block ()
                   node.form-id
                 exception (e :: <condition>)
                   block ()
                     new-or-other.form-id
                   exception (e :: <condition>)
                     block ()
                       old-or-false.form-id
                     exception (e :: <condition>)
                       node.fo
                     end;
                   end;
                 end;
      write-data(list(#"type", key,
                      #"nodeid", id,
                      #"formid", form,
                      #"other", safe-node-id(write-data, new-or-other),
                      #"old", safe-node-id(write-data, old-or-false)));
  end;
end;

define function visualize
    (write-data :: <function>,
     key :: <symbol>,
     desc :: <string>,
     object :: <object>)
  let fo = safe-form-id(write-data, object, #f);
  select (key by \==)
    #"highlight-queue" =>
      let id = map(curry(safe-node-id, write-data), object);
      write-data(list(#"type", key,
                      #"formid", fo,
                      #"nodeids", id));

    #"optimizing" =>
      let id = safe-node-id(write-data, object);
      write-data(list(#"type", key,
                      #"formid", fo,
                      #"nodeid", id,
                      #"description", desc));

    otherwise =>
      write-data(list(#"type", key,
                      #"formid", fo,
                      #"description", desc));
  end;
end;
