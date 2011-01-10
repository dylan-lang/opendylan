module:    binary-builder
Synopsis:  Support for assembling and dumping BINARY files
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $code-section           = ".text";     // Code
define constant $init-code-section      = ".text$i";   // Initialization code
define constant $elf-init-code-section  = ".init";     // SO initialization code
define constant $elf-fini-code-section  = ".fini";     // SO finalization code
define constant $untraced-objs-section  = ".dyutr$m";  // Untraced Dylan objects 
define constant $untraced-data-section  = ".dyutr$r";  // Untraced random data
define constant $data-section           = ".dydat$m";  // Ambiguously traced data
define constant $data-start-section     = ".dydat$a";
define constant $data-end-section       = ".dydat$z";
define constant $vars-section           = ".dyvar$m";  // Dylan roots (variables)
define constant $vars-start-section     = ".dyvar$a";
define constant $vars-end-section       = ".dyvar$z";
define constant $objs-section           = ".dyobj$m";  // Dylan objects (a static heap)
define constant $objs-start-section     = ".dyobj$a";
define constant $objs-end-section       = ".dyobj$z";
define constant $fixup-section          = ".dyfix$m";  // Import relocation fixups
define constant $fixup-start-section    = ".dyfix$a";
define constant $fixup-end-section      = ".dyfix$z";
define constant $import-section         = ".dyimp$m";  // Import relocation fixups
define constant $import-start-section   = ".dyimp$a";
define constant $import-end-section     = ".dyimp$z";
define constant $directives-section     = ".drectve";

define constant $data-start-symbol  = "_dylan_data_start";
define constant $data-end-symbol    = "_dylan_data_end";
define constant $vars-start-symbol  = "_dylan_vars_start";
define constant $vars-end-symbol    = "_dylan_vars_end";
define constant $objs-start-symbol  = "_dylan_objs_start";
define constant $objs-end-symbol    = "_dylan_objs_end";
define constant $fixup-start-symbol = "_dylan_fixup_start";
define constant $fixup-end-symbol   = "_dylan_fixup_end";
define constant $import-start-symbol = "_dylan_import_start";
define constant $import-end-symbol   = "_dylan_import_end";

define constant $obj-file-start-data-symbol = "_dylan_object_file_data_start";
define constant $obj-file-start-vars-symbol = "_dylan_object_file_vars_start";
define constant $obj-file-start-objs-symbol = "_dylan_object_file_objs_start";
define constant $obj-file-start-untraced-objs-symbol = "_dylan_untraced_objs_start";
define constant $obj-file-start-untraced-data-symbol = "_dylan_untraced_data_start";


define function ensure-optional-section  
    (builder :: <binary-builder>, 
     optional-section :: false-or(<section-with-fixups>),
     section-name :: <byte-string>, 
     start-symbol :: <byte-string>)
    => (section-with-fixups :: <section-with-fixups>)
  let section-with-fixups :: <section-with-fixups> = 
    if (optional-section)
      builder.current-section := optional-section.actual-section;
      optional-section;
    else
      select-data-section(builder, section-name);
      add-binary-symbol-definition(builder, start-symbol, public?: #f);
      make(<section-with-fixups>, section: builder.current-section);
    end if;
  builder.current-fixups := section-with-fixups.import-fixups;
  builder.id-current-fixups := section-with-fixups.id-import-fixups;
  section-with-fixups;
end function;


define function ensure-code-section  
    (builder :: <binary-builder>, 
     optional-section :: false-or(<binary-section>),
     section-name :: <byte-string>,
     code-item-increment)
    => (section :: <binary-section>)
  builder.current-fixups := #f;
  builder.id-current-fixups := #f;
  if (optional-section)
    builder.current-section := optional-section;
    optional-section;
  else
    select-code-section(builder, section-name, code-item-increment);
    builder.current-section;
  end if;
end function;


define function select-dylan-section 
    (builder :: <binary-builder>, section :: <symbol>,
     code-item-increment) => ()
  if (*dll-support*)
    select (section)
      #"code" =>
        builder.code-section
          := ensure-code-section(builder, builder.code-section,
                                 $code-section, code-item-increment);
      #"init-code" =>
        builder.init-code-section
          := ensure-code-section(builder, builder.init-code-section,
                                 $init-code-section, code-item-increment);
      #"elf-init-code" =>
        builder.elf-init-code-section
          := ensure-code-section(builder, builder.elf-init-code-section,
                                 $elf-init-code-section, code-item-increment);
      #"elf-fini-code" =>
        builder.elf-fini-code-section
          := ensure-code-section(builder, builder.elf-fini-code-section,
                                 $elf-fini-code-section, code-item-increment);
      #"data", #"ambiguous-data" => 
        builder.optional-data-section
          := ensure-optional-section(builder, builder.optional-data-section, 
                                     $data-section,
                                     $obj-file-start-data-symbol);
      #"objects" =>
        builder.optional-objs-section
          := ensure-optional-section(builder, builder.optional-objs-section, 
                                     $objs-section,
                                     $obj-file-start-objs-symbol);
      #"variables" =>
        builder.optional-vars-section
          := ensure-optional-section(builder, builder.optional-vars-section, 
                                     $vars-section,
                                     $obj-file-start-vars-symbol);
      #"untraced-objects" =>
        builder.optional-untraced-objs-section
          := ensure-optional-section(builder,
                                     builder.optional-untraced-objs-section, 
                                     $untraced-objs-section,
                                     $obj-file-start-untraced-objs-symbol);
      #"untraced-data" =>
        builder.optional-untraced-data-section
          := ensure-optional-section(builder,
                                     builder.optional-untraced-data-section, 
                                     $untraced-data-section,
                                     $obj-file-start-untraced-data-symbol);
      otherwise => 
        select-data-section(builder, as(<string>, section));
        builder.current-fixups := #f;
        builder.id-current-fixups := #f;
    end select;
  else
    select-data-section(builder, ".data"); // for backwards compatibility
  end if;
end function;


define method select-data-section (builder :: <binary-builder>, name :: <byte-string>)
 => ()
  select-binary-section(builder, name, alignment: 4,
			flags: select (name by \=)
				 ".data" => data-flags(builder);
				 otherwise => dylan-data-flags(builder);
			       end);
end method;


define method select-code-section 
    (builder :: <binary-builder>, name :: <byte-string>, code-item-increment) => ()
  select-binary-section(builder, name, alignment: code-item-increment,
			flags: if (name = $init-code-section)
				 init-code-flags(builder)
			       else code-flags(builder)
			       end);
end method;


/// Management of section data
//
// The slot "raw-section-size" in a <binary-section> is used as a fill pointer into
// <byte-vector> in the "section-data" slot.  When we want to add more data, we
// have to first check to see if we need a bigger <byte-vector>. In effect, the 
// <binary-section> objects is itself being used as a stretchy vector. The management
// for all this happens here.


define method ensure-size-of-section-data 
    (section :: <binary-section>, len :: <integer>)
  let data = section.section-data;
  let curr-len = data.size;
  if (len > curr-len)
    // Time to grow the vector. Keep multiplying by 2 until it's big enough
    let want = max(curr-len, 1024);
    while (want < len)
      want := want + want;
    end while;
    let new-data = make(<byte-vector>, size: want);
    copy-bytes(new-data, 0, data, 0, curr-len);
    section.section-data := new-data;
  end if;
end method;


define open generic  current-position 
    (section :: <binary-section>) => (pos :: <integer>);

define open generic current-position-setter 
    (new-pos :: <integer>, section :: <binary-section>) => (pos :: <integer>);

define method current-position 
    (section :: <binary-section>) => (pos :: <integer>)
  section.raw-data-size;
end method;


define method current-position-setter 
    (new-pos :: <integer>, section :: <binary-section>) => (pos :: <integer>)
  ensure-size-of-section-data(section, new-pos);
  section.raw-data-size := new-pos;
end method;


define method bytes-for-realignment 
      (pos :: <integer>, alignment :: <integer>) => (padding :: <integer>)
  let excess = modulo(pos, alignment);
  if (excess == 0) 0 else alignment - excess end if;
end method;


define open generic fill-section-data
     (section :: <binary-section>, fill :: <integer>, #key)
     => ();

define method align-section-data
     (section :: <binary-section>, 
      #key alignment = section.section-alignment, fill = 0) 
     => ()
  let pos = section.current-position;
  let padding = bytes-for-realignment(pos, alignment);
  if (padding > 0)
    let new-size = pos + padding;
    section.current-position := new-size;
    fill-section-data(section, fill, start: pos, end: new-size);
  end if;
end method;


define method select-binary-section 
    (builder :: <binary-builder>, section-name :: <byte-string>,
     #key alignment = 4, flags = init-flags(builder)) => ()
  builder.current-section :=
    share-or-create(builder.binary-file.sections, section-name, unsupplied(),
                    method ()
                      make-binary-section(builder, section-name, 
                                        alignment, flags);
                    end method);
end method;

define open generic  make-binary-section 
    (builder :: <binary-builder>, name :: <byte-string>, 
     alignment :: <integer>, flags)
    => (new :: <binary-section>);


define open generic add-symbol-definition
    (outputter :: <binary-builder>,
     name :: <byte-string>, model-object,
     #key section);

define method add-binary-symbol-definition
    (builder :: <binary-builder>,
     name :: <byte-string>,
     #rest all-keys,
     #key, #all-keys)
    => ()
    apply(add-symbol-definition,
          builder, name, unsupplied(), all-keys);
end method;


// Adding data to sections

define open generic add-word-to-section
    (section :: <binary-section>, data) => ();

define open generic add-short-to-section
    (section :: <binary-section>, data) => ();

define open generic add-byte-to-section 
    (section :: <binary-section>, data :: <integer>) => ();

define open generic add-string-to-section 
    (section :: <binary-section>, string :: <byte-string>) => ();


define method add-integer-data 
    (builder :: <binary-builder>,
     data :: <abstract-integer>,
     #key section = builder.current-section)
    => ()
  add-word-to-section(section, data);
end method;

define method add-integer-data-short
    (builder :: <binary-builder>,
     data :: <integer>,
     #key section = builder.current-section)
    => ()
  add-short-to-section(section, data);
end method;

define method add-data-byte
    (builder :: <binary-builder>, data :: <integer>, 
     #key section = builder.current-section)
    => ()
  add-byte-to-section(section, data);
end method;

define method add-data-byte
    (builder :: <binary-builder>, char :: <character>, 
     #key section = builder.current-section)
    => ()
  add-byte-to-section(section, as(<integer>, char));
end method;

define open generic add-data 
    (builder :: <binary-builder>, data, model-object,
     #key section = builder.current-section)
    => ();

define method add-data 
    (builder :: <binary-builder>,
     data :: <abstract-integer>, model-object,
     #key section = builder.current-section)
    => ()
  add-word-to-section(section, data);
end method;

define method add-data-string
    (builder :: <binary-builder>, string :: <byte-string>, 
     #key section = builder.current-section)
    => ()
  add-string-to-section(section, string);
end method;


// Binary Section flags

define open generic directives-flags(outputter :: <binary-builder>)
 => (flags);

define open generic data-flags(outputter :: <binary-builder>)
 => (flags);

define open generic dylan-data-flags(outputter :: <binary-builder>)
 => (flags);

define open generic code-flags(outputter :: <binary-builder>)
 => (flags);

define open generic init-code-flags(outputter :: <binary-builder>)
 => (flags);

define open generic init-flags(outputter :: <binary-builder>)
 => (flags);

define open generic fixup-flags(outputter :: <binary-builder>)
 => (flags);
