Module:    DUIM-OLE-Control
Synopsis:  Connect the DUIM gadget protocols with the OLE Control conventions.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define function compute-type-info ( doc-sheet :: duim/<sheet>,
				    title :: false-or(<string>),
				    short-name :: false-or(<string>),
				    class-id, value-type,
				    disp-typeinfo-options :: <sequence> )
 => (typeinfo :: <coclass-type-info>)

  let properties :: <vector> = make(<stretchy-vector>, size: 0);
  let methods    :: <vector> = make(<stretchy-vector>, size: 0);
  if ( title == #f ) title := $NULL-OLESTR; end if;
  let doc-string = title;
  let (maybe-gadget, maybe-draw) = find-gadget(doc-sheet);
  if ( maybe-gadget )
    let gadget :: duim/<gadget> = maybe-gadget;
    debug-out("generating gadget properties for %=\n", gadget);
    let tabstop? :: <boolean> = instance?(gadget, duim/<tab-control>);
    if ( instance?(gadget, duim/<value-gadget>) )
      // has a "value property"
      properties := add!(properties,
			 make(<variable-description>,
			      name: "Value", disp-id: $DISPID-VALUE,
			      type: value-type |
				duim/gadget-value-type(gadget),
			      documentation: "gadget value",
			      getter: ocx-gadget-value-property,
			      setter: ocx-gadget-value-property-setter));
    end if;
    if ( instance?(gadget, duim/<collection-gadget>) )
      // has an "items property"
      properties := add!(properties,
			 make(<variable-description>,
			      name: "items",
			      type: ole-array-type(<LPVARIANT>),
			      documentation: "gadget items",
			      getter: ocx-gadget-items-property,
			      setter: ocx-gadget-items-property-setter));
      tabstop? := #t;
      block ()
	properties := add!(properties,
			   make(<constant-description>,
				name: "mode",
				documentation: "gadget selection mode",
				value: as(<string>,
					  duim/gadget-selection-mode(gadget))
				  ));
      exception ( <error> )
      end block;
    end if;
    if ( instance?(gadget, duim/<action-gadget>) )
      // has an "activate method"
      methods := add!(methods,
		      make(<function-description>,
			   function: ocx-gadget-activate,
			   name: "activate", disp-id: $DISPID-DOCLICK,
			   documentation: "call activate-gadget"));
    end if;
    if ( instance?(gadget, duim/<labelled-gadget-mixin>) )
      properties := add!(properties,
			 make(<variable-description>,
			      name: "Caption", disp-id: $DISPID-CAPTION,
			      type: <BSTR>,
			      documentation: "gadget label",
			      getter: ocx-gadget-label-property,
			      setter: ocx-gadget-label-property-setter));
    end if;
    if ( instance?(gadget, duim/<text-gadget>) )
      properties := add!(properties,
			 make(<variable-description>,
			      name: "Text", disp-id: $DISPID-TEXT,
			      type: <BSTR>,
			      documentation: "gadget text",
			      getter: ocx-gadget-text-property,
			      setter: ocx-gadget-text-property-setter));
      tabstop? := #t;
    end if;
    if ( any-UI?(gadget) )
      properties := add!(properties,
			 make(<variable-description>,
			      name: "Enabled", disp-id: $DISPID-ENABLED,
			      type: <boolean>,
			      documentation: "gadget-enabled?",
			      getter: ocx-gadget-enabled-property,
			      setter: ocx-gadget-enabled-property-setter));
    end if;
    if ( tabstop? )
      properties := add!(properties,
			 make(<constant-description>,
			      name: "TabStop", disp-id: $DISPID-TABSTOP,
			      value: tabstop?));
    end if;
    let gadget-doc = duim/gadget-documentation(gadget);
    if ( instance?(gadget-doc, <string>) & ~ empty?(gadget-doc) )
      doc-string := gadget-doc;
    end if;
  end if; // end gadget

  if ( maybe-draw ) // found a drawing pane
    debug-out("generating color properties for %=\n", maybe-draw);
    properties := add!(properties,
		       make(<variable-description>,
			    name: "ForeColor", disp-id: $DISPID-FORECOLOR,
			    type: <C-long>,
			    documentation: "foreground color",
			    getter: ocx-foreground-property,
			    setter: ocx-foreground-property-setter));
    properties := add!(properties,
		       make(<variable-description>,
			    name: "BackColor", disp-id: $DISPID-BACKCOLOR,
			    type: <C-long>,
			    documentation: "background color",
			    getter: ocx-background-property,
			    setter: ocx-background-property-setter));
  end if;

  let dispatch-typeinfo :: <Disp-Type-Info> =
    make(<Disp-Type-Info>,
	 name:  // can't have same name as coclass type
	   if ( maybe-gadget )
	     "DUIM_gadget_dispatch"
	   else
	     "DUIM_default_dispatch"
	   end if,
	 documentation: doc-string,
	 properties: properties,
	 methods: methods);

  unless ( empty?(disp-typeinfo-options) )
    // add any user-defined members
    dispatch-typeinfo := apply(make, <Disp-Type-Info>,
			       inherit: dispatch-typeinfo,
			       disp-typeinfo-options);
  end unless;

  let coclass-type-info =
    make(<coclass-Type-Info>,
	 uuid: class-id,
	 name: short-name | title,
	 documentation: title,
	 class: <DUIM-OCX>,
	 interfaces: vector(make(<component-interface-description>,
				 class: <ocx-dispatch>,
				 typeinfo: dispatch-typeinfo))
	   );
  coclass-type-info
end compute-type-info;

define method find-gadget ( sheet :: duim/<gadget> )
  values(sheet, #f)
end;

define method find-gadget ( sheet :: duim/<sheet> )
  values(#f, #f)
end;

define method find-gadget ( sheet :: duim/<sheet-with-medium-mixin> )
  if ( duim/sheet-handles-repaint?(sheet) &
	~ instance?(sheet, duim/<top-level-sheet>) &
	~ instance?(sheet, <ole-main-sheet>) )
    values(#f, sheet)      
  else
    next-method()
  end if
end;

define method find-gadget ( sheet :: duim/<layout> )
  let children = duim/sheet-children(sheet);
  if ( size(children) = 1 )
    find-gadget(children[0])
  else
    let gadget-count :: <integer> = 0;
    let gadget = #f;
    let draw-count :: <integer> = 0;
    let drawing-pane = #f;
    for ( child :: duim/<sheet> in children )
      if ( instance?(child, duim/<gadget>) )
	gadget := child;
	gadget-count := gadget-count + 1;
      elseif ( duim/sheet-handles-repaint?(child)
		& instance?(child, duim/<sheet-with-medium-mixin>) )
	drawing-pane := child;
	draw-count := draw-count + 1;
      end if;
    end for;
    values((gadget-count = 1) & gadget,
	   (draw-count = 1) & drawing-pane)
  end if
end;

define function compute-misc-status ( frame :: duim/<frame>,
				      doc-sheet :: duim/<sheet>)
 => misc-status :: <integer>;

  let misc-status :: <integer> = 0;
  let ( edit-menus, object-menus, help-menus ) =
    frame-container-menus(frame);
  if ( any-menu?(edit-menus) | any-menu?(object-menus) |
	any-menu?(help-menus) )
    // has a menu bar to merge with the container's menu bar.
    misc-status := logior(misc-status, $OLEMISC-WANTSTOMENUMERGE);
  elseif ( frame.duim/frame-tool-bar == #f )
    // no menu bar and no tool bar
    misc-status := logior(misc-status,
			  if ( any-UI?(doc-sheet) )
			    // can be active at the same time as others.
			    $OLEMISC-INSIDEOUT
			  else
			    // never needs to be active.
			    $OLEMISC-NOUIACTIVATE
			  end if );
  end if;

  let maybe-gadget = find-gadget(doc-sheet);
  if ( maybe-gadget )
    let gadget :: duim/<gadget> = maybe-gadget;
    if ( ~ zero?(logand(misc-status, $OLEMISC-INSIDEOUT)) )
      // control should be active whenever it is visible.
      misc-status := logior(misc-status, $OLEMISC-ACTIVATEWHENVISIBLE);
      if ( instance?(gadget, duim/<button>) &
	   instance?(gadget, duim/<default-gadget-mixin>) )
	// acts like a button by responding to $DISPID-AMBIENT-DISPLAYASDEFAULT
	misc-status := logior(misc-status, $OLEMISC-ACTSLIKEBUTTON);
      end if;
    end if;
  end if;

  // Note: do not yet know how to automatically compute these:
  //	$OLEMISC-RECOMPOSEONRESIZE, $OLEMISC-ONLYICONIC,
  //	$OLEMISC-INSERTNOTREPLACE, $OLEMISC-STATIC,
  //	$OLEMISC-CANTLINKINSIDE, $OLEMISC-CANLINKBYOLE1,
  //	$OLEMISC-ISLINKOBJECT, $OLEMISC-INVISIBLEATRUNTIME,
  //	$OLEMISC-ALWAYSRUN, $OLEMISC-ACTSLIKELABEL, $OLEMISC-ALIGNABLE,
  //	$OLEMISC-SIMPLEFRAME, $OLEMISC-SETCLIENTSITEFIRST,
  //	$OLEMISC-IMEMODE, $OLEMISC-IGNOREACTIVATEWHENVISIBLE,
  //	$OLEMISC-NOTMINIMUMNOTIFY, $OLEMISC-SUPPORTSMULTILEVELUNDO

  debug-out("misc-status = #x%x\n", misc-status);
  misc-status
end;

define method any-menu? ( menu-bar :: <sequence> )
  ~ empty?(menu-bar)
end;

define method any-menu? ( menu-bar :: duim/<sheet> )
  #t
end;

define method any-menu? ( menu-bar :: <object> )
  #f
end;

define method any-UI? ( sheet :: duim/<sheet> ) => (UI? :: <boolean>);
  // does this sheet accept some kind of user input?
  sheet.duim/sheet-handles-events? |
    sheet.duim/sheet-handles-keyboard? |
    (~ null?(duim/accelerator-table(sheet))) |
    any?( any-UI?, duim/sheet-children(sheet) )
end;


// default dispatch properties

define sealed method dispatch-doc-sheet ( dispatch :: <IDispatch> )
 => (sheet :: false-or(duim/<sheet>))
  let obj :: <DUIM-OLE-server> = dispatch.controlling-unknown;
  obj.doc-sheet
end method;

// This should only be called when we already know that there is a gadget.
define sealed method dispatch-gadget ( dispatch :: <ocx-dispatch> )
 => ( gadget :: duim/<gadget> )
  dispatch.ocx-gadget |
    begin dispatch.ocx-gadget := next-method() end
end method;

define sealed method dispatch-gadget ( dispatch :: <IDispatch> )
 => ( gadget :: duim/<gadget> )
  let doc-sheet = dispatch.dispatch-doc-sheet;
  find-gadget(doc-sheet)
end method;

define function ocx-gadget-value-property ( dispatch :: <ocx-dispatch> )
 => ( value :: <object> );
  let gadget = dispatch.dispatch-gadget;
  gadget.duim/gadget-value
end;

define function ocx-gadget-value-property-setter ( value :: <object>,
						   dispatch :: <ocx-dispatch> )
 => ( value :: <object> );
  let gadget = dispatch.dispatch-gadget;
  duim/gadget-value-setter(value, gadget)
end;

define function ocx-gadget-items-property ( dispatch :: <ocx-dispatch> )
 => ( value :: <sequence> );
  let gadget = dispatch.dispatch-gadget;
  gadget.duim/gadget-items
end;

define function ocx-gadget-items-property-setter ( value :: <sequence>,
						   dispatch :: <ocx-dispatch> )
 => ( value :: <sequence> );
  let gadget = dispatch.dispatch-gadget;
  gadget.duim/gadget-items := value
end;

define function ocx-gadget-label-property ( dispatch :: <ocx-dispatch> )
 => ( label :: <string> );
  let gadget = dispatch.dispatch-gadget;
  let label = gadget.duim/gadget-label;
  label | $NULL-BSTR
end;

define function ocx-gadget-label-property-setter ( label :: <object>,
						   dispatch :: <ocx-dispatch> )
 => ( label :: <object> );
  let gadget = dispatch.dispatch-gadget;
  gadget.duim/gadget-label := if ( empty?(label) ) #f else label end if;
end;

define function ocx-gadget-text-property ( dispatch :: <ocx-dispatch> )
 => ( text :: <string> );
  let gadget = dispatch.dispatch-gadget;
  let text = gadget.duim/gadget-text;
  text | $NULL-BSTR
end;

define function ocx-gadget-text-property-setter ( text :: <object>,
						  dispatch :: <ocx-dispatch> )
 => ( text :: <object> );
  let gadget = dispatch.dispatch-gadget;
  gadget.duim/gadget-text := text;
end;

define function ocx-gadget-enabled-property ( dispatch :: <ocx-dispatch> )
 => ( enabled? :: <boolean> );
  let gadget = dispatch.dispatch-gadget;
  gadget.duim/gadget-enabled?
end;

define function ocx-gadget-enabled-property-setter ( enabled? :: <boolean>,
						   dispatch :: <ocx-dispatch> )
 => ( enabled? :: <boolean> );
  let gadget = dispatch.dispatch-gadget;
  gadget.duim/gadget-enabled? := enabled?;
end;


define function ocx-foreground-property ( dispatch :: <ocx-dispatch> )
 => ( color :: <integer> );
  get-ocx-color(dispatch, duim/medium-foreground)
end;

define function ocx-foreground-property-setter
 ( ole-color :: <integer>, dispatch :: <ocx-dispatch> )
 => ( ole-color :: <integer> );
  set-ocx-color(ole-color, dispatch, duim/medium-foreground-setter)
end;

define function ocx-background-property ( dispatch :: <ocx-dispatch> )
 => ( color :: <integer> );
  get-ocx-color(dispatch, duim/medium-background)
end;

define function ocx-background-property-setter
 ( ole-color :: <integer>, dispatch :: <ocx-dispatch> )
 => ( ole-color :: <integer> );
  set-ocx-color(ole-color, dispatch, duim/medium-background-setter)
end;

define function dispatch-drawing-medium ( dispatch :: <ocx-dispatch> )
  let doc-sheet = dispatch.dispatch-doc-sheet;
  let ( gadget, drawing-sheet ) = find-gadget(doc-sheet);
  drawing-sheet.duim/sheet-medium
end;

define function get-ocx-color
    ( dispatch :: <ocx-dispatch>, getter :: <function> )
 => ( color :: <integer> );
  let medium = dispatch-drawing-medium(dispatch);
  let duim-color = getter(medium);
  duim/color->native-color(duim-color, medium)
end;

define function set-ocx-color
 ( ole-color :: <integer>, dispatch :: <ocx-dispatch>, setter :: <function> )
 => ( ole-color :: <integer> );
  let medium = dispatch-drawing-medium(dispatch);
  let obj :: <DUIM-OLE-server> = dispatch.controlling-unknown;
  let native-color = OLE-util-translate-color(obj, ole-color);
  let duim-color = duim/native-color->color(native-color, medium);
  setter(duim-color,medium);
  ole-color
end;


// dispatch methods

define function ocx-gadget-activate ( dispatch :: <ocx-dispatch> )
 => ( status :: <SCODE> )
  let gadget = dispatch.dispatch-gadget;
  duim/activate-gadget(gadget);
  $S-OK
end;



define method OLE-part-mnemonics ( obj :: <DUIM-OCX> )
 => table :: false-or(<HACCEL>);

  let sheet = obj.doc-sheet;
  if ( instance?(sheet, duim/<accelerator-mixin>)
	| instance?(sheet, duim/<top-level-sheet>) )
    // use DUIM accelerators
    let table = duim/accelerator-table(sheet);
    if ( table == #f & ~ duim/sheet-mapped?(sheet) )
      // DUIM won't compute the accelerator table until after the sheet
      // is mapped, so make a note to do this again later.
      obj.deferred-accelerator := #t;
    end if;
    table
  else
    #f
  end if
end method;

define method OLE-part-show-window (obj :: <DUIM-OCX>, window :: <HWND>) => ();
  next-method();
  if ( obj.deferred-accelerator )
    // Now that the sheet is mapped, try again to get the accelerator table.
    unless ( null?(duim/accelerator-table(obj.doc-sheet)) )
      OLE-util-key-change(obj);
    end unless;
    obj.deferred-accelerator := #f;
  end if;
end method;

define method OLE-part-ambient-properties ( obj :: <DUIM-OCX> )
 => ( properties :: <sequence> );
  let properties = next-method();
  let top-sheet = obj.doc-sheet;
  let gadget = find-gadget(top-sheet);
  if ( instance?(gadget, duim/<default-gadget-mixin>) )
    add-new(properties, $DISPID-AMBIENT-DISPLAYASDEFAULT)
  else
    properties
  end if
end method;

define method OLE-part-set-ambient-property
    ( obj :: <DUIM-OCX>,
      disp-id == $DISPID-AMBIENT-DISPLAYASDEFAULT,
      default? :: <boolean> ) => ();
  debug-out("  gadget-default? := %=\n", default?);
  let top-sheet = obj.doc-sheet;
  let gadget = find-gadget(top-sheet);
  if ( instance?(gadget, duim/<default-gadget-mixin>) )
    duim/gadget-default?(gadget) := default?;
  end if;
end method;
