Module:    button-ocx
Synopsis:  Demonstrate using a DUIM gadget as an OLE Control.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function text-property (dispatch :: <ocx-dispatch>)
 => (text :: <string>);
  dispatch.ocx-frame.my-text
end;

define function text-property-setter (text :: <string>,
				      dispatch :: <ocx-dispatch>)
 => (text :: <string>);
  let frame = dispatch.ocx-frame;
  unless (frame.my-text = text)
    frame.my-text := text;
    frame.embedded-data-changed? := #t;
  end unless;
  text
end;

define dispatch-interface <button-dispatch> (<ocx-dispatch>)
  name "my-button";
  uuid "{C27B26F4-4CCD-11D1-9A58-006097C90313}";
  virtual property text-property :: <string>,
    name: "Text", disp-id: $DISPID-TEXT;
end;

define coclass $button-type-info
  name "DuimButton";
  uuid "{C27B26F4-4CCD-11D1-9A58-006097C90313}";
  documentation "Dylan button gadget demo";
  interface <button-dispatch>;
end;

// Initialize the application
initialize-ole-control(frame-class: <my-frame>,
		       typeinfo: $button-type-info);
