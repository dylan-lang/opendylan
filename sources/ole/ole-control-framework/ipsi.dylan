Module:    OLE-Control-Framework
Synopsis:  Persistent storage using IPersistStreamInit
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open generic OLE-part-max-save-size(obj) => (max-size);
define open generic OLE-part-Load-From-Stream(obj, stream) => ();
define open generic OLE-part-Save-To-Stream(obj, stream) => ();

define COM-interface <CPersistStreamInit> ( <IPersistStreamInit> )

  constant slot get-obj :: <ole-control-framework>,
		required-init-keyword: server-framework: ;
  slot storage-state :: <symbol> = #"created";
end;

define method initialize ( This :: <CPersistStreamInit>, #rest ignore, #key )
  => ();

  Output-Debug-String("initialize(<CPersistStreamInit>)\r\n");

  next-method();
  // Also supports the IPersistStream interface as well.
  // The same Dylan methods here should work for both; the only drawback
  // is the possibility that neither the InitNew nor Load methods
  // would get called.
  add-interface(This, $IID-IPersistStream);
end initialize;

define method IPersist/GetClassID(this :: <CPersistStreamInit>,
				  lpClassID :: <LPCLSID>)
		=> status :: <HRESULT>;
  Output-Debug-String("IPersist/GetClassID\r\n");
  copy-class-ID(this.get-obj, lpClassID)
end method IPersist/GetClassID;

// Find out how big the stream needs to be.
define method IPersistStreamInit/GetSizeMax
    (this :: <CPersistStreamInit>, max-size-ptr :: <PULARGE-INTEGER>)
 => (status :: <HRESULT>)
  Output-Debug-String("IPersistStreamInit/GetSizeMax\r\n");

  let max-size = OLE-part-max-save-size(this);
  if ( max-size == #f )
    $E-NOTIMPL    
  else
    pointer-value(max-size-ptr) := max-size;
    $S-OK
  end if
end method IPersistStreamInit/GetSizeMax;


// default method, should be overridden by user:
define method OLE-part-max-save-size ( obj :: <ole-control-framework> )
 => (size);
  #f
end method;

// Informs the object that it is newly created, not loaded from storage.
define method IPersistStreamInit/InitNew (this :: <CPersistStreamInit>)
 => (status :: <HRESULT>)

  Output-Debug-String("IPersistStreamInit/InitNew\r\n");

  if ( this.storage-state == #"created" )
    // this.storage-state := #"new";
    let obj = this.get-obj;
    OLE-part-dirty?(obj) := #t;
    OLE-part-init-new(obj) | $S-OK
  else
    $E-UNEXPECTED
  end if
end method IPersistStreamInit/InitNew;


// Instructs the object to save itself into the storage.

define method IPersistStreamInit/Save (this :: <CPersistStreamInit>,
				       stream :: <LPSTREAM>,
				       clear-dirty? :: <boolean>)
 => (status :: <HRESULT>)

  Output-Debug-String("IPersistStreamInit/Save\r\n");

  returning-error-status
    // save user data
    let obj = this.get-obj;
    OLE-part-Save-To-Stream(obj, stream);
    this.storage-state := #"saved";
    if ( clear-dirty? )
      OLE-part-dirty?(obj) := #f;
    end if;
  end returning-error-status
end method IPersistStreamInit/Save;


// Instructs the object to be loaded from storage.

define method IPersistStreamInit/Load (this :: <CPersistStreamInit>,
				       stream :: <LPSTREAM>)
 => (status :: <HRESULT>)

  Output-Debug-String("IPersistStreamInit/Load\r\n");

  if ( this.storage-state == #"created" )
    returning-error-status
      let obj = this.get-obj;
      // might need to do a seek here???
      OLE-part-Load-From-Stream(obj, stream);
      this.storage-state := #"loaded";
      OLE-part-dirty?(obj) := #f;
    end returning-error-status
  else
    $E-UNEXPECTED
  end if
end method IPersistStreamInit/Load;


// Return whether or not the object is dirty with respect to its Storage.

define method IPersistStreamInit/IsDirty(this :: <CPersistStreamInit>)
 => (status :: <HRESULT>)
	
  Output-Debug-String("IPersistStreamInit/IsDirty\r\n");

  // Has the application data changed since being written to storage?
  if ( OLE-part-dirty?(this.get-obj) )
    $S-OK	// Yes.
  else
    $S-FALSE	// No.
  end if
end method IPersistStreamInit/IsDirty;
