Module:    OLE-Server
Synopsis:  Additional methods for in-process version of 
	   persistent storage via the IPersistStorage interface.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define COM-interface <inproc-PersistStorage> ( <CPersistStorage> )
  slot default-IPersistStorage :: <LPPERSISTSTORAGE> = $null-interface;
end <inproc-PersistStorage>;


define method initialize ( this :: <inproc-PersistStorage>,
			  #rest args, #key) => ();
  next-method();
  this.default-IPersistStorage :=
    pointer-cast(<LPPERSISTSTORAGE>, 
		 query-cache-interface(this.get-obj, $IID-IPersistStorage));
end method initialize;

define method terminate (this :: <inproc-PersistStorage>) => ();
  next-method();
  unless ( null-pointer?(this.default-IPersistStorage) )
    AddRef(this.get-obj); // because following Release will decrement it.
    Release(this.default-IPersistStorage);
    this.default-IPersistStorage := $null-interface;
  end unless;
  values();
end method terminate;


define method IPersistStorage/InitNew(this :: <inproc-PersistStorage>,
				      storage :: <LPSTORAGE>)
		=> status :: <HRESULT>;

  let result = next-method();
  if ( FAILED?(result) )
    result
  else
    // Initialize the cache:
    IPersistStorage/InitNew(this.default-IPersistStorage, storage)
  end if
end method IPersistStorage/InitNew;


define method IPersistStorage/Save(this :: <inproc-PersistStorage>,
				   storage :: <LPSTORAGE>,
				   same-as-load? :: <boolean>)
			=> status :: <HRESULT>;

  // It doesn't seem right that same-as-load? should be true when neither
  // IPersistStorage/InitNew nor IPersistStorage/Load has been called,
  // but the VC++ OLE Control Test Container does that then a control
  // is copied.  So we'll try to compensate for its confusion.
  let same? :: <boolean> = same-as-load? & ~ null?(this.container-IStorage);
  let result = next-method(this, storage, same?);
  if ( FAILED?(result) )
    result
  else
    // We also need to tell the cache to save cached graphics.
    IPersistStorage/Save(this.default-IPersistStorage, storage, same?)
  end if
end method IPersistStorage/Save;


define method IPersistStorage/SaveCompleted(this :: <inproc-PersistStorage>,
					    new-storage :: <LPSTORAGE>)
		=> status :: <HRESULT>;

  let result = next-method();
  if ( FAILED?(result) )
    result
  else
    IPersistStorage/SaveCompleted(this.default-IPersistStorage, new-storage)
  end if
end method IPersistStorage/SaveCompleted;


define method IPersistStorage/Load(this :: <inproc-PersistStorage>,
				   storage :: <LPSTORAGE>)
	=> status :: <HRESULT>;

  let result = next-method();
  if ( FAILED?(result) )
    result
  else
    // We also need to tell the cache to load cached graphics.
    IPersistStorage/Load(this.default-IPersistStorage, storage)
  end if
end method IPersistStorage/Load;


define method IPersistStorage/HandsOffStorage(this :: <inproc-PersistStorage>)
		=> status :: <HRESULT>;

  let result = next-method();
  if ( FAILED?(result) )
    result
  else
    IPersistStorage/HandsOffStorage(this.default-IPersistStorage)
  end if
end method IPersistStorage/HandsOffStorage;
