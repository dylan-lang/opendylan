Module:    OLE-Server
Synopsis:  Persistent storage via the IPersistStorage interface --
	   methods for class <CPersistStorage> and
	   helper functions for <LPSTREAM>.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Name of storage stream for saving the embedded window size.
define constant $size-stream-name :: <LPOLESTR> = as(<LPOLESTR>, "HDWsize");

// Used to give a new OLE object a pointer to its storage.
define method IPersistStorage/InitNew(this :: <CPersistStorage>,
				      storage :: <LPSTORAGE>)
		=> status :: <HRESULT>;

  Output-Debug-String("IPersistStorage/InitNew\r\n");

  let obj :: <ole-server-framework> = this.get-obj;

  returning-error-status
    // release any streams and storages that may be open
    Release-Streams-And-Storage(this);

    this.container-IStorage := storage;
    AddRef(storage);

    // create a stream to save the window size
    this.size-storage-stream := OLE-util-Create-Stream(storage,
						      $size-stream-name);
    // create user streams
    OLE-part-Create-Streams(obj, storage);

    // do any user-defined initialization
    OLE-part-init-new(obj);

  end returning-error-status
end method IPersistStorage/InitNew;

define open generic OLE-part-init-new (obj);

// default method:
define method OLE-part-init-new (obj :: <basic-ole-server>)
 => (status :: <HRESULT>);
  $E-NOTIMPL
end;


define method OLE-util-Create-Stream(storage :: <LPSTORAGE>,
				     name :: <LPOLESTR>,
     #key mode :: <integer> = logior($STGM-READWRITE, $STGM-SHARE-EXCLUSIVE,
				     $STGM-CREATE))
 => stream :: <LPSTREAM>;

  let ( status, stream-interface ) =
    IStorage/CreateStream(storage, name, mode, 0, 0);
  check-ole-status(status, "CreateStream", storage, name);
  pointer-cast(<LPSTREAM>, stream-interface)
end method;

define method OLE-util-open-stream (storage :: <LPSTORAGE>,
				    name :: <LPOLESTR>,
     #key mode :: <integer> = logior($STGM-READWRITE,$STGM-SHARE-EXCLUSIVE))
 => stream :: <LPSTREAM>;

  let ( status, stream-interface ) =
    IStorage/OpenStream(storage, name, $NULL-VOID, mode, 0);
  check-ole-status(status, "OpenStream", storage, name);
  pointer-cast(<LPSTREAM>, stream-interface)
end method;


// Returns the class ID of this object.

define method IPersist/GetClassID(this :: <CPersistStorage>,
				  lpClassID :: <LPCLSID>)
		=> status :: <HRESULT>;

  Output-Debug-String("IPersist/GetClassID\r\n");
  copy-class-ID(this.get-obj, lpClassID)
end method IPersist/GetClassID;


// Instructs the object to save itself into the storage.

define method IPersistStorage/Save(this :: <CPersistStorage>,
				   storage :: <LPSTORAGE>,
				   same-as-load? :: <boolean>)
			=> status :: <HRESULT>;

  Output-Debug-String("IPersistStorage/Save\r\n");

  returning-error-status
    let obj :: <ole-server-framework> = this.get-obj;

    let size-stream :: <LPSTREAM> = $null-istream;
    if ( ~ same-as-load? )
      size-stream := OLE-util-Create-Stream(storage, $size-stream-name);
    else
      size-stream := this.size-storage-stream;
      AddRef(size-stream);
    end if;

    // discard old stream contents and prepare to write from the beginning
    istream-rewrite(size-stream);

    // write the size to the stream
    let ( width, height ) = OLE-util-current-size(obj);
    istream-write-int16(size-stream, width);
    istream-write-int16(size-stream, height);

    // save user data
    OLE-part-Save-To-Storage(obj, storage, same-as-load? );

    Release(size-stream);
    this.save-with-same-as-load? := same-as-load?;
    this.no-scribble-mode? := #t;
  end returning-error-status
end method IPersistStorage/Save;


// Called when the container is finished saving the object.

define method IPersistStorage/SaveCompleted(this :: <CPersistStorage>,
					    new-storage :: <LPSTORAGE>)
		=> status :: <HRESULT>;

  Output-Debug-String("IPersistStorage/SaveCompleted\r\n");

  let obj :: <ole-server-framework> = this.get-obj;

  unless ( null-pointer?(new-storage) )
    Release-Streams-And-Storage(this);
    this.container-IStorage := new-storage;
    AddRef(new-storage);
    this.size-storage-stream := OLE-util-open-stream(new-storage,
						    $size-stream-name);
    OLE-part-Open-Streams(obj, new-storage);
  end unless;

	/* OLE2NOTE: it is only legal to perform a Save or SaveAs operation
	**    on an embedded object. If the document is a file-based document
	**    then we can not be changed to a IStorage-base object.
	**
	**      fSameAsLoad   lpStgNew     Type of Save     Send OnSave
	**    ---------------------------------------------------------
	**         TRUE        NULL        SAVE             YES
	**         TRUE        ! NULL      SAVE *           YES
	**         FALSE       ! NULL      SAVE AS          YES
	**         FALSE       NULL        SAVE COPY AS     NO
	**
	**    * this is a strange case that is possible. it is inefficient
	**    for the caller; it would be better to pass lpStgNew==NULL for
	**    the Save operation.
	*/ 

  if ( ~ null-pointer?(new-storage) | this.save-with-same-as-load? )
    if ( this.no-scribble-mode? & ~ null?(container-IOleAdviseHolder(obj)) )
      IOleAdviseHolder/SendOnSave(container-IOleAdviseHolder(obj));
    end if;  // normally would clear a dirty bit
    this.save-with-same-as-load? := #f;
  end if;

  this.no-scribble-mode? := #f;

  $S-OK
end method IPersistStorage/SaveCompleted;


// Instructs the object to be loaded from storage.

define method IPersistStorage/Load(this :: <CPersistStorage>,
				   storage :: <LPSTORAGE>)
	=> status :: <HRESULT>;

  Output-Debug-String("IPersistStorage/Load\r\n");
  let obj :: <ole-server-framework> = this.get-obj;
  Release(this.container-IStorage);
  this.container-IStorage := storage;
  AddRef(storage);
  returning-error-status
    // open the streams
    Release(this.size-storage-stream);
    this.size-storage-stream := $null-istream; // in case error on following
    let size-stream :: <LPSTREAM> =
      OLE-util-open-stream(storage, $size-stream-name);
    this.size-storage-stream := size-stream;
    OLE-part-Open-Streams(obj, storage);
    // read the size
    obj.embedded-width  := istream-read-int16(size-stream);
    obj.embedded-height := istream-read-int16(size-stream);
    // read the user data
    OLE-part-Load-From-Storage(obj, obj.embedded-width, obj.embedded-height);
  end returning-error-status
end method IPersistStorage/Load;


// Return whether or not the object is dirty with respect to its Storage.

define method IPersistStorage/IsDirty(this :: <CPersistStorage>)
			=> status :: <HRESULT>;
	
  Output-Debug-String("IPersistStorage/IsDirty\r\n");

  // Has the application data changed since being written to storage?
  if ( OLE-part-dirty?(this.get-obj) )
    $S-OK	// Yes.
  else
    $S-FALSE	// No.
  end if
end method IPersistStorage/IsDirty;


// Forces the object to release its handle to its storage.

define method IPersistStorage/HandsOffStorage(this :: <CPersistStorage>)
		=> status :: <HRESULT>;

  Output-Debug-String("IPersistStorage/HandsOffStorage\r\n");

  returning-error-status
    Release-Streams-And-Storage(this);
  end returning-error-status
end method IPersistStorage/HandsOffStorage;


//      Releases the stream and storage pointers

define method Release-Streams-And-Storage (obj :: <ole-server-framework>)
 => ();
  Release-Streams-And-Storage(obj.server-IPersistStorage);
  next-method()
end;

define method Release-Streams-And-Storage(this :: <CPersistStorage>) => ();

  // release size stream
  Release(this.size-storage-stream);
  this.size-storage-stream := $null-istream;

  // release user streams
  OLE-part-Release-Streams(this.get-obj);

  // release the storage
  Release(this.container-IStorage);
  this.container-IStorage := null-pointer(<LPSTORAGE>);

  values()
end method;


//********************************************************************
//	Helper functions for application to use
//********************************************************************

define variable short-ptr :: <C-signed-short*> = make(<C-signed-short*>);
define constant $short-size :: <integer> = size-of(<C-signed-short>);
define constant $long-size :: <integer> = size-of(<C-long>);


define method IStream/Write-int16( stream :: <LPSTREAM>,
				    value :: <integer> )
	=> ( status :: <HRESULT>, bytes :: <integer> );
  pointer-value(short-ptr) := value;
  /* return */ IStream/Write(stream, short-ptr, $short-size );
end;

define method IStream/Read-int16( stream :: <LPSTREAM> )
	=> ( status :: <HRESULT>, value :: <integer>, bytes :: <integer> );
  let ( status, bytes ) =
    IStream/Read(stream, short-ptr, $short-size );
  values( status, pointer-value(short-ptr), bytes );
end;

define method istream-write-int16( stream :: <LPSTREAM>, value :: <integer> )
 => ();
  let ( status, count ) = IStream/Write-int16(stream, value);
  check-istream-status(status, "IStream/Write", stream, count, $short-size);
end method;

define method istream-write-integer( stream :: <LPSTREAM>,
				    value :: <ffi-integer> )
 => ();
  let ( status, count ) = IStream/Write-integer(stream, value);
  check-istream-status(status, "IStream/Write", stream, count, $long-size);
end method;

define method istream-read-integer( stream :: <LPSTREAM> )
 => value :: type-union(<integer>,<machine-word>);
  let ( status, value, count ) = IStream/Read-integer( stream );
  check-istream-status(status, "IStream/Read-integer",
		       stream, count, $long-size);
  value
end method;

define method istream-read-int16( stream :: <LPSTREAM> )
 => value :: <integer>;
  let ( status, value, count ) = IStream/Read-int16( stream );
  check-istream-status(status, "IStream/Read-int16",
		       stream, count, $short-size);
  value
end method;

define inline constant $float-size :: <integer> = size-of(<C-float>);

define method istream-write-float( stream :: <LPSTREAM>,
				   value :: <single-float> )
 => ();
  with-stack-structure( float-ptr :: <C-float*> )
    pointer-value(float-ptr) := value;
    let ( status, count ) = IStream/Write(stream, float-ptr, $float-size );
    check-istream-status(status, "istream-write-float",
			 stream, count, $float-size);
  end with-stack-structure;
  values()
end method;

define method istream-read-float( stream :: <LPSTREAM> )
 => value :: <single-float>;  
  with-stack-structure( float-ptr :: <C-float*> )
    let ( status, count ) = IStream/Read(stream, float-ptr, $float-size );
    check-istream-status(status, "istream-read-float",
			 stream, count, $float-size);
    pointer-value(float-ptr)
  end with-stack-structure
end method;

define method istream-write-string( stream :: <LPSTREAM>,
				    value :: <string> )
 => ();
  with-c-string ( c-string = value )
    let length :: <integer> = size(value);
    istream-write-integer(stream, length);
    if ( length > 0 )
      // write includes the nul byte at the end and the rest of that word.
      let write-length = logand(length + 4, lognot(3));
      let ( status, count ) = IStream/Write(stream, c-string, write-length);
      check-istream-status(status, "istream-write-string",
			   stream, count, write-length);
    end if;
  end with-c-string;
  values()
end method;

define method istream-read-string( stream :: <LPSTREAM> )
 => value :: <string>;  

  let string-length :: <integer> = istream-read-integer(stream);
  if ( string-length = 0 )
    ""
  else
    let read-length :: <integer> = logand(string-length + 4, lognot(3));
    with-stack-structure( c-string :: <C-string>, size: read-length )
      let ( status, count ) = IStream/Read(stream, c-string, read-length);
      check-istream-status(status, "istream-read-string",
			   stream, count, read-length);
      let result = make(<byte-string>, size: string-length);
      for ( i :: <integer> from 0 below string-length )
	result[i] := c-string[i];
      end for;
      result
    end with-stack-structure
  end if
end method;


define constant $NULL-ULARGE-INTEGER = null-pointer(<PULARGE-INTEGER>);

define constant $unsigned-large-zero :: <PULARGE-INTEGER> =
  make( <PULARGE-INTEGER>, value: 0 );

define constant $large-zero :: <PLARGE-INTEGER> =
  make( <PLARGE-INTEGER>, value: 0 );

define method istream-rewrite( stream :: <LPSTREAM> ) => ();
  let status = IStream/SetSize(stream, $unsigned-large-zero);
  check-ole-status(status, "IStream/SetSize", stream);
  status := IStream/Seek(stream, $large-zero,
			 $STREAM-SEEK-SET, $NULL-ULARGE-INTEGER);
  check-ole-status(status, "IStream/Seek", stream);
  values()
end method;

define function check-istream-status (status :: <HRESULT>,
				      name :: <string>,
				      stream :: <LPSTREAM>,
				      actual-count :: <integer>,
				      expected-count :: <integer> ) => ();
  if ( FAILED?(status) )
    ole-error(status, name, stream);
  elseif ( actual-count ~= expected-count )
    if ( actual-count < expected-count )
      // premature EOF
      ole-error(HRESULT-FROM-WIN32($ERROR-HANDLE-EOF), name, stream,
		actual-count, expected-count);
    else // something really unexpected
      error("Buffer over-run in %s", name);
    end if;
  end if;
end;

