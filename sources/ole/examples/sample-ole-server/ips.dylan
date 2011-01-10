Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//**********************************************************************
//
// CPersistStorage::InitNew
//
// Purpose:
//
//      Used to give a new OLE object a ptr to its storage.
//
// Parameters:
//
//      LPSTORAGE pStg  - Pointer to the storage
//
// Return Value:
//
//
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IStorage::Release           OLE
//      IStorage::AddRef            OLE
//
//
// Comments:
//
//
//********************************************************************


define method IPersistStorage/InitNew(this :: <CPersistStorage>,
				      pStg :: <LPSTORAGE>)
		=> status :: <HRESULT>;

  OutputDebugString("In CPersistStorage::InitNew\r\n");

  // release any streams and storages that may be open
  ReleaseStreamsAndStorage(this);

  this.m-lpObj.m-lpStorage := pStg;

  // AddRef the new Storage
  let storage = this.m-lpObj.m-lpStorage;
  unless ( null?(storage) )
    AddRef(storage);
  end unless;

  CreateStreams(this, storage);

  $S-OK

end method IPersistStorage/InitNew;

//**********************************************************************
//
// CPersistStorage::GetClassID
//
// Purpose:
//
//      Returns the CLSID of this object.
//
// Parameters:
//
//      LPCLSID lpClassID   - Out ptr in which to return the CLSID
//
// Return Value:
//
//       S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
//
// Comments:
//
//
//********************************************************************


define method IPersist/GetClassID(this :: <CPersistStorage>,
				  lpClassID :: <LPCLSID>)
		=> status :: <HRESULT>;

  OutputDebugString("In CPersistStorage::GetClassID\r\n");

  copy-into!(lpClassID, $GUID-SIMPLE, size-of(<CLSID>));

  $S-OK
end method IPersist/GetClassID;

//**********************************************************************
//
// CPersistStorage::Save
//
// Purpose:
//
//      Instructs the object to save itself into the storage.
//
// Parameters:
//
//      LPSTORAGE pStgSave  - Storage in which the object should be saved
//
//      BOOL fSameAsLoad    - TRUE if pStgSave is the same as the storage
//                            that the object was originally created with.
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CPersistStorage::InitNew    IPS.CPP
//      CSimpSvrObj::SaveToStorage  OBJ.CPP
//
//
// Comments:
//
//      A real app will want better error checking in this method.
//
//********************************************************************


define method IPersistStorage/Save(this :: <CPersistStorage>,
				   pStgSave :: <LPSTORAGE>,
				   fSameAsLoad :: <boolean>)
			=> status :: <HRESULT>;

  OutputDebugString("In CPersistStorage::Save\r\n");

  // save the data
  let obj :: <CSimpSvrObj> = this.m-lpObj;
  SaveToStorage(obj, pStgSave, fSameAsLoad );

  obj.m-fSaveWithSameAsLoad := fSameAsLoad;
  obj.m-fNoScribbleMode := #t;

  $S-OK
end method IPersistStorage/Save;

//**********************************************************************
//
// CPersistStorage::SaveCompleted
//
// Purpose:
//
//      Called when the container is finished saving the object
//
// Parameters:
//
//      LPSTORAGE pStgNew   - ptr to the new storage
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
//
// Comments:
//
//
//********************************************************************


define method IPersistStorage/SaveCompleted(this :: <CPersistStorage>,
					    pStgNew :: <LPSTORAGE>)
		=> status :: <HRESULT>;

  OutputDebugString("In CPersistStorage::SaveCompleted\r\n");

  let lpObj :: <CSimpSvrObj> = this.m-lpObj;

  unless ( null?( pStgNew ) )
    ReleaseStreamsAndStorage(this);
    lpObj.m-lpStorage := pStgNew;
    AddRef(lpObj.m-lpStorage);
    OpenStreams(this, pStgNew);
  end unless;


	/* OLE2NOTE: it is only legal to perform a Save or SaveAs operation
	**    on an embedded object. if the document is a file-based document
	**    then it cannot be changed to a IStorage-base object.
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

  if ( ~ null?(pStgNew) | (lpObj.m-fSaveWithSameAsLoad) )
		
    if ( lpObj.m-fNoScribbleMode )
      IOleAdviseHolder/SendOnSave(GetOleAdviseHolder(lpObj));
    end if;  // normally would clear a dirty bit
    lpObj.m-fSaveWithSameAsLoad := #f;
  end if;

  lpObj.m-fNoScribbleMode := #f;

  $S-OK
end method IPersistStorage/SaveCompleted;

//**********************************************************************
//
// CPersistStorage::Load
//
// Purpose:
//
//      Instructs the object to be loaded from storage.
//
// Parameters:
//
//      LPSTORAGE pStg  - Ptr to the storage in which to be loaded
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                        Location
//
//      OutputDebugString               Windows API
//      CSimpSvrObj::LoadFromStorage    OBJ.CPP
//
//
// Comments:
//
//      A real app will want better error checking in this method.
//
//********************************************************************


define method IPersistStorage/Load(this :: <CPersistStorage>,
				   pStg :: <LPSTORAGE>)
	=> status :: <HRESULT>;

  OutputDebugString("In CPersistStorage::Load\r\n");
  let lpObj :: <CSimpSvrObj> = this.m-lpObj;
  let old-storage :: <Interface> = lpObj.m-lpStorage;

  // remember the storage
  unless ( null?( old-storage ) )
    Release(old-storage);
  end unless;

  lpObj.m-lpStorage := pStg;

  AddRef(pStg);

  OpenStreams(this, pStg);

  LoadFromStorage(lpObj);


 $S-OK
end method IPersistStorage/Load;


//**********************************************************************
//
// CPersistStorage::IsDirty
//
// Purpose:
//
//      Returns whether or not the object is dirty w/respect to its
//      Storage
//
// Parameters:
//
//      None
//
// Return Value:
//
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
//
// Comments:
//
//      This sample does not implement this function, although a
//      real application should.
//
//********************************************************************


define method IPersistStorage/IsDirty(this :: <CPersistStorage>)
			=> status :: <HRESULT>;
	
  OutputDebugString("In CPersistStorage::IsDirty\r\n");
  $S-OK
end method IPersistStorage/IsDirty;

//**********************************************************************
//
// CPersistStorage::HandsOffStorage
//
// Purpose:
//
//      Forces the object to release its handle to its storage.
//
// Parameters:
//
//      None
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IStorage::Release           OLE
//
// Comments:
//
//
//********************************************************************


define method IPersistStorage/HandsOffStorage(this :: <CPersistStorage>)
		=> status :: <HRESULT>;

  OutputDebugString("In CPersistStorage::HandsOffStorage\r\n");

  ReleaseStreamsAndStorage(this);

  $S-OK
end method IPersistStorage/HandsOffStorage;

//**********************************************************************
//
// CPersistStorage::CreateStreams
//
// Purpose:
//
//      Creates the streams that are held open for the object's lifetime.
//
// Parameters:
//
//      LPSTORAGE lpStg -   Storage in which to create the streams
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IStorage::Release           OLE
//      IStream::Release            OLE
//      IStorage::CreateStream      OLE
//
// Comments:
//
//
//********************************************************************


define method CreateStreams(this :: <CPersistStorage>, lpStg :: <LPSTORAGE>)
	=> ();

  let lpObj :: <CSimpSvrObj> = this.m-lpObj;	
  unless ( null?( lpObj.m-lpColorStm ) )
    Release(lpObj.m-lpColorStm);
  end unless;

  unless ( null?( lpObj.m-lpSizeStm ) )
    Release(lpObj.m-lpSizeStm);
  end unless;

  // create a stream to save the colors
  let ( status, ppstm ) =
    IStorage/CreateStream(lpStg, OLESTR("RGB"),
		       logior($STGM-READWRITE, $STGM-SHARE-EXCLUSIVE,
			      $STGM-CREATE),
		       0, 0);
  lpObj.m-lpColorStm := pointer-cast(<LPSTREAM>, ppstm);

  // create a stream to save the size
  let ( status, ppstm ) =
    IStorage/CreateStream(lpStg, OLESTR("size"),
		       logior($STGM-READWRITE, $STGM-SHARE-EXCLUSIVE,
			      $STGM-CREATE),
		       0, 0);
  lpObj.m-lpSizeStm := pointer-cast(<LPSTREAM>, ppstm);
 values()
end method CreateStreams;

//**********************************************************************
//
// CPersistStorage::OpenStreams
//
// Purpose:
//
//      Opens the streams in a storage.
//
// Parameters:
//
//      LPSTORAGE lpStg -   Storage in which to open the streams.
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IStorage::Release           OLE
//
// Comments:
//
//
//********************************************************************


define method OpenStreams(this :: <CPersistStorage>, lpStg :: <LPSTORAGE>)
			 => ();

  let lpObj :: <CSimpSvrObj> = this.m-lpObj;	
  unless ( null?( lpObj.m-lpColorStm ) )
    Release(lpObj.m-lpColorStm);
  end unless;

  unless ( null?( lpObj.m-lpSizeStm ) )
    Release(lpObj.m-lpSizeStm);
  end unless;

  // open the color stream
  let ( status, ppstm ) =
    IStorage/OpenStream(lpStg, OLESTR("RGB"), $NULL-VOID,
		     logior($STGM-READWRITE,$STGM-SHARE-EXCLUSIVE),
		     0);
  lpObj.m-lpColorStm := pointer-cast(<LPSTREAM>, ppstm);

  // open the color stream
  let ( status, ppstm ) =
    IStorage/OpenStream(lpStg, OLESTR("size"), $NULL-VOID,
		     logior($STGM-READWRITE,$STGM-SHARE-EXCLUSIVE),
		     0);
  lpObj.m-lpSizeStm := pointer-cast(<LPSTREAM>, ppstm);
  values();
end method OpenStreams;

//**********************************************************************
//
// CPersistStorage::ReleaseStreamsAndStorage
//
// Purpose:
//
//      Releases the stream and storage ptrs
//
// Parameters:
//
//      None
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IStorage::Release           OLE
//
// Comments:
//
//
//********************************************************************


define method ReleaseStreamsAndStorage(this :: <CPersistStorage>) => ();

  let lpObj :: <CSimpSvrObj> = this.m-lpObj;	

  unless ( null?( lpObj.m-lpColorStm ) )
    Release(lpObj.m-lpColorStm);
    lpObj.m-lpColorStm := null-pointer(<LPSTREAM>);
  end unless;

  unless ( null?( lpObj.m-lpSizeStm ) )
    Release(lpObj.m-lpSizeStm);
    lpObj.m-lpSizeStm := null-pointer(<LPSTREAM>);
  end unless;

  unless ( null?( lpObj.m-lpStorage ) )
    Release(lpObj.m-lpStorage);
    lpObj.m-lpStorage := null-pointer(<LPSTORAGE>);
  end unless;

  values()
end method;

//**********************************************************************
//
// CPersistStorage::CreateStreams
//
// Purpose:
//
//      Creates temporary streams in a storage.
//
// Parameters:
//
//      LPSTORAGE lpStg                 - Pointer to the storage
//
//      LPSTREAM FAR* lplpTempColor     - Color Stream
//
//      LPSTREAM FAR* lplpTempSize      - Size Stream
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IStorage::Release           OLE
//
// Comments:
//
//
//********************************************************************


define method CreateTempStreams(this :: <CPersistStorage>,
				lpStg :: <LPSTORAGE>)
			 => ( color-stream :: <LPSTREAM>,
			      size-stream :: <LPSTREAM> );

  // create a stream to save the colors
  let ( status, color-interface ) =
    IStorage/CreateStream(lpStg, OLESTR("RGB"),
		       logior($STGM-READWRITE, $STGM-SHARE-EXCLUSIVE,
			      $STGM-CREATE),
		       0, 0);
  let color-stream = pointer-cast(<LPSTREAM>, color-interface);

  // create a stream to save the size
  let ( status, size-interface ) =
    IStorage/CreateStream(lpStg, OLESTR("size"),
		       logior($STGM-READWRITE, $STGM-SHARE-EXCLUSIVE,
			      $STGM-CREATE),
		       0, 0);
  let size-stream = pointer-cast(<LPSTREAM>, size-interface);

  values( color-stream, size-stream )
end method CreateTempStreams;
