Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DOM "Implementation"

//
// interface DOMImplementation {
//   boolean hasFeature(in DOMString feature, 
//                      in DOMString version);
//   DocumentType createDocumentType(in DOMString qualifiedName, 
//                                   in DOMString publicId, 
//                                   in DOMString systemId)
//                                   raises(DOMException);
//   Document     createDocument(in DOMString namespaceURI, 
//                               in DOMString qualifiedName, 
//                               in DocumentType doctype)
//                               raises(DOMException);
// };
//

define sealed class <DOM-implementation> (<object>)
  sealed constant slot %version  = #"1.0";
  sealed constant slot %features = #[#"xml", #"html"];
end class <DOM-implementation>;

define variable $DOM-implementation :: false-or(<DOM-implementation>) = #f;

define sealed method make
    (class == <DOM-implementation>, #key)
 => (implementation :: <DOM-implementation>)
  $DOM-implementation
  | ($DOM-implementation := next-method())
end method make;

define sealed domain make (subclass(<DOM-implementation>));
define sealed domain initialize (<DOM-implementation>);

define sealed method \=
    (impl1 :: <DOM-implementation>, impl2 :: <DOM-implementation>) => (equal? :: <boolean>)
  impl1 == impl2
  | (  impl1.%version  = impl2.%version
     & impl1.%features = impl2.%features)
end method \=;

define sealed method has-feature?
    (implementation :: <DOM-implementation>,
     feature :: <DOM-string>, version :: false-or(<DOM-string>))
 => (has-feature? :: <boolean>)
  (~version | as(<symbol>, version) == implementation.%version)
  & (member?(as(<symbol>, feature), implementation.%features))
end method has-feature?;

define sealed method create-document-type
    (implementation :: <DOM-implementation>,
     qualified-name :: <DOM-string>,
     public-id :: false-or(<DOM-string>), system-id :: false-or(<DOM-string>),
     #key internal-subset :: false-or(<DOM-string>) = #f)
 => (doctype :: <document-type>)
  check-qualified-name(qualified-name);
  make(<document-type>,
       document: #f,
       name: qualified-name,
       public-id: public-id,
       system-id: system-id,
       internal-subset: internal-subset)
end method create-document-type;

define sealed method create-document
    (implementation :: <DOM-implementation>,
     namespace-uri :: false-or(<DOM-string>), qualified-name :: <DOM-string>,
     doctype :: false-or(<document-type>),
     #key  type :: subclass(<document>) = <xml-document>)
 => (document :: <document>)
  check-qualified-name(qualified-name, namespace: namespace-uri);
  let document = make(type, 
		      name: "#document");
  when (doctype)
    when (owner-document(doctype))
      error(make(<wrong-document-error>,
	         format-string: "The document type %= has already been used in the document %=",
	         format-arguments: vector(doctype, owner-document(doctype))))
    end;
    owner-document(doctype) := document;
    append-child(document, doctype)
  end;
  append-child(document,
	       create-element(document, qualified-name, namespace: namespace-uri));
  document
end method create-document;

define function check-qualified-name
    (qualified-name :: <DOM-string>, #key namespace :: false-or(<DOM-string>)) => ()
  when (#f)		//---*** what should the error check be?
    error(make(<invalid-character-error>,
	       format-string: "The qualified name %s contains an invalid character",
	       format-arguments: vector(qualified-name)))
  end;
  when (#f)		//---*** what should the error check be?
    error(make(<namespace-error>,
	       format-string: "The name %s has a namespace error with respect to %s",
	       format-arguments: vector(qualified-name, namespace)))
  end;
end function check-qualified-name;
