Module:       Dylan-User
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module xml-dom
  create <DOM-string>,
	 <DOM-timestamp>,
	 <DOM-key>;

  create <DOM-exception>,
	 <hierarchy-request-error>,
	 <index-size-error>,
	 <inuse-attribute-error>,
	 <invalid-access-error>,
	 <invalid-character-error>,
	 <invalid-modification-error>,
	 <invalid-state-error>,
	 <namespace-error>,
	 <no-data-allowed-error>,
	 <no-modification-allowed-error>,
	 <not-found-error>,
	 <not-supported-error>,
	 <string-size-error>,
	 <syntax-error>,
	 <wrong-document-error>,
	 $hierarchy-request-error,
	 $index-size-error,
	 $inuse-attribute-error,
	 $invalid-access-error,
	 $invalid-character-error,
	 $invalid-modification-error,
	 $invalid-state-error,
	 $namespace-error,
	 $no-data-allowed-error,
	 $no-modification-allowed-error,
	 $not-found-error,
	 $not-supported-error,
	 $string-size-error,
	 $syntax-error,
	 $wrong-document-error,
	 exception-code;

  create <DOM-implementation>,
	 create-document-type,	
	 create-document, 
	 has-feature?;

  create <node>,
	 $attribute-node,
	 $cdata-section-node,
	 $comment-node,
	 $document-fragment-node,
	 $document-node,
	 $document-type-node,
	 $element-node,
	 $entity-node,
	 $entity-reference-node,
	 $notation-node,
	 $processing-instruction-node,
	 $text-node,
	 append-child,
	 attributes,
	 child-nodes,
	 compare-document-order,
	 compare-tree-position,
	 clone-node,
	 do-nodes,			// not part of DOM
	 first-child,
	 has-attribute?,
	 has-attributes?,
	 has-child-nodes?,
	 insert-before,
	 is-supported?,
	 last-child,
	 namespace-uri,
	 next-sibling,
	 node-name,
	 node-type,
	 node-value, node-value-setter,
	 normalize,
	 owner-document,
	 parent-node,
	 prefix,
	 previous-sibling,
	 remove-child,
	 replace-child;

  create <document-order>,
	 $document-order-preceding,
	 $document-order-following,
	 $document-order-same,
	 $document-order-unordered,
	 <tree-position>,
	 $tree-position-preceding,
	 $tree-position-following,
	 $tree-position-ancestor,
	 $tree-position-descendant,
	 $tree-position-same,
	 $tree-position-unordered;

  create <node-list>,
	 item,
	 length;

  create <named-node-map>,
	 /* item, */
	 get-named-item,
	 /* length, */
	 remove-named-item,
	 set-named-item;

  create <character-data>,
	 append-data,
	 data, data-setter,
	 delete-data,
	 insert-data,
	 /* length, */
	 replace-data,
	 substring-data;

  create <text>,
	 split-text;

  create <CDATA-section>;

  create <comment>;

  create <document-fragment>;

  create <document>,
	 <xml-document>,		// not part of DOM
	 actual-encoding, actual-encoding-setter,
	 adopt-node,
	 base-uri, base-uri-setter,
	 create-CDATA-section,
	 create-attribute,
	 create-comment,
	 create-document-fragment,
	 create-element,
	 create-entity-reference,
	 create-processing-instruction,
	 create-text-node,
	 create-xml-document,		// not currently part of DOM Level 1
	 do-elements,			// not part of DOM
	 do-elements-by-tag-name,	// not part of DOM
	 doctype,
	 document-element,
	 encoding, encoding-setter,
	 get-element-by-id,
	 get-elements-by-tag-name,
	 implementation,
	 import-node,
	 standalone?, standalone?-setter,
	 strict-error-checking?, strict-error-checking?-setter,
	 version, version-setter;

  create <document-type>,
	 entities, set-entity,
	 internal-subset,
	 name,
	 notations, set-notation,
	 public-id,
	 system-id;

  create <attribute>,
	 default-value, default-value-setter,
	 /* name, */
	 owner-element, owner-element-setter,
	 specified?,
	 value, value-setter;

  create <element>,
	 do-attributes,			// not part of DOM
	 get-attribute,
	 get-attribute-node,
	 /* get-elements-by-tag-name, */
	 remove-attribute,
	 remove-attribute-node,
	 set-attribute,
	 set-attribute-node,
	 tag-name,
	 text;				// not part of DOM

  create <entity>,
	 notation-name,
	 /* public-id, */
	 /* system-id  */;

  create <entity-reference>;

  create <notation>
	 /* public-id, */
	 /* system-id  */ ;

  create <processing-instruction>,
	 /* data, */
	 target;
end module xml-dom;

define module html-dom
  create <html-collection>,
	 /* item, */
	 /* length, */
	 named-item;

  create <html-document>,		// not part of DOM
	 anchors,
	 applets,
	 body,
	 cookie, cookie-setter,
	 create-html-document,		// not currently part of DOM Level 1
	 domain,
	 forms,
	 // get-element-by-id,
	 get-elements-by-name,
	 images,
	 links,
	 referrer,
	 title,
	 url;

  create <html-element>,
	 html/id, html/id-setter;

  // HTML element methods
  //---*** The "html/" in these names is just not right...
  create html/blur,
	 html/click,
	 create-caption,
	 create-tFoot,
	 create-tHead,
	 delete-caption,
	 delete-cell,
	 delete-row,
	 delete-tFoot,
	 delete-tHead,
	 insert-cell,
	 insert-row,
	 html/focus,
	 html/reset,
	 html/select,
	 html/submit;

  // HTML element attributes
  create html/abbr, html/abbr-setter,
	 html/accept, html/accept-setter,
	 html/accept-charset, html/accept-charset-setter,
	 html/access-key, html/access-key-setter,
	 html/action, html/action-setter,
	 html/align, html/align-setter,
	 html/alink, html/alink-setter,
	 html/alt, html/alt-setter,
	 html/archive, html/archive-setter,
	 html/axis, html/axis-setter,
	 html/background, html/background-setter,
	 html/bg-color, html/bg-color-setter,
	 html/border, html/border-setter,
	 html/class, html/class-setter,
	 html/cell-index, html/cell-index-setter,
	 html/cell-padding, html/cell-padding-setter,
	 html/cell-spacing, html/cell-spacing-setter,
	 html/ch, html/ch-setter,
	 html/ch-off, html/ch-off-setter,
	 html/charset, html/charset-setter,
	 html/checked?, html/checked?-setter,
	 html/cite, html/cite-setter,
	 html/clear, html/clear-setter,
	 html/code, html/code-setter,
	 html/code-base, html/code-base-setter,
	 html/code-type, html/code-type-setter,
	 html/col-span, html/col-span-setter,
	 html/color, html/color-setter,
	 html/cols, html/cols-setter,
	 html/compact?, html/compact?-setter,
	 html/content, html/content-setter,
	 html/coords, html/coords-setter,
	 html/data, html/data-setter,
	 html/date-time, html/date-time-setter,
	 html/declare?, html/declare?-setter,
	 html/default-checked?, html/default-checked?-setter,
	 html/default-selected?, html/default-selected?-setter,
	 html/default-value, html/default-value-setter,
	 html/defer?, html/defer?-setter,
	 html/dir, html/dir-setter,
	 html/disabled?, html/disabled?-setter,
	 html/enctype, html/enctype-setter,
	 html/event, html/event-setter,
	 html/face, html/face-setter,
	 html/form,
	 html/frame, html/frame-setter,
	 html/frame-border, html/frame-border-setter,
	 html/headers, html/headers-setter,
	 html/height, html/height-setter,
	 html/href, html/href-setter,
	 html/href-lang, html/href-lang-setter,
	 html/hspace, html/hspace-setter,
	 html/html-for, html/html-for-setter,
	 html/http-equiv, html/http-equiv-setter,
	 html/index, html/index-setter,
	 html/is-map?, html/is-map?-setter,
	 html/label, html/label-setter,
	 html/lang, html/lang-setter,
	 html/link, html/link-setter,
	 html/long-desc, html/long-desc-setter,
	 html/low-src, html/low-src-setter,
	 html/margin-height, html/margin-height-setter,
	 html/margin-width, html/margin-width-setter,
	 html/max-length, html/max-length-setter,
	 html/media, html/media-setter,
	 html/method, html/method-setter,
	 html/multiple?, html/multiple?-setter,
	 html/name, html/name-setter,
	 html/ncols, html/ncols-setter,
	 html/nrows, html/nrows-setter,
	 html/no-href?, html/no-href?-setter,
	 html/no-resize?, html/no-resize?-setter,
	 html/no-shade?, html/no-shade?-setter,
	 html/no-wrap?, html/no-wrap?-setter,
	 html/object, html/object-setter,
	 html/profile, html/profile-setter,
	 html/prompt, html/prompt-setter,
	 html/read-only?, html/read-only?-setter,
	 html/rel, html/rel-setter,
	 html/rev, html/rev-setter,
	 html/row-index, html/row-index-setter,
	 html/row-span, html/row-span-setter,
	 html/rows, html/rows-setter,
	 html/rules, html/rules-setter,
	 html/scheme, html/scheme-setter,
	 html/scope, html/scope-setter,
	 html/scrolling, html/scrolling-setter,
	 html/section-row-index, html/section-row-index-setter,
	 html/selected?, html/selected?,
	 html/selected-index, html/selected-index-setter,
	 html/shape, html/shape-setter,
	 html/size, html/size-setter,
	 html/span, html/span-setter,
	 html/src, html/src-setter,
	 html/standby, html/standby-setter,
	 html/start, html/start-setter,
	 html/summary, html/summary-setter,
	 html/tab-index, html/tab-index-setter,
	 html/target, html/target-setter,
	 html/text, html/text-setter,
	 html/title, html/title-setter,
	 html/type, html/type-setter,
	 html/use-map, html/use-map-setter,
	 html/valign, html/valign-setter,
	 html/value, html/value-setter,
	 html/value-type, html/value-type-setter,
	 html/version, html/version-setter,
	 html/vlink, html/vlink-setter,
	 html/vspace, html/vspace-setter,
	 html/width, html/width-setter;
end module html-dom;

define module dom
  use xml-dom,  export: all;
  use html-dom, export: all;
end module dom;

define module dom-internals
  use functional-dylan,
    exclude: { position, position-if, count };
  use dylan-extensions,
    import: { \without-bounds-checks,
	      element-no-bounds-check,
	      element-no-bounds-check-setter,
	      element-range-error };
  use simple-format;			// for debugging
  use table-extensions;

  use dom, export: all;

  export \inc!, \dec!,
	 \min!, \max!,
	 \push!, \pop!;

  export count,
	 find, find-if,
	 position, position-if,
	 insert-at!, remove-at!;

  export char-equal?, char-less?, char-greater?,
	 string-equal?, string-less?, string-greater?,
	 alpha-char?, digit-char?, alphanumeric-char?,
	 upper-case?, lower-case?,
	 graphic-char?, standard-char?,
	 whitespace-char?, any-whitespace-char?, xml-whitespace?;

  export ncname-start-char?,
	 ncname-char?;

  // Namespace-sensitive property lists for attributes
  export get-property,
	 set-property!,
	 remove-property!;

  // Export a few more things needed by DOM implementors,
  // but which should not be visible to DOM clients
  export $xml-namespace-uri,
	 $xmlns-namespace-uri,
	 <node-name>,
	 copy-string, copy-string-into!,
	 doctype-setter,
	 inherited-attribute,
	 intern-string,
	 make-node-name,
	 namespace-attribute?,
	 node-parent, node-parent-setter,
	 xml-base-uri,
	 xml-language,
	 xml-preserve-space?;
end module dom-internals;
