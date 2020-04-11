Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2018 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <llvm-debug-info-metadata> (<llvm-metadata>)
  constant slot llvm-metadata-distinct? :: <boolean>,
    init-keyword: distinct?:, init-value: #f;
end class;

define method metadata-partition-key
    (metadata :: <llvm-debug-info-metadata>)
 => (key :: <vector>);
  if (metadata.llvm-metadata-distinct?)
    vector(metadata)
  else
    vector(object-class(metadata))
  end if
end method;

define constant <llvm-debug-emission-kind>
  = one-of(#"no-debug", #"full-debug", #"line-tables-only",
           #"debug-directives-only");

define class <llvm-DICompileUnit-metadata> (<llvm-debug-info-metadata>)
  constant slot llvm-DICompileUnit-metadata-language :: <integer>,
    required-init-keyword: language:;
  constant slot llvm-DICompileUnit-metadata-file :: <llvm-metadata>,
    required-init-keyword: file:;
  constant slot llvm-DICompileUnit-metadata-producer
      :: false-or(<llvm-metadata-string>),
    init-value: #f, init-keyword: producer:;
  constant slot llvm-DICompileUnit-metadata-optimized? :: <boolean>,
    init-value: #f, init-keyword: isOptimized:;
  constant slot llvm-DICompileUnit-metadata-flags
      :: false-or(<llvm-metadata-string>),
    init-value: #f, init-keyword: flags:;
  constant slot llvm-DICompileUnit-metadata-runtime-version :: <integer>,
    init-value: 0, init-keyword: runtimeVersion:;
  constant slot llvm-DICompileUnit-metadata-emission-kind :: <llvm-debug-emission-kind>,
    init-value: #"no-debug", init-keyword: emissionKind:;
  constant slot llvm-DICompileUnit-metadata-enums :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: enums:;
  constant slot llvm-DICompileUnit-retained-types :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: retainedTypes:;
  constant slot llvm-DICompileUnit-metadata-split-debug-inlining? :: <boolean>,
    init-value: #t, init-keyword: splitDebugInlining:;
end class;

define method metadata-partition-key
    (metadata :: <llvm-DICompileUnit-metadata>)
 => (key :: <vector>);
  if (metadata.llvm-metadata-distinct?)
    vector(metadata)
  else
    vector(object-class(metadata),
           metadata.llvm-DICompileUnit-metadata-language,
           metadata.llvm-DICompileUnit-metadata-optimized?,
           metadata.llvm-DICompileUnit-metadata-runtime-version,
           metadata.llvm-DICompileUnit-metadata-emission-kind,
           metadata.llvm-DICompileUnit-metadata-split-debug-inlining?)
  end if
end method;

define method metadata-referenced-metadata
    (metadata :: <llvm-DICompileUnit-metadata>)
 => (referenced :: <vector>);
  choose(true?,
         vector(metadata.llvm-DICompileUnit-metadata-file,
                metadata.llvm-DICompileUnit-metadata-producer,
                metadata.llvm-DICompileUnit-metadata-flags,
                metadata.llvm-DICompileUnit-metadata-enums,
                metadata.llvm-DICompileUnit-retained-types))
end method;

define class <llvm-DIBasicType-metadata> (<llvm-debug-info-metadata>)
  constant slot llvm-DIBasicType-metadata-tag :: <integer>,
    init-value: $DW-TAG-base-type, init-keyword: tag:;
  constant slot llvm-DIBasicType-metadata-name :: false-or(<llvm-metadata-string>),
    init-value: #f, init-keyword: name:;
  constant slot llvm-DIBasicType-metadata-size :: <integer>,
    init-value: 0, init-keyword: size:;
  constant slot llvm-DIBasicType-metadata-encoding :: <integer>,
    init-value: 0, init-keyword: encoding:;
end class;

define method metadata-partition-key
    (metadata :: <llvm-DIBasicType-metadata>)
 => (key :: <vector>);
  if (metadata.llvm-metadata-distinct?)
    vector(metadata)
  else
    vector(object-class(metadata),
           metadata.llvm-DIBasicType-metadata-tag,
           metadata.llvm-DIBasicType-metadata-size,
           metadata.llvm-DIBasicType-metadata-encoding)
  end if
end method;

define method metadata-referenced-metadata
    (metadata :: <llvm-DIBasicType-metadata>)
 => (referenced :: <vector>);
  choose(true?,
         vector(metadata.llvm-DIBasicType-metadata-name))
end method;

define class <llvm-DICompositeType-metadata> (<llvm-debug-info-metadata>)
  constant slot llvm-DICompositeType-metadata-tag :: <integer>,
    required-init-keyword: tag:;
  constant slot llvm-DICompositeType-metadata-name :: false-or(<llvm-metadata-string>),
    init-value: #f, init-keyword: name:;
  constant slot llvm-DICompositeType-metadata-file :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: file:;
  constant slot llvm-DICompositeType-metadata-line :: <integer>,
    init-value: 0, init-keyword: line:;
  constant slot llvm-DICompositeType-metadata-scope :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: scope:;
  constant slot llvm-DICompositeType-metadata-base-type :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: baseType:;
  constant slot llvm-DICompositeType-metadata-size :: <integer>,
    init-value: 0, init-keyword: size:;
  constant slot llvm-DICompositeType-metadata-elements :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: elements:;
end class;

define method metadata-partition-key
    (metadata :: <llvm-DICompositeType-metadata>)
 => (key :: <vector>);
  if (metadata.llvm-metadata-distinct?)
    vector(metadata)
  else
    vector(object-class(metadata),
           metadata.llvm-DICompositeType-metadata-tag,
           metadata.llvm-DICompositeType-metadata-line,
           metadata.llvm-DICompositeType-metadata-size)
  end if
end method;

define method metadata-referenced-metadata
    (metadata :: <llvm-DICompositeType-metadata>)
 => (referenced :: <vector>);
  choose(true?,
         vector(metadata.llvm-DICompositeType-metadata-name,
                metadata.llvm-DICompositeType-metadata-file,
                metadata.llvm-DICompositeType-metadata-scope,
                metadata.llvm-DICompositeType-metadata-base-type,
                metadata.llvm-DICompositeType-metadata-elements))
end method;

define class <llvm-DIDerivedType-metadata> (<llvm-debug-info-metadata>)
  constant slot llvm-DIDerivedType-metadata-tag :: <integer>,
    required-init-keyword: tag:;
  constant slot llvm-DIDerivedType-metadata-name :: false-or(<llvm-metadata-string>),
    init-value: #f, init-keyword: name:;
  constant slot llvm-DIDerivedType-metadata-file :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: file:;
  constant slot llvm-DIDerivedType-metadata-line :: <integer>,
    init-value: 0, init-keyword: line:;
  constant slot llvm-DIDerivedType-metadata-scope :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: scope:;
  constant slot llvm-DIDerivedType-metadata-base-type :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: baseType:;
  constant slot llvm-DIDerivedType-metadata-size :: <integer>,
    init-value: 0, init-keyword: size:;
  constant slot llvm-DIDerivedType-metadata-align :: <integer>,
    init-value: 0, init-keyword: align:;
  constant slot llvm-DIDerivedType-metadata-offset :: <integer>,
    init-value: 0, init-keyword: offset:;
  constant slot llvm-DIDerivedType-metadata-flags :: <integer>,
    init-value: 0, init-keyword: flags:;
  constant slot llvm-DIDerivedType-metadata-extra-data :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: extraData:;
end class;

define method metadata-partition-key
    (metadata :: <llvm-DIDerivedType-metadata>)
 => (key :: <vector>);
  if (metadata.llvm-metadata-distinct?)
    vector(metadata)
  else
    vector(object-class(metadata),
           metadata.llvm-DIDerivedType-metadata-tag,
           metadata.llvm-DIDerivedType-metadata-line,
           metadata.llvm-DIDerivedType-metadata-size,
           metadata.llvm-DIDerivedType-metadata-align,
           metadata.llvm-DIDerivedType-metadata-offset,
           metadata.llvm-DIDerivedType-metadata-flags)
  end if
end method;

define method metadata-referenced-metadata
    (metadata :: <llvm-DIDerivedType-metadata>)
 => (referenced :: <vector>);
  choose(true?,
         vector(metadata.llvm-DIDerivedType-metadata-name,
                metadata.llvm-DIDerivedType-metadata-file,
                metadata.llvm-DIDerivedType-metadata-scope,
                metadata.llvm-DIDerivedType-metadata-base-type,
                metadata.llvm-DIDerivedType-metadata-extra-data))
end method;

define class <llvm-DIExpression-metadata> (<llvm-debug-info-metadata>)
  // FIXME
end class;

define class <llvm-DIFile-metadata> (<llvm-debug-info-metadata>)
  constant slot llvm-DIFile-metadata-filename :: false-or(<llvm-metadata-string>),
    required-init-keyword: filename:;
  constant slot llvm-DIFile-metadata-directory :: false-or(<llvm-metadata-string>),
    required-init-keyword: directory:;
end class;

define method metadata-referenced-metadata
    (metadata :: <llvm-DIFile-metadata>)
 => (referenced :: <vector>);
  choose(true?,
         vector(metadata.llvm-DIFile-metadata-filename,
                metadata.llvm-DIFile-metadata-directory))
end method;

define class <llvm-DILexicalBlock-metadata> (<llvm-debug-info-metadata>)
  constant slot llvm-DILexicalBlock-metadata-scope :: <llvm-metadata>,
    required-init-keyword: scope:;
  constant slot llvm-DILexicalBlock-metadata-file :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: file:;
  constant slot llvm-DILexicalBlock-metadata-line :: <integer>,
    init-value: 0, init-keyword: line:;
  constant slot llvm-DILexicalBlock-metadata-column :: <integer>,
    init-value: 0, init-keyword: column:;
end class;

define method metadata-partition-key
    (metadata :: <llvm-DILexicalBlock-metadata>)
 => (key :: <vector>);
  if (metadata.llvm-metadata-distinct?)
    vector(metadata)
  else
    vector(object-class(metadata),
           metadata.llvm-DILexicalBlock-metadata-line,
           metadata.llvm-DILexicalBlock-metadata-column)
  end if
end method;

define method metadata-referenced-metadata
    (metadata :: <llvm-DILexicalBlock-metadata>)
 => (referenced :: <vector>);
  choose(true?,
         vector(metadata.llvm-DILexicalBlock-metadata-scope,
                metadata.llvm-DILexicalBlock-metadata-file))
end method;

define class <llvm-DILocalVariable-metadata> (<llvm-debug-info-metadata>)
  constant slot llvm-DILocalVariable-metadata-scope :: <llvm-metadata>,
    init-value: #f, init-keyword: scope:;
  constant slot llvm-DILocalVariable-metadata-name :: false-or(<llvm-metadata-string>),
    init-value: #f, init-keyword: name:;
  constant slot llvm-DILocalVariable-metadata-arg :: <integer>,
    init-value: 0, init-keyword: arg:;
  constant slot llvm-DILocalVariable-metadata-file :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: file:;
  constant slot llvm-DILocalVariable-metadata-line :: <integer>,
    init-value: 0, init-keyword: line:;
  constant slot llvm-DILocalVariable-metadata-type :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: type:;
  constant slot llvm-DILocalVariable-metadata-flags :: <integer>,
    init-value: 0, init-keyword: flags:;
  constant slot llvm-DILocalVariable-metadata-align :: <integer>,
    init-value: 0, init-keyword: align:;
end class;

define method metadata-partition-key
    (metadata :: <llvm-DILocalVariable-metadata>)
 => (key :: <vector>);
  if (metadata.llvm-metadata-distinct?)
    vector(metadata)
  else
    vector(object-class(metadata),
           metadata.llvm-DILocalVariable-metadata-arg,
           metadata.llvm-DILocalVariable-metadata-line,
           metadata.llvm-DILocalVariable-metadata-flags,
           metadata.llvm-DILocalVariable-metadata-align)
  end if
end method;

define method metadata-referenced-metadata
    (metadata :: <llvm-DILocalVariable-metadata>)
 => (referenced :: <vector>);
  choose(true?,
         vector(metadata.llvm-DILocalVariable-metadata-scope,
                metadata.llvm-DILocalVariable-metadata-name,
                metadata.llvm-DILocalVariable-metadata-file,
                metadata.llvm-DILocalVariable-metadata-type))
end method;

define class <llvm-DILocation-metadata> (<llvm-debug-info-metadata>)
  constant slot llvm-DILocation-metadata-line :: <integer>,
    init-value: 0, init-keyword: line:;
  constant slot llvm-DILocation-metadata-column :: <integer>,
    init-value: 0, init-keyword: column:;
  constant slot llvm-DILocation-metadata-scope :: <llvm-metadata>,
    required-init-keyword: scope:;
  constant slot llvm-DILocation-metadata-inlined-at :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: inlinedAt:;
end class;

define method metadata-partition-key
    (metadata :: <llvm-DILocation-metadata>)
 => (key :: <vector>);
  if (metadata.llvm-metadata-distinct?)
    vector(metadata)
  else
    vector(object-class(metadata),
           metadata.llvm-DILocation-metadata-line,
           metadata.llvm-DILocation-metadata-column)
  end if
end method;

define method metadata-referenced-metadata
    (metadata :: <llvm-DILocation-metadata>)
 => (referenced :: <vector>);
  let at = metadata.llvm-DILocation-metadata-inlined-at;
  if (at)
    vector(metadata.llvm-DILocation-metadata-scope, at)
  else
    vector(metadata.llvm-DILocation-metadata-scope)
  end if
end method;

define class <llvm-DISubprogram-metadata> (<llvm-debug-info-metadata>)
  constant slot llvm-DISubprogram-metadata-scope :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: scope:;
  constant slot llvm-DISubprogram-metadata-name :: false-or(<llvm-metadata-string>),
    init-value: #f, init-keyword: name:;
  constant slot llvm-DISubprogram-metadata-linkage-name :: false-or(<llvm-metadata-string>),
    init-value: #f, init-keyword: linkageName:;
  constant slot llvm-DISubprogram-metadata-file :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: file:;
  constant slot llvm-DISubprogram-metadata-line :: <integer>,
    init-value: 0, init-keyword: line:;
  constant slot llvm-DISubprogram-metadata-type :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: type:;
  constant slot llvm-DISubprogram-metadata-local? :: <boolean>,
    init-value: #f, init-keyword: isLocal:;
  constant slot llvm-DISubprogram-metadata-definition? :: <boolean>,
    init-value: #t, init-keyword: isDefinition:;
  constant slot llvm-DISubprogram-metadata-scope-line :: <integer>,
    init-value: 0, init-keyword: scopeLine:;
  constant slot llvm-DISubprogram-metadata-flags :: <integer>,
    init-value: 0, init-keyword: flags:;
  constant slot llvm-DISubprogram-metadata-optimized? :: <boolean>,
    init-value: #f, init-keyword: isOptimized:;
  constant slot llvm-DISubprogram-metadata-unit :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: unit:;
  constant slot llvm-DISubprogram-metadata-retained-nodes :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: retainedNodes:;
end class;

define method metadata-partition-key
    (metadata :: <llvm-DISubprogram-metadata>)
 => (key :: <vector>);
  if (metadata.llvm-metadata-distinct?)
    vector(metadata)
  else
    vector(object-class(metadata),
           metadata.llvm-DISubprogram-metadata-line,
           metadata.llvm-DISubprogram-metadata-local?,
           metadata.llvm-DISubprogram-metadata-definition?,
           metadata.llvm-DISubprogram-metadata-scope-line,
           metadata.llvm-DISubprogram-metadata-flags,
           metadata.llvm-DISubprogram-metadata-optimized?)
  end if
end method;

define method metadata-referenced-metadata
    (metadata :: <llvm-DISubprogram-metadata>)
 => (referenced :: <vector>);
  choose(true?,
         vector(metadata.llvm-DISubprogram-metadata-scope,
                metadata.llvm-DISubprogram-metadata-name,
                metadata.llvm-DISubprogram-metadata-linkage-name,
                metadata.llvm-DISubprogram-metadata-file,
                metadata.llvm-DISubprogram-metadata-type,
                metadata.llvm-DISubprogram-metadata-unit,
                metadata.llvm-DISubprogram-metadata-retained-nodes))
end method;

define class <llvm-DISubrange-metadata> (<llvm-debug-info-metadata>)
  constant slot llvm-DISubrange-metadata-count :: <llvm-metadata>,
    required-init-keyword: count:;
  constant slot llvm-DISubrange-metadata-lower-bound :: <integer>,
    init-value: 0, init-keyword: lowerBound:;
end class;

define sealed method make
    (class == <llvm-DISubrange-metadata>, #next next-method,
     #key distinct?, count, lowerBound = 0, #all-keys)
 => (instance :: <llvm-concrete-builder>);
  if (instance?(count, <integer>))
    let count-constant
      = make(<llvm-integer-constant>, type: $llvm-i64-type, integer: count);
    next-method(<llvm-DISubrange-metadata>,
                distinct?: distinct?,
                count: make(<llvm-value-metadata>, value: count-constant),
                lowerBound: lowerBound)
  else
    next-method()
  end if
end method;

define method metadata-partition-key
    (metadata :: <llvm-DISubrange-metadata>)
 => (key :: <vector>);
  if (metadata.llvm-metadata-distinct?)
    vector(metadata)
  else
    vector(object-class(metadata),
           metadata.llvm-DISubrange-metadata-lower-bound)
  end if
end method;

define method metadata-referenced-metadata
    (metadata :: <llvm-DISubrange-metadata>)
 => (referenced :: <vector>);
  choose(true?,
         vector(metadata.llvm-DISubrange-metadata-count))
end method;

define class <llvm-DISubroutineType-metadata> (<llvm-debug-info-metadata>)
  constant slot llvm-DISubroutineType-metadata-flags :: <integer>,
    init-value: 0, init-keyword: flags:;
  constant slot llvm-DISubroutineType-metadata-types :: false-or(<llvm-metadata>),
    init-value: #f, init-keyword: types:;
end class;

define method metadata-partition-key
    (metadata :: <llvm-DISubroutineType-metadata>)
 => (key :: <vector>);
  if (metadata.llvm-metadata-distinct?)
    vector(metadata)
  else
    vector(object-class(metadata),
           metadata.llvm-DISubroutineType-metadata-flags)
  end if
end method;

define method metadata-referenced-metadata
    (metadata :: <llvm-DISubroutineType-metadata>)
 => (referenced :: <vector>);
  choose(true?,
         vector(metadata.llvm-DISubroutineType-metadata-types))
end method;
