module: disasm
author: Jon Thackray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// A disassembler for i386, i486 and i586 code

define macro concrete-class-definer
  { define concrete-class ?:name (?supers:*)
      ?slot-body:*
    end
  }
   => { define sealed class ?name (?supers)
          ?slot-body
        end class ?name;
        define sealed domain make (singleton(?name));
        define sealed domain initialize (?name)
      }

supers:
  { } => { }
  { ?name1:name, ... } => { ?name1, ... }

end macro concrete-class-definer;

define constant $empty-vector = #[];

// define constant $empty-byte-vector = as(<byte-vector>, #[]);

define constant <argument-vector> = <simple-object-vector>;

define constant <four-byte-integer> = <integer>;

// First some classes to handle externals

define abstract class <external> (<object>)
end class <external>;

define concrete class <no-external> (<external>)
end class <no-external>;

define constant $no-external = make(<no-external>);

define abstract class <some-external> (<external>)
  constant slot position-in-code-vector :: <integer>, required-init-keyword: ext-code-pos:;
  constant slot label-name :: <byte-string>, required-init-keyword: init-label-name:;
end class <some-external>;

define concrete class <labelled-external> (<some-external>)
end class;

define concrete class <relative-external> (<some-external>)
  constant slot offset :: <four-byte-integer>, required-init-keyword: init-offset:;
end class;

define abstract class <opcode-argument> (<object>)
end class <opcode-argument>;

define abstract class <disassembly-failure> (<error>)
  constant slot position-in-code-vector :: <integer>, required-init-keyword: position:;
end class <disassembly-failure>;

define concrete-class <disassembly-no-more-bytes-error> (<disassembly-failure>)
end concrete-class <disassembly-no-more-bytes-error>;

define concrete-class <disassembly-unexpected-byte-error> (<disassembly-failure>)
end concrete-class <disassembly-unexpected-byte-error>;

define abstract class <immediate-value> (<object>)
end class <immediate-value>;

define concrete-class <byte-immediate-value> (<immediate-value>)
  constant slot byte-immediate-value :: <integer>, required-init-keyword: byte-immediate-value:;
end concrete-class <byte-immediate-value>;


define constant $zero-byte-immediate-value = make(<byte-immediate-value>, byte-immediate-value: 0);

define concrete-class <short-immediate-value> (<immediate-value>)
  constant slot short-immediate-value :: <integer>, required-init-keyword: short-immediate-value:;
end concrete-class <short-immediate-value>;

define constant $zero-short-immediate-value = make(<short-immediate-value>, short-immediate-value: 0);

define concrete-class <word-immediate-value> (<immediate-value>)
  constant slot word-immediate-value :: <four-byte-integer>, required-init-keyword: word-immediate-value:;
  constant slot word-relocation :: <external>, required-init-keyword: word-relocation:;
end concrete-class <word-immediate-value>;

define constant $zero-word-immediate-value = make(<word-immediate-value>, word-immediate-value: 0, word-relocation: $no-external);

define concrete-class <register> (<object>)
  constant slot register-integer-rep :: <integer>, required-init-keyword: register-integer-rep:;
  constant slot register-name :: <byte-string>, required-init-keyword: register-name:;
end concrete-class <register>;

define concrete-class <fp-register> (<object>)
// Really a position in the fp stack
  constant slot fp-register-pos :: <integer>, required-init-keyword: fp-register-pos:;
end concrete-class <fp-register>;

define abstract class <general-opcode> (<object>)
end class <general-opcode>;

define concrete-class <general-opcode-and-offsets> (<object>)
  constant slot general-opcode-opcode :: <general-opcode>, required-init-keyword: general-opcode-opcode:;
  constant slot general-opcode-offset :: <integer>, required-init-keyword: general-opcode-offset:;
  constant slot general-opcode-end-offset :: <integer>, required-init-keyword: general-opcode-end-offset:;
end concrete-class <general-opcode-and-offsets>;

define abstract class <segment-override> (<object>)
end class <segment-override>;

define concrete-class <no-segment-override> (<segment-override>)
end concrete-class <no-segment-override>;

define concrete-class <segment-register> (<object>)
  constant slot segment-register-name :: <byte-string>, required-init-keyword: segment-register-name:;
end concrete-class;

define concrete-class <some-segment-override> (<segment-override>)
  constant slot segment-register :: <segment-register>, required-init-keyword: segment-register:;
end concrete-class <some-segment-override>;

define concrete-class <proper-opcode> (<general-opcode>)
  constant slot proper-opcode-name :: <byte-string>,
    required-init-keyword: proper-opcode-name:;
  constant slot proper-opcode-args :: <argument-vector>,
    required-init-keyword: proper-opcode-args:;
  constant slot proper-opcode-seg :: <segment-override>,
    required-init-keyword: proper-opcode-seg:;
end concrete-class <proper-opcode>;

define constant $nop = make(<proper-opcode>, proper-opcode-name: "nop", proper-opcode-args: $empty-vector, proper-opcode-seg: make(<no-segment-override>));

define constant $unknown = make(<proper-opcode>, proper-opcode-name: "???", proper-opcode-args: $empty-vector, proper-opcode-seg: make(<no-segment-override>));

define constant $nop-and-offset = make(<general-opcode-and-offsets>, general-opcode-opcode: $nop, general-opcode-offset: 0, general-opcode-end-offset: 1);

define concrete-class <not-an-opcode> (<general-opcode>)
  constant slot bytes-read :: <byte-vector>, required-init-keyword: not-an-opcode-bytes-read:;
  constant slot not-an-opcode-from :: <integer>, required-init-keyword: not-an-opcode-from:;
end concrete-class <not-an-opcode>;

ignore(not-an-opcode-from);

define concrete-class <unspecified-not-an-opcode> (<general-opcode>)
  // Just like not-an-opcode, but we don't say what produced it
end concrete-class <unspecified-not-an-opcode>;

define constant $unspecified-not-an-opcode = make(<unspecified-not-an-opcode>);

//define abstract class <floating-point-opcode> (<proper-opcode>)
//end class <floating-point-opcode>;

define concrete-class <integer-opcode> (<proper-opcode>)
end concrete-class <integer-opcode>;

define concrete-class <immediate-arg> (<opcode-argument>)
  constant slot arg-immediate-value :: <immediate-value>, required-init-keyword: arg-immediate-value:;
end concrete-class <immediate-arg>;

define concrete-class <register-arg> (<opcode-argument>)
  constant slot register-arg :: <register>, required-init-keyword: register-arg:;
end concrete-class <register-arg>;

define concrete-class <fp-register-arg> (<opcode-argument>)
  constant slot fp-register-arg :: <fp-register>, required-init-keyword: fp-register-arg:;
end concrete-class <fp-register-arg>;

define abstract class <offset> (<opcode-argument>)
end class <offset>;

define concrete-class <byte-offset> (<offset>)
  constant slot byte-offset :: <integer>, required-init-keyword: byte-offset:;
end concrete-class <byte-offset>;

define concrete-class <short-offset> (<offset>)
  constant slot short-offset :: <integer>, required-init-keyword: short-offset:;
end concrete-class <short-offset>;

define concrete-class <word-offset> (<offset>)
  constant slot word-offset :: <four-byte-integer>, required-init-keyword: word-offset:;
  constant slot word-offset-relocation :: <external>, required-init-keyword: word-offset-relocation:;
end concrete-class <word-offset>;

define concrete-class <offset-arg> (<opcode-argument>)
  constant slot offset-arg :: <offset>, required-init-keyword: offset-arg:;
end concrete-class <offset-arg>;

define abstract class <memory-index> (<object>)
end class <memory-index>;

define concrete-class <no-memory-index> (<memory-index>)
end concrete-class <no-memory-index>;

define constant $no-memory-index = make(<no-memory-index>);

define concrete-class <scaled-indexed-memory-index> (<memory-index>)
  constant slot indexed-memory-index-reg :: <register>, required-init-keyword: indexed-memory-index-reg:;
  constant slot indexed-memory-index-scale :: <integer>, required-init-keyword: indexed-memory-index-scale:;
end concrete-class <scaled-indexed-memory-index>;

define abstract class <memory-displacement> (<object>)
end class <memory-displacement>;

define concrete-class <no-memory-displacement> (<memory-displacement>)
end concrete-class <no-memory-displacement>;

define constant $no-memory-displacement = make(<no-memory-displacement>);

define concrete-class <some-memory-displacement> (<memory-displacement>)
  constant slot memory-displacement :: <immediate-value>, required-init-keyword: memory-displacement:;
end concrete-class <some-memory-displacement>;

define abstract class <memory-base> (<object>)
end class <memory-base>;

define concrete-class <no-memory-base> (<memory-base>)
end concrete-class <no-memory-base>;

define constant $no-memory-base = make(<no-memory-base>);

define concrete-class <some-memory-base> (<memory-base>)
  constant slot memory-base-reg :: <register>, required-init-keyword: memory-base-reg:;
end concrete-class <some-memory-base>;

define abstract class <memory-arg-size> (<object>)
end class <memory-arg-size>;

define concrete-class <memory-arg-size-byte> (<memory-arg-size>)
end concrete-class <memory-arg-size-byte>;

define concrete-class <memory-arg-size-short> (<memory-arg-size>)
end concrete-class <memory-arg-size-short>;

define concrete-class <memory-arg-size-word> (<memory-arg-size>)
end concrete-class <memory-arg-size-word>;

define concrete-class <memory-arg-size-double-word> (<memory-arg-size>)
end concrete-class <memory-arg-size-double-word>;

define concrete-class <memory-arg-size-word-real> (<memory-arg-size>)
end concrete-class <memory-arg-size-word-real>;

define concrete-class <memory-arg-size-double-word-real> (<memory-arg-size>)
end concrete-class <memory-arg-size-double-word-real>;

define concrete-class <memory-arg-size-extended-real> (<memory-arg-size>)
end concrete-class <memory-arg-size-extended-real>;

define constant $byte-arg-size = make(<memory-arg-size-byte>);

define constant $short-arg-size = make(<memory-arg-size-short>);

define constant $word-arg-size = make(<memory-arg-size-word>);

define constant $double-word-arg-size = make(<memory-arg-size-double-word>);

define constant $word-real-arg-size = make(<memory-arg-size-word-real>);

define constant $double-word-real-arg-size = make(<memory-arg-size-double-word-real>);

define constant $extended-real-arg-size = make(<memory-arg-size-extended-real>);

define concrete-class <memory-arg> (<opcode-argument>)
  constant slot memory-arg-disp :: <memory-displacement>, required-init-keyword: memory-arg-disp:;
  constant slot memory-arg-base :: <memory-base>, required-init-keyword: memory-arg-base:;
  constant slot memory-arg-index :: <memory-index>, required-init-keyword: memory-arg-index:;
  constant slot memory-arg-size :: <memory-arg-size>, required-init-keyword: memory-arg-size:;
end concrete-class <memory-arg>;

define concrete-class <is-16-bit-operands> (<object>)
  slot is-16-bit :: <boolean>;
end concrete-class <is-16-bit-operands>;

define concrete-class <is-16-bit-addressing> (<object>)
  slot is-16-bit :: <boolean>;
end concrete-class <is-16-bit-addressing>;

define abstract class <repeater> (<object>)
end class <repeater>;

define concrete-class <no-repeater> (<repeater>)
end concrete-class <no-repeater>;

define concrete-class <some-repeater> (<repeater>)
  constant slot repeater-value :: <integer>, required-init-keyword: repeater-value:;
end concrete-class <some-repeater>;

ignore(repeater-value);
