module:    access-path-implementation
synopsis:  Intepretation of machine instructions on the dummy simulator
author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// MACHINE INSTRUCTION FLOW DESCRIPTIONS
// These abstract over the various flow behaviours of machine-level
// instructions.

define constant $flowLinear = 1;
define constant $flowCallDirect = 2;
define constant $flowCallIndirect = 3;
define constant $flowJumpDirect = 4;
define constant $flowJumpIndirect = 5;
define constant $flowReturn = 6;
define constant $flowInterrupt = 7;
define constant $flowIllegal = 8;

///// INTERPRET-INSTRUCTION-AT-CURRENT-LOCATION
//    Abstracts over simple instruction disassembly. Returns enough
//    information to decide what kind of instruction this is (flow-wise),
//    and what the destination address is.
//    All addresses are absolute <remote-address> instances. PC-relative
//    addresses will have been pre-calculated and resolved.

define method interpret-instruction-at-current-location
    (ap :: <access-path>, thread :: <remote-thread>)
      => (flow, 
          destination :: false-or(<remote-value>),
          length :: false-or(<integer>))

  // Read the instruction from "memory", along with any operands,
  // even though they might not exist. We don't really care in this
  // dummy stuff.

  let address = thread.nub-descriptor.eip;
  let opcode = read-value (ap, address);
  let operand1 = read-value (ap, address + 1);
  let operand2 = read-value (ap, address + 2);

  let flow = $flowIllegal;
  let destination = address;
  let length = 0;

  select(opcode)
    $CALL =>
      destination := operand1;
      flow := $flowCallDirect;
      length := 2;

    $RET =>
      destination := ap.connection.process.memory.contents
                          [thread.nub-descriptor.esp];
      flow := $flowReturn;
      length := 1;

    $LINK =>
      destination := address + 1;
      length := 1;
      flow := $flowLinear;

    $SPAWN =>
      destination := operand1;
      length := 2;
      flow := $flowDirectCall;

    $END =>
      destination := #f;
      length := 1;
      flow := $flowIllegal;

    $NOP =>
      destination := address + 1;
      length := 1;
      flow := $flowLinear;

    $STOPCODE =>
      destination := address + 2;
      length := 2;
      flow := $flowLinear;

    $LOCALASSIGN =>
      destination := address + 3;
      length := 3;
      flow := $flowLinear;

    $GLOBALASSIGN =>
      destination := address + 3;
      length := 3;
      flow := $flowLinear;

    $LDA =>
      destination := address + 2;
      length := 2;
      flow := $flowLinear;

    $JREL =>
      destination := address + 2 + operand1;
      length := 2;
      flow := $flowDirectJump;

    $JRELNZ =>
      if (thread.nub-descriptor.zero-flag)
        flow := $flowLinear;
        destination := address + 2;
        length := 2;
      else
        flow := $flowDirectJump;
        length := 2;
        destination := address + 2 + operand1;
      end if;

    $DECA =>
      destination := address + 1;
      length := 1;
      flow := $flowLinear;

    $INCA =>
      destination := address + 1;
      length := 1;
      flow := $flowLinear;

    $PUSHA =>
      destination := address + 1;
      length := 1;
      flow := $flowLinear;

    $POPA =>
      destination := address + 1;
      length := 1;
      flow := $flowLinear;

    otherwise =>
      length := #f;
      destination := #f;
      flow := $flowIllegal;

  end select;

  // An return whatever that gave us...

  values(flow, destination, length);
end method;

