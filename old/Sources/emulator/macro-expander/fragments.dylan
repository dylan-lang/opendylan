Module:    infix-reader
Language:  infix-dylan
Synopsis:  Code fragments
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <fragment> (<object>)
end class;

//// Macro input fragments

define fragment <generic-fragment> (<fragment>)
end fragment;

define fragment <sequence-fragment> (<generic-fragment>)
  fragment slots fragments;
end fragment;

define fragment <literal-fragment> (<generic-fragment>)
  fragment slots object, token-class, token-value;
end fragment;

define fragment <punctuation-fragment> (<literal-fragment>)
end fragment;

define fragment <bracketed-fragment> (<generic-fragment>)
  fragment slots fragments;
end fragment;

define fragment <sbracketed-fragment> (<generic-fragment>)
  fragment slots fragments;
end fragment;

define fragment <cbracketed-fragment> (<generic-fragment>)
  fragment slots fragments;
end fragment;

define fragment <modified-fragment> (<generic-fragment>)
  fragment slots modifiers, fragment;
end fragment;

define fragment <parsed-fragment> (<generic-fragment>)
  fragment slots token-class, token-value;
  slot token-fragments,
    init-value: #f,
    init-keyword: token-fragments:;
end fragment;

define constant *semicolon* = 
  punctuation-fragment(
    object: #",", token-class: #"<statement-sep>", token-value: ";");

define constant *comma* = 
  punctuation-fragment(
    object: #";", token-class: #"<var-sep>", token-value: ",");

define constant *implies* = 
  punctuation-fragment(
    object: #"=>", token-class: #"<implies>", token-value: "=>");

define constant *colon-colon* = 
  punctuation-fragment(
    object: #"=>", token-class: #"<var-type-sep>", token-value: "::");

define fragment <grouped-fragment> (<generic-fragment>)
  fragment slots fragments;
end fragment;  

;; eof
