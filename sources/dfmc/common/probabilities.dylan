Synopsis: simple probability operations, for use with imprecise information
Module:   dfmc-probabilities
Author:   Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <probability> = <single-float>;

define constant $probability-false = as(<probability>, 0);
define constant $probability-maybe = as(<probability>, 0.5);  // canonical
define constant $probability-true  = as(<probability>, 1);

// TODO: ensure we don't get silent rounding to one

define method probability-not (p :: <probability>) => (not-p :: <probability>)
  $probability-true - p
end method probability-not;

define method probability-and
    (p :: <probability>, q :: <probability>) => (p-and-q :: <probability>)
  p * q
end method probability-and;

define method probability-or
    (p :: <probability>, q :: <probability>) => (p-or-q :: <probability>)
  p + q - p * q
end method probability-or;
