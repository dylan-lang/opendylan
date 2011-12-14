Module: common-dylan-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2003-2011 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Integers

define generic integer-length
    (integer :: <abstract-integer>) => (length :: <integer>);

define method integer-length
    (integer :: <integer>) => (length :: <integer>);
  let word :: <machine-word>
    = if (negative?(integer))
        %lognot(as(<machine-word>, integer))
      else
        as(<machine-word>, integer)
      end;
  $machine-word-size - %count-high-zeros(word)
end method;


/// Floating point

define generic decode-float
    (float :: <float>)
 => (significand :: <float>, exponent :: <integer>, sign :: <float>);

define generic scale-float
    (float :: <float>, scale :: <integer>) => (result :: <float>);

define function float-radix (float :: <float>) => (radix :: <integer>);
  2;
end;

define generic float-digits (float :: <float>) => (digits :: <integer>);
define generic float-precision (float :: <float>) => (digits :: <integer>);


/// Single float

// ----------------------------------------------------------------
// |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|  ...   | 1| 0|
// ----------------------------------------------------------------
// |sg|       exponent        |            significand            |
// ----------------------------------------------------------------

define constant $ieee-754-single-float-digits :: <integer> = 23 + 1;
define constant $ieee-754-single-float-exponent-bias :: <integer> = #x7F;

define constant $minimum-single-float-exponent :: <integer>
  = #x01 - ($ieee-754-single-float-exponent-bias - 1);
define constant $maximum-single-float-exponent :: <integer>
  = #xFE - ($ieee-754-single-float-exponent-bias - 1);

define constant $sign-bit-mask :: <machine-word> = %shift-left(1, 31);

define method %float-exponent
    (float :: <single-float>)
 => (exponent :: <integer>);
  let decoded :: <machine-word> = decode-single-float(float);
  let raw-exponent :: <machine-word>
    = %logand(u%shift-right(decoded, $ieee-754-single-float-digits - 1), #xFF);
  as(<integer>, raw-exponent) - ($ieee-754-single-float-exponent-bias - 1)
end method;

define method decode-float
    (float :: <single-float>)
 => (significand :: <single-float>, exponent :: <integer>,
     sign :: <single-float>);
  let sign :: <single-float> = if (negative?(float)) -1.0s0 else 1.0s0 end;
  let decoded :: <machine-word> = decode-single-float(float);
  let significand-bits :: <machine-word> = %logand(decoded, #x7FFFFF);

  let exponent = %float-exponent(float);
  case
    exponent < $minimum-single-float-exponent => // Zero or denormal
      if (zero?(significand-bits))
        values(0.0s0, 0, sign)
      else
        error("Not handling denormals yet");
      end if;

    exponent > $maximum-single-float-exponent => // Infinity or NaN
      values(abs(float), 0, sign);

    otherwise =>
      let raw-significand :: <machine-word>
        = %logior(u%shift-left($ieee-754-single-float-exponent-bias - 1,
                               $ieee-754-single-float-digits - 1),
                  significand-bits);
      values(encode-single-float(raw-significand), exponent, sign)
  end case;
end;

define method scale-float
    (float :: <single-float>, scale :: <integer>)
 => (result :: <single-float>);
  let decoded :: <machine-word> = decode-single-float(float);
  let significand-bits :: <machine-word> = %logand(decoded, #x7FFFFF);
  let exponent = %float-exponent(float);
  let scaled-exponent = exponent + scale;

  case
    exponent < $minimum-single-float-exponent =>
      // Argument is denormal: Use multiplication
      float * scale-float(1.0s0, scale);

    scaled-exponent > $maximum-single-float-exponent =>
      // Result overflow: Infinity: exponent = #xFF, significand = 0
      let raw-result :: <machine-word>
        = %logior(%logand(decoded, $sign-bit-mask),
                  u%shift-left(#xFF, $ieee-754-single-float-digits - 1));
      encode-single-float(raw-result);

    scaled-exponent < $minimum-single-float-exponent =>
      // Result will be denormal: Use multiplication
      scale-float(float, scale - ($minimum-single-float-exponent - 1))
        * scale-float(1.0s0, $minimum-single-float-exponent - 1);

    otherwise =>
      let raw-result :: <machine-word>
        = %logior(%logand(decoded, $sign-bit-mask),
                  u%shift-left(scaled-exponent
                                 + ($ieee-754-single-float-exponent-bias - 1),
                               $ieee-754-single-float-digits - 1),
                  significand-bits);
      encode-single-float(raw-result);
  end case
end method;

define inline method float-digits
    (float :: <single-float>) => (digits :: <integer>);
  $ieee-754-single-float-digits
end;

define inline method float-precision
    (float :: <single-float>) => (digits :: <integer>);
  let exponent = %float-exponent(float);
  if (exponent > $minimum-single-float-exponent)
    $ieee-754-single-float-digits
  else
    let decoded :: <machine-word> = decode-single-float(float);
    let significand-bits :: <machine-word> = %logand(decoded, #x7FFFFF);
    $machine-word-size - %count-high-zeros(significand-bits)
  end if
end;

define constant $single-float-epsilon :: <single-float>
  = scale-float(1.0s0, 1 - $ieee-754-single-float-digits);


/// Double float

// ----------------------------------------------------------------
// |63|62|61|60|59|58|57|56|55|54|53|52|51|50|49|48|  ...   | 1| 0|
// ----------------------------------------------------------------
// |sg|            exponent            |       significand        |
// ----------------------------------------------------------------

define constant $ieee-754-double-float-digits :: <integer> = 52 + 1;
define constant $ieee-754-double-float-exponent-bias :: <integer> = #x3FF;

define constant $minimum-double-float-exponent :: <integer>
  = #x001 - ($ieee-754-double-float-exponent-bias - 1);
define constant $maximum-double-float-exponent :: <integer>
  = #x7FE - ($ieee-754-double-float-exponent-bias - 1);

define method %float-exponent
    (float :: <double-float>)
 => (exponent :: <integer>);
  let (decoded-low :: <machine-word>, decoded-high :: <machine-word>)
    = decode-double-float(float);
  let raw-exponent :: <machine-word>
    = %logand(u%shift-right
                (decoded-high,
                 $ieee-754-double-float-digits - 1 - $machine-word-size),
              #x7FF);
  as(<integer>, raw-exponent) - ($ieee-754-double-float-exponent-bias - 1);
end method;

define method decode-float
    (float :: <double-float>)
 => (significand :: <double-float>, exponent :: <integer>,
     sign :: <double-float>);
  let sign :: <double-float> = if (negative?(float)) -1.0d0 else 1.0d0 end;
  let (decoded-low :: <machine-word>, decoded-high :: <machine-word>)
    = decode-double-float(float);
  let significand-high-bits :: <machine-word> = %logand(decoded-high, #xFFFFF);

  let exponent = %float-exponent(float);  
  case
    exponent < $minimum-double-float-exponent => // Zero or denormal
      if (zero?(significand-high-bits) & zero?(decoded-low))
        values(0.0d0, 0, sign)
      else
        error("Not handling denormals yet");
      end if;

    exponent > $maximum-double-float-exponent => // Infinity or NaN
      error("Can't decode-float infinity or NaN");

    otherwise =>
      let raw-high-significand :: <machine-word>
        = %logior(u%shift-left($ieee-754-double-float-exponent-bias - 1,
                               $ieee-754-double-float-digits - 1
                                 - $machine-word-size),
                  significand-high-bits);
      values(encode-double-float(decoded-low, raw-high-significand),
             exponent, sign)
  end case;
end;

define method scale-float
    (float :: <double-float>, scale :: <integer>)
 => (result :: <double-float>);
  let (decoded-low :: <machine-word>, decoded-high :: <machine-word>)
    = decode-double-float(float);
  let significand-high-bits :: <machine-word> = %logand(decoded-high, #xFFFFF);
  let exponent = %float-exponent(float);
  let scaled-exponent = exponent + scale;

  case
    exponent < $minimum-double-float-exponent =>
      // Argument is denormal: Use multiplication
      float * scale-float(1.0d0, scale);

    scaled-exponent > $maximum-double-float-exponent =>
      // Result overflow: Infinity: exponent = #x7FF, significand = 0
      let raw-high-result :: <machine-word>
        = %logior(%logand(decoded-high, $sign-bit-mask),
                  u%shift-left(#x7FF,
                               $ieee-754-double-float-digits - 1
                                 - $machine-word-size));
      encode-double-float($machine-word-zero, raw-high-result);

    scaled-exponent < $minimum-double-float-exponent =>
      // Result will be denormal: Use multiplication
      scale-float(float, scale - ($minimum-double-float-exponent - 1))
        * scale-float(1.0d0, $minimum-double-float-exponent - 1);

    otherwise =>
      let raw-high-result :: <machine-word>
        = %logior(%logand(decoded-high, $sign-bit-mask),
                  u%shift-left(scaled-exponent
                                 + ($ieee-754-double-float-exponent-bias - 1),
                               $ieee-754-double-float-digits - 1
                                 - $machine-word-size),
                  significand-high-bits);
      encode-double-float(decoded-low, raw-high-result)
  end case
end method;

define inline method float-digits
    (float :: <double-float>) => (digits :: <integer>);
  $ieee-754-double-float-digits
end;

define inline method float-precision
    (float :: <double-float>) => (digits :: <integer>);
  let exponent = %float-exponent(float);
  if (exponent > $minimum-double-float-exponent)
    $ieee-754-double-float-digits
  else
    let (decoded-low :: <machine-word>, decoded-high :: <machine-word>)
      = decode-double-float(float);
    let significand-high-bits :: <machine-word>
      = %logand(decoded-high, #xFFFFF);
    if (zero?(significand-high-bits))
      $machine-word-size - %count-high-zeros(decoded-low)
    else
      $machine-word-size - %count-high-zeros(significand-high-bits) + 32
    end if
  end if
end;

define constant $double-float-epsilon :: <double-float>
  = scale-float(1.0d0, 1 - $ieee-754-double-float-digits);
