Module:    licensing
Author:    Gary Palter
Synopsis:  License validator for Functional Developer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define settings <licensing-settings> (<unversioned-open-dylan-local-settings>)
  key-name "License";
  slot data :: <string> = "None";
  slot serial :: <string> = "HDeee-rrrr";
  slot expiration :: <string> = "None";
  slot user :: <string> = "";
  slot company :: <string> = "";
end settings <licensing-settings>;

define constant $licensing-settings = make(<licensing-settings>);

define settings <library-pack-licensing-settings> (<licensing-settings>)
  variable key-name "LibraryPack.NN";
  slot data :: <string> = "None";
  slot serial :: <string> = "HDeee-rrrr";
  slot expiration :: <string> = "None";
end settings <library-pack-licensing-settings>;

define constant $library-pack-licensing-settings = make(<library-pack-licensing-settings>);

// The standard evaluation period is hereby set at 30 days ...
define constant $evaluation-period = 30;

define settings <current-version-settings> (<unversioned-open-dylan-local-settings>)
  key-name "CurrentVersion";
  slot edition :: <string> = "0";
  slot version :: <string> = "Unknown";
  slot installed :: <string> = "0000";
end settings <current-version-settings>;

define constant $current-version-settings = make(<current-version-settings>);

define settings <library-pack-installed-settings> (<current-version-settings>)
  variable key-name "LibraryPack.NN";
  slot installed :: <string> = "0000";
end settings <library-pack-installed-settings>;

define constant $library-pack-installed-settings = make(<library-pack-installed-settings>);

define C-function compute-sha-key
  parameter input :: <C-string>;
  parameter input-size :: <C-signed-int>;
  parameter buffer :: <C-string>;
  parameter buffer-size :: <C-signed-int>;
  result buffer-used :: <C-signed-int>;
  c-name: "sha";
end C-function compute-sha-key;

define abstract class <license-validation-failure> (<error>)
  constant slot lvf-product :: <string>, required-init-keyword: product:;
end class <license-validation-failure>;

define class <unregistered-software> (<license-validation-failure>)
end class <unregistered-software>;

define sealed method condition-to-string (condition :: <unregistered-software>)
 => (description :: <string>)
  format-to-string("This copy of %s is not licensed.", lvf-product(condition))
end method condition-to-string;

define class <invalid-key> (<license-validation-failure>)
end class <invalid-key>;

define sealed method condition-to-string (condition :: <invalid-key>)
 => (description :: <string>)
  format-to-string("The license for this copy of %s is invalid.", lvf-product(condition))
end method condition-to-string;

define class <license-expired> (<license-validation-failure>)
  constant slot lvf-year :: <integer>, required-init-keyword: expiration-year:;
  constant slot lvf-month :: <integer>, required-init-keyword: expiration-month:;
  constant slot lvf-day :: <integer>, required-init-keyword: expiration-day:;
  constant slot lvf-installation :: <symbol>, required-init-keyword: installation:;
end class <license-expired>;

define constant $months = #["Jan", "Feb", "Mar", "Apr", "May", "Jun",
			    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

define sealed method condition-to-string (condition :: <license-expired>)
 => (description :: <string>)
  format-to-string("This %scopy of %s expired on %s-%s-%s.",
		   select (lvf-installation(condition))
		     #"evaluation" => "evaluation ";
		     #"beta"       => "test ";
		     otherwise     => "";
		   end,
		   lvf-product(condition),
		   integer-to-string(lvf-day(condition), size: 2),
		   $months[lvf-month(condition) - 1],
		   integer-to-string(lvf-year(condition), size: 2))
end method condition-to-string;

define constant <encoded-edition> = <integer>;

define inline-only function encode-edition (edition) => (encoded-edition :: <encoded-edition>)
  if (instance?(edition, <byte-string>) & (copy-sequence(edition, end: 4) = "FDLP"))
    string-to-integer(edition, base: 36, start: 4)
  else
    select (edition by \=)
      #"console-tools", "FDCBT" => -1;
      #"basic", "FDBAS"         =>  0;
      #"personal", "HDPER"      =>  0;  // For compatibility with Harlequin Dylan ...
      #"professional", "HDPRO"  =>  1;  // For compatibility with Harlequin Dylan ...
      #"enhanced", "FDENH"      =>  2;
      #"enterprise", "HDENT"    =>  2;  // For compatibility with Harlequin Dylan ...
      #"internal", "FDTNG"      =>  3;
      "HDTNG"                   =>  3;  // For compatibility with Harlequin Dylan ...
      otherwise                 =>  4;  // E.g., the emulator ...
    end
  end
end function encode-edition;

define inline-only function decode-edition (encoded-edition :: <encoded-edition>)
 => (decoded-edition :: <byte-string>)
  select (encoded-edition)
    -1 => "Console-Based Tools";
     0 => "Basic";
     1 => "Professional";	        // For compatibility with Harlequin Dylan ...
     2 => "Enhanced";
     3 => "Internal";
     4 => "Internal";
  end
end function decode-edition;

define class <edition-mismatch> (<license-validation-failure>)
  constant slot lvf-expected-edition :: <encoded-edition>,
    required-init-keyword: expected-edition:;
  constant slot lvf-actual-edition :: <encoded-edition>,
    required-init-keyword: actual-edition:;
end class <edition-mismatch>;

define class <incorrect-edition> (<edition-mismatch>)
end class <incorrect-edition>;

define sealed method condition-to-string (condition :: <incorrect-edition>)
 => (description :: <string>)
  format-to-string("This license is for the %s Edition of %s, not the %s Edition.",
		   decode-edition(lvf-expected-edition(condition)),
		   lvf-product(condition),
		   decode-edition(lvf-actual-edition(condition)))
end method condition-to-string;

define class <incorrect-library-pack> (<edition-mismatch>)
end class <incorrect-library-pack>;

define sealed method condition-to-string (condition :: <incorrect-library-pack>)
 => (description :: <string>)
  format-to-string("This license is for %s, not %s.",
		   library-pack-full-name(lvf-expected-edition(condition)),
		   library-pack-full-name(lvf-actual-edition(condition)))
end method condition-to-string;

define class <missing-library-pack> (<license-validation-failure>)
end class <missing-library-pack>;

define sealed method condition-to-string (condition :: <missing-library-pack>)
 => (description :: <string>)
  format-to-string("%s is not installed.\n"
		     "It must be present or %s will not operate properly.",
		   lvf-product(condition),
		   release-product-name())
end method condition-to-string;

define constant <encoded-version> = <integer>;

define class <incorrect-version> (<license-validation-failure>)
  constant slot lvf-version :: <encoded-version>, required-init-keyword: expected-version:;
end class <incorrect-version>;

/// Only the major version of the product is relevant for comparisons ...
define method encode-version (version :: <symbol>) => (encoded-version :: <encoded-version>)
  if (version == #"1.0 beta" | version == #"internal")
    0
  else
    string-to-integer(as(<string>, version), default: 0)
  end
end method encode-version;

/// Only the major version of the product is relevant for comparisons ...
define method encode-version (version :: <byte-string>)
 => (encoded-version :: <encoded-version>)
  string-to-integer(version, start: 0, end: 2, default: 0)
end method encode-version;

define sealed method condition-to-string (condition :: <incorrect-version>)
 => (description :: <string>)
  let expected-version = lvf-version(condition);
  ignore(expected-version);
  format-to-string("This license is for an older version of %s.\n"
		     "You must purchase an upgrade to use %s",
		   lvf-product(condition), release-version())
end method condition-to-string;

define inline function decode-expiration-date (date :: <string>)
 => (year :: <integer>, month :: <integer>, day :: <integer>)
  let encoded-date :: <integer> = string-to-integer(date, base: 16);
  values(truncate/(encoded-date, 512) + 1990,
	 remainder(truncate/(encoded-date, 32), 16),
	 remainder(encoded-date, 32))
end function decode-expiration-date;

define inline function evaluation-end-date (date :: <string>)
 => (year :: <integer>, month :: <integer>, day :: <integer>)
  if (date = "0000")
    values($maximum-integer, $maximum-integer, $maximum-integer)
  else
    // Must use date arithmetic to avoid returning a result that
    // can't be encoded as a <date> by license-info, below ...
    let (year :: <integer>, month :: <integer>, day :: <integer>)
      = decode-expiration-date(date);
    let installed :: <date> = encode-date(year, month, day, 0, 0, 0);
    let expiration :: <date>
      = installed + encode-day/time-duration($evaluation-period, 0, 0, 0, 0);
    let (year :: <integer>, month :: <integer>, day :: <integer>)
      = decode-date(expiration);
    values(year, month, day)
  end
end function evaluation-end-date;

define function validate-one-license
    (data :: <byte-string>, serial :: <byte-string>, expiration :: <byte-string>,
     product :: <byte-string>, edition :: <encoded-edition>,
     edition-exact-match? :: <boolean>, edition-mismatch :: subclass(<edition-mismatch>),
     version :: <encoded-version>, installed-date :: <byte-string>)
 => ()
  let stored-edition :: <encoded-edition>
    = encode-edition(copy-sequence(serial, start: 0, end: 5));
  let stored-version :: <encoded-version>
    = encode-version(copy-sequence(serial, start: 6, end: 10));
  local method validate-license-internal
	    (edition :: <encoded-edition>, version :: <encoded-version>)
	 => ()
	  case
	    edition = stored-edition & version = stored-version =>
	      let input :: <byte-string> = concatenate-as(<byte-string>, serial, expiration);
	      let computed-key :: <byte-string> = make(<byte-string>, size: 64, fill: '\0');
	      let used :: <integer>
		= compute-sha-key(input, size(input), computed-key, size(computed-key));
	      if (copy-sequence(computed-key, end: used) = data)
		// License key is valid -- Check if the key has expired
		let (year :: <integer>, month :: <integer>, day :: <integer>)
		  = decode-date(current-date());
		let (expiry-year :: <integer>, expiry-month :: <integer>, 
		     expiry-day :: <integer>)
		  = if (expiration = "0000")
		      values($maximum-integer, $maximum-integer, $maximum-integer)
		    elseif (expiration = "xxxx")
		      evaluation-end-date(installed-date)
		    else
		      decode-expiration-date(expiration)
		    end;
		if (year > expiry-year 
		      | (year = expiry-year
			   & (month > expiry-month 
				| (month = expiry-month & (day > expiry-day)))))
		  error(make(<license-expired>, 
			     expiration-year: expiry-year,
			     expiration-month: expiry-month,
			     expiration-day: expiry-day,
			     product: product,
			     installation: case
					     expiration = "xxxx"
					       => #"evaluation";
					     release-beta?()
					       => #"beta";
					     otherwise
					       => #"normal";
					   end))
		end
	      else
		error(make(<invalid-key>, product: product))
	      end;
	    edition > stored-edition =>
	      // License is for a less "comprehensive" edition of the product ...
	      error(make(edition-mismatch, 
			 product: product,
			 expected-edition: stored-edition,
			 actual-edition: edition));
	    edition-exact-match? & edition ~= stored-edition =>
	      // License is not for the same edition of the product and the 
	      // caller requires it to be so (e.g., for Library Packs) ...
	      error(make(edition-mismatch, 
			 product: product,
			 expected-edition: stored-edition,
			 actual-edition: edition));
	    version > stored-version =>
	      // License is for an older version of the product ...
	      error(make(<incorrect-version>,
			 product: product,
			 expected-version: stored-version));
	    otherwise =>
	      // License is for a more "comprehensive" edition and/or a newer version ...
	      validate-license-internal(stored-edition, stored-version)
	  end
	end method validate-license-internal;
  if (data ~= "None")
    validate-license-internal(edition, version)
  else
    error(make(<unregistered-software>, product: product))
  end
end function validate-one-license;

define function validate-license (console-tools? :: <boolean>) => ()
  let version :: <encoded-version> = encode-version(release-version-type());
  // Validate the master (IDE) license
  let edition :: <encoded-edition> = encode-edition(release-edition-type());
  validate-one-license($licensing-settings.data, $licensing-settings.serial,
		       $licensing-settings.expiration,
		       release-product-name(),
		       edition, #f, <incorrect-edition>,
		       version,
		       $current-version-settings.installed);
  if (console-tools?)
    // If the developer has overriden Library Pack license checks, we'll disable
    // the console tools check as well ...
    unless (environment-variable("OPEN_DYLAN_LIBRARY_PACKS"))
      // We'll use the Library Pack settings variables because it's an easy thing to do...
      $library-pack-licensing-settings.settings-key-name := "ConsoleTools";
      $library-pack-installed-settings.settings-key-name := "ConsoleTools";
      validate-one-license($library-pack-licensing-settings.data, 
			   $library-pack-licensing-settings.serial,
			   $library-pack-licensing-settings.expiration,
			   "Console-Based Developer Tools",
			   encode-edition(#"console-tools"), #t, <incorrect-edition>,
			   version,
			   $library-pack-installed-settings.installed)
    end
  end;
  // Check for required library packs
  for (library-pack :: <integer> in release-required-library-packs())
    unless (release-contains-library-pack?(library-pack))
      error(make(<missing-library-pack>, 
		 product: library-pack-full-name(library-pack)))
    end
  end;
  // Validate library pack licenses
  for (library-pack :: <integer> in release-optional-library-packs())
    let key-name :: <byte-string>
      = format-to-string("LibraryPack.%s", integer-to-string(library-pack, size: 2));
    let pack-name :: <byte-string> = library-pack-full-name(library-pack);
    $library-pack-licensing-settings.settings-key-name := key-name;
    $library-pack-installed-settings.settings-key-name := key-name;
    validate-one-license($library-pack-licensing-settings.data, 
			 $library-pack-licensing-settings.serial,
			 $library-pack-licensing-settings.expiration,
			 pack-name, 
			 library-pack, #t, <incorrect-library-pack>, 
			 version,
			 $library-pack-installed-settings.installed)
  end
end function validate-license;

define function license-info ()
 => (serial-number :: <byte-string>, evaluation? :: <boolean>, expiration :: false-or(<date>),
     user :: false-or(<byte-string>), company :: false-or(<byte-string>))
  let serial-number :: <byte-string> = $licensing-settings.serial;
  let raw-user :: <byte-string> = $licensing-settings.user;
  let raw-company :: <byte-string> = $licensing-settings.company;
  let (expiry-year :: <integer>, expiry-month :: <integer>, expiry-day :: <integer>)
    = if ($licensing-settings.expiration = "0000")
	values($maximum-integer, $maximum-integer, $maximum-integer)
      elseif ($licensing-settings.expiration = "xxxx")
	evaluation-end-date($current-version-settings.installed)
      else
	decode-expiration-date($licensing-settings.expiration)
      end;
  values(serial-number,
	 $licensing-settings.expiration = "xxxx",
	 if (expiry-year < $maximum-integer)
	   encode-date(expiry-year, expiry-month, expiry-day, 0, 0, 0)
	 else
	   #f
	 end,
	 ~empty?(raw-user) & raw-user,
	 ~empty?(raw-company) & raw-company)
end function license-info;

define function unregistered-products () => (products :: <sequence>)
  let products = make(<stretchy-object-vector>);
  let version :: <encoded-version> = encode-version(release-version-type());
  // Check the master (IDE) license
  let edition :: <encoded-edition> = encode-edition(release-edition-type());
  block ()
    validate-one-license($licensing-settings.data,
			 $licensing-settings.serial,
			 $licensing-settings.expiration,
			 release-product-name(),
			 edition, #f, <incorrect-edition>,
			 version,
			 $current-version-settings.installed);
    if ($licensing-settings.expiration = "xxxx")
      add!(products, #"IDE")
    end
  exception (<license-validation-failure>)
    add!(products, #"IDE")
  end;
  // Check the console tools license (if any)
  if (release-contains-console-tools?())
    // If the developer has overriden Library Pack license checks, we'll disable
    // the console tools check as well ...
    unless (environment-variable("OPEN_DYLAN_LIBRARY_PACKS"))
      // We'll use the Library Pack settings variables because it's an easy thing to do...
      $library-pack-licensing-settings.settings-key-name := "ConsoleTools";
      $library-pack-installed-settings.settings-key-name := "ConsoleTools";
      block ()
	validate-one-license($library-pack-licensing-settings.data, 
			     $library-pack-licensing-settings.serial,
			     $library-pack-licensing-settings.expiration,
			     "Console-Based Developer Tools",
			     encode-edition(#"console-tools"), #t, <incorrect-edition>,
			     version,
			     $library-pack-installed-settings.installed);
	if ($library-pack-licensing-settings.expiration = "xxxx")
	  add!(products, #"console-tools")
	end
      exception (<license-validation-failure>)
	add!(products, #"console-tools")
      end
    end
  end;
  // Check library pack licenses
  for (library-pack :: <integer> in release-optional-library-packs())
    let key-name :: <byte-string>
      = format-to-string("LibraryPack.%s", integer-to-string(library-pack, size: 2));
    let pack-name :: <byte-string> = library-pack-full-name(library-pack);
    $library-pack-licensing-settings.settings-key-name := key-name;
    $library-pack-installed-settings.settings-key-name := key-name;
    block ()
      validate-one-license($library-pack-licensing-settings.data, 
			   $library-pack-licensing-settings.serial,
			   $library-pack-licensing-settings.expiration,
			   pack-name, 
			   library-pack, #t, <incorrect-library-pack>, 
			   version,
			   $library-pack-installed-settings.installed);
      if ($library-pack-licensing-settings.expiration = "xxxx")
	add!(products, library-pack)
      end
    exception (<license-validation-failure>)
      add!(products, library-pack)
    end;
  end;
  products
end function unregistered-products;

define function register-product
    (product, serial :: <byte-string>, key :: <byte-string>)
 => (registered? :: <boolean>)
  let serial :: <byte-string> = as-uppercase(serial);
  let key :: <byte-string> = as-lowercase(key);
  let expiration :: <byte-string> = copy-sequence(key, start: 12);
  let version :: <encoded-version> = encode-version(release-version-type());
  local method do-register-product
	    (product :: <byte-string>, 
	     edition :: <encoded-edition>,
	     edition-exact-match? :: <boolean>, 
	     edition-mismatch :: subclass(<edition-mismatch>),
	     version :: <encoded-version>,
	     installed-date :: <byte-string>)
	 => (data :: <byte-string>)
	  let supplied-edition :: <encoded-edition>
	    = encode-edition(copy-sequence(serial, start: 0, end: 5));
	  let supplied-version :: <encoded-version>
	    = encode-version(copy-sequence(serial, start: 6, end: 10));
	  let input :: <byte-string> = concatenate-as(<byte-string>, serial, expiration);
	  let computed-key :: <byte-string> = make(<byte-string>, size: 64, fill: '\0');
	  let used :: <integer>
	    = compute-sha-key(input, size(input), computed-key, size(computed-key));
	  if (copy-sequence(computed-key, start: 8, end: 20) = copy-sequence(key, end: 12))
	    // License key is valid -- Check if the user entered an expired key ...
	    let (year :: <integer>, month :: <integer>, day :: <integer>)
	      = decode-date(current-date());
	    let (expiry-year :: <integer>, expiry-month :: <integer>, 
		 expiry-day :: <integer>)
	      = if (expiration = "0000")
		  values($maximum-integer, $maximum-integer, $maximum-integer)
		elseif (expiration = "xxxx")
		  evaluation-end-date(installed-date)
		else
		  decode-expiration-date(expiration)
		end;
	    if (year > expiry-year 
		  | (year = expiry-year
		       & (month > expiry-month 
			    | (month = expiry-month & (day > expiry-day)))))
	      error(make(<license-expired>, 
			 expiration-year: expiry-year,
			 expiration-month: expiry-month,
			 expiration-day: expiry-day,
			 product: product,
			 installation: case
					 expiration = "xxxx"
					   => #"evaluation";
					 release-beta?()
					   => #"beta";
					 otherwise
					   => #"normal";
				       end))
	    end;
	    // Check if the user entered a key for the wrong edition of the product ...
	    if (edition > supplied-edition)
	      // License is for a less "comprehensive" edition ...
	      error(make(edition-mismatch, 
			 product: product,
			 expected-edition: supplied-edition,
			 actual-edition: edition))
	    elseif (edition-exact-match? & edition ~= supplied-edition)
	      // License is not for the same edition and the 
	      // caller requires it to be so (e.g., for Library Packs) ...
	      error(make(edition-mismatch, 
			 product: product,
			 expected-edition: supplied-edition,
			 actual-edition: edition))
	    end;
	    // Check if the user entered a key for the wrong version of the product ...
	    if (version > supplied-version)
	      error(make(<incorrect-version>,
			 product: product,
			 expected-version: supplied-version))
	    end;
	    // We get here iff the key is valid and has passed all consistency checks ...
	    copy-sequence(computed-key, end: used)
	  else
	    error(make(<invalid-key>, product: product))
	  end;
	end method do-register-product;
  case
    product = #"IDE" =>
      let edition :: <encoded-edition> = encode-edition(release-edition-type());
      let data :: <byte-string>
	= do-register-product(release-product-name(),
			      edition, #f, <incorrect-edition>,
			      version,
			      $current-version-settings.installed);
      $licensing-settings.data := data;
      $licensing-settings.serial := serial;
      $licensing-settings.expiration := expiration;
      #t;
    product = #"console-tools" => 
      // We'll use the Library Pack settings variables because it's an easy thing to do...
      $library-pack-licensing-settings.settings-key-name := "ConsoleTools";
      $library-pack-installed-settings.settings-key-name := "ConsoleTools";
      let data :: <byte-string>
	= do-register-product("Console-Based Tools",
			      encode-edition(#"console-tools"), #t, <incorrect-edition>,
			      version,
			      $library-pack-installed-settings.installed);
      $library-pack-licensing-settings.data := data;
      $library-pack-licensing-settings.serial := serial;
      $library-pack-licensing-settings.expiration := expiration;
      #t;
    otherwise =>
      let key-name :: <byte-string>
	= format-to-string("LibraryPack.%s", integer-to-string(product, size: 2));
      let pack-name :: <byte-string> = library-pack-full-name(product);
      $library-pack-licensing-settings.settings-key-name := key-name;
      $library-pack-installed-settings.settings-key-name := key-name;
      let data :: <byte-string>
	= do-register-product(pack-name, 
			      product, #t, <incorrect-library-pack>, 
			      version,
			      $library-pack-installed-settings.installed);
      $library-pack-licensing-settings.data := data;
      $library-pack-licensing-settings.serial := serial;
      $library-pack-licensing-settings.expiration := expiration;
      #t;
  end
end function register-product;
