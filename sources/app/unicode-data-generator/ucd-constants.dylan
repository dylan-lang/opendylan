Module: unicode-data-generator
Synopsis: UCD database constants
Author: Ingo Albrecht <prom@berlin.ccc.de>
Copyright:    Original Code is Copyright (c) 2014 Dylan Hackers
              All rights reserved.
License:      See License.txt in this distribution for details.

/* Number of planes in the unicode codespace
 *
 * This value may change with new versions of the standard.
 */
define constant $u-plane-count = 17;

/* Size of a unicode plane
 *
 * This truly is constant.
 */
define constant $u-plane-size  = 2 ^ 16;

/* Number of valid codepoints in the codespace
 */
define constant $u-code-count = $u-plane-count * $u-plane-size;

