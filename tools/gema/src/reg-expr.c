/*
 * regex - Regular expression pattern matching
 *         and replacement
 *
 * By:  Ozan S. Yigit (oz), Dept. of Computer Science, York University
 * Mods: Craig Durland
 *
 * These routines are the PUBLIC DOMAIN equivalents of regex routines as
 * found in 4.nBSD UN*X, with minor extensions.
 *
 * These routines are derived from various implementations found in software
 * tools books, and Conroy's grep.  They are NOT derived from
 * licensed/restricted software.  For more interesting/academic/complicated
 * implementations, see Henry Spencer's regexp routines, or GNU Emacs
 * pattern matching module.
 *
 * dfa = deterministic finite automata
 * Routines:
 *  regexp_comp: compile a regular expression into a DFA.
 *	char *regexp_comp(s)
 *	char *s;
 *	returns: NULL if OK, else error string
 *	If s is NULL or 0 length, last compiled pattern is used.
 *  regexp_exec: execute the DFA to match a pattern.
 *	int regexp_exec(s)
 *	char *s;
 *  regexp_subs: substitute the matched portions in a new string.
 *	int regexp_subs(src, dst)
 *	char *src;
 *	char *dst;
 *  regexp_fail:	failure routine for regexp_exec.
 *	void regexp_fail(msg, op)
 *	char *msg;
 *	char op;
 *  
 * Regular Expressions:
 *
 *      [1]     char    matches itself, unless it is a special
 *                      character (metachar): . \ [ ] * + ^ $
 *
 *      [2]     .       matches any character.
 *
 *      [3]     \       matches the character following it, except
 *			when followed by one of: ()123456789<> adnwW
 *			(see [7], [8] and [9])
 *			It is used as an escape character for all other
 *			meta-characters, and itself.  When used in a set
 *			([4]), it is treated as an ordinary character.
 *
 *      [4]     [set]   matches one of the characters in the set.
 *                      If the first character in the set is "^",
 *                      it matches a character NOT in the set. A
 *                      shorthand S-E is used to specify a set of
 *                      characters S upto E, inclusive. The special
 *                      characters "]" and "-" have no special
 *                      meaning if they appear as the first chars
 *                      in the set.
 *                      examples:        match:
 *
 *                              [a-z]    any lowercase alpha
 *
 *                              [^]-]    any char except ] and -
 *
 *                              [^A-Z]   any char except uppercase
 *                                       alpha
 *
 *                              [a-zA-Z] any alpha
 *
 *      [5]     *       any regular expression form [1] to [4], followed by
 *                      closure char (*) matches zero or more matches of
 *                      that form.
 *
 *      [6]     +       same as [5], except it matches one or more.
 *
 *      [7]             a regular expression in the form [1] to [10], enclosed
 *                      as \(form\) matches what form matches. The enclosure
 *                      creates a set of tags, used for [8] and for
 *                      pattern substution. The tagged forms are numbered
 *			starting from 1.
 *
 *      [8]             a \ followed by a digit 1 to 9 matches whatever a
 *                      previously tagged regular expression ([7]) matched.
 *
 *  I disabled [9] to be able to use the standard ctype
 *
 *	[9]	\<	a regular expression starting with a \< construct
 *		\>	and/or ending with a \> construct, restricts the
 *			pattern matching to the beginning of a word, and/or
 *			the end of a word. A word is defined to be a character
 *			string beginning and/or ending with the characters
 *			A-Z a-z 0-9 and _. It must also be preceded and/or
 *			followed by any character outside those mentioned.
 *
 *      [10]            a composite regular expression xy where x and y
 *                      are in the form [1] to [10] matches the longest
 *                      match of x followed by a match for y.
 *
 *      [11]	^	a regular expression starting with a ^ character
 *		$	and/or ending with a $ character, restricts the
 *                      pattern matching to the beginning of the line,
 *                      or the end of line. [anchors] Elsewhere in the
 *			pattern, ^ and $ are treated as ordinary characters.
 *
 * Acknowledgements:
 *   HCR's Hugh Redelmeier has been most helpful in various stages of
 *   development.  He convinced me to include BOW and EOW constructs,
 *   originally invented by Rob Pike at the University of Toronto.
 * References:
 *   Software tools		Kernighan & Plauger
 *   Software tools in Pascal	Kernighan & Plauger
 *   Grep [rsx-11 C dist]	David Conroy
 *   ed - text editor		Un*x Programmer's Manual
 *   Advanced editing on Un*x	B. W. Kernighan
 *   RegExp routines		Henry Spencer
 * Notes:
 *  This implementation uses a bit-set representation for character sets for
 *    speed and compactness.  Each character is represented by one bit in a
 *    128-bit block.  Thus, SET or NSET always takes a constant 16 bytes in
 *    the internal dfa, and regexp_exec does a single bit comparison to locate
 *    the character in the set.
 *  Put CLO in front of what gets closed for ease of interpreting.
 *  Put END at end of what gets closed to limit recursion.
 * Examples:
 *	pattern:	foo*.*
 *	compile:	CHR f CHR o CLO CHR o END CLO ANY END END
 *	matches:	fo foo fooo foobar fobar foxx ...
 *
 *	pattern:	fo[ob]a[rz]	
 *	compile:	CHR f CHR o SET bitset CHR a SET bitset END
 *	matches:	fobar fooar fobaz fooaz
 *
 *	pattern:	foo\\+
 *	compile:	CHR f CHR o CHR o CHR \ CLO CHR \ END END
 *	matches:	foo\ foo\\ foo\\\  ...
 *
 *	pattern:	\(foo\)[1-3]\1	(same as foo[1-3]foo)
 *	compile:	BOT 1 CHR f CHR o CHR o EOT 1 SET bitset REF 1 END
 *	matches:	foo1foo foo2foo foo3foo
 *
 *	pattern:	\(fo.*\)-\1
 *	compile:	BOT 1 CHR f CHR o CLO ANY END EOT 1 CHR - REF 1 END
 *	matches:	foo-foo fo-fo fob-fob foobar-foobar ...
 */

#include "reg-expr.h"
#include <ctype.h>

	/* ***************************************************** */
	/* ************ Constants ****************************** */
	/* ***************************************************** */

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

#ifndef NULL
#define NULL 0
#endif

	/* ***************************************************** */
	/* ************ Typedefs ******************************* */
	/* ***************************************************** */

typedef int  (*pfi)();		/* pointer to function returning int */
typedef void (*pfv)();		/* pointer to function returning void */

	/* ***************************************************** */
	/* ************ Macros ********************************* */
	/* ***************************************************** */

	/* size of static array */
#define NITEMS(array) (sizeof(array)/sizeof(array[0]))

	/* increment a type (usually pointer) by n bytes */
#define INC_TYPE(type,ptr,n) (type)((char *)ptr +n)

/* This is to allow arbitrary character remapping, for example
 * for case insensitive match
 * #define ceq(c1,c2)		\
 *	(_cmap[(unsigned char)(c1)] == _cmap[(unsigned char)(c2)])
*/
#define ceq(c1,c2) ((c1) == (c2))

#define MAXTAG  10

#define CHR     1	/* character		:: CHR<character> */
#define ANY     2	/* .			:: ANY */
#define SET     3	/* set: [...]		:: SET bitset */
#define NSET	4	/* not set: [^...]	:: SET bitset */
#define BOL     5	/* beginning of line: ^ :: BOL */
#define EOL     6	/* end of line: $	:: EOL */
#define BOT     7	/* beginning of tag: \( */
#define EOT     8	/* end of tag: \) */
#define BOW	9	/* beginning of word: \< */
#define EOW	10	/* end of word: \> */
#define REF     11	/* tag reference: \1,...,\9 */
#define CLO     12	/* closure: +, *	:: CLO dfa END */
#define SPACE	13	/* ": ": match isspace() */
#define ALPHA	14	/* :a	match isalpha() */
#define DIGIT	15	/* :d 	match isdigit() */
#define ALNUM	16	/* :n	match isalnum() */
#define WORD	17	/* :w	match isword() */
#define NWORD	18	/* :W	match !isword() */

#define END     0

/*
 * The following defines are not meant to be changeable.
 * They are for readability only.
 */
#define MAXCHR	128
#define CHRBIT	8
#define BITBLK	MAXCHR/CHRBIT
#define BLKIND	0x78
#define BITIND	0x7

#define CHAR unsigned char

static int
  tagstk[MAXTAG],		/* subpat tag stack */
  pattern_compiled = FALSE;	/* status of lastpat */

static CHAR bittab[BITBLK];	/* bit table for SET */

#define ISINSET(bittab,c) \
 ((bittab)[((((unsigned int)(c)) & BLKIND) >> 3)] & (CHAR) (1<<((c) & BITIND)))
#define CHSET(bittab,c) \
 (bittab)[((((unsigned int)(c)) & BLKIND) >> 3)] |= (CHAR) (1<<((c) & BITIND))

static void chset(c)  CHAR c; { CHSET(bittab,c); }

CHAR *regexp_dfa_buffer_too_short = 
  (CHAR *) "The buffer provided to store the compiled DFA is too small.";
#define DFA_SHORT ((char *) regexp_dfa_buffer_too_short)

#define badpat(msg) return (*dfa = END, (CHAR *)((dfa_short)?(DFA_SHORT):msg))
#define CHECK_RETURN() return ((CHAR *)((dfa_short)?(DFA_SHORT):NULL))
#define store(x) ((mp < dfa_limit)?(*mp++ = x):(dfa_short = 1))
 
	/* compile RE to internal format & store in dfa[] */
CHAR *regexp_comp (CHAR *pat, CHAR *dfa, int bufsize)
{
  CHAR
    *p,                         /* pattern pointer */
  *mp = dfa,                    /* dfa pointer */
  *lp,                          /* saved pointer */
  *sp = dfa;                    /* another one */
  CHAR *dfa_limit = dfa + bufsize;
  int dfa_short = 0;
  int
    tagi = 0,                   /* tag stack index */
  tagc = 1,                     /* actual tag count */
  n, c1, c2;

  if (pat==NULL || *pat=='\0') {
    if (pattern_compiled) CHECK_RETURN();
    else badpat("No previous regular expression");
  }
  pattern_compiled = FALSE;

  for (p = pat; *p; p++)
    {
      lp = mp;
      switch(*p)
        {
        case '.': store(ANY); break; /* match any characters */
        case '^':               /* match beginning of line */
          if (p==pat) store(BOL); else { store(CHR); store(*p); }
          break;
        case '$':               /* match end of line */
          if (*(p+1)=='\0') store(EOL); else { store(CHR); store(*p); }
          break;
        case '[':               /* match a set of characters */
          if (*++p=='^') { store(NSET); p++; } else store(SET);
          if (*p=='-') chset(*p++); /* real dash */
          if (*p==']') chset(*p++); /* real bracket */
          while (*p && *p!=']')
            {
              if (*p=='-' && *(p+1)!='\0' && *(p+1)!=']') /* a-z */
                {
                  p++;
                  c1 = *(p-2) +1; /* 'b' since 'a' already put into bittab */
                  c2 = *p++;    /* 'z' */
                  /* if (c1>c2) badpat("Empty set"); */	/* tried something like z-a */
                  while (c1<=c2) chset(c1++); /* build bit table */
                }
#ifdef EXTEND
              else if (*p=='\\' && *(p+1)) { p++; chset(*p++); }
#endif
              else chset(*p++);
            }
          if (*p=='\0') badpat("Missing ]");
          for (n = 0; n < BITBLK; bittab[n++] = '\0') store(bittab[n]);
          break;
        case '*':               /* match 0 or more of preceding RE */
        case '+':               /* match 1 or more.  Note: x+ == xx* */
          if (p==pat) badpat("Empty closure");
          lp = sp;		/* previous opcode */
          if (*lp==CLO) break;	/* equivalence: x** == x*  */
          switch(*lp)
            {
            case BOL: case BOT: case EOT: case BOW: case EOW: case REF:
              badpat("Illegal closure");
            }
          if (*p=='+') for (sp = mp; lp < sp; lp++) store(*lp);
          store(END); store(END); sp = mp;
          while (--mp > lp) *mp = mp[-1]; store(CLO); /* open hole for CLO */
          mp = sp;
          break;
        case '\\':              /* tags, backrefs */
          switch(*++p)
            {
            case '\0': badpat("Bad quote");
            case '(':
              if (tagc < MAXTAG)
                { tagstk[++tagi] = tagc; store(BOT); store(tagc++); }
              else badpat("Too many \\(\\) pairs");
              break;
            case ')':
              if (*sp==BOT) badpat("Null pattern inside \\(\\)");
              if (tagi > 0) { store(EOT); store(tagstk[tagi--]); }
              else badpat("Unmatched \\)");
              break;
            case '<': store(BOW); break;
            case '>':
              if (*sp==BOW) badpat("Null pattern inside \\<\\>");
              store(EOW);
              break;
            case '1': case '2': case '3': case '4': case '5': case '6': 
            case '7': case '8': case '9':
              n = *p-'0';
              if (tagi > 0 && tagstk[tagi]==n) badpat("Cyclical reference");
              if (tagc > n) { store(REF); store(n); }
              else badpat("Undetermined reference");
              break;
            case ' ': store(SPACE); break;
            case 'a': store(ALPHA); break;
            case 'd': store(DIGIT); break;
            case 'n': store(ALNUM); break;
            case 'w': store(WORD);  break;
            case 'W': store(NWORD); break;
#ifdef EXTEND
            case 'b': store(CHR); store('\b'); break;
            case 'n': store(CHR); store('\n'); break;
            case 'f': store(CHR); store('\f'); break;
            case 'r': store(CHR); store('\r'); break;
            case 't': store(CHR); store('\t'); break;
#endif
            default: store(CHR); store(*p);
            }
          break;
          default : store(CHR); store(*p); break; /* an ordinary character */
        }
      sp = lp;
    }
  if (tagi > 0) badpat("Unmatched \\(");
  store(END);
  pattern_compiled = TRUE;
  CHECK_RETURN();
}


static CHAR *bol;
CHAR *regexp_bopat[MAXTAG], *regexp_eopat[MAXTAG];
static CHAR *pmatch();
int regexp_errorcode;	/* sleaze */

/* regexp_exec:  execute dfa to find a match.
 *
 * special cases: (dfa[0])	
 *  BOL
 *	Match only once, starting from the beginning.
 *  CHR
 *	First locate the character without calling pmatch, and if found,
 *	call pmatch for the remaining string.
 *  END
 *	regexp_comp failed, poor luser did not check for it. Fail fast.
 *
 * If a match is found, regexp_bopat[0] and regexp_eopat[0] are set to 
 *   the beginning and the end of the matched fragment, respectively.
 *
 * Input:
 *	lp: string to search
 *	SoL==TRUE if lp starts line
 *	move==TRUE if search the entire string for match
 */

int regexp_exec (CHAR *lp, int SoL, int move, CHAR *dfa)
{
  CHAR *ap = dfa, c;
  CHAR *ep = NULL;

  regexp_errorcode = FALSE;     /* assume no match */
  bol = (SoL ? lp : NULL);

  regexp_bopat[0] = regexp_bopat[1] = regexp_bopat[2] = 
    regexp_bopat[3] = regexp_bopat[4] = regexp_bopat[5] = 
      regexp_bopat[6] = regexp_bopat[7] = regexp_bopat[8] = 
        regexp_bopat[9] = NULL;

  switch(*ap)
    {
    case END: return FALSE;     /* munged automaton. fail always */
    case BOL:                   /* anchored: match from BOL only */
      if (!SoL) return FALSE;   /* BoL can only be at front of dfa */
      ep = pmatch(lp,++ap); break;
    case CHR:                   /* ordinary char: locate it fast */
      if (move)
        {
          c = *(ap+1);
          while (*lp && !ceq(*lp,c)) lp++;
        }
    default:                    /* regular matching all the way. */
      if (!move) { ep = pmatch(lp,ap); break; }
      while (*lp)
        {
          if ((ep = pmatch(lp,ap))) break;
          lp++;
        }
    }
  if (!ep) return regexp_errorcode; /* only if pmatch() returns NULL */

  regexp_bopat[0] = lp; regexp_eopat[0] = ep; 
  return TRUE;
}

/* 
 * pmatch: internal routine for the hard part
 *
 * This code is mostly snarfed from an early grep written by David Conroy.
 *  The backref and tag stuff, and various other mods are by oZ.
 *
 * special cases: (dfa[n], dfa[n+1])
 *  CLO ANY
 *    We KNOW ".*" will match ANYTHING upto the end of line.  Thus, go to
 *    the end of line straight, without calling pmatch recursively.  As in
 *    the other closure cases, the remaining pattern must be matched by
 *    moving backwards on the string recursively, to find a match for xy (x
 *    is ".*" and y is the remaining pattern) where the match satisfies the
 *    LONGEST match for x followed by a match for y.
 *  CLO CHR
 *    Scan forward matching the single char without recursion, and at the
 *    point of failure, we execute the remaining dfa recursively, as
 *    described above.
 *
 * At the end of a successful match, regexp_bopat[n] and regexp_eopat[n]
 * are set to the beginning and end of subpatterns matched by tagged
 * expressions (n = 1 to 9).
*/

void regexp_fail (CHAR *msg, CHAR op);

	/* skip values for CLO XXX to skip past the closure */
#define ANYSKIP	2 		/* CLO ANY END ...	   */
#define CHRSKIP	3		/* CLO CHR chr END ...	   */
#define SETSKIP (2 +BITBLK)	/* CLO SET 16bytes END ... */

static CHAR *pmatch (CHAR *lp, CHAR *dfa)
{
  CHAR
    *e,                         /* extra pointer for CLO */
  *bp, *ep;                     /* beginning and ending of subpat */
  CHAR *are;                    /* to save the line ptr */
  int op, c, n;

  while ((op = *dfa++) != END)
    switch(op)
      {
      case CHR:	if (!ceq(*lp++,*dfa++)) return NULL; break;
      case ANY: if (*lp++=='\0') return NULL; break;
      case SET:
        c = *lp++;
	if (!ISINSET(dfa,c)) return NULL; /* ISINSET(dfa,0) is FALSE */
	dfa += BITBLK;
	break;
      case NSET:
	if ((c = *lp++) == '\0' || ISINSET(dfa,c)) return NULL;
	dfa += BITBLK;
	break;
      case EOL: if (*lp != '\0') return NULL; break;
      case BOT: regexp_bopat[*dfa++] = lp; break;
      case EOT: regexp_eopat[*dfa++] = lp; break;
      case ALNUM: if (!isalnum(*lp++)) return NULL; break;
      case ALPHA: if (!isalpha(*lp++)) return NULL; break;
      case DIGIT: if (!isdigit(*lp++)) return NULL; break;
      case SPACE: if (!isspace(*lp++)) return NULL; break;
#if 0
        *      case WORD:  if (!isword(*lp++))  return NULL; break;
        *      case NWORD: if (*lp == '\0' || isword(*lp++)) return NULL; break;
        *      case BOW:
        *        if (!(lp != bol && isword(lp[-1])) && isword(*lp)) break;
        *	return NULL;
        *      case EOW:        /* 'w\0' is OK here */
        *        if ((lp != bol && isword(lp[-1])) && !isword(*lp)) break;
        *	return NULL;
#endif
      case REF:                 /* !!! case_fold? */
        n = *dfa++; bp = regexp_bopat[n]; ep = regexp_eopat[n];
	while (bp < ep) if (*bp++ != *lp++) return NULL; /* !!! recurse? */
	break;
      case CLO:
        are = lp; n = ANYSKIP;
	switch(*dfa)
          {
	  case ANY:   while (*lp) lp++;		 break;
	  case ALNUM: while (isalnum(*lp)) lp++; break;
	  case ALPHA: while (isalpha(*lp)) lp++; break;
	  case DIGIT: while (isdigit(*lp)) lp++; break;
	  case SPACE: while (isspace(*lp)) lp++; break;
#if 0
            *	  case WORD:  while (isword(*lp))  lp++; break;
            *	  case NWORD: while (*lp && !isword(*lp)) lp++; break;
#endif
	  case CHR:
	    c = *(dfa+1);       /* we know c!='\0' */
	    while (ceq(*lp,c)) lp++;
	    n = CHRSKIP;
	    break;
	  case SET: case NSET:
	    while (*lp && (e = pmatch(lp,dfa))) lp = e;
	    n = SETSKIP;
	    break;
	  default: regexp_fail((unsigned char*)"closure: bad dfa.",
			       *dfa);
	    return NULL;
          }
	dfa += n;
	while (lp >= are)	/* backup up till match next pattern */
          {
	    e = pmatch(lp,dfa);
            if ( e ) return e;
            --lp;
          }
	return NULL;
      default: regexp_fail((unsigned char*)"regexp_exec: bad dfa.",(CHAR)op);
	return NULL;
      }
  return lp;
}

/*
 * regexp_subs: substitute the matched portions of the src in dst.
 *	&	substitute the entire matched pattern.
 *	\digit	substitute a subpattern, with the given
 *		tag number. Tags are numbered from 1 to
 *		9. If the particular tagged subpattern
 *		does not exist, null is substituted.
 * 	!!!Note: if the line that was used regexp_exec() has gone byebye
 *	  then \digit will blow cookies since the tags point into the line.
 */
int regexp_subs (CHAR *src, CHAR *dst)
{
  CHAR c, *bp, *ep;
  int pin;

  if (!regexp_bopat[0]) return FALSE;

  while ( (c = *src++) )
    {
      switch(c)
        {
        case '&': pin = 0; break;
        case '\\': 
          c = *src++;
          if (c >= '0' && c <= '9') { pin = c - '0'; break; }
        default: *dst++ = c; continue;
        }
      if ((bp = regexp_bopat[pin]) && (ep = regexp_eopat[pin]))
        {
          while (*bp && bp < ep) *dst++ = *bp++;
          if (bp < ep) return FALSE;
        }
    }
  *dst = '\0';
  return TRUE;
}


void 
regexp_fail (CHAR *msg, CHAR op)
{
}

/* ******************************************************************** */
/* ************************* DEBUG ************************************ */
/* ******************************************************************** */

#ifdef DEBUG
/*
 * symbolic - produce a symbolic dump of the dfa
 */
void symbolic (char *s)
{
  printf("pattern: %s\n", s);
  printf("dfacode:\n");
  dfadump(dfa);
}

static void
dfadump (CHAR *dfa)
{
  int n;

  while (*dfa != END)
    switch(*dfa++)
      {
      case CLO:
        printf("CLOSURE");
	dfadump(dfa);
	switch(*dfa)
          {
	  case CHR: n = CHRSKIP; break;
	  case ANY: n = ANYSKIP; break;
	  case SET: case NSET: n = SETSKIP; break;
          }
	dfa += n;
	break;
      case CHR: printf("\tCHR %c\n",*dfa++); break;
      case ANY: printf("\tANY .\n"); break;
      case BOL: printf("\tBOL -\n"); break;
      case EOL: printf("\tEOL -\n"); break;
      case BOT: printf("BOT: %d\n",*dfa++); break;
      case EOT: printf("EOT: %d\n",*dfa++); break;
      case BOW: printf("BOW\n"); break;
      case EOW: printf("EOW\n"); break;
      case REF: printf("REF: %d\n",*dfa++); break;
      case SET:
        printf("\tSET [");
	for (n = 0; n < MAXCHR; n++)
	  if (ISINSET(dfa,(CHAR)n)) printf("%c",n);
	printf("]\n");
	dfa += BITBLK;
	break;
      case NSET:
        printf("\tNSET [");
	for (n = 0; n < MAXCHR; n++)
	  if (ISINSET(dfa,(CHAR)n)) printf("%c",n);
	printf("]\n"); dfa += BITBLK;
	break;
      default:
        printf("bad dfa. opcode %o\n", dfa[-1]);
	exit(1);
	break;
      }
}
#endif

