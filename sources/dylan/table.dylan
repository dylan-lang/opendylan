module: internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Parameterization of the table implementation.
//
// These are pretty much just picked out of a hat, and might benefit
// from benchmarking and tuning.

// The fraction of entries that can be in use in a table vector before
// it needs to be rehashed.  This and the entry count determine the
// additions limit for a table vector.
define constant $grow-threshold = 0.75;

// When rehashing for growth, this is the minimum number of entries
// to add.  The new desired size is the larger of the current size
// adjusted by the growth rate and the current size plus this value.
define constant $minimum-growth-amount = ceiling/(10, $grow-threshold);

// The default growth rate if the caller of make on the table class
// doesn't supply a value.
define constant $default-growth-rate = 2.0;

// The default number of elements to initially allocate space for.
// This is the number of elements that can be added before a rehash
// is needed.
define constant $default-table-size = 10;

// The minimum number of entries in a table vector.
// Must be larger than the largest prime used in $secondary-strides.
// Must be a prime number.
define constant $minimum-entries = 31;


define constant $entry-counts =
  #[      2,       3,       5,       7,      11,      13,      17,      19,
         23,      29,      31,      37,      41,      47,      53,      59,
         67,      71,      79,      83,      89,      97,     103,     109,
        127,     137,     149,     157,     167,     179,     191,     211,
        223,     239,     251,     269,     283,     307,     331,     349,
        367,     389,     409,     431,     457,     487,     521,     557,
        587,     617,     653,     691,     727,     769,     809,     853,
        907,     953,    1009,    1061,    1117,    1181,    1249,    1319,
       1399,    1471,    1549,    1627,    1709,    1801,    1901,    1997,
       2099,    2207,    2333,    2459,    2591,    2729,    2879,    3023,
       3181,    3343,    3511,    3691,    3877,    4073,    4283,    4507,
       4733,    4973,    5227,    5501,    5779,    6073,    6379,    6701,
       7039,    7393,    7789,    8179,    8597,    9029,    9491,    9967,
      10477,   11003,   11579,   12161,   12781,   13421,   14107,   14813,
      15559,   16339,   17159,   18041,   18947,   19913,   20921,   21977,
      23081,   24239,   25453,   26729,   28069,   29473,   30949,   32497,
      34123,   35831,   37633,   39521,   41507,   43591,   45779,   48073,
      50497,   55547,   61121,   67247,   73973,   81371,   89513,   98467,
     108343,  119179,  131101,  144223,  158647,  174527,  191999,  211199,
     232333,  255571,  281131,  309251,  340183,  374203,  411637,  452807,
     498089,  547901,  602711,  662999,  729301,  802253,  882481,  970747,
    1067831, 1174619, 1292089, 1421309, 1563449, 1719799, 1891789, 2080979,
    2289083, 2518003, 2769841, 3046829, 3351521, 3686677, 4055347, 4460891,
    4906991, 5397697, 5937469, 6531221, 7184351, 7902787, 8693071, 9562387];

define constant $entry-last :: <integer> = $entry-counts.size - 1;


// Search the entry-counts vector for the smallest entry that is not
// smaller than the value of needed.  The caller must have already
// verified that needed is not greater than the element for last-index.
define function search-for-entry-count
     (needed :: <integer>)
  => count :: <integer>;
  local method loop (lower :: <integer>, upper :: <integer>)
          // binary search over the vector of entry counts to find the
          // smallest good entry count that is not smaller than the
          // needed number of entries.
          if (lower = upper)
            vector-element($entry-counts, lower);
          else
            let index = ash(lower + upper, -1);
            let elt :: <integer> = vector-element($entry-counts, index);
            if (elt < needed)
              loop(index + 1, upper);
            else
              loop(lower, index);
            end if;
          end if;
        end method loop;
  loop(0, $entry-last);
end;

// Compute an entry count suitable for the desired number of entries.
// The entry count allows for space to account for the growth threshold,
// and is rounded up to a prime.
define function compute-entry-count (desired :: <integer>)
  => count :: <integer>;
  if (desired < 0)
    // Maybe this should error instead.
    compute-entry-count(0);
  else
    let needed = max(ceiling/(desired, $grow-threshold), $minimum-entries);
    if (needed <= vector-element($entry-counts, $entry-last))
      search-for-entry-count(needed);
    else
      // Signal some more specific class of error?
      error("Desired size %d exceeds maximum table vector size.",
            desired);
    end if;
  end if;
end;


//
// ---------------- Functions on table vectors ------------

// Cache of the test function returned by table-protocol when the
// table was created.  It's cached in a slot for faster access,
// avoiding the generic function call to table-protocol each time
// the table is accessed.
define generic test-function (t :: <table-vector>)
  => fun :: <function>;


// Cache of the hash function returned by table-protocol when the
// table was created.  It's cached in a slot for faster access,
// avoiding the generic function call to table-protocol each time
// the table is accessed.
define generic hash-function (t :: <table-vector>)
  => fun :: <function>;


// The rehash token is used to detect a rehash in progress or completed.
// This is only needed in order to support inplace rehash.
// Operations which need to detect a rehash should get the token, perform
// whatever part of the operation can be done without locking the tv,
// and then verify the token before completing the operation.  If the
// operation is destructive, then the token should be verified while
// holding the tv's lock.
//
// The function rehash-token-valid?() is used to verify a token.
// The function make-rehash-token() is used to create a token.
define generic rehash-token (tv :: <table-vector>) => rt :: <rehash-token>;
define generic rehash-token-setter (t :: <rehash-token>, tv :: <table-vector>)
  => t :: <rehash-token>;


// Returns the number of entries that have been added to the vector.
define generic additions
  (tv :: <table-vector>) => adds :: <integer>;
define generic additions-setter
    (adds :: <integer>, tv :: <table-vector>)
 => (adds :: <integer>);


// Returns the maximum number of entries that can be added to the vector
// before a rehash is required.
// --- What is the interaction with deletions?
define generic additions-limit (tv :: <table-vector>)
  => limit :: <integer>;

// Returns the number of entries that have been deleted from the vector.
define generic deletions (tv :: <table-vector>) => dels :: <integer>;
define generic deletions-setter (dels :: <integer>, tv :: <table-vector>)
  => dels :: <integer>;

// The number of entries for which space exists in the table-vector.
define generic entry-count (tv :: <table-vector>)
  => count :: <integer>;


// ---------------- accessing keys and values ----------------
// Keys and values are stored in separate "virtual" vectors.  Whether either
// of those vectors is collocated with the table-vector is no business of
// anyone but the functions for getting the vectors, the functions for
// accessing elements, and $entry-index-delta.
//
// As it happens, both the keys and the values are stored in separate
// vectors which the table-vector points to.  This is needed so that the
// we can allocate either of them as weak, in order to implement weak
// tables.  (The most recent design for weakness from the mm group would
// provide us with a mechanism for specifying when allocating an object
// that either all references within it are normal or they are all weak.)

// The type for the key vector might eventually change.  It would be nice
// if we could compute a limited <simple-vector> type from the specializers
// for the test function (or from some other mechanism) and make use of that
// to limit the element type, but it's not clear that this could really be
// done in a reasonable way.  Even if we could do that, it would add some
// additional complications because we would not be able to use special
// markers in the key vector to indicate empty or deleted, and would instead
// have to use some other mechanism which might introduce difficult threads
// issues.  Also note that we don't need to do bounds checking when
// accessing the elements of the entry-key vector.

define sealed primary class <entry-vector> (<vector>)
  slot partner = #f, init-keyword: partner:;
  repeated slot entry-element,
    init-keyword: fill:,
    init-value: $table-entry-empty,
    size-getter: size,
    size-init-keyword: size:,
    size-init-value: 0;
end;

ignore(partner); // used by mps alone

define sealed domain size (<entry-vector>);
define sealed domain make (singleton(<entry-vector>));
define sealed domain initialize(singleton(<entry-vector>));

define constant <entry-keys> = <entry-vector>;

define generic entry-keys (tv :: <table-vector>)
  => keys :: <entry-keys>;

define generic entry-key (v :: <entry-keys>, index :: <integer>)
  => key :: <object>;

define generic entry-key-setter
    (new-key :: <object>, v :: <entry-keys>, index :: <integer>)
  => new-key :: <object>;

// The type might change eventually.  It would be nice if it could be
// a limited <simple-vector> of the table's element-type.
// Also note that we don't need to do bounds checking when accessing
// the elements of the entry-value vector.

define constant <entry-values> = <entry-vector>;

define generic entry-values (tv :: <table-vector>)
  => values :: <entry-values>;

define generic entry-value (v :: <entry-values>, index :: <integer>)
  => value :: <object>;

define generic entry-value-setter
    (new-value :: <object>, v :: <entry-values>, index :: <integer>)
  => new-value :: <object>;

// ---------------- rehash tokens ----------------

define sealed class <rehash-token> (<object>)
  slot rehashing? :: <boolean> = #f, init-keyword: rehashing?:;
end class;

define sealed domain make (singleton(<rehash-token>));
define sealed domain initialize (<rehash-token>);

define function make-rehash-token (rehash?)
  => rt :: <rehash-token>;
  make(<rehash-token>, rehashing?: rehash?)
end;

define inline function rehash-token-valid?
     (tv :: <table-vector>, token :: <rehash-token>)
  => valid? :: <boolean>;
  (~rehashing?(token)) & pointer-id?(token, rehash-token(tv));
end;

define inline function mark-token-rehashing (token :: <rehash-token>) => ()
  rehashing?(token) := #t
end function;


// ----------------- rehashed-bits ---------------------------

define constant $rehashed-bits-word-size :: <integer> = 32;
define constant $rehashed-bits-log-word-size :: <integer> = 5;

define constant $rehashed-bits-machine-word-zero       = as(<machine-word>, 0);
define constant $rehashed-bits-machine-word-one        = as(<machine-word>, 1);
// define constant $rehashed-bits-machine-word-minus-one  = as(<machine-word>, -1);



define sealed inline-only generic rehashed-bits-word-setter (a :: <raw-machine-word>, b, c)
  => (v :: <raw-machine-word>);


define sealed concrete primary class <rehashed-bits> (<object>)
//  slot size :: <integer> = 0, init-keyword: #"size";
  repeated sealed inline slot rehashed-bits-word :: <raw-machine-word>,
    init-value: $rehashed-bits-machine-word-zero,  // Except this is ignored?
//    init-keyword: word-fill,
    size-getter: size-in-words,
    size-init-keyword: size-in-words:,
    size-init-value: 0;
end class;

define sealed domain make (singleton(<rehashed-bits>));
define sealed domain initialize (<rehashed-bits>);


define constant $empty-rehashed-bits = make(<rehashed-bits>, size: 0);


define function rehashed-bit? (vector :: <rehashed-bits>, index :: <integer>)
 => (rehashed? :: <boolean>)
  primitive-machine-word-logbit?(integer-as-raw(logand(index, ($rehashed-bits-word-size - 1))),
                                 rehashed-bits-word(vector, ash(index, -$rehashed-bits-log-word-size)));
end function;


define function rehashed-bit?-setter
    (rehashed? :: <boolean>, vector :: <rehashed-bits>, index :: <integer>)
 => (rehashed? :: <boolean>)

  let word-offset :: <integer> = ash(index, -$rehashed-bits-log-word-size);
  let bit-offset :: <integer> = logand(index, ($rehashed-bits-word-size - 1));
  let word :: <machine-word>
    = primitive-wrap-machine-word(rehashed-bits-word(vector, word-offset));
  let new-word :: <machine-word>
    = if (~rehashed?)
        machine-word-logand
          (word, machine-word-lognot
             (machine-word-shift-left-with-overflow
                ($rehashed-bits-machine-word-one, bit-offset)));
      else
        machine-word-logior
          (word, machine-word-shift-left-with-overflow
             ($rehashed-bits-machine-word-one, bit-offset));
      end if;
  rehashed-bits-word(vector, word-offset)
    := primitive-unwrap-machine-word(new-word);
  rehashed?
end function;


define function clear-rehashed-bits (v :: <rehashed-bits>) => ()
  for (i :: <integer> from 0 below size-in-words(v))
    rehashed-bits-word(v, i) := primitive-unwrap-machine-word($rehashed-bits-machine-word-zero)
  end for
end function;


// ---------------- <table-vector> ----------------

define class <table-vector> (<object>)
  constant slot table-vector-lock :: <simple-lock>,
    required-init-keyword: table-lock:;
  constant slot test-function :: <function>,
    required-init-keyword: test-function:;
  constant slot hash-function :: <function>,
    required-init-keyword: hash-function:;
  slot rehash-token :: <rehash-token>,
    init-function: method () make-rehash-token(#f) end;
  slot additions :: <integer>,
    init-value: 0;
  constant slot additions-limit :: <integer>,
    required-init-keyword: rehash-limit:;
  slot deletions :: <integer>,
    init-value: 0;
  slot hash-state :: <hash-state> = make(<hash-state>);
  constant slot entry-keys :: <entry-keys>,
    required-init-keyword: keys:;
  constant slot entry-values :: <entry-values>,
    required-init-keyword: values:;
  slot iteration-state-references :: <integer> = 0;
  slot rehashed-bits :: <rehashed-bits> = $empty-rehashed-bits;
end class <table-vector>;

define sealed domain make (singleton(<table-vector>));
define sealed domain initialize (<table-vector>);

define inline method entry-count (tv :: <table-vector>)
  => count :: <integer>;
  size(entry-keys(tv));
end method entry-count;

// These methods for key and value access don't need to do bounds checking.

// Ideally the MM should use $table-entry-deleted for zapped entries, rather
// than 0.  If this turns out to be impossible then we should rearrange the
// code to avoid multiple checks for deleted entries.

define constant $raw-table-entry-deleted =
  coerce-abstract-integer-to-machine-word(0);

define inline method entry-key
     (keys :: <entry-keys>, index :: <integer>)
  => key :: <object>;
  let key = entry-element(keys, index);
  if (primitive-machine-word-equal?
        (primitive-cast-pointer-as-raw(key),
         primitive-unwrap-machine-word($raw-table-entry-deleted)))
    $table-entry-deleted
  else
    key
  end
end method entry-key;

define inline method entry-key-setter
    (new-key :: <object>, keys :: <entry-keys>, index :: <integer>)
  => new-key :: <object>;
  entry-element(keys, index) := new-key;
end method entry-key-setter;

define inline method entry-value
     (vals :: <entry-values>, index :: <integer>)
  => value :: <object>;
  let val = entry-element(vals, index);
  if (primitive-machine-word-equal?
        (primitive-cast-pointer-as-raw(val),
         primitive-unwrap-machine-word($raw-table-entry-deleted)))
    $table-entry-deleted
  else
    val
  end
end method entry-value;

define inline method entry-value-setter
    (new-value :: <object>, vals :: <entry-values>, index :: <integer>)
  => new-value :: <object>;
  entry-element(vals, index) := new-value;
end method entry-value-setter;

// Unique markers for the table vector data.
define constant $table-entry-empty :: <pair> = #("empty");
define constant $table-entry-deleted :: <pair> = #("deleted");

define inline function table-entry-deleted? (entry) => (well? :: <boolean>)
  pointer-id?(entry, $table-entry-deleted)
end;


// ----------------- utilities ---------------------------

define inline function entry-index-limit (tv :: <table-vector>)
  => limit :: <integer>;
  entry-count(tv);
end;

define constant $entry-index-delta :: <integer> = 1;

// Currently we use a lock to ensure single process update.  However, it
// might be more efficient to use the technique outlined for stretchy
// vectors, in which there is a boolean atomic slot which is tested and
// set when entering the "critical" region, with failure of the test
// indicating unsynchronized access and signaling an error in that case.
// Currently we attempt to limit the extent of the locking as much as
// possible to minimize the possibility of contention.  However, in the
// error signaling approach it might be better to expand the region in
// order to simplify the implementation.

define macro with-table-vector-locked
  { with-table-vector-locked (?tv:expression) ?:body end }
    => { with-lock (?tv.table-vector-lock) ?body end }
end;

// Callers must ensure that tv is locked to get a reliable result.
define function present-entry-count (tv :: <table-vector>)
  => count :: <integer>;
  additions(tv) - deletions(tv);
end;

define function full? (tv :: <table-vector>)
  => full? :: <boolean>;
  additions(tv) >= additions-limit(tv);
end;

define inline function needs-rehash?
     (tv :: <table-vector>, token :: <rehash-token>)
  => result :: <boolean>;
  ~rehash-token-valid?(tv, token) | is-stale?(hash-state(tv));
end;

define thread variable $default-hash-state = make(<hash-state>);

define function increment-iteration-state-references (tv :: <table-vector>)
  with-table-vector-locked (tv)
    let refs :: <integer> = iteration-state-references(tv);
  if (refs < $maximum-integer) iteration-state-references(tv) := refs + 1; end if;
  end with-table-vector-locked;
end function;

define function decrement-iteration-state-references (tv :: <table-vector>)
  with-table-vector-locked (tv)
    let refs :: <integer> = iteration-state-references(tv);
    if (refs < $maximum-integer) iteration-state-references(tv) := refs - 1; end if;
  end with-table-vector-locked;
end function;



// The following function should be called for any thread which
// is to run Dylan code, apart from the master thread

define inline function initialize-default-hash-state ()
  $default-hash-state := make(<hash-state>);
end;


define inline function hash (tv :: <table-vector>, key)
 => (id :: <integer>, hash-state :: <hash-state>)

  // I suppose we had better play safe and assume that user-defined
  // hashing functions can recursively call tables (yuk).  So we indicate the
  // thread-local hash-state is in use by setting it to #f during the call to
  // the hashing function.  If a recursive use is detected then we just
  // allocate a new hash state.

  if ($default-hash-state)
    let thread-hash-state = $default-hash-state;
    $default-hash-state := #f;
    primitive-mps-ld-reset(thread-hash-state);
    let (id :: <integer>, hash-state :: <hash-state>) =
      (hash-function(tv))(key, thread-hash-state);
    $default-hash-state := thread-hash-state;
    values(id, hash-state)
  else
    (hash-function(tv))(key, make(<hash-state>))
  end
end;


define constant $dummy-hash-state = make(<hash-state>);

define inline function hash-for-lookup (tv :: <table-vector>, key)
 => (id :: <integer>)

  (hash-function(tv))(key, $dummy-hash-state)
end;

// --------------------- creating table vectors --------------------

define inline function make-table-entries
    (count :: <integer>, values? :: <boolean>, weak?)
 => (keys :: <entry-keys>, vals :: <entry-values>)
  if (values?)
    select (weak?)
      key: =>
        let vals :: <entry-values> = system-allocate-strong-repeated-instance
          (<entry-values>, count, $table-entry-empty);
        let keys :: <entry-keys> = system-allocate-weak-repeated-instance
          (<entry-keys>, count, $table-entry-empty, vals);
        vals.partner := keys;
        values(keys, vals);
      value: =>
        let keys :: <entry-values> = system-allocate-strong-repeated-instance
          (<entry-keys>, count, $table-entry-empty);
        let vals :: <entry-values> = system-allocate-weak-repeated-instance
          (<entry-values>, count, $table-entry-empty, keys);
        keys.partner := vals;
        values(keys, vals);
      otherwise =>
        values(make(<entry-keys>,   size: count),
               make(<entry-values>, size: count));
    end
  else
    let keys = make(<entry-keys>, size: count);
    values(keys, keys)
  end
end;


define function make-table-vector
     (desired-entries :: <integer>,
      test-function :: <function>,
      hash-function :: <function>,
      lock :: <simple-lock>,
      values? :: <boolean>,
      weak?)
  => tv :: <table-vector>;
  let count :: <integer> = compute-entry-count(desired-entries);
  let (keys, vals) = make-table-entries(count, values?, weak?);

  make(<table-vector>,
       test-function: test-function,
       hash-function: hash-function,
       rehash-limit: ceiling(count * $grow-threshold),
       table-lock: lock,
       keys: keys, values: vals);
end;


// ---------------- the table lock pool ----------------
//
// As locks may be in short supply, we cannot allocate a lock for each
// individual table.  Instead we create a pool of locks, and then use
// these in a cyclic fashion.

define constant $table-lock-pool-size = 20;

define constant $table-lock-pool :: <simple-object-vector> =
  make(<vector>, size: $table-lock-pool-size);

define variable *table-lock-pool-index* :: <integer> = 0;

$table-lock-pool[0] := make-simple-lock();

define function make-table-lock() => (lock :: <simple-lock>)
  *table-lock-pool-index* :=
    modulo(*table-lock-pool-index* + 1, $table-lock-pool-size);
  let lock = $table-lock-pool[*table-lock-pool-index*];
  if (lock)
    lock
  else
    $table-lock-pool[*table-lock-pool-index*] := make-simple-lock()
  end;
end;


// ---------------- the initial table vector ----------------
//
// The initial table vector is a table vector for which all accesses
// will fail because the test and hash functions signal an error.
//
// This object exists solely to provide an initial value for the
// table-vector slot of tables, so that said slot can be typed and
// guaranteed to be initialized, making accesses fast.

define function uninitialized-table-test (key1, key2)
  error("Invocation of uninitialized table test function on %= and %=.",
        key1,
        key2);
end;

define function uninitialized-table-hash (key)
  error("Invocation of uninitialized table hash function on %=.", key);
end;

define constant *initial-table-vector* =
  make-table-vector(0, uninitialized-table-test, uninitialized-table-hash,
                    $table-lock-pool[0], #t, #f);

define function initial-table-vector ()
  => tv :: <table-vector>;
  *initial-table-vector*;
end;

// ---------------- search table vectors --------------------

// Mask for the length of the $secondary-strides vector.
// The length must be a power of 2.
// size($secondary-strides) - 1;
define constant $secondary-strides-mask :: <integer> = 7;

// A sequence of small primes, scaled by multiplying by $index-entry-delta.
// The length of the sequence is a power of two for the mask.  All the primes
// on which this sequence is based must be less than the minimum number of
// entries in a table vector (see $minimum-entries), so that the secondary
// lookup loop in table-vector-lookup can do a quick modulo operation that
// doesn't involve a division.  Also, small strides are probably better for
// locality in the search.  However, the period for collision chain pairs
// (same primary index, different secondary strides) is the product of the
// secondary strides, which makes small secondary strides less desirable.
define constant $secondary-strides :: <simple-object-vector>
  = #[5, 7, 11, 13, 17, 19, 23, 29];

// Returns the key index for the first probe.
// count is the number of entries in the table vector (a prime).
define inline function primary-index
     (hash-id :: <integer>, count :: <integer>)
  => index :: <integer>;
  // --- If we need to deal with an architecture where a slow division
  // --- operation is a significant performance problem, then I think
  // --- the following alternative to using modulo should work:
  // ---
  // ---   Compute at table-vector allocation time a suitably scaled
  // ---   approximation to 1/entry-count.  Compute the double word
  // ---   product of hash-id and that approximation, and scale that
  // ---   result to get the desired index.
  modulo(hash-id, count) * $entry-index-delta;
end;

// Returns the offset to add (modulo the number of entries) to the
// key index to get the next key index for a secondary probe.
define inline function secondary-stride (hash-id :: <integer>)
  => stride :: <integer>;
  vector-element($secondary-strides, logand(hash-id, $secondary-strides-mask));
end;

// --- A specialized version of search will eventually be needed for use
// --- by in-place rehash.  The difference is that the rehash search will
// --- receive a vector indicating which entries have already been rehashed,
// --- and a non-rehashed entry is treated as empty for purposes of finding
// --- the entry for a key.
//
// --- A specialized version of search for <object-table> might improve
// --- performance a little bit.  The idea is to keep track of whether any
// --- of the keys require more than pointer equivalence for comparison,
// --- in order to speed up the comparisons by avoiding a generalized call
// --- to \==.

// Search for the table vector for the index to use for key.
// Returns the index and the current key.  ?table-key is
// bound to the table key while ?test is executing.
define macro do-search
  { do-search (?table-key:name in (?tv:name, ?id:name)) ?test:body end }
    =>
    { begin
        let index = primary-index(?id, entry-count(?tv));
        let keys = entry-keys(?tv);
        let ?table-key = entry-key(keys, index);
        if (pointer-id?(?table-key, $table-entry-empty)
              // Check if the keys match, with deleted entries never matching
              | (~table-entry-deleted?(?table-key)
                   & begin ?test end))
          values(index, ?table-key);
        else
          let stride = secondary-stride(?id);
          let adjust = entry-count(?tv);
          local method next-index (index :: <integer>)
                 => next :: <integer>;
                  let next :: <integer> = index - stride;
                  if (next < 0) next + adjust;
                  else next;
                  end if;
                end method next-index,
            method loop
                (previous-index :: <integer>) => (index :: <integer>, table-key)
              let index = next-index(previous-index);
              let ?table-key = entry-key(keys, index);
              if ((pointer-id?(?table-key, $table-entry-empty)
                     | (~table-entry-deleted?(?table-key)
                         & begin ?test end)))
                values(index, ?table-key);
              else
                loop(index);
              end if;
            end method;
          loop(index);
        end if;
      end }
end macro;

define inline function search
     (tv :: <table-vector>, key, id :: <integer>)
  => (index :: <integer>, table-key);
  let test :: <function> = test-function(tv);
  if (test ~== \==)
    do-search(table-key in (tv, id)) test(key, table-key) end;
  elseif (value-object?(key))
    do-search(table-key in (tv, id)) key == table-key end;
  else
    do-search(table-key in (tv, id)) pointer-id?(key, table-key) end;
  end if;
end;

define inline function find-rehash-insertion-point
    (tv :: <table-vector>, key, id :: <integer>, bits :: <rehashed-bits>)
 => (index :: <integer>, table-key);
  let index = primary-index(id, entry-count(tv));
  let keys = entry-keys(tv);
  let table-key = entry-key(keys, index);
  if (pointer-id?(table-key, $table-entry-empty)
        | table-entry-deleted?(table-key)
        | ~rehashed-bit?(bits, index))
    values(index, table-key)
  else
    let stride = secondary-stride(id);
    let adjust = entry-count(tv);
    local method next-index (index :: <integer>)
           => next :: <integer>;
            let next :: <integer> = index - stride;
            if (next < 0) next + adjust else next
            end if
          end method next-index,
      method loop (previous-index :: <integer>) => (index :: <integer>, table-key)
        let index = next-index(previous-index);
        let table-key = entry-key(keys, index);
        if (pointer-id?(table-key, $table-entry-empty)
              | table-entry-deleted?(table-key)
              | ~rehashed-bit?(bits, index))
          values(index, table-key)
        else
          loop(index)
        end if
      end method;
    loop(index)
  end if
end;

//


define method clear-table-vector! (tv :: <table-vector>) => ()
  deletions(tv) := 0;
  additions(tv) := 0;
  let keys :: <entry-keys> = entry-keys(tv);
  for (i :: <integer> from 0 below size(keys))
    entry-element(keys, i) := $table-entry-empty;
  end for;
  let vals :: <entry-values> = entry-values(tv);
  if (vals ~== keys)
    for (i :: <integer> from 0 below size(vals))
      entry-element(vals, i) := $table-entry-empty;
    end for;
  end if;
  hash-state(tv)   := make(<hash-state>);
  synchronize-side-effects();
  rehash-token(tv) := make-rehash-token(#f);
end method;


define variable *rehash-table-vectors-in-place?* = #t;

define variable *in-place-rehash-count* :: <integer> = 0;
define variable *punted-in-place-rehash-count* :: <integer> = 0;


// Table vector needs to be locked here, else someone could start an iteration
// after we do that check.
define inline function in-place-rehashable? (tv :: <table-vector>) => (well? :: <boolean>)
  // *rehash-table-vectors-in-place?* & iteration-state-references(tv) == 0
  if (*rehash-table-vectors-in-place?*)
    if (iteration-state-references(tv) == 0)
      *in-place-rehash-count* := *in-place-rehash-count* + 1;
      #t
    else
      *punted-in-place-rehash-count* := *punted-in-place-rehash-count* + 1;
      #f
    end if
  else
    #f
  end if
end function;


define function rehash-table
    (table :: <table>, tv :: <table-vector>, grow? :: <boolean>) => ()
  with-table-vector-locked (tv)
    if (pointer-id?(tv, table-vector(table)))
      mark-rehashing(tv);
      let values? = ~pointer-id?(tv.entry-values, tv.entry-keys);
      if (~grow? & in-place-rehashable?(tv))
        rehash-in-place(table, tv, values?)
      else
        let rehash-tv
          = make-table-vector(rehash-entry-count(table, tv, grow?),
                              test-function(tv),
                              hash-function(tv),
                              table-vector-lock(tv),
                              values?, weak?(table));
        rehash-into-copy(table, tv, rehash-tv);
      end if;
    end if;
  end with-table-vector-locked;
end;

// This function should only be called with the tv locked.
define function mark-rehashing (tv :: <table-vector>)
//  rehash-token(tv) := make-rehash-token(#t);
  mark-token-rehashing(rehash-token(tv));
  // Ensure that the modification is globally visible before proceeding.
  synchronize-side-effects();
end;

define function rehash-into-copy
     (table :: <table>, src :: <table-vector>, dst :: <table-vector>)
  let src-keys = entry-keys(src);
  let src-values = entry-values(src);
  let dst-keys = entry-keys(dst);
  let dst-values = entry-values(dst);
  local method loop (index :: <integer>,
                     state :: <hash-state>, count :: <integer>)
          if (negative?(index))
            additions(dst) := count;
            hash-state(dst) := state;
            // Ensure above updates occur before installation of dst.
            // This is necessary because we are not currently holding the
            // lock for dst, so installing it without first ensuring that
            // it is consistent could allow some other thread to see a
            // bogus state.
            synchronize-side-effects();
            table-vector(table) := dst;
          else
            let key = entry-key(src-keys, index);
            if (pointer-id?(key, $table-entry-empty) | table-entry-deleted?(key))
              loop(index - $entry-index-delta, state, count);
            else
              let (id, hstate) = hash(dst, key);
              let dst-index = search(dst, key, id);
              entry-key(dst-keys, dst-index) := key;
              entry-value(dst-values, dst-index)
                := entry-value(src-values, index);
              loop(index - $entry-index-delta,
                   merge-hash-state!(state, hstate),
                   count + 1);
            end if;
          end if;
        end method loop;
  loop(entry-index-limit(src) - $entry-index-delta, hash-state(dst), 0);
end;



define function rehash-in-place (table :: <table>, tv :: <table-vector>, values? :: <boolean>) => ()
  let lim :: <integer> = entry-index-limit(tv);
  let bits :: <rehashed-bits>
    = if (rehashed-bits(tv) == $empty-rehashed-bits)
        rehashed-bits(tv) := make(<rehashed-bits>,
                                  size-in-words: ash(logior(lim, 7), -3))
      else
        rehashed-bits(tv)
      end if;
  clear-rehashed-bits(bits);
  let keys = entry-keys(tv);
  let (count :: <integer>, state :: <hash-state>)
    = if (values?)
        let vals = entry-values(tv);
        local method loop (i :: <integer>, state :: <hash-state>, count :: <integer>)
                if (i < 0)
                  values(count, state)
                elseif (rehashed-bit?(bits, i))
                  loop (i - $entry-index-delta, state, count)
                else
                  let nxti :: <integer> = i - $entry-index-delta;
                  let k = entry-key(keys, i);
                  let v = entry-value(vals, i);
                  entry-key(keys, i) :=  $table-entry-empty;
                  entry-value(vals, i) := $table-entry-empty;
                  local method storenext (k, v, count :: <integer>, state :: <hash-state>)
                          if (pointer-id?(k, $table-entry-empty) | table-entry-deleted?(k))
                            loop(nxti, state, count)
                          else
                            let (id, hstate) = hash(tv, k);
                            let (j :: <integer>, nxtk) = find-rehash-insertion-point(tv, k, id, bits);
                            let nxtv = entry-value(vals, j);
                            entry-key(keys, j) := k;
                            entry-value(vals, j) := v;
                            rehashed-bit?(bits, j) := #t;
                            storenext(nxtk, nxtv, count + 1, merge-hash-state!(state, hstate));
                          end if
                        end method;
                  storenext(k, v, count, state)
                end if
              end method;
        loop(entry-index-limit(tv) - $entry-index-delta, make(<hash-state>), 0);
      else
        local method loop (i :: <integer>, state :: <hash-state>, count :: <integer>)
                if (i < 0)
                  values(count, state)
                elseif (rehashed-bit?(bits, i))
                  loop(i - $entry-index-delta, state, count)
                else
                  let nxti :: <integer> = i - $entry-index-delta;
                  let k = entry-key(keys, i);
                  entry-key(keys, i) :=  $table-entry-empty;
                  local method storenext (k, count :: <integer>, state :: <hash-state>)
                          if (pointer-id?(k, $table-entry-empty) | table-entry-deleted?(k))
                            loop(nxti, state, count)
                          else
                            let (id, hstate) = hash(tv, k);
                            let (j :: <integer>, nxtk) = find-rehash-insertion-point(tv, k, id, bits);
                            entry-key(keys, j) := k;
                            rehashed-bit?(bits, j) := #t;
                            storenext(nxtk, count + 1, merge-hash-state!(state, hstate));
                          end if
                        end method;
                  storenext(k, count, state)
                end if
              end method;
        loop(entry-index-limit(tv) - $entry-index-delta, make(<hash-state>), 0);
      end if;
  additions(tv) := count;
  deletions(tv) := 0;
  hash-state(tv) := state;
  synchronize-side-effects();
  rehash-token(tv) := make-rehash-token(#f);
end function;


// Compute the desired number of entries for the destination of
// a rehash, based on the number of entries currently present
// and the requested table size.

define function calculate-real-size(tv :: <table-vector>) => (size :: <integer>);
  let keys :: <entry-keys> = tv.entry-keys;
  local method loop (index :: <integer>, count :: <integer>)
          if (negative?(index))
            count
          else
            let key = entry-key(keys, index);
            loop(index - $entry-index-delta,
              if (pointer-id?(key, $table-entry-empty) | table-entry-deleted?(key))
                count
              else
                count + 1
              end);
          end if;
        end method loop;
  loop(entry-index-limit(tv) - $entry-index-delta, 0);
end;

define function rehash-entry-count
     (table :: <table>, tv :: <table-vector>, grow? :: <boolean>)
  => entries :: <integer>;
  let present =
    if (table.weak?)
      calculate-real-size(tv);
    else
      present-entry-count(tv);
    end;
  if (~grow? | additions-limit(tv) > present)
    // Not full or not growing.  Use the maximum of the initially requested
    // number of entries and the actually present number of entries.
    max(initial-entries(table), present);
  else
    // Full.  Compute growth size.
    // Note that this computation is fairly robust against bogus
    // values for the growth rate.  About the worst that can happen
    // is that the value to be returned will prove to not be a
    // small integer, leading to a type error.
    let new :: <integer> = (grow-size-function(table))(present);
    max(present + $minimum-growth-amount, new);
  end if;
end;

// Default for the grow-size-function() of a table.
define function default-grow-size (old :: <integer>)
  => size :: <integer>;
  let grow-size :: <integer> = ceiling(old * $default-growth-rate);
  grow-size
end;


//
// ---------------- Functions on tables (external) -------------------

define open generic table-protocol (t :: <table>)
  => (test :: <function>, hash :: <function>);


// --- Temporarily here, pending approval and moving to a more
// --- appropriate place.
define open generic remove-all-keys! (c :: <mutable-explicit-key-collection>);

// ---------------- Functions on tables (internal) -------------------

// The <table-vector> containing the data for the table.
define generic table-vector (t :: <table>)
  => tv :: <table-vector>;

define generic table-vector-setter (tv :: <table-vector>, t :: <table>)
  => tv :: <table-vector>;

// The initial maximum expected number of entries,
// specified by the size: initarg.
define generic initial-entries (t :: <table>)
  => number :: <integer>;

// When the table needs to grow, this function is used to determine the
// desired number of entries to allow for in the expanded table.  It
// is a function of one argument, the current number of entries allowed,
// and must return an integer value greater than that which is the
// number of entries desired.
define generic grow-size-function (t :: <table>)
  => fun :: <function>;

// ---------------- <table> ----------------

define open abstract primary class <table>
    (<mutable-explicit-key-collection>, <stretchy-collection>, <limited-collection>)
  slot table-vector :: <table-vector>,
    init-value: initial-table-vector();
  constant slot initial-entries :: <integer>,
    init-keyword: size:,
    init-value: $default-table-size;
  constant slot grow-size-function :: <function> = default-grow-size,
    init-keyword: grow-size-function:,
    init-value: default-grow-size;
  constant slot weak? :: one-of(#"key", #"value", #f) = #f,
    init-keyword: weak:;
  // RECYCLING TABLE-VECTOR
  // slot rehash-table-vector :: false-or(<table-vector>) = #f;
end class <table>;

define sealed domain element-type (<table>);

define class <object-table> (<table>)
end class <object-table>;

define sealed domain make (singleton(<object-table>));
define sealed domain initialize (<object-table>);

// define class <standard-object-table> (<object-table>)
// end class <standard-object-table>;

// define sealed domain make (singleton(<standard-object-table>));
// define sealed domain initialize (<standard-object-table>);


define sealed inline method make
    (c == <table>, #rest initargs, #key, #all-keys) => (object)
  // ignore(c);
  apply(make, <object-table>, initargs);
end method make;

// define sealed inline method make
//     (c == <object-table>, #rest initargs, #key, #all-keys) => (object)
//   // ignore(c);
//   apply(make, <standard-object-table>, initargs);
// end method make;

define sealed method key-test (table :: <table>)
  => test :: <function>;
  test-function(table-vector(table));
end method key-test;

define sealed method table-protocol (table :: <object-table>)
  => (test :: <function>, hash :: <function>);
  // ignore(table);
  values(\==, object-hash);
end method table-protocol;

define method initialize (table :: <table>, #key lock = make-table-lock(),
                                                 values? = #t)
  next-method();
  let (test, hash) = table-protocol(table);
  table-vector(table)
    := make-table-vector(initial-entries(table), test, hash, lock, values?,
                         weak?(table));
end method initialize;

define sealed method empty? (table :: <table>)
  => result :: <boolean>;
  let tv = table-vector(table);
  with-table-vector-locked (tv)
    zero?(present-entry-count(tv));
  end with-table-vector-locked;
end method empty?;

define sealed method size (table :: <table>)
  => size :: <integer>;
  let tv = table-vector(table);
  with-table-vector-locked (tv)
    present-entry-count(tv);
  end with-table-vector-locked;
end method size;

// ----------------------- element() ----------------------------------

define function gethash
  (table :: <table>, key, default, first-attempt? :: <boolean>)
  => value;
  let tv = table-vector(table);
  let token = rehash-token(tv);
  // Ensure token fetched before computing hash code.
  sequence-point();
  // Don't need hash state here.
  let id = hash-for-lookup(tv, key);
  let (index, table-key) = search(tv, key, id);
  // Fetch value vector early to allow better scheduling of the loads.
  let vals = entry-values(tv);
  if (~pointer-id?(table-key, $table-entry-empty))
    // Ensure that value is looked up after search is completed.
    sequence-point();
    let value = entry-value(vals, index);
    // Force value fetch to occur before token validation.  Allowing value
    // fetch to occur later could lead to inconsistencies due to in place
    // rehash, because a rehash could then clobber the value.
    sequence-point();
    if (rehash-token-valid?(tv, token) & ~table-entry-deleted?(value))
      value;
    else
      // Rehash has been initiated.
//      rehash-table(table, tv, #f);      // Why do this?
      with-table-vector-locked (tv) end;  // Just wait on lock instead.
      gethash(table, key, default, #f);          // try again
    end if;
  elseif (needs-rehash?(tv, token))
    // TODO: If this is not the first attempt at rehashing then perhaps we
    // should look for the key during the rehash.
    rehash-table(table, tv, #f);
    gethash(table, key, default, #f);        // try again
  elseif (pointer-id?(default, $table-entry-empty))
    // --- Signal some more specific class of error?
    key-missing-error(table, key, default);
  else
    check-type(default, element-type(table));
    default;
  end if;
end;

define sealed inline method element (table :: <table>, key,
                       #key default = $table-entry-empty)
  => value;
  gethash(table, key, default, #t);
end method element;

/*
define sealed inline method element (table :: <standard-object-table>, key,
                                     #key default = $table-entry-empty)
  => value;
  gethash(table, key, default, #t);
end method element;
*/

define method key-missing-error (table :: <table>, key, default)
  => value;
  block ()
    error("%= is not present as a key for %=.", key, table);
  exception (<simple-restart>,
             init-arguments:
                vector(format-string:    "Try looking up %= in %= again.",
                       format-arguments: vector(key, table)))
    gethash(table, key, default, #t);
  end;
end method;

// ----------------------- element-setter() -------------------------

define function try-to-puthash-old
     (tv :: <table-vector>,
      token :: <rehash-token>,
      index :: <integer>,
      new-value :: <object>)
  => success? :: <boolean>;
  // The lock is needed to synchronize against rehash.  We don't care about
  // a competing puthash or deletion (so long as puthash doesn't attempt to
  // reuse deleted slots).  In the puthash case, one of them will finish
  // first, and it is not defined which one, assuming there is no external
  // synchronization.  In the deletion case, storing into the deleted slot
  // here is the same as doing the store first and then doing the deletion.
  with-table-vector-locked (tv)
    // Fetch value vector early to allow better scheduling of the loads.
    let vals = entry-values(tv);
    if (rehash-token-valid?(tv, token))
      entry-value(vals, index) := new-value;
      #t;                                // return success flag
    else
      #f;                                // return failure flag
    end if;
  end with-table-vector-locked;
end;

define function try-to-puthash-new
     (tv :: <table-vector>,
      token :: <rehash-token>,
      hstate :: <hash-state>,
      index :: <integer>,
      key :: <object>,
      new-value :: <object>)
  => success? :: <boolean>;
  with-table-vector-locked (tv)
    merge-hash-state!(hash-state(tv), hstate);
    // Only proceed if:
    //
    // * The key at index is still empty.  If not, then somebody else has
    //   usurped the entry we were going to store into.
    //
    // * The table vector is not full.  This is needed to avoid using up
    //   the last empty entry, making the tv completely full and risking
    //   an infinite loop in search().
    //
    // * There hasn't been any rehash activity.  The proper location may
    //   have changed if there's been an in place rehash, and if there's
    //   been a copying rehash then the tv is no longer the current tv
    //   for the table, so stores into it would just be thrown away.
    //
    // * A rehash is not needed for the merged hash-state.  If a rehash is
    //   needed then either the hash code used to decide where to install
    //   the key/value pair is invalid and might not belong at index after
    //   all, or the tv needs rehashing and search() might have failed to
    //   find the key even though there is an entry present for it.
    //
    // The combination of the above checks (excluding the fullness check) is
    // sufficient to ensure that some other entry has not been allocated for
    // the key, so that there is no possibility of duplicate keys being
    // present, assuming deleted slots are not reused.
    let keys = entry-keys(tv);
    if (pointer-id?(entry-key(keys, index), $table-entry-empty)
         & ~full?(tv)
         & rehash-token-valid?(tv, token)
         & ~is-stale?(tv.hash-state))
      additions(tv) := additions(tv) + 1;
      entry-value(entry-values(tv), index) := new-value;
      // Force the global completion of the store of the new value and
      // hash-state before the store of the key, so that gethash can rely
      // on that order and avoid the need to lock.
      synchronize-side-effects();
      entry-key(keys, index) := key;
      #t;                                // success flag
    else
      #f;                                // failure flag
    end if;
  end with-table-vector-locked;
end;

// It is intentional that this does not attempt to reuse deleted slots.
// Doing so would significantly complicate things, probably with very
// little real benefit.  The complications arise because reusing deleted
// slots would make it more difficult to prevent multiple setters for
// the same key from simultaneously installing the same key in multiple
// slots.  Consider the following scenario:
//
// 1. Process P1 finds a deleted entry D in the collision chain for key K1 and
//    decides to use D as the location for K1.
// 2. Process P2 uses D as the location for some other key K2.
// 3. Process P3 installs K1 in some entry other than D because D is in use.
// 4. Process P4 removes K2, marking D as deleted.
// 5. Process P1 must now be prevented from installing a duplicate K1 entry
//    at D.
//
// That prevention could be implemented by noticing that the additions or
// deletions counters had changed between the time of the lookup and the
// installation, but that would add overhead to the much more common case
// where no such problem exists.  Instead, by avoiding the reuse of deleted
// entries, this situation is avoided without adding overhead to common
// operations, at the cost of more frequent rehashing of tables that undergo
// frequent deletions.

define function puthash (new-value, table :: <table>, key)
  => new-value;
  let tv = table-vector(table);
  let token = rehash-token(tv);
  // Ensure that the above occurs before computing the hash code.
  sequence-point();
  let (id, hstate) = hash(tv, key);
  let (index, fkey) = search(tv, key, id);
  if (if (~pointer-id?(fkey, $table-entry-empty))
        // Try to update value for existing entry.
        try-to-puthash-old(tv, token, index, new-value);
      else
        // Otherwise, adding a new entry.
        try-to-puthash-new(tv, token, hstate, index, key, new-value);
      end if)
    new-value;                        // store successful, return the new value
  else
    // Store failed for some reason.  Rehash if needed and retry.
    if (needs-rehash?(tv, token))
      rehash-table(table, tv, full?(tv));
    elseif (full?(tv))
      rehash-table(table, tv, #t);
    end;
    puthash(new-value, table, key);        // try again
  end if;
end;

define sealed inline method element-setter (new-value, table :: <table>, key)
 => new-value;
  check-type(new-value, element-type(table));
  puthash(new-value, table, key);
end method element-setter;

/*
define sealed inline method element-setter
    (new-value, table :: <standard-object-table>, key) => new-value;
  check-type(new-value, element-type(table));
  puthash(new-value, table, key);
end method element-setter;
*/

// --------------------- remove-key!() -------------------------

define function try-to-remhash
     (tv :: <table-vector>,
      token :: <rehash-token>,
      index :: <integer>)
  => success? :: <boolean>;
  with-table-vector-locked (tv)
    // Only proceed if:
    //
    // * The entry's key is the key we're removing.  Because we don't
    //   reuse deleted entries, either the entry's key is the key or it
    //   is the deleted marker, so just compare with the deleted marker.
    //   This means we don't need the key here, and should also result in
    //   a faster comparison because the compiler can determine the type
    //   of the deleted marker and know that it only requires a pointer
    //   equivalence test, rather than the fully general \== test.
    //
    // * There hasn't been any rehash activity.
    //
    // The combination of these two tests is also sufficient to protect
    // against an in place rehash, assuming that deleted entries aren't
    // reused by puthash.
    let keys = entry-keys(tv);
    if (~table-entry-deleted?(entry-key(keys, index))
        & rehash-token-valid?(tv, token))
      // Set the key to the deleted marker.  Note that we must not modify
      // the value associated with the key, as that could confuse gethash.
      entry-key(keys, index) := $table-entry-deleted;
      let vals = entry-values(tv);
      entry-value(vals, index) := $table-entry-deleted;
      deletions(tv) := deletions(tv) + 1;
      #t;                                // success flag
    else
      #f;                                // failure flag
    end if;
  end with-table-vector-locked;
end;

define sealed method remove-key! (table :: <table>, key)
  => present? :: <boolean>;
  local method loop ()
          let tv = table-vector(table);
          let token = rehash-token(tv);
          // Ensure token is obtained before calling hash function.
          sequence-point();
          let id = hash(tv, key);
          let (index, fkey) = search(tv, key, id);
          let found? = ~pointer-id?(fkey, $table-entry-empty);
          if (found? & try-to-remhash(tv, token, index))
            #t;
          elseif (needs-rehash?(tv, token))
            rehash-table(table, tv, #f);
            loop();
          elseif (found?)
            // Failed to remove an existing entry.  Try again.
            loop();
          else
            // Failed to find an existing entry and no rehash needed, so
            // the key is not present in the table.  Return not-found value.
            #f;
          end if;
        end method loop;
  loop();
end method remove-key!;

// --------------------- remove-all-keys! -----------------------


define sealed method remove-all-keys! (table :: <table>)
  let tv = table-vector(table);
  unless (tv.additions = 0)
    unless (with-table-vector-locked (tv)
              if (in-place-rehashable?(tv))
                clear-table-vector!(tv);
                #t
              end if
            end with-table-vector-locked)
      let new = make-table-vector(initial-entries(table),
                                  test-function(tv),
                                  hash-function(tv),
                                  table-vector-lock(tv),
                                  tv.entry-values ~== tv.entry-keys,
                                  weak?(table));
      local method clear ()
              let tv = table-vector(table);
              with-table-vector-locked (tv)
                if (pointer-id?(tv, table-vector(table)))
                  mark-rehashing(tv);
                  table-vector(table) := new;
                  #t;
                else #f;
                end if;
              end with-table-vector-locked;
            end method clear;
      for (until: clear()) end;
    end unless;
  end unless;
end method remove-all-keys!;


//
// Iteration states are not thread safe.  Attempting to use the same
// iteration state simultaneously in different threads has undefined
// consequences.
//
// There is a tricky problem in the interaction between the iteration protocol
// and weak tables.  First note that weak entry removal must behave just like
// remove-key!(), i.e. set the key part to the deleted marker, leave the value
// alone, and increment the deleted counter.  Any other behavior will lead to
// problems elsewhere.  For example, removing the value could screw gethash.
//
// The problem with the iteration protocol is that entry removal could
// effectively remove the current entry out from under an iteration state.
// For weak-on-key tables, this can be addressed by having the iteration state
// include the current key, thereby ensuring that it is live if the iteration
// state is live, blocking removal of the corresponding entry.
//
// For weak-on-value tables, things are more difficult.  The problem is that
// simply making the iteration state hold onto the current value won't work
// if direct calls to element-setter on the same key are also permitted.
// However, because remove-key!() doesn't touch the value part of the entry,
// we do have a "default" value to use if the entry has been (re)moved out
// from under us.
//
// So what we do is make the current-element function check for the entry
// having been deleted, and if so it does the fully general lookup (i.e.
// gethash), using the value from the deleted entry as the default.  This
// can produce somewhat surprising results in some cases, but the data
// structure integrity is maintained and it is possible to explain these
// anomalies in terms of the state having "pre-fetched" the value.
//
// A different approach that would deal with weak tables but could still
// lead to problems with remove-key!() would be to record the existence
// of an iteration state in the vector and prohibit weak entry removal for
// any table vector that was undergoing iteration, with a termination
// method for iteration states that removed that record.  A complication
// in this scheme is that rehash would need to forward that record to the
// new vector.  Of course, this scheme also requires that termination be
// implemented.
//
// Note that we can't implement in-place rehash without some mechanism for
// inhibiting it while an active iteration state exists for the same
// table vector.  That also requires termination, so that when an iteration
// state becomes inaccessible its associated inhibition against in-place
// rehash gets removed.

// The table vector being iterated over.  It may or may not be the current
// vector for the table being iterated over; a rehash may have changed the
// current table vector for the table.
define generic source (state :: <iteration-state>)
  => source :: <table-vector>;

// The rehash token for the table vector when the iteration was initiated.
define generic token (state :: <iteration-state>)
  => rt :: <rehash-token>;

// The key for the current entry of the iteration.
define generic state-key (state :: <iteration-state>)
  => key :: <object>;

define generic state-key-setter (key, state :: <iteration-state>)
  => key;

// The index in the table vector for the current entry of the iteration.
// Starts at the end and decremented as we walk through the table.
// A negative value indicates that the iteration is exhausted.
define generic state-index (state :: <iteration-state>)
  => index :: <integer>;

define generic state-index-setter
     (index :: <integer>, state :: <iteration-state>)
  => index :: <integer>;


define primary class <iteration-state> (<object>)
  constant slot source :: <table-vector>,
    required-init-keyword: vector:;
  constant slot token :: <rehash-token>,
    required-init-keyword: token:;
  slot state-key,
    required-init-keyword: key:;
  slot state-index :: <integer>,
    required-init-keyword: index:;
  slot state-count :: <integer> = 0,
    init-keyword: count:;
  constant slot state-initial-additions :: <integer>,
    required-init-keyword: additions:;
  constant slot state-initial-deletions :: <integer>,
    required-init-keyword: deletions:;
end class <iteration-state>;

define sealed domain make (singleton(<iteration-state>));
define sealed domain initialize (<iteration-state>);

define inline function finished-state-index? (index :: <integer>)
  => finished? :: <boolean>;
  negative?(index);
end;

// Note that this doesn't receive the exhausted state and try to use it
// in the description of the condition, because doing so would make the
// state no longer have dynamic extent.
define function exhausted-iteration (table :: <table>)
  // --- Signal some more specific class of error?
  error("Attempt to use exhausted iteration state for %=.", table);
end;

define function table-next-state (table, state :: <iteration-state>)
  => state :: <iteration-state>;
  // ignore(table);
  let keys = entry-keys(source(state));
  let init :: <integer> = state-index(state);
  if (~finished-state-index?(init))
    local method loop (index :: <integer>)
            if (index < 0)
              // If we've run off the end, set the state's index to negative
              // value, which is how we recognize a finished state.
              state-index(state) := index;
              decrement-iteration-state-references(source(state));
            else
              let key = entry-key(keys, index);
              if ((pointer-id?(key, $table-entry-empty)) | (table-entry-deleted?(key)))
                loop(index - $entry-index-delta);
              else
                state-index(state) := index;
                state-key(state) := key;
                state-count(state) := state-count(state) + 1;
              end if;
            end if;
          end method loop;
    loop(init - $entry-index-delta);
  end if;
  state
end function;


define inline function table-finished-state?
     (table, state :: <iteration-state>, limit)
  => finished? :: <boolean>;
  // ignore(table, limit);
  finished-state-index?(state-index(state))
  & (~table.weak?
     | begin
         let tv :: <table-vector> = state.source;
         with-table-vector-locked(tv)
           if (state.state-initial-additions == tv.additions &
               state.state-initial-deletions == tv.deletions)
             tv.deletions := 0;
             tv.additions := state.state-count;
           end
         end;
         #t
       end)
end;

define inline function table-current-key
     (table, state :: <iteration-state>)
  // ignore(table>);
  if (finished-state-index?(state-index(state)))
    exhausted-iteration(table);
  else
    state-key(state);
  end if;
end;

define function table-current-element (table :: <table>, state :: <iteration-state>)
  let index = state-index(state);
  if (finished-state-index?(index))
    exhausted-iteration(table);
  else
    let tv = source(state);
    let value = entry-value(entry-values(tv), index);
    let keys = entry-keys(tv);
    // Ensure order between value fetch and below tests.
    // The keys vector can be fetched prior to the sequence point
    // because it is constant wrto the tv.
    sequence-point();
    // If tv in state is still the current tv for the table, then
    // the value found is still active at this point, so return it.
    if (~table-entry-deleted?(entry-key(keys, index))
         & rehash-token-valid?(tv, token(state)))
      value;
    else
      // Either the entry was deleted after the iteration state
      // reached it or the table is being or has been rehashed since
      // the iteration was started.  It is possible that the value
      // in the state's tv is not the value in the table, because
      // somebody might have changed it after the rehash or deletion.
      // Doing so is permitted because we're assuming that the current
      // element can be modified directly.  There is a weird case here
      // that we will return the old value if the entry was actually
      // removed (either by remove-key! or by weak entry removal).  I
      // can't think of anything better to do in the remove-key! case
      // other than signaling an error, and that would cause problems
      // for weak entry removal.
      if (table-entry-deleted?(value))
        error("Current element not found in table %=\n", table);
      else
        gethash(table, state-key(state), value, #t);
      end if
    end if;
  end if;
end;

define function table-current-element-setter
     (value, table :: <table>, state :: <iteration-state>)
  check-type(value, element-type(table));
  let index = state-index(state);
  if (finished-state-index?(index))
    exhausted-iteration(table);
  else
    // Store value into state's vector as the value of current element.
    // This is related to try-to-puthash-old, but differs because here
    // we don't need to worry about the possibility of a rehash in place
    // disturbing things while we're doing the operation.  Because of
    // that we don't need a lock here.
    let tv = source(state);
    entry-value(entry-values(tv), index) := value;
    let keys = entry-keys(tv);
    // Ensure global completion of the store before the checks for
    // falling back to using puthash.  Without this, another thread could
    // start a rehash and pass over index before the value store was done,
    // thereby losing the store.  The key vector can be fetched early
    // because it is constant wrto the tv.
    synchronize-side-effects();
    // If the entry has been deleted or rehash has been initiated,
    // use puthash to really install the value.  Use "rehash initiated"
    // rather than "has been rehashed" because a rehash might have
    // already copied the current element into the new tv.  As with
    // current-element, the entry shouldn't be deleted by remove-key!,
    // but could have been removed by weak entry removal, and treating
    // them differently is problematic.
    if (table-entry-deleted?(entry-key(keys, index))
         | ~rehash-token-valid?(tv, token(state)))
      puthash(value, table, state-key(state));
    end if;
    value;                        // return the new value
  end if;
end;

define function table-copy-state (table, state :: <iteration-state>)
  => new-state :: <iteration-state>;
  // ignore(table);
  let tv = source(state);
  increment-iteration-state-references(tv);
  make(<iteration-state>,
       vector: tv,
       token: token(state),
       index: state-index(state),
       key: state-key(state),
       count: state-count(state),
       additions: state-initial-additions(state),
       deletions: state-initial-deletions(state));
end;

define function make-initial-state (table :: <table>)
  => state :: <iteration-state>;
  let tv = table-vector(table);
  let token = rehash-token(tv);
  if (needs-rehash?(tv, token))
    // If needs rehash, do so and try again.
    // This isn't strictly necessary, but could give better performance
    // when actually doing the iteration, since the tv will likely
    // remain valid.  Of course, by doing the rehash now we might be
    // slowing down some other iteration over the same table, but such
    // is life.
    rehash-table(table, tv, #f);
    make-initial-state(table);
  else
    increment-iteration-state-references(tv);
    let state = make(<iteration-state>,
                     vector: tv,
                     token: token,
                     key: #f, // dummy value, fixed by table-next-state
                     index: entry-index-limit(tv),
                     additions: tv.additions,
                     deletions: tv.deletions);
    // Set up to point to the first non-empty entry.
    table-next-state(table, state);
  end if;
end;

define sealed inline method forward-iteration-protocol (table :: <table>)
  => (initial-state                :: <iteration-state>,
      limit                        :: <object>,
      next-state                :: <function>,
      finished-state?                :: <function>,
      current-key                :: <function>,
      current-element                :: <function>,
      current-element-setter        :: <function>,
      copy-state                :: <function>);
  values(make-initial-state(table),
         #f,                            // limit (ignored)
         table-next-state,
         table-finished-state?,
         table-current-key,
         table-current-element,
         table-current-element-setter,
         table-copy-state);
end method forward-iteration-protocol;


//
// <STRING-TABLE>
//

define sealed class <string-table>(<table>)
end;

define sealed method table-protocol (table :: <string-table>)
  => (test :: <function>, hash :: <function>);
  // ignore(table);
  values(method (x :: <string>, y :: <string>) x = y end, string-hash);
end method table-protocol;

///
/// LIMITED TABLES
///

define method limited-table
     (of :: <type>, size :: false-or(<integer>)) => (type :: <limited-table-type>)
  make(<limited-table-type>,
       class:          <table>,
       element-type:   of,
       concrete-class: <object-table>,
       size:           size);
end method;

/// TODO: COULD BE EXPENSIVE UNLESS TYPES ARE CACHED

define inline method type-for-copy (x :: <table>)
 => (type :: <type>)
  let elt-type = element-type(x);
  if (elt-type == <object>)
    object-class(x)
  else
    limited-table(element-type(x), #f)
  end if
end method type-for-copy;

