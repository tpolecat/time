tick
====

Pure functional date/time API. In progress.

My intent is to support:

 * Date and time instants and durations at year, month, week, day, and fractional precisions.
 * Typeclass-based open implementation.
 * A variety of consistent external representations:
   * Julian and modified Julian dates (integral and fractional)
   * Julian and proleptic Gregorian calendar dates
   * ISO-8601 calendar dates and times
   * Arbitrary and fixed-precision fractional time
   * POSIX date/time
 * Typesafe conversion among representations
 * Typesafe date/time arithmetic
 * Reasonable default `Show` instances (and `Enum` where applicable)
 * Timezones and offset arithmetic
 * Parsing and formatting (likely delegating to `java.util.Text` initially)

My background and personal interest makes it important to also support:

 * Local and mean sideral time (which requires geographic calculations)
 * Basic astronomic computations (sunrise, moonrise, moon phase, etc.)

At this point everything is in flux, so keep your distance.

Preliminaries
-------------

Tick relies on scalaz and shares a similar structure. Typeclasses, instances, and syntax are _a la carte_ but can also be imported _en masse_ via

```scala
import tick._
import Tick._
```

The examples here assume you have done this, as well as the analogous full import of Scalaz.


Tagging
-------

Tick uses tags to differentiate uses of raw numeric types. For example, the length of a `Year` has type `Int @@ Years`, which can be treated as a bare `Int` if desired.

Date arithmetic via operator sytntax requires tagged types to distinguish units. You can construct tagged types directly via scalaz's constructor `Tag[Int, Months](3)` or via Tick syntax `3.months`.




