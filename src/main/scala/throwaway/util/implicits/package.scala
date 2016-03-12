// The MIT License (MIT)
//
// Copyright (c) 2015 Michael Pitidis
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package throwaway.util

package object implicits {
  
  /** Handle overflow of positive numbers through saturation. */
  implicit class Saturated(val a: Long) extends AnyVal {
    def times(b: Long): Long = {
      require(a >= 0)
      require(b >= 0)
      val p = a * b
      if (((a | b) >>> 31) == 0L || p / b == a) { p } else { Long.MaxValue }
    }
  }

  implicit class LongRNG(val underlying: java.util.Random) extends AnyVal {
    /** Return a random number in [a, b).
      * Source: http://gee.cs.oswego.edu/cgi-bin/viewcvs.cgi/jsr166/src/main/java/util/concurrent/ThreadLocalRandom.java?view=co */
    def nextLong(origin: Long, bound: Long): Long = {
      require(bound > origin, "bound must be > origin")
      var r = underlying.nextLong()
      val n = bound - origin
      val m = n - 1
      if ((n & m) == 0L) { // power of two
        r = (r & m) + origin
      } else if (n > 0L) {
        // reject over-represented candidates, i.e.
        // those that fall outside
        var u = r >>> 1 // ensure nonnegative
        r = u % n
        // reject over-represented candidates, i.e.
        // those that fall in ranges that are not a
        // multiple of `n`, by checking for overflow
        while (u + m - r < 0L) {
          u = underlying.nextLong() >>> 1
          r = u % n
        }
        r += origin
      } else { // range not representable as long
        while (r < origin || r >= bound) {
          r = underlying.nextLong()
        }
      }
      r
    }
  }
}
