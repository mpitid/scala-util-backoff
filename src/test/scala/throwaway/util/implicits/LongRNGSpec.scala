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

package throwaway.util.implicits

import java.util.Random

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}
import throwaway.util.{GetEnv, PropertyConfig, TestSeed}

class LongRNGSpec
 extends FlatSpec
    with MustMatchers
    with PropertyChecks
    with GetEnv with TestSeed with PropertyConfig {

  val minDelta: Long = long("MIN_DELTA", 1L).ensuring(_ > 0, "MIN_DELTA must be > 0")
  val maxDelta: Long = long("MAX_DELTA", Long.MaxValue).ensuring(d => d >= minDelta && d <= Long.MaxValue, s"MAX_DELTA must be > $minDelta and <= ${Int.MaxValue}")

  note(s"MIN_DELTA=$minDelta MAX_DELTA=$maxDelta")

  // Generate random pairs of longs at most maxDelta apart.
  def pairs: Gen[(Long, Long)] = {
    for {d <- Gen.choose(minDelta, maxDelta)
         a <- Gen.choose(Long.MinValue, Long.MaxValue - 1)
         b <- Gen.choose(a, if (a > Long.MaxValue - d) Long.MaxValue else a + d)} yield {
      (a, b)
    }
  }

  "nextLong(a, b)" should "return a number in [a, b)" in {
    val rng = new Random(seed)
    forAll(pairs) {
      case (a, b) =>
        val l = rng.nextLong(a, b)
        l must be >= a
        l must be < b
    }
  }

  it should "fail when a >= b" in {
    val rng = new Random(seed)
    forAll(pairs) {
      case (a, b) =>
        intercept[IllegalArgumentException] { rng.nextLong(b, a) }
    }
  }
}
