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

package throwaway.util.backoff

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import throwaway.util.{GetEnv, PropertyConfig, TestSeed}

class DelayPolicySpec
 extends FlatSpec
    with MustMatchers
    with PropertyChecks
    with GetEnv with TestSeed with PropertyConfig {


  val choice = DelayPolicy.makeUniformChoice(new java.util.Random(seed))

  type Factory = (Long, Long) => DelayPolicy
  val factories = Seq[Factory](
      DelayPolicy.exponentialBackOff(_, _, choice)
    , DelayPolicy.decorrelated(_, _, f = choice)
    , DelayPolicy.exponentialTransition(_, _, choice)
    , DelayPolicy.quantizedBackOff(_, _, choice)
    , DelayPolicy.fixed
  )

  val goodBounds = Table(
    "min" -> "max"
    , 0L -> 1000L
    , 0L -> 0L
    , 1L -> 5000000L
    , 200000L -> 2323234234234L
    , 0L -> Long.MaxValue
    , Long.MaxValue - 1 -> Long.MaxValue
    , Long.MaxValue -> Long.MaxValue
  )

  val badBounds = Table(
    "min" -> "max"
    , -1L -> 0L
    , Long.MinValue -> Long.MaxValue
    , 1L -> 0L
    , Long.MaxValue -> Long.MinValue
    , 1L -> -1L
    , -2L -> -1L
  )

  val mmax = Long.MaxValue >> 1
  val arbitraryBounds: Gen[(Long, Long)] =
    for {a <- Gen.choose(0L, mmax-1)
         b <- Gen.choose(a, mmax)} yield a -> b

  "Delay policies" should "always return delays within bounds" in {
    forAll(goodBounds) { (min, max) =>
      boundsTest(factories, min, max)
    }
    forAll(arbitraryBounds) { case (min, max) =>
      boundsTest(factories, min, max)
    }
  }

  they should "reject negative and swapped bounds" in {
    forAll(badBounds) { (min, max) =>
      for (factory <- factories) {
        intercept[IllegalArgumentException] { factory(min, max) }
      }
    }
    forAll(arbitraryBounds) { case (min, max) =>
      whenever (min != max) {
        for (factory <- factories) {
          intercept[IllegalArgumentException] { factory(max, min) }
        }
      }
    }
  }

  "Exponential back off" should "pick uniform numbers between min and min times 2 to the current exponent" in {
    import throwaway.util.implicits.Saturated
    val reps = 100
    forAll(arbitraryBounds) {
      case (min, max) =>
        withBackOff(66, DelayPolicy.exponentialBackOff(min, max, choice)) {
          case p: ExponentialBackOffPolicy =>
            for (i <- 1 to reps) {
              val b = p.backOff
              b.exponent must equal(DelayPolicy.MaxExponent.min(p.exponent+1))
              b.delay must be >= min
              b.delay must be < min.max(max.min(min.max(1).times(1L << b.exponent)))
            }
        }
    }
  }

  def boundsTest(factories: Seq[Factory], min: Long, max: Long, n: Int = 1000): Unit = {
    for (factory <- factories) {
      withBackOff(n, factory(min, max)) { p =>
        p.delay must be >= p.minDelay
        p.delay must be <= p.maxDelay
      }
    }
  }

  def withBackOff[U](n: Int, policy: DelayPolicy)(f: (DelayPolicy => U)): Unit = {
    var p = policy
    var i = 0
    while (i < n) {
      f(p)
      p = policy.backOff
      i += 1
    }
  }
}
