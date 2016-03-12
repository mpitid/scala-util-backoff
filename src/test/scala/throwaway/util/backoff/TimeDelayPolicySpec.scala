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

import org.scalatest.{FlatSpec, MustMatchers}

import scala.concurrent.duration._

class TimeDelayPolicySpec extends FlatSpec with MustMatchers {
  "Time delay policy" should "not convert time units unnecessarily" in {
    val min = 1.seconds
    val max = (1L + (1L << 53)).nanos
    val t = TimeDelayPolicy(min, max, DelayPolicy.exponentialBackOff(_, _))
    t.unit must equal(max.unit)
    t.policy.maxDelay must equal(max.length)
    t.policy.minDelay must equal(min.toUnit(max.unit).toLong)
    max.toUnit(max.unit).toLong must not equal max.length
  }
}
