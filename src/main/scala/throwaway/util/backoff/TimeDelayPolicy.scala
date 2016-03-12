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

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

/** Convenience wrapper for delay policies representing time. */
case class TimeDelayPolicy(
    policy: DelayPolicy
  , unit: TimeUnit = TimeUnit.MILLISECONDS
  ) {
  def minDelay = time(policy.minDelay)
  def maxDelay = time(policy.maxDelay)
  def delay = time(policy.delay)
  def speedUp = if (delay == minDelay) this else copy(policy = policy.speedUp)
  def backOff = if (delay == maxDelay) this else copy(policy = policy.backOff)
  protected def time(value: Long): FiniteDuration = {
    new FiniteDuration(value, unit)
  }
}

object TimeDelayPolicy {
  type From = Long
  type Until = Long
  type PolicyFactory = (From, Until) => DelayPolicy

  def apply(minDelay: FiniteDuration, maxDelay: FiniteDuration, policy: PolicyFactory): TimeDelayPolicy = {
    val unit = Seq(minDelay.unit, maxDelay.unit).min
    TimeDelayPolicy(policy(toUnit(minDelay, unit), toUnit(maxDelay, unit)), unit)
  }

  /** Avoid unnecessary conversions to Double and loss of accuracy if units match (e.g. 2^53 + 1). */
  def toUnit(f: FiniteDuration, unit: TimeUnit): Long = {
    if (f.unit != unit) {
      Math.rint(f.toUnit(unit)).toLong
    } else {
      f.length
    }
  }
}

