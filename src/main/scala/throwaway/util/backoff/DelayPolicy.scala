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

import java.util.concurrent.ThreadLocalRandom

import throwaway.util.backoff.DelayPolicy.{Choice, MaxExponent, MinExponent}
import throwaway.util.implicits.{Saturated, LongRNG}

/** Represent a dimensionless, bounded delay policy,
  * than can grow (backoff) or shrink (speedup). */
trait DelayPolicy {
  require(minDelay >= 0, "minDelay must be > 0")
  require(minDelay <= maxDelay, "maxDelay must be >= minDelay")
  def delay: Long
  def minDelay: Long
  def maxDelay: Long
  def backOff: DelayPolicy
  def speedUp: DelayPolicy
}

object DelayPolicy {

  type Choice = (Long, Long) => Long

  val MaxExponent: Int = 62
  val MinExponent: Int = 0

  val uniformChoice: Choice =
    (a, b) =>
      if (a == b) { a } else { ThreadLocalRandom.current().nextLong(a, b) }

  def makeUniformChoice(rng: java.util.Random): Choice = {
    (a, b) =>
      if (a == b) { a } else { rng.nextLong(a, b) }
  }

  def decorrelated(min: Long, max: Long, multiplier: Int = 3, f: Choice = uniformChoice) = {
    DecorrelatedExponentialBackOffPolicy(min, max, min, multiplier, f)
  }

  def quantizedBackOff(min: Long, max: Long, f: Choice = uniformChoice) = {
    QuantizedBackOffPolicy(min, max, 0, f)
  }

  def exponentialBackOff(min: Long, max: Long, f: Choice = uniformChoice) = {
    ExponentialBackOffPolicy(min, max, 0, f)
  }

  def exponentialTransition(min: Long, max: Long, f: Choice = uniformChoice) = {
    ExponentialTransitionPolicy(min, max, 0, f)
  }

  def fixed(min: Long, max: Long) = {
    FixedDelayPolicy(min, max, min)
  }
}

/** Reference: https://www.awsarchitectureblog.com/2015/03/backoff.html */
trait DecorrelatedExponentialDelayPolicy extends DelayPolicy {
  /** Return an integer between [0, N) */
  def previous: Long
  def multiplier: Int
  def chooseBetween: Choice
  // This is a val because chooseBetween might not be deterministic
  val delay = maxDelay.min(chooseBetween(minDelay, previous.max(1).times(multiplier)))
}

trait ExponentialDelayPolicy extends DelayPolicy {
  /** Return an integer between [0, N) */
  def chooseBetween: Choice
  def exponent: Int
  // This is a val because chooseBetween might not be deterministic
  val delay = chooseBetween(minDelay, maxDelay.min(minDelay.max(1).times(1L << exponent)))
}

case class DecorrelatedExponentialBackOffPolicy(
    minDelay: Long
  , maxDelay: Long
  , previous: Long
  , multiplier: Int
  , chooseBetween: Choice
) extends DecorrelatedExponentialDelayPolicy {
  require(multiplier > 1, "multiplier should be > 1")
  def speedUp = copy(previous = minDelay)
  def backOff = copy(previous = delay)
}

case class ExponentialBackOffPolicy(
    minDelay: Long
  , maxDelay: Long
  , exponent: Int
  , chooseBetween: Choice
) extends ExponentialDelayPolicy {
  require(exponent >= MinExponent && exponent <= MaxExponent)
  def speedUp = if (exponent == MinExponent) this else copy(exponent = MinExponent)
  def backOff = copy(exponent = MaxExponent.min(exponent + 1))
}

/** Choose a random exponent only, so that delay takes a smaller number of possible values. */
case class QuantizedBackOffPolicy(
    minDelay: Long
  , maxDelay: Long
  , exponent: Int
  , chooseBetween: Choice
) extends DelayPolicy {
  val delay = maxDelay.min(minDelay.max(1).times(1L << chooseBetween(0, exponent + 1).toInt))
  def speedUp = if (exponent == MinExponent) this else copy(exponent = MinExponent)
  def backOff = copy(exponent = MaxExponent.min(exponent + 1))
}

case class ExponentialTransitionPolicy(
    minDelay: Long
  , maxDelay: Long
  , exponent: Int
  , chooseBetween: Choice
) extends ExponentialDelayPolicy {
  require(exponent >= MinExponent && exponent <= MaxExponent)
  def speedUp = copy(exponent = MinExponent.max(exponent - 1))
  def backOff = copy(exponent = MaxExponent.min(exponent + 1))
}

case class FixedDelayPolicy(
    minDelay: Long
  , maxDelay: Long
  , delay: Long
) extends DelayPolicy {
  def speedUp = if (delay == minDelay) this else copy(delay = minDelay)
  def backOff = if (delay == maxDelay) this else copy(delay = maxDelay)
}

