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

import org.scalatest.Notifying
import org.scalatest.prop.{Configuration, PropertyChecks}

trait PropertyConfig {
  self: Notifying with GetEnv with Configuration =>

  override implicit val generatorDrivenConfig: PropertyCheckConfig = {
    val c = new PropertyChecks{}.generatorDrivenConfig
    val minSuccessful = int("MIN_SUCCESSFUL", c.minSuccessful)
    val maxDiscarded = int("MAX_DISCARDED", c.maxDiscarded)
    val minSize = int("MIN_SIZE", c.minSize)
    val maxSize = int("MAX_SIZE", c.minSize).max(minSuccessful)
    val workers = int("WORKERS", c.workers)
    val p = PropertyCheckConfig(minSuccessful, maxDiscarded, minSize, maxSize, workers)
    note(s"MIN_SUCCESSFUL=$minSuccessful MAX_DISCARDED=$maxDiscarded MIN_SIZE=$minSize MAX_SIZE=$maxSize WORKERS=$workers")
    p
  }
}
