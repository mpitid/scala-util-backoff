
package throwaway.util.implicits

import java.util.Random

import org.apache.commons.math3.distribution.ChiSquaredDistribution
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}
import throwaway.util.{GetEnv, PropertyConfig, TestSeed}

/** Perform a chi-squared test for uniform fitness.
  * References:
  * - http://www.cimt.plymouth.ac.uk/projects/mepres/alevel/fstats_ch5.pdf
  * - http://www.math.umn.edu/~garrett/students/reu/pRNGs.pdf
  *
  * This test cannot succeed every time, hence mark the test as ignored by default. */
class UniformFitSpec
 extends FlatSpec
    with MustMatchers
    with PropertyChecks
    with GetEnv with TestSeed with PropertyConfig {

  val significance: BigDecimal = get("SIGNIFICANCE", BigDecimal("0.05"))(BigDecimal.apply).ensuring(s => s > 0 && s < 1.0, "SIGNIFICANCE must be > 0.0 and < 1.0")
  val minDelta: Long = long("MIN_DELTA", 100L).ensuring(_ > 0, "MIN_DELTA must be > 0")
  val maxDelta: Long = long("MAX_DELTA", 100000L).ensuring(d => d >= minDelta && d <= Int.MaxValue, s"MAX_DELTA must be > $minDelta and <= ${Int.MaxValue}")
  val minSamples: Long = long("MIN_SAMPLES", 1000L).ensuring(_ > 0, s"MIN_SAMPLES must be > 0")

  note(s"SIGNIFICANCE=$significance MIN_DELTA=$minDelta MAX_DELTA=$maxDelta MIN_SAMPLES=$minSamples")

  // Generate random pairs of longs at most maxDelta apart.
  def pairs: Gen[(Long, Long)] = {
    for {d <- Gen.choose(minDelta, maxDelta)
         a <- Gen.choose(Long.MinValue, Long.MaxValue - 1)
         b <- Gen.choose(a, if (a > Long.MaxValue - d) Long.MaxValue else a + d)} yield {
      (a, b)
    }
  }

  "sampled numbers" should "be uniformly distributed" ignore {
    val rng = new Random(seed)
    forAll(pairs) {
      case (a, b) =>
        require(a < b)
        val delta = b - a
        require(delta > 0)
        require(delta <= Int.MaxValue)
        val samples = minSamples.max(delta * 2)
        val observed = Array.fill(delta.toInt)(0L)
        var i = 0L
        while (i < samples) {
          val l = rng.nextLong(a, b)
          observed((l - a).toInt) += 1
          i += 1
        }
        val ps = BigDecimal(1)/delta
        val statistic = observed.map { y => BigDecimal(y).pow(2) / ps }.sum / samples - samples
        val pValue = new ChiSquaredDistribution(delta - 1.0).cumulativeProbability(statistic.toDouble)
        // note(f"delta $delta samples $samples statistic $statistic%6.2f (1-p) ${100 * (1-pValue)}%5.2f%% p $pValue%.4f critical region ${pValue < significance}")
        BigDecimal(pValue) must be >= significance
    }
  }
}
