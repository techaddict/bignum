package bignum

import com.google.caliper.{Runner => CaliperRunner}

object Runner {

  def main(args: Array[String]) {
    CaliperRunner.main(classOf[BenchmarkAdd], args: _*)
  }

}
