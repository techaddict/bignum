package bignum

import com.google.caliper.{Runner => CaliperRunner}

object Runner {

  def main(args: Array[String]) {
    CaliperRunner.main(classOf[Add], args: _*)
    CaliperRunner.main(classOf[Multiply], args: _*)
  }

}
