package chapter4

class InsuranceCalculator {

  /**
   * Top secret formula for computing an annual car insurance premium from two key factors
   */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 5.0

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)

    // this doesn't type check as optAge and optTickets are Option[Int] not Int
    //insuranceRateQuote(optAge, optTickets)
    Some(5)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }
}
