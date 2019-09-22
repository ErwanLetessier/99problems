package com.example._99problems

import java.time.format.DateTimeFormatter
import java.time.temporal.{TemporalAdjusters, WeekFields}
import java.time._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

object StoreOrderSchedule {

  val s =
   """[{"orderDay":"1","weekType":"P","orderBegTime":"000000","orderEndTime":"123000","supplyLeadTime":0.0,"preparationLeadTime":1.0,"siteDeliveryLeadTime":2.0,"shelvingLeadTime":0.0},
      |{"orderDay":"1","weekType":"I","orderBegTime":"000000","orderEndTime":"123000","supplyLeadTime":0.0,"preparationLeadTime":1.0,"siteDeliveryLeadTime":2.0,"shelvingLeadTime":0.0},
      |{"orderDay":"3","weekType":"P","orderBegTime":"000000","orderEndTime":"123000","supplyLeadTime":0.0,"preparationLeadTime":1.0,"siteDeliveryLeadTime":2.0,"shelvingLeadTime":0.0},
      |{"orderDay":"3","weekType":"I","orderBegTime":"000000","orderEndTime":"123000","supplyLeadTime":0.0,"preparationLeadTime":1.0,"siteDeliveryLeadTime":2.0,"shelvingLeadTime":0.0},
      |{"orderDay":"4","weekType":"P","orderBegTime":"000000","orderEndTime":"123000","supplyLeadTime":0.0,"preparationLeadTime":1.0,"siteDeliveryLeadTime":4.0,"shelvingLeadTime":0.0},
      |{"orderDay":"4","weekType":"I","orderBegTime":"000000","orderEndTime":"123000","supplyLeadTime":0.0,"preparationLeadTime":1.0,"siteDeliveryLeadTime":4.0,"shelvingLeadTime":0.0}]""".stripMargin


  case class OrderWeekPatternDetails(
                     orderDay: String /* Int 1-7 */,
                     weekType: String /* P / I  ) even/odd */,
                     siteDeliveryLeadTime: Double,
                     shelvingLeadTime: Double
                     )

  sealed trait WeekType
  case object EvenWeek extends WeekType
  case object OddWeek extends WeekType
  case object AnyWeekType extends WeekType

  object WeekType {
    def of(s: String) : WeekType = {
      s match {
        case "P" => EvenWeek
        case "I" => OddWeek
        case _   => AnyWeekType
      }

    }
  }

  case class NormalizedSchedule(orderDay: DayOfWeek, weekType: WeekType, daysUntilAvailable: Long)

  case class OrderSchedule(orderDate: LocalDate, deliveryDate: LocalDate)


  def toNormalizedSchedule(s: OrderWeekPatternDetails): NormalizedSchedule = {
    NormalizedSchedule(
      DayOfWeek.of(s.orderDay.toInt),
      WeekType.of(s.weekType),
      (s.siteDeliveryLeadTime + s.shelvingLeadTime).toLong
    )
  }

  type AsComparable[A] = A => Comparable[_ >: A]

  implicit def ordered[A: AsComparable]: Ordering[A] = new Ordering[A] {
    def compare(x: A, y: A): Int = x compareTo y
  }

  def infiniteDaysGenerator(cadence: Seq[DayOfWeek]): Stream[LocalDate] = {
    def daysGenerator(fromDay: LocalDate): Stream[LocalDate] = {
      println(s"daysGenerator $fromDay")
      val occurrence = (cadence map fromDay.next sorted).toStream
      occurrence #::: daysGenerator(occurrence.head)
    }
    daysGenerator(LocalDate.now(clock))
  }

  def main(args: Array[String]): Unit = {
    import DayOfWeek._
    infiniteDaysGenerator(Seq(TUESDAY, FRIDAY)) take 3 foreach println
  }


  val OneWeek = 1L

  def toOrderSchedule(schedule: NormalizedSchedule): OrderSchedule = {
    schedule match {
      case NormalizedSchedule(dayOfWeek, weekType, daysUntilDelivery) =>
        val day = today.next(dayOfWeek)
        val orderDate = weekType match {
          case AnyWeekType                          => day
          case weekType if weekType == day.weekType => day
          case _                                    => day.plusWeeks(OneWeek)
        }
        val deliveryDate = orderDate.plusDays(daysUntilDelivery)
        OrderSchedule(orderDate, deliveryDate)
    }
  }

  private def earlierSchedule(earlier: OrderSchedule, current: OrderSchedule): OrderSchedule = {
    if (current.orderDate.isBefore(earlier.orderDate)) current else earlier
  }

  def nextOrderSchedule(schedules: List[OrderWeekPatternDetails]): OrderSchedule = {
    (schedules map toNormalizedSchedule map toOrderSchedule) reduceLeft earlierSchedule
  }


  implicit val clock: Clock = Clock.fixed(Instant.from(DateTimeFormatter.ISO_ZONED_DATE_TIME.parse("2019-07-01T00:00:00.00+02:00")), ZoneId.of("Europe/Paris"))
  val yyyyMMdd: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")

  val today = LocalDate.now(clock)

  def dateOf(dateString: String): LocalDate = {
    LocalDate.parse(dateString, yyyyMMdd)
  }

  implicit class ExtAny(a: Any) {
    def print: Unit = println(s"${a.toString}")
  }

  implicit class ExtendedLocalDate(d: LocalDate) {
    def next(dayOfWeek: DayOfWeek): LocalDate = {
      d.`with`(TemporalAdjusters.next(dayOfWeek))
    }
    def weekOfYear: Int = {
      d.get(WeekFields.ISO.weekOfYear())
    }
    def isInEvenWeek: Boolean = weekOfYear % 2 == 0
    def isInOddWeek: Boolean = weekOfYear % 2 > 0
    def weekType: WeekType = if (isInEvenWeek) EvenWeek else OddWeek
  }

}

@RunWith(classOf[JUnitRunner])
class StoreOrderScheduleTest extends FlatSpec with Matchers {
  import StoreOrderSchedule._

  val inputSchedules = List(
    OrderWeekPatternDetails("1", "P", 2.0, 0.0),
    OrderWeekPatternDetails("1", "I", 2.0, 1.0),
    OrderWeekPatternDetails("3", "P", 2.0, 0.0),
    OrderWeekPatternDetails("3", "I", 2.0, 1.0),
    OrderWeekPatternDetails("4", "P", 4.0, 0.0),
    OrderWeekPatternDetails("4", "I", 4.0, 1.0)
  )

  val inputSchedulesWithNeutralWeek = List(
    OrderWeekPatternDetails("1", "", 2.0, 0.0),
    OrderWeekPatternDetails("3", "", 2.0, 0.0),
    OrderWeekPatternDetails("4", "", 4.0, 0.0)
  )

  val expectedSchedules = List(
    OrderSchedule(dateOf("20190708"), dateOf("20190710")),
    OrderSchedule(dateOf("20190715"), dateOf("20190718")),
    OrderSchedule(dateOf("20190710"), dateOf("20190712")),
    OrderSchedule(dateOf("20190703"), dateOf("20190706")),
    OrderSchedule(dateOf("20190711"), dateOf("20190715")),
    OrderSchedule(dateOf("20190704"), dateOf("20190709"))
  )

  val expectedSchedulesWithNeutralWeek = List(
    OrderSchedule(dateOf("20190708"), dateOf("20190710")),
    OrderSchedule(dateOf("20190703"), dateOf("20190705")),
    OrderSchedule(dateOf("20190704"), dateOf("20190708"))
  )

  "orderSchedule" should "return 20190702-20190705 for odd week" in {
    val schedule = OrderWeekPatternDetails("2", "I", 3.0, 0.5)
    val expected = OrderSchedule(dateOf("20190702"), dateOf("20190705"))

    val result = toOrderSchedule(toNormalizedSchedule(schedule))
    expected.print
    result.print
    result shouldEqual expected
  }

  "orderSchedule" should "return 20190709-20190712 for even week" in {
    val schedule = OrderWeekPatternDetails("2", "P", 3.0, 0.5)
    val expected = OrderSchedule(dateOf("20190709"), dateOf("20190712"))

    val result = toOrderSchedule(toNormalizedSchedule(schedule))
    expected.print
    result.print
    result shouldEqual expected
  }

  "mapOrderSchedule" should "return schedules" in {
    val result = inputSchedules map toNormalizedSchedule map toOrderSchedule
    expectedSchedules.print
    result.print
    result should contain theSameElementsAs expectedSchedules
  }

  "mapOrderSchedule" should "return schedules with Neutral Week" in {
    val result = inputSchedulesWithNeutralWeek map toNormalizedSchedule map toOrderSchedule
    expectedSchedulesWithNeutralWeek.print
    result.print
    result should contain theSameElementsAs expectedSchedulesWithNeutralWeek
  }

  "nextOrderSchedule" should "return 20190704-20190709" in {
    val expected = OrderSchedule(dateOf("20190703"), dateOf("20190706"))
    val result = nextOrderSchedule(inputSchedules)
    expected.print
    result.print
    result shouldEqual expected
  }

}



