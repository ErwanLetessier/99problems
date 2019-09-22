package com.example._99problems

import java.time.{Clock, DayOfWeek, Instant, LocalDate, ZoneId}
import java.time.format.DateTimeFormatter
import com.example._99problems.implicits._


case class SaleForecast(day: LocalDate, quantity: Double) {
  def dayAfter(referenceDay: LocalDate): Boolean = day == referenceDay.plusDays(1L)
  def dayBefore(referenceDay: LocalDate): Boolean = day == referenceDay.minusDays(1L)
}

case class Requirement(
                        product: String,
                        quantity: Double,
                        coverageStartDay: LocalDate,
                        coverageEndDay: LocalDate,
                        itemsPerPallet: Int,
                        saleForecasts: Seq[SaleForecast]
                      ) {
  def pallets: Int = {
    Stream.from(itemsPerPallet, itemsPerPallet)
      .zip(Stream.from(1))
      .dropWhile{ case (items, _) => items < quantity }
      .head match {case (_, pallets) => pallets }
  }

  import Requirement._

  private def toUpdatedRequirement(op: (Double, Double) => Double)(forecast: SaleForecast): Requirement = {
    this.copy(
      coverageEndDay = forecast.day,
      quantity = op(this.quantity, forecast.quantity)
    )
  }

  def withAdditionalCoverageDay: Option[Requirement] = {
    saleForecasts
      .find(_.dayAfter(coverageEndDay))
      .map(toUpdatedRequirement(+))
  }

  def withRemovedCoverageDay: Option[Requirement] = {
    saleForecasts
      .find(_.dayBefore(coverageEndDay))
      .map(toUpdatedRequirement(-))
  }

  def orderLine: String = s"$product requirement until $coverageEndDay: $quantity items -> $pallets pallets"
}

object Requirement {

  def +(d1: Double, d2: Double): Double = d1 + d2
  def -(d1: Double, d2: Double): Double = d1 - d2
}


object Truck {

  implicit val clock: Clock = Clock.fixed(Instant.from(DateTimeFormatter.ISO_ZONED_DATE_TIME.parse("2019-07-01T00:00:00.00+02:00")), ZoneId.of("Europe/Paris"))

  val today = LocalDate.now(clock)

  def optimizeForCapacity(truckCapacity: Int, requirements: List[Requirement]): List[Requirement] = {

    def increasedOrder(current: Requirement, remaining: List[Requirement], missingPallets: Int) = {
      current.withAdditionalCoverageDay match {
        case Some(updatedRequirement) =>
          val last = if (updatedRequirement.pallets - current.pallets <= missingPallets) {
            println(s"adding 1 day of ${current.product} at ${current.itemsPerPallet} u/pal is ok/not enough")
            updatedRequirement
          } else {
            println(s"adding 1 day of ${current.product} at ${current.itemsPerPallet} u/pal is too much")
            current.copy(quantity = current.quantity + missingPallets * current.itemsPerPallet)
          }
          val newRequirements = remaining ++ List(last)
          val newMissingPallets = truckCapacity - sumPallets(newRequirements)
          fillUpToCapacity(newRequirements, newMissingPallets)
        case _ => current :: remaining
      }
    }

    def fillUpToCapacity(requirements: List[Requirement], missingPallets: Int): List[Requirement] = {
      if (0 == missingPallets) requirements
      else increasedOrder(requirements.head, requirements.tail, missingPallets)
    }

    def decreasedOrder(current: Requirement, remaining: List[Requirement], exceedingPallets: Int) = {
      val updatedCurrent = current.withRemovedCoverageDay
      updatedCurrent match {
        case Some(updatedRequirement) =>
          val removedPallets = current.pallets - updatedRequirement.pallets
          val last = if (removedPallets <= exceedingPallets) {
            println(s"removing 1 day of ${current.product} at ${current.itemsPerPallet} u/pal is ok/not enough")
            updatedRequirement
          }else {
            println(s"removing 1 day of ${current.product} at ${current.itemsPerPallet} u/pal is too much")
            updatedRequirement.copy(quantity = current.quantity - (exceedingPallets * current.itemsPerPallet))
          }
          val newRequirements = remaining ++ List(last)
          val newExceedingPallets = sumPallets(newRequirements) - truckCapacity
          pruneDownToCapacity(newRequirements, newExceedingPallets)
        case _ => current :: remaining
      }
    }

    def pruneDownToCapacity(requirements: List[Requirement], exceedingPallets: Int): List[Requirement] = {
      if (0 == exceedingPallets) requirements
      else decreasedOrder(requirements.head, requirements.tail, exceedingPallets)
    }

    sumPallets(requirements) match {
      case sum if sum > truckCapacity => pruneDownToCapacity(requirements, sum - truckCapacity)
      case sum                        => fillUpToCapacity(requirements, truckCapacity - sum)
    }
  }

  private def sumPallets(requirements: List[Requirement]): Int = {
    requirements.map(_.pallets).sum
  }

  def saleForecast(itemsPerDay: Double): Seq[SaleForecast] = {
    (0L to 9L).map(today.plusDays).map{
      case day if day isSunday  => SaleForecast(day, 0)
      case day                  => SaleForecast(day, itemsPerDay)
    }
  }

}

object implicits {
  implicit class ExtLocalDate(date: LocalDate) {
    def isSunday: Boolean = {
      date.getDayOfWeek == DayOfWeek.SUNDAY
    }

    def isBeforeOrEquals(otherDate: LocalDate): Boolean = {
      date == otherDate || date.isBefore(otherDate)
    }

    def isAfterOrEquals(otherDate: LocalDate): Boolean = {
      date == otherDate || date.isAfter(otherDate)
    }
  }

  implicit class ExtAny(a: Any) {
    def print: Unit = println(s"${a.toString}")
  }
}
