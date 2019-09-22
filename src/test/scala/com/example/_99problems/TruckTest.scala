package com.example._99problems

import com.example._99problems.implicits._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class TruckTest extends FlatSpec with Matchers {

  import Truck._

  val p1SaleForecasts = saleForecast(400)
  val p2SaleForecasts = saleForecast(300)
  val p1Requirement = Requirement("P1", 400, today, today, 500, p1SaleForecasts)
  val p2Requirement = Requirement("P2", 300, today, today, 500, p2SaleForecasts)

  "optimizeForCapacity" should "return input requirement when capacity is 1" in {

    val requirements = List(p1Requirement)
    val truckCapacity = 1
    val expected = requirements

    val result = optimizeForCapacity(truckCapacity, requirements)
    result.foreach(_.orderLine.print)
    result should contain theSameElementsAs expected
    result.map(_.pallets).sum shouldBe truckCapacity
  }

  it should "return requirement when capacity is 2" in {

    val requirements = List(p1Requirement)
    val truckCapacity = 2
    val expected = List(p1Requirement.copy(quantity = 800, coverageEndDay = today.plusDays(1L)))

    val result = optimizeForCapacity(truckCapacity, requirements)
    result.foreach(_.orderLine.print)
    result should contain theSameElementsAs expected
    result.map(_.pallets).sum shouldBe truckCapacity
  }

  it should "return requirement when capacity is 2 and starting required quantity is negative" in {

    val requirements = List(p1Requirement.copy(quantity = -1 * p1Requirement.quantity))
    val truckCapacity = 2
    val expected = List(p1Requirement.copy(quantity = 800, coverageEndDay = today.plusDays(3L)))

    val result = optimizeForCapacity(truckCapacity, requirements)
    result.foreach(_.orderLine.print)
    result should contain theSameElementsAs expected
    result.map(_.pallets).sum shouldBe truckCapacity
  }

  it should "return requirement of 8 pallets when capacity is 20 which too much for 10 days forecast" in {

    val requirements = List(p1Requirement)
    val truckCapacity = 20
    val expected = List(p1Requirement.copy(quantity = 3600, coverageEndDay = today.plusDays(9L)))

    val result = optimizeForCapacity(truckCapacity, requirements)
    result.foreach(_.orderLine.print)
    result should contain theSameElementsAs expected
    result.map(_.pallets).sum shouldBe 8
  }

  it should "return requirement when capacity is 4 and there is 2 products and 1 product with more than 1 pallet a day forecast" in {

    val overOnePalletPerDay = p1Requirement.copy(itemsPerPallet = 300) /* pallet contains less items */
    val requirements = List(overOnePalletPerDay, p2Requirement)
    val truckCapacity = 4
    val expected = List(
      overOnePalletPerDay.copy(quantity = 800, coverageEndDay = today.plusDays(1L)),
      p2Requirement.copy(quantity = 300, coverageEndDay = today)
    )

    val result = optimizeForCapacity(truckCapacity, requirements)
    result.foreach(_.orderLine.print)
    result should contain theSameElementsAs expected
    result.map(_.pallets).sum shouldBe truckCapacity
  }

  it should "return requirement when capacity is 4 and there is 2 products and 1 product with more 2 pallets a day forecast" in {

    val twoPalletPerDay = p1Requirement.copy(quantity = 400, itemsPerPallet = 200) /* pallet contains less items */
    val requirements = List(twoPalletPerDay, p2Requirement)
    val truckCapacity = 4
    val expected = List(
      twoPalletPerDay.copy(quantity = 600, coverageEndDay = today),
      p2Requirement.copy(quantity = 300)
    )

    val result = optimizeForCapacity(truckCapacity, requirements)
    expected.foreach(_.orderLine.print)
    result.foreach(_.orderLine.print)
    result should contain theSameElementsAs expected
    result.map(_.pallets).sum shouldBe truckCapacity
  }

  it should "return requirement when capacity is 4 and there is 2 products" in {

    val requirements = List(p1Requirement, p2Requirement)
    val truckCapacity = 4
    val expected = List(
      p1Requirement.copy(quantity = 800, coverageEndDay = today.plusDays(1L)),
      p2Requirement.copy(quantity = 600, coverageEndDay = today.plusDays(1L))
    )

    val result = optimizeForCapacity(truckCapacity, requirements)
    result.foreach(_.orderLine.print)
    result should contain theSameElementsAs expected
    result.map(_.pallets).sum shouldBe truckCapacity
  }


  it should "return requirement when minimum requirement fills more than 1 truck" in {

    val largeP1Requirement = p1Requirement.copy(quantity = 1200, coverageEndDay = today.plusDays(1L))
    val largeP2Requirement = p2Requirement.copy(quantity = 600)
    val requirements = List(largeP1Requirement, largeP2Requirement)
    val truckCapacity = 4
    val expected = List(
      largeP1Requirement.copy(quantity = 800, coverageEndDay = today),
      largeP2Requirement.copy(quantity = 600, coverageEndDay = today)
    )

    val result = optimizeForCapacity(truckCapacity, requirements)

    result.foreach(_.orderLine.print)
    result should contain theSameElementsAs expected
    result.map(_.pallets).sum shouldBe truckCapacity
  }

  it should "return requirement when minimum requirement fills more than 1 truck, and remove 1 day is too much" in {

    val largeP1Requirement = p1Requirement.copy(quantity = 800, itemsPerPallet = 200, coverageEndDay = today.plusDays(1L))
    val largeP2Requirement = p2Requirement.copy(quantity = 300)
    val requirements = List(largeP1Requirement, largeP2Requirement)
    val truckCapacity = 4
    val expected = List(
      largeP1Requirement.copy(quantity = 600, coverageEndDay = today),
      largeP2Requirement.copy(quantity = 300, coverageEndDay = today)
    )

    val result = optimizeForCapacity(truckCapacity, requirements)

    result.foreach(_.orderLine.print)
    result should contain theSameElementsAs expected
    result.map(_.pallets).sum shouldBe truckCapacity
  }

  it should "return requirement when minimum requirement fills more than 1 truck, removing also on each product" in {

    val largeP1Requirement = p1Requirement.copy(quantity = 1400, coverageEndDay = today.plusDays(1L))
    val largeP2Requirement = p2Requirement.copy(quantity = 1200, coverageEndDay = today.plusDays(1L))
    val requirements = List(largeP1Requirement, largeP2Requirement)
    val truckCapacity = 4
    val expected = List(
      largeP1Requirement.copy(quantity = 1000, coverageEndDay = today),
      largeP2Requirement.copy(quantity = 900, coverageEndDay = today)
    )

    val result = optimizeForCapacity(truckCapacity, requirements)

    result.foreach(_.orderLine.print)
    result should contain theSameElementsAs expected
    result.map(_.pallets).sum shouldBe truckCapacity
  }

  it should "return requirement when minimum requirement fills more than 1 truck, with a single product" in {

    val largeP1Requirement = p1Requirement.copy(quantity = 2500, coverageEndDay = today.plusDays(3L))

    val requirements = List(largeP1Requirement)
    val truckCapacity = 4
    val expected = List(
      largeP1Requirement.copy(quantity = 1700, coverageEndDay = today.plusDays(1L))
    )

    val result = optimizeForCapacity(truckCapacity, requirements)

    result.foreach(_.orderLine.print)
    result should contain theSameElementsAs expected
    result.map(_.pallets).sum shouldBe truckCapacity
  }

}


