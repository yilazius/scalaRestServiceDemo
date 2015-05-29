package com.basti.finance.entities


import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

class PortfolioSpec extends FlatSpec with Matchers {

  sealed abstract class Tradeable {
    def isin: String

    def shares: Int

    def price: Double
  }

  case class Stock(isin: String, shares: Int, price: Double) extends Tradeable

  case class Fund(isin: String, shares: Int, price: Double) extends Tradeable


  sealed abstract class Validation {
    def isFailure: Boolean;
  }

  case class ValidationSuccess() extends Validation {
    override def isFailure = false
  }

  case class ValidationFailure(errorMessage: String) extends Validation {
    override def isFailure = true
  }

  def validate(condition: Boolean, validationError: String) = {
    if (condition) {
      ValidationFailure(validationError)
    }
    ValidationSuccess()
  }

  def validate(validationFunctions: Validation*) = {
    val failures = validationFunctions.filter(_.isFailure)
    if (!failures.isEmpty)
      Failure(new IllegalArgumentException(failures.map(_.asInstanceOf[ValidationFailure].errorMessage).mkString(",")))
    Success()
  }

  object Tradeable {

    sealed abstract class TradeableType

    case class StockTradeableType() extends TradeableType
    case class FundTradeableType() extends TradeableType
    case class UnsupportedTradeableType() extends TradeableType

    class StringToTradeableTypeConverter(val original: String) {
      def TradeableType = {
        original match {
          case "stock" => StockTradeableType()
          case "fund" => FundTradeableType()
          case _ => UnsupportedTradeableType()
        }
      }
    }

    implicit def stringToTradeableTypeConverter(value: String) = new StringToTradeableTypeConverter(value)


    def apply(tradeableType: String, isin: String, shares: Int, price: Double): Try[Tradeable] = {
      validate(
        validate(Option(isin).getOrElse("").isEmpty, "isin not defined"),
        validate(shares < 0, "negative shares defined"),
        validate(price < 0, "negative doubles defined"),
        validate(tradeableType.TradeableType.isInstanceOf[UnsupportedTradeableType], "unsupported tradeable type")
      ).map((unit) => {
        tradeableType.TradeableType match {
          case StockTradeableType() => Stock(isin, shares, price)
          case FundTradeableType() => Fund(isin, shares, price)
        }
      })


    }

  }

  class Portfolio(porfolioItems: Seq[Tradeable]) {
    def containsTradeable(isin: String) = {
      porfolioItems.exists(isin == _.isin)
    }
  }

  behavior of "A portfolio"

  it should "contain different stock items" in {

    val portfolioItems = Seq(
      Tradeable("stock", "bmw", 123, 34) get,
      Tradeable("stock", "google", 123, 2) get
    )
    val portfolio = new Portfolio(portfolioItems);
    portfolio containsTradeable "bmw" should be(true)


  }

  case class Tick(lastPrice: Double, Volume: Double, Open: Double, High: Double, Low: Double)

}
