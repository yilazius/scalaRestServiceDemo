package com.basti.finance.entities

import scala.util.{Failure, Success, Try}

import com.basti.finance.entities.Validator._

sealed abstract class Tradeable {
  def isin: String

  def shares: Int

  def price: Double
}

case class Stock(isin: String, shares: Int, price: Double) extends Tradeable

case class Fund(isin: String, shares: Int, price: Double) extends Tradeable


object Tradeable {

  abstract class TradeableType

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
    val validationErrors = validate(
      (Option(isin).getOrElse("").isEmpty, "isin not defined"),
      (shares < 0, "negative shares defined"),
      (price < 0, "negative doubles defined"),
      (tradeableType.TradeableType.isInstanceOf[UnsupportedTradeableType], "unsupported tradeable type")
    );

    if (validationErrors.isDefined)
      return Failure(new IllegalArgumentException(validationErrors.get.mkString(" , ")))

    tradeableType.TradeableType match {
      case StockTradeableType() => Success(Stock(isin, shares, price))
      case FundTradeableType() => Success(Fund(isin, shares, price))
    }

  }

}
