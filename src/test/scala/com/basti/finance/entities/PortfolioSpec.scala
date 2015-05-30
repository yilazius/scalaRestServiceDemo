package com.basti.finance.entities


import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

class PortfolioSpec extends FlatSpec with Matchers {


  behavior of "A portfolio"

  val portfolioItems = Set[Tradeable](
    Stock("bmw", 123, 34.0),
    Stock("google", 123, 2.0)
  )
  val portfolio = new Portfolio(portfolioItems);

  it can "contain a specific Tradeable" in {
    portfolio containsTradeable "bmw" should be(true)
  }

  it should "contain a collection of Tradeables associated to the portfolio" in {
    portfolio.getTradeables should contain(Stock("bmw", 123, 34.0))
    portfolio.getTradeables should contain(Stock("google", 123, 2.0))
  }

  it should "contain newly added Tradeable" in {
    //    portfilio.addTradeable()o
  }


  //maybe mixin with Tradeable when returned to the client
  case class Tick(isin:String,lastPrice: Double, Volume: Double, Open: Double, High: Double, Low: Double)

}
