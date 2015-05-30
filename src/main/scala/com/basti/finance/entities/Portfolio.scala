package com.basti.finance.entities


class Portfolio(porfolioItems: Set[Tradeable]) {
  def getTradeables = porfolioItems;


  def containsTradeable(isin: String) = {
    porfolioItems.exists(isin == _.isin)
  }

}
