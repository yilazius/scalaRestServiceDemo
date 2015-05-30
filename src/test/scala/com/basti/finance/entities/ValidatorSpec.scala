package com.basti.finance.entities

import org.scalatest.{FlatSpec, Matchers}

class ValidatorSpec extends FlatSpec with Matchers {

  behavior of "A Validator"

  it should "should collect error messages on validation errors" in {
    val x = -5
    val validationErrors = Validator.validate(
      (x < 0, "x is less than 0"),
      (x == -5, "x equals -5"),
      (x == 0, "x equals 0")
    )

    validationErrors should be(Some(List("x is less than 0", "x equals -5")))
  }

  it should "indicate, that validation was successful" in {
    val x = 5
    val validationErrors = Validator.validate(
      (x < 0, "x is less than 0"),
      (x == 0, "x equals 0")
    )

    validationErrors should be(None)
  }


}
