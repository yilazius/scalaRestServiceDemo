package com.basti.finance.entities


object Validator {

  type ValidationExpression = Boolean
  type ValidationErrorMessage = String
  type ValidationCondition = (ValidationExpression, ValidationErrorMessage)

  def failedCondition(validationCondition: ValidationCondition) = validationCondition _1

  def errorMessage(validationCondition: ValidationCondition) = validationCondition _2

  def validate(validationConditions: ValidationCondition*) = {
    val validationErrorMessages =
      validationConditions filter failedCondition map errorMessage
    if (validationErrorMessages.isEmpty) None else Some(List() ++ validationErrorMessages)

  }
}