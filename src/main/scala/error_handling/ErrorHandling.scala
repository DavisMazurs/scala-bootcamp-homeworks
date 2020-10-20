package error_handling

import java.time.LocalDate
import java.time.temporal.TemporalAdjusters

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxTuple4Semigroupal, catsSyntaxValidatedIdBinCompat0}
import error_handling.ErrorHandling.CardData._
import error_handling.ErrorHandling.ValidationError._

import scala.util.control.NonFatal

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object ErrorHandling {

  object CardData {
    final case class CardOwnerName(name: String) extends AnyVal
    final case class CardNumber(number: Long) extends AnyVal
    final case class CardExpiryDate(date: LocalDate) extends AnyVal
    final case class CVV(cvv: Int) extends AnyVal
  }
  case class PaymentCard(ownerName: CardOwnerName, number: CardNumber, expiryDate: CardExpiryDate, cvv: CVV)

  sealed trait ValidationError
  object ValidationError {
    final case object OwnerNameLengthInvalid extends ValidationError {
      override def toString: String = "Card owner name is invalid (must be between 3 and 30 characters)"
    }
    final case object OwnerNameContentsInvalid extends ValidationError {
      override def toString: String = "Card owner name contains special characters"
    }
    final case object CardNumberLengthInvalid extends ValidationError {
      override def toString: String = "Card number is invalid (must be exactly 16 digits)"
    }
    final case object CardNumberContentsInvalid extends ValidationError {
      override def toString: String = "Card number contains special characters"
    }
    final case object ExpiryDateFormatInvalid extends ValidationError {
      override def toString: String = "Expiry date format is invalid (must be MM/YY and contain only digits)"
    }
    final case object ExpiryDateOverdue extends ValidationError {
      override def toString: String = "Card has already expired"
    }
    final case object CvvLengthInvalid extends ValidationError {
      override def toString: String = "CVV is invalid (must be exactly 3 digits)"
    }
    final case object CvvContentInvalid extends ValidationError {
      override def toString: String = "CVV contains special characters"
    }

  }

  object PaymentCardValidator {

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateName(name: String): AllErrorsOr[CardOwnerName] = {
      def validateNameContents: AllErrorsOr[String] = {
        if (name.matches("^[a-zA-Z ]+$")) name.validNec
        else OwnerNameContentsInvalid.invalidNec
      }
      def validateNameLength(validName: String): AllErrorsOr[CardOwnerName] = {
        if (validName.length >= 3 && validName.length <= 30) CardOwnerName(validName).validNec
        else OwnerNameLengthInvalid.invalidNec
      }
      validateNameContents andThen validateNameLength
    }

    private def validateNumber(number: String): AllErrorsOr[CardNumber] = {
      def validateNumberContents: AllErrorsOr[String] = {
        if (number forall Character.isDigit) number.validNec
        else CardNumberContentsInvalid.invalidNec
      }
      def validateNumberLength(validNumber: String): AllErrorsOr[CardNumber] = {
        if (validNumber.length == 16) CardNumber(validNumber.toLong).validNec
        else CardNumberLengthInvalid.invalidNec
      }
      validateNumberContents andThen validateNumberLength
    }

    private def validateExpiryDate(date: String): AllErrorsOr[CardExpiryDate] = {
      def validateDateFormat: AllErrorsOr[LocalDate] = {
        try {
          val dateSplit = date.split("/")
          val month = dateSplit.head.toInt
          val year = dateSplit.last.toInt
          LocalDate
            .of(year.toInt, month.toInt, 1)
            .`with`(TemporalAdjusters.lastDayOfMonth())
            .validNec
        } catch {
          case NonFatal(_) => ExpiryDateFormatInvalid.invalidNec
        }
      }
      def validateExpired(date: LocalDate): AllErrorsOr[CardExpiryDate] = {
        if (!date.isAfter(LocalDate.now())) CardExpiryDate(date).validNec
        else ExpiryDateOverdue.invalidNec
      }
      validateDateFormat andThen validateExpired
    }

    private def validateCvv(cvv: String): AllErrorsOr[CVV] = {
      def validateCvvContents: AllErrorsOr[String] = {
        if (cvv forall Character.isDigit) cvv.validNec
        else CvvContentInvalid.invalidNec
      }
      def validateCvvLength(validCvv: String): AllErrorsOr[CVV] = {
        if (validCvv.length == 3) CVV(validCvv.toInt).validNec
        else CvvLengthInvalid.invalidNec
      }
      validateCvvContents andThen validateCvvLength
    }

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] =
      (validateName(name),
        validateNumber(number),
        validateExpiryDate(expirationDate),
        validateCvv(securityCode)
        ).mapN(PaymentCard)
  }
}
