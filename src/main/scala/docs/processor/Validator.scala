package docs.processor

/**
 * Created by romastyi on 23/01/15.
 */

case class ValidationErrorException( msg: String ) extends RuntimeException(s"Validation error:\n$msg")

abstract class Validator {

    import KeywordType.KeywordType

    def validate( keyword: Keyword ): KeywordType
}

