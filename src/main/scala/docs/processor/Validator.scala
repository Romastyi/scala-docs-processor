package docs.processor

/**
 * Created by romastyi on 23/01/15.
 */

import scala.collection.immutable

import KeywordType._

case class ValidationErrorException( msg: String ) extends RuntimeException(s"Validation error:\n$msg")

trait Validator {

    import BindType._

    val BindMap = immutable.HashMap(
        ModelBind -> "model",
        FieldBind -> "field"
    )

    def validate( keyword: Keyword ): KeywordType
}

class DummyValidator extends Validator {

    override def validate( keyword: Keyword ): KeywordType = { Unknown }
}
