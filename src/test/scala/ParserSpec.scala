/**
 * Created by romastyi on 23/01/15.
 */

import docs.processor._
import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {

    import KeywordType._

    "Parser" should "parse keyword (default syntax, custom validator)" in {

        implicit val syntax = new DefaultSyntax()
        implicit object myValidator extends Validator {
            override def validate( keyword: Keyword ): KeywordType = {
                Unknown
            }
        }

//        Parser.parseKeyword("")
    }

}
