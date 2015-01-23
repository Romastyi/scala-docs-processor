package docs.processor

/**
 * Created by romastyi on 23/01/15.
 */

import play.api.libs.json._

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

case class JsonValidator( json: JsValue ) extends Validator {

    import KeywordType._
    import OperationItem._
    import Qualifier._

    def this( str: String ) {
        this(Json.parse(str))
    }

    override def validate( keyword: Keyword ): KeywordType = {

        def findJsValue( obj: JsValue, items: Seq[String], value: String ): Option[(String, JsValue)] = {

            var res: Option[(String, JsValue)] = None

            breakable {

                items foreach { i =>

                    var v: JsValue = obj
                    (obj \ "child").asOpt[JsValue] match {
                        case Some(o: JsValue) => v = o
                        case None => ;
                    }

                    def check( o: JsValue ) = {

                        (o \ i \ "name").asOpt[String] match {
                            case Some(s: String) if s == value =>
                                res = Some((i, o \ i))
                                break()
                            case Some(_) => ;
                            case None => ;
                        }
                    }

                    if (v.asOpt[JsArray].isDefined) {
                        v.as[JsArray].value foreach { check(_) }
                    } else {
                        check(v)
                    }
                }
            }

            res
        }

        def checkQualifier( obj: JsValue, ancestors: Seq[JsValue] ) = {

            keyword.qual match {
                case q if Repeatable.contains(q) =>
                    val msg = s"Keyword '${keyword.text}' cannot contain any repeat qualifier " +
                        s"(${Repeatable.mkString(", ")})."
                    (obj :: Nil ++ ancestors.reverse) filter { o => (o \ "repeatable").asOpt[String].isDefined } headOption match {
                        case None =>
                            throw new ValidationErrorException(msg)
                        case Some(o: JsValue) =>
                            if ((o \ "repeatable").as[String] != "yes")
                                throw new ValidationErrorException(msg)
                    }
                case _ => ;
            }
        }

        var obj = ("", json)
        var ancestors = new ListBuffer[JsValue]()

        keyword.parsed foreach { o =>

            if (o.item != KeywordItem)
                throw new ValidationErrorException(s"Keyword can contain only $KeywordItem (found: ${o.item}).")

            findJsValue(obj._2, "group" :: "word" :: Nil, o.text) match {
                case None => ;
                    throw new ValidationErrorException(s"Could not find object with field 'name' == '${o.text}'.")
                case Some(n: (String, JsValue)) =>
                    ancestors += n._2
                    obj = n
            }
        }

        checkQualifier(obj._2, ancestors)

        obj._1 match {
            case "word" => Keyword
            case "group" => Group
            case _ => Unknown
        }
    }

}

