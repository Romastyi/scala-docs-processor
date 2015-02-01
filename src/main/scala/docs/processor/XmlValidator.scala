package docs.processor

/**
 * Created by romastyi on 23/01/15.
 */

import scala.xml._
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

case class XmlValidator( xml: Node ) extends Validator {

    import KeywordType._
    import OperationItem._
    import Qualifier._

    def this( xmlFile: String ) {
        this(XML.loadFile(xmlFile))
    }

    override def validate( keyword: Keyword ): KeywordType = {

        def checkQualifier( node: Node, ancestors: Seq[Node] ) = {

            keyword.qual match {
                case q if Repeatable.contains(q) =>
                    val msg = s"Keyword '${keyword.text}' cannot contain any repeat qualifier " +
                        s"(${Repeatable.mkString(", ")})."
                    (node.theSeq ++ ancestors.reverse).find(_.attribute("repeatable").isDefined) match {
                        case None =>
                            throw new ValidationErrorException(msg)
                        case Some(n: Node) =>
                            if (n.attribute("repeatable").get.head.text != "yes")
                                throw new ValidationErrorException(msg)
                    }
                case _ => ;
            }
        }

        var node: Node = null
        var ancestors = new ListBuffer[Node]()

        (xml descendant_or_self).find(_.label == "keywords") match {
            case None =>
                throw new ValidationErrorException("Could not find root node.")
            case Some(n: Node) => node = n
        }

        keyword.parsed foreach { o =>

            if (o.item != FunctionItem) {
                // Some keyword element
                if (o.item != KeywordItem)
                    throw new ValidationErrorException(s"Keyword cannot contain ${o.item}.")

                (node child).find(n => (n \ "@name").text == o.text) match {
                    case None =>
                        throw new ValidationErrorException(s"Could not find node with attribute 'name' == '${o.text}'.")
                    case Some(n: Node) =>
                        ancestors += n
                        node = n
                        breakable {
                            BindMap foreach { case (k, f) =>
                                (n \ s"@$f").headOption match {
                                    case Some(obj: Node) =>
                                        o.bind = Some(k)
                                        o.bindName = obj.text
                                        break()
                                    case None => ;
                                }
                            }
                        }
                }
            }
        }

        checkQualifier(node, ancestors)

        node.label match {
            case "word" => Keyword
            case "group" => Group
            case _ => Unknown
        }
    }

}
