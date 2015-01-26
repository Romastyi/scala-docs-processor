package docs.processor

/**
 * Created by romastyi on 26.01.15.
 */

import scala.collection.mutable
import scala.xml._
import scala.util.control.Breaks._

object XmlEvaluator {

    implicit class EvaluateDocumentWithXmlModel( document: Document ) extends Evaluator {

        import BindType._

        var _model: Node = null
        val context: mutable.Map[String, Node] = new mutable.HashMap()

        def evaluate( model: Node )( implicit syntax: Syntax, validator: Validator ): Document = {

            _model = model
            evaluate(document)
        }

        protected def findModel( node: Node, name: String, after: Option[Node] ): Option[Node] = {

            var opt: Option[Node] = None
            var found = false

            breakable {
                (node descendant_or_self) filter { _.label == name } foreach { n =>
                    after match {
                        case None =>
                            opt = Some(n)
                            break()
                        case Some(a: Node) =>
                            if (a.equals(n)) {
                                found = true
                            } else if (found) {
                                opt = Some(n)
                                break()
                            }
                    }
                }
            }

            opt
        }

        protected def modelQueue( key: Keyword ): mutable.Queue[String] = {

            val que = new mutable.Queue[String]()
            key.parsed.filter( _.bind == Some(ModelBind) ) foreach { o => que.enqueue(o.bindName) }
            que
        }

        protected def modelName( que: mutable.Queue[String] ): String = {

            var name = ""
            que foreach { s => name += (if (name.nonEmpty) "." else "") + s }
            name
        }

        protected def nearestModel( key: Keyword, after: Option[Node] = None ): Option[Node] = {

            val que = modelQueue(key)
            var str = ""
            var node = _model

            while (que.nonEmpty) {

                val s = que.dequeue()
                str += (if (str.nonEmpty) "." else "") + s
                context find { e => e._1 == str } headOption match {
                    case None =>
                        findModel(node, s, after) match {
                            case None =>
                                return None
//                                throw new EvaluatorErrorException(s"Could not found model by name '$str'.")
                            case Some(n: Node) =>
                                context += str -> n
                                node = n
                        }
                    case Some(n: (String, Node)) => node = n._2
                }
            }

            Some(node)
        }

        protected def firstModel( key: Keyword ): Boolean = {

            val name = modelName(modelQueue(key))
            context.remove(name)
            nearestModel(key).isDefined
        }

        protected def nextModel( key: Keyword ): Boolean = {

            val name = modelName(modelQueue(key))
            nearestModel(key, context.remove(name)).isDefined
        }

    }
}

