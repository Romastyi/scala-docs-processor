package docs.processor

/**
 * Created by romastyi on 26.01.15.
 */

import scala.collection.mutable
import scala.xml._
import scala.util.control.Breaks._

object XmlEvaluator {

    implicit class EvaluateDocumentWithXmlModel( document: Document )
                                               ( implicit syntax: Syntax, validator: Validator ) extends Evaluator {

        protected var _model: Node = null
        protected val _context: mutable.Map[String, Node] = new mutable.HashMap()

        override protected def copy( other: Document ): Evaluator = {

            val obj = new EvaluateDocumentWithXmlModel(document)
            obj._doc = other
            obj._model = this._model
            obj._context ++= this._context
            obj
        }

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

        protected def nearestModel( key: Keyword, after: Option[Node] = None ): Option[Node] = {

            val que = modelQueue(key)
            var str = ""
            var node = _model

            while (que.nonEmpty) {

                val s = que.dequeue()
                str += (if (str.nonEmpty) separator else "") + s
                _context find { e => e._1 == str } headOption match {
                    case None =>
                        findModel(node, s, after) match {
                            case None =>
                                return None
                                //throw new EvaluatorErrorException(s"Could not found model by name '$str'.")
                            case Some(n: Node) =>
                                _context += str -> n
                                node = n
                        }
                    case Some(n: (String, Node)) => node = n._2
                }
            }

            Some(node)
        }

        protected def firstModel( key: Keyword ): Boolean = {

            removeModel(key)
            nearestModel(key).isDefined
        }

        protected def nextModel( key: Keyword ): Boolean = {

            nearestModel(key, removeModel(key)).isDefined
        }

        protected def cleanModel( key: Keyword ): Boolean = {

            removeModel(key).isDefined
        }

        protected def removeModel( key: Keyword ): Option[Node] = {

            val name = modelPath(modelQueue(key))
            _context.keys.filter( _.indexOf(name + separator) == 0 ) foreach { k => _context.remove(k) }
            _context.remove(name)
        }

        protected def getValue( key: Keyword ): Option[Any] = {

            nearestModel(key) match {
                case None => None
                case Some(n: Node) =>
                    val field = key.parsed.last.bindName
                    (n descendant_or_self) find { c => c.label == field } headOption match {
                        case None => None
                        case Some(c: Node) => Some(c.text)
                    }
            }
        }

    }
}

