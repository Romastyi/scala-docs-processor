package docs.processor

/**
 * Created by romastyi on 27.01.15.
 */

import scala.collection.mutable
import scala.util.control.Breaks._

import play.api.libs.json._

object JsonEvaluator {

    implicit class EvaluateDocumentWithJsonModel( document: Document )
                                                (implicit syntax: Syntax, validator: Validator) extends Evaluator {

        protected var _model: JsValue = JsNull
        protected val _context: mutable.Map[String, JsValue] = new mutable.HashMap()

        override protected def copy(other: Document): Evaluator = {

            val obj = new EvaluateDocumentWithJsonModel(document)
            obj._doc = other
            obj._model = this._model
            obj._context ++= this._context
            obj
        }

        def evaluate(model: JsValue)(implicit syntax: Syntax, validator: Validator): Document = {

            _model = model
            evaluate(document)
        }

        protected def findModel(node: JsValue, name: String, after: Option[JsValue]): Option[JsValue] = {

            var opt: Option[JsValue] = None
            var found = false

            def checkNode( n: JsValue ): Option[JsValue] = {

                var o: Option[JsValue] = None
                after match {
                    case None => o = Some(n)
                    case Some(e: JsValue) =>
                        if (e.equals(n)) {
                            found = true
                        } else if (found) {
                            o = Some(n)
                        }
                }
                o
            }

            breakable {
                (node \\ name) foreach {
                    case a: JsArray =>
                        a.value foreach { e =>
                            if (checkNode(e).isDefined) {
                                opt = Some(e)
                                break()
                            }
                        }
                    case e =>
                        if (checkNode(e).isDefined) {
                            opt = Some(e)
                            break()
                        }
                }
            }

            opt
        }

        protected def nearestModel(key: Keyword, after: Option[JsValue] = None): Option[JsValue] = {

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
                            case Some(n: JsValue) =>
                                _context += str -> n
                                node = n
                        }
                    case Some(n: (String, JsValue)) => node = n._2
                }
            }

            Some(node)
        }

        protected def firstModel(key: Keyword): Boolean = {

            removeModel(key)
            nearestModel(key).isDefined
        }

        protected def nextModel(key: Keyword): Boolean = {

            nearestModel(key, removeModel(key)).isDefined
        }

        protected def cleanModel(key: Keyword): Boolean = {

            removeModel(key).isDefined
        }

        protected def removeModel(key: Keyword): Option[JsValue] = {

            val name = modelPath(modelQueue(key))
            _context.keys.filter(_.indexOf(name + separator) == 0) foreach { k => _context.remove(k)}
            _context.remove(name)
        }

        protected def getValue(key: Keyword): Option[Any] = {

            nearestModel(key) match {
                case None => None
                case Some(n: JsValue) =>
                    val field = key.parsed.last.bindName
                    (n \\ field) headOption match {
                        case Some(b: JsBoolean) => Some(b.value)
                        case Some(i: JsNumber) => Some(i.value)
                        case Some(s: JsString) => Some(s.value)
                        case _ => None
                    }
            }
        }

    }

}
