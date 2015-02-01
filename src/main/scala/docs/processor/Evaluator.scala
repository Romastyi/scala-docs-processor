package docs.processor

/**
 * Created by romastyi on 26.01.15.
 */

import scala.collection.mutable

case class EvaluatorErrorException( msg: String ) extends RuntimeException(s"Evaluator error:\n$msg")

abstract class Evaluator( implicit syntax: Syntax, validator: Validator ) {

    import KeywordType._
    import OperationItem._
    import Qualifier._
    import BindType._
    import Operator._

    protected var _doc: Document = null
    protected val separator = "\\"

    protected def copy( other: Document ): Evaluator

    protected def evaluate( document: Document ): Document = {

        document.parse
        if (_doc == null) _doc = document.copy()

        _doc.keywords.foreach( (text, key) => Table.contains(key.qual) , fillRepeat )
        _doc.keywords.foreach( (text, key) => Repeat.contains(key.qual), fillRepeat )
        _doc.keywords.foreach( (text, key) => key.t == Formula && !Repeatable.contains(key.qual), fillFormula )
        _doc.keywords.foreach( (text, key) => key.t == Keyword && !Repeatable.contains(key.qual), fillKeyword )

        _doc
    }

    // Duplicates and evaluates all repetitions (lists or tables)
    protected def fillRepeat( text: String, key: Keyword ) = {

        if (_doc.first(text))
            do {
                if (Table.contains(key.qual)) {
                    if (!_doc.findTableRow())
                        throw new EvaluatorErrorException(s"Keyword '$text' is not surrounded by a row of the table.")
                } else if (Repeat.contains(key.qual)) {
                    val start1 = syntax.start + key.text + syntax.qualifier + QualifierMap.getOrElse(REPEAT_BEGIN, "") + syntax.finish
                    val start2 = syntax.start + key.text + syntax.qualifier + QualifierMap.getOrElse(REPEAT_NUMBER, "") + syntax.finish
                    val finish = syntax.start + key.text + syntax.qualifier + QualifierMap.getOrElse(REPEAT_END, "") + syntax.finish
                    var found = false

                    if (key.qual == REPEAT_END) {
                        found = _doc.findRepetition(start1, text, exactMatching = true) ||
                                _doc.findRepetition(start2, text, exactMatching = true)
                    } else {
                        found = _doc.findRepetition(text, finish, exactMatching = true)
                    }

                    if (!found)
                        throw new EvaluatorErrorException(s"Could not found a pair for keyword '$text'.")
                } else {
                    throw new EvaluatorErrorException(s"Unknown repetition type in keyword '$text'.")
                }
                if (firstModel(key)) {
                    var rowIndex = 1
                    do {
                        val row = _doc.duplicateRepetition()
                        val eval = this.copy(row)
                        row.parse
                        row.keywords.map { case(t, k) =>
                            if (t == text) {
                                k.value = Some(rowIndex)
                                k.evaluated = true
                            }
                            (t, k)
                        }
                        row.keywords.foreach( (t, k) => t == text, eval.fillRepeatNumber )
                        _doc.insertInCurrentPos(eval.evaluate(row))
                        rowIndex += 1
                    } while (nextModel(key))
                    cleanModel(key)
                }
                _doc.finalizeRepetition()
            } while (_doc.next())
    }

    // Replace keyword with current repetition number passed in keyword value
    protected def fillRepeatNumber( text: String, key: Keyword ) = {

        Repeatable foreach { q =>
            val k = syntax.start + key.text + syntax.qualifier + QualifierMap.getOrElse(q, "") + syntax.finish
            _doc.replace(k, if (RepeatNumber.contains(q) && key.value.isDefined) key.value.get.toString else "")
        }
    }

    // Evaluates all formulas
    protected def fillFormula( text: String, key: Keyword ) = {

        if (key.parsed.nonEmpty) {

            val stack = new mutable.Stack[Double]()

            key.parsed foreach { op =>

                op.item match {
                    case NumberItem =>
                        stack.push(op.text.toDouble)
                    case KeywordItem =>
                        if (!_doc.keywords.contains(op.text)) {
                            _doc.keywords ++= evaluate(new TextDocument(op.text)).keywords
                        } else {
                            val k = _doc.keywords.get(op.text).get

                        }
                        stack.push(_doc.keywords.get(op.text).get.value.toString.toDouble)
                    case OperatorItem =>
                        val o = op.text
                        val c = Grammar.operatorDescription(o).operands
                        if (stack.size < c)
                            throw new EvaluatorErrorException(s"Wrong operands number for operator '$o' " +
                                s"(need: $c, found: ${stack.size}).")
                        stack.push(evalOperation(Grammar.validateOperator(o), stack.take(c).toList))
                }
            }

            if (stack.size > 1)
                throw new IllegalExpressionException("There is too many operands in expression.")
            key.value = Some(stack.pop())
        }
        key.evaluated = true
    }

    // Evaluates all simple keywords
    protected def fillKeyword( text: String, key: Keyword ) = {

        if (!key.evaluated) {
            key.value = getValue(key)
            key.evaluated = key.value.isDefined
        }
        _doc.replace(text, if (key.value.isDefined) key.value.get.toString else "")
    }

    protected def firstModel( key: Keyword ): Boolean
    protected def nextModel( key: Keyword ): Boolean
    protected def cleanModel( key: Keyword ): Boolean

    protected def modelQueue( key: Keyword ): mutable.Queue[String] = {

        val que = new mutable.Queue[String]()
        key.parsed.filter( _.bind == Some(ModelBind) ) foreach { o => que.enqueue(o.bindName) }
        que
    }

    protected def modelPath( que: mutable.Queue[String] ): String = que.mkString(separator)

    protected def getValue( key: Keyword ): Option[Any]

    protected def evalOperation( operator: Operator, args: List[Double] ): Double = {

        0
    }

}

