package docs.processor

/**
 * Created by romastyi on 26.01.15.
 */

case class EvaluatorErrorException( msg: String ) extends RuntimeException(s"Evaluator error:\n$msg")

abstract class Evaluator {

    import docs.processor.KeywordType._
    import docs.processor.Qualifier._

    protected var doc: Document = null

    def evaluate( document: Document )( implicit syntax: Syntax, validator: Validator ): Document = {

        doc = document.copy()
        if (!doc.parsed) doc.parse

        fill( (text, key) => Table.contains(key.qual), fillTable )
        fill( (text, key) => Repeat.contains(key.qual), fillRepeat )
        fill( (text, key) => key.t == Formula && !Repeatable.contains(key.qual), fillFormula )
        fill( (text, key) => key.t == Keyword && !Repeatable.contains(key.qual), fillKeyword )

        doc
    }

    protected def fill( p: (String, Keyword) => Boolean, f: (String, Keyword) => Any ) = {

        doc.keywords.filter(e => p(e._1, e._2)) foreach { e => f(e._1, e._2) }
    }

    // Duplicates and evaluates all rows in document
    protected def fillTable( text: String, key: Keyword ) = {

        if (doc.first(text))
            do {
                if (!doc.findTableRow())
                    throw new EvaluatorErrorException(s"Keyword '$text' is not surrounded by a row of the table.")
                if (firstModel(key)) {
                    var row = 1
                    do {
                        doc.duplicateTableRow(row)
                        row += 1
                    } while (nextModel(key))
                }
                doc.finalizeTable()
            } while (doc.next())
    }

    // Duplicates and evaluates all repetitions
    protected def fillRepeat( text: String, key: Keyword ) = {

        println("repeat: " + text + "(" + key.t + ")")
    }

    // Evaluates all formulas
    protected def fillFormula( text: String, key: Keyword ) = {

        println("formula: " + text + "(" + key + ")")
    }

    // Evaluates all simple keywords
    protected def fillKeyword( text: String, key: Keyword ) = {

        println("keyword: " + text + "(" + key.t + ")")
    }

    protected def firstModel( key: Keyword ): Boolean
    protected def nextModel( key: Keyword ): Boolean
}

