package docs.processor

/**
 * Created by romastyi on 20/01/15.
 */

import scala.collection.immutable
import scala.util.control.Breaks._

object Qualifier extends Enumeration {
    type Qualifier = Value
    val NoneQual, FORMAT, ROW, ROW_NUMBER, REPEAT_BEGIN, REPEAT_END, REPEAT_NUMBER = Value
    val Repeatable = Set(ROW, ROW_NUMBER, REPEAT_BEGIN, REPEAT_END, REPEAT_NUMBER)
    val Table = Set(ROW, ROW_NUMBER)
    var Repeat = Set(REPEAT_BEGIN, REPEAT_END, REPEAT_NUMBER)
    val RepeatNumber = Set(ROW_NUMBER, REPEAT_NUMBER)
    //
    val QualifierMap = immutable.HashMap(
        FORMAT -> "%.+",
        ROW -> "row",
        ROW_NUMBER -> "row.number",
        REPEAT_BEGIN -> "begin",
        REPEAT_END -> "end",
        REPEAT_NUMBER -> "begin.number"
    )
}

object Statistics extends Enumeration {
    type Statistics = Value
    val NoneStat, EXISTS, COUNT, AVG, MIN, MAX, SUM = Value
    //
    val StatisticsMap = immutable.HashMap(
        EXISTS -> "exists",
        COUNT -> "count",
        AVG -> "avg",
        MIN -> "min",
        MAX -> "max",
        SUM -> "sum"
    )
}

object Operator extends Enumeration {
    type Operator = Value
    val NoneOper, SUM, SUB, MUL, POW, DIV, MOD = Value
    //
    case class OperatorDescription( text: String, priority: Int, operands: Int )
    val OperatorMap = immutable.HashMap(
        POW -> OperatorDescription("^", 3, 2),
        MUL -> OperatorDescription("*", 2, 2),
        DIV -> OperatorDescription("/", 2, 2),
        MOD -> OperatorDescription("%", 2, 2),
        SUM -> OperatorDescription("+", 1, 2),
        SUB -> OperatorDescription("-", 1, 2)
    )
}

object Functions extends Enumeration {
    type Functions = Value
    val NoneFunc, TRIM = Value
    //
    val FunctionsMap = immutable.HashMap(
        TRIM -> "trim"
    )
}

object Grammar {

    import docs.processor.KeywordType._
    import docs.processor.Operator._
    import docs.processor.Qualifier._
    import docs.processor.Statistics._
    import docs.processor.Functions._

    class Pos[T]( var value: T )
    val PosNotFound = 0
    val PosStarted = 1
    val PosFinished = 2

    def checkSymbolPos( c: Char, str: String, pos: Pos[Int] = new Pos(0) ): Int = {

        if (pos.value == str.length) pos.value = 0
        if (str.charAt(pos.value) == c) {
            pos.value += 1
            if (pos.value == str.length)
                PosFinished
            else
                PosStarted
        } else {
            pos.value = 0
            PosNotFound
        }
    }

    def validateKeyword( keyword: Keyword )( implicit validator: Validator ): KeywordType = {

        validator.validate(keyword)
    }

    def validateQualifier( qualifier: String ): Qualifier = {

        val default = (NoneQual, "")
        QualifierMap.find( qualifier matches "(?i)" + _._2 ).getOrElse(default)._1
    }

    def validateStatistics( statistics: String ): Statistics = {

        val default = (NoneStat, "")
        StatisticsMap.find( statistics matches "(?i)" + _._2 ).getOrElse(default)._1
    }

    def checkOperatorPos( c: Char, operator: String, pos: Pos[Int] = new Pos(0) ): Int = {

        var res = PosNotFound

        breakable {
            OperatorMap foreach { case (o, s) =>
                if (operator.isEmpty || s.text.indexOf(operator) == 0) {
                    val p = checkSymbolPos(c, s.text, pos)
                    if (p != PosNotFound) {
                        res = p
                        break()
                    }
                }
            }
        }

        res
    }
    
    def validateOperator( operator: String ): Operator = {

        val default = (NoneOper, OperatorDescription("", 0, 0))
        OperatorMap.find( operator == _._2.text ).getOrElse(default)._1
    }

    def operatorDescription( operator: String ): OperatorDescription = {

        val default = (NoneOper, OperatorDescription("", 0, 0))
        OperatorMap.find( operator == _._2.text ).getOrElse(default)._2
    }

    def validateFunction( function: String ): Functions = {

        val default = (NoneFunc, "")
        FunctionsMap.find( function.toLowerCase == _._2 ).getOrElse(default)._1
    }
}
