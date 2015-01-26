package docs.processor

/**
 * Created by romastyi on 20/01/15.
 */

import scala.collection.immutable

object Qualifier extends Enumeration {
    type Qualifier = Value
    val NoneQual, FORMAT, ROW, ROW_NUMBER, REPEAT, REPEAT_NUMBER = Value
    val Repeatable = Set(ROW, ROW_NUMBER, REPEAT, REPEAT_NUMBER)
    val Table = Set(ROW, ROW_NUMBER)
    var Repeat = Set(REPEAT, REPEAT_NUMBER)
    //
    val QualifierMap = immutable.HashMap(
        FORMAT -> "%.+",
        ROW -> "row",
        ROW_NUMBER -> "row.number",
        REPEAT -> "begin|end",
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

object Grammar {

    import docs.processor.KeywordType._
    import docs.processor.Operator._
    import docs.processor.Qualifier._
    import docs.processor.Statistics._

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

    def validateOperator( operator: String ): Operator = {

        val default = (NoneOper, OperatorDescription("", 0, 0))
        OperatorMap.find( operator == _._2.text).getOrElse(default)._1
    }

    def operatorDescription( operator: String ): OperatorDescription = {

        val default = (NoneOper, OperatorDescription("", 0, 0))
        OperatorMap.find( operator == _._2.text).getOrElse(default)._2
    }
}
