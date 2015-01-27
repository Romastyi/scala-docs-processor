/**
 * Created by romastyi on 23/01/15.
 */

import org.scalatest._
import docs.processor._

class GrammarSpec extends FlatSpec with Matchers {

    import Qualifier._
    import Statistics._

    implicit val syntax = new DefaultSyntax()

    "Grammar" should "check qualifiers" in {
        Grammar.validateQualifier("") shouldBe NoneQual
        Grammar.validateQualifier(" ") shouldBe NoneQual
        // FORMAT
        Grammar.validateQualifier("%.2f") shouldBe FORMAT
        Grammar.validateQualifier("%YY.DD.MM") shouldBe FORMAT
        Grammar.validateQualifier("%%") shouldBe FORMAT
        Grammar.validateQualifier("% ") shouldBe FORMAT
        Grammar.validateQualifier(" % ") shouldBe NoneQual
        Grammar.validateQualifier("%") shouldBe NoneQual
        Grammar.validateQualifier(".2f") shouldBe NoneQual
        Grammar.validateQualifier("YY.DD.MM") shouldBe NoneQual
        Grammar.validateQualifier("111") shouldBe NoneQual
        // ROW
        Grammar.validateQualifier("ROW") shouldBe ROW
        Grammar.validateQualifier("row") shouldBe ROW
        Grammar.validateQualifier("RoW") shouldBe ROW
        Grammar.validateQualifier("ROW ") shouldBe NoneQual
        Grammar.validateQualifier(" ROW") shouldBe NoneQual
        Grammar.validateQualifier("ROW1213") shouldBe NoneQual
        // ROW.NUMBER
        Grammar.validateQualifier("ROW.NUMBER") shouldBe ROW_NUMBER
        Grammar.validateQualifier("row.number") shouldBe ROW_NUMBER
        Grammar.validateQualifier("RoW.Number") shouldBe ROW_NUMBER
        Grammar.validateQualifier("ROW::number") shouldBe NoneQual
        Grammar.validateQualifier("ROW.NUMBER ") shouldBe NoneQual
        Grammar.validateQualifier("ROWnumber") shouldBe NoneQual
        // REPEAT (BEGIN)
        Grammar.validateQualifier("BEGIN") shouldBe REPEAT_BEGIN
        Grammar.validateQualifier("begin") shouldBe REPEAT_BEGIN
        Grammar.validateQualifier("bEGin") shouldBe REPEAT_BEGIN
        Grammar.validateQualifier("BEGIN ") shouldBe NoneQual
        Grammar.validateQualifier(" BEGIN") shouldBe NoneQual
        Grammar.validateQualifier("BEGINqweq") shouldBe NoneQual
        Grammar.validateQualifier("BEGINEND") shouldBe NoneQual
        // REPEAT (BEGIN.NUMBER)
        Grammar.validateQualifier("BEGIN.NUMBER") shouldBe REPEAT_NUMBER
        Grammar.validateQualifier("begin.number") shouldBe REPEAT_NUMBER
        Grammar.validateQualifier("bEGin.Number") shouldBe REPEAT_NUMBER
        Grammar.validateQualifier("BEGIN::NUMBER") shouldBe NoneQual
        Grammar.validateQualifier("BEGIN.NUMBER ") shouldBe NoneQual
        Grammar.validateQualifier("BEGINnumber") shouldBe NoneQual
        // REPEAT (END)
        Grammar.validateQualifier("END") shouldBe REPEAT_END
        Grammar.validateQualifier("end") shouldBe REPEAT_END
        Grammar.validateQualifier("End") shouldBe REPEAT_END
        Grammar.validateQualifier("end ") shouldBe NoneQual
        Grammar.validateQualifier(" end") shouldBe NoneQual
        Grammar.validateQualifier("ENDqweq") shouldBe NoneQual
        Grammar.validateQualifier("END.NUMBER") shouldBe NoneQual
    }

    "Grammar" should "check statistics" in {
        Grammar.validateStatistics("") shouldBe NoneStat
        Grammar.validateStatistics(" ") shouldBe NoneStat
        // EXISTS
        Grammar.validateStatistics("EXISTS") shouldBe EXISTS
        Grammar.validateStatistics("exists") shouldBe EXISTS
        Grammar.validateStatistics("exIsts") shouldBe EXISTS
        Grammar.validateStatistics("EXISTS ") shouldBe NoneStat
        Grammar.validateStatistics(" EXISTS") shouldBe NoneStat
        Grammar.validateStatistics("exists1") shouldBe NoneStat
        // COUNT
        Grammar.validateStatistics("COUNT") shouldBe COUNT
        Grammar.validateStatistics("count") shouldBe COUNT
        Grammar.validateStatistics("cOUnt") shouldBe COUNT
        Grammar.validateStatistics("COUNT ") shouldBe NoneStat
        Grammar.validateStatistics(" COUNT") shouldBe NoneStat
        Grammar.validateStatistics("cOUnt43") shouldBe NoneStat
        // AVG
        Grammar.validateStatistics("AVG") shouldBe AVG
        Grammar.validateStatistics("avg") shouldBe AVG
        Grammar.validateStatistics("avG") shouldBe AVG
        Grammar.validateStatistics("AVG ") shouldBe NoneStat
        Grammar.validateStatistics(" AVG") shouldBe NoneStat
        Grammar.validateStatistics("avgeqwrq") shouldBe NoneStat
        // MIN
        Grammar.validateStatistics("MIN") shouldBe MIN
        Grammar.validateStatistics("min") shouldBe MIN
        Grammar.validateStatistics("mIN") shouldBe MIN
        Grammar.validateStatistics("MIN ") shouldBe NoneStat
        Grammar.validateStatistics(" MIN") shouldBe NoneStat
        Grammar.validateStatistics("MIN121241") shouldBe NoneStat
        // MAX
        Grammar.validateStatistics("MAX") shouldBe MAX
        Grammar.validateStatistics("max") shouldBe MAX
        Grammar.validateStatistics("mAx") shouldBe MAX
        Grammar.validateStatistics("mAx ") shouldBe NoneStat
        Grammar.validateStatistics(" mAx") shouldBe NoneStat
        Grammar.validateStatistics("max1fadsf") shouldBe NoneStat
        // SUM
        Grammar.validateStatistics("SUM") shouldBe SUM
        Grammar.validateStatistics("sum") shouldBe SUM
        Grammar.validateStatistics("suM") shouldBe SUM
        Grammar.validateStatistics("SUM ") shouldBe NoneStat
        Grammar.validateStatistics(" SUM") shouldBe NoneStat
        Grammar.validateStatistics("SUM2142") shouldBe NoneStat
    }
}
