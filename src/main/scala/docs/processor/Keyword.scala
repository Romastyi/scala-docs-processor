package docs.processor

/**
 * Created by romastyi on 20/01/15.
 */

object KeywordType extends Enumeration {
    type KeywordType = Value
    val Unknown, Keyword, Group, Formula = Value
}

object OperationItem extends Enumeration {
    type OperationItem = Value
    val KeywordItem, NumberItem, OperatorItem = Value
}

import scala.collection.mutable

import KeywordType._
import OperationItem._
import Qualifier._
import Statistics._

case class Operation( text: String, item: OperationItem )

class ParsedQueue extends mutable.Queue[Operation]

case class Keyword( text: String = "", t: KeywordType = Unknown,
                    qual: Qualifier = NoneQual, format: String = "", stat: Statistics = NoneStat,
                    parsed: ParsedQueue = new ParsedQueue(),
                    var value: Any = None, var evaluated: Boolean = false )

class KeywordsMap extends mutable.HashMap[String, Keyword]