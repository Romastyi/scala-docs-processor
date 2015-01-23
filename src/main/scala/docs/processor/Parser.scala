package docs.processor

/**
 * Created by romastyi on 17/01/15.
 */

import scala.collection.mutable
import scala.util.control.Breaks._

case class IllegalExpressionException( msg: String ) extends RuntimeException(s"Illegal expression:\n$msg")

object Parser {

    import KeywordType._
    import OperationItem._
    import Operator._
    import Qualifier._
    import Statistics._

    class Pos[T]( var value: T )
    val PosNotFound = 0
    val PosStarted = 1
    val PosFinished = 2
    // Convert string to number (Int or Double)
    case class ParseOp[T](op: String => T)
    implicit val popDouble = ParseOp[Double](_.toDouble)
    implicit val popInt = ParseOp[Int](_.toInt)
    def parseAny[T: ParseOp](s: String) = try { Some(implicitly[ParseOp[T]].op(s)) } catch { case _: Throwable => None }

    def checkSymbol(c: Char, str: String, pos: Pos[Int] = new Pos(0)): Int = {

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

    def parse( text: String )( implicit syntax: Syntax, validator: Validator ): KeywordsMap = {

        val keywords = new KeywordsMap()
        val startPoses: mutable.Stack[Int] = new mutable.Stack()
        val inStartPos = new Pos(0)
        val inFinishPos = new Pos(0)

        text.zipWithIndex foreach { case (c, i) =>
            // Parsing
            if (checkSymbol(c, syntax.start, inStartPos) == PosFinished) {
                // Start of keyword is detected
                startPoses.push(i - syntax.start.length + 1)
            } else if (checkSymbol(c, syntax.finish, inFinishPos) == PosFinished && startPoses.nonEmpty) {
                // Finish of keyword is detected
                val keyword = text.substring(startPoses.pop(), i + 1)
                try {
                    val k = parseExpression(keyword)
                    if (k.t != Unknown) keywords += keyword -> k
                } catch {
                    case e: Throwable =>
                        Console.println(e.toString)
                        startPoses.clear()
                }
            }
        }

        keywords
    }

    def parseKeyword( keyword: String, onlyKeyword: Boolean = false )( implicit syntax: Syntax, validator: Validator ): Keyword = {

        if (!keyword.startsWith(syntax.start) || !keyword.endsWith(syntax.finish)) {
            throw new IllegalExpressionException(keyword)
        }

        val que = new ParsedQueue()
        var text = ""

        def enqueue( str: String ) = {
            if (str.isEmpty)
                throw new IllegalExpressionException(s"Keyword cannot contain empty parts ($keyword).")
            if (que.nonEmpty) text += syntax.separator
            text += str
            que.enqueue(new Operation(str, KeywordItem))
        }

        var kwrd = keyword.drop(syntax.start.length).dropRight(syntax.finish.length)
        var qual: Qualifier = NoneQual
        var stat: Statistics = NoneStat
        var fmt = ""

        // Find qualifiers
        var i = 0
        var qPos = kwrd.lastIndexOf(syntax.qualifier)
        while (qPos >= 0) {
            i += 1
            if (i > 1)
                throw new IllegalExpressionException(s"Keyword can contain only one qualifier ($keyword).")
            val q = kwrd.substring(qPos + syntax.qualifier.length)
            Grammar.validateQualifier(q) match {
                case NoneQual =>
                    throw new IllegalExpressionException(s"Unknown qualifier '$q' in keyword ($keyword).")
                case FORMAT =>
                    qual = FORMAT
                    fmt = q
                case t: Qualifier =>
                    qual = t
            }
            kwrd = kwrd.dropRight(q.length + syntax.qualifier.length)
            qPos = kwrd.lastIndexOf(syntax.qualifier)
        }

        // Parse keyword on parts
        var pos = kwrd.indexOf(syntax.separator)
        while (pos >= 0) {
            val part = kwrd.substring(0, pos)
            if (part.lastIndexOf(syntax.qualifier) >= 0)
                throw new IllegalExpressionException(s"Qualifier can be only at the end of keyword ($keyword).")
            enqueue(part)
            kwrd = kwrd.drop(pos + syntax.separator.length)
            pos = kwrd.indexOf(syntax.separator)
        }

        Grammar.validateStatistics(kwrd) match {
            case NoneStat => enqueue(kwrd)
            case s: Statistics => stat = s
        }

        Grammar.validateKeyword(new Keyword(text, Keyword, qual, fmt, stat, que)) match {
            case Unknown =>
                throw new IllegalExpressionException(s"Unknown keyword ($keyword).")
            case Group =>
                if (!Repeatable.contains(qual))
                    throw new IllegalExpressionException(s"Group keyword must contain repeat qualifier " +
                        s"(found: $qual, expected: ${Repeatable.mkString(", ")}) ($keyword).")
                if (stat != NoneStat)
                    throw new IllegalExpressionException(s"Group keyword cannot contain statistics function " +
                        s"($keyword)")
                new Keyword(text, Group, qual, fmt, stat, que)
            case t: KeywordType =>
                new Keyword(text, t, qual, fmt, stat, que)
        }
    }

    protected def parseExpression( text: String )( implicit syntax: Syntax, validator: Validator ): Keyword = {

        val que = new ParsedQueue()
        var typ: KeywordType = Unknown
        var qlf: Qualifier = NoneQual
        var fmt: String = ""
        var stt: Statistics = NoneStat

        // Process expression (keyword, formula, etc.)
        def procExpr( expr: String, operStack: mutable.Stack[Operation], level: Int = 0 ): Unit = {

            if (!expr.startsWith(syntax.start) || !expr.endsWith(syntax.finish)) {
                throw new IllegalExpressionException(expr)
            }

            val text = expr.drop(syntax.start.length).dropRight(syntax.finish.length)
            val startPoses: mutable.Stack[Int] = new mutable.Stack()
            val inStartPos = new Pos(0)
            val inFinishPos = new Pos(0)
            var op = ""
            var t: KeywordType = Unknown
            var l = level

            // Iterate through expression and find inner keywords or expressions
            text.zipWithIndex foreach { case (c, i) =>

                if (checkSymbol(c, syntax.start, inStartPos) == PosFinished) {
                    // Start of keyword is detected
                    startPoses.push(i - syntax.start.length + 1)
                    // Process operations before keyword
                    procOper(op, operStack, l, withoutFormat = true)
                    l += 1
                    // Null operations
                    op = ""
                } else if (checkSymbol(c, syntax.finish, inFinishPos) == PosFinished) {
                    if (startPoses.isEmpty)
                        throw new IllegalExpressionException(expr)
                    // Finish of keyword is detected
                    t = Formula
                    procExpr(text.substring(startPoses.pop(), i + 1), operStack, l)
                    l -= 1
                    // Null operations
                    op = ""
                } else {
                    op += c
                }
            }

            // Loop ends
            if (t == Formula) {
                if (level == 0) {
                    procOper(op, operStack, level)
                    while (operStack.nonEmpty) {
                        val o = operStack.pop()
                        if (o.text == "(" || o.text == ")")
                            throw new IllegalExpressionException(s"Not matching '(' or ')' in expression '$expr'.")
                        que.enqueue(o)
                    }
                    typ = t
                } else {
                    procOper(op, new mutable.Stack[Operation](), level)
                    que.enqueue(new Operation(expr, KeywordItem))
                }
            } else {
                val k = parseKeyword(expr)
                if (k.t == Unknown)
                    throw new IllegalExpressionException(s"Unknown keyword '$expr'.")
                if (level == 0) {
                    que.enqueue(k.parsed: _*)
                    typ = k.t
                    fmt = k.format
                    stt = k.stat
                } else {
                    if (k.t != Keyword)
                        throw new IllegalExpressionException(s"Unexpected keyword in formula ($expr).")
                    if (k.qual != NoneQual)
                        throw new IllegalExpressionException(s"Unexpected keyword qualifier in formula ($expr).")
                    if (level == 1) que.enqueue(new Operation(expr, KeywordItem))
                }
            }
        }

        // Process operations using "Reverse Polish notation"
        def procOper( oper: String, operStack: mutable.Stack[Operation], level: Int = 0, withoutFormat: Boolean = false )
                    ( implicit syntax: Syntax )= {

            var num = ""

            def enqueue( text: String, item: OperationItem ) = { if (level == 0) que.enqueue(new Operation(text, item)) }

            def validateNumber() = {
                if (num.nonEmpty) {
                    parseAny[Double](num) match {
                        case None =>
                            throw new NumberFormatException(s"Not valid number '$num' in expression '$oper'.")
                        case Some(d: Double) =>
                            enqueue(num, NumberItem)
                    }
                }
                num = ""
            }

            val qualPos = new Pos(0)

            breakable {

                oper.zipWithIndex foreach { case (c, i) =>

                    if (c.isDigit) {
                        num += c
                    } else if ((c.equals('.') || c.equals(',')) && num.nonEmpty) {
                        num += java.text.DecimalFormatSymbols.getInstance.getDecimalSeparator
                    } else {
                        validateNumber()
                        if (c.isSpaceChar) {
                            // continue
                        } else if (c == '(') {
                            operStack.push(new Operation("(", OperatorItem))
                        } else if (c == ')') {
                            var o = ""
                            while (o != "(" && operStack.nonEmpty) {
                                val obj = operStack.pop()
                                o = obj.text
                                if (o != "(") enqueue(o, obj.item)
                            }
                            if (o != "(")
                                throw new IllegalExpressionException(s"Missing '(' in expression '$oper' (pos: $i).")
                        } else if (Grammar.validateOperator(s"$c") != NoneOper) {
                            val desc = Grammar.operatorDescription(s"$c")
                            breakable {
                                while (operStack.nonEmpty) {
                                    val o = operStack.head.text
                                    if (desc.priority <= Grammar.operatorDescription(o).priority && o != "(") {
                                        val obj = operStack.pop()
                                        enqueue(obj.text, obj.item)
                                    } else
                                        break()
                                }
                            }
                            operStack.push(new Operation(s"$c", OperatorItem))
                        } else {
                            if (checkSymbol(c, syntax.qualifier, qualPos) == PosNotFound)
                                throw new IllegalExpressionException(s"Unknown symbol in expression '$oper' (pos: $i).")
                            validateNumber()
                            val s = oper.substring(i + 1).trim
                            if (s.isEmpty)
                                throw new IllegalExpressionException(s"Empty qualifier in expression '$oper' (pos: $i).")
                            if (s.contains(syntax.qualifier))
                                throw new IllegalExpressionException(s"Expression can contain only one qualifier ($oper) " +
                                    s"(pos: $i).")
                            qlf = Grammar.validateQualifier(s)
                            qlf match {
                                case NoneQual =>
                                    throw new IllegalExpressionException(s"Unknown qualifier '$s' in expression '$oper' " +
                                        s"(pos: $i).")
                                case q if Repeatable.contains(q) =>
                                    throw new IllegalExpressionException(s"Expression cannot contains repeat qualifier " +
                                        s"($oper) (pos: $i).")
                                case FORMAT => if (withoutFormat)
                                    throw new IllegalExpressionException(s"Expression cannot contains qualifier ($oper) " +
                                        s"(pos: $i).")
                            }
                            fmt = s
                            break()
                        }
                    }
                }
            }

            validateNumber()
        }

        // Checks if parsed queue is right
        def fakeEval( parsed: mutable.Queue[Operation] ) {

            if (parsed.nonEmpty) {

                var operands = 0

                parsed foreach { op =>

                    op.item match {
                        case KeywordItem | NumberItem => operands += 1
                        case OperatorItem =>
                            val o = op.text
                            val c = Grammar.operatorDescription(o).operands
                            if (operands < c)
                                throw new IllegalExpressionException(s"Wrong operands number for operator '$o' (need: $c, found: $operands).")
                            operands -= c - 1
                    }
                }

                if (operands > 1)
                    throw new IllegalExpressionException("There is too many operands in expression.")
                else if (operands <= 0)
                    throw new IllegalExpressionException("There is not enough operands in expression.")
            }
        }

        procExpr(text, new mutable.Stack())
        if (typ == Formula) fakeEval(que)
        new Keyword(text, typ, qlf, fmt, stt, que)
    }
}
