package docs.processor

/**
 * Created by romastyi on 26/01/15.
 */

import StringExtension._

case class TextDocument ( var text: String ) extends Document {

    protected var findPos = -1
    protected var findStr = ""
    protected var repeatStartPos = -1
    protected var repeatFinishPos = -1
    protected var repeatText = ""
    protected var lastRepeatPos = -1

    override def plainText: String = text

    override protected def clone: Document = new TextDocument(this.text)

    override def insertInCurrentPos( other: Document ) = {

        if (lastRepeatPos >= 0) {
            val str = other.plainText
            val (left, right) = text.splitAt(lastRepeatPos)
            text = left + str + right
            findPos += str.length
            lastRepeatPos = findPos
        }
    }

    override def first( str: String ): Boolean = {

        findStr = str
        findPos = 0
        repeatStartPos = -1
        repeatFinishPos = -1
        repeatText = ""
        lastRepeatPos = -1
        next()
    }

    override def next(): Boolean = {

        findPos = if (findPos < 0) -1 else text.indexOf(findStr, findPos + findStr.length)
        findPos >= 0
    }

    override def replace( search: String, replace: String, matchCase: Boolean = false ) =
        text = text.replaceAllSubstring(search, replace, matchCase)

    override def findRepetition( start: String, finish: String, exactMatching: Boolean ): Boolean = {

        if (findPos >= 0) {
            // Case insensitive search of substring
            repeatStartPos = text.toLowerCase.lastIndexOf(start.toLowerCase, findPos)
            repeatFinishPos = text.toLowerCase.indexOf(finish.toLowerCase, findPos)
            //
            if (!exactMatching || (repeatStartPos > 0 && repeatFinishPos > 0)) {
                if (repeatStartPos < 0) repeatStartPos = 0
                if (repeatFinishPos < 0)
                    repeatFinishPos = text.length - 1
                else
                    repeatFinishPos += finish.length
                repeatText = text.substring(repeatStartPos, repeatFinishPos)
                lastRepeatPos = repeatFinishPos
            }
        }
        repeatStartPos >= 0 && repeatFinishPos >= 0
    }

    override def duplicateRepetition(): Document = {

        if (lastRepeatPos >= 0) {
            findPos = lastRepeatPos
            new TextDocument(repeatText)
        } else {
            new TextDocument("")
        }
    }

    override def finalizeRepetition() = {

        if (repeatStartPos >= 0 && repeatFinishPos >= 0) {
            val (left, right) = text.splitAt(repeatStartPos)
            text = left + right.drop(repeatFinishPos - repeatStartPos)
            findPos = lastRepeatPos
        }
        repeatStartPos = -1
        repeatFinishPos = -1
        repeatText = ""
        lastRepeatPos = -1
    }

    override def findTableRow(): Boolean = {

        if (findRepetition("\n", "\n", exactMatching = false)) {
            if (repeatText.last == '\n') {
                repeatFinishPos -= 1
                repeatText = repeatText.drop(1)
            }
            true
        } else {
            false
        }
    }

}
