package docs.processor

/**
 * Created by romastyi on 26/01/15.
 */

case class TextDocument ( var text: String ) extends Document {

    protected var findPos = -1
    protected var findStr = ""
    protected var rowStartPos = -1
    protected var rowFinishPos = -1
    protected var rowText = ""
    protected var lastRowPos = -1

    override def plainText: String = text

    override protected def clone(): Document = new TextDocument(this.text)

    override def first( str: String ): Boolean = {

        findStr = str
        findPos = 0
        rowStartPos = -1
        rowFinishPos = -1
        rowText = ""
        lastRowPos = -1
        next()
    }

    override def next(): Boolean = {

        findPos = if (findPos < 0) -1 else text.indexOf(findStr, findPos + findStr.length)
        findPos >= 0
    }

    override def findTableRow(): Boolean = {

        if (findPos >= 0) {
            rowStartPos = text.lastIndexOf("\n", findPos) + 1
            rowFinishPos = text.indexOf("\n", findPos)
            if (rowFinishPos < 0) rowFinishPos = text.length - 1
            rowText = text.substring(rowStartPos, rowFinishPos)
            lastRowPos = rowFinishPos
        }
        rowStartPos >= 0 && rowFinishPos >= 0
    }

    override def duplicateTableRow( row: Int ): Boolean = {

        if (lastRowPos >= 0) {
            val num = row.toString + ") "
            val (left, right) = text.splitAt(lastRowPos)
            text = left + "\n" + num + rowText + right
            lastRowPos += rowText.length + 1 + num.length
        }
        rowStartPos >= 0 && rowFinishPos >= 0
    }

    override def finalizeTable() = {

        if (rowStartPos >= 0 && rowFinishPos >= 0) {
            val (left, right) = text.splitAt(rowStartPos)
            text = left + right.drop(rowFinishPos - rowStartPos + 1)
            findPos = lastRowPos
        }
        rowStartPos = -1
        rowFinishPos = -1
        rowText = ""
        lastRowPos = -1
    }
}
