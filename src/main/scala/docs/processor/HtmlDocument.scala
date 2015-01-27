package docs.processor

/**
 * Created by romastyi on 27.01.15.
 */

class HtmlDocument( html: String ) extends TextDocument(html) {

    override protected def clone: Document = new HtmlDocument(this.text)

    override def findTableRow(): Boolean = {

        findRepetition("<tr>", "</tr>", exactMatching = true)
    }

}
