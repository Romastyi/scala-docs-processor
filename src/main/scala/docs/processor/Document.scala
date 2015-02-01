package docs.processor

/**
 * Created by romastyi on 26.01.15.
 */

class Document {

    private var _keywords = new KeywordsMap()
    private var _parsed = false

    def copy(): Document = {
        val obj: Document = this.clone()
        obj._keywords = this._keywords.clone()
        obj._parsed = this._parsed
        obj
    }

    override protected def clone: Document = null

    def keywords = _keywords
    def parsed = _parsed

    def plainText: String = ""

    def parse( implicit syntax: Syntax, validator: Validator ): Document = {

        if (!_parsed) {
            _keywords = Parser.parse(plainText)
            _parsed = true
        }
        this
    }

    def insertInCurrentPos( other: Document ) = {}
    def first( str: String ): Boolean = false
    def next(): Boolean = false
    def replace(search: String, replace: String, matchCase: Boolean = false) = {}

    def findTableRow(): Boolean = false
    def findRepetition( start: String, finish: String, exactMatching: Boolean ): Boolean = false
    def duplicateRepetition(): Document = new Document
    def finalizeRepetition() = {}

}
