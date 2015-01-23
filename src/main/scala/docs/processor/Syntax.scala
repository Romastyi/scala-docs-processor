package docs.processor

/**
 * Created by romastyi on 23/01/15.
 */

case class Syntax( start: String, finish: String, separator: String, qualifier: String )

class DefaultSyntax extends Syntax( start = "{", finish = "}", separator = ".", qualifier = ":" )
