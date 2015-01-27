package docs.processor

import scala.annotation.tailrec

/**
 * Created by romastyi on 27.01.15.
 */

object StringExtension {

    implicit class ImplicitStringExtension( source: String ) {

        // replace all, case insensitive
        def replaceAllSubstring( target: String, replacement: String, matchCase: Boolean = false): String = {

            // initialize the builder to sufficient size to reduce chance of needing to grow
            val out = new StringBuilder(source.size * 2)
            // last index we need to check for match
            val lastIdx = source.length - target.length
            // simple optimization
            val targetLower = target.toLowerCase

            // check for match at given index, at char offset along target
            @tailrec
            def matches(idx: Int, offset: Int): Boolean =
                if (offset >= target.length)
                    true
                else if ((matchCase && target.charAt(offset) == source.charAt(idx + offset)) ||
                    (!matchCase && targetLower.charAt(offset) == source.charAt(idx + offset).toLower))
                    matches(idx, offset + 1)
                else false

            // search source and append to builder
            @tailrec
            def search(idx: Int): Unit =
                if (idx > lastIdx)
                    out.append(source.substring(idx))
                else if (matches(idx, 0)) {
                    out.append(replacement)
                    search(idx + target.length)
                }
                else {
                    out.append(source.charAt(idx))
                    search(idx + 1)
                }

            search(0)
            out.toString()
        }
    }
}
