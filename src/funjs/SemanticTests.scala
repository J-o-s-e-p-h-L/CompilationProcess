package funjs

import org.kiama.util.Tests

/**
 * General support for
 */
class SemanticTests extends SyntaxAnalysis with Tests {

    import FunJSTree._
    import org.kiama.attribution.Attribution.initTree
    import org.kiama.util.Messaging.Messages

    /**
     * Parse some test input and, if the parse succeeds with no input left,
     * return the program tree. If the parse fails, fail the test.
     */
    def parseProgram (str : String) : Program =
        parseAll (program, str) match {
            case Success (r, in) =>
                if (!in.atEnd) fail ("input remaining at " + in.pos)
                r
            case f : Error =>
                fail ("parse error: " + f)
            case f : Failure =>
                fail ("parse failure: " + f)
        }

    /**
     * Parse some test input and run the semantic analyser over the resulting
     * tree (if the parse succeeds).
     */
    def semanticTest (str : String) : Messages = {
        val ast = parseProgram (str)
        initTree (ast)
        val messages = SemanticAnalysis.errors (ast)
        messages
    }

    /**
     * Assert that a message was produced at a given position.
     */
    def assertMessage (messages : Messages, index : Int, line : Int, column : Int, msg : String) {
        val m = messages (index)
        assertResult (msg, "wrong text in message " + index) (m.label)
        assertResult (line, "wrong line number in message " + index) (m.line)
        assertResult (column, "wrong column number in message " + index) (m.column)
    }

}

