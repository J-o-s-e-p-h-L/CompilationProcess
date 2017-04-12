package funjs

import FunJSTree.Program
import org.kiama.util.ParsingREPL

/**
 * Conduct syntax analysis on the Moama program in the file given as the
 * first command-line argument. If the file is syntactically valid, go on
 * to perform semantic checks, and if they pass, translate the code into
 * SEC code and run it.
 */
object Main extends SyntaxAnalysis with ParsingREPL[Program] {

    import FunJSTree.Program
    import SemanticAnalysis.errors
    import SemanticAnalysis.env
    import Translator.translate
    import org.kiama.attribution.Attribution.initTree
    import org.kiama.output.PrettyPrinter.pretty_any
    import org.kiama.util.OutputEmitter
    import org.kiama.util.Messaging.report
    import org.kiama.util.REPLConfig
    import java.io.FileReader
    import java.io.FileNotFoundException

    override def main (args : Array[String]) {

        args.size match {

            // If there are no command-line arguments, we want to enter a
            // read-eval-print-loop (REPL) to read expressions and run them
            // one-by-one.
            case 0 =>
                super.main (args)

            // If there is exactly one command-line argument, we want to
            // compile and run that file.
            case 1 =>
                try {
                    // Create a reader for the argument file name
                    // val reader = new PackratReader (new FileReader (args (0)))
                    val reader = new FileReader (args (0))

                    // Parse the file
                    parseAll (program, reader) match {

                        // If it worked, we get a source tree
                        case Success (sourcetree, _) =>
                            // Pretty print the source tree
                            // println (pretty_any (sourcetree))

                            // Compile and run the program source tree
                            compileAndRun (sourcetree)

                        // Parsing failed, so report it
                        case f =>
                            println (f)
                    }
                } catch {
                    case e : FileNotFoundException =>
                        println (e.getMessage)
                }

            // Complain otherwise
            case _ =>
                println ("usage: run [file.hsl]")

        }

    }

    /**
     * The banner to print when the REPL is entered.
     */
    val banner = "funjs REPL. Type expressions..."

    /**
     * The prompt to use in REPL mode.
     */
    override val prompt = "funjs> "

    /**
     * The parser to use in REPL mode. We parse each line entered by the user
     * as an expression (not a program).
     */
    val parser = program

    /**
     * Process a parsed input line in REPL mode. Just compile and run it as
     * a program containing that expression.
     */
    override def process (program : Program, config : REPLConfig) {
        compileAndRun (program)
    }

    /**
     * Compile and run a program source tree.
     */
    def compileAndRun (program : Program) {

        initTree (program)
        val messages = errors (program)
        if (messages.length > 0)
            report (messages)
        else {

            // For Assignment Three, if there are no errors, perform the
            // translation into SEC instructions and then execute those
            // instructions on an SEC machine.

            // Translation
            val instrs = translate (program)
            println (instrs)

            // A machine to perform the run
            val machine = new SECMachine (new OutputEmitter)

            // Execution
            machine.run (instrs) match {
                case machine.FatalError (message) =>
                    println ("execution error: " + message)
                case _ =>
                    // All ok, do nothing
            }
        }

    }

}
