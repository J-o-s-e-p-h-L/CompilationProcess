package funjs

/**
 * Translator from funjs source programs to SEC target programs.
 */
object Translator {

    import SECTree._
    import FunJSTree._
    import scala.collection.mutable.ListBuffer
    //import SemanticAnalysis.{entity, tipe}
    
    /**
     * Return a frame that represents the SEC instructions for a funjs program.
     */
    def translate (program : Program) : Frame = {

        // An instruction buffer for accumulating the program instructions
        val programInstrBuffer = new ListBuffer[Instr] ()

        /**
         * Translate the program by translating its expression.
         */
        val expInstrs = translateExpression (program.exp)
        programInstrBuffer.appendAll (expInstrs)
        programInstrBuffer.append (IPrint ())

        // Gather the program's instructions and return them
        programInstrBuffer.result ()

    }

    /**
     * Translate an expression and return a list of the instructions that
     * form the translation.
     */
    def translateExpression (exp : Exp) : Frame = {
        exp match {

            // Simple expressions. Each of these just corresponds to a single
            // instruction that uses a value from the source tree.

            case BoolExp (value) =>
                List (IBool (value))

            case IdnUse (i) =>
                List (IVar (i))

            case IntExp (value) =>
                List (IInt (value))

            // Application, relational expressions and arithmetic expressions. Generate
            // code to translate the two operand expressions and then generate the
            // appropriate instruction to perform the operation.

            case AppExp (l, r) =>
                (translateExpression (l) ++ translateExpression (r)) :+ (ICall ())

            case EqualExp (l, r) =>
                (translateExpression (l) ++ translateExpression (r)) :+ (IEqual ())

            case LessExp (l, r) =>
                (translateExpression (l) ++ translateExpression (r)) :+ (ILess ())

            case MinusExp (l, r) =>
                (translateExpression (l) ++ translateExpression (r)) :+ (ISub ())

            case PlusExp (l, r) =>
                (translateExpression (l) ++ translateExpression (r)) :+ (IAdd ())

            case SlashExp (l, r) =>
                (translateExpression (l) ++ translateExpression (r)) :+ (IDiv ())

            case StarExp (l, r) =>
                (translateExpression (l) ++ translateExpression (r)) :+ (IMul ())

            // An if expression translate into the code for the condition and a
            // branch instruction containing the code for the left and right
            // sides of the expression.

            case IfExp (c, l, r) =>
                (translateExpression (c)) :+ (IBranch (translateExpression (l), translateExpression (r)))

            case FunExp (IdnDef (name, _), body) => 
                List (IClosure (name, translateExpression (body) :+ IPopEnv ()))
                
            // FIXME
            case WhereExp(IdnUse(name), _) =>
               List (IClosure (name, translateExpression (IdnUse(name)) :+ IVar (name) :+  ICall () :+ IPopEnv ()))
            
            case ObjExp (List (Defn (IdnDef (name, _), exp))) =>
                List (IClosure (name, translateExpression (exp)))  
                
            case n@DeRefExp(IdnUse(v), _) =>
                List (IClosure (v, translateExpression (n) :+ IPopEnv ()))
        }
    }

}
