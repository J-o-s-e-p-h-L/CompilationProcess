package funjs

import org.kiama.util.PositionedParserUtilities

/**
 * Module containing parsers for funjs.
 */
class SyntaxAnalysis extends PositionedParserUtilities {

    import FunJSTree._
    import scala.collection.immutable.Seq
    import scala.language.postfixOps

    lazy val program : PackratParser[Program] =
        exp ^^ Program

    lazy val exp : PackratParser[Exp] =
        ("if" ~> "(" ~> exp <~ ")") ~ ("then" ~> exp) ~ ("else" ~> exp) ^^ {
            case c ~ t ~ e => IfExp (c, t, e)
        } |
        where |
        exp1

    lazy val exp1 : PackratParser[Exp] =
        exp2 ~ ("==" ~> exp2) ^^ { case e ~ t => EqualExp (e, t) } |
        exp2 ~ ("<" ~> exp2) ^^ { case e ~ t => LessExp (e, t) } |
        exp2

    lazy val exp2 : PackratParser[Exp] =
        exp2 ~ ("+" ~> exp3) ^^ { case e ~ t => PlusExp (e, t) } |
        exp2 ~ ("-" ~> exp3) ^^ { case e ~ t => MinusExp (e, t) } |
        exp3

    lazy val exp3 : PackratParser[Exp] =
        exp3 ~ ("*" ~> factor) ^^ { case e ~ t => StarExp (e, t) } |
        exp3 ~ ("/" ~> factor) ^^ { case e ~ t => SlashExp (e, t) } |
        factor

    lazy val factor : PackratParser[Exp] =
        app   |
        fun   |
        obj   |
        deref |
        "false" ^^ (_ => BoolExp (false)) |
        "true" ^^ (_ => BoolExp (true)) |
        identifier ^^ {case e => IdnUse (e)} |
        integer ^^ (s => IntExp (s.toInt)) |
        "(" ~> exp <~ ")" |
        failure ("exp expected")

    lazy val obj : PackratParser[Exp] = 
        "{" ~> definitions <~ "}" ^^ {case ds => ObjExp(ds)}

    lazy val deref : PackratParser[Exp] = 
        (identifier <~ ".") ~ identifier ^^ {case n ~ f => DeRefExp(IdnUse(n), f)}

    lazy val tipe : PackratParser[Type] =
        basictipe ~ ("->" ~> tipe) ^^ {
            case l ~ r => FunType (l, r)
        } |
        basictipe

    lazy val basictipe : PackratParser[Type] =
        "bool" ^^ (_ => BoolType ()) |
        "int" ^^ (_ => IntType ()) |
        "obj" ^^ (_ => ObjType ()) |
        "field" ^^ (_ => InsideObjType ()) |
        "(" ~> tipe <~ ")"
    
    lazy val app : PackratParser[AppExp] =
        exp ~ exp ^^ {
            case f ~ arg => AppExp (f, arg)
        }
    lazy val fun : PackratParser[FunExp] = {
        ("fun" ~> ("(" ~> idndef <~ ")")) ~ ("{" ~> exp <~ "}") ^^ {case idn ~ body => FunExp (idn, body)}
    }
    lazy val where : PackratParser[WhereExp] =
        exp1 ~ "where" ~ obj ^^ {
            case e ~ s ~ (ObjExp(ds)) => WhereExp (e, ds)
        }

    lazy val definitions : PackratParser[List[Defn]] =
        rep1sep (defn, ";")

    lazy val defn : PackratParser[Defn] =
        idndef ~ (":" ~> exp) ^^ {
            case i ~ e => Defn (i, e)
        }

    // NOTE: You should not need to change anything below here...

    lazy val integer =
        regex ("[0-9]+".r)

    lazy val idndef =
        tipe ~ identifier ^^ {case t ~ i => IdnDef (i,t)}

    val keywordStrings =
        Seq ("where", "else", "false", "if", "then", "true")

    lazy val keyword =
        keywords ("[^a-zA-Z0-9_]".r, keywordStrings)

    lazy val identifier =
        not (keyword) ~> identifierBase

    lazy val identifierBase =
        regex ("[a-zA-Z][a-zA-Z0-9_]*".r) |
        failure ("identifier expected")

    lazy val whitespaceParser : PackratParser[Any] =
        rep (whiteSpace | comment)

    lazy val comment : PackratParser[Any] =
        "/*" ~ rep (not ("*/") ~ (comment | any)) ~ "*/" |
        "//.*(\n|\\z)".r

}
