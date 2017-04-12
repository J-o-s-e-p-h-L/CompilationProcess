package funjs

import org.kiama.attribution.Attribution

object SemanticAnalysis {

    import FunJSTree._
    import org.kiama.==>
    import org.kiama.attribution.Attribution.attr
    import org.kiama.attribution.Decorators.{chain, Chain}
    import org.kiama.rewriting.Rewriter.{collect, collectall}
    import org.kiama.util.{Entity, MultipleEntity, UnknownEntity}
    import org.kiama.util.Messaging.{check, message}

    /**
     * Useful method to pretty-print types for error messages.
     */
    def prettyType (tipe : Type) : String =
        tipe match {
            case BoolType ()           => "bool"
            case IntType ()            => "int"
            case FunType (from, to)    => s"${prettyType (from)} -> ${prettyType (to)}"
            case UnknownType (s)       => "type error: "+ s
            case RedeclaredType (i)    => s"!!${i}!!"
            case ObjType ()            => "obj"
            case InsideObjType ()      => "field"
            case TypeClash (i, t1, t2) => s"type error, ${prettyType (t1)} is different to ${prettyType (t2)}"
        }

    /**
     * Collect the semantic error messages for a given tree.
     */
    val errors =
        attr (collectall {
            case e : Exp =>
                (tipe (e) match {
                    case RedeclaredType (i) => message (e, s"declared more than once")
                    case UnknownType (s) => message (e, s)
                    case _ => Nil
                    })
        })

    
    /**
     * The environment containing all bindings visible "before" a
     * particular node in the tree.  I.e., it's the environment in which that
     * node is evaluated.
     */
    val env : FunJSNode => Map[Identifier, Type] =
        attr {
            case p : Program             => Map[Identifier, Type]()
            case n : Defn                => env (n.parent[FunJSNode])
            case n                       => env (n.parent[FunJSNode]) ++ funbind(n.parent[FunJSNode]) ++ defedin (n)
        }
        
    val funbind : FunJSNode => Map[Identifier, Type] = 
        attr {
            case FunExp (IdnDef (i, t), e)   => Map( (i -> t))
            case _ => Map ()
        }

    val defedin : FunJSNode => Map[Identifier, Type] = 
        attr {
            case WhereExp (exp, defns) => {val defs = defns.collect {case Defn (IdnDef(i,t), e) => (i -> t)}
                                           var map = Map[Identifier, Type]()
                                           defs.foreach {case (i, t) => 
                                              map.get(i) match {
                                                  case Some (t) => map = map + (i -> RedeclaredType(i))
                                                  case None     => map = map + (i -> t)
                                              }
                                           }
                                           map
                                          } 
            case _                     => Map[Identifier, Type]()
        }

    /**
     * What is the type of an expression?
     */
    val tipe : Exp => Type =
        attr {

            case AppExp (fn, e) if ! (e eq fn) =>
                tipe (fn) match {
                     case FunType (from, to) => if (tipe (e) == from) {to} 
                                                else                  {UnknownType (s"expected ${prettyType (from)} got ${prettyType (tipe (e))}")}
                     case t                  => UnknownType (s"application of non-function ${prettyType (t)}")
                }
            
            case FunExp (IdnDef (i,t), body) => 
                FunType (t, tipe (body))
            
            case WhereExp (e, defns) => {
                // the defns are not exps so errors in them don't propagate, we need to do that here
                val decl_types = defns.map { case Defn (IdnDef (i, t), e) => if (tipe (e) == t) 
                                                                                {t} else
                                                                                {UnknownType (s"expected ${prettyType (t)} got ${prettyType (tipe (e))}")}
                                           }
                // if there is a problem, we will simply announce one is wrong - you can surely do better?
                if (decl_types.filter{case u: UnknownType => true; case _ => false}.length > 0)
                   {UnknownType (s"one of the defintion has a type different to that it was declared as $decl_types")} else
                   {tipe (e)}
            }

            case BoolExp (_) =>
                BoolType ()

            case EqualExp (a, b) => (tipe (a), tipe (b)) match {
                case (IntType (), IntType ()) => BoolType ()
                case (IntType (), other     ) => UnknownType (s"expected int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected int got ${prettyType (other)}")
            }

            case n @ IdnUse (i) =>
                env (n).get (i) match {
                    case None     => UnknownType (s"variable not defined $i")
                    case Some (t) => t
                }

            case IfExp (c, e1, e2) => (tipe (c), tipe (e1), tipe (e2)) match {
                case (BoolType (), t1, t2) if t1 == t2 => t1
                case (BoolType (), t1 ,t2) => UnknownType (s"expected ${prettyType (t1)} got ${prettyType (t2)}")
                case (other      , _  ,_ ) => UnknownType (s"expected bool got ${prettyType (other)}")
            }

            case IntExp (_) =>
                IntType ()

            case LessExp (a, b) => (tipe (a), tipe (b)) match {
                case (IntType (), IntType ()) => BoolType ()
                case (IntType (), other     ) => UnknownType (s"expected int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected int got ${prettyType (other)}")
            }

            case MinusExp (a, b) => (tipe (a), tipe (b)) match {
                case (IntType (), IntType ()) => IntType ()
                case (IntType (), other     ) => UnknownType (s"expected int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected int got ${prettyType (other)}")
            }

            case PlusExp (a, b) => (tipe (a), tipe (b)) match {
                case (IntType (), IntType ()) => IntType ()
                case (IntType (), other     ) => UnknownType (s"expected int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected int got ${prettyType (other)}")
            }

            case SlashExp (a, b) => (tipe (a), tipe (b)) match {
                case (IntType (), IntType ()) => IntType ()
                case (IntType (), other     ) => UnknownType (s"expected int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected int got ${prettyType (other)}")
            }

            case StarExp (a, b) => (tipe (a), tipe (b)) match {
                case (IntType (), IntType ()) => IntType ()
                case (IntType (), other     ) => UnknownType (s"expected int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected int got ${prettyType (other)}")
            }

            case ObjExp (defns) => ObjType ()
            
            case n@DeRefExp (IdnUse(name), idn) => InsideObjType ()

        }


}
