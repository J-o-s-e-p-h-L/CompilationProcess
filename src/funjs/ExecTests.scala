package funjs

/**
 * Tests that check that the translation works correctly.
 */
class ExecTests extends SemanticTests {

    import org.kiama.attribution.Attribution.initTree
    import org.kiama.util.StringEmitter
    import SECTree._

    /**
     * Parse some test input, perform semantic analysis checks, expect no
     * semantic errors. Then translate into SEC machine code and run the code.
     * The value `expected` should be the output that we expect from this
     * run.
     */
    def execTest (str : String, expected : String) {
        val tree = parseProgram (str)
        initTree (tree)
        val messages = SemanticAnalysis.errors (tree)
        assert (messages.length === 0, messages.toString)

        val instrs = Translator.translate (tree)
        // println (instrs)

        val emitter = new StringEmitter ()
        val machine = new SECMachine (emitter)

        machine.run (instrs) match {
            case _ : machine.State =>
                // Terminated correctly in some state
                assertResult (expected + "\n", "wrong execution output") (emitter.result ())
            case machine.FatalError (message) =>
                fail (message)
        }
    }
    
    // Pass-level tests
    test ("an integer expression evaluates to the correct result") {
        execTest ("""
            |1
            """.stripMargin,
            "1")
    }

    test ("an addition expression evaluates to the correct result") {
        execTest ("""
            |3 + 4
            """.stripMargin,
            "7")
    }

    // Added tests below here...

    test ("a true expression evaluates to the correct result") {
        execTest ("""
            |true
            """.stripMargin,
            "true")
    }

    test ("a false expression evaluates to the correct result") {
        execTest ("""
            |false
            """.stripMargin,
            "false")
    }

    test ("a subtraction expression evaluates to the correct result") {
        execTest ("""
            |3 - 4
            """.stripMargin,
            "-1")
    }

    test ("a multiplication expression evaluates to the correct result") {
        execTest ("""
            |5 * 3
            """.stripMargin,
            "15")
    }

    test ("a division expression evaluates to the correct result") {
        execTest ("""
            |12 / 5
            """.stripMargin,
            "2")
    }

    test ("an equality expression evaluates to the correct result (true)") {
        execTest ("""
            |12 == 5
            """.stripMargin,
            "false")
    }

    test ("an equality expression evaluates to the correct result (false)") {
        execTest ("""
            |9 == 9
            """.stripMargin,
            "true")
    }

    test ("a less-than expression evaluates to the correct result (true)") {
        execTest ("""
            |8 < 9
            """.stripMargin,
            "true")
    }

    test ("a less-than expression evaluates to the correct result (false)") {
        execTest ("""
            |9 < 8
            """.stripMargin,
            "false")
    }

    test ("a multi-operator expression evaluates to the correct result") {
        execTest ("""
            |3 * (12 / 4)
            """.stripMargin,
            "9")
    }

    // credit level tests

    test ("a true less-than conditional expression evaluates to the correct result") {
        execTest ("""
            |if (1 < 2) then 15 else 0
            """.stripMargin,
            "15")
    }

    test ("a false less-than conditional expression evaluates to the correct result") {
        execTest ("""
            |if (4 < 2) then 15 else 0
            """.stripMargin,
            "0")
    }

    test ("a false equal conditional expression evaluates to the correct result") {
        execTest ("""
            |if (1 == 2) then 0 else 12
            """.stripMargin,
            "12")
    }

    test ("a true equal conditional expression evaluates to the correct result") {
        execTest ("""
            |if (1 == 1) then 0 else 12
            """.stripMargin,
            "0")
    }

    test ("a true literal conditional expression evaluates to the correct result") {
        execTest ("""
            |if (true) then 8 else 12
            """.stripMargin,
            "8")
    }

    // distinction level tests

    test ("a single def let evaluates to the correct result") {
        execTest ("""
            |x where {
            |   int x : 1
            |}
            |""".stripMargin,
            "1")
    }

    test ("a multiple def let evaluates to the correct result (use first def)") {
        execTest ("""
            |x where {
            |   int x : 1;
            |   int y : 2
            |}
            """.stripMargin,
            "1")
    }

    test ("a multiple def let evaluates to the correct result (use second def)") {
        execTest ("""
            |y where {
            |  int x : 1;
            |  int y : 2
            |}
            """.stripMargin,
            "2")
    }

    test ("a multiple def let evaluates to the correct result (use both defs)") {
        execTest ("""
            |x + y where {
            |  int x : 1;
            |  int y : 2
            |}
            """.stripMargin,
            "3")
    }

    test ("a let with a calculation evaluates to the correct result") {
        execTest ("""
            |x + 4 where {
            |  int x : 5
            |}
            |""".stripMargin,
            "9")
    }

    // def with function definitions

    test ("a where with a single function definition evaluates to the correct result") {
        execTest ("""
            |(inc 1) where {
            |  int -> int inc : fun(int a){a + 1}
            |}
            """.stripMargin,
            "2")
    }

    test ("a single function where evaluates to the correct result") {
        execTest ("""
            |f where {
            |  int -> int f : fun(int x){x}
            |}
            """.stripMargin,
            "function of x")
    }

    test ("a multiple function where evaluates to the correct result (use first fun)") {
        execTest ("""
            |(f 4) where {
            |  int -> int f : fun(int x){x + 1};
            |  int -> int g : fun(int y){y * 2}
            |}
            """.stripMargin,
            "5")
    }

    test ("a multiple function where evaluates to the correct result (use second fun)") {
        execTest ("""
            |(g 4) where {
            |  int -> int f : fun(int x){x + 1};
            |  int -> int g : fun(int y){y * 2}
            |}
            """.stripMargin,
            "8")
    }

    test ("a multiple function where evaluates to the correct result (use both funs)") {
        execTest ("""
            |((f 4) + (g 4)) where {
            |  int -> int f : fun(int x){x + 1};
            |  int -> int g : fun(int y){y * 2}
            |}
            """.stripMargin,
            "13")
    }

    test ("backward reference is evaluated correctly (same group)") {
        execTest ("""
            |(h 3) where {
            |  int -> int h : fun(int y){g y} where {
            |    int -> int g : fun(int x){x * 2}   
            |  }    
            |}
            """.stripMargin,
            "6")
    }

    test ("call with call argument is evaluated correctly") {
        execTest ("""
            |(inc (dec 4)) where {
            |  int -> int inc : fun(int x){x + 1};
            |  int -> int dec : fun(int x){x - 1}
            |}
            """.stripMargin,
            "4")
    }

    // Blocks with value and function definitions

    test ("a function using a val is evaluated correctly (1)") {
        execTest ("""
            |(f 4) where {
            |  int -> int f : fun(int y){x where {int x : 1}}
            |}
            """.stripMargin,
            "1")
    }

    test ("a function using a val is evaluated correctly (2)") {
        execTest ("""
            |(f 4) where {int -> int f: fun(int y){x + y} where {int x: 7}}
            """.stripMargin,
            "11")
    }

    test ("a function using def def val is evaluated correctly") {
        execTest ("""
            |(g 4) where {
            |  int -> int f : fun(int x){x + 1};
            |  int -> int g : fun(int x){x + 2};
            |  int z : 3
            |}
            """.stripMargin,
            "6")
    }

    test ("backward reference is evaluated correctly") {
        execTest ("""
            |(h 4) where {
            |  int -> int h : fun(int y){g (x + y)} where {
            |    int -> int g : fun(int x){x * 2};
            |    int x : 1
            |  }
            |}
            """.stripMargin,
            "10")
    }

    // Nested blocks

    test ("an inner binding is evaluated correctly (val)") {
        execTest ("""
            |y where {
            |  int x : 1;
            |  int y : z where {int z:2}
            |}
            """.stripMargin,
            "2")
    }

    test ("an inner binding is evaluated correctly (fun)") {
        execTest ("""
            |y where {
            |  int y : ((f x) where {int -> int f : fun(int a){a+1}; int x : 1})
            |}
            """.stripMargin,
            "2")
    }

    test ("an outer binding is evaluated correctly (outer accces, val)") {
        execTest ("""
            |y where {
            |  int y : (x where {
            |    int z : 2;
            |    int x : 1
            |  })
            |}
            |
            """.stripMargin,
            "1")
    }

    test ("an outer binding is evaluated correctly (outer accces, fun)") {
        execTest ("""
            |y where {
            |   int y : ((f z) where {int z : 2;
            |                         int -> int f : fun(int x){x + 1}
            |                         })
            |}
            """.stripMargin,
            "3")
    }

    test ("an overridden outer binding is evaluated correctly (val-val)") {
        execTest ("""
            |y where {int y : ((x where {int x:2}) where {int x: 1})}
            """.stripMargin,
            "2")
    }

    test ("an overridden outer binding is evaluated correctly (fun-val)") {
        execTest ("""
            |y where {
            |  int -> int f : fun(int x){x + 1};
            |  int y : (f where {int f : 3})
            |}
            """.stripMargin,
            "3")
    }

    test ("an overridden outer binding is evaluated correctly (val-fun)") {
        execTest ("""
            |y where {
            |  int f : 3;
            |  int y : ((f 2) where {int -> int f : fun(int x){x * 3}})
            |}
            """.stripMargin,
            "6")
    }

    test ("an overridden outer binding is evaluated correctly (fun-fun)") {
        execTest ("""
            |(y where {int y : ((f 2) where {int -> int f : fun(int x){x * 1}})}) where {
            |   int -> int f : fun(int x){x + 1}
            |}
            """.stripMargin,
            "2")
    }

    test ("a basic object"){
        execTest ("{int x : 5}", "an object")
    }

    test ("a more complex object"){
        execTest ("{int x: 5; int -> int f: fun(int y){y}}", "an object")
    }

    test ("object dereferencing"){
        execTest ("v.x where {obj v: {int x: 5}}", "5")
    }

    test ("dereferencing in a more complex setting - this one won't work under matt's type rules for objects"){
        execTest ("(f v) where {obj v: {int y: 5}; obj -> field f : fun (obj x){x.y}}", "5")
    }

    // Translation tests that check the byte code that is produced.
    // Used to narrow down faults during marking...

    def translateTest (str: String, expected : Frame) {
        val tree = parseProgram(str)
        val instrs = Translator.translate(tree)

        assertResult (expected, "wrong translation output") (instrs)
    }

    test ("translate 3 < 4")
    {
        translateTest("3 < 4", List(IInt(3), IInt(4), ILess(), IPrint()))
    }

    test ("translate x")
    {
        translateTest("x", List(IVar("x"), IPrint()))
    }

    test ("translate if(true) then 3 else 4")
    {
        translateTest("if(true) then 3 else 4",
            List(IBool(true), IBranch(List(IInt(3)),List(IInt(4))), IPrint()))
    }

    test ("translate (f 3)") {
        translateTest("f 3", List(IVar("f"), IInt(3), ICall(), IPrint()))
    }

    test ("translate (x + 4) where {int x: 3}")
    {
        translateTest("(x + 4) where {int x: 3}",
            List(IClosure("x",List(IVar("x"), IInt(4), IAdd(), IPopEnv())),
                 IInt(3), ICall(), IPrint()))
    }

    test ("translate (f 4) where {int -> int f : fun(int x){2 * x}}")
    {
        translateTest("(f 4) where {int -> int f : fun(int x){2 * x}}",
            List(IClosure("f",List(IVar("f"), IInt(4), ICall(), IPopEnv())),
                 IClosure("x",List(IInt(2), IVar("x"), IMul(), IPopEnv())),
                 ICall(), IPrint()))
    }

    test ("translate ((f 4) + (g 4)) where {int -> int f: fun(int x){x+1}; int -> int g : fun(int y){y * 2}}")
    {
        translateTest("((f 4) + (g 4)) where {int -> int f: fun(int x){x+1}; int -> int g : fun(int y){y * 2}}",
            List(IClosure("f",List(IClosure("g",List(IVar("f"), IInt(4),
                ICall(), IVar("g"), IInt(4), ICall(), IAdd(), IPopEnv())),
                IClosure("y",List(IVar("y"), IInt(2), IMul(), IPopEnv())),
                ICall(), IPopEnv())),
                IClosure("x",List(IVar("x"), IInt(1), IAdd(), IPopEnv())),
                ICall(), IPrint()))
    }

    test ("translate ((f z) + (g 4)) where {int w : 7; int -> int f : fun(int x){x+1}; int -> int g : fun(int y){y *2}; int z : f w}")
    {
        translateTest(
            "((f z) + (g 4)) where {int w : 7; int -> int f : fun(int x){x+1}; int -> int g : fun(int y){y *2}; int z : f w}",
            List(IClosure("w",List(IClosure("f",List(IClosure("g",
                List(IClosure("z", List(IVar("f"), IVar("z"), ICall(),
                IVar("g"), IInt(4), ICall(), IAdd(), IPopEnv())), IVar("f"),
                IVar("w"), ICall(), ICall(), IPopEnv())),
                IClosure("y",List(IVar("y"), IInt(2), IMul(),
                IPopEnv())), ICall(), IPopEnv())), IClosure("x",List(IVar("x"),
                IInt(1), IAdd(), IPopEnv())), ICall(), IPopEnv())), IInt(7),
                ICall(), IPrint()))
    }

}

