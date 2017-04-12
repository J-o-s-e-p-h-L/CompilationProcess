package funjs

/**
 * Tests that check that the type analyser detects the appropriate errors.
 */
class TypeTests extends SemanticTests {

    import FunJSTree._
    import org.kiama.attribution.Attribution.initTree
    import scala.collection.immutable.Seq

    /**
     * Parse some test input and check that its type is as expected.
     */
    def typeTest (str : String, expectedType : Type) {
        val ast = parseProgram (str)
        initTree (ast)
        val tipe = SemanticAnalysis.tipe (ast.exp)
        assertResult (expectedType) (tipe)
    }

    // Basic expression type tests

    test ("a number expression is of integer type") {
        typeTest ("42", IntType ())
    }

    test ("true is of Boolean type") {
        typeTest ("true", BoolType ())
    }

    test ("false is of Boolean type") {
        typeTest ("false", BoolType ())
    }

    test ("a name that is not defined is of unknown type") {
        typeTest ("x", UnknownType ("variable not defined x"))
    }

    test ("a block's type is the type of its final expression (val)") {
        typeTest ("x where {int x :1}", IntType ())
    }

    test ("a block's type is the type of its final expression") {
        typeTest ("f where {int -> int f : fun(int x){x}}", FunType (IntType (), IntType ()))
    }

    test ("a primitive addition is of integer type") {
        typeTest ("1 + 2", IntType ())
    }

    test ("a primitive divide is of integer type") {
        typeTest ("8 / 4", IntType ())
    }

    test ("a primitive multiplication is of integer type") {
        typeTest ("3 * 2", IntType ())
    }

    test ("a primitive subtraction is of integer type") {
        typeTest ("5 - 8", IntType ())
    }

    test ("a primitive less than is of Boolean type") {
        typeTest ("10 < 3", BoolType ())
    }

    test ("a primitive equality of integers is of Boolean type") {
        typeTest ("10 == 3", BoolType ())
    }

    test ("the type of a fun application is the fun result type") {
        typeTest ("(f 42) where {int -> bool f: fun(int x){true}}", BoolType ())
    }

    test ("a conditional expression's type is the type of its first branch - Int") {
        typeTest ("if (true) then 3 else 4", IntType ())
    }

    test ("a conditional expression's type is the type of its first branch - Bool") {
        typeTest ("if (true) then true else false", BoolType ())
    }

    test ("a conditional expression's type is the type of its first branch - Fun") {
        typeTest ("(if (true) then f else f) where {int -> bool f: fun(int x){true}}",
                FunType(IntType(), BoolType ()))
    }

    // Tests of type compatibility

    test ("the left argument of an addition can't be a Boolean") {
        val messages = semanticTest ("true + 1")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("the right argument of an addition can't be a Boolean") {
        val messages = semanticTest ("1 + false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("the left argument of a division can't be a Boolean") {
        val messages = semanticTest ("true / 1")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("the right argument of a division can't be a Boolean") {
        val messages = semanticTest ("1 / false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("the left argument of a multiplication can't be a Boolean") {
        val messages = semanticTest ("true * 1")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("the right argument of a multiplication can't be a Boolean") {
        val messages = semanticTest ("1 * false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("the left argument of a subtraction can't be a Boolean") {
        val messages = semanticTest ("true - 1")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("the right argument of a subtraction can't be a Boolean") {
        val messages = semanticTest ("1 - false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("the left argument of an equality than can't be a Boolean") {
        val messages = semanticTest ("true == 1")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("the right argument of a equality can't be a Boolean") {
        val messages = semanticTest ("1 == false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("cannot test equality of two Booleans") {
        val messages = semanticTest ("true == false")
        assert (messages.length >= 1)
    }

    test ("the left argument of a less than can't be a Boolean") {
        val messages = semanticTest ("true < 1")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("the right argument of a less than can't be a Boolean") {
        val messages = semanticTest ("1 < false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("the tested expression in a conditional can't be an integer") {
        val messages = semanticTest ("if (20) then 3 else 4")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected bool got int")
    }

    test ("the else expression in a conditional must have the same type as the then expression") {
        val messages = semanticTest ("if (true) then 3 else false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected int got bool")
    }

    test ("an applied expression must be a function") {
        val messages = semanticTest ("(x 10) where {int x : 1}")
        assertMessage (messages, 1, 1, 2, "application of non-function int")
    }

    test ("the arguments in a fun application must match the function") {
        val messages = semanticTest ("(f true) where { int-> int f : fun(int x){x}}")
        assertMessage (messages, 1, 1, 2, "expected int got bool")
    }

    test ("comparing funtype against int inside IF") {
        val messages = semanticTest ("(if (true) then f else 1) where {int -> int f : fun(int x){x}}")
        assertMessage (messages, 1, 1, 2, "expected int -> int got int")
    }

    test ("comparing two different funtypes inside IF") {
        val messages = semanticTest ("(if (true) then f else g) where {" +
                                     "     (bool -> int) -> (bool -> int) f : fun(bool->int x){x};" +
                                     "     (int -> bool) -> (int -> bool) g : fun(int->bool x){x} } ")
        assertMessage (messages, 1, 1, 2, "expected bool -> int -> bool -> int got int -> bool -> int -> bool")
    }

    test ("comparing two identical funtypes inside IF") {
        typeTest ("(if (true) then f else g) where {" +
                  "     (int -> bool) -> (int -> bool) f : fun(int->bool x){x};" +
                  "     (int -> bool) -> (int -> bool) g : fun(int->bool x){x} } ",
                  FunType (FunType (IntType (), BoolType ()), FunType (IntType (), BoolType ())))
    }

    test ("declaring the same thing twice is an error") {
        val messages = semanticTest ("f where {int f :3; bool f : true}")
        assert (messages.length === 2)
    }

    test ("backwards references do now work") {
        typeTest ("(y + 5) where {int f : 5; int y : f + 5}",
                 IntType ())
    }

    test ("even with the recursion restrictions you can nest lets to get what you want") {
        typeTest ("(y + 5) where {int y: ((f + 5) where {int f : 5})}", IntType ())
    }

    test ("pulling a variable from outside and using at the right type"){
        typeTest("fun(int x){y where {int y : x}}", FunType (IntType (), IntType()))
        typeTest("fun(int x){fun(bool x){y where {bool y : x}}}", FunType (IntType (), FunType (BoolType (), BoolType())))
        typeTest("fun(int x){fun(bool x2){y where {int y :x}}}", FunType (IntType (), FunType (BoolType (), IntType())))
    }

    // objects
    test ("basic object"){
        typeTest("{int x: 4; int y: 5}", ObjType ())
    }
    test ("basic object dereference"){
        typeTest("x.a where {obj x: {int a: 4; int y: 5}}", InsideObjType ())
    }

    test ("object dereference that was failing in exec tests"){
        typeTest("v.x where {obj v: {int x: 5}}", InsideObjType ())
    }

    test("another basic object"){
        typeTest ("{int x : 5}", ObjType ())
    }

    // not for realease
    test ("a bug I am pulling apart from exec tests"){
        typeTest ("((f x) where {int -> int f : fun(int a){a+1}; int x : 1})", IntType())
    }

    test("comparing two different funypes"){
        typeTest("g where {int -> bool f : fun(int x){true}; int -> bool g: f}", FunType (IntType(), BoolType()))
    }
    test("comparing two different funypes (longer)"){
        typeTest("g where {int -> (bool -> int) f : fun(int x){fun(bool y){x}}; int -> bool -> int g: f}", FunType (IntType(), FunType (BoolType(), IntType())))
    }

    test ("types of parts must be correct") {
        var messages = semanticTest ("x")
        messages = semanticTest ("true + 5"); println (messages); 
          assertMessage (messages, 0, 1, 1, "expected int got bool")
        messages = semanticTest ("5 + false"); println (messages); 
          assert (messages.length == 1)
          assertMessage (messages, 0, 1, 1, "expected int got bool")
    }
}
