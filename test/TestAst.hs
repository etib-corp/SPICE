module TestAst where
import Test.HUnit
import Ast ( createAst )
import Structures

-- Tests pour la fonction createAst

testCreateAstInteger :: Test
testCreateAstInteger = TestCase $ do
  let expr = Integer 42
  let expectedAst = Node (Integer 42) []
  assertEqual "createAst should create AST for Integer" expectedAst (createAst expr)

testCreateAstFloat :: Test
testCreateAstFloat = TestCase $ do
  let expr = Float 3.14
  let expectedAst = Node (Float 3.14) []
  assertEqual "createAst should create AST for Float" expectedAst (createAst expr)

testCreateAstVar :: Test
testCreateAstVar = TestCase $ do
  let expr = Var "x"
  let expectedAst = Node (Var "x") []
  assertEqual "createAst should create AST for Var" expectedAst (createAst expr)

testCreateAstOperator :: Test
testCreateAstOperator = TestCase $ do
  let expr = Operator "+"
  let expectedAst = Node (Operator "+") []
  assertEqual "createAst should create AST for Operator" expectedAst (createAst expr)

testCreateAstArithmeticOp :: Test
testCreateAstArithmeticOp = TestCase $ do
  let expr = ArithmeticOp "+" (Integer 1) (Integer 2)
  let expectedAst = Node (Operator "+") [Node (Integer 1) [], Node (Integer 2) []]
  assertEqual "createAst should create AST for ArithmeticOp" expectedAst (createAst expr)

testCreateAstFunction :: Test
testCreateAstFunction = TestCase $ do
  let expr = Function "f" ["x", "y"] (Integer 42)
  let expectedAst = Node (Declarator "f") [Node (List [Var "x", Var "y"]) [], Node (Integer 42) []]
  assertEqual "createAst should create AST for Function" expectedAst (createAst expr)

-- testCreateAstIf :: Test
-- testCreateAstIf = TestCase $ do
--   let expr = If (Integer 1) "cond1" (Integer 2) (Integer 3)
--   let expectedAst = Node (Condition "if")
--                          [Node (Integer 1) [],
--                           Node (Condition "cond1") [],
--                           Node (Integer 2) [],
--                           Node (Integer 3) []]
--   assertEqual "createAst should create AST for If" expectedAst (createAst expr)

testCreateAstCallable :: Test
testCreateAstCallable = TestCase $ do
  let expr = Callable "myFunc" [Integer 1, Integer 2]
  let expectedAst = Node (Call "myFunc") [Node (List [Integer 1, Integer 2]) []]
  assertEqual "createAst should create AST for Callable" expectedAst (createAst expr)

testCreateAstList :: Test
testCreateAstList = TestCase $ do
  let expr = List [Integer 1, Float 2.5]
  let expectedAst = Node (List []) [Node (Integer 1) [], Node (Float 2.5) []]
  assertEqual "createAst should create AST for List" expectedAst (createAst expr)

-- Liste de tous les tests
testsAst :: Test
testsAst = TestList
  [ testCreateAstInteger
  , testCreateAstFloat
  , testCreateAstVar
  , testCreateAstOperator
  , testCreateAstArithmeticOp
  , testCreateAstFunction
--   , testCreateAstIf
  , testCreateAstCallable
  , testCreateAstList
  ]
-- testFloat :: Test
-- testFloat = TestCase (assertEqual "Pour Float 3.14" (Node (Float 3.14) []) (createAst (Float 3.14)))

-- testVar :: Test
-- testVar = TestCase (assertEqual "Pour Var 'x'" (Node (Var "x") []) (createAst (Var "x")))

-- testOperator :: Test
-- testOperator = TestCase (assertEqual "Pour Operator '+'" (Node (Operator "+") []) (createAst (Operator "+")))

-- testArithmeticOp :: Test
-- testArithmeticOp = TestCase (assertEqual "Pour ArithmeticOp '+' 3 5"
--                                 (Node (Operator "+") [Node (Integer 3) [], Node (Integer 5) []])
--                                 (createAst (ArithmeticOp "+" (Integer 3) (Integer 5))))

-- testFunction :: Test
-- testFunction = TestCase (assertEqual "Pour Function 'sum' ['x', 'y'] e"
--                                (Node (Declarator "sum") [Node (List [Var "x", Var "y"]) [], Node (Node (Integer 10) []) []])
--                                (createAst (Function "sum" ["x", "y"] (Integer 10))))

-- testIf :: Test
-- testIf = TestCase (assertEqual "Pour If"
--                      (Node (Condition "if") [Node (Integer 1) [], Node (Condition "then") [], Node (Integer 2) [], Node (Integer 3) []])
--                      (createAst (If (Integer 1) "then" (Integer 2) (Integer 3))))

-- testCallable :: Test
-- testCallable = TestCase (assertEqual "Pour Callable"
--                          (Node (Call "func") [Node (List [Integer 1, Integer 2]) []])
--                          (createAst (Callable "func" [Integer 1, Integer 2])))

-- testList :: Test
-- testList = TestCase (assertEqual "Pour List"
--                     (Node (List []) [Node (Integer 1) [], Node (Float 2.5) []])
--                     (createAst (List [Integer 1, Float 2.5])))

-- -- Regrouper tous les tests
-- allTests :: Test
-- allTests = TestList [testInteger, testFloat, testVar, testOperator, testArithmeticOp,
--                      testFunction, testIf, testCallable, testList]

-- -- Exécuter les tests
-- runTests :: IO Counts
-- runTests = runTestTT allTests