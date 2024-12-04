module TestStructures where
import Test.HUnit
import Structures
import Data.Ratio ((%))

-- Tests pour l'instance Num d'Expr
testAdditionInteger :: Test
testAdditionInteger = Integer 3 + Integer 5 ~?= Integer 8

testAdditionFloat :: Test
testAdditionFloat = Float 3.5 + Float 2.5 ~?= Float 6.0

testAdditionMixed :: Test
testAdditionMixed = Integer 3 + Float 2.5 ~?= Float 5.5

testAdditionMixed2 :: Test
testAdditionMixed2 = Float 2.5 + Integer 3 ~?= Float 5.5

testSubInteger :: Test
testSubInteger = Integer 5 - Integer 3 ~?= Integer 2

testSubFloat :: Test
testSubFloat = Float 3.5 - Float 2.5 ~?= Float 1.5

testSubMixed :: Test
testSubMixed = Integer 3 - Float 2.5 ~?= Float 0.5

testSubMixed2 :: Test
testSubMixed2 = Float 5.5 - Integer 3 ~?= Float 2.5

testMulInteger :: Test
testMulInteger = (Integer 3) * (Integer 4) ~?= Integer 12

testMulFloat :: Test
testMulFloat = (Float 3.5) * (Float 2.0) ~?= Float 7.0

testMulMixed :: Test
testMulMixed = (Integer 3) * (Float 2.5) ~?= Float 7.5

testMulMixed2 :: Test
testMulMixed2 = (Float 2.5) * (Integer 3) ~?= Float 7.5

-- Tests pour l'instance Num d'Expr (abs et signum)
testAbsInteger :: Test
testAbsInteger = abs (Integer (-5)) ~?= Integer 5

testAbsFloat :: Test
testAbsFloat = abs (Float (-3.14)) ~?= Float 3.14

testSignumInteger :: Test
testSignumInteger = signum (Integer (-5)) ~?= Integer (-1)

testSignumFloat :: Test
testSignumFloat = signum (Float (-3.14)) ~?= Float (-1)

-- Tests pour l'instance Fractional d'Expr
testDivisionInteger :: Test
testDivisionInteger = Integer 6 / Integer 3 ~?= Float 2.0

testDivisionFloat :: Test
testDivisionFloat = Float 7.5 / Float 2.5 ~?= Float 3.0

testDivisionMixed :: Test
testDivisionMixed = Integer 5 / Float 2.0 ~?= Float 2.5

testDivisionMixed2 :: Test
testDivisionMixed2 = (Float 5.5) / (Integer 3) ~?= Float 1.8333333333333333


testFromInteger :: Test
testFromInteger = fromInteger 42 ~?= Integer 42

-- Tests pour l'instance Enum d'Expr
testToEnum :: Test
testToEnum = toEnum 3 ~?= Integer 3

testFromEnum :: Test
testFromEnum = fromEnum (Float 3.5) ~?= 3

testFromEnumInteger :: Test
testFromEnumInteger = fromEnum (Integer 3) ~?= 3

testFromEnumFloat :: Test
testFromEnumFloat = fromEnum (Float 3.5) ~?= 3


-- Tests pour l'instance Real d'Expr
testToRational :: Test
testToRational = toRational (Float 5.5) ~?= 11 % 2

-- Tests pour l'instance Integral d'Expr
testToInteger1 :: Test
testToInteger1 = toInteger (Float 5.5) ~?= 5

testToInteger2 :: Test
testToInteger2 = toInteger (Integer 5) ~?= 5

-- Test pour quotRem (division entière et reste)
testQuotRemInteger :: Test
testQuotRemInteger = quotRem (Integer 10) (Integer 3) ~?= (Integer 3, Integer 1)

-- Test pour mod (modulo entre Integer)
testModInteger :: Test
testModInteger = (Integer 10) `mod` (Integer 3) ~?= Integer 1

-- Test pour mod (modulo entre Integer et Float)
testModMixed1 :: Test
testModMixed1 = (Integer 10) `mod` (Float 3.5) ~?= Float 1.0

-- Test pour mod (modulo entre Float et Integer)
testModMixed2 :: Test
testModMixed2 = (Float 10.5) `mod` (Integer 3) ~?= Float 1.5

-- Test pour mod (modulo entre Float)
testModFloat :: Test
testModFloat = (Float 10.5) `mod` (Float 3.5) ~?= Float 0.0

-- Test pour la conversion d'Expr en rationnel
testToRationalInteger :: Test
testToRationalInteger = toRational (Integer 5) ~?= 5 % 1

testToRationalFloat :: Test
testToRationalFloat = toRational (Float 3.5) ~?= 7 % 2

testMod :: Test
testMod = Integer 10 `mod` Integer 3 ~?= Integer 1

-- Tests pour l'instance Show d'Expr
testShowInteger :: Test
testShowInteger = show (Integer 42) ~?= "42"

testShowFloat :: Test
testShowFloat = show (Float 3.14) ~?= "3.14"

-- Test pour Var
testShowVar :: Test
testShowVar = show (Var "x") ~?= "x"


-- Test pour Operator
testShowOperator :: Test
testShowOperator = show (Operator "+") ~?= "+"

-- Test pour List
testShowList :: Test
testShowList = show (List [Integer 1, Integer 2, Integer 3]) ~?= "[1, 2, 3]"

-- Test pour ArithmeticOp
testShowArithmeticOp :: Test
testShowArithmeticOp = show (ArithmeticOp "+" (Integer 1) (Integer 2)) ~?= "1 + 2"

-- Test pour Function
testShowFunction :: Test
testShowFunction = show (Function "f" ["x", "y"] (Integer 3)) ~?= "f(x y) = 3"

-- Test pour If
-- testShowIf :: Test
-- testShowIf = show (If (Integer 1) (Integer 2) (Integer 3)) ~?= "if 1 then 2 else 3 end"

-- Test pour Callable
testShowCallable :: Test
testShowCallable = show (Callable "myFunc" [Integer 1, Float 2.5]) ~?= "myFunc(1 2.5)"

-- Test pour Declarator
testShowDeclarator :: Test
testShowDeclarator = show (Declarator "x") ~?= "x"

-- Tests pour l'instance Semigroup d'Env
testEnvConcat :: Test
testEnvConcat = (Env [(Integer 1, Integer 2)]) <> (Env [(Integer 1, Integer 3)]) ~?= Env [(Integer 1, Integer 3), (Integer 1, Integer 2)]

-- Tests pour l'instance Monoid d'Env
testEnvEmpty :: Test
testEnvEmpty = mempty ~?= Env []

-- Tests pour la fonction `emptyEnv`
testEmptyEnv :: Test
testEmptyEnv = emptyEnv ~?= Env []


-- Test pour la conversion d'un rationnel en Expr
testFromRational :: Test
testFromRational = fromRational (3 % 2) ~?= Float 1.5


-- Test pour Empty == Empty
testEmptyEquality :: Test
testEmptyEquality = (Empty :: AST Int) == (Empty :: AST Int) ~?= True

-- Test pour Node == Node avec la même valeur et les mêmes sous-arbres
testNodeEquality :: Test
testNodeEquality = (Node 1 [Empty, Empty]) == (Node 1 [Empty, Empty]) ~?= True

-- Test pour Node == Node avec une valeur différente
testNodeValueInequality :: Test
testNodeValueInequality = (Node 1 [Empty]) == (Node 2 [Empty]) ~?= False

-- Test pour Node == Node avec des sous-arbres différents
testNodeSubtreeInequality :: Test
testNodeSubtreeInequality = (Node 1 [Empty]) == (Node 1 [Node 2 [Empty]]) ~?= False

-- Test pour Node == Empty
testNodeEmptyInequality :: Test
testNodeEmptyInequality = (Node 1 [Empty]) == Empty ~?= False


-- Test pour Empty
testShowEmpty :: Test
testShowEmpty = show (Empty :: AST Int) ~?= ""

-- Test pour Node avec une seule valeur et une liste vide
testShowNodeEmptyList :: Test
testShowNodeEmptyList = show (Node 42 [] :: AST Int) ~?= "42"

-- Test pour Node avec une valeur et une liste non vide
testShowNodeWithList :: Test
testShowNodeWithList = show (Node 42 [Node 10 [], Node 20 []] :: AST Int) ~?= "42 -> \n10 -> \n20 -> \n"

-- Test pour Node avec une valeur et une liste non vide de sous-arbres
testShowNodeWithSubtree :: Test
testShowNodeWithSubtree = show (Node "root" [Node "child1" [], Node "child2" []] :: AST String) ~?= "root -> \nchild1 -> \nchild2 -> \n"


-- Test pour fmap sur Empty
testFmapEmpty :: Test
testFmapEmpty = fmap (+1) Empty ~?= Empty

-- Test pour fmap sur un Node avec une seule valeur
testFmapSingleNode :: Test
testFmapSingleNode = fmap (+1) (Node 42 []) ~?= Node 43 []

-- Test pour fmap sur un Node avec des sous-arbres
testFmapNodeWithSubtrees :: Test
testFmapNodeWithSubtrees = fmap (+1) (Node 42 [Node 10 [], Node 20 []]) 
  ~?= Node 43 [Node 11 [], Node 21 []]

-- Test pour fmap avec une fonction plus complexe
testFmapComplex :: Test
testFmapComplex = fmap (++ " modified") (Node "root" [Node "child1" [], Node "child2" []])
  ~?= Node "root modified" [Node "child1 modified" [], Node "child2 modified" []]


-- Test pour mempty
testMempty :: Test
testMempty = mempty ~?= Env []

-- Test pour mappend (combinaison de deux Env)
testMappend :: Test
testMappend = (Env [(Integer 1, Integer 2), (Integer 3, Integer 4)] `mappend` Env [(Integer 5, Integer 6)]) 
  ~?= Env [(Integer 1, Integer 2), (Integer 3, Integer 4), (Integer 5, Integer 6)]

-- Test pour l'associativité de mappend
testAssociativity :: Test
testAssociativity = 
  (Env [(Integer 1, Integer 2), (Integer 3, Integer 4)] `mappend` (Env [(Integer 5, Integer 6)] `mappend` Env [(Integer 7, Integer 8)])) 
  ~?= ((Env [(Integer 1, Integer 2), (Integer 3, Integer 4)] `mappend` Env [(Integer 5, Integer 6)]) 
       `mappend` Env [(Integer 7, Integer 8)])



-- Collecte tous les tests
testStructures :: Test
testStructures = TestList [
    testAdditionInteger,
    testAdditionFloat,
    testAdditionMixed,
    testAdditionMixed2,
    testSubInteger,
    testSubFloat,
    testSubMixed,
    testSubMixed2,
    testMulInteger,
    testMulFloat,
    testMulMixed,
    testMulMixed2,
    testAbsInteger,
    testAbsFloat,
    testSignumInteger,
    testSignumFloat,

    testDivisionInteger,
    testDivisionFloat,
    testDivisionMixed,
    testDivisionMixed2,
    testFromInteger,

    testToEnum,
    testFromEnum,
    testFromEnumInteger,
    testFromEnumFloat,

    testToRational,
    testToInteger1,
    testToInteger2,
    testToRationalInteger,
    testToRationalFloat,

    testMod,

    testShowInteger,
    testShowFloat,
    testShowVar,
    testShowOperator,
    testShowList,
    testShowArithmeticOp,
    testShowFunction,
    -- testShowIf,
    testShowCallable,
    testShowDeclarator,

    testShowEmpty,
    testShowNodeEmptyList,
    testShowNodeWithList,
    testShowNodeWithSubtree,

    testEnvConcat,
    testEnvEmpty,
    testEmptyEnv,
    testFromRational,

    testEmptyEquality,
    testNodeEquality,
    testNodeSubtreeInequality,
    testNodeEmptyInequality,

    testFmapEmpty,
    testFmapSingleNode,
    testFmapNodeWithSubtrees,
    testFmapComplex,

    testMempty,
    -- testMappend,
    testAssociativity,

    testQuotRemInteger,
    testModInteger,
    testModMixed1,
    testModMixed2,
    testModFloat
  ]
