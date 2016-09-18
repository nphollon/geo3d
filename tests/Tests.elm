module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Vector as V exposing (Vector)
import Quaternion as Q exposing (Quaternion)


all : Test
all =
    describe "Quaternion"
        [ test "Addition" <|
            \() ->
                Expect.equal
                    (Q.quaternion 1 2 3 4)
                    (Q.add (Q.quaternion 0 1 1 5)
                        (Q.quaternion 1 1 2 -1)
                    )
        , test "Multiplication" <|
            \() ->
                Expect.equal
                    (Q.quaternion -4 -25 2 0)
                    (Q.mul (Q.quaternion 1 2 -3 1)
                        (Q.quaternion -4 -1 1 5)
                    )
        , test "Composition" <|
            \() ->
                Expect.equal
                    (Q.quaternion -4 -25 2 0)
                    (Q.compose (Q.quaternion -4 -1 1 5)
                        (Q.quaternion 1 2 -3 1)
                    )
        , test "Equality" <|
            \() ->
                expectEqualQuat
                    (Q.quaternion 0 0 0 0)
                    (Q.quaternion 1.0e-11 0 0 0)
        , test "Rotation by 90 degrees about X axis" <|
            \() ->
                expectEqualVec
                    (V.vector 0 0 1)
                    (Q.rotateVector (Q.quaternion 1 1 0 0)
                        (V.vector 0 1 0)
                    )
        , test "Rotation by zero" <|
            \() ->
                expectEqualVec
                    (V.vector 1 0 0)
                    (Q.rotateVector
                        (Q.quaternion 0 0 0 0)
                        (V.vector 1 0 0)
                    )
        , fuzz3 quatFuzz quatFuzz quatFuzz "Mul associativity" <|
            \p q r ->
                expectEqualQuat
                    (Q.mul p (Q.mul q r))
                    (Q.mul (Q.mul p q) r)
        , fuzz2 quatFuzz vecFuzz "rotations preserve length" <|
            \q v ->
                expectAboutEqual
                    (V.length v)
                    (V.length (Q.rotateVector q v))
        ]



-- associativity of addition
-- associativity of multiplication
-- distributivity of multiplication over addition
-- rotations preserve length
-- rotation by zero


expectEqualQuat : Quaternion -> Quaternion -> Expectation
expectEqualQuat p q =
    if Q.equal p q then
        Expect.pass
    else
        Expect.equal p q


expectEqualVec : Vector -> Vector -> Expectation
expectEqualVec u v =
    if V.equal u v then
        Expect.pass
    else
        Expect.equal u v


expectAboutEqual : Float -> Float -> Expectation
expectAboutEqual x y =
    Expect.lessThan
        1.0e-10
        ((x - y) ^ 2)


quatFuzz : Fuzzer Quaternion
quatFuzz =
    Fuzz.map4 Q.quaternion Fuzz.float Fuzz.float Fuzz.float Fuzz.float


vecFuzz : Fuzzer Vector
vecFuzz =
    Fuzz.map3 V.vector Fuzz.float Fuzz.float Fuzz.float
