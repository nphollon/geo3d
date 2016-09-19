module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Vector as V exposing (Vector)
import Quaternion as Q exposing (Quaternion)
import Frame as F exposing (Frame)


all : Test
all =
    describe "All Tests"
        [ quaternionTests
        , frameTests
        ]


quaternionTests : Test
quaternionTests =
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
                expectEqualFloat
                    (V.length v)
                    (V.length (Q.rotateVector q v))
        ]


frameTests : Test
frameTests =
    let
        testFrame =
            { position = V.vector -3 -4 1
            , orientation = Q.quaternion 1 5 6 4
            }

        testParticle =
            V.vector 2 -2 -1
    in
        describe "Reference Frames"
            [ test "Transforming a vector into a frame" <|
                \() ->
                    expectEqualVec
                        (V.vector (-5 / 3) (128 / 39) (172 / 39))
                        (F.transformInto testFrame testParticle)
            , test "Transforming a vector out of a frame" <|
                \() ->
                    expectEqualVec
                        (V.vector (-75 / 13) (-43 / 13) (25 / 13))
                        (F.transformOutOf testFrame testParticle)
            , fuzz frameFuzz "Inverse transforms" <|
                \frame ->
                    expectEqualVec
                        (F.transformOutOf frame testParticle)
                        (F.transformInto (F.inverse frame) testParticle)
            , fuzz frameFuzz "frame * inverse == identity" <|
                \frame ->
                    expectEqualVec
                        testParticle
                        (F.transformInto
                            (F.compose frame (F.inverse frame))
                            testParticle
                        )
            , fuzz2 frameFuzz frameFuzz "Frame composition" <|
                \a b ->
                    expectEqualVec
                        (F.transformInto (F.compose a b) testParticle)
                        (testParticle
                            |> F.transformInto b
                            |> F.transformInto a
                        )
            , fuzz3 frameFuzz frameFuzz frameFuzz "Frame composition should be associative" <|
                \a b c ->
                    expectEqualVec
                        (F.transformInto
                            (F.compose a (F.compose b c))
                            testParticle
                        )
                        (F.transformInto
                            (F.compose (F.compose a b) c)
                            testParticle
                        )
            ]


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


expectEqualFloat : Float -> Float -> Expectation
expectEqualFloat x y =
    Expect.lessThan
        1.0e-10
        ((x - y) ^ 2)


quatFuzz : Fuzzer Quaternion
quatFuzz =
    Fuzz.map3 (Q.quaternion 1) Fuzz.float Fuzz.float Fuzz.float


vecFuzz : Fuzzer Vector
vecFuzz =
    Fuzz.map3 V.vector Fuzz.float Fuzz.float Fuzz.float


frameFuzz =
    Fuzz.map2 Frame vecFuzz quatFuzz
