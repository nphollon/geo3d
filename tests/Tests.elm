module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3
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
        , test "Scaling" <|
            \() ->
                expectEqualQuat
                    (Q.quaternion 2 4 6 8)
                    (Q.scale 2 (Q.quaternion 1 2 3 4))
        , test "Normalizing" <|
            \() ->
                expectEqualQuat
                    (Q.quaternion (3 / 85) (12 / 85) (84 / 85) (4 / 85))
                    (Q.normalize (Q.quaternion 3 12 84 4)
                        |> Maybe.withDefault Q.identity
                    )
        , test "Vector quaternions in different directions aren't similar" <|
            \() ->
                Expect.false "Expected vectors to not be similar"
                    (Q.similar
                        (Q.quaternion 0 1 2 3)
                        (Q.quaternion 0 1 4 6)
                    )
        , test "Rotation by 90 degrees about X axis" <|
            \() ->
                expectEqualVec
                    (V.vector 0 0 1)
                    (Q.rotate (Q.quaternion 1 1 0 0)
                        (V.vector 0 1 0)
                    )
        , test "Rotation by zero" <|
            \() ->
                expectEqualVec
                    (V.vector 1 0 0)
                    (Q.rotate
                        (Q.quaternion 0 0 0 0)
                        (V.vector 1 0 0)
                    )
        , test "Rotation from +X axis to -X axis" <|
            \() ->
                expectEqualQuat
                    (Q.quaternion 0 0 1 0)
                    (Q.rotationFor (V.vector 1 0 0) (V.vector -1 0 0))
        , fuzz3 quatFuzz quatFuzz quatFuzz "Mul associativity" <|
            \p q r ->
                expectEqualQuat
                    (Q.mul p (Q.mul q r))
                    (Q.mul (Q.mul p q) r)
        , fuzz2 quatFuzz vecFuzz "rotations preserve length" <|
            \q v ->
                expectEqualFloat
                    (V.length v)
                    (V.length (Q.rotate q v))
        , fuzz2 Fuzz.float quatFuzz "similarity" <|
            \f q ->
                let
                    qScaled =
                        Q.scale f q
                in
                    case ( f == 0, Q.similar q qScaled ) of
                        ( True, True ) ->
                            Expect.fail "Expect no similarity"

                        ( False, False ) ->
                            "Product = "
                                ++ toString
                                    (Q.mul (Q.conjugate q) qScaled)
                                |> Expect.fail

                        _ ->
                            Expect.pass
        , test "Converting to rotation matrix, Q1 angle" <|
            \() ->
                Expect.equal
                    (Mat4.makeRotate (degrees 30) (Vec3.vec3 0 1 0))
                    (Q.toMat4 (Q.quaternion 1 0 0.26794919243112303 0))
        , test "Converting to rotation matrix, Q2 angle" <|
            \() ->
                Expect.equal
                    (Mat4.makeRotate (degrees 150) (Vec3.vec3 0 1 0))
                    (Q.toMat4 (Q.quaternion 1 0 3.7320508075688776 0))
        , test "Converting to rotation matrix, Q3 angle" <|
            \() ->
                Expect.equal
                    (Mat4.makeRotate (degrees 210) (Vec3.vec3 0 1 0))
                    (Q.toMat4 (Q.quaternion -1 0 3.7320508075688776 0))
        , test "Converting to rotation matrix, Q4 angle" <|
            \() ->
                Expect.equal
                    (Mat4.makeRotate (degrees 330) (Vec3.vec3 0 1 0))
                    (Q.toMat4 (Q.quaternion 1 0 -0.26794919243112303 0))
        , test "Converting to rotation matrix, 180 degrees" <|
            \() ->
                Expect.equal
                    (Mat4.makeRotate (degrees 180) (Vec3.vec3 0 1 0))
                    (Q.toMat4 (Q.quaternion 0 0 1 0))
        ]


frameTests : Test
frameTests =
    let
        testFrame =
            { position = V.vector -3 -4 1
            , orientation = Q.quaternion 1 5 6 4
            }

        testVec =
            V.vector 2 -2 -1

        yRotation =
            Q.quaternion 1 0 1 0

        xRotFrame =
            { position = V.zero
            , orientation = Q.quaternion 1 1 0 0
            }
    in
        describe "Reference Frames"
            [ test "Extrinsic nudge" <|
                \() ->
                    expectEqualFrame
                        { testFrame | position = V.vector -1 -6 0 }
                        (F.extrinsicNudge testVec testFrame)
            , test "Intrinsic nudge" <|
                \() ->
                    expectEqualFrame
                        { testFrame
                            | position =
                                V.vector (-75 / 13) (-43 / 13) (25 / 13)
                        }
                        (F.intrinsicNudge testVec testFrame)
            , test "Extrinsic rotate" <|
                \() ->
                    expectEqualVec
                        (V.vector -1 0 0)
                        (F.transformInto
                            (F.extrinsicRotate yRotation xRotFrame)
                            (V.vector 0 0 1)
                        )
            , test "Intrinsic rotate" <|
                \() ->
                    expectEqualVec
                        (V.vector 0 1 0)
                        (F.transformInto
                            (F.intrinsicRotate yRotation xRotFrame)
                            (V.vector 0 0 1)
                        )
            , test "Frame equality with similar orientations" <|
                \() ->
                    expectEqualFrame
                        { position = testVec
                        , orientation = Q.quaternion 1 1 1 1
                        }
                        { position = testVec
                        , orientation = Q.quaternion 2 2 2 2
                        }
            , test "Frame inequality" <|
                \() ->
                    Expect.false "Frames should not be equal" <|
                        F.equal
                            { position = testVec
                            , orientation = Q.quaternion 1 1 1 1
                            }
                            { position = testVec
                            , orientation = Q.quaternion 1 2 2 2
                            }
            , test "Transforming a vector into a frame" <|
                \() ->
                    expectEqualVec
                        (V.vector (-25 / 39) (68 / 39) (212 / 39))
                        (F.transformInto testFrame testVec)
            , test "Transforming a vector out of a frame" <|
                \() ->
                    expectEqualVec
                        (V.vector (-17 / 3) (-103 / 39) (31 / 39))
                        (F.transformOutOf testFrame testVec)
            , fuzz frameFuzz "Inverse transforms" <|
                \frame ->
                    expectEqualVec
                        (F.transformOutOf frame testVec)
                        (F.transformInto (F.inverse frame) testVec)
            , fuzz frameFuzz "frame * inverse == identity" <|
                \frame ->
                    expectEqualVec
                        testVec
                        (F.transformInto
                            (F.compose frame (F.inverse frame))
                            testVec
                        )
            , fuzz2 frameFuzz frameFuzz "Frame composition" <|
                \a b ->
                    expectEqualVec
                        (F.transformInto (F.compose a b) testVec)
                        (testVec
                            |> F.transformInto a
                            |> F.transformInto b
                        )
            , fuzz2 frameFuzz frameFuzz "Frame multiplication" <|
                \a b ->
                    expectEqualVec
                        (F.transformInto (F.mul a b) testVec)
                        (testVec
                            |> F.transformInto b
                            |> F.transformInto a
                        )
            , fuzz3 frameFuzz frameFuzz frameFuzz "Frame composition should be associative" <|
                \a b c ->
                    expectEqualVec
                        (F.transformInto
                            (F.compose a (F.compose b c))
                            testVec
                        )
                        (F.transformInto
                            (F.compose (F.compose a b) c)
                            testVec
                        )
            , test "Mat4 form should transform vectors the same way" <|
                \() ->
                    expectEqualVec
                        (V.vector (-17 / 3) (-103 / 39) (31 / 39))
                        (V.toVec3 testVec
                            |> Mat4.transform (F.toMat4 testFrame)
                            |> V.fromVec3
                        )
            ]


expectEqualFrame : Frame -> Frame -> Expectation
expectEqualFrame f g =
    if F.equal f g then
        Expect.pass
    else
        Expect.equal f g


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


expectEqualMat4 : Mat4 -> Mat4 -> Expectation
expectEqualMat4 a b =
    let
        transform v m =
            V.fromVec3 (Mat4.transform m v)

        firstCol =
            transform (Vec3.vec3 1 0 0)

        secondCol =
            transform (Vec3.vec3 0 1 0)

        thirdCol =
            transform (Vec3.vec3 0 0 1)

        equal xf =
            V.equal (xf a) (xf b)
    in
        if equal firstCol && equal secondCol && equal thirdCol then
            Expect.pass
        else
            Expect.equal a b


quatFuzz : Fuzzer Quaternion
quatFuzz =
    Fuzz.map3 (flip Q.quaternion 1) Fuzz.float Fuzz.float Fuzz.float


vecFuzz : Fuzzer Vector
vecFuzz =
    Fuzz.map3 V.vector Fuzz.float Fuzz.float Fuzz.float


frameFuzz =
    Fuzz.map2 Frame vecFuzz quatFuzz
