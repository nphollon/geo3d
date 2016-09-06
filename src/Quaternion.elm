module Quaternion exposing (Quaternion, toVector, fromVector, fromAxisAngle, compose, rotateVector, rotationFor, quaternion, conjugate, encode, decode, identity, toMat4, rotateX, rotateY, rotateZ)

{-| A quaternion type. Used for rotations in three dimensions.

# Building

@docs Quaternion, quaternion, identity, rotationFor, rotateX, rotateY, rotateZ, fromAxisAngle, fromVector, toVector

# Using

@docs compose, conjugate, rotateVector

# Interop

@docs encode, decode, toMat4
-}

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Vector exposing (Vector)
import Math.Matrix4 as Mat4 exposing (Mat4)


{-| Quaternion is a combination of a 3-vector and a scalar.
-}
type alias Quaternion =
    { vector : Vector
    , scalar : Float
    }


{-| Convert to a [Json Value](http://package.elm-lang.org/packages/elm-lang/core/4.0.5/Json-Encode).
-}
encode : Quaternion -> Value
encode q =
    Encode.list
        [ Encode.float q.scalar
        , Encode.float (Vector.getX q.vector)
        , Encode.float (Vector.getY q.vector)
        , Encode.float (Vector.getZ q.vector)
        ]


{-| A [Json Decoder](http://package.elm-lang.org/packages/elm-lang/core/4.0.5/Json-Decode) for vectors encoded with `encode`.
-}
decode : Decoder Quaternion
decode =
    Decode.tuple4 quaternion
        Decode.float
        Decode.float
        Decode.float
        Decode.float


{-| Construct a quaternion given its four components. The first argument is the scalar component, the rest are the vector components.
-}
quaternion : Float -> Float -> Float -> Float -> Quaternion
quaternion w x y z =
    { vector = Vector.vector x y z
    , scalar = w
    }


{-| The identity quaternion corresponds to no rotation.

    identity == quaternion 1 0 0 0
-}
identity : Quaternion
identity =
    quaternion 1 0 0 0


{-| Create a quaternion corresponding to a rotation about the x axis by the given angle. Rotation is counter-clockwise.

    q = rotateX (degrees 90)
    v = vector 0 1 0 -- y axis

    rotateVector q v == vector 0 0 1 -- rotated into z axis
-}
rotateX : Float -> Quaternion
rotateX angle =
    quaternion (cos (0.5 * angle)) (sin (0.5 * angle)) 0 0


{-| Create a quaternion corresponding to a rotation about the y axis by the given angle.
-}
rotateY : Float -> Quaternion
rotateY angle =
    quaternion (cos (0.5 * angle)) 0 (sin (0.5 * angle)) 0


{-| Create a quaternion corresponding to a rotation about the z axis by the given angle.
-}
rotateZ : Float -> Quaternion
rotateZ angle =
    quaternion (cos (0.5 * angle)) 0 0 (sin (0.5 * angle))


{-| The conjugate of a quaternion.
-}
conjugate : Quaternion -> Quaternion
conjugate q =
    { q | vector = Vector.negate q.vector }


{-| Multiply two quaternions.
-}
compose : Quaternion -> Quaternion -> Quaternion
compose p q =
    { vector =
        (Vector.scale q.scalar p.vector)
            `Vector.add` (Vector.scale p.scalar q.vector)
            `Vector.add` (q.vector `Vector.cross` p.vector)
    , scalar =
        (q.scalar * p.scalar) - (q.vector `Vector.dot` p.vector)
    }


{-| Get the angle of rotation for a quaternion.

    angle (rotateY 0.2) == 0.2
-}
angle : Quaternion -> Float
angle q =
    2 * acos q.scalar


{-| Get the axis of rotation for a quaternion. Defaults to the x axis if there is no rotation.

    axis (rotateY 0.2) == vector 0 1 0
    axis identity == vector 1 0 0
-}
axis : Quaternion -> Vector
axis q =
    Vector.normalize q.vector
        |> Maybe.withDefault (Vector.vector 1 0 0)


{-| Rotate a vector by a quaternion.
-}
rotateVector : Quaternion -> Vector -> Vector
rotateVector q v =
    let
        vectorQuat =
            { vector = v
            , scalar = 0
            }
    in
        compose vectorQuat q
            |> compose (conjugate q)
            |> .vector
            |> Vector.scale (1 / quadrance q)


quadrance : Quaternion -> Float
quadrance q =
    q.scalar ^ 2 + (Vector.lengthSquared q.vector)


{-| Given two vectors, return the quaternion that would rotate the first vector to the second vector. The lengths of the vectors are ignored. If one or both vectors are zero, return the identity quaternion.
-}
rotationFor : Vector -> Vector -> Quaternion
rotationFor u v =
    let
        cross =
            Vector.cross u v

        crossMag =
            Vector.length cross

        angle =
            atan2 crossMag (Vector.dot u v)
    in
        if angle == 0 then
            identity
        else if crossMag == 0 then
            Vector.vector 1.0e-10 0 0
                |> Vector.add v
                |> rotationFor u
        else
            Vector.scale (angle / crossMag) cross
                |> fromVector


{-| Convert a rotation vector to a quaternion. A rotation vector's direction is the axis of rotation, and its length is the angle of rotation.
-}
fromVector : Vector -> Quaternion
fromVector v =
    let
        angle =
            Vector.length v
    in
        fromAxisAngle v angle
            |> Maybe.withDefault (quaternion 1 0 0 0)


{-| Create a quaternion given an axis and angle of rotation. The length of the axis is ignored, but returns Nothing if the axis is the zero vector.
-}
fromAxisAngle : Vector -> Float -> Maybe Quaternion
fromAxisAngle axis angle =
    Vector.normalize axis
        |> Maybe.map
            (\vector ->
                { scalar = cos (0.5 * angle)
                , vector = Vector.scale (sin (0.5 * angle)) vector
                }
            )


{-| Convert a quaternion to a rotation vector.
-}
toVector : Quaternion -> Vector
toVector q =
    let
        halfSin =
            Vector.length q.vector
    in
        if halfSin == 0 then
            q.vector
        else
            Vector.scale (2 * acos q.scalar / halfSin) q.vector


{-| Convert to an [elm-linear-algebra Mat4](http://package.elm-lang.org/packages/elm-community/elm-linear-algebra/latest)
-}
toMat4 : Quaternion -> Mat4
toMat4 q =
    Mat4.makeRotate (angle q) (Vector.toVec3 (axis q))
