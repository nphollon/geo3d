module Quaternion exposing (Quaternion, toVector, fromVector, fromAxisAngle, getX, getY, getZ, getW, equal, similar, add, scale, mul, compose, rotate, reverseRotate, rotationFor, quaternion, conjugate, lengthSquared, length, encode, decode, identity, toMat4, rotateX, rotateY, rotateZ)

{-| A quaternion type. Used for rotations in three dimensions.

# Building

@docs Quaternion, quaternion, identity, rotateX, rotateY, rotateZ, rotationFor, fromAxisAngle, fromVector, toVector

# Transforming

@docs compose, conjugate, rotate

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


{-| Returns true when two quaternions are about equal.
-}
equal : Quaternion -> Quaternion -> Bool
equal p q =
    let
        threshold =
            max 1 (lengthSquared p) * 1.0e-10

        aboutEqual x y =
            (x - y) ^ 2 < threshold
    in
        (aboutEqual (getW p) (getW q))
            && (aboutEqual (getX p) (getX q))
            && (aboutEqual (getY p) (getY q))
            && (aboutEqual (getZ p) (getZ q))


similar : Quaternion -> Quaternion -> Bool
similar p q =
    if lengthSquared p > 0 && lengthSquared q > 0 then
        mul p (conjugate q)
            |> pureScalar
    else
        False


pureScalar : Quaternion -> Bool
pureScalar q =
    Vector.lengthSquared q.vector / lengthSquared q < 1.0e-10


{-| Get scalar component.
-}
getW : Quaternion -> Float
getW q =
    q.scalar


{-| Get first vector component.
-}
getX : Quaternion -> Float
getX q =
    q.vector.x


{-| Get second vector component.
-}
getY : Quaternion -> Float
getY q =
    q.vector.y


{-| Get third vector component.
-}
getZ : Quaternion -> Float
getZ q =
    q.vector.z


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

    rotate q v == vector 0 0 1 -- rotated into z axis
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


{-| Add two quaternions.

If you are trying to combine rotations, you should to use `mul` or `compose` instead.
-}
add : Quaternion -> Quaternion -> Quaternion
add p q =
    { scalar = p.scalar + q.scalar
    , vector = Vector.add p.vector q.vector
    }


scale : Float -> Quaternion -> Quaternion
scale f q =
    { scalar = f * q.scalar
    , vector = Vector.scale f q.vector
    }


{-| Multiply two quaternions.
-}
mul : Quaternion -> Quaternion -> Quaternion
mul p q =
    { scalar = q.scalar * p.scalar - (Vector.dot q.vector p.vector)
    , vector =
        (Vector.scale q.scalar p.vector)
            `Vector.add` (Vector.scale p.scalar q.vector)
            `Vector.add` (Vector.cross p.vector q.vector)
    }


{-| Multiplication with the operands flipped.

This can make multiplication easier to use along with the pipe operators `|>` and `<|`

    compose p q == mul q p
-}
compose : Quaternion -> Quaternion -> Quaternion
compose =
    flip mul


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

The quaternion does not have to be a unit quaternion. The vector length will be preserved.

If given the zero quaternion, no rotation will be performed.
-}
rotate : Quaternion -> Vector -> Vector
rotate q v =
    let
        quadrance =
            lengthSquared q
    in
        if quadrance == 0 then
            v
        else
            { vector = v
            , scalar = 0
            }
                |> mul q
                |> compose (conjugate q)
                |> .vector
                |> Vector.scale (1 / quadrance)


{-| Rotate vector in the opposite direction.
    reverseRotate q v == rotate (conjugate q) v
-}
reverseRotate : Quaternion -> Vector -> Vector
reverseRotate q =
    rotate (conjugate q)


lengthSquared : Quaternion -> Float
lengthSquared q =
    q.scalar ^ 2 + (Vector.lengthSquared q.vector)


length : Quaternion -> Float
length =
    sqrt << lengthSquared


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
