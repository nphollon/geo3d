module Vector exposing (Vector, vector, zero, identity, xAxis, yAxis, zAxis, getX, getY, getZ, add, sub, negate, scale, dot, cross, normalize, direction, length, lengthSquared, distance, distanceSquared, equal, fromTuple, toTuple, encode, decode, fromVec3, toVec3)

{-| Three-dimensional vector type.

# Building
@docs Vector, vector, zero, identity, xAxis, yAxis, zAxis

# Math
@docs getX, getY, getZ, equal, add, sub, negate, scale, dot, cross, normalize, length, lengthSquared, direction, distance, distanceSquared

# Interop
@docs fromTuple, toTuple, encode, decode, fromVec3, toVec3

-}

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Math.Vector3 as Vec3 exposing (Vec3)


{-| Vector is just a type alias for a record.
-}
type alias Vector =
    { x : Float, y : Float, z : Float }


{-| Construct a vector from x, y, and z coordinates.
-}
vector : Float -> Float -> Float -> Vector
vector x y z =
    { x = x, y = y, z = z }


{-| The zero vector.
    zero == vector 0 0 0
-}
zero : Vector
zero =
    vector 0 0 0


{-| Alias for zero.
-}
identity : Vector
identity =
    zero


{-| Normal vector along the x axis.

    xAxis == vector 1 0 0
-}
xAxis : Vector
xAxis =
    vector 1 0 0


{-| Normal vector along the y axis.

    yAxis == vector 0 1 0
-}
yAxis : Vector
yAxis =
    vector 0 1 0


{-| Normal vector along the z axis.

    zAxis == vector 0 0 1
-}
zAxis : Vector
zAxis =
    vector 0 0 1


{-| Get the x component of a vector.

    getX (vector 1 2 3) == 1
-}
getX : Vector -> Float
getX =
    .x


{-| Get the y component of a vector.

    getY (vector 1 2 3) == 2
-}
getY : Vector -> Float
getY =
    .y


{-| Get the z component of a vector.

    getZ (vector 1 2 3) == 3
-}
getZ : Vector -> Float
getZ =
    .z


{-| Add two vectors.
-}
add : Vector -> Vector -> Vector
add a b =
    vector (getX a + getX b)
        (getY a + getY b)
        (getZ a + getZ b)


{-| Subtract two vectors.
-}
sub : Vector -> Vector -> Vector
sub a b =
    vector (getX a - getX b)
        (getY a - getY b)
        (getZ a - getZ b)


{-| Negate a vector.
-}
negate : Vector -> Vector
negate =
    sub (vector 0 0 0)


{-| Multiply a vector by a number.
-}
scale : Float -> Vector -> Vector
scale c v =
    vector (c * getX v)
        (c * getY v)
        (c * getZ v)


{-| Compute the [dot product](https://en.wikipedia.org/wiki/Dot_product) of two vectors.
-}
dot : Vector -> Vector -> Float
dot u v =
    (getX u * getX v) + (getY u * getY v) + (getZ u * getZ v)


{-| Compute the [cross product](https://en.wikipedia.org/wiki/Cross_product) of two vectors.
-}
cross : Vector -> Vector -> Vector
cross u v =
    vector ((getY u * getZ v) - (getZ u * getY v))
        ((getZ u * getX v) - (getX u * getZ v))
        ((getX u * getY v) - (getY u * getX v))


{-| Return the unit vector that points in the same direction as the given vector. Return Nothing if given the zero vector.

    normalize (vector 0 3 0) == Just (vector 0 1 0)
    normalize (vector 0 0 0) == Nothing
    normalize (vector 1 1 1) == Just (scale (1/sqrt 3) (vector 1 1 1))
-}
normalize : Vector -> Maybe Vector
normalize v =
    if length v == 0 then
        Nothing
    else
        Just (scale (1 / length v) v)


{-| Return the unit vector that points from the second vector to the first vector. Return Nothing if the vectors are the same.

    direction (vector 1 1 0) (vector -1 1 0) == Just (vector 1 0 0)
    direction a b == normalize (a `sub` b)
-}
direction : Vector -> Vector -> Maybe Vector
direction u v =
    normalize (u `sub` v)


{-| Length of a vector. Always greater than or equal to zero.
-}
length : Vector -> Float
length =
    lengthSquared >> sqrt


{-| Square of the length of a vector.
-}
lengthSquared : Vector -> Float
lengthSquared v =
    v `dot` v


{-| Distance from one vector to another. Always greater than or equal to zero.

    direction (vector 1 1 0) (vector -1 1 0) == 2
    direction a b == length (a `sub` b)
-}
distance : Vector -> Vector -> Float
distance u v =
    length (u `sub` v)


{-| Square of the distance between two vectors.
-}
distanceSquared : Vector -> Vector -> Float
distanceSquared u v =
    lengthSquared (u `sub` v)


{-| Returns true when two vectors are about equal (within 10^-5).
-}
equal : Vector -> Vector -> Bool
equal u v =
    let
        threshold =
            max 1 (lengthSquared u) * 1.0e-10

        equalFloat p q =
            (p - q) ^ 2 < threshold
    in
        (equalFloat (getX u) (getX v))
            && (equalFloat (getY u) (getY v))
            && (equalFloat (getZ u) (getZ v))


{-| Convert a 3-tuple to a vector.

    fromTuple (1, 2, 3) == vector 1 2 3
-}
fromTuple : ( Float, Float, Float ) -> Vector
fromTuple ( x, y, z ) =
    vector x y z


{-| Convert a vector to a 3-tuple.

    toTuple (vector 1 2 3) == (1, 2, 3)
-}
toTuple : Vector -> ( Float, Float, Float )
toTuple v =
    ( getX v, getY v, getZ v )


{-| Convert a vector into a [Json Value](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Encode).
-}
encode : Vector -> Value
encode v =
    Encode.list
        [ Encode.float (getX v)
        , Encode.float (getY v)
        , Encode.float (getZ v)
        ]


{-| A [Json Decoder](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode) for vectors encoded with `encode`.
-}
decode : Decoder Vector
decode =
    Decode.tuple3 vector Decode.float Decode.float Decode.float


{-| Convert from an [elm-linear-algebra Vec3](http://package.elm-lang.org/packages/elm-community/elm-linear-algebra/latest)
-}
fromVec3 : Vec3 -> Vector
fromVec3 =
    Vec3.toRecord


{-| Convert to an [elm-linear-algebra Vec3](http://package.elm-lang.org/packages/elm-community/elm-linear-algebra/latest)
-}
toVec3 : Vector -> Vec3
toVec3 =
    Vec3.fromRecord
