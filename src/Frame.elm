module Frame exposing (Frame, identity, equal, transformInto, transformOutOf, toMat4, inverse, mul, compose, setPosition, setOrientation, intrinsicNudge, intrinsicRotate, extrinsicNudge, extrinsicRotate, encode, decode)

{-| A Frame describes the difference between two coordinate systems -- the position and orientation of one reference frame relative to another.

# Definition

@docs Frame, identity, equal

# Coordinate Transformations
@docs transformInto, transformOutOf, mul, compose, inverse

# Mutating Frames
@docs setPosition, intrinsicNudge, extrinsicNudge, setOrientation, intrinsicRotate, extrinsicRotate

# Interop
@docs encode, decode, toMat4
-}

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Vector as Vector exposing (Vector)
import Quaternion as Quaternion exposing (Quaternion)


{-| Frame consists of a position and an orientation.
-}
type alias Frame =
    { position : Vector
    , orientation : Quaternion
    }


{-| The identity frame corresponds to no transformation.
-}
identity : Frame
identity =
    { position = Vector.identity
    , orientation = Quaternion.identity
    }


{-| Two frames are equal if their positions and orientations match.
-}
equal : Frame -> Frame -> Bool
equal f g =
    Vector.equal f.position g.position
        && Quaternion.similar f.orientation g.orientation


{-| Given a frame and an extrinsic vector, return the corresponding intrinsic vector.
-}
transformInto : Frame -> Vector -> Vector
transformInto frame point =
    Quaternion.rotate (Quaternion.conjugate frame.orientation)
        (Vector.sub point frame.position)


{-| Given a frame and an intrinsic vector, return the corresponding extrinsic vector.
-}
transformOutOf : Frame -> Vector -> Vector
transformOutOf frame point =
    Quaternion.rotate frame.orientation point
        |> Vector.add frame.position


{-| Given a frame B to C, and another frame A to B, return the frame A to C.
-}
mul : Frame -> Frame -> Frame
mul =
    flip compose


{-| Given a frame A to B, and another frame B to C, return the frame A to C.

    transformOutOf (compose a b) == transformOutOf b >> transformOutOf a
    transformInto (compose a b) == transformInto a >> transformInto b
-}
compose : Frame -> Frame -> Frame
compose parent child =
    { position =
        Quaternion.rotate parent.orientation child.position
            |> Vector.add parent.position
    , orientation =
        Quaternion.mul parent.orientation child.orientation
    }


{-| Given a frame A to B, return the frame B to A
-}
inverse : Frame -> Frame
inverse frame =
    let
        invOrientation =
            Quaternion.conjugate frame.orientation
    in
        { position =
            Vector.negate frame.position
                |> Quaternion.rotate invOrientation
        , orientation = invOrientation
        }


{-| Set the position of a frame.
-}
setPosition : Vector -> Frame -> Frame
setPosition newPosition frame =
    { frame | position = newPosition }


{-| Translate a frame by a displacement definined within the frame.
-}
intrinsicNudge : Vector -> Frame -> Frame
intrinsicNudge delta frame =
    let
        transformedDelta =
            Quaternion.rotate (Quaternion.conjugate frame.orientation)
                delta
    in
        { frame | position = Vector.add frame.position transformedDelta }


{-| Translate a frame by a displacement definined outside the frame.
-}
extrinsicNudge : Vector -> Frame -> Frame
extrinsicNudge delta frame =
    { frame
        | position =
            Vector.add frame.position delta
    }


{-| Set the orientation of a frame.
-}
setOrientation : Quaternion -> Frame -> Frame
setOrientation newOrientation frame =
    { frame | orientation = newOrientation }


{-| Rotate a frame by a rotation defined within the frame.
-}
intrinsicRotate : Quaternion -> Frame -> Frame
intrinsicRotate delta frame =
    { frame
        | orientation =
            Quaternion.mul frame.orientation delta
    }


{-| Rotate a frame by a rotation defined outside the frame.
-}
extrinsicRotate : Quaternion -> Frame -> Frame
extrinsicRotate delta frame =
    { frame
        | orientation =
            Quaternion.mul delta frame.orientation
    }


{-| Convert a frame into a [Json Value](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Encode).
-}
encode : Frame -> Value
encode frame =
    Encode.object
        [ ( "position", Vector.encode frame.position )
        , ( "orientation", Quaternion.encode frame.orientation )
        ]


{-| A [Json Decoder](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode) for frames encoded with `encode`.
-}
decode : Decoder Frame
decode =
    Decode.map2 Frame
        (Decode.field "position" Vector.decode)
        (Decode.field "orientation" Quaternion.decode)


{-| Convert to an [elm-linear-algebra Mat4](http://package.elm-lang.org/packages/elm-community/elm-linear-algebra/latest)
-}
toMat4 : Frame -> Mat4
toMat4 frame =
    Mat4.mul
        (Mat4.makeTranslate (Vector.toVec3 frame.position))
        (Quaternion.toMat4 frame.orientation)
