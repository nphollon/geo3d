module Frame exposing (Frame, identity, transformInto, transformOutOf, toMat4, compose, setPosition, setOrientation, intrinsicNudge, intrinsicRotate, extrinsicNudge, extrinsicRotate, encode, decode)

{-| A Frame describes the difference between two coordinate systems -- the position and orientation of one reference frame relative to another.

# Definition
@docs Frame, identity

# Changing Frames
@docs setPosition, intrinsicNudge, extrinsicNudge, setOrientation, intrinsicRotate, extrinsicRotate, transformInto, transformOutOf, compose

# Interop
@docs encode, decode, toMat4
-}

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, (:=))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Vector as Vector exposing (Vector)
import Quaternion as Quaternion exposing (Quaternion)


{-| Frame consists of a position and an orientation.
-}
type alias Frame =
    { position : Vector
    , orientation : Quaternion
    }


{-| Convert to an [elm-linear-algebra Mat4](http://package.elm-lang.org/packages/elm-community/elm-linear-algebra/latest)
-}
toMat4 : Frame -> Mat4
toMat4 frame =
    Mat4.mul (Mat4.makeTranslate (Vector.toVec3 frame.position))
        (Quaternion.toMat4 frame.orientation)


{-| The identity frame corresponds to no transformation.
-}
identity : Frame
identity =
    { position = Vector.identity
    , orientation = Quaternion.identity
    }


{-| Given a frame and an extrinsic vector, return the corresponding intrinsic vector.
-}
transformInto : Frame -> Vector -> Vector
transformInto frame point =
    Quaternion.rotateVector
        (Quaternion.conjugate frame.orientation)
        (Vector.sub point frame.position)


{-| Given a frame and an intrinsic vector, return the corresponding extrinsic vector.
-}
transformOutOf : Frame -> Vector -> Vector
transformOutOf frame point =
    Quaternion.rotateVector frame.orientation point
        |> Vector.add frame.position


{-| Given a frame A to B, and another frame B to C, return the frame A to C.
-}
compose : Frame -> Frame -> Frame
compose parent child =
    { position = transformOutOf parent child.position
    , orientation = Quaternion.compose parent.orientation child.orientation
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
    { frame
        | position =
            Vector.add frame.position
                (Quaternion.rotateVector frame.orientation delta)
    }


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
    { frame | orientation = Quaternion.compose delta frame.orientation }


{-| Rotate a frame by a rotation defined outside the frame.
-}
extrinsicRotate : Quaternion -> Frame -> Frame
extrinsicRotate delta frame =
    { frame | orientation = Quaternion.compose frame.orientation delta }


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
    Decode.object2 Frame
        ("position" := Vector.decode)
        ("orientation" := Quaternion.decode)
