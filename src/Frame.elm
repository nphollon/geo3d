module Frame exposing (Frame, identity, transformInto, transformOutOf, toMat4, compose, setPosition, setOrientation, intrinsicNudge, intrinsicRotate, extrinsicNudge, extrinsicRotate)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Vector as Vector exposing (Vector)
import Quaternion as Quaternion exposing (Quaternion)


type alias Frame =
    { position : Vector
    , orientation : Quaternion
    }


toMat4 : Frame -> Mat4
toMat4 frame =
    Mat4.mul (Mat4.makeTranslate (Vector.toVec3 frame.position))
        (Quaternion.toMat4 frame.orientation)


identity : Frame
identity =
    { position = Vector.identity
    , orientation = Quaternion.identity
    }


transformInto : Frame -> Vector -> Vector
transformInto frame point =
    Quaternion.rotateVector
        (Quaternion.conjugate frame.orientation)
        (Vector.sub point frame.position)


transformOutOf : Frame -> Vector -> Vector
transformOutOf frame point =
    Quaternion.rotateVector frame.orientation point
        |> Vector.add frame.position


compose : Frame -> Frame -> Frame
compose parent child =
    { position = transformOutOf parent child.position
    , orientation = Quaternion.compose parent.orientation child.orientation
    }


setPosition : Vector -> Frame -> Frame
setPosition newPosition frame =
    { frame | position = newPosition }


intrinsicNudge : Vector -> Frame -> Frame
intrinsicNudge delta frame =
    { frame
        | position =
            Vector.add frame.position
                (Quaternion.rotateVector frame.orientation delta)
    }


extrinsicNudge : Vector -> Frame -> Frame
extrinsicNudge delta frame =
    { frame
        | position =
            Vector.add frame.position delta
    }


setOrientation : Quaternion -> Frame -> Frame
setOrientation newOrientation frame =
    { frame | orientation = newOrientation }


intrinsicRotate : Quaternion -> Frame -> Frame
intrinsicRotate delta frame =
    { frame | orientation = Quaternion.compose delta frame.orientation }


extrinsicRotate : Quaternion -> Frame -> Frame
extrinsicRotate delta frame =
    { frame | orientation = Quaternion.compose frame.orientation delta }
