module FsRaytracer.Scene

open System.Numerics
open MathExt

type Camera = {
    origin: Vector3
    lowerLeftCorner: Vector3
    horizontal: Vector3
    vertical: Vector3
}

let defaultCamera = { origin = (vec3 0.0f 0.0f 0.0f); vertical = (vec3 0.0f 2.0f 0.0f); horizontal = (vec3 4.0f 0.0f 0.0f); lowerLeftCorner = (vec3 -2.0f -1.0f -1.0f) }

type Sphere = {
    center: Vector3
    radius: float32
}

type SceneObject =
    | Sphere of Sphere
    | Objects of SceneObject seq