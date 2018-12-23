module FsRaytracer.Scene

open System.Numerics

type Sphere = {
    center: Vector3
    radius: float32
}

type SceneObject =
    | Sphere of Sphere
    | Objects of SceneObject seq