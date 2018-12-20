module FsRaytracer.TracerData

open System.Numerics

type Ray = {
    origin: Vector3
    direction: Vector3
}

let calculatePosition (ray: Ray) (p: float32) = ray.origin + (p * ray.direction)