module FsRaytracer.Objects

open System.Numerics

type Ray = {
    origin: Vector3
    direction: Vector3
}

type Hit = {
    parameter: float32
    position: Vector3
    normal: Vector3
}