﻿module FsRaytracer.TracerData

open System.Numerics

type Ray = {
    origin: Vector3
    direction: Vector3
}

let inline makeRay (origin: Vector3) (direction: Vector3) = { origin = origin; direction = direction }

let calculatePosition (ray: Ray) (p: float32) = ray.origin + (p * ray.direction)

let inline vec3 (x: float32) (y: float32) (z: float32) = Vector3(x, y, z)

let inline mul (s: float32) (vec: Vector3) = Vector3.Multiply(vec, s)

let inline dotP (vec1: Vector3) (vec2: Vector3) = Vector3.Dot(vec1, vec2)

let inline norm (vec: Vector3) = Vector3.Normalize(vec)