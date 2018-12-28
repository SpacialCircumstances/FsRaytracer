module FsRaytracer.MathExt

open System.Numerics

let inline vec3 (x: float32) (y: float32) (z: float32) = Vector3(x, y, z)

let inline mul (s: float32) (vec: Vector3) = Vector3.Multiply(vec, s)

let inline div (s: float32) (vec: Vector3) = Vector3.Divide(vec, s)

let inline dotP (vec1: Vector3) (vec2: Vector3) = Vector3.Dot(vec1, vec2)

let inline norm (vec: Vector3) = Vector3.Normalize(vec)

let pi = float32 System.Math.PI

let inline crossP (vec1: Vector3) (vec2: Vector3) = Vector3.Cross(vec1, vec2)