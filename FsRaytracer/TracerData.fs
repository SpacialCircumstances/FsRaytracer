module FsRaytracer.TracerData

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

type Sphere = {
    center: Vector3
    radius: float32
}

type SceneObject =
    | Sphere of Sphere

let inline makeRay (origin: Vector3) (direction: Vector3) = { origin = origin; direction = direction }

let calculatePosition (ray: Ray) (p: float32) = ray.origin + (p * ray.direction)

let inline vec3 (x: float32) (y: float32) (z: float32) = Vector3(x, y, z)

let inline mul (s: float32) (vec: Vector3) = Vector3.Multiply(vec, s)

let inline dotP (vec1: Vector3) (vec2: Vector3) = Vector3.Dot(vec1, vec2)

let inline norm (vec: Vector3) = Vector3.Normalize(vec)

let hit (ray: Ray) (tmin: float32) (tmax: float32) (body: SceneObject) =
    match body with
        | Sphere sphere ->
            let oc = ray.origin - sphere.center
            let a = dotP ray.direction ray.direction
            let b = dotP oc ray.direction
            let c = (dotP oc oc) - (sphere.radius * sphere.radius)
            let discriminant = (b * b) - (a * c)
            if discriminant > 0.0f then
                ()
            else
                ()