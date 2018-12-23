module FsRaytracer.TracerData

open System.Numerics
open MathExt

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
    | Objects of SceneObject seq

let inline makeRay (origin: Vector3) (direction: Vector3) = { origin = origin; direction = direction }

let calculatePosition (ray: Ray) (p: float32) = ray.origin + (p * ray.direction)

let rec hit (ray: Ray) (tmin: float32) (tmax: float32) (body: SceneObject) =
    match body with
        | Sphere { center = center; radius = radius } ->
            let isHit t =
                if t < tmax && t > tmin then
                    let pos = (calculatePosition ray t)
                    Some { parameter = t; position = pos; normal = (pos - center) / radius }
                else
                    None

            let oc = ray.origin - center
            let a = dotP ray.direction ray.direction
            let b = 2.0f * (dotP oc ray.direction)
            let c = (dotP oc oc) - (radius * radius)
            let discriminant = b * b - 4.0f * a * c
            if discriminant < 0.0f then
                None
            else
                isHit ((-b - (sqrt discriminant)) / (2.0f * a))

        | Objects objects ->
            let hitFound, _ = Seq.fold (fun (currentHit, closest) o ->
                                let hitThis = hit ray tmin closest o
                                match hitThis with
                                    | None -> (currentHit, closest)
                                    | Some newHit -> (Some newHit, newHit.parameter)
                                ) (None, tmax) objects
            hitFound