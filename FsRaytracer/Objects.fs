module FsRaytracer.Objects

open System.Numerics
open MathExt

type Ray = {
    origin: Vector3
    direction: Vector3
}

let inline calculatePosition (ray: Ray) (p: float32) = ray.origin + (p * ray.direction)

type Hit = {
    parameter: float32
    position: Vector3
    normal: Vector3
}

type SceneBody = {
    hit: Ray -> float32 -> float32 -> Hit Option
}

let sphere (center: Vector3) (radius: float32) =
    let hitDetection ray tmin tmax =
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

    { hit = hitDetection }

let group (bodies: SceneBody seq) =
    let hitDetection ray tmin tmax =
        let hitFound, _ = Seq.fold (fun (currentHit, closest) o ->
                                let hitThis = o.hit ray tmin closest
                                match hitThis with
                                    | None -> (currentHit, closest)
                                    | Some newHit -> (Some newHit, newHit.parameter)
                                ) (None, tmax) bodies
        hitFound

    { hit = hitDetection }