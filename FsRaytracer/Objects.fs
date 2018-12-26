module FsRaytracer.Objects

open System.Numerics
open MathExt

type Camera = {
    origin: Vector3
    lowerLeftCorner: Vector3
    horizontal: Vector3
    vertical: Vector3
}

let defaultCamera = { origin = (vec3 0.0f 0.0f 0.0f); vertical = (vec3 0.0f 2.0f 0.0f); horizontal = (vec3 4.0f 0.0f 0.0f); lowerLeftCorner = (vec3 -2.0f -1.0f -1.0f) }

type Ray = {
    origin: Vector3
    direction: Vector3
}

let inline calculatePosition (ray: Ray) (p: float32) = ray.origin + (p * ray.direction)

let inline makeRay (origin: Vector3) (direction: Vector3) = { origin = origin; direction = direction }

let inline castRay (camera: Camera) (u: float32) (v: float32) =
    makeRay camera.origin (camera.lowerLeftCorner + (mul u camera.horizontal) + (mul v camera.vertical) - camera.origin)

type Material = Ray -> Hit -> (Vector3 * Ray) option

and Hit = {
    parameter: float32
    position: Vector3
    normal: Vector3
    material: Material
}

type SceneBody = {
    hit: Ray -> float32 -> float32 -> Hit Option
}

let sphere (center: Vector3) (radius: float32) (material: Material) =
    let hitDetection ray tmin tmax =
        let isHit t =
            if t < tmax && t > tmin then
                let pos = (calculatePosition ray t)
                Some { parameter = t; position = pos; normal = (pos - center) / radius; material = material }
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

let dummyMaterial (r: Ray) (h: Hit) = None

let group (bodies: SceneBody seq)  =
    let hitDetection ray tmin tmax =
        let hitFound, _ = Seq.fold (fun (currentHit, closest) o ->
                                let hitThis = o.hit ray tmin closest
                                match hitThis with
                                    | None -> (currentHit, closest)
                                    | Some newHit -> (Some newHit, newHit.parameter)
                                ) (None, tmax) bodies
        hitFound
    
    { hit = hitDetection }