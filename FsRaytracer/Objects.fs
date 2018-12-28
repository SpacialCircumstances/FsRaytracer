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

type Rng = unit -> float32

let randomInUnitSphere (rng: Rng) =
    let randomNumbers () = Seq.initInfinite (fun _ -> rng ())
    Seq.zip3 (randomNumbers ()) (randomNumbers ()) (randomNumbers ()) |> Seq.find (fun (x, y, z) -> ((mul 2.0f (vec3 x y z)) - Vector3.One).LengthSquared () < 1.0f) |> fun (a, b, c) -> vec3 a b c

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

let reflect (vec: Vector3) (normal: Vector3) =
    vec - ((2.0f * (dotP vec normal)) * normal)

let refract (vec: Vector3) (normal: Vector3) (refrRelation: float32) =
    let uvec = norm vec
    let dt = dotP uvec normal
    let discr = 1.0f - (refrRelation * refrRelation * (1.0f - (dt * dt)))
    if discr > 0.0f then
        Some (((uvec - (normal * dt)) * refrRelation) - (normal * (sqrt discr)))
    else
        None

let lambertian (albedo: Vector3) (rng: Rng) =
    fun (ray: Ray) (hit: Hit) ->
        let target = hit.position + hit.normal + (randomInUnitSphere rng)
        let scattered = makeRay hit.position (target - hit.position)
        Some (albedo, scattered)

let metal (albedo: Vector3) (fuzziness: float32) (rng: Rng) =
    if fuzziness > 1.0f then invalidArg "fuzziness" "Must be <= 1.0"
    fun (ray: Ray) (hit: Hit) ->
        let reflected = reflect (norm ray.direction) hit.normal
        let scattered = makeRay hit.position (reflected + (randomInUnitSphere rng * fuzziness))
        if dotP scattered.direction hit.normal > 0.0f then
            Some (albedo, scattered)
        else None

let dielectric (ri: float32) =
    fun (ray: Ray) (hit: Hit) ->
        let (outwardNormal, refr) = if dotP ray.direction hit.normal > 0.0f then
                                        (-hit.normal, ri)
                                    else
                                        (hit.normal, 1.0f / ri)
        match refract ray.direction outwardNormal refr with
            | Some refracted ->
                Some (vec3 1.0f 1.0f 1.0f, makeRay hit.position refracted)
            | None ->
                let reflected = reflect ray.direction hit.normal
                Some (vec3 1.0f 1.0f 1.0f, makeRay hit.position reflected)
