module FsRaytracer.Objects

open System.Numerics
open MathExt

type Camera = {
    origin: Vector3
    lowerLeftCorner: Vector3
    horizontal: Vector3
    vertical: Vector3
    lensRadius: float32
    u: Vector3
    v: Vector3
    w: Vector3
}

let createCamera (lookFrom: Vector3) (lookAt: Vector3) (up: Vector3) (verticalFov: float32) (aspectRatio: float32) (aperture: float32) (focusDistance: float32) =
    let lensRadius = aperture / 2.0f
    let theta = (verticalFov * pi) / 180.0f
    let hh = tan (theta / 2.0f)
    let hw = aspectRatio * hh
    let w = norm (lookFrom - lookAt)
    let u = norm (crossP up w)
    let v = crossP w u
    let llc = lookFrom - (u * hw * focusDistance) - (v * hh * focusDistance) - (w * focusDistance)
    let horiz = 2.0f * hw * u * focusDistance
    let vert = 2.0f * hh * v * focusDistance
    { origin = lookFrom; vertical = vert; horizontal = horiz; lowerLeftCorner = llc; lensRadius = lensRadius; u = u; v = v; w = w }

let fromOrigin = createCamera Vector3.Zero (vec3 0.0f 0.0f -1.0f) (vec3 0.0f 1.0f 0.0f)

type Rng = unit -> float32

let rec randomInUnitSphere (rng: Rng) =
    let p = 2.0f * (vec3 (rng ()) (rng ()) (rng ())) - Vector3.One
    if p.LengthSquared () >= 1.0f then
        randomInUnitSphere rng
    else p


let rec randomInUnitDisk (rng: Rng) =
    let p = 2.0f * (vec3 (rng ()) (rng ()) 0.0f) - (vec3 1.0f 1.0f 0.0f)
    if dotP p p >= 1.0f then
        randomInUnitDisk rng
    else p

type Ray = {
    origin: Vector3
    direction: Vector3
}

let inline calculatePosition (ray: Ray) (p: float32) = ray.origin + (p * ray.direction)

let inline makeRay (origin: Vector3) (direction: Vector3) = { origin = origin; direction = direction }

let inline castRay (camera: Camera) (rng: Rng) (s: float32) (t: float32) =
    let rand = (randomInUnitDisk rng) * camera.lensRadius
    let offset = (camera.u * rand.X) + (camera.v * rand.Y)
    makeRay (camera.origin + offset) (camera.lowerLeftCorner + (camera.horizontal * s) + (camera.vertical * t) - camera.origin - offset)

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

let schlick (cosine: float32) (refr: float32) =
    let rr = (1.0f - refr) / (1.0f + refr)
    let r = rr * rr
    r + ((1.0f - r) * (pown (1.0f - cosine) 5))

let dielectric (ri: float32) (rng: Rng) =
    fun (ray: Ray) (hit: Hit) ->
        let (outwardNormal, refr, cosine) = if dotP ray.direction hit.normal > 0.0f then
                                                (-hit.normal, ri, ri * (dotP ray.direction hit.normal) / ray.direction.Length())
                                            else
                                                (hit.normal, 1.0f / ri, - (dotP ray.direction hit.normal) / ray.direction.Length())
        match refract ray.direction outwardNormal refr with
                            | Some refracted ->
                                let reflectProb = schlick cosine ri
                                if rng () < reflectProb then
                                    Some (vec3 1.0f 1.0f 1.0f, makeRay hit.position (reflect ray.direction hit.normal))
                                else
                                    Some (vec3 1.0f 1.0f 1.0f, makeRay hit.position refracted)
                            | None ->
                                Some (vec3 1.0f 1.0f 1.0f, makeRay hit.position (reflect ray.direction hit.normal))