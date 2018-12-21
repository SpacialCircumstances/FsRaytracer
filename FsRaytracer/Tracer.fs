module FsRaytracer.Tracer

open TracerData
open System.Numerics
open System

type RenderSurface = {
    height: int
    width: int
    setColor: int * int -> Vector3 -> unit
}

let oneVector = vec3 1.0f 1.0f 1.0f
let colorVector = vec3 0.5f 0.7f 1.0f

let sphereCenter = vec3 0.0f 0.0f -1.0f
let sphereRadius = 0.5f

let colorRed = vec3 1.0f 0.0f 0.0f

let inline sqrt (f: float32) = float32 (Math.Sqrt (float f))

let hitSphere (center: Vector3) (radius: float32) (ray: Ray) =
    let oc = ray.origin - center
    let a = dotP ray.direction ray.direction
    let b = 2.0f * (dotP oc ray.direction)
    let c = (dotP oc oc) - (radius * radius)
    let discriminant = b * b - 4.0f * a * c
    if discriminant < 0.0f then
        None
    else
        Some ((-b - (sqrt discriminant)) / (2.0f * a))
    

let color (ray: Ray) =
    match (hitSphere sphereCenter sphereRadius ray) with
        | Some hit ->
            if hit < 0.0f then invalidOp "Hit may not be negative"
            let n = (norm (calculatePosition ray hit) - (vec3 0.0f 0.0f -1.0f))
            0.5f * (n + oneVector)
        | None ->
            let normDirection = Vector3.Normalize ray.direction
            let t = 0.5f * (normDirection.Y + 1.0f)
            (oneVector * (1.0f - t)) + (t * colorVector)

let trace (surface: RenderSurface) =
    let { height = height; width = width; setColor = setColor } = surface
    let h = float32(height)
    let w = float32(width)
    let lowerLeft = vec3 -2.0f -1.0f -1.0f
    let horiz = vec3 4.0f 0.0f 0.0f
    let vert = vec3 0.0f 2.0f 0.0f
    let origin = vec3 0.0f 0.0f 0.0f

    for j = (height - 1) downto 0 do
        for i = 0 to (width - 1) do
            let u = float32(i) / w
            let v = float32(j) / h
            let ray = makeRay origin (lowerLeft + (mul u horiz) + (mul v vert))
            let col = color ray
            setColor (i, j) col

    surface