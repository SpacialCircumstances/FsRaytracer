module FsRaytracer.Tracer

open TracerData
open System.Numerics
open System
open MathExt

type RenderSurface = {
    height: int
    width: int
    setColor: int * int -> Vector3 -> unit
}

let oneVector = vec3 1.0f 1.0f 1.0f
let colorVector = vec3 0.5f 0.7f 1.0f

let colorRed = vec3 1.0f 0.0f 0.0f

let color (ray: Ray) (world: SceneObject) =
    match hit ray 0.0f Single.MaxValue world with
        | None ->
            let unitDirection = norm ray.direction
            let t = 0.5f * (unitDirection.Y + 1.0f)
            (1.0f - t) * oneVector + (t * colorVector)
        | Some hit ->
            0.5f * (hit.normal + oneVector)

let trace (surface: RenderSurface) (world: SceneObject) =
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
            let col = color ray world
            setColor (i, j) col

    surface