module FsRaytracer.Tracer

open SixLabors.ImageSharp
open TracerData
open System.Numerics
open SixLabors.ImageSharp.PixelFormats

type Surface = Image<Rgba32>

let oneVector = vec3 1.0f 1.0f 1.0f
let colorVector = vec3 0.5f 0.7f 1.0f

let color (ray: Ray) =
    let normDirection = Vector3.Normalize ray.direction
    let t = 0.5f * (normDirection.Y + 1.0f)
    (oneVector * (1.0f - t)) + (t * colorVector)

let toRgba (vec: Vector3) = Rgba32 vec

let trace (surface: Surface) =
    let height = surface.Height
    let width = surface.Width
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
            surface.Item (i, height - 1 - j) <- (toRgba col)

    surface