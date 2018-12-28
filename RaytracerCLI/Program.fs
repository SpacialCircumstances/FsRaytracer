module FsRaytracer.Program

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open FsRaytracer.Tracer
open System.Numerics
open FsRaytracer.MathExt
open FsRaytracer.Objects

let toRgba32 (vec: Vector3) = Rgba32 vec

let random = Random()

let rng () = float32 (random.NextDouble ())

let imageSurface (image: Image<Rgba32>) =
    let setColor (x, y) col =
        image.Item (x, image.Height - 1 - y) <- (toRgba32 col)
    { height = image.Height; width = image.Width; setColor = setColor }

[<EntryPoint>]
let main argv =
    let world = group [ sphere (vec3 0.0f 0.0f -1.0f) 0.5f (dielectric 0.1f); sphere (vec3 0.0f -100.5f -1.0f) 100.0f (lambertian (vec3 0.5f 0.6f 0.3f) rng) ]
    let image = new Image<Rgba32>(200, 100)
    let traced = trace defaultCamera world defaultSettings (imageSurface image)
    image.Save("output.png")
    0 // return an integer exit code
