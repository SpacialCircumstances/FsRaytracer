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
    let width = 200
    let height = 100
    let ar = (float32 width) / (float32 height)
    let camera = createCamera 60.0f ar
    let r = pi / 4.0f
    let world = group [
        sphere (vec3 -r 0.0f -1.0f) r (lambertian (vec3 0.0f 0.0f 1.0f) rng);
        sphere (vec3 r 0.0f -1.0f) r (lambertian (vec3 0.0f 1.0f 0.0f) rng)
    ]
    let image = new Image<Rgba32>(width, height)
    let traced = trace camera world defaultSettings (imageSurface image)
    image.Save("output.png")
    0 // return an integer exit code
