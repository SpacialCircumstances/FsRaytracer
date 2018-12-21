module FsRaytracer.Program

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open FsRaytracer.Tracer
open System.Numerics

let toRgba32 (vec: Vector3) = Rgba32 vec

let imageSurface (image: Image<Rgba32>) =
    let setColor (x, y) col =
        image.Item (x, image.Height - 1 - y) <- (toRgba32 col)
    { height = image.Height; width = image.Width; setColor = setColor }

[<EntryPoint>]
let main argv =
    let image = new Image<Rgba32>(200, 100)
    let traced = trace (imageSurface image)
    image.Save("output.png")
    0 // return an integer exit code
