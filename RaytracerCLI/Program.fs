module FsRaytracer.Program

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open FsRaytracer.Tracer

[<EntryPoint>]
let main argv =
    let image = new Image<Rgba32>(200, 100)
    let traced = trace image
    traced.Save("output.png")
    0 // return an integer exit code
