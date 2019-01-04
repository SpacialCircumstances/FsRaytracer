module FsRaytracer.Program

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open FsRaytracer.Tracer
open System.Numerics
open FsRaytracer.MathExt
open FsRaytracer.Objects

let toRgba32 (vec: Vector3) = Rgba32 vec

let rng = multithreadRandom

let imageSurface (image: Image<Rgba32>) =
    let setColor (x, y) col =
        image.Item (x, image.Height - 1 - y) <- (toRgba32 col)
    { height = image.Height; width = image.Width; setColor = setColor }

let randomScene =
    let ground = sphere (vec3 0.0f -1000.0f 0.0f) 1000.0f (lambertian (vec3 0.5f 0.5f 0.5f) rng)
    let rp () = rng () - 0.5f
    let randomSpheres = [0..100] |> List.map (fun _ ->
        let y = (rng () * 0.5f) + 0.1f
        let center = (vec3 (rp ()) (y / 10.0f) (rp ())) * 10.0f
        sphere center y (lambertian (vec3 (rng ()) (rng ()) (rng ())) rng)
    )
    ground :: randomSpheres
    
[<EntryPoint>]
let main argv =
    let width = 200
    let height = 100
    let ar = (float32 width) / (float32 height)
    let camera = createCamera (vec3 0.0f 2.0f -4.0f) (vec3 0.0f 0.0f -1.0f) (vec3 0.0f 1.0f 0.0f) 60.0f ar 0.0f 2.0f
    let r = pi / 4.0f
    let world = group randomScene
    let image = new Image<Rgba32>(width, height)
    let time = traceParallel camera world defaultSettings (imageSurface image)
    image.Save("output.png")
    printfn "Rendering: %fms" time.TotalMilliseconds
    do System.Console.ReadKey() |> ignore
    0 // return an integer exit code
