module FsRaytracer.Tracer

open System.Numerics
open System
open MathExt
open Objects
open FSharp.Collections.ParallelSeq
open System.Diagnostics

type RenderSurface = {
    height: int
    width: int
    setColor: int * int -> Vector3 -> unit
}

type Antialiasing = Off | On of int

type RenderSettings = {
    rngSeed: int option
    antialiasing: Antialiasing
    gammaCorrectColors: bool
    maxReflections: int
}

let defaultSettings = { rngSeed = None; antialiasing = On 100; gammaCorrectColors = true; maxReflections = 50 }

let oneVector = vec3 1.0f 1.0f 1.0f
let colorVector = vec3 0.5f 0.7f 1.0f

let rec color (maxDepth: int) (ray: Ray) (rng: unit -> float32) (world: SceneBody) (depth: int) =
    match world.hit ray 0.001f Single.MaxValue with
        | Some hit ->
            if depth < maxDepth then
                match hit.material ray hit with
                    | Some (attenuation, scattered) -> attenuation * (color maxDepth scattered rng world (depth + 1))
                    | None -> Vector3.Zero
            else
                Vector3.Zero
        | None ->
            let unitDirection = norm ray.direction
            let t = 0.5f * (unitDirection.Y + 1.0f)
            (1.0f - t) * oneVector + (t * colorVector)

let private createRng (seed: int option) =
    let random = match seed with
                    | None -> Random()
                    | Some seed -> Random(seed)
    fun () -> float32 (random.NextDouble ())
 
let createRenderer (w: int) (h: int) (settings: RenderSettings) =
    let colorModification = match settings.gammaCorrectColors with
                                | true -> (fun (oldVec: Vector3) -> vec3 (sqrt oldVec.X) (sqrt oldVec.Y) (sqrt oldVec.Z))
                                | false -> id
    let color = color settings.maxReflections
    match settings.antialiasing with
        | Off ->
            let w = float32 w
            let h = float32 h
            let rng = createRng settings.rngSeed
            let render (world: SceneBody) (camera: Camera) (x: int) (y: int) =
                let u = (float32 x) / w
                let v = (float32 y) / h
                let ray = castRay camera u v
                colorModification (color ray rng world 0)
            render
        | On level ->
            let w = float32 w
            let h = float32 h
            let rng = createRng settings.rngSeed
            let render (world: SceneBody) (camera: Camera) (x: int) (y: int) =
                (*let combinedColor = [ 0..level ]
                                    |> PSeq.map (fun _ -> 
                                        let u = (float32 x + rng ()) / w
                                        let v = (float32 y + rng ()) / h
                                        let ray = castRay camera u v
                                        color ray rng world 0)
                                    |> PSeq.fold (fun v n -> v + n) Vector3.Zero
                                    |> div (float32 level)*)
                let combinedColor = [ 0..level ] 
                                    |> Seq.fold (fun c _ ->
                                    let u = (float32 x + rng ()) / w
                                    let v = (float32 y + rng ()) / h
                                    let ray = castRay camera u v
                                    c + color ray rng world 0) Vector3.Zero
                
                colorModification (div (float32 level) combinedColor)
            render

let trace (camera: Camera) (world: SceneBody) (settings: RenderSettings) (surface: RenderSurface) =
    let { height = height; width = width; setColor = setColor } = surface
    let renderer = createRenderer width height settings
    let sw = Stopwatch()
    do sw.Start()

    for y = (height - 1) downto 0 do
        for x = 0 to (width - 1) do
            let col = renderer world camera x y
            setColor (x, y) col
        
    do sw.Stop()
    sw.Elapsed