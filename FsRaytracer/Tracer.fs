module FsRaytracer.Tracer

open System.Numerics
open System
open MathExt
open Objects

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
}

let defaultSettings = { rngSeed = None; antialiasing = On 100; gammaCorrectColors = true }

let oneVector = vec3 1.0f 1.0f 1.0f
let colorVector = vec3 0.5f 0.7f 1.0f

let rec color (ray: Ray) (rng: unit -> float32) (world: SceneBody) =
    match world.hit ray 0.001f Single.MaxValue with
        | None ->
            let unitDirection = norm ray.direction
            let t = 0.5f * (unitDirection.Y + 1.0f)
            (1.0f - t) * oneVector + (t * colorVector)
        | Some hit ->
            let target = hit.position + hit.normal + (randomInUnitSphere rng)
            0.5f * (color (makeRay hit.position (target - hit.position)) rng world)

let private createRng (seed: int option) =
    let random = match seed with
                    | None -> Random()
                    | Some seed -> Random(seed)
    fun () -> float32 (random.NextDouble ())
 
let createRenderer (w: int) (h: int) (settings: RenderSettings) =
    let colorModification = match settings.gammaCorrectColors with
                                | true -> (fun (oldVec: Vector3) -> vec3 (sqrt oldVec.X) (sqrt oldVec.Y) (sqrt oldVec.Z))
                                | false -> id
    match settings.antialiasing with
        | Off ->
            let w = float32 w
            let h = float32 h
            let rng = createRng settings.rngSeed
            let render (world: SceneBody) (camera: Camera) (x: int) (y: int) =
                let u = (float32 x) / w
                let v = (float32 y) / h
                let ray = castRay camera u v
                colorModification (color ray rng world)
            render
        | On level ->
            let w = float32 w
            let h = float32 h
            let rng = createRng settings.rngSeed
            let render (world: SceneBody) (camera: Camera) (x: int) (y: int) =
                let fullColor = [ 0..level ] 
                                |> Seq.fold (fun c _ ->
                                    let u = (float32 x + rng ()) / w
                                    let v = (float32 y + rng ()) / h
                                    let ray = castRay camera u v
                                    c + color ray rng world) Vector3.Zero
                
                colorModification (div (float32 level) fullColor)
            render

let trace (camera: Camera) (world: SceneBody) (settings: RenderSettings) (surface: RenderSurface) =
    let { height = height; width = width; setColor = setColor } = surface
    let renderer = createRenderer width height settings

    for y = (height - 1) downto 0 do
        for x = 0 to (width - 1) do
            let col = renderer world camera x y
            setColor (x, y) col
        
    surface