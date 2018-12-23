module FsRaytracer.Tracer

open TracerData
open System.Numerics
open System
open MathExt
open Scene

type RenderSurface = {
    height: int
    width: int
    setColor: int * int -> Vector3 -> unit
}

type RenderSettings = {
    rngSeed: int option
}

let defaultSettings = { rngSeed = None }

let oneVector = vec3 1.0f 1.0f 1.0f
let colorVector = vec3 0.5f 0.7f 1.0f

let colorRed = vec3 1.0f 0.0f 0.0f

let aaLevel = 100

let color (ray: Ray) (world: SceneObject) =
    match hit ray 0.0f Single.MaxValue world with
        | None ->
            let unitDirection = norm ray.direction
            let t = 0.5f * (unitDirection.Y + 1.0f)
            (1.0f - t) * oneVector + (t * colorVector)
        | Some hit ->
            0.5f * (hit.normal + oneVector)

let private createRng (seed: int option) =
    let random = match seed with
                    | None -> Random()
                    | Some seed -> Random(seed)
    fun () -> float32 (random.NextDouble ())
 
let trace (camera: Camera) (world: SceneObject) (settings: RenderSettings) (surface: RenderSurface) =
    let rng = createRng settings.rngSeed
    let { height = height; width = width; setColor = setColor } = surface
    let h = float32(height)
    let w = float32(width)

    for j = (height - 1) downto 0 do
        for i = 0 to (width - 1) do
            let fullColor = Seq.fold (fun c _ ->
                let u = (float32(i) + rng ()) / w
                let v = (float32(j) + rng ()) / h
                let ray = castRay camera u v
                c + color ray world) Vector3.Zero [ 0..aaLevel ]
                
            let col = div (float32 aaLevel) fullColor
            setColor (i, j) col

    surface