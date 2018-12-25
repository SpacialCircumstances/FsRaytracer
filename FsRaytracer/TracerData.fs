﻿module FsRaytracer.TracerData

open System.Numerics
open MathExt
open Scene
open Objects

let inline makeRay (origin: Vector3) (direction: Vector3) = { origin = origin; direction = direction }

let inline castRay (camera: Camera) (u: float32) (v: float32) =
    makeRay camera.origin (camera.lowerLeftCorner + (mul u camera.horizontal) + (mul v camera.vertical) - camera.origin)

let rec hit (ray: Ray) (tmin: float32) (tmax: float32) (body: SceneObject) =
    match body with
        | Sphere { center = center; radius = radius } ->
            let isHit t =
                if t < tmax && t > tmin then
                    let pos = (calculatePosition ray t)
                    Some { parameter = t; position = pos; normal = (pos - center) / radius }
                else
                    None

            let oc = ray.origin - center
            let a = dotP ray.direction ray.direction
            let b = 2.0f * (dotP oc ray.direction)
            let c = (dotP oc oc) - (radius * radius)
            let discriminant = b * b - 4.0f * a * c
            if discriminant < 0.0f then
                None
            else
                isHit ((-b - (sqrt discriminant)) / (2.0f * a))

        | Objects objects ->
            let hitFound, _ = Seq.fold (fun (currentHit, closest) o ->
                                let hitThis = hit ray tmin closest o
                                match hitThis with
                                    | None -> (currentHit, closest)
                                    | Some newHit -> (Some newHit, newHit.parameter)
                                ) (None, tmax) objects
            hitFound