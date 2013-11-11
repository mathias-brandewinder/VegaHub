#I """..\packages"""
#r """Owin.1.0\lib\net40\Owin.dll"""
#r """Microsoft.Owin.2.0.0-rc1\lib\net45\Microsoft.Owin.dll"""
#r """Microsoft.Owin.FileSystems.0.25.0-pre-21004-655-rel\lib\net40\Microsoft.Owin.FileSystems.dll"""
#r """Microsoft.Owin.Hosting.2.0.0-rc1\lib\net45\Microsoft.Owin.Hosting.dll"""
#r """Microsoft.Owin.Security.2.0.0-rc1\lib\net45\Microsoft.Owin.Security.dll"""
#r """Microsoft.Owin.StaticFiles.0.25.0-pre-21004-655-rel\lib\net40\Microsoft.Owin.StaticFiles.dll"""
#r """Microsoft.Owin.Host.HttpListener.2.0.0-rc1\lib\net45\Microsoft.Owin.Host.HttpListener.dll"""
#r """Newtonsoft.Json.5.0.6\lib\net45\Newtonsoft.Json.dll"""
#r """Microsoft.AspNet.SignalR.Core.2.0.0-rc1\lib\net45\Microsoft.AspNet.SignalR.Core.dll"""
#r """ImpromptuInterface.6.2.2\lib\net40\ImpromptuInterface.dll"""
#r """ImpromptuInterface.FSharp.1.2.13\lib\net40\ImpromptuInterface.FSharp.dll"""
#load "../src/WebApp.fs"
#load "../src/Vega.fs"

open System
open System.IO
open VegaHub
open System

let rng = Random()

type Est = { Real:float; Pred:float; Obs:int; Group:string }

type DataPoint = { Prints: int; Clicks: int; Tokens: bool [] } 

let ctr p = float p.Clicks / float p.Prints

let features = 10
let sampleSize = 99

let dataset =
    [| for i in 0..sampleSize -> 
        let p = rng.Next(1000)
        { Prints = p; 
          Clicks = p / rng.Next(2,10); 
          Tokens = [| for f in 1 .. features -> if rng.Next(2) = 1 then true else false |] } |]

let trainingset = dataset.[0..sampleSize / 2]
let validationset = dataset.[1+sampleSize / 2 ..]
 
let fakeAlgo dataset iters (handler: float [] -> unit) =
    let theta = [| for f in 1 .. features -> 0. |]
    let rec update theta i =
        if i > iters 
        then theta
        else
            let theta' = theta |> Array.map (fun x -> x + 0.01 * (rng.NextDouble() - 0.5))
            theta' |> handler
            update theta' (i+1)

    update theta 0

            
let disposable = Vega.connect "http://localhost:8081"

let fullset = [|
    for t in trainingset -> (t,0)
    for v in validationset -> (v,1) |]

type Point = { X: float; Y: float; C: string; S: float }

let pred (theta: float[]) x =
    (theta, x) ||> Array.map2 (fun x y -> x * y) |> Array.sum

let handleUpdate update =
    let f = pred update
    let plot = 
        fullset 
        |> Array.map (fun (x,i) -> 
            { X = ctr x; Y = f (x.Tokens |> Array.map (fun z -> if z then 1. else 0.)); C = i |> string; S = x.Prints |> float})
    VegaHub.Templates.scatter plot (fun x -> x.X) (fun x -> x.Y) (fun x -> x.C) (fun x -> x.S) 


fakeAlgo trainingset 1000 handleUpdate 

disposable.Dispose()