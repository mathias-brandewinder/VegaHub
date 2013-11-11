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

let datapath = __SOURCE_DIRECTORY__ + @"\iris.data"

type Observation = { 
    SepalLength: float;
    SepalWidth: float;
    PetalLength: float;
    PetalWidth: float;
    Class: string; }

let data =
    File.ReadAllLines(datapath)
    |> Array.map (fun line -> line.Split(','))
    |> Array.map (fun line -> 
        {   SepalLength = line.[0] |> float;
            SepalWidth = line.[1] |> float;
            PetalLength = line.[2] |> float;
            PetalWidth = line.[3] |> float;
            Class = line.[4]; })

let disposable = Vega.connect "http://localhost:8081"

VegaHub.Templates.scatter data
                (fun x -> x.PetalWidth) 
                (fun x -> x.SepalLength) 
                (fun x -> x.Class)
                (fun x -> 200.)

// Clustering Algorithm

// Assign each point to a Centroid
let assign centroids points dist =
    points 
    |> Array.map (fun (x, c) ->
        x, 
        centroids |> Array.minBy (fun (y, _) -> dist x y) |> snd)

// Compute centroids based on clusters
let update centroids points reduce =
    centroids
    |> Array.map (fun (x, c) ->
        points 
        |> Array.filter (fun (_, i) -> i = c) 
        |> Array.map (fun (x, _) -> x) 
        |> reduce, c)

// recursively update
let clusterize (data: 'a []) dist red k (handler: ('a * int) [] -> unit)=    
    let centroids = [| 1 .. k |] |> Array.map (fun c -> data.[c], c)
    let points = data |> Array.map (fun x -> x, centroids |> Array.minBy (fun (c,i) -> dist c x) |> snd)
    let iters = 20
    let rec search cs ps i =
        let ps' = assign cs ps dist
        let cs' = update cs ps' red
        handler (Array.append ps' cs')
        if i > iters && (ps |> Array.map snd) = (ps' |> Array.map snd)
        then cs'
        else search cs' ps' (i+1)
    search centroids points 0

// End clustering

let distance p1 p2 =
    (p1.SepalWidth - p2.SepalWidth) ** 2. +
    (p1.SepalLength - p2.SepalLength) ** 2. +
    (p1.PetalWidth - p2.PetalWidth) ** 2. +
    (p1.PetalLength - p2.PetalLength) ** 2.

let reducer ps =
    { SepalWidth = ps |> Seq.averageBy (fun p -> p.SepalWidth);
      SepalLength = ps |> Seq.averageBy (fun p -> p.SepalLength);
      PetalWidth = ps |> Seq.averageBy (fun p -> p.PetalWidth);
      PetalLength = ps  |> Seq.averageBy (fun p -> p.PetalLength);
      Class = "Centroid" }

let handleUpdate (update: (Observation * int) []) =
    VegaHub.Templates.scatter 
        update
        (fun (x,y) -> x.PetalLength) 
        (fun (x,y) -> x.PetalWidth) 
        (fun (x,y) -> if x.Class = "Centroid" then 4 else y) 
        (fun (x,y) -> if x.Class = "Centroid" then 300. else 100.)

let test = clusterize data distance reducer 3 handleUpdate

disposable.Dispose()