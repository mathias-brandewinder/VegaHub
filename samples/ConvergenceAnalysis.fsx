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

//type Est = { Real:float;Pred:float;Obs:int;Group:string }

let fakeAlgo iters (handler: (float*float) -> unit) =

    let theta = [| 0.; 0.; 0.; |]
    let rec update theta i p =
        if i > iters 
        then "Done"
        else
            let prev' = p * (0.8 + 0.2 * rng.NextDouble())
            let upd = i |> float, prev'
            upd |> handler
            let theta' = [| rng.NextDouble(); rng.NextDouble(); rng.NextDouble() |]
            update theta' (i+1) prev'

    let prev = 1.
    update theta 0 prev


            
let disposable = Vega.connect "http://localhost:8081"

let mutable history = [| (0.,0.) ; |]

let handleUpdate update =
        history <- Array.append history [| update |]    
        VegaHub.Templates.scatter history (fun x -> fst x) (fun x -> snd x) (fun x -> "Color") (fun x -> 120.) 


fakeAlgo 100 handleUpdate 

disposable.Dispose()