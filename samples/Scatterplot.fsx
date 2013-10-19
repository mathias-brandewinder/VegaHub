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
                (fun x -> x.PetalLength) 
                (fun x -> x.PetalWidth) 
                (fun x -> x.Class)
                (fun x -> 100.)

disposable.Dispose()