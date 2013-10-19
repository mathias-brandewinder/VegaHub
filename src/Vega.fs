namespace VegaHub

open System
open System.Collections.Generic
open System.Diagnostics
open Microsoft.AspNet.SignalR
open Newtonsoft.Json
open ImpromptuInterface.FSharp
open VegaHub

(**
 * Vega Grammar
 *)

type Padding() =
    member val Top    : int = 0 with get,set
    member val Left   : int = 0 with get,set
    member val Bottom : int = 0 with get,set
    member val Right  : int = 0 with get,set

type Point() =
    member val X : int = 0 with get,set
    member val Y : int = 0 with get,set

type Data() =
    member val Name   : string  = null with get,set
    member val Values : Point[] = null with get,set

type ScaleDomain() =
    member val Data  : string = null with get,set
    member val Field : string = null with get,set

type Scale() =
    member val Name   : string = null with get,set
    member val Range  : string = null with get,set
    member val Domain : ScaleDomain = ScaleDomain() with get,set
    member val Nice   : bool = false with get,set
    member val Type   : string = null with get,set
    member val Zero   : bool = true with get,set

type Axis() =
    member val Type  : string = null with get,set
    member val Scale : string = null with get,set
    member val Ticks : int = 0 with get,set

type MarkFrom() =
    member val Data : string = null with get,set

type Mark() =
    member val Type : string = null with get,set
    member val From : MarkFrom = MarkFrom() with get,set
    member val Properties : IDictionary<string, obj> = null with get,set // TODO: Create a typed wrapper with property accessors

type Spec() =
    member val Name    : string = null with get,set
    member val Width   : int = 0 with get,set
    member val Height  : int = 0 with get,set
    member val Padding : Padding = Padding() with get,set
    member val Data    : Data[] = null with get,set
    member val Scales  : Scale[] = null with get,set
    member val Axes    : Axis[] = null with get,set
    member val Marks   : Mark[] = null with get,set


(**
 * Serializer helpers
 *)

module internal Serialization =
    let private settings =
        JsonSerializerSettings(ContractResolver = Serialization.CamelCasePropertyNamesContractResolver(), NullValueHandling = NullValueHandling.Ignore)

    let deserialize spec =
        JsonConvert.DeserializeObject<Spec>(spec, settings)

    let serialize spec =
        JsonConvert.SerializeObject(spec, settings)


(**
 * Vega Templates
 *)

module Templates =
//    let arc =
//        Serialization.deserialize """{
//  "name": "arc",
//  "width": 400,
//  "height": 400,
//  "data": [
//    {
//      "name": "table",
//      "values": [12, 23, 47, 6, 52, 19],
//      "transform": [
//        {"type": "pie", "value": "data"}
//      ]
//    }
//  ],
//  "scales": [
//    {
//      "name": "r",
//      "type": "sqrt",
//      "domain": {"data": "table", "field": "data"},
//      "range": [20, 100]
//    }
//  ],
//  "marks": [
//    {
//      "type": "arc",
//      "from": {"data": "table"},
//      "properties": {
//        "enter": {
//          "x": {"group": "width", "mult": 0.5},
//          "y": {"group": "height", "mult": 0.5},
//          "startAngle": {"field": "startAngle"},
//          "endAngle": {"field": "endAngle"},
//          "innerRadius": {"value": 20},
//          "outerRadius": {"scale": "r"},
//          "stroke": {"value": "#fff"}
//        },
//        "update": {
//          "fill": {"value": "#ccc"}
//        },
//        "hover": {
//          "fill": {"value": "pink"}
//        }
//      }
//    }
//  ]
//}"""

    let area =
        Serialization.deserialize """{
  "width": 500,
  "height": 200,
  "padding": {"top": 10, "left": 30, "bottom": 30, "right": 10},
  "data": [
    {
      "name": "table",
      "values": [
        {"x": 1,  "y": 28}, {"x": 2,  "y": 55},
        {"x": 3,  "y": 43}, {"x": 4,  "y": 91},
        {"x": 5,  "y": 81}, {"x": 6,  "y": 53},
        {"x": 7,  "y": 19}, {"x": 8,  "y": 87},
        {"x": 9,  "y": 52}, {"x": 10, "y": 48},
        {"x": 11, "y": 24}, {"x": 12, "y": 49},
        {"x": 13, "y": 87}, {"x": 14, "y": 66},
        {"x": 15, "y": 17}, {"x": 16, "y": 27},
        {"x": 17, "y": 68}, {"x": 18, "y": 16},
        {"x": 19, "y": 49}, {"x": 20, "y": 15}
      ]
    }
  ],
  "scales": [
    {
      "name": "x",
      "type": "linear",
      "range": "width",
      "zero": false,
      "domain": {"data": "table", "field": "data.x"}
    },
    {
      "name": "y",
      "type": "linear",
      "range": "height",
      "nice": true,
      "domain": {"data": "table", "field": "data.y"}
    }
  ],
  "axes": [
    {"type": "x", "scale": "x", "ticks": 20},
    {"type": "y", "scale": "y"}
  ],
  "marks": [
    {
      "type": "area",
      "from": {"data": "table"},
      "properties": {
        "enter": {
          "interpolate": {"value": "monotone"},
          "x": {"scale": "x", "field": "data.x"},
          "y": {"scale": "y", "field": "data.y"},
          "y2": {"scale": "y", "value": 0},
          "fill": {"value": "steelblue"}
        },
        "update": {
          "fillOpacity": {"value": 1}
        },
        "hover": {
          "fillOpacity": {"value": 0.5}
        }
      }
    }
  ]
}"""
        
    let bar =
        Serialization.deserialize """{
  "width": 400,
  "height": 200,
  "padding": {"top": 10, "left": 30, "bottom": 30, "right": 10},
  "data": [
    {
      "name": "table",
      "values": [
        {"x": 1,  "y": 28}, {"x": 2,  "y": 55},
        {"x": 3,  "y": 43}, {"x": 4,  "y": 91},
        {"x": 5,  "y": 81}, {"x": 6,  "y": 53},
        {"x": 7,  "y": 19}, {"x": 8,  "y": 87},
        {"x": 9,  "y": 52}, {"x": 10, "y": 48},
        {"x": 11, "y": 24}, {"x": 12, "y": 49},
        {"x": 13, "y": 87}, {"x": 14, "y": 66},
        {"x": 15, "y": 17}, {"x": 16, "y": 27},
        {"x": 17, "y": 68}, {"x": 18, "y": 16},
        {"x": 19, "y": 49}, {"x": 20, "y": 15}
      ]
    }
  ],
  "scales": [
    {
      "name": "x",
      "type": "ordinal",
      "range": "width",
      "domain": {"data": "table", "field": "data.x"}
    },
    {
      "name": "y",
      "range": "height",
      "nice": true,
      "domain": {"data": "table", "field": "data.y"}
    }
  ],
  "axes": [
    {"type": "x", "scale": "x"},
    {"type": "y", "scale": "y"}
  ],
  "marks": [
    {
      "type": "rect",
      "from": {"data": "table"},
      "properties": {
        "enter": {
          "x": {"scale": "x", "field": "data.x"},
          "width": {"scale": "x", "band": true, "offset": -1},
          "y": {"scale": "y", "field": "data.y"},
          "y2": {"scale": "y", "value": 0}
        },
        "update": {
          "fill": {"value": "steelblue"}
        },
        "hover": {
          "fill": {"value": "red"}
        }
      }
    }
  ]
}"""
    
    // Generate i random colors - needs improvement
    let private randomColors i = 
        let rng = Random(42)
        [ for i in 0 .. i -> sprintf "\"#%i%i%i\"" (rng.Next(10)) (rng.Next(10)) (rng.Next(10)) ]
        |> fun a -> System.String.Join (", ", a)

    // Create palette based on # of cases
    let private paletize<'a,'b when 'b:equality> (data: 'a seq) (categorize: 'a -> 'b)  =
        let cols = data |> Seq.distinctBy categorize |> Seq.length
        if cols <= 10 then "\"category10\""
        elif cols <= 20 then "\"category20\""
        else sprintf "[%s]" (randomColors cols)

    // Transform dataset to map predefined expected format
    let private prepareScatter<'a,'b>  (data: 'a seq) 
                               (fx: 'a -> float) // X coord
                               (fy: 'a -> float) // Y coord
                               (fc: 'a -> 'b) // color category
                               (fs: 'a -> float) = // size
        data
        |> Seq.map (fun a -> sprintf """{"x": %f,"y": %f,"c": %A, "s": %f}""" (fx a) (fy a) (fc a) (fs a))
        |> fun a -> System.String.Join (",", a)

    let private scatterTemplate data fx fy fc fs = 
        let dataString = prepareScatter data fx fy fc fs
        let palette = paletize data fc
        sprintf """
    {
      "width": 400,
      "height": 400,
      "data": [
        {
          "name": "table",
          "values": [ %s ]
        }
      ],
      "scales": [
        {
          "name": "x",
          "nice": true,
          "range": "width",
          "domain": {"data": "table", "field": "data.x"}
        },
        {
          "name": "y",
          "nice": true,
          "range": "height",
          "domain": {"data": "table", "field": "data.y"}
        },
        {
          "name": "c",
          "type": "ordinal",
          "domain": {"data": "table", "field": "data.c"},
          "range": %s
        }  
      ],
      "axes": [
        {"type": "x", "scale": "x"},
        {"type": "y", "scale": "y"}
      ],
      "marks": [
        {
          "type": "symbol",
          "from": {"data": "table"},
          "properties": {
            "enter": {
              "x": {"scale": "x", "field": "data.x"},
              "y": {"scale": "y", "field": "data.y"},
              "fill": {"scale": "c", "field": "data.c"},
              "size": {"field": "data.s"},
              "fillOpacity": {"value": 0.5}
            }
          }
        }
      ]
    }""" dataString palette

    // older version, directly sending spec as JSON
    let private sendSpec (spec: string) (hub: IHubContext) : unit =
        hub.Clients.All?parse spec 

    let scatter data fx fy fc fs = 
        GlobalHost.ConnectionManager.GetHubContext<WebApp.ChartHub>()
        |> sendSpec (scatterTemplate data fx fy fc fs)

(**
 * Vega Core
 *)

module Vega =
    /// Launch the default web browser and connect SignalR using the specified url.
    let connect url =
        let disposable = WebApp.launch url
        Console.WriteLine("Running chart hub on " + url)
        // TODO: Use canopy?
        Process.Start(url + "/index.html") |> ignore
        disposable

    /// Send the spec to the Vega browser client via SignalR.
    let send (spec: Spec) : unit = 
        let hub = GlobalHost.ConnectionManager.GetHubContext<WebApp.ChartHub>()
        hub.Clients.All?parse (Serialization.serialize spec)
