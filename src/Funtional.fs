namespace Vega

module Funtional =

    type Feature<'a> = string * ('a -> float)

    let writeData (data: 'a seq) (feats: Feature<'a> list) =
        let values = 
            data 
            |> Seq.map (fun x -> 
                feats 
                |> List.map (fun (n,f) -> sprintf """"%s":%f""" n (f x))
                |> String.concat ","
                |> fun item -> sprintf """{%s}""" item)
            |> String.concat ",\n"
                
        sprintf """"data": [
                      {
                        "name": "table",
                        "values": [ %s ]
                      }
                    ]""" values


    type BuiltInScale<'a> =
        | Width of Feature<'a>
        | Height of Feature<'a>
        | Category10 of Feature<'a>
        | Category20 of Feature<'a>

    type ScaleDomain<'a> =
        | Array of float[]
        | Data of ('a -> float)

    type ScaleType<'a> = 
        | BuiltIn of BuiltInScale<'a>
        | Custom of ScaleDomain<'a>

    type Scale<'a> = string * ScaleType<'a>
      
    // should it receive data extractors / Features as input, too?
    let writeScale (scale:Scale<'a>) =
        let name, scaleType = scale 
        match scaleType with
        | BuiltIn(builtIn) ->
            match builtIn with
            | Width(n,f)      -> sprintf """{"name": "%s","range": "width","domain": {"data": "table", "field": "data.%s"}}""" name n
            | Height(n,f)     -> sprintf """{"name": "%s","range": "height","domain": {"data": "table", "field": "data.%s"}}""" name n
            | Category10(n,f) -> sprintf """{"name": "%s","range": "category10","domain": {"data": "table", "field": "data.%s"}}""" name n
            | Category20(n,f) -> sprintf """{"name": "%s","range": "category20","domain": {"data": "table", "field": "data.%s"}}""" name n
        | _ -> "Not supported"

    let writeScales (scales:Scale<'a> list) = 
        let values =
            scales 
            |> Seq.map writeScale 
            |> String.concat ",\n"
        sprintf """"scales": [ %s ]""" values

    type Axes<'a> = { X: Scale<'a>; Y: Scale<'a> }

    let writeAxes axes =
        let (xName, _) = axes.X
        let (yName, _) = axes.Y
        sprintf """ "axes": [ {"type": "x", "scale": "%s"}, {"type": "y", "scale": "%s"}]""" xName yName

    type Point<'a> = Scale<'a> * Scale<'a>

    type Property =
        | Enter
        | Exit
        | Update
        | Hover

    type Mark<'a> =
        | Rect
        | Symbol of (Property * Point<'a>)
        | Path
        | Arc
        | Area
        | Line
        | Image
        | Text

    let writePointProperty (prop:Property, details:Point<'a>) =
        match prop with
        | Enter  -> 
            let (xname, x), (yname, y) = details
            let (fx,_) =
                match x with
                | BuiltIn(scale) ->
                    match scale with
                    | Width(f) -> f
                    | Height(f) -> f
                    | _ -> failwith "not supported"
                | _ -> failwith "not supported"
            let (fy,_) =
                match y with
                | BuiltIn(scale) ->
                    match scale with
                    | Width(f) -> f
                    | Height(f) -> f
                    | _ -> failwith "not supported"
                | _ -> failwith "not supported"
            sprintf """"enter": {"x": {"scale": "%s", "field": "data.%s"},"y": {"scale": "%s", "field": "data.%s"},"fill": "steelblue"}""" fx xname fy yname       
        | Exit   -> failwith "not supported"
        | Update -> failwith "not supported"
        | Hover  -> failwith "not supported"

    let writeMark (mark: Mark<'a>) =
        match mark with 
        | Symbol(prop,point) -> 
            let properties = writePointProperty (prop,point)
            sprintf """{"type": "symbol","from": {"data": "table"},"properties": {%s}}""" properties
        | _ -> failwith "Not supported"

    let writeMarks (marks: Mark<'a> seq) =
        let values =
            marks 
            |> Seq.map writeMark 
            |> String.concat ",\n"
        sprintf """"marks": [ %s ]""" values

    (*
    Example of ScatterPlot: manual
    *)

    type obs = { First:float; Second:float; }
    let rng = System.Random()
    let dataset = [ for i in 1 .. 20 -> { First = rng.NextDouble(); Second = rng.NextDouble() } ]
        
    let xs = "X", fun x -> x.First
    let ys = "Y", fun x -> x.Second
     
    let xScale = "X", BuiltIn(Width(xs))
    let yScale = "Y", BuiltIn(Height(ys))

    let axes = { X = xScale; Y = yScale }

    let mark = Symbol(Enter, (xScale, yScale))

    let chart = 
        [
            """"width": 400, "height": 400""";
            (writeData dataset [xs;ys;]);
            (writeScales [xScale;yScale;]);
            (writeAxes axes);
            (writeMarks [mark]);]
        |> String.concat ","
        |> fun body -> sprintf "{%s}" body

    (*
    Example of ScatterPlot: defining a template
    *)

    let scatterplot dataset (fx, fy) =
        let xs = "X", fx
        let ys = "Y", fy
     
        let xScale = "X", BuiltIn(Width(xs))
        let yScale = "Y", BuiltIn(Height(ys))

        let axes = { X = xScale; Y = yScale }

        let mark = Symbol(Enter, (xScale, yScale))

        [
            """"width": 400, "height": 400""";
            (writeData dataset [xs;ys;]);
            (writeScales [xScale;yScale;]);
            (writeAxes axes);
            (writeMarks [mark]);]
        |> String.concat ","
        |> fun body -> sprintf "{%s}" body
        