namespace Vega

module Funtional =

    type FieldType = 
        | Int 
        | Float

    type Field<'a> = string * ('a -> float)

    type BuiltInScale<'a> =
        | Width of Field<'a>
        | Height of Field<'a>
        | Category10 of Field<'a>
        | Category20 of Field<'a>

    type ScaleDomain<'a> =
        | Array of float[]
        | Data of ('a -> float)

    type ScaleType<'a> = 
        | BuiltIn of BuiltInScale<'a>
        | Custom of ScaleDomain<'a>

    type Scale<'a> = string * ScaleType<'a>
    
    type obs = { X:float; Y:float; S:float }
    
    let (xScale:Scale<obs>) = "X", BuiltIn(Width("X", fun x -> x.X))
    let (yScale:Scale<obs>) = "Y", BuiltIn(Height("Y", fun x -> x.Y))
    let (scale:Scale<obs>) = "Name", Custom(Array([|1.;2.;3.|]))
    
    // should it receive data extractors / fields as input, too?
    let writeScale (data:seq<'a>) (scales:Scale<'a>[]) =
        scales
        |> Array.map (fun s ->
            let name, scaleType = s 
            match scaleType with
            | BuiltIn(builtIn) ->
                match builtIn with
                | Width(n,f)      -> sprintf """{"name": "%s","range": "width","domain": {"data": "table", "field": "data.%s"}""" name n
                | Height(n,f)     -> sprintf """{"name": "%s","range": "height","domain": {"data": "table", "field": "data.%s"}""" name n
                | Category10(n,f) -> sprintf """{"name": "%s","range": "category10","domain": {"data": "table", "field": "data.%s"}""" name n
                | Category20(n,f) -> sprintf """{"name": "%s","range": "category20","domain": {"data": "table", "field": "data.%s"}""" name n
            | _ -> "Not supported")
    

