namespace Vega

module Json =

    type Obj = Prop list
    and Prop =
    | Val of (string * string)
    | NVal of (string * float)
    | Nested of string * Obj
    | List of string * Obj list

    let rec writeObj obj =
        obj 
        |> List.map writeProp
        |> String.concat ","
        |> sprintf "{%s}"            
    and private writeProp prop =
        match prop with
        | Val(key,value)  -> sprintf "\"%s\":\"%s\"" key value
        | NVal(key,value) -> sprintf "\"%s\":%f" key value
        | Nested(key,obj) -> sprintf "\"%s\":%s" key (writeObj obj) 
        | List(key,list)  -> sprintf "\"%s\":[%s]" key (list |> List.map writeObj |> String.concat ",")
    
    let test =
        [   Val("Width","400");
            NVal("Height",300.0);
            List("Axes",
                [   [ Nested("x", [Val("field","data.x")]) ];
                    [ Nested("y", [Val("field","data.y")]) ];
                ])
        ]
                 
module Functional = 

    open Json

    type DataType = 
        | Numeric
        | Categorical

    type Num<'a> = ('a -> float)
    type Cat<'a> = ('a -> string)

    type Feature<'a> = 
        | Numeric of string * Num<'a>
        | Categorical of string * Cat<'a>

    type Datasource<'a> = 
        | NumericValue of float
        | CategoricalValue of string
        | Field of Feature<'a>

    let writeSource source =
        match source with
        | NumericValue(x) -> "value", string x
        | CategoricalValue(x) -> "value", string x
        | Field(feature) -> 
            match feature with
            | Numeric(name,feat) -> "field", "data." + name
            | Categorical(name,feat) -> "field", "data." + name
           
    type Scale<'a> =
        | Width of string * Feature<'a>
        | Height of string * Feature<'a>

    let writeScale scale = 
        match scale with 
        | Width(name,feature) -> 
            match feature with
            | Numeric(n,_) -> [ Val("name",name); Val("range","width"); Nested("domain",[Val("data","table");Val("field","data."+n)]) ]
            | Categorical(n,_) -> [ Val("name",name); Val("type","ordinal"); Val("range","width"); Nested("domain",[Val("data","table");Val("field","data."+n)]) ]
        | Height(name,feature) -> 
            match feature with
            | Numeric(n,_) -> [ Val("name",name); Val("range","height"); Nested("domain",[Val("data","table");Val("field","data."+n)]) ]
            | Categorical(n,_) -> [ Val("name",name); Val("type","ordinal"); Val("range","height"); Nested("domain",[Val("data","table");Val("field","data."+n)]) ]

    type Axes<'a> = { XAxis: Scale<'a>; YAxis: Scale<'a> } 

    let writeAxes axes =
        let x = 
            match (axes.XAxis) with
            | Width(name,feature) -> [ Val("type","x"); Val("scale",name) ]
            | _                   -> failwith "X axis should match width"   
        let y = 
            match (axes.YAxis) with
            | Height(name,feature) -> [ Val("type","y"); Val("scale",name)]
            | _                    -> failwith "Y axis should match height"   
        List ("axes", [ x; y ])

    type Point<'a> = 
        {   XScale:Scale<'a>;
            XSource:Datasource<'a>;
            YScale:Scale<'a>;
            YSource:Datasource<'a> }
        
    type Mark<'a> = 
        | Symbol of Point<'a>

    let prepareSymbol (point:Point<'a>) =
        let xs = Nested("x", [Val("scale","X");Val(writeSource point.XSource)])
        let ys = Nested("y", [Val("scale","Y");Val(writeSource point.YSource)])
        let color = Nested("fill",[Val("value","steelblue")])
        let size = Nested("size",[Val("value","100")])
        let enter = Nested("enter",[xs;ys;color;size;])
        enter

    let render mark = 
        match mark with
        | Symbol(point) -> 
            [   Val("type","symbol");
                Nested("from", [ Val("data","table") ]);
                Nested("properties", [ prepareSymbol point ])
            ]        

    let writeItem a extractors =
        extractors
        |> List.map (fun ext ->
            match ext with
            | Numeric(name,func) -> Val(name,string(func a))
            | Categorical(name,func) -> Val(name,func a))
                  
    let writeData dataset extractors =
        let name = Val("name","table"); 
        let values = List("values", dataset |> List.map (fun x -> writeItem x extractors))
        List("data", [ [ name; values] ] ) 

module Demo =

    open Json
    open Functional

    type obs = { First:float; Second:float; }

    let test () =

        let rng = System.Random()
        let dataset = 
            [ for i in 1 .. 10 -> { First = rng.NextDouble(); Second = rng.NextDouble() } ]
                                     
        let scatterplot dataset (fx, fy) =

            let xs = Numeric("fst", fx)
            let ys = Numeric("snd", fy)
     
            let xScale = Width ("X", xs)
            let yScale = Height ("Y", ys)

            let point = 
                {   XScale = xScale;
                    XSource = Field(xs);
                    YScale = yScale;
                    YSource = Field(ys) }

            let axes = { XAxis = xScale; YAxis = yScale }

            let mark = Symbol(point)

            let template = 
                [   NVal("width",400.);
                    NVal("height",400.);
                    writeData dataset [xs;ys];
                    List ("scales", [ writeScale xScale; writeScale yScale ]);
                    writeAxes axes;
                    List ("marks", [ render mark ])
                ]

            writeObj template

        scatterplot dataset ((fun x -> x.First), (fun x -> x.Second))