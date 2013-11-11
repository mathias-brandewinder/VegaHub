namespace Vega

module Json =

    type Obj = Prop list
    and Prop =
    | Val of (string * string) // string property
    | NVal of (string * float) // numeric property
    | Nested of (string * Obj) // object property
    | List of (string * Obj list) // list of objects
    | VList of (string * string list) // raw list of strings

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
        | VList(key,list) -> sprintf "\"%s\":[%s]" key (list |> List.map (fun x -> sprintf "\"%s\"" x) |> String.concat ",")
    let test =
        [   Val("Width","400");
            NVal("Height",300.0);
            List("Axes",
                [   [ Nested("x", [Val("field","data.x")]) ];
                    [ Nested("y", [Val("field","data.y")]) ];
                ]);
            VList("Raw",["A";"B";"C"]);
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

    let featureName f = 
        match f with
        | Numeric(n,_) -> n
        | Categorical(n,_) -> n

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

    type Range =
        | Width
        | Height
        | Color10
        | Color20
//        | Array of string list
                               
    type Scale<'a> = string * Range * Feature<'a> // Feature == Domain ?

    let private scaleName (scale:Scale<'a>) = 
        let (name,_,_) = scale
        name

    let writeScale (scale:Scale<'a>) = 
        let name, range, domain = scale
        let range =
            match range with
            | Width -> Val("range","width")
            | Height -> Val("range","height")
            | Color10 -> Val("range","category10")
            | Color20 -> Val("range","category20")
        let (domain, featType) =
            match domain with
            | Numeric(n,_)     -> Nested("domain",[Val("data","table");Val("field","data."+n)]), Val("type","linear")
            | Categorical(n,_) -> Nested("domain",[Val("data","table");Val("field","data."+n)]), Val("type","ordinal")
        [   Val("name",name); 
            featType;
            range; 
            domain; ]

    type AxisValues = string list
    type Axes<'a> = { XAxis: Scale<'a> * AxisValues option; YAxis: Scale<'a> * AxisValues option} 

    let writeAxes axes =
        let x = 
            let (name,scaleType,feature),values = axes.XAxis
            match scaleType with
            | Width -> 
                let basic = [ Val("type","x"); Val("scale",name); ]
                match values with
                | None -> basic
                | Some(v) -> List.append basic [VList("values",v)]
            | _     -> failwith "X axis should match width"   
        let y = 
            let (name,scaleType,feature),values = axes.YAxis
            match scaleType with
            | Height -> 
                let basic = [ Val("type","y"); Val("scale",name); ]
                match values with
                | None -> basic
                | Some(v) -> List.append basic [VList("values",v)]
            | _      -> failwith "Y axis should match height"   
        List ("axes", [ x; y ])

    type Point<'a> = 
        {   XScale:Scale<'a>;
            XSource:Datasource<'a>;
            YScale:Scale<'a>;
            YSource:Datasource<'a> }

    type RectangleSideLength<'a> =
        | Band
        | Length of Datasource<'a>

    type RectangleSide<'a> =
        | Absolute of Datasource<'a> * Datasource<'a>
        | Relative of Datasource<'a> * RectangleSideLength<'a>

    type Rectangle<'a> =
        {   XScale:Scale<'a>;
            XSide:RectangleSide<'a>;
            YScale:Scale<'a>;
            YSide:RectangleSide<'a>;
        }
    
    type Color<'a> = 
        | Fixed of string // should enforce "color" string
        | Dynamic of Scale<'a> * Feature<'a> // should enforce categorical?

    let writeColor c =
        match c with 
        | Fixed(color) -> [Val("value",color)]
        | Dynamic(scale,feature) -> 
            let scalename = scaleName scale
            let fieldname = featureName feature
            [Val("scale",scalename);Val("field","data."+fieldname)]

    type SharedDecoration<'a> = {
        Fill: Color<'a> }

    type PointDecoration<'a> = {
        Size: Datasource<'a> }
         
    type Mark<'a> = 
        | Symbol of Point<'a> * SharedDecoration<'a> * PointDecoration<'a>
        | Rectangle of Rectangle<'a> * SharedDecoration<'a>
        | Text of Point<'a> * Datasource<'a>
        | Path

    let prepareSymbol (point:Point<'a>, decoration:SharedDecoration<'a>, pointDecoration:PointDecoration<'a>) =

        let xName = scaleName point.XScale
        let yName = scaleName point.YScale
        let c = decoration.Fill

        let xs = Nested("x", [Val("scale",xName);Val(writeSource point.XSource)])
        let ys = Nested("y", [Val("scale",yName);Val(writeSource point.YSource)])
        let color = Nested("fill",writeColor c)
        let size = Nested("size",[Val(writeSource pointDecoration.Size)])
        let enter = Nested("enter",[xs;ys;color;size;])
        enter

    let prepareRectangle (rect:Rectangle<'a>, decoration:SharedDecoration<'a>) =

        let xName = scaleName rect.XScale
        let yName = scaleName rect.YScale
        let c = decoration.Fill

        let xs = rect.XSide
        let x1,x2 = 
            match xs with
            | Absolute(x1,x2) -> Nested("x", [Val("scale",xName);Val(writeSource x1)]), Nested("x2", [Val("scale",xName);Val(writeSource x2)])
            | Relative(x1,l1) -> Nested("x", [Val("scale",xName);Val(writeSource x1)]), Nested("width", [Val("scale",xName);Val("band","true");Val("offset","-1")])
        let ys = rect.YSide
        let y1,y2 =
            match ys with
            | Absolute(y1,y2) -> Nested("y", [Val("scale",yName);Val(writeSource y1)]), Nested("y2", [Val("scale",yName);Val(writeSource y2)])
            | Relative(y1,l2) -> Nested("y", [Val("scale",yName);Val(writeSource y1)]), Nested("height", [Val("scale",yName);Val("band","true");Val("offset","-1")])
        let color = Nested("fill", writeColor c)
        let enter = Nested("enter", [x1;x2;y1;y2;color;])

        enter

    let prepareText (point:Point<'a>, text:Datasource<'a>) =

        let xName = scaleName point.XScale
        let yName = scaleName point.YScale
        let c = Fixed("black")

        let t = 
            match text with
            | NumericValue(f) -> [Val("value",string f)]
            | CategoricalValue(f) -> [Val("value",f)]
            | Field (f) -> [Val("field",featureName f)]
        let xs = Nested("x", [Val("scale",xName);Val(writeSource point.XSource)])
        let ys = Nested("y", [Val("scale",yName);Val(writeSource point.YSource)])
        let txt = Nested("text",t)
        let color = Nested("fill",writeColor c)
        let enter = Nested("enter",[xs;ys;color;txt;])
        enter

    let preparePath = 
        let path = Nested("path",[Val("field","path")])
        let stroke = Nested("stroke",[Val("value","#ccc")])
        let width = Nested("strokeWidth",[NVal("value",1.0)])
        Nested("update",[path;stroke;width])

    let render mark = 
        match mark with
        | Symbol(point,decoration,pointDecoration) -> 
            [   Val("type","symbol");
                Nested("from", [ Val("data","table") ]);
                Nested("properties", [ prepareSymbol (point, decoration, pointDecoration) ])
            ]    
        | Rectangle(rect,decoration) ->
            [   Val("type","rect");
                Nested("from", [ Val("data","table") ]);
                Nested("properties", [ prepareRectangle (rect, decoration) ])
            ]             
        | Text(point,text) ->
            [   Val("type","text");
                Nested("from", [ Val("data","table") ]);
                Nested("properties", [ prepareText (point,text) ])
            ]         
        | Path ->    
            [   Val("type","path");
                Nested("from", [ Val("data","edges"); List("transform", [ [Val("type","link");Val("shape","line")]]); ]);               
                Nested("properties", [ preparePath ])
            ]         

    let writeItem a extractors =
        extractors
        |> List.map (fun ext ->
            match ext with
            | Numeric(name,func) -> NVal(name,func a)
            | Categorical(name,func) -> Val(name,func a))
                  
    let writeData dataset extractors =
        List("data", 
            [ [ 
                Val("name","table"); 
                List("values", dataset |> List.map (fun x -> writeItem x extractors))
              ] 
            ] ) 

module Basics =

    open Json
    open Functional

    let scatterplot dataset (fx, fy, fc, fs) =

        let xs = Numeric("fst", fx)
        let ys = Numeric("snd", fy)
        let cs = Categorical("col", fc)
        let ss = Numeric("size", fs)

        let xScale = ("X", Width, xs)
        let yScale = ("Y", Height, ys)
        let colorScale = ("Color", Color10, cs)

        let axes = { XAxis = xScale, None; YAxis = yScale, None}

        let point = 
            {   XScale = xScale;
                XSource = Field(xs);
                YScale = yScale;
                YSource = Field(ys) }

        let decoration = { Fill = Dynamic(colorScale,cs) }
        let pointDecoration = { Size = Field(ss) }
        let mark = Symbol(point,decoration,pointDecoration)

        let template = 
            [   NVal("width",400.);
                NVal("height",400.);
                writeData dataset [xs;ys;cs;ss];
                List ("scales", [ writeScale xScale; writeScale yScale; writeScale colorScale; ]);
                writeAxes axes;
                List ("marks", [ render mark; ])
            ]

        writeObj template

    let barplot dataset (fx, fy) =

        let xs = Categorical("fst", fx)
        let ys = Numeric("snd", fy)
     
        let xScale = ("X", Width, xs)
        let yScale = ("Y", Height, ys)

        let axes = { XAxis = xScale, None; YAxis = yScale, None }

        let rectangle =
            {   XSide = Relative(Field(xs), Band);
                XScale = xScale;
                YSide = Absolute(NumericValue(0.), Field(ys));
                YScale = yScale }
        let decoration = { Fill = Fixed("red") }

        let mark = Rectangle(rectangle, decoration)

        let template = 
            [   NVal("width",400.);
                NVal("height",300.);
                writeData dataset [xs;ys];
                List ("scales", [ writeScale xScale; writeScale yScale ]);
                writeAxes axes;
                List ("marks", [ render mark ])
            ]

        writeObj template

    let coloredBar dataset (fx, fy) fc =

        let xs = Categorical("fst", fx)
        let ys = Numeric("snd", fy)
        let cs = Categorical("col", fc)

        let xScale = ("X", Width, xs)
        let yScale = ("Y", Height, ys)
        let colorScale = ("Color", Color20, cs)

        let rectangle =
            {   XSide = Relative(Field(xs), Band);
                XScale = xScale;
                YSide = Absolute(NumericValue(0.), Field(ys));
                YScale = yScale }
        let decoration = { Fill = Dynamic(colorScale,cs) }
        let mark = Rectangle(rectangle, decoration)

        let axesValues = 
            let len = List.length dataset
            if len > 10
            then 
                let interval = len / 5
                let vs = dataset |> List.map fx
                Some([ for i in 0 .. (len - 1) do if (i % interval = 0) then yield vs.[i] ])
            else None
        let axes = { XAxis = xScale, axesValues; YAxis = yScale, None }

        let template = 
            [   NVal("width",400.);
                NVal("height",300.);
                writeData dataset [xs;ys;cs];
                List ("scales", [ writeScale xScale; writeScale yScale; writeScale colorScale ]);
                List ("legends", [ [Val("fill","Color")] ])
                writeAxes axes;
                List ("marks", [ render mark ])
            ]

        writeObj template

    let force nodes (ns) edges (src,tgt,v) =

        let nodeName = Categorical("name", ns)

        let nodeExtractors = [ nodeName ]

        let edgeSource = Numeric("source", src)
        let edgeTarget = Numeric("target", tgt)
        let edgeValue = Numeric("value", v)

        let edgeExtractors = [edgeSource; edgeTarget; edgeValue]

        let connections = Path
        let bubbles = 
            [   Val("type","symbol");
                Nested("from", [ Val("data","nodes") ]);
                Nested("properties", 
                    [   Nested("enter",
                            [   Nested("x",[Val("field","x")]);
                                Nested("y",[Val("field","y")]);
                                Nested("fill",[Val("value","steelblue")]);
                                Nested("fillOpacity",[NVal("value",0.2)]);
                                Nested("stroke",[Val("value","steelblue")]);
                            ])
                            
                    ])
            ]

        let template = 
            [   NVal("width",400.);
                NVal("height",300.);
                List("data", 
                    [ [ 
                        Val("name","edges"); 
                        List("values", edges |> List.map (fun x -> writeItem x edgeExtractors));
                        List("transform", [ [ Val("type","copy"); Val("from","data"); VList("fields",[ "source"; "target" ]) ] ]);
                      ]; 
                      [ 
                        Val("name","nodes"); 
                        List("values", nodes |> List.map (fun x -> writeItem x nodeExtractors));
                        List("transform", [ [ Val("type","force"); Val("links","edges"); NVal("linkDistance",50.); NVal("charge",-50.); NVal("iterations",1000.) ] ]);
                      ]; 
                    ])
                List ("marks", [ render connections; bubbles ])
            ]

        writeObj template

module Demo =

    open Json
    open Functional
    open Basics

    type obs = { First:float; Second:float; Cat:int }
    let rng = System.Random()
    let dataset = 
        [ for i in 1 .. 10 -> { First = rng.NextDouble(); Second = rng.NextDouble(); Cat = rng.Next(3) } ]
                                  
    let plot = scatterplot dataset ((fun x -> x.First), (fun x -> x.Second), (fun x -> x.Cat |> string), (fun x -> if x.Cat = 0 then 250. else 100.))

    let bar = barplot dataset ((fun x -> x.Cat |> string), (fun x -> x.Second))

    type Pres = { Year:int; Debt:float; Pres:string }
    let test = 
        [ for i in 1950 .. 2010 -> { Year = i; Debt = rng.NextDouble(); Pres = if i < 1960 then "Alpha" elif i < 1980 then "Bravo" else "Charlie" } ]

    let debt = coloredBar test ((fun x -> x.Year |> string), (fun x -> x.Debt)) (fun x -> x.Pres)

    type Node = { Name:string }
    let nodes = 
        [   { Name = "Alpha" };
            { Name = "Bravo" };
            { Name = "Charlie"};
            { Name = "Delta" } ]

    type Edge = { From:int; To:int; Value:int }
    let edges = 
        [   { From=0; To=1; Value= 5};
            { From=0; To=2; Value= 5};
            { From=0; To=3; Value= 5};
            { From=1; To=2; Value= 5};
            { From=1; To=3; Value= 5};
            { From=2; To=3; Value= 5}; ]

    let graph = force nodes (fun x -> x.Name) edges ((fun x -> x.From |> float), (fun x -> x.To |> float), (fun x -> x.Value |> float))

    ignore ()
