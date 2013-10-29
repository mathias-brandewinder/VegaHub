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
            | Color10 -> Val("range","color10")
            | Color20 -> Val("range","color20")
        let (domain, featType) =
            match domain with
            | Numeric(n,_)     -> Nested("domain",[Val("data","table");Val("field","data."+n)]), Val("type","linear")
            | Categorical(n,_) -> Nested("domain",[Val("data","table");Val("field","data."+n)]), Val("type","ordinal")
        [   Val("name",name); 
            featType;
            range; 
            domain; ]

    type Axes<'a> = { XAxis: Scale<'a>; YAxis: Scale<'a> } 

    let writeAxes axes =
        let x = 
            let name,scaleType,feature = axes.XAxis //string * ScaleType * Feature<'a>
            match scaleType with
            | Width -> [ Val("type","x"); Val("scale",name) ]
            | _     -> failwith "X axis should match width"   
        let y = 
            let name,scaleType,feature = axes.YAxis //string * ScaleType * Feature<'a>
            match scaleType with
            | Height -> [ Val("type","y"); Val("scale",name)]
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
      
    type Mark<'a> = 
        | Symbol of Point<'a>
        | Rectangle of Rectangle<'a>

    let prepareSymbol (point:Point<'a>) =
        let xName = scaleName point.XScale
        let yName = scaleName point.YScale
        let xs = Nested("x", [Val("scale",xName);Val(writeSource point.XSource)])
        let ys = Nested("y", [Val("scale",yName);Val(writeSource point.YSource)])
        let color = Nested("fill",[Val("value","steelblue")])
        let size = Nested("size",[Val("value","100")])
        let enter = Nested("enter",[xs;ys;color;size;])
        enter

    let prepareRectangle (rect:Rectangle<'a>) =
        let xName = scaleName rect.XScale
        let yName = scaleName rect.YScale
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
        let color = Nested("fill",[Val("value","steelblue")])
        let enter = Nested("enter",[x1;x2;y1;y2;color;])
        enter

    let render mark = 
        match mark with
        | Symbol(point) -> 
            [   Val("type","symbol");
                Nested("from", [ Val("data","table") ]);
                Nested("properties", [ prepareSymbol point ])
            ]    
        | Rectangle(rect) ->
            [   Val("type","rect");
                Nested("from", [ Val("data","table") ]);
                Nested("properties", [ prepareRectangle rect ])
            ]               

    let writeItem a extractors =
        extractors
        |> List.map (fun ext ->
            match ext with
            | Numeric(name,func) -> Val(name,string(func a))
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

    let scatterplot dataset (fx, fy, fc) =

        let xs = Numeric("fst", fx)
        let ys = Numeric("snd", fy)
        let cs = Categorical("col", fc)

        let xScale = ("X", Width, xs)
        let yScale = ("Y", Height, ys)
        let colorScale = ("Color", Color10, cs)

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
                List ("scales", [ writeScale xScale; writeScale yScale; writeScale colorScale; ]);
                writeAxes axes;
                List ("marks", [ render mark ])
            ]

        writeObj template

    let barplot dataset (fx, fy) =

        let xs = Categorical("fst", fx)
        let ys = Numeric("snd", fy)
     
        let xScale = ("X", Width, xs)
        let yScale = ("Y", Height, ys)

        let rectangle =
            {   XSide = Relative(Field(xs), Band);
                XScale = xScale;
                YSide = Absolute(NumericValue(0.), Field(ys));
                YScale = yScale }

        let axes = { XAxis = xScale; YAxis = yScale }

        let mark = Rectangle(rectangle)

        let template = 
            [   NVal("width",400.);
                NVal("height",300.);
                writeData dataset [xs;ys];
                List ("scales", [ writeScale xScale; writeScale yScale ]);
                writeAxes axes;
                List ("marks", [ render mark ])
            ]

        writeObj template

module Demo =

    open Json
    open Functional
    open Basics

    type obs = { First:float; Second:float; Cat:int }
    let rng = System.Random()
    let dataset = 
        [ for i in 1 .. 10 -> { First = rng.NextDouble(); Second = rng.NextDouble(); Cat = i } ]
                                  
    let plot = scatterplot dataset ((fun x -> x.First), (fun x -> x.Second))

    let bar = barplot dataset ((fun x -> x.Cat |> string), (fun x -> x.Second))

    ignore ()
