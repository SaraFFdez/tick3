module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes

type Msg = Unit // no messages needed

type Model = Unit // No model needed

//-------------------------Helper functions for Tick 3-------------------------------------//

// Helper functions that are useful should normally be put into Helpers module
// For Tick3 only put Helper functions here for ease of assessment and feedback
// The obvious helpers to simplify solution are those that create and manipulate SVG elements.
// SVG boilerplate should be greatly reduced in a well-written system.

let posOf x y = {X=x;Y=y} // helper

// add your own functions as needed
//Gotta change this!!!!!
let isaComponent = true  

let decComp comp = 
    match comp.Type with
    |BusDecoder (w,a,n) -> (w,a,n)
    |_ -> failwithf "not a valid component"
    
//-----------------------------------------------------------------------------------------//


/// write this for Tick3 using your modified ComponentType
/// you may add to type definition in CommonTypes
let makeBusDecoderComponent (pos:XYPos) (w: int) (a: int) (n: int) = 
    let comp =  {
        X = int pos.X
        Y = int pos.Y
        W = 60 
        H = 17+(11*n) 
        Type = BusDecoder (w,a,n)
    }
    comp

/// demo function - not needed for Tick3 answer
let makeDummyComponent (pos: XYPos): Component =
    { 
        X = int pos.X
        Y = int pos.Y
        W = 0 // dummy
        H = 0 // dummy
        Type = Not // dummy
    }

//-----------------------Elmish functions with no content in Tick3----------------------//

/// For this program init() generates the required result
let init () =
    (), Cmd.none

/// update function does nothing!
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | () -> model, Cmd.none // do nothing if we receive the (only) message

//----------------------------View Function for Symbol----------------------------//

/// Tick3 answer
let busDecoderView (comp: Component) =
    if isaComponent 
        then 
            let fX = float comp.X
            let fY = float comp.Y 
            // in real code w,a,n would come from the component, but as the busDecoder case is not yet written this
            // is a workaround compatible with the dummy components
            let w,a,n = decComp comp // workaround
            //
            // This code demonstrates svg transformations, not needed for Tick3 but useful.
            // The elmish react syntax here uses CSS style transforms, not SVG attribute transforms. They are different.
            // In addition, svg elements transform under css differently from html.
            // See https://css-tricks.com/transforms-on-svg-elements/ for details if needed.
            //
            let scaleFactor=1.0 // to demonstrate svg scaling
            let rotation=0 // to demonstrate svg rotation (in degrees)
            let lst = [a..(a+n-1)]
            let stringLst = List.map (fun x -> (string(x)) ) lst
            let indexedStrLst = List.indexed stringLst
            let transToElem (i,x) = 
                let value = float(30+10*i)
                text [ // a demo text svg element
                    X 20.; 
                    Y value; 
                    Style [
                        TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                        DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                        FontSize "6px"
                        FontWeight "Bold"
                        Fill "Black" // demo font color
                    ]
                ] [str <| sprintf "%A" x]
            let printNumbers = List.map (transToElem) indexedStrLst
            let allElement = 
                        polygon [ // a demo svg polygon quad
                            SVGAttr.Points (sprintf "-30,10 %i,10 %i,%i -30,%i" (-30+comp.W) (-30+comp.W) (10+comp.H) (10+comp.H)) // "--30,10 -30+comp.W,10 -30+comp.W,10+comp.H -30,10+comp.H"
                            SVGAttr.StrokeWidth "2px"
                            SVGAttr.Stroke "Black"
                            SVGAttr.FillOpacity 0.1
                            SVGAttr.Fill "Gray"] []::
        
                        text [ // a demo text svg element
                            X 0.; 
                            Y 20.; 
                            Style [
                                TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "6px"
                                FontWeight "Bold"
                                Fill "Black" // demo font color
                            ]
                        ] [str <| sprintf "Bus Decode"]::
        
        
        
                        text [ // a demo text svg element
                            X -23.; 
                            Y 50.; 
                            Style [
                                TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "6px"
                                FontWeight "Bold"
                                Fill "Black" // demo font color
                            ]
                        ] [str <| sprintf "In"]::
                        printNumbers
            g   [ Style [ 
                    // the transform here does rotation, scaling, and translation
                    // the rotation and scaling happens with TransformOrigin as fixed point first
                    TransformOrigin "0px 50px" // so that rotation is around centre of line
                    Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " fX fY rotation scaleFactor )
                    ]

                ]  // g optional attributes in first list
                // use g svg element (SVG equivalent of div) to group any number of ReactElements into one.
                // use transform with scale and/or translate and/or rotate to transform group
                allElement    
    else failwithf "The element provided is not a Component and cannot be viewed"

/// demo function can be deleted
let busDecoderViewDummy (comp: Component) = 
    let fX = float comp.X
    let fY = float comp.Y 
    // in real code w,a,n would come from the component, but as the busDecoder case is not yet written this
    // is a workaround compatible with the dummy components
    let w,a,n = if fX < 100. then (3,0,8) else (4,3,5) // workaround
    //
    // This code demonstrates svg transformations, not needed for Tick3 but useful.
    // The elmish react syntax here uses CSS style transforms, not SVG attribute transforms. They are different.
    // In addition, svg elements transform under css differently from html.
    // See https://css-tricks.com/transforms-on-svg-elements/ for details if needed.
    //
    let scaleFactor=1.0 // to demonstrate svg scaling
    let rotation=0 // to demonstrate svg rotation (in degrees)
    let lst = [a..(a+n-1)]
    let stringLst = List.map (fun x -> (string(x)) ) lst
    let indexedStrLst = List.indexed stringLst
    let transToElem (i,x) = 
        let value = float(10+10*i)
        text [ // a demo text svg element
            X 20.; 
            Y value; 
            Style [
                TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                FontSize "6px"
                FontWeight "Bold"
                Fill "Black" // demo font color
            ]
        ] [str <| sprintf "%A" x]
    let printNumbers = List.map (transToElem) indexedStrLst
    let utkufunc = 
                polygon [ // a demo svg polygon quad
                    SVGAttr.Points "-30,10 30,10 30,120 -30,120" // "--30,10 -30+comp.W,10 -30+comp.W,10+comp.H -30,10+comp.H"
                    SVGAttr.StrokeWidth "2px"
                    SVGAttr.Stroke "Black"
                    SVGAttr.FillOpacity 0.1
                    SVGAttr.Fill "Gray"] []::

                text [ // a demo text svg element
                    X 0.; 
                    Y 20.; 
                    Style [
                        TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                        DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                        FontSize "6px"
                        FontWeight "Bold"
                        Fill "Black" // demo font color
                    ]
                ] [str <| sprintf "Bus Decode"]::



                text [ // a demo text svg element
                    X -20.; 
                    Y 70.; 
                    Style [
                        TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                        DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                        FontSize "6px"
                        FontWeight "Bold"
                        Fill "Black" // demo font color
                    ]
                ] [str <| sprintf "In"]::
                printNumbers

    g   [ Style [ 
            // the transform here does rotation, scaling, and translation
            // the rotation and scaling happens with TransformOrigin as fixed point first
            TransformOrigin "0px 50px" // so that rotation is around centre of line
            Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " fX fY rotation scaleFactor )
            ]
        
        ]  // g optional attributes in first list
        // use g svg element (SVG equivalent of div) to group any number of ReactElements into one.
        // use transform with scale and/or translate and/or rotate to transform group
        //[
        utkufunc
       


/// View function - in this case view is independent of model
let view (model : Model) (dispatch : Msg -> unit) =    
    [   // change for Tick3 answer
        makeBusDecoderComponent {X=70.; Y=20.} 3 0 8// for Tick 3 two components
        makeBusDecoderComponent {X=200.; Y=20.} 4 3 5
    ] 
    |> List.map busDecoderView // change for Tick3 answer
    |> (fun svgEls -> 
        svg [
            Style [
                Border "3px solid green"
                Height 200.
                Width 300.   
            ]
        ]   svgEls )


type ValidateError =
   | WIsInvalid // ignoring a,n
   | AIsInvalid // for given w, ignoring n
   | NIsInvalid // for given a,w

/// Tick3 answer
let busDecoderValidate (comp:Component) : Result<Component, ValidateError*string> =
    if(isaComponent)
        then
            let w,a,n = decComp comp //comp.Type(BusDecoder (w,a,n)) 
            if (w<1) then Error (WIsInvalid, "w is invalid")
            else if (float(a)>((2.0**float(w))-1.0)) || (a<0)then Error (AIsInvalid, "A is invalid")
            else if (float(a+n) > (2.0**float(w))) || (n<1) then Error (NIsInvalid, "N is invalid")
            else Ok comp
    else failwithf "Component was not correct"
    



    


