module Rendering.Transforms

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Data.Vectors
open Data.Shapes
open Data.Styling
open Data.Boxes
open Data.Pictures
open Fable.Import.Browser

type Bounds = (int * int)

type PictureModel = (Bounds * Box list * (Shape * Style) list)

let mapper { a = a; b = b; c = c }
           { x = x; y = y } =
   a + b * x + c * y

let mapShape m = function
  | Polygon { points = pts } ->
    Polygon { points = pts |> List.map m }
  | Polyline { pts = pts } ->
    Polyline { pts = pts |> List.map m }
  | Curve { point1 = v1
            point2 = v2
            point3 = v3
            point4 = v4 } ->
    Curve { point1 = m v1
            point2 = m v2
            point3 = m v3
            point4 = m v4 }
  | x -> x

let size { x = x; y = y } =
  sqrt(x * x + y * y)

let getStrokeWidth { a = _; b = b; c = c } =
  let s = min (size b) (size c)
  s / 80.

let getStyle box =
  let sw = getStrokeWidth box
  { stroke = Some { strokeWidth = sw
                    strokeColor = StyleColor.Black }
    fill = None }

let createPicture (shapes : Shape list) : Picture =
   fun box ->
     let m = mapper box
     let style = getStyle box
     shapes |> List.map (mapShape m) |> List.map (fun s -> s, style)

let toAxisLine { lineStart = { x = x1; y = y1 }
                 lineEnd = { x = x2; y = y2 } } =
   line
     [ SVGAttr.Stroke "black"
       SVGAttr.StrokeWidth "1.5"
       X1 x1
       Y1 y1
       X2 x2
       Y2 y2 ] []

(*
[
    svg [ SVGAttr.Width "600px"
          SVGAttr.Height "100px" ]
        [ defs [ ]
            [ svgEl "marker" [ SVGAttr.Custom("id", "arrow")
                               SVGAttr.Custom ("markerWidth", "10")
                               SVGAttr.Custom ("markerHeight", "10")
                               SVGAttr.Custom ("refX", "0")
                               SVGAttr.Custom ("refY", "3")
                               SVGAttr.Custom ("orient", "auto")
                               SVGAttr.Custom ("markerUnits", "strokeWidth")
                               SVGAttr.ViewBox "0 0 20 20" ]
                [ path [ SVGAttr.D "M0,0 L0,6 L9,3 z"
                         SVGAttr.Fill "#f00" ]
                    [ ] ] ]
          line [ SVGAttr.X1 "50"
                 SVGAttr.Y1 "50"
                 SVGAttr.X2 "250"
                 SVGAttr.Y2 "50"
                 SVGAttr.Stroke "#000"
                 SVGAttr.StrokeWidth "5"
                 SVGAttr.MarkerEnd "url(#arrow)" ]
*)

let createMarker markerId color =
    let attr k v =
        SVGAttr.Custom(k, v)
    svgEl "marker"
        [ attr "id" markerId
          attr "markerWidth" "10"
          attr "markerHeight" "10"
          attr "refX" "9"
          attr "refY" "3"
          attr "orient" "auto"
          attr "markerUnits" "strokeWidth" ]
        [ path
            [ SVGAttr.D "M0,0 L0,6 L9,3 z"
              SVGAttr.Fill color ]
            [] ]

let acolor = "#B22327"
let bcolor = "#2381bf"
let ccolor = "#27b15b"

let toArrow name color { lineStart = { x = x1; y = y1 }
                         lineEnd = { x = x2; y = y2 } } =
   line
     [ SVGAttr.Stroke color
       SVGAttr.StrokeWidth "1.5"
       X1 x1
       Y1 y1
       X2 x2
       Y2 y2
       SVGAttr.MarkerEnd (sprintf "url(#%s)" name) ] []

let toDottedLine { lineStart = { x = x1; y = y1 }
                   lineEnd = { x = x2; y = y2 } } =
   line
     [ SVGAttr.Stroke "grey"
       SVGAttr.StrokeDasharray "2"
       SVGAttr.StrokeWidth "1"
       X1 x1
       Y1 y1
       X2 x2
       Y2 y2 ] []

let mirrorVector height { x = x; y = y } =
  { x = x; y = height - y }

let mirrorShape mirror = function
    | Line { lineStart = lineStart
             lineEnd = lineEnd } ->
      Line { lineStart = mirror lineStart
             lineEnd = mirror lineEnd }
    | Polygon { points = pts } ->
      Polygon { points = pts |> List.map mirror }
    | Polyline { pts = pts } ->
      Polyline { pts = pts |> List.map mirror }
    | Curve { point1 = v1
              point2 = v2
              point3 = v3
              point4 = v4 } ->
      Curve { point1 = mirror v1
              point2 = mirror v2
              point3 = mirror v3
              point4 = mirror v4 }
    | x -> x


let getStrokeWidthFromStyle = function
  | Some strokeStyle ->
    1.
  | None -> 1.

let toSvgElement (style : Style) = function
    | Line { lineStart = { x = x1; y = y1 }
             lineEnd = { x = x2; y = y2 } } ->
      let lineElement =
        line
          [ SVGAttr.Stroke "green"
            X1 x1
            Y1 y1
            X2 x2
            Y2 y2 ] []
      lineElement
    | Polygon { points = pts } ->
      let pt { x = x; y = y } = sprintf "%f,%f" x y
      let s = pts |> List.map pt |> List.fold (fun acc it -> if acc = "" then it else acc + " " + it) ""
      let polygonElement =
        polygon
          [ SVGAttr.Stroke "black"
            SVGAttr.Fill "none"
            Points s ] []
      polygonElement
    | Polyline { pts = pts } ->
      let pt { x = x; y = y } = sprintf "%f,%f" x y
      let s = pts |> List.map pt |> List.fold (fun acc it -> if acc = "" then it else acc + " " + it) ""
      let polylineElement =
        polyline
          [ SVGAttr.Stroke "black"
            SVGAttr.Fill "none"
            Points s ] []
      polylineElement
    | Curve { point1 = { x = x1; y = y1 }
              point2 = { x = x2; y = y2 }
              point3 = { x = x3; y = y3 }
              point4 = { x = x4; y = y4 } } ->
      let d = sprintf "M%f %f C %f %f, %f %f, %f %f" x1 y1 x2 y2 x3 y3 x4 y4
      let strokeWidth = getStrokeWidthFromStyle style.stroke
      let curveElement =
        path
          [ SVGAttr.Stroke "black"
            SVGAttr.Fill "none"
            SVGAttr.StrokeWidth strokeWidth
            SVGAttr.StrokeLinecap "butt"
            SVGAttr.D d ] []
      curveElement
    | _ -> failwith "unmatched shape in toSvgElement"


let toAxis length =
   let dotPositions = [0 .. 25 .. length]

   let dashes =
     let fn n =
       let x = float n
       { lineStart = { x = x; y = 0. }
         lineEnd = { x = x; y = 3. } }
     dotPositions |> List.map fn
   let ln =
     { lineStart = { x = 0.; y = 0. }
       lineEnd = { x = float length; y = 0. } }
   ln :: dashes

let mirrorLine mv { lineStart = ls; lineEnd = le } =
  { lineStart = mv ls; lineEnd = mv le }

let toXAxis mv svgWidth =
  toAxis svgWidth
  |> List.map (mirrorLine mv >> toAxisLine)

let swap { x = x; y = y } = { x = y; y = x }

let toYAxis mv svgHeight =
  toAxis svgHeight
  |> List.map (fun { lineStart = s; lineEnd = e } ->
                   { lineStart = swap s; lineEnd = swap e })
  |> List.map (mirrorLine mv >> toAxisLine)

let boxLines mv box =
    let a = box.a
    let b = box.b
    let c = box.c
    let aarrow =
        let ln = { lineStart = mv { x = 0.; y = 0. }
                   lineEnd = mv a }
        toArrow "a-arrow" acolor ln
    let barrow =
        let ln = { lineStart = mv a
                   lineEnd = mv (a + b) }
        toArrow "b-arrow" bcolor ln
    let carrow =
        let ln = { lineStart = mv a
                   lineEnd = mv (a + c) }
        toArrow "c-arrow" ccolor ln
    let bdotted = toDottedLine { lineStart = mv (a + c)
                                 lineEnd = mv (a + c + b) }
    let cdotted = toDottedLine { lineStart = mv (a + b)
                                 lineEnd = mv (a + b + c) }
    let arrows = aarrow :: barrow :: carrow :: bdotted :: [ cdotted ]
    arrows

let view ((bounds, boxes, shapes) : PictureModel) =
    let (svgWidth, svgHeight) = bounds
    let mv = (mirrorVector <| float svgHeight)
    let fn (shape, style) =
      (mirrorShape mv >> toSvgElement style) shape
    let svgElements = shapes |> List.map fn

    let xAxis = toXAxis mv svgWidth
    let yAxis = toYAxis mv svgHeight

    let arrows = boxes |> List.collect (boxLines mv)
    let elements =
      if boxes |> List.isEmpty then arrows @ svgElements
      else xAxis @ yAxis @ arrows @ svgElements
    let defsElement =
      defs []
           [ createMarker "a-arrow" acolor
             createMarker "b-arrow" bcolor
             createMarker "c-arrow" ccolor ]
    svg [ HTMLAttr.Width svgWidth
          HTMLAttr.Height svgHeight ]
        (defsElement :: elements)
