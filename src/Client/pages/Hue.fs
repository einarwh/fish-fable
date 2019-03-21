module Pages.Hue

open Data.Vectors
open Data.Fishier
open Data.Boxes
open Data.Lenses
open Data.Shades
open Data.Styling
open Data.Colorites
open Rendering.Reform

let init() : Omega3Model = 
  createLensPicture fishShapes |> Omega3
  
let transform (Omega3 p) : ColorPictureModel = 
  let bounds = (500, 250)
  let box = { a = { x = 200.; y = 50. }
              b = { x = 150.; y = 0. }
              c = { x = 0.; y = 150. } }
  let shapes = (box, Whiteish) |> p
  (bounds, Grey, shapes)
