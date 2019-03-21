module Pages.Xutile

open Data.Vectors
open Data.Fishier
open Data.Boxes
open Data.Lenses
open Data.Shades
open Data.Styling
open Data.Colorites
open Rendering.Reform

let init() : Fish3Model =
  createLensPicture fishShapes |> Fish3

let transform (Fish3 p) : ColorPictureModel =
  let bounds = (550, 200)
  let box = { a = { x = 50.; y = 50. }
              b = { x = 450.; y = 0. }
              c = { x = 0.; y = 100. } }
  let lens = (box, Whiteish)
  let shapes = lens |> p
  (bounds, Grey, shapes)
