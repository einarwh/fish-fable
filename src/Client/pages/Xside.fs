module Pages.Xside

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
  let bounds = (500, 200)
  let box = { a = { x = 50.; y = 50. }
              b = { x = 420.; y = 0. }
              c = { x = 0.; y = 140. } }
  let lens = (box, Whiteish)
  let shapes = []
  (bounds, Grey, shapes)