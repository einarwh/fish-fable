module Pages.Corner

open Data.Vectors
open Data.Fishy
open Data.Boxes
open Data.Pictures
open Data.Models
open Rendering.Transforms

let init() : FishModel =
  createPicture hendersonFishShapes |> Fish

let transform (Fish p) : PictureModel =
  let bounds = (400, 400)
  let box = { a = { x = 50.; y = 50. }
              b = { x = 300.; y = 0. }
              c = { x = 0.; y = 300. } }
  let n = 2
  let shapes = box |> corner n p
  (bounds, cornerBoxes n box, shapes)
