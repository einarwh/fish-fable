module Pages.Over

open Data.Vectors
open Data.Fishy
open Data.Boxes
open Data.Pictures
open Data.Models
open Rendering.Transforms

let init() : FishModel =
  createPicture hendersonFishShapes |> Fish

let transform (Fish p) : PictureModel =
  let bounds = (300, 300)
  let box = { a = { x = 50.; y = 50. }
              b = { x = 200.; y = 0. }
              c = { x = 0.; y = 200. } }
  let shapes = box |> over p (p |> turn |> turn)
  (bounds, [], shapes)
