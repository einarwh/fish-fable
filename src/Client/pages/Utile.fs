module Pages.Utile

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
  let box = { a = { x = 75.; y = 75. }
              b = { x = 250.; y = 0. }
              c = { x = 0.; y = 250. } }
  let shapes = box |> p
  (bounds, [], shapes)
