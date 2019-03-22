module Pages.Limit

open Data.Vectors
open Data.Fishy
open Data.Boxes
open Data.Pictures
open Data.Models
open Rendering.Transforms

let init() : FishModel = 
  createPicture hendersonFishShapes |> Fish
  
let transform (Fish p) : PictureModel =
  let bounds = (440, 440)
  let box = { a = { x = 30.; y = 40. }
              b = { x = 380.; y = 0. }
              c = { x = 0.; y = 380. } }
  let shapes = box |> blank
  (bounds, [], shapes)
