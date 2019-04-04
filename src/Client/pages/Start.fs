module Pages.Start

open Data.Vectors
open Data.Letters
open Data.Figures
open Data.Boxes
open Data.Pictures
open Data.Models
open Rendering.Transforms

let init () : BasicModel = { letter = createPicture fLetter; figure = createPicture george }

let transform { letter = letter; figure = figure }
  : PictureModel =
  let bounds = (300, 300)
  let box = { a = { x = 100.; y = 50. }
              b = { x = 100.; y = 10. }
              c = { x = -30.; y = 20. } }
  let shapes = box |> letter
  (bounds, [ box ], shapes)
