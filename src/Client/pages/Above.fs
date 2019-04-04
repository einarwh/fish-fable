module Pages.Above

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
  let box = { a = { x = 50.; y = 50. }
              b = { x = 200.; y = 0. }
              c = { x = 0.; y = 200. } }
  let shapes = box |> turn (aboveRatio 1 2 letter (above (turn letter) (times 2 turn letter)))
  let (top, bot) = splitVertically (1. / 3.) box
  let (b1, b2) = splitVertically 0.5 bot

  (bounds, [ box ], shapes)
