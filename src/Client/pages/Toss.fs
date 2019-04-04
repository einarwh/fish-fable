module Pages.Toss

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
  let box = { a = { x = 75.; y = 50. }
              b = { x = 150.; y = 0. }
              c = { x = 0.; y = 150. } }
  let n = 4
  let shapes = box |> times n toss figure
  let rec gatherBoxes n box =
    if n < 1 then [ box]
    else
        times n tossBox box :: gatherBoxes (n - 1) box
  (bounds, gatherBoxes n box, shapes)
