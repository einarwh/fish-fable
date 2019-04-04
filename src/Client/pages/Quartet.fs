module Pages.Quartet

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
  let bounds = (400, 400)
  let box = { a = { x = 50.; y = 50. }
              b = { x = 300.; y = 0. }

              c = { x = 0.; y = 300. } }
  let nw = figure
  let ne = figure |> flip |> turn |> turn
  let sw = figure |> turn |> turn
  let se = figure |> flip
  let q4 p = quartet p p p p

  let shapes = box |> (quartet nw ne sw se |> times 2 q4 |> turn)
  (bounds, [], shapes)
