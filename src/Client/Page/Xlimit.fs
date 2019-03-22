module Pages.Xlimit

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
  let bounds = (460, 460)
  let box = { a = { x = 30.; y = 40. }
              b = { x = 400.; y = 0. }
              c = { x = 0.; y = 400. } }
  let lens = (box, Greyish)
  let shapes = lens |> blank
  (bounds, White, shapes)