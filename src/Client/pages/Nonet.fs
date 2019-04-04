module Pages.Nonet

open Data.Vectors
open Data.Letters
open Data.Figures
open Data.Boxes
open Data.Pictures
open Data.Models
open Rendering.Transforms

let init() : NonetModel =
  let p = createPicture
  {
    nw = p hLetter
    nm = p eLetter
    ne = p nLetter
    mw = p dLetter
    mm = p eLetter
    me = p rLetter
    sw = p sLetter
    sm = p oLetter
    se = p nLetter
  }

let transform { nw = nw; nm = nm; ne = ne;
                mw = mw; mm = mm; me = me;
                sw = sw; sm = sm; se = se } : PictureModel =
  let bounds = (400, 400)
  let box = { a = { x = 40.; y = 60. }
              b = { x = 320.; y = 0. }
              c = { x = 0.; y = 320. } }
  let zoom p = nonet nw nm ne mw p me sw sm se
  let shapes = box |> (mm |> times 5 zoom)
  (bounds, [], shapes)
