module Data.Boxes

open Data.Vectors

type Box = { a : Vector; b : Vector; c : Vector}

// (a’, b’, c’) = (a + b, c, -b)
// turnBox : Box -> Box
let turnBox { a = a; b = b; c = c } =
  { a = a + b
    b = c
    c = -b }

// flipBox : Box -> Box
let flipBox { a = a; b = b; c = c } =
  { a = a + b
    b = -b
    c = c }

// (a’, b’, c’) = (a + (b + c) / 2, (b + c) / 2, (c − b) / 2)
// tossBox : Box -> Box
let tossBox { a = a; b = b; c = c } =
  { a = a + (b + c) / 2.
    b = (b + c) / 2.
    c = (c - b) / 2. }

// moveVertically : float -> Box -> Box
let moveVertically f { a = a; b = b; c = c } =
  { a = a + f * c
    b = b
    c = c }

// scaleVertically : float -> Box -> Box
let scaleVertically f { a = a; b = b; c = c } =
  { a = a
    b = b
    c = f * c }

// splitVertically : float -> Box -> (Box * Box)
let splitVertically f box =
    let top = box |> moveVertically (1. - f) |> scaleVertically f
    let bot = box |> scaleVertically (1. - f)
    (top, bot)

// moveHorizontally : float -> Box -> Box
let moveHorizontally f { a = a; b = b; c = c } =
  { a = a + f * b
    b = b
    c = c }

// scaleHorizontally : float -> Box -> Box
let scaleHorizontally f { a = a; b = b; c = c } =
  { a = a
    b = f * b
    c = c }

// splitHorizontally : float -> Box -> (Box * Box)
let splitHorizontally f box =
  let left = box |> scaleHorizontally f
  let right = box |> moveHorizontally f |> scaleHorizontally (1. - f)
  (left, right)

let rec sideBoxes n box =
  if n < 1 then [ box ]
  else
    let top, bot = splitVertically 0.5 box
    let nw, ne = splitHorizontally 0.5 top
    let sw, se = splitHorizontally 0.5 bot
    sideBoxes (n - 1) nw @ sideBoxes (n - 1) ne @ [sw; se]

let rec westSideBoxes n box =
  if n < 1 then [ box ]
  else
    let top, bot = splitVertically 0.5 box
    let nw, ne = splitHorizontally 0.5 top
    let sw, se = splitHorizontally 0.5 bot
    westSideBoxes (n - 1) nw @ westSideBoxes (n - 1) sw @ [ne; se]

let rec cornerBoxes n box =
  if n < 1 then [ box ]
  else
    let top, bot = splitVertically 0.5 box
    let nw, ne = splitHorizontally 0.5 top
    let sw, se = splitHorizontally 0.5 bot
    cornerBoxes (n - 1) nw @ sideBoxes (n - 1) ne @ westSideBoxes (n - 1) sw @ [se]
