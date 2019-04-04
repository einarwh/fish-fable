module Data.Pictures

open Data.Shapes
open Data.Styling
open Data.Boxes

type Rendering = (Shape * Style) list

type Picture = Box -> Rendering

let blank : Picture = fun _ -> []

let rec times (n : int) (fn : 'a -> 'a) =
    if n < 1 then id
    else fn >> times (n - 1) fn

// (Box -> Box) >> (Box -> Rendering) ==> (Box -> Rendering)
// turn : Picture -> Picture
let turn (p : Picture) : Picture =
    turnBox >> p

let flip p = flipBox >> p

let toss p = tossBox >> p

let aboveRatio (m : int) (n : int) (p1 : Picture) (p2 : Picture) : Picture =
    fun box ->
        let f = float m / float (m + n)
        let (top, bot) = splitVertically f box
        p1 top @ p2 bot

let above = aboveRatio 1 1

// besideRatio : Int -> Int -> Picture -> Picture -> Picture
let besideRatio m n p1 p2 : Picture =
  fun box ->
    let f = float m / float (m + n)
    let (b1, b2) = splitHorizontally f box
    (p1 b1) @ (p2 b2)

// beside : Picture -> Picture -> Picture
let beside = besideRatio 1 1

// quartet : Picture -> Picture -> Picture -> Picture -> Picture
let quartet nw ne sw se =
    above (beside nw ne)
          (beside sw se)

// nonet : Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
let nonet nw nm ne mw mm me sw sm se =
    let row w m e = besideRatio 1 2 w (beside m e)
    let col n m s = aboveRatio 1 2 n (above m s)
    col (row nw nm ne)
        (row mw mm me)
        (row sw sm se)

let over p1 p2 =
    fun box -> p1 box @ p2 box

let ttile fish =
    let fishN = fish |> toss |> flip
    let fishE = fishN |> times 3 turn
    fishE |> over fishN |> over fish

let utile fish =
    let fishN = fish |> toss |> flip
    let fishW = turn fishN
    let fishS = turn fishW
    let fishE = turn fishS
    fishE |> over fishS |> over fishW |> over fishN

let rec side n fish =
    if n < 1 then blank
    else
        let s = side (n - 1) fish
        let t = ttile fish
        quartet s s (turn t) t

let rec corner n fish =
    if n < 1 then blank
    else
        let c = corner (n - 1) fish
        let s = side (n - 1) fish
        quartet c s (turn s) (utile fish)

// squareLimit : int -> Picture -> Picture
let squareLimit n fish =
  let mm = utile fish
  let nw = corner n fish
  let sw = nw |> turn
  let se = sw |> turn
  let ne = se |> turn
  let nm = side n fish
  let mw = nm |> turn
  let sm = mw |> turn
  let me = sm |> turn
  nonet nw nm ne mw mm me sw sm se





