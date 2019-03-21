module Data.Models

open Data.Pictures

type BasicModel = 
  { letter : Picture
    figure : Picture }

type NonetModel = 
  { nw : Picture
    nm : Picture
    ne : Picture
    mw : Picture
    mm : Picture
    me : Picture
    sw : Picture
    sm : Picture
    se : Picture }

type FishModel = Fish of Picture

type DataModel = 
    | BasicModel of BasicModel
    | NonetModel of NonetModel 
    | FishModel of FishModel

