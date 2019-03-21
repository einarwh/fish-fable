module Data.Colorites

open Data.Shades

type Fish3Model = Fish3 of Picture

type Colorite =
    | Fish3Model of Fish3Model
