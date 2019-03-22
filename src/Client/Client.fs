module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Thoth.Json

open Rendering.Transforms
open Data.Models
open Data.Colorites
open Pages
open Pages.Start
open Pages.Turn
open Pages.Flip
open Pages.Toss
open Pages.Above
open Pages.Beside
open Pages.Quartet
open Pages.Nonet
open Pages.Hue
open Pages.Xlimit

open Fulma
open Fable.Import

type Data =
  | Nothing
  | DataModel of DataModel
  | Colorite of Colorite

type Model =
    { Page : Page
      Next : Page option
      Prev : Page option
      Data : Data }

let handleNotFound (model : Model) =
    Browser.console.error("Error parsing url: " + Browser.window.location.href)
    (model, Cmd.none)

let toModel page =
    match page with
    | Page.Home ->
        { Page = page
          Prev = None
          Next = Some Page.Henderson
          Data = Nothing }
    | Page.Henderson ->
        { Page = page
          Prev = Some Page.Home
          Next = Some Page.Start
          Data = Nothing }
    // | Page.Keynote ->
    //     { Page = page
    //       Prev = Some Page.Henderson
    //       Next = Some Page.Sicp
    //       Data = Nothing }
    // | Page.Sicp ->
    //     { Page = page
    //       Prev = Some Page.Keynote
    //       Next = Some Page.Safe
    //       Data = Nothing }
    // | Page.Safe ->
    //     { Page = page
    //       Prev = Some Page.Sicp
    //       Next = Some Page.Abstraction
    //       Data = Nothing }
    // | Page.Abstraction  ->
    //     { Page = page
    //       Prev = Some Page.Safe
    //       Next = Some Page.Start
    //       Data = Nothing }
    | Page.Start ->
        { Page = page
          Prev = Some Page.Henderson
          Next = Some Page.Turn
          Data = Start.init() |> BasicModel |> DataModel }
    | Page.Turn ->
        { Page = page
          Prev = Some Page.Start
          Next = Some Page.Flip
          Data = Turn.init() |> BasicModel |> DataModel }
    | Page.Flip ->
        { Page = page
          Prev = Some Page.Turn
          Next = Some Page.Toss
          Data = Flip.init() |> BasicModel |> DataModel }
    | Page.Toss ->
        { Page = page
          Prev = Some Page.Flip
          Next = Some Page.Above
          Data = Toss.init() |> BasicModel |> DataModel }
    | Page.Above ->
        { Page = page
          Prev = Some Page.Toss
          Next = Some Page.Beside
          Data = Above.init() |> BasicModel |> DataModel }
    | Page.Beside ->
        { Page = page
          Prev = Some Page.Above
          Next = Some Page.Quartet
          Data = Beside.init() |> BasicModel |> DataModel }
    | Page.Quartet ->
        { Page = page
          Prev = Some Page.Beside
          Next = Some Page.Nonet
          Data = Quartet.init() |> BasicModel |> DataModel }
    | Page.Nonet ->
        { Page = page
          Prev = Some Page.Quartet
          Next = Some Page.Over
          Data = Nonet.init() |> NonetModel |> DataModel }
    | Page.Over ->
        { Page = page
          Prev = Some Page.Nonet
          Next = Some Page.Ttile
          Data = Over.init() |> FishModel |> DataModel }
    | Page.Ttile ->
        { Page = page
          Prev = Some Page.Over
          Next = Some Page.Utile
          Data = Ttile.init() |> FishModel |> DataModel }
    | Page.Utile ->
        { Page = page
          Prev = Some Page.Ttile
          Next = Some Page.Side
          Data = Utile.init() |> FishModel |> DataModel }
    | Page.Side ->
        { Page = page
          Prev = Some Page.Utile
          Next = Some Page.Corner
          Data = Side.init() |> FishModel |> DataModel }
    | Page.Corner ->
        { Page = page
          Prev = Some Page.Side
          Next = Some Page.Limit
          Data = Corner.init() |> FishModel |> DataModel }
    | Page.Limit ->
        { Page = page
          Prev = Some Page.Corner
          Next = Some Page.Hue
          Data = Limit.init() |> FishModel |> DataModel }
    | Page.Hue ->
        { Page = page
          Prev = Some Page.Limit
          Next = Some Page.Xlimit
          Data = Hue.init() |> Fish3Model |> Colorite }
    | Page.Xlimit ->
        { Page = page
          Prev = Some Page.Hue
          Next = None
          Data = Xlimit.init() |> Fish3Model |> Colorite }
    | _ ->
        { Page = page
          Prev = None
          Next = None
          Data = Nothing }

let updateModel (page : Page) (model : Model) =
    toModel page

let urlUpdate (result : Page option) (model : Model) =
    match result with
    | None -> handleNotFound model
    | Some page ->
      updateModel page model, Cmd.none

type Msg =
    | Noop

// defines the initial state and initial command (= side-effect) of the application
let init result : Model * Cmd<Msg> =
    let initialModel = toModel Page.Home
    initialModel, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    currentModel, Cmd.none

let viewLink page description =
    a [ Style []
        Href (Pages.toHash page) ]
      [ str description ]


let centerStyle direction =
    Style [ Display "flex"
            FlexDirection direction
            AlignItems "center"
            JustifyContent "center"
            Padding "20px 0" ]

let heading text = p [ ClassName "heading" ] [ str text ]
let subheading text = p [ ClassName "subheading" ] [ str text]

let spacing = div [ Style [ Padding "12px" ] ] []

let matchfail text = [ p [] [ str <| sprintf "pattern match fail %s" text ] ]

let viewPage model dispatch =
    match model.Page with
    | Page.Home ->
        [ heading "A Fable about Fish"
          heading ""
          heading "Einar W. Høst"
          p [] []
          img [ Src "images/escher-square-limit.png" ] ]

    | Page.Henderson ->
        [ heading "Peter Henderson"
          heading "Functional geometry (1982, 2002)"
          p [] []
          img [ Src "images/henderson.png" ] ]

    | Page.Keynote ->
        [ heading "Inspiration for this talk"
          p [] []
          img [ Src "images/why-fp.png" ] ]

    | Page.Sicp ->
        [ heading "SICP videos"
          p [] []
          img [ Src "images/sicp-3A.png" ] ]

    | Page.Abstraction ->
        [ heading "abstraction"
          p [] []
          img [ Src "images/abstraction.jpg" ] ]

    | Page.Safe ->
        [ heading "F# inside"
          p [] []
          img [ Src "images/safe-logo.png" ] ]

    | Page.Start ->
        match model.Data with
        | DataModel (BasicModel data) ->
            [ heading "picture"
              subheading "type Picture = Box -> Rendering"
              spacing
              data |> Start.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "start"

    | Page.Turn ->
        match model.Data with
        | DataModel (BasicModel data) ->
            [ heading "turn"
              subheading "(a’, b’, c’) = (a + b, c, -b)"
              spacing
              data |> Turn.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "turn"

    | Page.Flip ->
        match model.Data with
        | DataModel (BasicModel data) ->
            [ heading "flip"
              subheading "(a’, b’, c’) = (a + b, -b, c)"
              spacing
              data |> Flip.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "flip"

    | Page.Toss ->
        match model.Data with
        | DataModel (BasicModel data) ->
            [ heading "toss"
              subheading "(a’, b’, c’) = (a + (b + c) / 2, (b + c) / 2, (c − b) / 2)"
              spacing
              data |> Toss.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "toss"

    | Page.Above ->
        match model.Data with
        | DataModel (BasicModel data) ->
            [ heading "above"
              subheading "put first picture above second picture"
              spacing
              data |> Above.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "above"

    | Page.Beside ->
        match model.Data with
        | DataModel (BasicModel data) ->
            [ heading "beside"
              subheading "put first picture to the left of second picture"
              spacing
              data |> Beside.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "beside"

    | Page.Quartet ->
        match model.Data with
        | DataModel (BasicModel data) ->
            [ heading "quartet"
              subheading "create a quartet of four pictures"
              spacing
              data |> Quartet.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "quartet"

    | Page.Nonet ->
        match model.Data with
        | DataModel (NonetModel data) ->
            [ heading "nonet"
              subheading "create a nonet of nine pictures"
              spacing
              data |> Nonet.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "nonet"

    | Page.Over ->
        match model.Data with
        | DataModel (FishModel data) ->
            [ heading "over"
              subheading "overlay two pictures inside the same box"
              spacing
              data |> Over.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "over"

    | Page.Ttile ->
        match model.Data with
        | DataModel (FishModel data) ->
            [ heading "ttile"
              subheading "create the t-tile in square limit"
              spacing
              data |> Ttile.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "ttile"

    | Page.Utile ->
        match model.Data with
        | DataModel (FishModel data) ->
            [ heading "utile"
              subheading "create the u-tile in square limit"
              spacing
              data |> Utile.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "utile"

    | Page.Side ->
        match model.Data with
        | DataModel (FishModel data) ->
            [ heading "side"
              spacing
              data |> Side.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "side"

    | Page.Corner ->
        match model.Data with
        | DataModel (FishModel data) ->
            [ heading "corner"
              spacing
              data |> Corner.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "corner"

    | Page.Limit ->
        match model.Data with
        | DataModel (FishModel data) ->
            [ heading "square limit"
              subheading "Henderson's replica of square limit"
              data |> Limit.transform |> Rendering.Transforms.view ]
        | _ -> matchfail "square limit"

    | Page.Hue ->
        match model.Data with
        | Colorite (Fish3Model data) ->
            [ heading "hue"
              spacing
              data |> Hue.transform |> Rendering.Reform.view ]
        | _ -> matchfail "hue"

    | Page.Xttile ->
        match model.Data with
        | Colorite (Fish3Model data) ->
            [ heading "ttile"
              subheading "create the t-tile in square limit"
              spacing
              data |> Xttile.transform |> Rendering.Reform.view ]
        | _ -> matchfail "xttile"

    | Page.Xutile ->
        match model.Data with
        | Colorite (Fish3Model data) ->
            [ heading "utile"
              subheading "create the u-tile in square limit"
              spacing
              data |> Xutile.transform |> Rendering.Reform.view ]
        | _ -> matchfail "xutile"

    | Page.Xside ->
        match model.Data with
        | Colorite (Fish3Model data) ->
            [ heading "side"
              spacing
              data |> Xside.transform |> Rendering.Reform.view ]
        | _ -> matchfail "xside"

    | Page.Xcorner ->
        match model.Data with
        | Colorite (Fish3Model data) ->
            [ heading "corner"
              spacing
              data |> Xcorner.transform |> Rendering.Reform.view ]
        | _ -> matchfail "xcorner"

    | Page.Xlimit ->
        match model.Data with
        | Colorite (Fish3Model data) ->
            [ heading "square limit"
              subheading "Tricolor replica of square limit"
              spacing
              data |> Xlimit.transform |> Rendering.Reform.view ]
        | _ -> matchfail "xlimit"

    | _ -> matchfail "what page is this?"

let view (model : Model) (dispatch : Msg -> unit) =
    let next =
        let link = match model.Next with | Some pg -> viewLink pg "=>" | None -> str ""
        span [ Style [ Float "right"] ] [ link ]
    let prev =
        let link = match model.Prev with | Some pg -> viewLink pg "<=" | None -> str ""
        span [ Style [ Float "left"] ] [ link ]
    div []
        [ div [ Style [ BackgroundColor "black"; Color "white"; Padding "4px" ] ]
              [ span [ Style [ Float "right"] ] [ str "@einarwh" ]
                span [] [ str "A Fable about Fish" ] ]
          div [ centerStyle "column" ]
              (viewPage model dispatch)
          div [ Style [ BackgroundColor "black"; Color "white"; Padding "4px"; Position "fixed"; Bottom "0px"; ZIndex 100.; Width "100%" ] ]
              [ next; prev ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.toNavigable Pages.urlParser urlUpdate
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
