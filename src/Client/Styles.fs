module Client.Styles

open Fable.React.Props
open System
open Fable.Core.JsInterop
open Fable.Import
open Elmish.Navigation
open Fable.React
open Feliz
open Feliz.MaterialUI
open Shared.Domain

let goToUrl (e: Browser.Types.MouseEvent) =
    e.preventDefault()
    let href = !!e.target?href
    Navigation.newUrl href |> List.map (fun f -> f ignore) |> ignore

let viewLink page description =
    a [ Style [ Padding "0 20px" ]
        Href (Pages.toHash page)
        OnClick goToUrl ]
        [ str description ]

let centerStyle direction =
    Style [ Display DisplayOptions.Flex
            FlexDirection direction
            AlignItems AlignItemsOptions.Center
            JustifyContent "center"
            Padding "20px 0"
    ]

let words size message =
    span [ Style [ FontSize (size |> sprintf "%dpx") ] ] [ str message ]

let buttonLink cssClass onClick elements =
    a [ ClassName cssClass
        OnClick (fun _ -> onClick())
        OnTouchStart (fun _ -> onClick())
        Style [ Cursor "pointer" ] ] elements

let menuLink onClick description =
    a [ Style [ Padding "0 20px" ]
        OnClick (fun _ -> onClick())
        OnTouchStart (fun _ -> onClick()) ]
        [ str description ]

let errorBox msg =
    div [] [
        match msg with
        | None -> ()
        | Some e -> yield p [ClassName "text-danger"][str e]
    ]

let iconButton (txt : string) (fn : _ -> unit) icon =
  Mui.button [
    // prop.type'.submit
    button.fullWidth true
    button.variant.contained
    button.color.primary
    // button.classes.root classes.submit
    if String.IsNullOrWhiteSpace txt then () 
    else button.children txt
    button.endIcon icon
    prop.onClick fn
  ]

let adminOnly (appUserOption : AppUser option) element =
  match appUserOption with
  | Some u when u.Role = Some Admin -> element
  | _ -> div [ ] [ ]

let subscriberOnly (appUserOption : AppUser option) element =
  match appUserOption with
  | Some u when u.Role = Some Admin || u.Role = Some Subscriber -> element
  | _ -> div [ ] [ ]


let useStyles : unit -> _ = 
  Styles.makeStyles(fun styles theme ->
    let drawerWidth = 240
    {|
      root = styles.create [
        style.display.flex
      ]
      appBar = styles.create [
        style.zIndex (theme.zIndex.drawer + 1)
      ]
      appBarTitle = styles.create [
        style.flexGrow 1
      ]
      drawer = styles.create [
        style.width (length.px drawerWidth)
        style.flexShrink 0  // TODO: Does this do anything?
      ]
      drawerPaper = styles.create [
        style.width (length.px drawerWidth)
      ]
      content = styles.create [
        style.width 0  // TODO: is there a better way to prevent long code boxes extending past the screen?
        style.flexGrow 1
        style.padding (theme.spacing 3)
      ]
      nestedMenuItem = styles.create [
        style.paddingLeft (theme.spacing 4)
      ]
      toolbar = styles.create [
        yield! theme.mixins.toolbar
      ]
      flashcardFront = styles.create [
        style.backgroundColor theme.palette.primary.main
      ]
      flashcardBack = styles.create [
        style.backgroundColor theme.palette.secondary.main
      ]
    |}
  )