module Client.Styles

open Fable.React.Props
open System
open Fable.Core.JsInterop
open Fable.Import
open Elmish.Navigation
open Fable.React
open Fulma
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

let onEnter msg dispatch =
    function
    | (ev:Browser.Types.KeyboardEvent) when ev.keyCode = 13. ->
        ev.preventDefault()
        dispatch msg
    | _ -> ()
    |> OnKeyDown

let errorBox msg =
    div [] [
        match msg with
        | None -> ()
        | Some e -> yield p [ClassName "text-danger"][str e]
    ]

let validatedTextBox (onChange: string -> unit) key placeholder errorText text =
    let status = if String.IsNullOrEmpty text then "" else "has-success"
    div [ClassName ("form-group has-feedback " + status)] [
         yield div [ClassName "input-group"] [
             yield span [ClassName "input-group-addon"] [span [ClassName "glyphicon glyphicon glyphicon-pencil"] [] ]
             yield input [
                    Key key
                    Name key
                    HTMLAttr.Type "text"
                    DefaultValue text
                    ClassName "form-control"
                    Placeholder placeholder
                    OnChange (fun ev -> onChange ev.Value)]
             match errorText with
             | Some _e -> yield span [ClassName "glyphicon glyphicon-remove form-control-feedback"] []
             | _ -> ()
         ]
         match errorText with
         | Some e -> yield p [ClassName "text-danger"][str e]
         | _ -> ()
    ]

let iconButton txt (fn : _ -> unit) icon =
  Button.button 
    [ Button.OnClick fn ]
    [ Icon.icon [ ] [ icon [] ]
      if String.IsNullOrWhiteSpace txt then () else span [] [ str txt ] ] 

let adminOnly (appUserOption : AppUser option) element =
  match appUserOption with
  | Some u when u.Role = Some Admin -> element
  | _ -> div [ ] [ ]

let subscriberOnly (appUserOption : AppUser option) element =
  match appUserOption with
  | Some u when u.Role = Some Admin || u.Role = Some Subscriber -> element
  | _ -> div [ ] [ ]