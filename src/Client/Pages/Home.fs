module Client.Home
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open System
open Shared.Domain
open Shared.Domain.Home
open Elmish
open Fulma
open Fulma.Extensions.Wikiki
open Fable.MaterialUI.MaterialDesignIcons
open Client.Styles
open Client.Utils
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

let init userOption = 
  let cmd =
    match userOption with
    | None -> Cmd.none
    | Some u -> Cmd.none
  { User = userOption }, cmd

let update (msg:Home.Msg) model : Home.Model*Cmd<Home.Msg> =
  model, Cmd.none

let view = elmishView "Home" (fun { Model = model; Dispatch = dispatch; } ->
  div [ ]
        [ // div [ ] [ a [ Href "#"; OnClick (fun _ -> authDisconnect dispatch) ] [ str "Disconnect" ] ]
        ]
)