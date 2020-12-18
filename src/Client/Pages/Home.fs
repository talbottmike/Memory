module Client.Home
open Fable.Core
open Fable.Core.JsInterop
open System
open Shared.Domain
open Shared.Domain.Home
open Elmish
open Fable.MaterialUI.MaterialDesignIcons
open Client.Styles
open Client.Utils
open Feliz
open Feliz.UseElmish
open Thoth.Json

let init userOption = 
  let cmd =
    match userOption with
    | None -> Cmd.none
    | Some u -> Cmd.none
  { User = userOption }, cmd

let update (msg:Home.Msg) model : Home.Model*Cmd<Home.Msg> =
  model, Cmd.none

let view = React.functionComponent (fun (input: {| userOption: AppUser option; |}) ->
  let model, dispatch = React.useElmish(init input.userOption, update, [| |])
  Html.div [
        // div [ ] [ a [ Href "#"; OnClick (fun _ -> authDisconnect (fun () -> dispatch AuthDisconnected)) ] [ str "Disconnect" ] ]
    ]
)