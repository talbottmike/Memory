module Client.App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Fulma.Extensions.Wikiki
open Thoth.Json
open System
open Fable.Core
open Shared.Domain
open Shared.Helpers
open Elmish.Navigation
open Client.Pages

let handleNotFound (model: Shared.Domain.Model) =
    //JS.console.error("Error parsing url: " + Browser.Dom.window.location.href)
    ( model, Navigation.newUrl (toHash Page.Home) )

let urlUpdate (result:Page option) (model:Shared.Domain.Model) =
  match result with
  | None ->
      handleNotFound model
  | Some (Page.Editor guidOption) ->
    let subModel, cmd = 
      match model.PageModel with
      | PageModel.EntriesModel e ->
        Client.Editor.init model.MenuModel.User guidOption e.Entries
      | _ -> Client.Editor.init model.MenuModel.User guidOption []
    { model with PageModel = EditorModel subModel }, Cmd.map EditorMsg cmd
  | Some Page.Entries ->
    let subModel, cmd = Client.Entries.init model.MenuModel.User
    { model with PageModel = EntriesModel subModel }, Cmd.map EntriesMsg cmd
  | Some Page.Home ->
    let subModel, cmd = Client.Home.init None
    { model with PageModel = HomeModel subModel }, Cmd.map HomeMsg cmd
  | Some (Page.Practice guidOption) ->
    let subModel, cmd = 
      match model.PageModel with
      | PageModel.EntriesModel e ->
        Client.Practice.init model.MenuModel.User guidOption e.Entries
      | _ -> Client.Practice.init model.MenuModel.User guidOption []
    { model with PageModel = PracticeModel subModel }, Cmd.map PracticeMsg cmd

let gapiImported: obj = Fable.Core.JsInterop.importAll "./platform.js"

let init page : Shared.Domain.Model * Cmd<Shared.Domain.Msg> =
  let model = { MenuModel = { User = None }; PageModel = HomeModel Home.Model.Empty }
  urlUpdate page model
  
let update (msg : Shared.Domain.Msg) (model : Shared.Domain.Model) : Shared.Domain.Model * Cmd<Shared.Domain.Msg> =
  match msg, model.PageModel with
  | EditorMsg msg, EditorModel m ->
      let m, cmd = Editor.update msg m
      { model with
          PageModel = EditorModel m }, Cmd.map EditorMsg cmd
  | EditorMsg _, _ -> model, Cmd.none
  | EntriesMsg msg, EntriesModel m ->
      let m, cmd = Entries.update msg m
      { model with
          PageModel = EntriesModel m }, Cmd.map EntriesMsg cmd
  | EntriesMsg _, _ -> model, Cmd.none
  | HomeMsg msg, HomeModel m ->
    let m, cmd = Home.update msg m
    { model with
        PageModel = HomeModel m }, Cmd.map HomeMsg cmd
  | HomeMsg _, _ -> model, Cmd.none
  | PracticeMsg msg, PracticeModel m ->
      let m, cmd = Practice.update msg m
      { model with
          PageModel = PracticeModel m }, Cmd.map PracticeMsg cmd
  | PracticeMsg _, _ -> model, Cmd.none
  | TokenReceived t,_ ->
    match model.MenuModel.User with
    | Some u ->
      let cmd =   
        match u.Provider, model.PageModel with
        | Google g, HomeModel _ -> 
          [ Cmd.OfPromise.perform Api.googleEntries { g with MemoriaToken = Some t.Token } (Entries.Msg.EntriesLoaded >> EntriesMsg)
            Navigation.newUrl (toHash Page.Entries) ]
          |> Cmd.batch
        | _ -> Cmd.none
      { model with MenuModel = { model.MenuModel with User = Some { u with MemoriaToken = Some t.Token; Role = t.Role; } } }, cmd
    | _ ->  model, Cmd.none
  | DemoteUser,_ ->
    model.MenuModel.User
    |> Option.map (fun x -> { model with MenuModel = { model.MenuModel with User = Some { x with Role = None; } } } )
    |> Option.defaultValue model, Cmd.none
  | AuthDisconnected,_
  | LoggedOut _,_
  | SignedOut,_ ->
    { model with MenuModel = { model.MenuModel with User = None; } }, Cmd.none
  | SignedIn p,_ ->
    let cmd =
      match p with
      | Sample -> Navigation.newUrl (toHash Page.Entries)
      | Google g -> Cmd.OfPromise.perform Api.getToken { IdToken = g.Token } TokenReceived
    { model with MenuModel = { model.MenuModel with User = Some { AppUser.Provider = p; MemoriaToken = None; Role = None; } }; }, cmd
  //   | StorageFailure _ ->
  //     model, Cmd.none

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

open Fable.Core.JsInterop
open Fable.Core
open Fable.Import
open Fable.Core.JS
open Fable.MaterialUI.MaterialDesignIcons

let [<Literal>] ESC_KEY = 27.
let [<Literal>] ENTER_KEY = 13.

let internal onEnter msg dispatch =
  function
  | (ev:Browser.Types.KeyboardEvent) when ev.keyCode = ENTER_KEY ->
    ev.target?value <- ""
    dispatch msg
  | _ -> ()
  |> OnKeyDown

let viewContent (model: Model) dispatch =
  Section.section [ ] [
    match model.PageModel with
    | EditorModel m -> Editor.view m (EditorMsg >> dispatch)
    | EntriesModel m -> Entries.view m (EntriesMsg >> dispatch)
    | HomeModel m -> 
      Section.section [(if model.MenuModel.User.IsSome then Section.Props [ Style [ Props.Display DisplayOptions.None ] ] else Section.Props [ ] ) ] [ Container.container [ ]
        [ Columns.columns [ Columns.IsCentered ]
            [ Column.column [ Column.Width (Screen.All, Column.IsNarrow) ]
                [ div [ Id "g-signin-btn" ] [ ] ] ]
          Columns.columns [ Columns.IsCentered ]
            [ Column.column [ Column.Width (Screen.All, Column.IsNarrow) ]
                [ Styles.iconButton "View as sample user" (fun _ -> dispatch (SignedIn Sample)) loginIcon ] ] ] ]
      Home.view { Home.Props.Model = m; Home.Props.Dispatch = (HomeMsg >> dispatch); }
    | PracticeModel m -> Practice.view m (PracticeMsg >> dispatch)
  ]

open Fable.React
open Fable.React.Props
open Client.Styles

let view model dispatch =
  div [ Key "Application"; OnLoad (fun _ -> Auth.init dispatch; ) ] [
    Menu.view { Model = model.MenuModel; OnLogout = (LoggedOut >> dispatch) }
    viewContent model dispatch ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

#if !DEBUG
open Fable.Core
open Fable.Core.JsInterop

let [<Global>] navigator: obj = jsNative
let [<Emit("$0 in $1")>] hasField (key: string) (o: obj): bool = jsNative
if hasField "serviceWorker" navigator then
  JS.console.log "Registering service worker"
  navigator?serviceWorker?register("./service-worker.js")
else
  JS.console.log "NOT Registering service worker"
#endif

Program.mkProgram init update view
|> Program.toNavigable Pages.urlParser urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
