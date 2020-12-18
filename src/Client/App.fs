module Client.App

open Fable.React
open Elmish
open Elmish.React
open Fetch.Types
open Thoth.Fetch
open Feliz
open Feliz.MaterialUI
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
    { model with PageModel = EditorModel guidOption }, Cmd.none
  | Some Page.Entries ->
    { model with PageModel = EntriesModel }, Cmd.none
  | Some Page.Home ->
    let subModel, cmd = Client.Home.init None
    { model with PageModel = HomeModel }, Cmd.none
  | Some Page.FlashCards ->
    { model with PageModel = FlashCardsModel }, Cmd.none
  | Some (Page.Practice guidOption) ->
    { model with PageModel = PracticeModel guidOption }, Cmd.none

let gapiImported: obj = Fable.Core.JsInterop.importAll "./platform.js"

let init page : Shared.Domain.Model * Cmd<Shared.Domain.Msg> =
  let model = { MenuModel = { User = None; IsBurgerOpen = false; }; IsLoading = true; PageModel = HomeModel; Entries = []; }
  urlUpdate page model
  
let update (msg : Shared.Domain.Msg) (model : Shared.Domain.Model) : Shared.Domain.Model * Cmd<Shared.Domain.Msg> =
  match msg, model.PageModel with
  | MenuBurgerToggled (),_ ->
    { model with MenuModel = { model.MenuModel with IsBurgerOpen = not model.MenuModel.IsBurgerOpen }}, Cmd.none
  | TokenReceived t,_ ->
    match model.MenuModel.User with
    | Some u ->
      let cmd =   
        match u.Provider, model.PageModel with
        | Google g, EntriesModel _
        | Google g, HomeModel _ -> 
          [ //Cmd.OfPromise.perform Api.getEntries { u with MemoriaToken = Some t.Token } (Entries.Msg.EntriesLoaded >> EntriesMsg)
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
  | AuthConfigured,_ ->
    { model with IsLoading = false; }, Cmd.none
  //   | StorageFailure _ ->
  //     model, Cmd.none

open Fable.Core.JsInterop
open Fable.Core
open Fable.Import
open Fable.Core.JS
open Fable.MaterialUI.MaterialDesignIcons

let viewContent (model: Model) dispatch =
  Html.div [
    match model.PageModel with
    | EditorModel m -> Editor.view {| userOption = model.MenuModel.User; entryIdOption = m; entries = model.Entries |}
    | EntriesModel -> Entries.view {| userOption = model.MenuModel.User; |}
    | HomeModel -> Home.view {| userOption = model.MenuModel.User; |}
    | PracticeModel m -> Practice.view {| userOption = model.MenuModel.User; entryIdOption = m; entries = model.Entries |}
    | FlashCardsModel -> FlashCards.view {| abandonComponent = (fun () -> ()) |} //m (FlashCardsMsg >> dispatch)
  ]

let toolbarView model dispatch =
  let classes = Styles.useStyles ()
  Mui.toolbar [
    Html.a [
      prop.href "/"
      prop.children [
        Html.img [
          prop.style [ 
            style.width (length.em 2.5) 
            style.marginRight (20)
          ]
          prop.src "shape.svg"        
        ]
      ]
    ]
    Mui.typography [
      typography.variant.h3
      typography.color.inherit'
      typography.children "memoria"
      typography.classes.root classes.appBarTitle
    ]
    Mui.button [
      button.color.inherit'
      match model.MenuModel.User with
      | Some _ -> 
        prop.onClick (fun _ -> Auth.signOut (LoggedOut >> dispatch))
        button.children "Sign out"
      | None -> 
        prop.href "/app.html#home"
        button.children "Sign In"
    ]
  ]

let App = FunctionComponent.Of((fun (model, dispatch) ->
  let classes = Styles.useStyles ()
  let hideLogin =
    match model.PageModel, model.MenuModel.User with
    | PageModel.HomeModel _, None -> false
    | _,_ -> true
  Mui.themeProvider [
    themeProvider.theme Theme.memoriaTheme
    themeProvider.children [
      Mui.cssBaseline []
      Html.div [ 
        prop.className classes.root
        prop.key "Application"
        prop.onLoad (fun _ -> Auth.init (fun () -> dispatch AuthConfigured) (SignedIn >> dispatch); )
        prop.children [
          Mui.cssBaseline []
          Mui.backdrop [
            backdrop.open' model.IsLoading
          ]
          Mui.appBar [
            appBar.classes.root classes.appBar
            appBar.color.default'
            appBar.position.fixed'
            appBar.children [
              toolbarView model dispatch
            ]
          ]              
          Html.main [
            prop.className classes.content
            prop.children [
              Html.div [ prop.className classes.toolbar ]
              Html.div [
                prop.hidden hideLogin 
                prop.children [
                  Html.div [
                    Mui.grid [
                      grid.container true
                      grid.children [ 
                        Mui.grid [
                          grid.item true
                          grid.children [
                            Html.div [ 
                              prop.id "g-signin-btn" 
                            ]
                          ]
                        ]
                        Mui.grid [
                          grid.item true
                          grid.children [
                            Mui.button [
                              prop.type'.submit
                              button.fullWidth true
                              button.variant.contained
                              button.color.primary
                              button.children "View as sample user"
                              button.startIcon (loginIcon [])
                              prop.onClick (fun _ -> dispatch (SignedIn Sample))
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]    
              viewContent model dispatch 
            ]
          ]
        ]
      ]
    ]
  ]
), "App", memoEqualsButFunctions)

let view model dispatch =
  App (model, dispatch)

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
