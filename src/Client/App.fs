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
open Client.Pages
open Feliz.Router

type UserState =
  | UserFound of AppUser
  | LookingUpUser
  | UserNotFound
  | Initializing
  | LoggedOut

type MenuModel = 
  { User : UserState
    IsBurgerOpen : bool }
type MenuProps = 
  { Model : MenuModel
    OnLogout : unit -> unit
    OnToggleBurger: unit -> unit }
    
type Model = 
  { MenuModel : MenuModel
    IsLoading : bool
    Entries : MemorizationEntryDisplay list
    Page : Page }

/// The composed set of messages that update the state of the application
type Msg =
  | DemoteUser
  | SignedIn of UserProvider
  | SignedOut
  | LoggedOut of unit
  | AuthDisconnected
  // | StorageFailure of exn
  | TokenReceived of TokenResult
  | MenuBurgerToggled of unit
  | AuthConfigured
  | PageChanged of Page
  
let handleNotFound (model: Model) =
    //JS.console.error("Error parsing url: " + Browser.Dom.window.location.href)
    ( model, Cmd.navigate(toHash Page.Home) )

let urlUpdate (result:Page option) (model:Model) =
  match result with
  | None ->
      handleNotFound model
  | Some p ->
    { model with Page = p }, Cmd.none

let gapiImported: obj = Fable.Core.JsInterop.importAll "./platform.js"

let init () : Model * Cmd<Msg> =
  let urlSegments = Router.currentUrl()
  let page = Pages.parseUrl urlSegments
  let model = { MenuModel = { User = UserState.Initializing; IsBurgerOpen = false; }; IsLoading = true; Page = page; Entries = []; }
  model, Cmd.none
  
let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
  match msg, model.Page with
  | MenuBurgerToggled (),_ ->
    { model with MenuModel = { model.MenuModel with IsBurgerOpen = not model.MenuModel.IsBurgerOpen }}, Cmd.none
  | TokenReceived t,_ ->
    match model.MenuModel.User with
    | UserState.UserFound u ->
      let cmd =   
        match u.Provider, model.Page with
        | Google g, Page.Entries _
        | Google g, Page.Home -> 
          [ //Cmd.OfPromise.perform Api.getEntries { u with MemoriaToken = Some t.Token } (Entries.Msg.EntriesLoaded >> EntriesMsg)
            Cmd.navigate(toHash Page.Entries) ]
          |> Cmd.batch
        | _ -> Cmd.none
      { model with MenuModel = { model.MenuModel with User = UserState.UserFound { u with MemoriaToken = Some t.Token; Role = t.Role; } } }, cmd
    | _ ->  model, Cmd.none
  | DemoteUser,_ ->
    match model.MenuModel.User with
    | UserState.UserFound u ->
      { model with MenuModel = { model.MenuModel with User = UserState.UserFound { u with Role = None; } } }, Cmd.none
    | _ -> model, Cmd.none
  | AuthDisconnected,_
  | LoggedOut _,_
  | SignedOut,_ ->
    { model with MenuModel = { model.MenuModel with User = UserState.LoggedOut; } }, Cmd.none
  | SignedIn p,_ ->
    let cmd =
      match p with
      | Sample -> Cmd.navigate(toHash Page.Entries)
      | Google g -> Cmd.OfPromise.perform Api.getToken { IdToken = g.Token } TokenReceived
    { model with MenuModel = { model.MenuModel with User = UserState.UserFound { AppUser.Provider = p; MemoriaToken = None; Role = None; } }; }, cmd
  | AuthConfigured,_ ->
    { model with IsLoading = false; }, Cmd.none
  //   | StorageFailure _ ->
  //     model, Cmd.none
  | PageChanged p, _ ->
    { model with Page = p }, Cmd.none

open Fable.Core.JsInterop
open Fable.Core
open Fable.Import
open Fable.Core.JS
open Fable.MaterialUI.MaterialDesignIcons

let viewContent (model: Model) dispatch =
  let user =
    match model.MenuModel.User with
    | UserState.UserFound u -> Some u
    | _ -> None
  Html.div [
    match model.Page with
    | Page.Editor m -> Editor.view {| userOption = user; entryIdOption = m; entries = model.Entries |}
    | Page.Entries -> Entries.view {| userOption = user; |}
    | Page.Home -> Home.view {| userOption = user; |}
    | Page.Practice m -> Practice.view {| userOption = user; entryIdOption = m; entries = model.Entries |}
    | Page.FlashCards -> FlashCards.view {| abandonComponent = (fun () -> ()) |} //m (FlashCardsMsg >> dispatch)
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
      | UserState.UserFound _ -> 
        prop.onClick (fun _ -> Auth.signOut (LoggedOut >> dispatch))
        button.children "Sign out"
      | _ -> 
        prop.href "/app.html#home"
        button.children "Sign In"
    ]
  ]

let App = FunctionComponent.Of((fun (model, dispatch) ->
  let classes = Styles.useStyles ()
  let hideLogin =
    match model.Page, model.MenuModel.User with
    | _, UserState.Initializing
    | _, UserState.LookingUpUser
    | Page.FlashCards, _
    | _, UserState.UserFound _ -> true
    | _,_ -> false
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
  React.router [
      router.onUrlChanged (parseUrl >> PageChanged >> dispatch)
      router.children [
        App (model, dispatch)
      ]
  ]

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
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
