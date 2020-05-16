module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json
open System
open Fable.FontAwesome
open Fable.Core

open Shared
open Shared.Helpers

let gapiImported: obj = Fable.Core.JsInterop.importAll "./platform.js"

let loadBrowserEntries () : MemorizationEntryDisplay list =
  let entryListDecoder = Decode.Auto.generateDecoder<MemorizationEntryDisplay list>()
  match WebStorage.load entryListDecoder "entryList" with
  | Ok entries -> entries
  | Error _ -> [ ]

let baseUrl =
  if Browser.Dom.window.location.host.ToUpper().Contains("MEMORIAMASTERED")
  then "https://memoria.azurewebsites.net/"
  else Browser.Dom.window.location.href

let getToken (tokenRequest : GoogleLoginRequest) = 
  JS.console.log "Get token run"
  let r = {| IdToken = tokenRequest.IdToken |}
  Fetch.post<_, TokenResult> (baseUrl + "api/token", data = r)

let addEntry (token, request : MemorizationEntry) =
  let authenticatedJsonHeaders =
      [ HttpRequestHeaders.Authorization (sprintf "Bearer %s" token)
        HttpRequestHeaders.ContentType "application/json" ]
  let r = request
  Fetch.post<_, MemorizationEntry> (baseUrl + "api/add", data = r, headers = authenticatedJsonHeaders)

let sampleEntries () = Fetch.fetchAs<unit, MemorizationEntry list> "/sample.json"
let googleEntries (g : GoogleUser) = 
  let authenticatedJsonHeaders =
      g.MemoriaToken 
      |> Option.map (sprintf "Bearer %s" >> HttpRequestHeaders.Authorization) 
      |> Option.toList 
      |> List.append [ HttpRequestHeaders.ContentType "application/json" ]
  Fetch.fetchAs<unit, MemorizationEntry list>(baseUrl + "api/init",headers = authenticatedJsonHeaders)

module Auth =
  open Fable.Core.JsInterop

  let init dispatch = 
    Fable.Core.JS.console.log("init called")
    let onSignIn g = 
      // JS.console.log("on sign in, granted scopes: ")
      // JS.console.log(g?getGrantedScopes())
      let token = g?getAuthResponse()?id_token
      let profile = g?getBasicProfile()
      let profileId = profile?getId()
      let profileName =  profile?getName()
      let profileEmail =  profile?getEmail()
      // JS.console.log(profile)
      let user = 
        { Token = token.ToString()
          Id = profileId.ToString()
          Name = profileName.ToString()
          Email = profileEmail.ToString()
          MemoriaToken = None }
        |> Google
      dispatch (SignedIn user)

    let configureAuth () =
      Browser.Dom.window?gapi?auth2?init(
        {|
          client_id = "189067839764-1i0jqhpp1igdf0cdenghhs6f09spbseu.apps.googleusercontent.com"
          fetch_basic_profile = false
          scope = "profile email openid"
        |}
      )
      let config = 
        {|
          scope = "profile email openid"
          width = 200
          height = 36
          longtitle = true
          theme = "light"
          onsuccess = onSignIn
          onfailure = null
        |}
      Browser.Dom.window?gapi?signin2?render("g-signin-btn", config);
    Browser.Dom.window?gapi?load("auth2", configureAuth)
    ()

  let disconnect dispatch =
    let auth2 = Browser.Dom.window?gapi?auth2?getAuthInstance()
    let isUserSignedIn = auth2?isSignedIn?get()
    if ((bool) isUserSignedIn)
    then
      auth2?disconnect()
    else 
      //JS.console.log("Not signed in, cannot disconnect")
      ()
    dispatch AuthDisconnected

  let signOut dispatch =
    let auth2 = Browser.Dom.window?gapi?auth2?getAuthInstance()
    let signOutFn _ =
      dispatch SignedOut
    auth2?signOut()?``then``(signOutFn)

let init () : Model * Cmd<Msg> =
  let entries = loadBrowserEntries ()
  let model = { User = None; Editor = None; Entries = entries; CurrentEntry = None; }
  model, Cmd.none
  
let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
  match msg with
  | DemoteUser ->
    model.User
    |> Option.map (fun x -> { model with User = Some { x with Role = None; } } )
    |> Option.defaultValue model, Cmd.none
  | EntryAddedToDatabase ->
    model, Cmd.none
  | TokenReceived t ->
    match model.User with
    | Some u ->
      let cmd =   
        match u.Provider with
        | Google g -> Cmd.OfPromise.perform googleEntries { g with MemoriaToken = Some t.Token } EntriesLoaded
        | _ -> Cmd.none
      { model with User = Some { u with MemoriaToken = Some t.Token; Role = t.Role; } }, cmd
    | _ ->  model, Cmd.none
  | AuthDisconnected
  | SignedOut ->
    { model with User = None; }, Cmd.none
  | SignedIn p ->
    let cmd =
      match p with
      | Sample -> Cmd.none
      | Google g -> Cmd.OfPromise.perform getToken { IdToken = g.Token } TokenReceived
    { model with User = Some { AppUser.Provider = p; MemoriaToken = None; Role = None; }; }, cmd
  | SelectEntry guid ->
    { model with CurrentEntry = Some guid; }, Cmd.none
  | UpdateText t ->
    let newEditor =
      model.Editor
      |> Option.map (fun e -> { e with Text = t })
    { model with Editor = newEditor; }, Cmd.none
  | UpdateTitle t ->
    let newEditor =
      model.Editor
      |> Option.map (fun e -> { e with Title = t })
    { model with Editor = newEditor; }, Cmd.none
  | RemoveEntry guid ->
    let newModel =
      let newEntries = model.Entries |> List.filter (fun x -> guid <> x.Id)
      { model with Editor = None; Entries = newEntries; CurrentEntry = None; }
    newModel, Cmd.ofMsg SaveEntries
  | HintLevelChanged hintLevel ->
    let newModel =
      match model.CurrentEntry with
      | None -> model
      | Some guid ->
        let newEntries =
          model.Entries |> List.map (fun x -> if guid = x.Id then { x with HintLevel = Some hintLevel; } else x)
        { model with Entries = newEntries }
    newModel, Cmd.none
  | AddOrUpdateEntry ->
    let newModel =
      match model.Editor with
      | None -> 
        model
      | Some e -> 
        let newCurrentEntry, newEntries =
          match e.EntryId with
          | None -> 
            match strOption e.Title, strOption e.Text with
            | None, None -> None, model.Entries 
            | _, _ -> 
              let id = Guid.NewGuid()
              let entries = { Id = id; Title = e.Title; Text = e.Text; TextParts = getTextParts e.Text; HintLevel = None; } :: model.Entries
              (Some id, entries)
          | Some guid -> 
            match strOption e.Title, strOption e.Text with
            | None, None -> 
              let entries = model.Entries |> List.filter (fun x -> guid <> x.Id)
              (None, entries)
            | _, _ -> 
              let entries = model.Entries |> List.map (fun x -> if guid = x.Id then { x with Title = e.Title; Text = e.Text; TextParts = getTextParts e.Text; } else x)
              (Some guid, entries)
        { model with Editor = None; Entries = newEntries; }
    newModel, Cmd.ofMsg SaveEntries
  | BulkToggleTextView textView ->
    let newModel =
      match model.CurrentEntry with
      | None -> model
      | Some guid ->
        let newEntries =
          let newTextParts (textParts : TextPart list) = textParts |> List.map (fun x -> if x.TextType = TextType.Word then { x with TextView = textView } else x)
          model.Entries |> List.map (fun x -> if guid = x.Id then { x with TextParts = newTextParts x.TextParts; } else x)
        { model with Entries = newEntries }
    newModel, Cmd.none
  | ToggleTextView request ->
    let newModel =
      match model.CurrentEntry with
      | None -> model
      | Some guid ->
        let newEntries =
          let newTextParts (textParts : TextPart list) = textParts |> List.map (fun x -> if x.Id = request.Id then { x with TextView = toggleTextView x } else x)
          model.Entries |> List.map (fun x -> if guid = x.Id then { x with TextParts = newTextParts x.TextParts; } else x)
        { model with Entries = newEntries }
    newModel, Cmd.none
  | AddEntry ->
    { model with Editor = Some { EntryId = None; Text = ""; Title = ""; }}, Cmd.none
  | UpdateEntry guid ->
    let editor = 
      model.Entries 
      |> List.tryFind (fun x -> x.Id = guid)
      |> Option.map (fun x -> { EntryId = Some x.Id; Text = x.Text; Title = x.Title; })
    { model with Editor = editor; }, Cmd.none
  | ViewList ->
    { model with Editor = None; CurrentEntry = None; }, Cmd.none
  | EntriesLoaded e ->
    let newEntries = e |> List.filter (fun x -> model.Entries |> List.exists (fun y -> y.Id = x.Id) |> not) |> List.map (fun x -> { Id = x.Id; Title = x.Title; Text = x.Text; TextParts = getTextParts x.Text; HintLevel = None; })
    { model with Entries = model.Entries |> List.append newEntries; }, Cmd.none
  | SaveEntries -> 
    let cmds =
      let browserSave = Cmd.OfFunc.either (WebStorage.save "entryList") model.Entries (fun _ -> SavedEntries) StorageFailure
      let dbSave =
        match model.User with
        | Some u ->
          match u.Role, u.MemoriaToken with
          | Some Admin, Some t ->
            let entry = model.Entries |> List.map (fun x -> { Id = x.Id; Title = x.Title; Text = x.Text }) |> List.head
            Cmd.OfPromise.perform addEntry (t, entry) (fun _ -> EntryAddedToDatabase) |> Some
          | _ -> None
        | _ -> None
      [ Some browserSave
        dbSave ]
      |> List.choose id
    model, Cmd.batch cmds
  | SavedEntries
  | StorageFailure _ ->
    model, Cmd.none

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

let viewTextPart (x : TextPart) (dispatch : Msg -> unit) =
  let t =
    match x.TextType, x.TextView with
    | TextType.Number, _ -> x.Text
    | TextType.Punctuation, _ -> x.Text
    | TextType.Word, TextView.FullText -> x.Text
    | TextType.Word, TextView.Letters v -> x.Text |> String.mapi (fun i x -> if i < v then x else '_')
    | TextType.Word, TextView.NoText -> x.Text |> String.map (fun x -> '_')

  let g = 
    match x.TextType with
    | TextType.Number -> ignore
    | TextType.Punctuation -> ignore
    | TextType.Word -> (fun _ -> dispatch (ToggleTextView { Id = x.Id; TextView = Helpers.toggleTextView x }))

  Button.span [ Button.OnClick g ] [ str (if x.HasSpaceBefore then " " + t else t) ]

let iconButton txt (fn : _ -> unit) icon =
  Button.button 
    [ Button.OnClick fn ]
    [ Icon.icon [ ] [ icon [] ]
      if String.IsNullOrWhiteSpace txt then () else span [] [ str txt ] ] 

let adminOnly (appUserOption : AppUser option) element =
  match appUserOption with
  | Some u when u.Role = Some Admin -> element
  | _ -> div [ ] [ ]

let viewContent (model: Model) dispatch =
  div [ ] [

        // for x in entry.TextParts do
        //   match x.TextType with
        //   | TextType.Word ->
        //     Button.button [] [ str x.Text ]
        //   | TextType.Punctuation ->
        //     Button.button [ Button.Disabled true; Button.Color IsPrimary; ] [ str x.Text ]
        //   | TextType.Number ->
        //     Button.button [ Button.Disabled true; Button.Color IsInfo; ] [ str x.Text ]
    let currentEntry = model.CurrentEntry |> Option.bind (fun guid -> model.Entries |> List.tryFind (fun x -> x.Id = guid))
    match model.Editor, currentEntry with
    | Some e,_ ->
      Columns.columns [ ]
        [ Column.column [ ]
            [ iconButton "" (fun _ -> dispatch AddOrUpdateEntry ) arrowLeftIcon ] ]
      Columns.columns [ ]
        [ Column.column [ ]
            [ Field.div [ ]
                [ Label.label [ ] [ str "Title" ]
                  Control.div [ ]
                    [ Input.text 
                        [ Input.Color IsPrimary
                          Input.Placeholder "Enter Title"
                          Input.ValueOrDefault e.Title
                          Input.OnChange (fun ev -> !!ev.target?value |> UpdateTitle |> dispatch) ] ] ]
              Field.div [ ]
                [ Label.label [ ] [ str "Memory text" ]
                  Control.div [ ]
                    [ Textarea.textarea
                        [ Textarea.Color IsPrimary
                          Textarea.Placeholder "Paste or enter text to memorize here"
                          Textarea.ValueOrDefault e.Text
                          Textarea.OnChange (fun ev -> !!ev.target?value |> UpdateText |> dispatch) ] [ ] ] ] ] ] 
      match e.EntryId with
      | None -> ()
      | Some id ->
        Columns.columns [ ] [ Column.column [ ] [ iconButton "Delete" (fun _ -> dispatch (RemoveEntry id) ) bookMinusIcon ] ]
    | None, Some e ->
      Columns.columns [ ]
        [ Column.column [ ]
            [ iconButton "" (fun _ -> dispatch ViewList ) arrowLeftIcon ] ]
      
      Columns.columns [ ] [ Column.column [ ] [ str e.Title ] ]
      Columns.columns [ ] [ Column.column [ ] 
        [ Button.list [ ]
            [ for x in e.TextParts do
                viewTextPart x dispatch ] ] ]
      Columns.columns [ ] [ Column.column [ ] [ Button.button [ Button.OnClick (fun _ -> dispatch (BulkToggleTextView (TextView.NoText))) ] [ str "Blanks" ] ] ]
      Columns.columns [ ] [ Column.column [ ] [ Button.button [ Button.OnClick (fun _ -> dispatch (BulkToggleTextView (TextView.Letters 1))) ] [ str "First letter" ] ] ]
      Columns.columns [ ] [ Column.column [ ] [ Button.button [ Button.OnClick (fun _ -> dispatch (BulkToggleTextView (TextView.FullText))) ] [ str "Full text" ] ] ]
      //           //match e.HintLevel with
      //           //| None ->
      //           //  yield View.Button(
      //           //    text = "Hint",
      //           //    command = (fun () -> dispatch (HintLevelChanged 1))
      //           //  )
      //           //| Some hintLevel ->
      //           //  let maxVal = e.TextParts |> List.map (fun x -> x.Text.Length) |> List.max
      //           //  yield View.Label(text = sprintf "Hint letter(s) %i" hintLevel)
      //           //  yield View.Slider(
      //           //    minimumMaximum = (0.0,double maxVal),
      //           //    value = double hintLevel,
      //           //    valueChanged = (fun args -> dispatch (HintLevelChanged (int (args.NewValue + 0.5)))))
    | None, None ->
      match model.Entries with
      | [] ->
        Columns.columns [ ] [ Column.column [ ] [ h3 [ ] [ str "Tap the blue book to create your first entry" ] ] ]
        Columns.columns [ ] [ Column.column [ ] [ bookPlusIcon [ OnClick (fun _ -> dispatch AddEntry) :> IHTMLProp; Style [ CSSProp.Color "blue"; FontSize "50"; ] :> IHTMLProp ] ] ]
      //         View.StackLayout(
      //           verticalOptions = LayoutOptions.Center,
      //           children =
      //             [ View.Image()
      //               View.Label(
      //                 text = "Tap the blue button to",
      //                 horizontalOptions = LayoutOptions.Center,
      //                 fontSize = FontSize.Named NamedSize.Large)
      //               View.Label(
      //                 text = "create your first entry",
      //                 horizontalOptions = LayoutOptions.Center,
      //                 fontSize = FontSize.Named NamedSize.Large)
      //               View.Button(
      //                 text = IconFont.BookPlus,
      //                 fontFamily = materialFont, 
      //                 fontSize = FontSize 60.,
      //                 fontAttributes = FontAttributes.Bold,
      //                 textColor = Color.RoyalBlue,
      //                 backgroundColor = Color.Transparent,
      //                 horizontalOptions = LayoutOptions.Center,
      //                 borderColor = Color.Transparent,
      //                 borderWidth = 0.0,
      //                 command = (fun () -> dispatch AddEntry))]
      //         )
      | _ ->
        Columns.columns [ ]
          [ Column.column [ Column.Width (Screen.All, Column.IsNarrow) ] [ Icon.icon [ ] [ bookshelfIcon [ ] ]; ]
            Column.column [ ] [ span [ ] [ str "Entries" ] ] ]
        for x in model.Entries do
          Columns.columns [ ] 
            [ Column.column [ Column.Width (Screen.All, Column.IsNarrow) ] [ iconButton "" (fun _ -> dispatch (SelectEntry x.Id)) glassesIcon ]
              Column.column [ Column.Width (Screen.All, Column.IsNarrow) ] [ iconButton "" (fun _ -> dispatch (UpdateEntry x.Id)) pencilIcon ]
              Column.column [ ] [ str x.Title ] ]
        Columns.columns [ ] [ Column.column [ ] [ iconButton "" (fun _ -> dispatch AddEntry) bookPlusIcon ] |> adminOnly model.User ]
  ]

let view (model : Model) (dispatch : Msg -> unit) =
  div [ OnLoad (fun _ -> Auth.init dispatch; ) ]
    [ Navbar.navbar [ Navbar.Color IsPrimary ]
                [ Navbar.Brand.div [ ]
                    [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                        [ img [ Style [ Width "2.5em" ] // Force svg display
                                Src "shape.svg" ] ]
                      Navbar.Item.div [ ]
                        [ Heading.h2 [ ]
                            [ str "memoria" ] ] ]
                  if model.User.IsSome then
                    Navbar.End.div [ ]
                      [ Navbar.Item.div [ ] 
                          [ Button.button [ Button.OnClick (fun _ -> Auth.signOut dispatch) ] 
                              [ str "Sign out"]
                            iconButton "Demote user" (fun _ -> dispatch DemoteUser) boomGateDownIcon |> adminOnly model.User ] ] ]
                              // div [ ] [ a [ Href "#"; OnClick (fun _ -> authDisconnect dispatch) ] [ str "Disconnect" ] ]
      Section.section [(if model.User.IsSome then Section.Props [ Style [ Props.Display DisplayOptions.None ] ] else Section.Props [ ] ) ] [ Container.container [ ]
        [ Columns.columns [ Columns.IsCentered ]
            [ Column.column [ Column.Width (Screen.All, Column.IsNarrow) ]
                [ div [ Id "g-signin-btn" ] [ ] ] ]
          Columns.columns [ Columns.IsCentered ]
            [ Column.column [ Column.Width (Screen.All, Column.IsNarrow) ]
                [ Text.p [ Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "or" ] ] ]
          Columns.columns [ Columns.IsCentered ]
            [ Column.column [ Column.Width (Screen.All, Column.IsNarrow) ]
                [ iconButton "View as sample user" (fun _ -> dispatch (SignedIn Sample)) loginIcon ] ] ] ] 
      match model.User with
      | None -> ()
      | Some user ->
        Section.section [ ] 
          [ viewContent model dispatch ] ]

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
