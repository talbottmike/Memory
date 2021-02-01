module Client.Editor

open Fable.Core
open Fable.Core.JsInterop
open System
open Shared.Domain
open Elmish
open Feliz
open Feliz.UseElmish
open Feliz.MaterialUI
open Fable.MaterialUI.MaterialDesignIcons
open Shared
open Elmish.React
open Fetch.Types
open Thoth.Fetch
open Client.Pages
open Feliz.Router

type Model = { User : AppUser option; EntryId : Guid option; Text : string; Title : string; } 

type Msg =
  | UpdateText of string
  | UpdateTitle of string
  | AddOrUpdateEntry
  | SaveEntry of MemorizationEntryDisplay
  | SaveSampleEntry of MemorizationEntry
  | SavedEntry
  | CancelEntry
  | RemoveEntry of Guid
  | RemovedEntry
  | EntryAddedToDatabase
  | EntryRemovedFromDatabase
type Props = 
  { Model: Model
    Dispatch: Msg -> unit }
    
let init userOption entryIdOption (entries : MemorizationEntryDisplay list) = 
  let foundEntry = 
    entryIdOption 
    |> Option.bind (fun entryId -> 
      entries 
      |> List.tryFind (fun x -> x.Id = entryId)
      |> Option.orElseWith (fun _ -> WebStorage.Entries.load () |> List.tryFind (fun x -> x.Id = entryId))
    )
  match foundEntry with
  | Some e -> { User = userOption; EntryId = entryIdOption; Text = e.Text; Title = e.Title }, Cmd.none
  | None -> { User = userOption; EntryId = entryIdOption; Text = ""; Title = "" }, Cmd.none

let addEntry (token, request : MemorizationEntryDisplay) =
  let authenticatedJsonHeaders =
      [ HttpRequestHeaders.Authorization (sprintf "Bearer %s" token)
        HttpRequestHeaders.ContentType "application/json" ]
  let r = { Id = request.Id; Text = request.Text; Title = request.Title }
  Fetch.post<_, MemorizationEntry> (Shared.baseUrl + "api/add", data = r, headers = authenticatedJsonHeaders)

let removeEntry (token, request : Guid) =
  let authenticatedJsonHeaders =
      [ HttpRequestHeaders.Authorization (sprintf "Bearer %s" token)
        HttpRequestHeaders.ContentType "application/json" ]
  let r = request
  Fetch.post<_, MemorizationEntry> (Shared.baseUrl + "api/remove", data = r, headers = authenticatedJsonHeaders)

let addSampleEntry (token, request : MemorizationEntry) =
  let authenticatedJsonHeaders =
      [ HttpRequestHeaders.Authorization (sprintf "Bearer %s" token)
        HttpRequestHeaders.ContentType "application/json" ]
  let r = request
  Fetch.post<_, MemorizationEntry> (Shared.baseUrl + "api/addSample", data = r, headers = authenticatedJsonHeaders)

let update (msg:Msg) model : Model*Cmd<Msg> =
  match msg with
  | AddOrUpdateEntry ->
    let cmd =
      match Helpers.strOption model.Title, Helpers.strOption model.Text with
      | None, None -> Cmd.none
      | _, _ -> 
        let id = model.EntryId |> Option.defaultWith Guid.NewGuid
        let entry = { Id = id; Title = model.Title; Text = model.Text; TextParts = Helpers.getTextParts model.Text; HintLevel = None; }
        Cmd.ofMsg (SaveEntry entry)
    model, cmd
  | SaveEntry entry ->
      let cmds =
        let browserSave = Cmd.OfFunc.perform (WebStorage.Entries.saveEntry) entry (fun _ -> SavedEntry)
        let dbSave =
          match model.User with
          | Some u ->
            match u.Role, u.MemoriaToken with
            | Some Subscriber, Some t
            | Some Admin, Some t ->
              Cmd.OfPromise.perform addEntry (t, entry) (fun _ -> EntryAddedToDatabase) |> Some
            | _ -> None
          | _ -> None
        [ Some browserSave
          dbSave ]
        |> List.choose id
      model, Cmd.batch cmds
  | SaveSampleEntry entry ->
      let cmds =
        let dbSave =
          match model.User with
          | Some u ->
            match u.Role, u.MemoriaToken with
            | Some Admin, Some t ->
              Cmd.OfPromise.perform addSampleEntry (t, entry) (fun _ -> EntryAddedToDatabase) |> Some
            | _ -> None
          | _ -> None
        [ dbSave ]
        |> List.choose id
      model, Cmd.batch cmds
  | EntryRemovedFromDatabase
  | EntryAddedToDatabase ->
    model, Cmd.none
  | CancelEntry
  | RemovedEntry
  | SavedEntry ->
    let navCmd = Cmd.navigate(toHash Page.Entries)
    model, navCmd
  | RemoveEntry guid ->
    let cmds =
      let browserRemove = Cmd.OfFunc.perform (WebStorage.Entries.removeEntry) guid (fun _ -> RemovedEntry)
      let dbRemove =
        match model.User with
        | Some u ->
          match u.Role, u.MemoriaToken with
          | Some Subscriber, Some t
          | Some Admin, Some t ->
            Cmd.OfPromise.perform removeEntry (t, guid) (fun _ -> EntryRemovedFromDatabase) |> Some
          | _ -> None
        | _ -> None
      [ Some browserRemove
        dbRemove ]
      |> List.choose id
    model, Cmd.batch cmds
  | UpdateText t ->
    { model with Text = t }, Cmd.none
  | UpdateTitle t ->
    { model with Title = t }, Cmd.none

let view = React.functionComponent (fun (input: {| userOption: AppUser option; entryIdOption: Guid option; entries : MemorizationEntryDisplay list; |}) ->
  let model, dispatch = React.useElmish(init input.userOption input.entryIdOption input.entries, update, [| |])
  Html.div [
    Html.form [
      Mui.grid [
        grid.container true
        grid.spacing._3
        grid.justify.flexStart
        grid.alignItems.center
        grid.children [
          Mui.grid [
            grid.item true
            grid.xs._12
            grid.children [ 
              Mui.textField [
                textField.value model.Title
                textField.onChange (UpdateTitle >> dispatch)
                textField.variant.outlined
                textField.margin.normal
                textField.required false
                textField.fullWidth true
                textField.id "Title"
                textField.label "Title"
                textField.name "title"
                textField.autoComplete "title"
                textField.autoFocus true
                textField.placeholder "Enter Title"
              ]
              Mui.textField [
                textField.value model.Text
                textField.onChange (UpdateText >> dispatch)
                textField.variant.outlined
                textField.margin.normal
                textField.required true
                textField.fullWidth true
                textField.id "text"
                textField.label "Memory text"
                textField.name "text"
                textField.autoComplete "text"
                textField.autoFocus false
                textField.multiline true
                //textField.rows 5
                textField.rowsMax 40
                textField.placeholder "Paste or enter text to memorize here"
              ]
              Mui.grid [
                grid.container true
                grid.spacing._3
                grid.item true
                grid.children [
                  Mui.grid [
                    grid.item true
                    grid.children [ 
                      Mui.button [
                        prop.type'.submit
                        button.fullWidth true
                        button.variant.contained
                        button.color.primary
                        // button.classes.root classes.submit
                        button.children "Submit"
                        //button.disabled model.IsBusy
                        prop.onClick (fun e -> dispatch AddOrUpdateEntry)
                      ]
                    ]
                  ]
                  Mui.grid [
                    grid.item true
                    grid.children [ 
                      Mui.button [
                        prop.type'.reset
                        button.fullWidth true
                        button.variant.contained
                        button.color.default'
                        // button.classes.root classes.submit
                        button.children "Cancel"
                        //button.disabled model.IsBusy
                        prop.onClick (fun e -> dispatch CancelEntry)
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
    match model.EntryId with
    | None -> ()
    | Some id ->
      Mui.grid [
        grid.container true
        grid.spacing._3
        grid.children [
          Mui.grid [
            grid.item true
            grid.children [ 
              Mui.button [
                button.fullWidth true
                button.variant.contained
                button.color.default'
                // button.classes.root classes.submit
                button.children "Delete"
                //button.disabled model.IsBusy
                button.startIcon (bookMinusIcon [])
                prop.onClick (fun e -> dispatch (RemoveEntry id))
              ]
            ]
          ]
        ]
      ]
      Mui.grid [
        grid.container true
        grid.spacing._3
        grid.children [
          Mui.grid [
            grid.item true
            grid.children [ 
              Mui.button [
                button.fullWidth true
                button.variant.contained
                button.color.default'
                // button.classes.root classes.submit
                button.children "Add as sample"
                //button.disabled model.IsBusy
                button.startIcon (accountSupervisorIcon [])
                prop.onClick (fun e -> dispatch (SaveSampleEntry { MemorizationEntry.Id = id; Text = model.Text; Title = model.Title }))
              ]
            ]
          ]
        ]
      ]
      |> Styles.adminOnly model.User
  ]
)