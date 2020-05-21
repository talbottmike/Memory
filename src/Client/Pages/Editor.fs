module Client.Editor

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open System
open Shared.Domain
open Shared.Domain.Editor
open Elmish
open Fulma
open Fable.MaterialUI.MaterialDesignIcons
open Client.Styles
open Client.Utils
open Shared
open Thoth.Json
open Elmish.React
open Fetch.Types
open Thoth.Fetch
open Fulma.Extensions.Wikiki
open Elmish.Navigation
open Client.Pages

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

let update (msg:Editor.Msg) model : Editor.Model*Cmd<Editor.Msg> =
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
    let navCmd = Navigation.newUrl (toHash Page.Entries)
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

let view (model : Editor.Model) (dispatch : Editor.Msg -> unit) =
  div [ ]
    [ Columns.columns [ ]
        [ Column.column [ ]
            [ Field.div [ ]
                [ Label.label [ ] [ str "Title" ]
                  Control.div [ ]
                    [ Input.text 
                        [ Input.Color IsPrimary
                          Input.Placeholder "Enter Title"
                          Input.ValueOrDefault model.Title
                          Input.OnChange (fun ev -> !!ev.target?value |> UpdateTitle |> dispatch) ] ] ]
              Field.div [ ]
                [ Label.label [ ] [ str "Memory text" ]
                  Control.div [ ]
                    [ Textarea.textarea
                        [ Textarea.Color IsPrimary
                          Textarea.Placeholder "Paste or enter text to memorize here"
                          Textarea.ValueOrDefault model.Text
                          Textarea.OnChange (fun ev -> !!ev.target?value |> UpdateText |> dispatch) ] [ ] ] ]
              Field.div [ Field.IsGrouped ]
                [ Control.div [ ]
                    [ Button.button [ Button.Color IsPrimary; Button.OnClick (fun _ -> dispatch AddOrUpdateEntry ) ]
                        [ str "Submit" ] ]
                  Control.div [ ]
                    [ Button.button [ Button.OnClick (fun _ -> dispatch CancelEntry) ]
                        [ str "Cancel" ] ] ] ] ] 
      match model.EntryId with
      | None -> ()
      | Some id ->
        Columns.columns [ ] [ Column.column [ ] [ Styles.iconButton "Delete" (fun _ -> dispatch (RemoveEntry id) ) bookMinusIcon ] ]
        Columns.columns [ ] [ Column.column [ ] [ Styles.iconButton "Add as sample" (fun _ -> dispatch (SaveSampleEntry { MemorizationEntry.Id = id; Text = model.Text; Title = model.Title })) accountSupervisorIcon |> adminOnly model.User ] ] ]