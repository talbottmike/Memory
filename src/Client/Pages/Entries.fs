module Client.Entries

open Fable.React
open Fable.React.Props
open System
open Shared.Domain
open Shared.Domain.Entries
open Elmish
open Fulma
open Fulma.Extensions.Wikiki
open Fable.MaterialUI.MaterialDesignIcons
open Client.Styles
open Client.Utils
open Elmish.Navigation
open Client.Pages
open Thoth.Json
open Fable.Core

let init (userOption : AppUser option) =
  let cmds =
    [ Cmd.OfFunc.perform (WebStorage.Entries.load) () (BrowserEntriesLoaded)
      match userOption with
      | Some u when u.MemoriaToken.IsSome -> Cmd.OfPromise.perform Api.getEntries u (EntriesLoaded) 
      | Some _
      | None -> () ]
  { User = userOption; Entries = []; }, Cmd.batch cmds

let update (msg:Entries.Msg) model : Entries.Model*Cmd<Entries.Msg> =
  match msg with
  | SelectEntry guid ->
    let navCmd = Navigation.newUrl (toHash (Page.Practice (Some guid)))
    model, navCmd
  | UpdateEntry guid ->
    let navCmd = Navigation.newUrl (toHash (Page.Editor (Some guid)))
    model, navCmd
  | EntriesLoaded e ->
    let newEntries = e |> List.filter (fun x -> model.Entries |> List.exists (fun y -> y.Id = x.Id) |> not) |> List.map (fun x -> { Id = x.Id; Title = x.Title; Text = x.Text; TextParts = Shared.Helpers.getTextParts x.Text; HintLevel = None; })
    let newModel = { model with Entries = model.Entries |> List.append newEntries; }
    let saveEntriesCmd = Cmd.OfFunc.perform (WebStorage.Entries.saveEntries) newModel.Entries (fun _ -> SavedEntries)
    newModel, saveEntriesCmd
  | BrowserEntriesLoaded e ->
    let newEntries = e |> List.filter (fun x -> model.Entries |> List.exists (fun y -> y.Id = x.Id) |> not)
    let newModel = { model with Entries = model.Entries |> List.append newEntries; }
    newModel, Cmd.none
  | AddEntry ->
    let navCmd = Navigation.newUrl (toHash (Page.Editor None))
    model, navCmd
  | SavedEntries ->
    model, Cmd.none

let view (model : Entries.Model) (dispatch : Entries.Msg -> unit) =
  div [ ] 
    [ 
      match model.Entries with
      | [] ->
        Columns.columns [ ] [ Column.column [ ] [ h3 [ ] [ str "Tap the blue book to create your first entry" ] ] ]
        Columns.columns [ ] [ Column.column [ ] [ bookPlusIcon [ OnClick (fun _ -> dispatch AddEntry) :> IHTMLProp; Style [ CSSProp.Color "blue"; FontSize "50"; ] :> IHTMLProp ] ] ]
      | _ ->
        Columns.columns [ Columns.IsMobile ]
          [ Column.column [ Column.Width (Screen.All, Column.IsNarrow) ] [ Icon.icon [ ] [ bookshelfIcon [ ] ]; ]
            Column.column [ ] [ Text.span [ Modifiers [ Modifier.TextWeight TextWeight.Bold ] ] [ str "Entries" ] ] ]
        for x in model.Entries do
          // Non mobile view
          Columns.columns [ Columns.Modifiers [ Modifier.IsHidden (Screen.Mobile, true) ] ] 
            [ Column.column [ Column.Width (Screen.All, Column.IsNarrow) ] [ Styles.iconButton "" (fun _ -> dispatch (SelectEntry x.Id)) glassesIcon ]
              Column.column [ Column.Width (Screen.All, Column.IsNarrow) ] [ Styles.iconButton "" (fun _ -> dispatch (UpdateEntry x.Id)) pencilIcon ]
              Column.column [ ] [ str x.Title ] ]
          // Mobile view
          ul [] [
            li [ ] [
              Columns.columns [ Columns.Modifiers [ Modifier.IsHidden (Screen.Tablet, true) ]; Columns.IsMobile ]
                [ Column.column [ Column.Width (Screen.All, Column.IsNarrow) ] 
                    [ Styles.iconButton "" (fun _ -> dispatch (SelectEntry x.Id)) glassesIcon
                      Styles.iconButton "" (fun _ -> dispatch (UpdateEntry x.Id)) pencilIcon ]
                  Column.column [ ] [ str x.Title ] ] ] ]
        // Only subscribers can add more than one entry
        Columns.columns [ ] [ Column.column [ ] [ Styles.iconButton "" (fun _ -> dispatch AddEntry) bookPlusIcon ] |> Styles.subscriberOnly model.User ] ]