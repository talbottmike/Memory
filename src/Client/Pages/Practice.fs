module Client.Practice

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open System
open Shared.Domain
open Shared.Domain.Practice
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
    
  let cmd =
    match foundEntry with
    | Some e -> Cmd.none
    | None -> 
      match entryIdOption with
      | Some id -> Cmd.OfPromise.either Api.getSample id (SampleEntryFetchResult) (fun _ -> SampleEntryFetchResult None)
      | None -> Navigation.newUrl (toHash Page.Entries)
  { User = userOption; CurrentEntry = foundEntry }, cmd

let update (msg:Msg) model : Model*Cmd<Msg> =
  match model.CurrentEntry, msg with
  | _, SampleEntryFetchResult v ->
    let cmd = 
      if model.CurrentEntry.IsNone && v.IsNone then 
        Navigation.newUrl (toHash Page.Entries)
      else Cmd.none
    { model with CurrentEntry = v |> Option.map Helpers.toDisplayModel }, cmd
  | None, _ ->
    model, Navigation.newUrl (toHash Page.Entries)
  | Some e, BulkToggleTextView textView ->
    let newTextParts (textParts : TextPart list) = textParts |> List.map (fun x -> if x.TextType = TextType.Word then { x with TextView = textView } else x)
    { model with CurrentEntry = Some { e with TextParts = newTextParts e.TextParts; } }, Cmd.none
  | Some e, ToggleTextView request ->
    let newTextParts (textParts : TextPart list) = textParts |> List.map (fun x -> if x.Id = request.Id then { x with TextView = Helpers.toggleTextView x } else x)
    { model with CurrentEntry = Some { e with TextParts = newTextParts e.TextParts; } }, Cmd.none
  | Some e, ViewList ->
    let savePracticeStateCmd = Cmd.OfFunc.perform (WebStorage.Entries.saveEntry) e (fun _ -> SavedEntryPracticeState)
    let navCmd = Navigation.newUrl (toHash Page.Entries)
    model, Cmd.batch [savePracticeStateCmd; navCmd; ]
  | _,SavedEntryPracticeState ->
    model, Cmd.none
  

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
    | TextType.Word -> (fun _ -> dispatch (ToggleTextView { Id = x.Id; TextView = Shared.Helpers.toggleTextView x }))

  Text.span [ Props [ OnClick g; Style [ CSSProp.FontFamily "monospace"; ] :> IHTMLProp ] ] [ str (if x.HasSpaceBefore then " " + t else t) ]


let view (model : Practice.Model) (dispatch : Practice.Msg -> unit) =
  div [ ] 
    [ Columns.columns [ ]
        [ Column.column [ ]
            [ Styles.iconButton "" (fun _ -> dispatch ViewList ) arrowLeftIcon ] ]
      
      match model.CurrentEntry with
      | None -> ()
      | Some e ->
        Columns.columns [ ] [ Column.column [ ] [ str e.Title ] ]
        Columns.columns [ Columns.Props [ Tooltip.dataTooltip "Tap text to cycle through visibility" ]; Columns.CustomClass (Tooltip.ClassName+ " " + Tooltip.IsTooltipTop) ] [ Column.column [ ] 
          [ for x in e.TextParts do
                  viewTextPart x dispatch ] ]
        Columns.columns [ ] [ 
          Column.column [ ] [ 
            Button.button
              [ Button.OnClick (fun _ -> dispatch (BulkToggleTextView (TextView.NoText)))
                Button.Props [ Tooltip.dataTooltip "Hide all text" ]
                Button.CustomClass (Tooltip.ClassName+ " " + Tooltip.IsTooltipTop) ] 
              [ str "Show blanks" ] ]
          Column.column [ ] [ 
            Button.button 
              [ Button.OnClick (fun _ -> dispatch (BulkToggleTextView (TextView.Letters 1)))
                Button.Props [ Tooltip.dataTooltip "Hide all but the first letter of each word" ]
                Button.CustomClass (Tooltip.ClassName+ " " + Tooltip.IsMultiline) ] 
              [ str "Show first letter" ] ]
          Column.column [ ] [ 
            Button.button 
              [ Button.OnClick (fun _ -> dispatch (BulkToggleTextView (TextView.FullText)))
                Button.Props [ Tooltip.dataTooltip "Show all text" ]
                Button.CustomClass (Tooltip.ClassName+ " " + Tooltip.IsTooltipTop) ] 
              [ str "Show full text" ] ] ] ]