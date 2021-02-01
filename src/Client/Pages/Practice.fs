module Client.Practice

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
open Feliz.Router
open Client.Pages

type Model = { User : AppUser option; CurrentEntry : MemorizationEntryDisplay option; } 
type Msg =
  | ToggleTextView of TextViewRequest
  | ViewList
  | BulkToggleTextView of TextView
  | SavedEntryPracticeState
  | SampleEntryFetchResult of MemorizationEntry option
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
    
  let cmd =
    match foundEntry with
    | Some e -> Cmd.none
    | None -> 
      match entryIdOption with
      | Some id -> Cmd.OfPromise.either Api.getSample id (SampleEntryFetchResult) (fun _ -> SampleEntryFetchResult None)
      | None -> Cmd.navigate (toHash Page.Entries)
  { User = userOption; CurrentEntry = foundEntry }, cmd

let update (msg:Msg) model : Model*Cmd<Msg> =
  match model.CurrentEntry, msg with
  | _, SampleEntryFetchResult v ->
    let cmd = 
      if model.CurrentEntry.IsNone && v.IsNone then 
        Cmd.navigate (toHash Page.Entries)
      else Cmd.none
    { model with CurrentEntry = v |> Option.map Helpers.toDisplayModel }, cmd
  | None, _ ->
    model, Cmd.navigate (toHash Page.Entries)
  | Some e, BulkToggleTextView textView ->
    let newTextParts (textParts : TextPart list) = textParts |> List.map (fun x -> if x.TextType = TextType.Word then { x with TextView = textView } else x)
    { model with CurrentEntry = Some { e with TextParts = newTextParts e.TextParts; } }, Cmd.none
  | Some e, ToggleTextView request ->
    let newTextParts (textParts : TextPart list) = textParts |> List.map (fun x -> if x.Id = request.Id then { x with TextView = Helpers.toggleTextView x } else x)
    { model with CurrentEntry = Some { e with TextParts = newTextParts e.TextParts; } }, Cmd.none
  | Some e, ViewList ->
    let savePracticeStateCmd = Cmd.OfFunc.perform (WebStorage.Entries.saveEntry) e (fun _ -> SavedEntryPracticeState)
    let navCmd = Cmd.navigate (toHash Page.Entries)
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

  Mui.typography [
    prop.onClick g
    prop.style [ style.fontFamily "monospace" ]
    typography.variant.h6
    typography.children (if x.HasSpaceBefore then " " + t else t)
  ]
let view = React.functionComponent (fun (input: {| userOption: AppUser option; entryIdOption: Guid option; entries : MemorizationEntryDisplay list; |}) ->
  let model, dispatch = React.useElmish(init input.userOption input.entryIdOption input.entries, update, [| |])
  Html.div [
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
            Mui.fab [
              fab.color.primary
              fab.size.medium
              fab.children [ 
                  arrowLeftIcon []
              ]
              prop.onClick (fun _ -> dispatch ViewList)
            ]
          ]
        ]
      ]
    ]
    
    match model.CurrentEntry with
    | None -> ()
    | Some e ->
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
              Mui.typography [ 
                typography.variant.h6
                typography.children e.Title 
              ]
            ]
          ]
        ]
      ]
      Mui.grid [
        grid.container true
        grid.spacing._1
        grid.justify.flexStart
        grid.alignItems.center
        grid.children [
          for x in e.TextParts do
            Mui.grid [
              grid.item true
              grid.children [ 
                viewTextPart x dispatch
              ]
            ]
        ]
      ]
      Mui.grid [
        grid.container true
        grid.spacing._3
        grid.justify.flexStart
        grid.alignItems.center
        grid.children [
          Mui.grid [
            grid.item true
            grid.children [ 
              Mui.button [
                prop.type'.reset
                button.variant.outlined
                button.color.default'
                button.children "Show blanks"
                prop.onClick (fun e -> dispatch (BulkToggleTextView (TextView.NoText)))
              ]
            ]
          ]
          Mui.grid [
            grid.item true
            grid.children [ 
              Mui.button [
                prop.type'.reset
                button.variant.outlined
                button.color.default'
                button.children "Show first letter"
                prop.onClick (fun e -> dispatch (BulkToggleTextView (TextView.Letters 1)))
              ]
            ]
          ]
          Mui.grid [
            grid.item true
            grid.children [ 
              Mui.button [
                prop.type'.reset
                button.variant.outlined
                button.color.default'
                button.children "Show full text"
                prop.onClick (fun e -> dispatch (BulkToggleTextView (TextView.FullText)))
              ]
            ]
          ]
        ]
      ]
  ]
)