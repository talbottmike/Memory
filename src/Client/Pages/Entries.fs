module Client.Entries

open System
open Shared.Domain
open Elmish
open Feliz
open Feliz.UseElmish
open Feliz.MaterialUI
open Fable.MaterialUI.MaterialDesignIcons
open Feliz.Router
open Client.Pages
open Feliz.Delay

type Model = { User : AppUser option; Entries : MemorizationEntryDisplay list; }
type Msg =
  | SelectEntry of Guid
  | AddEntry
  | UpdateEntry of Guid
  | BrowserEntriesLoaded of MemorizationEntryDisplay list
  | EntriesLoaded of MemorizationEntry list
  | SavedEntries
type Props = 
  { Model: Model
    Dispatch: Msg -> unit }

let init (userOption : AppUser option) =
  let cmds =
    [ Cmd.OfFunc.perform (WebStorage.Entries.load) () (BrowserEntriesLoaded)
      match userOption with
      | Some u when u.MemoriaToken.IsSome -> Cmd.OfPromise.perform Api.getEntries u (EntriesLoaded) 
      | Some _
      | None -> () ]
  { User = userOption; Entries = []; }, Cmd.batch cmds

let update (msg:Msg) model : Model*Cmd<Msg> =
  match msg with
  | SelectEntry guid ->
    let navCmd = Cmd.navigate (toHash (Page.Practice (Some guid)))
    model, navCmd
  | UpdateEntry guid ->
    let navCmd = Cmd.navigate (toHash (Page.Editor (Some guid)))
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
    let navCmd = Cmd.navigate (toHash (Page.Editor None))
    model, navCmd
  | SavedEntries ->
    model, Cmd.none

let view = React.functionComponent (fun (input: {| userOption: AppUser option; |}) ->
  let model, dispatch = React.useElmish(init input.userOption, update, [| |])
  Html.div [
    match model.Entries with
    | [] ->
      React.delay [
        delay.waitFor 2000

        delay.children [
          Mui.grid [
            grid.container true
            grid.children [
              Mui.grid [
                grid.item true
                grid.children [
                  Html.h3 [ Html.text "Tap the blue book to create your first entry" ]
                ]
              ]
            ]
          ]
          Mui.grid [
            grid.container true
            grid.children [
              Mui.grid [
                grid.item true
                grid.children [
                  Mui.fab [
                    fab.color.primary
                    fab.size.medium
                    fab.children [ 
                        bookPlusIcon []
                    ]
                    prop.onClick (fun _ -> dispatch AddEntry) 
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    | _ ->
      Mui.grid [
        grid.container true
        grid.spacing._3
        grid.justify.flexStart
        grid.alignItems.center
        grid.children [
          Mui.grid [
            grid.item true
            grid.children [
                bookshelfIcon [ icon.fontSize.large ]
            ]
          ]
          Mui.grid [
            grid.item true
            grid.children [
                Mui.typography [ 
                  typography.variant.h5
                  typography.children "Entries" 
                ]
            ]
          ]
        ]
      ]
      for x in model.Entries do
        Mui.grid [
          grid.container true
          grid.spacing._1
          grid.justify.flexStart
          grid.alignItems.center
          grid.children [
            Mui.grid [
              grid.item true
              grid.children [
                Mui.iconButton [
                  prop.ariaLabel "View"
                  iconButton.children [ glassesIcon [] ]
                  prop.onClick (fun _ -> dispatch (SelectEntry x.Id)) 
                ]
              ]
            ]
            Mui.grid [
              grid.item true
              grid.children [
                Mui.iconButton [
                  prop.ariaLabel "Edit"
                  iconButton.children [ pencilIcon [] ]
                  prop.onClick (fun _ -> dispatch (UpdateEntry x.Id)) 
                ]
              ]
            ]
            Mui.grid [
              grid.item true
              grid.children [
                  Html.text x.Title
              ]
            ]
          ]
        ]
      // Only subscribers can add more than one entry
      Mui.grid [
        grid.container true
        grid.spacing._3
        grid.children [
          Mui.grid [
            grid.item true
            grid.children [
              Mui.fab [
                fab.color.primary
                fab.size.medium
                fab.children [ 
                    bookPlusIcon []
                ]
                prop.onClick (fun _ -> dispatch AddEntry) 
              ]
            ]
          ]
        ]
      ]
      |> Styles.subscriberOnly model.User
  ]
)