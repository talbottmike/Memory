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

open Shared
open Shared.Helpers

let defaultText = """Four score and seven years ago our fathers brought forth, upon this continent, a new nation, conceived in liberty, and dedicated to the proposition that "all men are created equal"/n/n Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived, and so dedicated, can long endure. We are met on a great battle field of that war. We have come to dedicate a portion of it, as a final resting place for those who died here, that the nation might live. This we may, in all propriety do. But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow, this ground-- The brave men, living and dead, who struggled here, have hallowed it, far above our poor power to add or detract. The world will little note, nor long remember what we say here; while it can never forget what they did here./n/n It is rather for us, the living, to stand here, we here be dedica-ted to the great task remaining before us -- that, from these honored dead we take increased devotion to that cause for which they here, gave the last full measure of devotion -- that we here highly resolve these dead shall not have died in vain; that the nation, shall have a new birth of freedom, and that government of the people by the people for the people, shall not perish from the earth."""
let defaultEntry = 
    { MemorizationEntry.Id = Guid.NewGuid()
      Title = "Gettysburg address"
      Text = defaultText
      TextParts = Helpers.getTextParts defaultText
      HintLevel = None }

let init () : Model * Cmd<Msg> =
  let model = { Editor = None; Entries = [ defaultEntry ]; CurrentEntry = Some defaultEntry.Id; }
  model, Cmd.none  
  
let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
  let newModel, msgs = Helpers.update msg model
  let cmd =
    match msgs with
    | [] -> Cmd.none
    | _ -> msgs |> List.map Cmd.ofMsg |> Cmd.batch
  newModel, cmd

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

let icon x = Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.i [ x ] [ ] ]

let iconButton txt icon (fn : _ -> unit) =
  Button.button 
    [ Button.OnClick fn ]
    [ Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.i [ icon ] [ ] ]
      span [] [ str txt ] ] 

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
            [ iconButton "Add or Update" Fa.Solid.ArrowLeft (fun _ -> dispatch AddOrUpdateEntry ) ] ]
      Columns.columns [ ]
        [ Column.column [ ]
            [ Field.div [ ]
                [ Label.label [ ] [ str "Title" ]
                  Control.div [ Control.HasIconLeft; ]
                    [ Input.text 
                        [ Input.Color IsPrimary
                          Input.Placeholder "Enter Title"
                          Input.ValueOrDefault e.Title
                          //Input.Props [(onEnter JoinGame dispatch :> IHTMLProp); AutoFocus true; ]
                          Input.OnChange (fun ev -> !!ev.target?value |> UpdateTitle |> dispatch) ]
                      Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ]
                        [ Fa.i [ Fa.Solid.User ] [ ] ] ] ]
              Field.div [ ]
                [ Label.label [ ] [ str "Memory text" ]
                  Control.div [ Control.HasIconLeft; ]
                    [ Input.text 
                        [ Input.Color IsPrimary
                          Input.Placeholder "Paste or enter text to memorize here"
                          Input.ValueOrDefault e.Text
                          //Input.Props [(onEnter JoinGame dispatch :> IHTMLProp); AutoFocus true; ]
                          Input.OnChange (fun ev -> !!ev.target?value |> UpdateText |> dispatch) ]
                      Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ]
                        [ Fa.i [ Fa.Solid.User ] [ ] ] ] ] ] ] 
      //       View.StackLayout(
      //         [ 
      //           yield View.Label(
      //             text = IconFont.ArrowLeft,
      //             fontFamily = materialFont, 
      //             fontSize = FontSize 40.,
      //             horizontalOptions = LayoutOptions.Start,
      //             gestureRecognizers = [ View.TapGestureRecognizer(command=(fun () -> dispatch AddOrUpdateEntry)) ])
      //           yield View.Label(
      //             text = "Title",
      //             fontSize = FontSize 16.)
      //           yield View.Entry(
      //             text = e.Title, 
      //             fontSize = FontSize 20.,
      //             textChanged = (fun (textArgs : TextChangedEventArgs) -> UpdateTitle textArgs.NewTextValue |> dispatch),
      //             placeholder = "Enter title"
      //             )
      //           yield View.Label(
      //             text = "Memory text",
      //             fontSize = FontSize 16.)
      //           yield View.Editor(
      //             text = e.Text, 
      //             fontSize = FontSize 20.,
      //             textChanged = (fun (textArgs : TextChangedEventArgs) -> UpdateText textArgs.NewTextValue |> dispatch),
      //             placeholder = "Paste or enter text to memorize here",
      //             autoSize = EditorAutoSizeOption.TextChanges
      //           )
      //           match e.EntryId with
      //           | None -> ()
      //           | Some id -> 
      //             yield View.Button(
      //               text = IconFont.BookMinus + " Delete",
      //               fontFamily = materialFont, 
      //               fontSize = FontSize 20.,
      //               horizontalOptions = LayoutOptions.End,
      //               command = (fun () -> dispatch (RemoveEntry id)))]
      //       )
    | None, Some e ->
      Columns.columns [ ]
        [ Column.column [ ]
            [ iconButton "View list" Fa.Solid.ArrowLeft (fun _ -> dispatch ViewList ) ] ]
      
      Columns.columns [ ] [ Column.column [ ] [ str e.Title ] ]
      Columns.columns [ ] [ Column.column [ ] 
        [ Button.list [ ]
            [ for x in e.TextParts do
                viewTextPart x dispatch ] ] ]
      //       View.StackLayout(
      //         children = [
      //           yield View.Label(text = e.Title, horizontalOptions = LayoutOptions.Start, horizontalTextAlignment=TextAlignment.Center)
      //           yield View.ScrollView(
      //             View.FlexLayout(
      //               direction = FlexDirection.Row,
      //               wrap = FlexWrap.Wrap,
      //               children = [ 
      //                   for x in e.TextParts do
      //                     yield viewTextPart x dispatch ]))
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
      //           yield View.Button(
      //             text = "Blanks",
      //             fontFamily = materialFont, 
      //             fontSize = FontSize 20.,
      //             horizontalOptions = LayoutOptions.Start,
      //             command = (fun () -> dispatch (BulkToggleTextView (TextView.NoText))))
      //           yield View.Button(
      //             text = "First letter",
      //             fontFamily = materialFont, 
      //             fontSize = FontSize 20.,
      //             horizontalOptions = LayoutOptions.Start,
      //             command = (fun () -> dispatch (BulkToggleTextView (TextView.Letters 1))))
      //           yield View.Button(
      //             text = "Full text",
      //             fontFamily = materialFont, 
      //             fontSize = FontSize 20.,
      //             horizontalOptions = LayoutOptions.Start,
      //             command = (fun () -> dispatch (BulkToggleTextView (TextView.FullText))))
      //           yield View.Button(
      //             text = IconFont.Pencil + " Edit",
      //             fontFamily = materialFont, 
      //             fontSize = FontSize 20.,
      //             horizontalOptions = LayoutOptions.End,
      //             command = (fun () -> dispatch (UpdateEntry e.Id)))])
    | None, None ->
      match model.Entries with
      | [] ->
        Columns.columns [ ]
          [ Column.column [ ]
              [ bookPlusIcon [ ] ] ]
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
          [ Column.column [ ]
              [ bookshelfIcon [ ] ] ]
        for x in model.Entries do
          Columns.columns [ ]
            [ Column.column [ ]
                [ iconButton x.Title Fa.Solid.Glasses (fun _ -> dispatch (SelectEntry x.Id)) ] ]
          
      //         View.StackLayout(
      //           [ yield View.Label(
      //               text = IconFont.Bookshelf + " Entries",
      //               fontFamily = materialFont, 
      //               fontSize = FontSize 30.)
      //             for x in model.Entries do
      //               yield View.Button(
      //                 text = IconFont.Glasses + " " + x.Title,
      //                 fontFamily = materialFont, 
      //                 fontSize = FontSize 18.,
      //                 fontAttributes = FontAttributes.Bold,
      //                 backgroundColor = Color.Transparent,
      //                 horizontalOptions = LayoutOptions.Start,
      //                 command = (fun () -> dispatch (SelectEntry x.Id)))
      //             yield View.Label(
      //               text = IconFont.BookPlus,
      //               fontFamily = materialFont, 
      //               fontSize = FontSize 40.,
      //               fontAttributes = FontAttributes.Bold,
      //               textColor = Color.RoyalBlue,
      //               backgroundColor = Color.Transparent,
      //               horizontalOptions = LayoutOptions.Start,
      //               gestureRecognizers = [ View.TapGestureRecognizer(command=(fun () -> dispatch AddEntry)) ])]
      //         )
  ]
let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Brand.div [ ]
                [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                    [ img [ Style [ Width "2.5em" ] // Force svg display
                            Src "shape.svg" ] ]
                  Navbar.Item.div [ ]
                    [ Heading.h2 [ ]
                        [ str "memoria" ] ] ] ]
              
        
          Container.container []
              [ Card.card []
                  [ Card.content []
                      [ viewContent model dispatch ] ] ] ]

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
