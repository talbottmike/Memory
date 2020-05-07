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
  let model = { Editor = Some { Title = ""; Text = ""; EntryId = None; }; Entries = [ defaultEntry ]; CurrentEntry = None; }
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
open Fable.FontAwesome
open Fable.Core
open Fable.Import
open Fable.Core.JS

let [<Literal>] ESC_KEY = 27.
let [<Literal>] ENTER_KEY = 13.
let internal onEnter msg dispatch =
    function
    | (ev:Browser.Types.KeyboardEvent) when ev.keyCode = ENTER_KEY ->
        ev.target?value <- ""
        dispatch msg
    | _ -> ()
    |> OnKeyDown
                                     
let view (model : Model) (dispatch : Msg -> unit) =
    let entry = model.Entries.Head
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Memorization" ] ] ]
        
          Container.container []
              [ str entry.Text
                Card.card []
                  [ Card.header []
                      [ h2 []
                          [ str entry.Title ] ]
                    Card.content []
                      [ for x in entry.TextParts do
                        match x.TextType with
                        | TextType.Word ->
                          Button.button [] [ str x.Text ]
                        | TextType.Punctuation ->
                          Button.button [ Button.Disabled true; Button.Color IsPrimary; ] [ str x.Text ]
                        | TextType.Number ->
                          Button.button [ Button.Disabled true; Button.Color IsInfo; ] [ str x.Text ] ] ] ] ]

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
