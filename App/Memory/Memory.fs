namespace Memory

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms
open System.Text.RegularExpressions
open System
open Microsoft.AppCenter
open Microsoft.AppCenter.Analytics
open Microsoft.AppCenter.Crashes
open Shared

module App =
    
    let materialFont =
      match Device.RuntimePlatform with
      | Device.iOS -> "Material Design Icons"
      | Device.Android -> "materialdesignicons-webfont.ttf#Material Design Icons"
      | _ -> null

    let init () : Model * Cmd<Msg> =
      let model = { Editor = Some { Title = ""; Text = ""; EntryId = None; }; Entries = []; CurrentEntry = None; }
      model, Cmd.none        
        
    let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
      let newModel, msgs = Helpers.update msg model
      let cmd =
        match msgs with
        | [] -> Cmd.none
        | _ -> msgs |> List.map Cmd.ofMsg |> Cmd.batch
      newModel, cmd
    
    let viewTextPart (x : TextPart) dispatch =
      let t =
        match x.TextType, x.TextView with
        | TextType.Number, _ -> x.Text
        | TextType.Punctuation, _ -> x.Text
        | TextType.Word, TextView.FullText -> x.Text
        | TextType.Word, TextView.Letters v -> x.Text |> String.mapi (fun i x -> if i < v then x else '_')
        | TextType.Word, TextView.NoText -> x.Text |> String.map (fun x -> '_')

      let g = 
        match x.TextType with
        | TextType.Number -> []
        | TextType.Punctuation -> []
        | TextType.Word -> [ View.TapGestureRecognizer(command=(fun () -> dispatch (ToggleTextView { Id = x.Id; TextView = Helpers.toggleTextView x }))) ]

      View.Label(
          text = (if x.HasSpaceBefore then " " + t else t),
          fontSize = FontSize.Named NamedSize.Large,
          gestureRecognizers = g
      )

    let view (model: Model) dispatch =
      let currentEntry = model.CurrentEntry |> Option.bind (fun guid -> model.Entries |> List.tryFind (fun x -> x.Id = guid))
      let content =
        match model.Editor, currentEntry with
        | Some e,_ ->
          View.StackLayout(
            [ 
              yield View.Label(
                text = IconFont.ArrowLeft,
                fontFamily = materialFont, 
                fontSize = FontSize 40.,
                horizontalOptions = LayoutOptions.Start,
                gestureRecognizers = [ View.TapGestureRecognizer(command=(fun () -> dispatch AddOrUpdateEntry)) ])
              yield View.Label(
                text = "Title",
                fontSize = FontSize 16.)
              yield View.Entry(
                text = e.Title, 
                fontSize = FontSize 20.,
                textChanged = (fun (textArgs : TextChangedEventArgs) -> UpdateTitle textArgs.NewTextValue |> dispatch),
                placeholder = "Enter title"
                )
              yield View.Label(
                text = "Memory text",
                fontSize = FontSize 16.)
              yield View.Editor(
                text = e.Text, 
                fontSize = FontSize 20.,
                textChanged = (fun (textArgs : TextChangedEventArgs) -> UpdateText textArgs.NewTextValue |> dispatch),
                placeholder = "Paste or enter text to memorize here",
                autoSize = EditorAutoSizeOption.TextChanges
              )
              match e.EntryId with
              | None -> ()
              | Some id -> 
                yield View.Button(
                  text = IconFont.BookMinus + " Delete",
                  fontFamily = materialFont, 
                  fontSize = FontSize 20.,
                  horizontalOptions = LayoutOptions.End,
                  command = (fun () -> dispatch (RemoveEntry id)))]
          )
        | None, Some e ->
          View.StackLayout(
            children = [
              yield View.Label(
                text = IconFont.ArrowLeft,
                fontFamily = materialFont, 
                fontSize = FontSize 40.,
                horizontalOptions = LayoutOptions.Start,
                gestureRecognizers = [ View.TapGestureRecognizer(command=(fun () -> dispatch ViewList)) ])
              yield View.Label(text = e.Title, horizontalOptions = LayoutOptions.Start, horizontalTextAlignment=TextAlignment.Center)
              yield View.ScrollView(
                View.FlexLayout(
                  direction = FlexDirection.Row,
                  wrap = FlexWrap.Wrap,
                  children = [ 
                      for x in e.TextParts do
                        yield viewTextPart x dispatch ]))
              //match e.HintLevel with
              //| None ->
              //  yield View.Button(
              //    text = "Hint",
              //    command = (fun () -> dispatch (HintLevelChanged 1))
              //  )
              //| Some hintLevel ->
              //  let maxVal = e.TextParts |> List.map (fun x -> x.Text.Length) |> List.max
              //  yield View.Label(text = sprintf "Hint letter(s) %i" hintLevel)
              //  yield View.Slider(
              //    minimumMaximum = (0.0,double maxVal),
              //    value = double hintLevel,
              //    valueChanged = (fun args -> dispatch (HintLevelChanged (int (args.NewValue + 0.5)))))
              yield View.Button(
                text = "Blanks",
                fontFamily = materialFont, 
                fontSize = FontSize 20.,
                horizontalOptions = LayoutOptions.Start,
                command = (fun () -> dispatch (BulkToggleTextView (TextView.NoText))))
              yield View.Button(
                text = "First letter",
                fontFamily = materialFont, 
                fontSize = FontSize 20.,
                horizontalOptions = LayoutOptions.Start,
                command = (fun () -> dispatch (BulkToggleTextView (TextView.Letters 1))))
              yield View.Button(
                text = "Full text",
                fontFamily = materialFont, 
                fontSize = FontSize 20.,
                horizontalOptions = LayoutOptions.Start,
                command = (fun () -> dispatch (BulkToggleTextView (TextView.FullText))))
              yield View.Button(
                text = IconFont.Pencil + " Edit",
                fontFamily = materialFont, 
                fontSize = FontSize 20.,
                horizontalOptions = LayoutOptions.End,
                command = (fun () -> dispatch (UpdateEntry e.Id)))])
        | None, None ->
          match model.Entries with
          | [] ->
            View.StackLayout(
              verticalOptions = LayoutOptions.Center,
              children =
                [ View.Image()
                  View.Label(
                    text = "Tap the blue button to",
                    horizontalOptions = LayoutOptions.Center,
                    fontSize = FontSize.Named NamedSize.Large)
                  View.Label(
                    text = "create your first entry",
                    horizontalOptions = LayoutOptions.Center,
                    fontSize = FontSize.Named NamedSize.Large)
                  View.Button(
                    text = IconFont.BookPlus,
                    fontFamily = materialFont, 
                    fontSize = FontSize 60.,
                    fontAttributes = FontAttributes.Bold,
                    textColor = Color.RoyalBlue,
                    backgroundColor = Color.Transparent,
                    horizontalOptions = LayoutOptions.Center,
                    borderColor = Color.Transparent,
                    borderWidth = 0.0,
                    command = (fun () -> dispatch AddEntry))]
            )
          | _ ->
            View.StackLayout(
              [ yield View.Label(
                  text = IconFont.Bookshelf + " Entries",
                  fontFamily = materialFont, 
                  fontSize = FontSize 30.)
                for x in model.Entries do
                  yield View.Button(
                    text = IconFont.Glasses + " " + x.Title,
                    fontFamily = materialFont, 
                    fontSize = FontSize 18.,
                    fontAttributes = FontAttributes.Bold,
                    backgroundColor = Color.Transparent,
                    horizontalOptions = LayoutOptions.Start,
                    command = (fun () -> dispatch (SelectEntry x.Id)))
                yield View.Label(
                  text = IconFont.BookPlus,
                  fontFamily = materialFont, 
                  fontSize = FontSize 40.,
                  fontAttributes = FontAttributes.Bold,
                  textColor = Color.RoyalBlue,
                  backgroundColor = Color.Transparent,
                  horizontalOptions = LayoutOptions.Start,
                  gestureRecognizers = [ View.TapGestureRecognizer(command=(fun () -> dispatch AddEntry)) ])]
            )

      View.ContentPage(
        //visual = VisualMarker.Material,
        content = content,
        padding = Thickness 0.)

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgram init update view

open System
open Microsoft.AppCenter
open Microsoft.AppCenter.Analytics
open Microsoft.AppCenter.Crashes

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> XamarinFormsProgram.run app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/tools.html#live-update for further  instructions.
    //do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/models.html#saving-application-state for further  instructions.
    let modelId = "model"

    override __.OnSleep() = 
        AppCenter.Start("android=08e8d768-d96d-43ad-b0dc-6d76ad907a19;",
          typeof<Analytics>, typeof<Crashes>);
        
        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)
        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<Shared.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
          Crashes.TrackError(ex,dict [],[||])
          App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
