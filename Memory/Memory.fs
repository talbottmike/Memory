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

module App = 
    type TextType = | Word | Punctuation | Number
    type TextView = | FullText | FirstLetter | NoText 
    type TextPart = { Id : int; Text : string; TextType : TextType; TextView : TextView; HasSpaceBefore : bool; }
    type MemorizationEntry = { Id : Guid; Title : string; Text : string; TextParts : TextPart list; }
    type EditorValues = { EntryId : Guid option; Title : string; Text : string; }
    type Model = { Entries : MemorizationEntry list; Editor : EditorValues option; CurrentEntry : Guid option } 

    type Msg =
      | AddEntry
      | UpdateEntry of Guid
      | RemoveEntry of Guid
      | SelectEntry of Guid
      | AddOrUpdateEntry
      | UpdateText of string
      | UpdateTitle of string
      | ToggleTextView of int
      | ViewList
    
    let materialFont =
      match Device.RuntimePlatform with
      | Device.iOS -> "Material Design Icons"
      | Device.Android -> "materialdesignicons-webfont.ttf#Material Design Icons"
      | _ -> null

    let (|FirstRegexGroup|_|) pattern input =
       let m = Regex.Match(input,pattern) 
       if (m.Success) then Some m.Groups.[1].Value else None  

    let identifyTextType id (s:string) =
      match s with
      | FirstRegexGroup "[\d]" v -> { Id = id; Text = s; TextType = TextType.Number; TextView = FullText; HasSpaceBefore = false; }
      | FirstRegexGroup "[^\w\s']" v -> { Id = id; Text = s; TextType = TextType.Punctuation; TextView = FullText; HasSpaceBefore = false; }
      | _ -> { Id = id; Text = s; TextType = TextType.Word; TextView = FullText; HasSpaceBefore = false; }
  
    let getTextParts (t : string) = 
      Analytics.TrackEvent("Getting text parts")
      Regex.Split(t, @"(\b[^\s]+\b)")
      |> Seq.mapi identifyTextType
      |> Seq.pairwise
      |> Seq.collect (fun (x,y) -> 
        [ (if x.Text = " " then { y with HasSpaceBefore = true; } else y) ]
      )
      |> Seq.filter (fun x -> x.Text.Trim() <> "")
      |> Seq.toList
      |> (fun x -> Analytics.TrackEvent(sprintf "Found %i text parts" x.Length); x)
  
    let strOption (s : string) = if String.IsNullOrWhiteSpace s then None else Some s
    let init () : Model * Cmd<Msg> =
      let model = { Editor = Some { Title = ""; Text = ""; EntryId = None; }; Entries = []; CurrentEntry = None; }
      model, Cmd.none        
        
    let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
      match msg with
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
                  let entries = { Id = id; Title = e.Title; Text = e.Text; TextParts = getTextParts e.Text; } :: model.Entries
                  (Some id, entries)
              | Some guid -> 
                match strOption e.Title, strOption e.Text with
                | None, None -> 
                  let entries = model.Entries |> List.filter (fun x -> guid <> x.Id)
                  (None, entries)
                | _, _ -> 
                  let entries = model.Entries |> List.map (fun x -> if guid = x.Id then { x with Title = e.Title; Text = e.Text; TextParts = getTextParts e.Text; } else x)
                  (Some guid, entries)
            { model with Editor = None; Entries = newEntries; CurrentEntry = newCurrentEntry }
        newModel, Cmd.none
      | ToggleTextView id ->
        let newModel =
          match model.CurrentEntry with
          | None -> model
          | Some guid ->
            let toggle x =
              let newTextView =
                match x.TextView with
                | TextView.FullText -> TextView.NoText
                | TextView.NoText -> TextView.FirstLetter
                | TextView.FirstLetter -> TextView.FullText
              { x with TextView = newTextView }
            let newEntries =
              let newTextParts (textParts : TextPart list) = textParts |> List.map (fun x -> if x.Id = id then toggle x else x)
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
    
    let viewTextPart (x : TextPart) dispatch =
      let t =
        match x.TextType, x.TextView with
        | TextType.Number, _ -> x.Text
        | TextType.Punctuation, _ -> x.Text
        | TextType.Word, TextView.FullText -> x.Text
        | TextType.Word, TextView.FirstLetter -> x.Text |> String.mapi (fun i x -> if i = 0 then x else '_')
        | TextType.Word, TextView.NoText -> x.Text |> String.map (fun x -> '_')

      let g = 
        match x.TextType with
        | TextType.Number -> []
        | TextType.Punctuation -> []
        | TextType.Word -> [ View.TapGestureRecognizer(command=(fun () -> dispatch (ToggleTextView x.Id))) ]

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
              yield View.Entry(
                text = e.Title, 
                textChanged = (fun (textArgs : TextChangedEventArgs) -> UpdateTitle textArgs.NewTextValue |> dispatch),
                placeholder = "Enter title"
                )
              yield View.Editor(
                text = e.Text, 
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
              View.Label(
                text = IconFont.ArrowLeft,
                fontFamily = materialFont, 
                fontSize = FontSize 40.,
                horizontalOptions = LayoutOptions.Start,
                gestureRecognizers = [ View.TapGestureRecognizer(command=(fun () -> dispatch ViewList)) ])
              View.Label(text = e.Title, horizontalOptions = LayoutOptions.Start, horizontalTextAlignment=TextAlignment.Center)
              View.ScrollView(
                View.FlexLayout(
                  direction = FlexDirection.Row,
                  wrap = FlexWrap.Wrap,
                  children = [ 
                      for x in e.TextParts do
                        yield viewTextPart x dispatch ]))
              View.Button(
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
                yield View.Button(
                  text = IconFont.BookPlus,
                  fontFamily = materialFont, 
                  fontSize = FontSize 40.,
                  fontAttributes = FontAttributes.Bold,
                  textColor = Color.RoyalBlue,
                  backgroundColor = Color.Transparent,
                  horizontalOptions = LayoutOptions.Start,
                  command = (fun () -> dispatch AddEntry))]
            )

      View.ContentPage(
        //visual = VisualMarker.Material,
        content = content)

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
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
          Crashes.TrackError(ex,dict [],[||])
          App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
