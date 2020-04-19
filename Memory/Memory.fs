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
      | SelectEntry of Guid
      | AddOrUpdateEntry
      | UpdateText of string
      | UpdateTitle of string
      | ToggleTextView of int
      | ViewList
  
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
      //Regex.Split(t, @"(\b[^\s]+\b)")
      //|> Seq.mapi identifyTextType
      //|> Seq.pairwise
      //|> Seq.collect (fun (x,y) -> 
      //  [ (if x.Text = " " then { y with HasSpaceBefore = true; } else y) ]
      //)
      //|> Seq.filter (fun x -> x.Text.Trim() <> "")
      //|> Seq.toList
      [ { Id = 1; Text = "Test"; TextView = FullText; TextType = TextType.Word; HasSpaceBefore = false; } ]
      |> (fun x -> Analytics.TrackEvent(sprintf "Found %i text parts" x.Length); x)
  
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
      | AddOrUpdateEntry ->
        let newModel =
          match model.Editor with
          | None -> 
            model
          | Some e -> 
            let newCurrentEntry, newEntries =
              match e.EntryId with
              | None -> 
                let id = Guid.NewGuid()
                let entries = { Id = id; Title = e.Title; Text = e.Text; TextParts = getTextParts e.Text; } :: model.Entries
                (id, entries)
              | Some guid -> 
                let entries = model.Entries |> List.map (fun x -> if guid = x.Id then { x with Title = e.Title; Text = e.Text; TextParts = getTextParts e.Text; } else x)
                (guid, entries)
            { model with Editor = None; Entries = newEntries; CurrentEntry = Some newCurrentEntry }
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
      match model.Editor, currentEntry with
      | Some e,_ ->
        View.ContentPage(
          View.StackLayout(
            [ View.Label(text = "Title")
              View.Entry(text = e.Title, textChanged = fun textArgs -> UpdateTitle textArgs.NewTextValue |> dispatch)
              View.Label(text = "Text to memorize")
              View.Entry(text = e.Text, textChanged = fun textArgs -> UpdateText textArgs.NewTextValue |> dispatch)
              View.Button(text = (if e.EntryId.IsSome then "Update" else "Ok"), command = (fun () -> dispatch AddOrUpdateEntry))]
          ))
      | None, Some e ->
        View.ContentPage(
          View.StackLayout(
            children = [
              View.Label(text = e.Title, horizontalOptions = LayoutOptions.Center, horizontalTextAlignment=TextAlignment.Center)
              View.ScrollView(
                View.FlexLayout(
                  direction = FlexDirection.Row,
                  wrap = FlexWrap.Wrap,
                  children = [ 
                      for x in e.TextParts do
                        viewTextPart x dispatch ]))
              View.Button(text = "Edit", command = (fun () -> dispatch (UpdateEntry e.Id)))
              View.Button(text = "Home", command = (fun () -> dispatch ViewList))]))
      | None, None ->
        match model.Entries with
        | [] ->
          View.ContentPage(
            View.StackLayout(
              [ View.Label(text = "Add entry to memorize")
                View.Button(text = "Start", command = (fun () -> dispatch AddEntry))]
            ))
        | _ ->
          View.ContentPage(
            View.StackLayout(
              [ View.Label(text = "Existing entries")
                for x in model.Entries do
                  let t = (x.Text |> Seq.truncate 100 |> String.Concat)
                  View.Label(text = x.Title)
                  View.Label(text = t)
                  View.Label(text = sprintf "Length: %i" x.TextParts.Length)
                  View.Button(text = "View", command = (fun () -> dispatch (SelectEntry x.Id)))
                View.Button(text = "Add New", command = (fun () -> dispatch AddEntry))]
            ))

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


