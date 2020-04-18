namespace Memory

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms
open System.Text.RegularExpressions

module App = 
    let memoryTitle = "Lorem ipsum"
    let memoryText = "Lorem ipsum dolor sit amet, aeterno commune scripserit vel ut, ne inani ullamcorper mea, usu in aperiri insolens neglegentur. Essent fierent deseruisse at pro, at sit magna tritani. Ex offendit neglegentur eos, an tota assum nonumy eam. Etiam delicatissimi usu id. Per cu adhuc iuvaret, nec ad etiam vocibus appetere, nec ne recusabo mediocrem."
    type TextType = | Word | Punctuation | Number
    type TextView = | FullText | FirstLetter | NoText 
    type TextPart = { Id : int; Text : string; TextType : TextType; TextView : TextView; HasSpaceBefore : bool; }
    type Model = { Title : string; Text : string; TextParts : TextPart list } 

    type Msg =
      | SetText of string
      | ToggleTextView of int
  
    let (|FirstRegexGroup|_|) pattern input =
       let m = Regex.Match(input,pattern) 
       if (m.Success) then Some m.Groups.[1].Value else None  

    let identifyTextType id (s:string) =
      match s with
      | FirstRegexGroup "[\d]" v -> { Id = id; Text = s; TextType = TextType.Number; TextView = FullText; HasSpaceBefore = false; }
      | FirstRegexGroup "[^\w\s']" v -> { Id = id; Text = s; TextType = TextType.Punctuation; TextView = FullText; HasSpaceBefore = false; }
      | _ -> { Id = id; Text = s; TextType = TextType.Word; TextView = FullText; HasSpaceBefore = false; }
  
    let getTextParts (t : string) = 
      Regex.Split(t, @"(\b[^\s]+\b)")
      |> Seq.mapi identifyTextType
      |> Seq.pairwise
      |> Seq.collect (fun (x,y) -> 
        //[ if x.Id = 1 then yield x else ()
        //  if x.Text = " " then yield { y with HasSpaceBefore = true; } else yield y ]
        [ if x.Text = " " then { y with HasSpaceBefore = true; } else y ]
      )
      |> Seq.filter (fun x -> x.Text.Trim() <> "")
      |> Seq.toList 
  
    let init () : Model * Cmd<Msg> =
      let textParts = getTextParts memoryText
      let model = { Title = memoryTitle; Text = memoryText; TextParts = textParts; }
      model, Cmd.none        
        
    let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
      match msg with
      | SetText t ->
        let tp = getTextParts t
        { model with Text = t; TextParts = tp; }, Cmd.none
      | ToggleTextView id ->
        let toggle x =
          let newTextView =
            match x.TextView with
            | TextView.FullText -> TextView.NoText
            | TextView.NoText -> TextView.FirstLetter
            | TextView.FirstLetter -> TextView.FullText
          { x with TextView = newTextView }
        let tp = model.TextParts |> List.map (fun x -> if x.Id = id then toggle x else x)
        { model with TextParts = tp; }, Cmd.none
    
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
        View.ContentPage(
          View.StackLayout(
            children = [
              View.Label(text = sprintf "%s" model.Title, horizontalOptions = LayoutOptions.Center, horizontalTextAlignment=TextAlignment.Center)
              View.ScrollView(
                View.FlexLayout(
                  direction = FlexDirection.Row,
                  wrap = FlexWrap.Wrap,
                  children = [ 
                      for x in model.TextParts do
                        viewTextPart x dispatch ]))
                  
              ]))

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgram init update view

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
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

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
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


