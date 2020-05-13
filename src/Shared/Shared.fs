namespace Shared
open System

type TextType = | Word | Punctuation | Number
type TextView = | FullText | Letters of int | NoText 
type TextPart = { Id : int; Text : string; TextType : TextType; TextView : TextView; HasSpaceBefore : bool; }
type MemorizationEntry = { Id : Guid; Title : string; Text : string; TextParts : TextPart list; HintLevel : int option; }
type EditorValues = { EntryId : Guid option; Title : string; Text : string; }
type GoogleUser = { Token : string; Id : string; Name : string; Email : string; }
type AppUser =
  | GoogleUser of GoogleUser
  | SampleUser
type Model = { User : AppUser option; Token : string option; Entries : MemorizationEntry list; Editor : EditorValues option; CurrentEntry : Guid option; } 
type TextViewRequest = { Id : int; TextView : TextView; }

type Msg =
  | AddEntry
  | UpdateEntry of Guid
  | RemoveEntry of Guid
  | SelectEntry of Guid
  | AddOrUpdateEntry
  | UpdateText of string
  | UpdateTitle of string
  | ToggleTextView of TextViewRequest
  | BulkToggleTextView of TextView
  | HintLevelChanged of int
  | ViewList
  | SignedIn of AppUser
  | SignedOut
  | AuthDisconnected

module Helpers =
  open System.Text.RegularExpressions
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
      [ (if x.Text = " " then { y with HasSpaceBefore = true; } else y) ]
    )
    |> Seq.filter (fun x -> x.Text.Trim() <> "")
    |> Seq.toList
    
  let strOption (s : string) = if String.IsNullOrWhiteSpace s then None else Some s
  let incrementTextView (x : TextPart) =
    match x.TextView with
    | TextView.FullText -> TextView.NoText
    | TextView.NoText -> TextView.Letters 1
    | TextView.Letters v -> 
      if x.Text.Length <= v then TextView.Letters (v + 1) else TextView.FullText
  let toggleTextView (x : TextPart) =
    match x.TextView with
    | TextView.FullText -> TextView.NoText
    | TextView.NoText -> TextView.Letters 1
    | TextView.Letters _ -> TextView.FullText
    
  let update (msg : Msg) (model : Model) : Model * Msg list =
    match msg with
    | AuthDisconnected
    | SignedOut ->
      { model with User = None }, []
    | SignedIn u ->
      { model with User = Some u; }, []
    | SelectEntry guid ->
      { model with CurrentEntry = Some guid; }, []
    | UpdateText t ->
      let newEditor =
        model.Editor
        |> Option.map (fun e -> { e with Text = t })
      { model with Editor = newEditor; }, []
    | UpdateTitle t ->
      let newEditor =
        model.Editor
        |> Option.map (fun e -> { e with Title = t })
      { model with Editor = newEditor; }, []
    | RemoveEntry guid ->
      let newModel =
        let newEntries = model.Entries |> List.filter (fun x -> guid <> x.Id)
        { model with Editor = None; Entries = newEntries; CurrentEntry = None; }
      newModel, []
    | HintLevelChanged hintLevel ->
      let newModel =
        match model.CurrentEntry with
        | None -> model
        | Some guid ->
          let newEntries =
            model.Entries |> List.map (fun x -> if guid = x.Id then { x with HintLevel = Some hintLevel; } else x)
          { model with Entries = newEntries }
      newModel, []
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
                let entries = { Id = id; Title = e.Title; Text = e.Text; TextParts = getTextParts e.Text; HintLevel = None; } :: model.Entries
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
      newModel, []
    | BulkToggleTextView textView ->
      let newModel =
        match model.CurrentEntry with
        | None -> model
        | Some guid ->
          let newEntries =
            let newTextParts (textParts : TextPart list) = textParts |> List.map (fun x -> if x.TextType = TextType.Word then { x with TextView = textView } else x)
            model.Entries |> List.map (fun x -> if guid = x.Id then { x with TextParts = newTextParts x.TextParts; } else x)
          { model with Entries = newEntries }
      newModel, []
    | ToggleTextView request ->
      let newModel =
        match model.CurrentEntry with
        | None -> model
        | Some guid ->
          let newEntries =
            let newTextParts (textParts : TextPart list) = textParts |> List.map (fun x -> if x.Id = request.Id then { x with TextView = toggleTextView x } else x)
            model.Entries |> List.map (fun x -> if guid = x.Id then { x with TextParts = newTextParts x.TextParts; } else x)
          { model with Entries = newEntries }
      newModel, []
    | AddEntry ->
      { model with Editor = Some { EntryId = None; Text = ""; Title = ""; }}, []
    | UpdateEntry guid ->
      let editor = 
        model.Entries 
        |> List.tryFind (fun x -> x.Id = guid)
        |> Option.map (fun x -> { EntryId = Some x.Id; Text = x.Text; Title = x.Title; })
      { model with Editor = editor; }, []
    | ViewList ->
      { model with Editor = None; CurrentEntry = None; }, []