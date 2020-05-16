namespace Shared
open System

type TextType = | Word | Punctuation | Number
type TextView = | FullText | Letters of int | NoText 
type TextPart = { Id : int; Text : string; TextType : TextType; TextView : TextView; HasSpaceBefore : bool; }
type MemorizationEntryDisplay = { Id : Guid; Title : string; Text : string; TextParts : TextPart list; HintLevel : int option; }
type MemorizationEntry = { Id : Guid; Title : string; Text : string; }
type EditorValues = { EntryId : Guid option; Title : string; Text : string; }
type GoogleUser = { Token : string; Id : string; Name : string; Email : string; MemoriaToken : string option }
type UserRole = | Admin | Subscriber
type TokenResult = { Token : string; Role : UserRole option; }
type UserProvider =
  | Google of GoogleUser
  | Sample
type AppUser = { Provider : UserProvider; Role : UserRole option; MemoriaToken : string option; }
type Model = { User : AppUser option; Entries : MemorizationEntryDisplay list; Editor : EditorValues option; CurrentEntry : Guid option; } 
type TextViewRequest = { Id : int; TextView : TextView; }
type GoogleLoginRequest = { IdToken : string }
type StorageUser = { EmailAddress : string; Role : UserRole option; Entries : MemorizationEntry list; }

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
  | SignedIn of UserProvider
  | SignedOut
  | AuthDisconnected
  | SaveEntries
  | SavedEntries
  | EntryAddedToDatabase
  | StorageFailure of exn
  | EntriesLoaded of MemorizationEntry list
  | TokenReceived of TokenResult
  | DemoteUser

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
   